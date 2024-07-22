# Load required packages/ set graph theme
library(tidyverse)
library(geosphere)
library(leaflet)
library(leaflet.extras)
theme_set(theme_bw())


###########################  Importing Data ###########################

# Load all the trip files into a list
triplist <- list.files(pattern = "*.csv")
triplist

# Import the data
all_trips <- map(triplist, read_csv)

# Evaluate the data structure before merging the list
glimpse(all_trips)

# Combining data into a single data frame.
all_trips <- bind_rows(all_trips)


# Verify ride_id is a unique primary key
anyDuplicated(all_trips$ride_id)

# Re-evaluate structure
head(all_trips)
summary(all_trips)
str(all_trips)
View(all_trips)


###########################  Data Wrangling  ###########################


# Extrapolation of dates
all_trips$date <- as.Date(all_trips$started_at)
all_trips$weekday <- weekdays(all_trips$started_at)
all_trips$month <- month(all_trips$started_at, label = T)
all_trips$year <- year(all_trips$started_at)

# Convert weekday into a factor, ensuring days are analyzed in a logical sequence rather than alphabetically.
all_trips$weekday <- ordered(all_trips$weekday,
                             levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

# Verify the class of our weekday variable
class(all_trips$weekday)

# Calculate ride length
all_trips$ride_length_sec <- difftime(all_trips$ended_at, all_trips$started_at)

# convert ride_length_sec to numeric format
all_trips$ride_length_sec <- as.numeric(all_trips$ride_length_sec)


### Addressing missing station names ###


# Identify the number of NA values in the dataset
colSums(is.na(all_trips))

# Create lists of start and end stations with their coordinates
start_station_list <- all_trips %>%
  filter(!is.na(start_station_name)) %>% 
  distinct(start_station_id, start_lat, start_lng, .keep_all = T) %>%
  select(station_id = start_station_id,
         station_name = start_station_name,
         lat = start_lat,
         lng = start_lng)

end_station_list <- all_trips %>%
  filter(!is.na(end_station_name)) %>% 
  distinct(end_station_id, end_lat, end_lng, .keep_all = T) %>% 
  select(station_id = end_station_id,
         station_name = end_station_name,
         lat = end_lat,
         lng = end_lng)

# Combine start and end station lists
full_station_list <- bind_rows(start_station_list, end_station_list)

# Filter to ensure unique station names per coordinate pair
full_station_list <- full_station_list %>% 
  group_by(lat, lng) %>% 
  filter(n_distinct(station_id) == 1) %>% 
  summarize(station_id = first(station_id),
            station_name = first(station_name)) %>% 
  ungroup()


# Join with the main dataset to recover missing start station names and IDs
all_trips2 <- left_join(all_trips, full_station_list,
                        by = c("start_lat" = "lat",
                               "start_lng" = "lng"))
View(all_trips2)


# Coalesce joined start station names
all_trips2 <- all_trips2 %>%
  mutate(start_station_name = coalesce(start_station_name, station_name),
         start_station_id = coalesce(start_station_id, station_id)) %>% 
  select(-station_name, -station_id)

# Join with the main dataset to recover missing end station names and IDs
all_trips2 <- left_join(all_trips2, full_station_list,
                        by = c("end_lat" = "lat", "end_lng" = "lng"))

all_trips2 <- all_trips2 %>% 
  mutate(end_station_name = coalesce(end_station_name, station_name),
         end_station_id = coalesce(end_station_id, station_id)) %>% 
  select(-station_name, -station_id)

# Recovered a couple hundred thousand station names and id's
# Process was limited due to clustering of station coordinates
print(colSums(is.na(all_trips)))
print(colSums(is.na(all_trips2)))


########################### Identifying Extreme Outliers ##############################

# Distribution of ride length
summary(all_trips2$ride_length_sec)

# In-depth view considering there is such a large gap in the 4th quartile
# Calculate percentiles for ride length in groups of 0.05
fourth_quar_ride_length <- quantile(all_trips2$ride_length_sec, probs = c(.80, .85, .90, .95, .99, .998, .999, 1))
fourth_quar_ride_length


# The distribution is vast with great extremes, with a majority of outliers associated with the docked bikes
# Perhaps this is an indication of an error occurring with docked bikes. 
ggplot(all_trips2, aes(x = ride_length_sec/3600, color = rideable_type)) +
  geom_boxplot(outlier.color = "darkred", alpha = 0.3) +
  labs(title = "Total Distribution of Ride Length (Split by Bike Type)",
       x = "Ride Length (Hours)") +
  annotate("Text", x = 312, y = 0.04, label = " Majority of Outliers", size = 5)


# Most trips fall within a few hours or less, with the exception of docked bikes
ggplot(all_trips2, aes(x = ride_length_sec/3600, fill = rideable_type)) +
  geom_histogram(binwidth = 10, color = "black") +
  labs(title = "Distribution of Ride Length (Split by Bike Type, Log10 scale)",
       x = "Ride Length (Hours)",
       y = "Log10(Count)") +
  scale_y_log10(labels = scales::comma_format()) + 
  xlim(0, 700) +
  facet_wrap(~rideable_type) +
  scale_fill_brewer(palette = "Dark2")


# Calculate the distance using the Haversine formula (Output is in meters)
# This is euclidean distance and is not a reflection of of road network distance
all_trips2 <- all_trips2 %>%
  rowwise() %>% 
  mutate(geo_distance_meters = distHaversine(c(start_lng, start_lat), c(end_lng, end_lat)))


# Filter and examine erroneous data
invalid_trips <- all_trips2 %>% 
  filter((ride_length_sec <= 0 |
            (geo_distance_meters == 0 & ride_length_sec <= 60)) |
           start_station_name == "Pawel Bialowas - Test- PBSC charging station")
View(invalid_trips)


# After review, remove NA values and erroneous trips
all_trips3 <- all_trips2 %>%
  na.omit() %>%
  anti_join(invalid_trips, by = "ride_id")


########################### Data Analysis ##############################


# Create a station list to have one set of coordinates per station
map_station_list <- full_station_list %>% 
  distinct(station_id, station_name, .keep_all = T)


# Create a cluster leaflet map
stations_map_cluster <- leaflet(map_station_list) %>%
  addProviderTiles(provider = "Stadia.AlidadeSmoothDark") %>%
  setView(lng = -87.70, lat = 41.85,
          zoom = 11) %>%
  addMarkers(lng = ~lng, lat = ~lat,
             popup = ~paste("Station ID: ", station_id, "<br>Station Name: ", station_name),
             clusterOptions = markerClusterOptions())
stations_map_cluster


### General summary ###


# Total number of rides exceeds 5 million trips (Excluding NA values and those defined as erroneous)
# Shortest ride is 1 second, with the longest being approximately 678 hours
# The substantial difference between the 95th and 99th percentile indicates a small portion of rides are significantly longer than most rides
# The high standard deviation compared to the average length indicates a notable range in ride length.
general_summary <- all_trips3 %>%
  group_by() %>%
  summarize("Total Number of Rides" = n(),
            "Shortest Ride Length (Seconds)" = min(ride_length_sec),
            "Median Ride Length (Minutes)" = median(ride_length_sec/60),
            "Average Ride Length (Minutes)" = mean(ride_length_sec/60),
            "Longest Ride Length (Minutes)" = max(ride_length_sec/60),
            "Standard Deviation (Minutes)" = sd(ride_length_sec/60),
            "Interquartile Range (Minutes)" = IQR(ride_length_sec/60),
            "95th Percentile of Ride Length (Minutes)" = quantile(ride_length_sec/60, probs = 0.95),
            "99th Percentile of Ride Length (Minutes)" = quantile(ride_length_sec/60, probs = 0.99),
            "Average Ride Distance (Euclidean) (Miles)" = mean(geo_distance_meters*.00062137),
            "Longest Ride Distance (Euclidean) (Miles)" = max(geo_distance_meters*.00062137))
View(general_summary)


# A general re-examination of the distribution of ride length (Limited to 120 Minutes)
# IQR is 13.27 minutes suggesting the middle 50% of ride lengths are relatively close to the median. 
ggplot(all_trips3, aes(x = ride_length_sec/60)) +
  geom_boxplot(fill = "darkblue", outlier.color = "darkred", alpha = 0.1) +
  labs(title = "Distribution of Rides under 120 minutes",
       x = "Ride Length (minutes)",
       y = "") +
  annotate("Text", x = 62.5, y = 0.045, label = "Outliers", size = 5) +
  scale_x_continuous(breaks = seq(0, 120, by = 10), limits = c(0, 120))


### member summary ###


# Members statistics
# Significantly higher standard deviation for casual riders suggests that there is more variability in their ride lengths.
member_summary <- all_trips3 %>%
  group_by(member_casual) %>%
  summarize("Total Number of Rides" = n(),
            "Shortest Ride Length (Seconds)" = min(ride_length_sec),
            "Median Ride Length (Minutes)" = median(ride_length_sec/60),
            "Average Ride Length (Minutes)" = mean(ride_length_sec/60),
            "Longest Ride Length (Minutes)" = max(ride_length_sec/60),
            "Standard Deviation (Minutes)" = sd(ride_length_sec/60),
            "Interquartile Range (Minutes)" = IQR(ride_length_sec/60),
            "95th Percentile of Ride Length (Minutes)" = quantile(ride_length_sec/60, probs = 0.95),
            "99th Percentile of Ride Length (Minutes)" = quantile(ride_length_sec/60, probs = 0.99),
            "Average Ride Distance (Euclidean) (Miles)" = mean(geo_distance_meters*.00062137),
            "Longest Ride Distance (Euclidean) (Miles)" = max(geo_distance_meters*.00062137))
View(member_summary)


# Total number of rides between Members and casual riders
# Non-members consist of 41% of the rides
ggplot(member_summary, aes(x = member_casual, y = `Total Number of Rides`, fill = member_casual)) +
  geom_col() +
  geom_text(aes(label = `Total Number of Rides`), vjust = -0.5, size = 4) +
  labs(title = "Total Number of Rides (Casual/Member)",
       x = "Rider Type",
       y = "Total Number of Rides") +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_brewer(palette = "Dark2")

# Distribution of ride length
# Non-members average ride duration is twice as long as members despite members using the service more frequently.
ggplot(all_trips3, aes(x = ride_length_sec / 60, fill = member_casual)) +
  geom_density(alpha = 0.7) +
  geom_vline(xintercept = 12.69740, linetype = "dotted", color = "darkorange", size = 0.85) +
  geom_vline(xintercept = 25.05755, linetype = "dotted", color = "turquoise", size = 0.85) +
  annotate("text", x = 11.5, y = 0.062, label = "Avg for Member Riders 12.69 Mins", angle = 90) +
  annotate("text", x = 23.8, y = 0.062, label = "Avg for Casual Riders 25.05 Mins", angle = 90) +
  labs(title = "Distribution of Ride Length by Membership Type (0 - 120 Minutes)",
       x = "Ride Length (Minutes)",
       y = "Density") +
  scale_x_continuous(breaks = seq(0, 120, by = 10), limits = c(0, 120)) +
  scale_fill_brewer(palette = "Dark2")


### weekday summary ###


# The 95th and 99th percentile of ride length from casual riders are consistently 2-3 times longer than those from members. 
weekday_summary <- all_trips3 %>%
  group_by(weekday, member_casual) %>%
  summarize("Total Number of Rides" = n(),
            "Shortest Ride Length (Seconds)" = min(ride_length_sec),
            "Median Ride Length (Minutes)" = median(ride_length_sec/60),
            "Average Ride Length (Minutes)" = mean(ride_length_sec/60),
            "Longest Ride Length (Minutes)" = max(ride_length_sec/60),
            "Standard Deviation (Minutes)" = sd(ride_length_sec/60),
            "Interquartile Range (Minutes)" = IQR(ride_length_sec/60),
            "95th Percentile of Ride Length (Minutes)" = quantile(ride_length_sec/60, probs = 0.95),
            "99th Percentile of Ride Length (Minutes)" = quantile(ride_length_sec/60, probs = 0.99),
            "Average Ride Distance (Euclidean) (Miles)" = mean(geo_distance_meters*.00062137),
            "Longest Ride Distance (Euclidean) (Miles)" = max(geo_distance_meters*.00062137))
View(weekday_summary)

# Total number of rides by weekday
# Casual ride count goes up throughout the week, peaking on Saturday
# Member riders slightly decrease throughout the week, reaching a low point on Sunday.
ggplot(weekday_summary, aes(x = weekday, y = `Total Number of Rides`, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Memebrship Type's Ride Count (Weekday)",
       x = "",
       y = "Number of Rides") +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_brewer(palette = "Dark2")


# Average ride duration by member type (Weekday)
# Casual rides tend to have longer rides on the weekends suggesting they may be using the service for leisure.
# Member rides are consistently shorter which may suggest they are using the service for commuting. 
ggplot(weekday_summary, aes(x = weekday, y = `Average Ride Length (Minutes)`, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Memebrship Type's Average Ride Duration (Weekday)",
       x = "",
       y = "Avg Ride Length (Minutes)") +
  scale_fill_brewer(palette = "Dark2")


### month summary ###


# Shortest ride length remains at 1 second throughout the year, suggesting there is a consistent recording error. 
# IQR is higher during the warmer months
# 99th percentile are higher during the warmer months
# Its clear rider activity is influenced by the weather.
month_summary <- all_trips3 %>%
  group_by(month) %>%
  summarize("Total Number of Rides" = n(),
            "Shortest Ride Length (Seconds)" = min(ride_length_sec),
            "Median Ride Length (Minutes)" = median(ride_length_sec/60),
            "Average Ride Length (Minutes)" = mean(ride_length_sec/60),
            "Longest Ride Length (Minutes)" = max(ride_length_sec/60),
            "Standard Deviation (Minutes)" = sd(ride_length_sec/60),
            "Interquartile Range (Minutes)" = IQR(ride_length_sec/60),
            "95th Percentile of Ride Length (Minutes)" = quantile(ride_length_sec/60, probs = 0.95),
            "99th Percentile of Ride Length (Minutes)" = quantile(ride_length_sec/60, probs = 0.99),
            "Average Ride Distance (Euclidean) (Miles)" = mean(geo_distance_meters*.00062137),
            "Longest Ride Distance (Euclidean) (Miles)" = max(geo_distance_meters*.00062137))
View(month_summary)


# Plot monthly ride count by membership type
# The amount of rides drop significantly in the winter months, peaking in September.
# During the slower months it appears members ride twice as much as casual riders. 
all_trips3 %>% 
  group_by(month, member_casual) %>% 
  summarize("Number of Rides" = n()) %>% 
  ggplot(aes(x = month, y = `Number of Rides`, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Ride Count by Membership Type (Month)",
       x = "",
       y = "Ride Count") +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_brewer(palette = "Dark2")


# Plot monthly average ride by membership type
# Casual riders trip length are about twice as long as members. This is consistent throughout the year.
all_trips3 %>%
  group_by(month, member_casual) %>%
  summarize(average_duration = mean(ride_length_sec/60)) %>%
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Average Ride Duration by Membership Type (Month)",
       x = "", 
       y = "Ride Duration (Minutes)") +
  scale_fill_brewer(palette = "Dark2")
