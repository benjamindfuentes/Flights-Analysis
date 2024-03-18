library(tidyverse)

glimpse(flights$flights)

glimpse(flights$airlines)

glimpse(flights$airports)

# Flight volume by airline
total_flights_summary <- flights$flights %>%
  group_by(airline) %>% # grouping by airline
  summarize(total_flights = n()) %>% # assigning total observations to total_flights
  arrange(desc(total_flights)) # rearranging in descending order by total flights

# Flight duration and total distance traveled by all airlines
total_flights_summary %>% 
  ggplot(aes(x = reorder(airline, 
                         rev(total_flights)), 
             y = total_flights, 
             fill = airline)) +
  geom_col() + # setting bar chart
  labs(title = "Total Flights by Airline", x = "Airline", y = "Total Flights") +
  theme(legend.position = "none") # removing legend

flights$flights %>%
  ggplot(aes(x = distance)) +
  geom_histogram(binwidth = 500, 
                 fill = "skyblue", 
                 color = "black") + 
  labs(title = "Flight Distance Overview", x = "Distance", y = "Frequency")

total_distance <- flights$flights %>% 
  group_by(airline) %>% # grouping by airline
  summarize(distance = sum(distance, na.rm = TRUE)) # new column calculating sum of distance, removing NA values

total_distance %>% 
  ggplot(aes(x = reorder(airline, -distance), y = distance, fill = airline)) + # arranging in descending order by total distance
  geom_col() +
  theme(legend.position = "none") + # removing legend from fill
  labs(title = "Total Distance Traveled by Airline", x = "Airline", y = "Distance")

max_arrival_delay <- flights$flights %>%
  group_by(airline) %>% # grouping by airline
  summarise(max_arrival_delay = max(arrival_delay, na.rm = TRUE)) # new column calculating max arrival delay, removing NA values

# Maximum flight arrival delay per airline
max_arrival_delay %>% 
  ggplot(aes(x = reorder(airline, max_arrival_delay), y = max_arrival_delay, fill = airline)) +
  geom_col() + # setting bar chart
  coord_flip() + # flip x and y coordinates so airlines is on y
  labs(title = "Longest Arrival Delays by Airline", x = "Airline", y = "Maximum Departure Delay (minutes)") +
  theme(legend.position = "none") # removing theme

# Percentage of total flights delayed by airline
flights_delayed_airline <- flights$flights %>%
  mutate(delayed = as.numeric(delayed)) %>% # converting delayed column to be treated as numeric for mean calc
  group_by(airline) %>% # group by airline
  summarise(percent_flights_delayed = mean(delayed, na.rm = TRUE) * 100) # new column calculating mean, removing NA values
  
flights_delayed_airline %>% 
  ggplot(aes(x = reorder(airline, percent_flights_delayed), y = percent_flights_delayed, fill = airline)) + # ordering in descending order with highest percentage at the top
  geom_bar(stat = "identity") + # displaying values in data frame as they are
  coord_flip() + # flip x and y coordinates so airlines is on y 
  labs(title = "Percentage of Flights Delayed by Airline", x = "Airline", y = "Percentage (%)") +
  theme(legend.position = "none") # removing legend

# Flights cancelled by each airline
flights_cancelled_summary <- flights$flights %>% 
  group_by(airline) %>% # grouping total count of delays by each airline
  summarize(total_cancelled = sum(cancelled, na.rm =TRUE)) # summing the count of delays and removing NA values 

flights_cancelled_summary %>% 
  ggplot(aes(x = reorder(airline, total_cancelled), y = total_cancelled, fill = airline)) + # reorder by airlines with highest cancelled flights
  geom_col() + 
  coord_flip() + # flip x and y coordinates so airlines is on y 
  theme(legend.position = "none") + # removing legend 
  labs(title = "Total Flights Cancelled by Airline", x = "Airline", y = "Total Flights Cancelled") # assigning title, x and y names

# Flights diverted by each airline
diverted_flights_summary <- flights$flights %>% 
  group_by(airline) %>% # grouping by airline
  summarize(total_diverted = sum(diverted, na.rm = TRUE)) # new column with sum of diverted flights, removing NA values

diverted_flights_summary %>% 
  ggplot(aes(x = reorder(airline, total_diverted), y = total_diverted, fill = airline)) + # setting x and y
  geom_col() +
  coord_flip() + # flipping coordinates to have Airline on y
  theme(legend.position = "none") + # removing legend
  labs(title = "Total Flights Diverted by Airline", x = "Airline", y = "Total Flights Diverted")

# Route diversity
unique_route <- flights$flights %>% 
  group_by(airline) %>% # grouping total distinct destinations by airline
  summarize(unique_route = n_distinct(destination_airport)) %>% # new column counting all distinct destinations
  arrange(desc(unique_route)) # arranged in descending order

unique_route %>% 
  ggplot(aes(x = reorder(airline, -unique_route), y = unique_route, fill = airline)) + # reordering descending (-) by unique routes
  geom_col() + 
  labs(title = "Route Diversity", x = "Airline", y = "Unique Routes") +
  theme(legend.position = "none") # removing legend

total_distance <- flights$flights %>% 
  group_by(airline) %>% # grouping by airline
  summarize(distance = sum(distance, na.rm = TRUE)) # creating new column storing sum of distance

# Hawaiian Airlines flight distance
total_distance <- flights$flights %>% 
  group_by(airline) %>% # grouping by airline
  summarize(distance = sum(distance, na.rm = TRUE)) # creating new column storing sum of distance

ua_total_distance <- total_distance %>% # creating new variable for UA to distinct its values
  mutate(ToHighlight = ifelse(airline == "HA", "yes", "no")) # creating new column to assign 'yes' to UA only

ua_total_distance %>% 
  ggplot(aes(x = reorder(airline, -distance), y = distance, fill = ToHighlight)) + # fill is based on ToHighlight values
  geom_col() + # bar graph
  theme(legend.position = "none") + # removing legend
  labs(title = "Total Distance Traveled by Hawaiian Airlines", x = "Airline", y = "Distance (miles)") + # assigning titles
  scale_fill_manual(values = c("yes" = "orange", "no" = "grey")) # only highlight airlines with 'yes' in ToHighlight

# Hawaiian Airlines flights cancelled
flights_cancelled_summary <- flights$flights %>% 
  group_by(airline) %>% # grouping total count of delays by each airline
  summarize(total_cancelled = sum(cancelled, na.rm =TRUE)) # summing the count of delays and removing NA values 

ua_flights_cancelled_summary <- flights_cancelled_summary %>% # creating new variable for UA to distinct its values
  mutate(ToHighlight = ifelse(airline == "HA", "yes", "no")) # creating new column to assign 'yes' to UA only

ua_flights_cancelled_summary %>% 
  ggplot(aes(x = reorder(airline, total_cancelled), y = total_cancelled, fill = ToHighlight)) + # reorder by airlines with highest cancelled flights
  geom_col() + # assigning bar chart
  coord_flip() + # flip x and y coordinates so airlines is on y 
  labs(title = "Total Flights Cancelled by Hawaiian Airlines", x = "Airline", y = "Total Flights Cancelled") +
  theme(legend.position = "none") + # removing legend
  scale_fill_manual(values = c("yes" = "orange", "no" = "grey")) # only highlight airlines with 'yes' in ToHighlight

# Hawaiian Airlines flights delayed
percent_delayed <- flights$flights %>%
  mutate(delayed = as.numeric(delayed)) %>% # converting delayed column to be treated as numeric for mean calc
  group_by(airline) %>% # grouping by airline
  summarise(percent_flights_delayed = mean(delayed, na.rm = TRUE) * 100) # creating new column storing percentage of flights delayed

ua_percent_delayed <- percent_delayed %>% # creating new variable for UA to distinct its values
  mutate(ToHighlight = ifelse(airline == "HA", "yes", "no")) # creating new column to assign 'yes' to UA only

ua_percent_delayed %>% 
  ggplot(aes(x = reorder(airline, percent_flights_delayed), y = percent_flights_delayed, fill = ToHighlight)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Percentage of Hawaiian Airlines Flights Delayed ", x = "Airline", y = "Percentage (%)") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("yes" = "orange", "no" = "grey")) # only highlight airlines with 'yes' in ToHighlight

# Hawaiian Airlines flight georgraphy
unique_route <- flights$flights %>% 
  group_by(airline) %>% # grouping total distinct destinations by airline
  summarize(unique_route = n_distinct(destination_airport)) %>% # counting all distinct destinations
  arrange(desc(unique_route)) # arranged in descending order
  
ua_unique_route <- unique_route %>% # creating new variable for UA to distinct its values
  mutate(ToHighlight = ifelse(airline == "HA", "yes", "no")) # creating new column to assign 'yes' to UA only

ua_unique_route %>% 
  ggplot(aes(x = reorder(airline, -unique_route), y = unique_route, fill = ToHighlight)) + # reordering descending (-) by unique routes
  geom_col() + 
  labs(title = "Hawaiian Airlines Route Diversity", x = "Airline", y = "Unique Routes") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("yes" = "orange", "no" = "grey")) # only highlight airlines with 'yes' in ToHighlight
