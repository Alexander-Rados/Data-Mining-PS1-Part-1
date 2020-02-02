library(tidyverse)
library(tidyr)
library(scales)

# creating a dataset specifically for Austin departures
aus_depart = filter(ABIA, Origin == "AUS")

# adding column of how early/late the flight was
ABIA = mutate(ABIA,
              gain = DepDelay - ArrDelay)

##### 
# Grouping section
#####

# By time of year
by_month = ABIA %>%
  group_by(Month) %>%
  summarise(
    count = n(),
    cancel = sum(Cancelled),
    percent_cancel = (cancel)/(count + cancel),
    avg_delay = mean(ArrDelay, na.rm = TRUE)
  )

# Plotting delays by time of year
ggplot(data = by_month) +
  geom_line(mapping = aes(x = Month, y = avg_delay)) +
  geom_point(mapping = aes(x = Month, y = avg_delay)) +
  xlab("Month") + ylab("Average Delay") +
  xlim(1, 12)

# By destination and getting number of cancelations
by_dest = aus_depart %>%
  group_by(Dest) %>%
  summarise(
    count = n(),
    cancel = sum(Cancelled)
  ) %>%
  filter(count>1000)

# By airline and getting number of cancelled flights
by_airline = ABIA %>%
  group_by(UniqueCarrier) %>%
  summarise(
    count = n(),
    cancel = sum(Cancelled),
    percent_cancel = (cancel)/(count + cancel),
    punctuality = mean(ArrDelay, na.rm = TRUE),
    gain = sum(gain, na.rm = TRUE),
    gain_per_flight = gain/count,
  ) %>%
  filter(count>1000)

# Plotting gain per flight by airline
ggplot(data = by_airline) +
  geom_bar(mapping = aes(x = reorder(UniqueCarrier, gain_per_flight), 
                         y = gain_per_flight), fill = "blue", stat = "identity") +
  xlab("Airline") + ylab("Airtime Efficiency") + 
  coord_flip()

# Plotting on average how delayed an airline is
ggplot(data = by_airline) +
  geom_bar(mapping = aes(x = reorder(UniqueCarrier, punctuality), 
                         y = punctuality), fill = "blue", stat = "identity") +
  xlab("Airline") + ylab("Average Delay") + 
  coord_flip()

# Plotting average delay by cancellations for airlines
ggplot(data = by_airline) + 
  geom_point(mapping = aes(x = punctuality, y = percent_cancel, color = UniqueCarrier))

# plotting cancellations by airline with a fill gradient of red and blue
ggplot(data = by_airline) +
  geom_bar(mapping = aes(x = reorder(UniqueCarrier, percent_cancel), 
                         y = percent_cancel, fill = count), stat = "identity") +
  xlab("Airline") + ylab("Number of Flights") + 
  scale_fill_gradient(low = "blue", high = "red", space = "Lab") +
  labs(fill = "Percent of Flights Cancelled") +
  coord_flip()

# plotting cancellations by airline without a fill gradient
ggplot(data = by_airline) +
  geom_bar(mapping = aes(x = reorder(UniqueCarrier, percent_cancel), 
                         y = percent_cancel, fill = count), stat = "identity") +
  xlab("Airline") + ylab("Percent of Flights Cancelled") + 
  labs(fill = "Number of Flights") +
  coord_flip()

ggplot(data = by_airline) +
  geom_bar(mapping = aes(x = reorder(UniqueCarrier, count), 
                         y = count, fill = percent_cancel), stat = "identity") +
  xlab("Airline") + ylab("Number of Flights") + 
  labs(fill = "Percent of Flights Cancelled") +
  coord_flip()

# creating a new column based on proportion of flights that weren't cancelled
# by destination
by_dest = mutate(by_dest,
       percent_flown = (count)/(count + cancel))    

# plotting flights and successful flights by destination
ggplot(data = by_dest) +
  geom_bar(mapping = aes(x = reorder(Dest, count), y = count, 
                         fill = percent_flown), stat = "identity") +
  xlab("Airport Destination") + ylab("Number of Flights") + 
  labs(fill = "Percent of Flights Not Cancelled") +
  coord_flip()

ggplot(data = by_dest) +
  geom_bar(mapping = aes(x = reorder(Dest, percent_flown), y = percent_flown, 
                         fill = count), stat = "identity") +
  xlab("Airport Destination") + ylab("Number of Flights") + 
  labs(fill = "Percent of Flights Not Cancelled") +
  coord_flip()

#####
# Attempt at using a map for this problem set
#####

# Creating a map of the US
usa = map_data(('state'), region = '.')

# Deleting unnecessary columns from airport codes
keeps = c("iata_code", "coordinates")
air_codes = airport.codes_csv[keeps]
air_codes

# Merging airport codes with ABIA data
ABIA_codes1 = ABIA_no_na %>% left_join(airport.codes_csv, by.x=c("Origin", "iata_code"), by.y =c("Dest", "iata_code"))
ABIA_codes = merge(airport.codes_csv, ABIA_no_na, 
                   sort = FALSE, by.x=c("Origin", "iata_code"), by.y =c("Dest", "iata_code"))
