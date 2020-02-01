library(tidyverse)
library(tidyr)

# creating a dataset specifically for Austin departures
aus_depart = filter(ABIA, Origin == "AUS")

# grouping by destination and getting number of cancelled flights
# figuring out portion of flights cancelled by destination
by_dest = aus_depart %>%
  group_by(Dest) %>%
  summarise(
    count = n(),
    cancel = sum(Cancelled)
  ) %>%
  filter(count>1000)

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

# creating a map of the US
usa = map_data(('state'), region = '.')


# deleting unnecessary columns from airport codes
keeps = c("iata_code", "coordinates")
air_codes = airport.codes_csv[keeps]
air_codes

# merging airport codes with ABIA data
ABIA_codes1 = ABIA_no_na %>% left_join(airport.codes_csv, by.x=c("Origin", "iata_code"), by.y =c("Dest", "iata_code"))
ABIA_codes = merge(airport.codes_csv, ABIA_no_na, 
                   sort = FALSE, by.x=c("Origin", "iata_code"), by.y =c("Dest", "iata_code"))

# dropping all the flights that don't have any delays and putting it into a new
# data frame titled ABIA_no_na
ABIA_no_na = drop_na(ABIA)

# filtering out all data that doesn't have a carrier delay or weather delay
car_del = filter(ABIA_no_na, CarrierDelay > 0)
iata_codes = filter(airport.codes_csv, iata_codes)
weather_del = filter(ABIA_no_na, WeatherDelay > 0)

# plotting weather delayed flights on departure time while taking into account month
ggplot(data = weather_del) + 
  geom_point(mapping = aes(x = CRSDepTime, y = WeatherDelay)) +
  facet_wrap(~ Month)

ggplot(data = car_del) + 
  geom_point(mapping = aes(x = CRSDepTime, y = CarrierDelay, color = Dest)) +
  facet_wrap(~ Month)

ggplot(data = ABIA_no_na) +
  geom_point(mapping = aes(x = CRSDepTime, y = CarrierDelay), position = "jitter")

ggplot(data = ABIA_no_na) +
  geom_point(mapping = aes(x = CRSDepTime, y = WeatherDelay))

ggplot(data = ABIA_no_na) +
  geom_point(mapping = aes(x = CRSDepTime, y = LateAircraftDelay))

ggplot(data = ABIA_no_na) +
  geom_point(mapping = aes(x = CRSDepTime, y = LateAircraftDelay, color = AirTime))

ggplot(data = ABIA_no_na) +
  geom_point(mapping = aes(x = CRSDepTime, y = WeatherDelay)) +
  facet_wrap(~ Month)
