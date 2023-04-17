# Air travel analysis
# We have a 1% sample of all air legs flown in Q2 2022. We will use this to derive
# basic information about air flows in the US.

# This data is extracted from the Bureau of Transportation Statistics DB1B dataset

library(tidyverse)

# first, we need to load the data
data = read_csv("data/air_sample.csv")

# I always like to look at a sample of my data to see what I'm dealing with
data[1:10,]

# The data have seven columns: origin and destination airport, origin and destination cities
# carrier, and distance. The city and carrier are coded, so we will merge in other 
# (the airports have codes as well, but these are fairly well known - e.g. RDU is
# Raleigh-Durham and LAX is Los Angeles; we won't match those with the official airport
# names)

market_ids = read_csv("data/L_CITY_MARKET_ID.csv")
data = left_join(data, rename(market_ids, OriginCity="Description"), by=c(OriginCityMarketID="Code"))
data = left_join(data, rename(market_ids, DestCity="Description"), by=c(DestCityMarketID="Code"))

carriers = read_csv("data/L_CARRIERS.csv")
data = left_join(data, rename(carriers, OperatingCarrierName="Description"), by=c(OpCarrier="Code"))
data = left_join(data, rename(carriers, TicketingCarrierName="Description"), by=c(TkCarrier="Code"))

# Now, we can see what the most popular air route is, by summing up the number of 
# passengers carried.
pairs = group_by(data, Origin, Dest) %>%
  summarize(Passengers=sum(Passengers), distance_km=first(Distance) * 1.609)
arrange(pairs, -Passengers)

# we see that LAX-JFK (Los Angeles to New York Kennedy) is represented separately
# from JFK-LAX. We'd like to combine these two. Create airport1 and airport2 fields
# with the first and second airport in alphabetical order.
pairs = mutate(
  pairs, airport1 = if_else(Origin < Dest, Origin, Dest),
  airport2 = if_else(Origin < Dest, Dest, Origin)
)

pairs = group_by(pairs, airport1, airport2) %>%
  summarize(Passengers=sum(Passengers), distance_km=first(distance_km))

arrange(pairs, -Passengers)

# This may be misleading, however, as some metropolitan areas have only one airport
# (for example, Raleigh-Durham or Las Vegas), while others have more (for example,
# New York or Los Angeles). We can repeat the analysis grouping by "market", which
# groups these airports together.
# Now, we can see what the most popular air route is, by summing up the number of 
# passengers carried.
pairs = group_by(data, OriginCity, DestCity) %>%
  summarize(Passengers=sum(Passengers), distance_km=first(Distance) * 1.609)

pairs = mutate(pairs,
               city1 = if_else(OriginCity < DestCity, OriginCity, DestCity),
               city2 = if_else(OriginCity < DestCity, DestCity, OriginCity))

pairs = group_by(pairs, city1, city2) %>%
  summarize(Passengers=sum(Passengers), distance_km=first(distance_km) * 1.609)
arrange(pairs, -Passengers)
