# Here, we want to look at what markets are most dominated by which airlines,
# using the same data

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
data = left_join(data, rename(carriers, CarrierName="Description"), by=c(OpCarrier="Code"))

mkt_shares = group_by(data, CarrierName, OriginCity) %>%
  summarize(Passengers=sum(Passengers)) %>%
  group_by(OriginCity) %>%
  mutate(market_share=Passengers/sum(Passengers), total_passengers=sum(Passengers)) %>%
  ungroup()

filter(mkt_shares, total_passengers > 1000) %>% arrange(-market_share)
