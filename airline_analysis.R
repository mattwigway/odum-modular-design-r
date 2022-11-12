# Here, we want to look at what airports are most dominated by which airlines,
# using the same data. For simplicity, we only look at departing flights. Since
# most departing flights have a corresponding return flight, this should be fairly
# accurate.

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

# Now, we can compute the market shares

mkt_shares = group_by(data, OperatingCarrierName, OriginCity) %>%
  summarize(Passengers=sum(Passengers)) %>%
  group_by(OriginCity) %>%
  mutate(market_share=Passengers/sum(Passengers), total_passengers=sum(Passengers)) %>%
  ungroup()

filter(mkt_shares, total_passengers > 1000) %>% arrange(-market_share)

# many of the smaller airlines actually operate regional aircraft for larger carriers
# For instance, PSA Airlines flies small aircraft for American Airlines, branded as
# American Eagle and sold with connections to/from American Airlines flights.
# Here, we repeat the analysis using the TicketingCarrierName instead of the
# OperatingCarrierName.

ticketing_mkt_shares = group_by(data, TicketingCarrierName, OriginCity) %>%
  summarize(Passengers=sum(Passengers)) %>%
  group_by(OriginCity) %>%
  mutate(market_share=Passengers/sum(Passengers), total_passengers=sum(Passengers)) %>%
  ungroup()

filter(ticketing_mkt_shares, total_passengers > 1000) %>% arrange(-market_share)

# American is much more dominant in Charlotte than before, for example