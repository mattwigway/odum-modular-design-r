# Air travel analysis
# We have a 1% sample of all air legs flown in Q2 2022. We will use this to derive
# basic information about air flows in the US.

# This data is extracted from the Bureau of Transportation Statistics DB1B dataset

library(tidyverse)
source("load.R")

KILOMETERS_PER_MILE = 1.609

data = load_data("data/air_sample.csv", "data/L_CITY_MARKET_ID.csv", "data/L_CARRIERS.csv")

# Now, we can see what the most popular air route is, by summing up the number of 
# passengers carried.
busiest_routes = function (dataframe, origcol, destcol) {
  stopifnot(all(dataframe$Passengers >= 1))
  stopifnot(all(!is.na(dataframe$Passengers)))
  pairs = group_by(dataframe, {{ origcol }}, {{ destcol }}) %>% summarize(Passengers=sum(Passengers), distance_km=first(Distance) * KILOMETERS_PER_MILE)
  arrange(pairs, -Passengers)
  
  # we see that LAX-JFK (Los Angeles to New York Kennedy) is represented separately
  # from JFK-LAX. We'd like to combine these two. Create airport1 and airport2 fields
  # with the first and second airport in alphabetical order.
  pairs = mutate(pairs, airport1 = if_else({{ origcol }} < {{ destcol }}, {{ origcol }}, {{ destcol }}), airport2 = if_else({{ origcol }} < {{ destcol }}, {{ destcol }}, {{ origcol }}))
  pairs = group_by(pairs, airport1, airport2) %>% summarize(Passengers=sum(Passengers), distance_km=first(distance_km))
  return(arrange(pairs, -Passengers))
}

busiest_routes(data, Origin, Dest)

# This may be misleading, however, as some metropolitan areas have only one airport
# (for example, Raleigh-Durham or Las Vegas), while others have more (for example,
# New York or Los Angeles). We can repeat the analysis grouping by "market", which
# groups these airports together.
# Now, we can see what the most popular air route is, by summing up the number of 
# passengers carried.
busiest_routes(data, OriginCity, DestCity)

