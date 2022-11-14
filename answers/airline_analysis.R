# Here, we want to look at what airports are most dominated by which airlines,
# using the same data. For simplicity, we only look at departing flights. Since
# most departing flights have a corresponding return flight, this should be fairly
# accurate.

library(tidyverse)
source("load.R")

data = load_data("data/air_sample.csv", "data/L_CITY_MARKET_ID.csv", "data/L_CARRIERS.csv")

# Now, we can compute the market shares
market_shares = function (dataframe, carrier, city) {
  mkt_shares = group_by(data, {{ carrier }}, {{ city }}) %>%
    summarize(Passengers=sum(Passengers)) %>%
    group_by({{ city }}) %>%
    mutate(market_share=Passengers/sum(Passengers), total_passengers=sum(Passengers)) %>%
    ungroup()
  
  res = filter(mkt_shares, total_passengers > 1000) %>% arrange(-market_share)
  return(res)
}

market_shares(data, OperatingCarrierName, OriginCity)

# many of the smaller airlines actually operate regional aircraft for larger carriers
# For instance, PSA Airlines flies small aircraft for American Airlines, branded as
# American Eagle and sold with connections to/from American Airlines flights.
# Here, we repeat the analysis using the TicketingCarrierName instead of the
# OperatingCarrierName.

market_shares(data, TicketingCarrierName, OriginCity)

# American is much more dominant in Charlotte than before, for example

# and by origin airport
market_shares(data, TicketingCarrierName, Origin)
