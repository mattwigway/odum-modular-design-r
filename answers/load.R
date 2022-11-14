load_data = function (mainfile, market, carrier) {
  # first, we need to load the data
  data = read_csv(mainfile)
  
  # The data have seven columns: origin and destination airport, origin and destination cities
  # carrier, and distance. The city and carrier are coded, so we will merge in other 
  # (the airports have codes as well, but these are fairly well known - e.g. RDU is
  # Raleigh-Durham and LAX is Los Angeles; we won't match those with the official airport
  # names)
  
  market_ids = read_csv(market)
  data = left_join(data, rename(market_ids, OriginCity="Description"), by=c(OriginCityMarketID="Code"))
  data = left_join(data, rename(market_ids, DestCity="Description"), by=c(DestCityMarketID="Code"))
  
  carriers = read_csv(carrier)
  data = left_join(data, rename(carriers, OperatingCarrierName="Description"), by=c(OpCarrier="Code"))
  data = left_join(data, rename(carriers, TicketingCarrierName="Description"), by=c(TkCarrier="Code"))  
  return(data)
}