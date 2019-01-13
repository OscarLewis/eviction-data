library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(stringr)
data("county.fips")
counties <- map_data("county")
fips <- county.fips

# Create the citation needed to use Eviction Lab data
citation <- "This research uses data from The Eviction Lab at Princeton University, 
      a project directed by Matthew Desmond and designed by Ashley Gromis, 
      Lavar Edmonds, James Hendrickson, Katie Krywokulski, Lillian Leung, 
      and Adam Porton. The Eviction Lab is funded by the JPB, Gates, 
      and Ford Foundations as well as the Chan Zuckerberg Initiative. 
      More information is found at evictionlab.org."

# Define the variables as listed in the provided data dictionary
data_dictionary <- list(
  "Poverty Rate" = "poverty.rate",
  "Percent of Renter Occupied" = "pct.renter.occupied",
  "Median Gross Rent" = "median.gross.rent",
  "Median Household Income" = "median.household.income",
  "Median Property Value" = "median.property.value",
  "Rent Burden" = "rent.burden",
  "Renter Occupied Households " = "renter.occupied.households",
  "Evictions" = "evictions",
  "Eviction Filings" = "eviction.filings",
  "Eviction Rate" = "eviction.rate",
  "Eviction Filing Rate" = "eviction.filing.rate"
)

# Read in the data
eviction_counties <- read.csv("data/evictionlab-us-counties.csv",
  stringsAsFactors = F
)

# Add polynames to the counties df
counties <- counties %>%
  mutate("polyname" = paste0(region, ",", subregion))

# Reduce down counties with multiple fips entires (ie "San Juan:orcas island")
# to just one
fips$polyname <- sub(":.*", "", fips$polyname)
fips <- unique(fips)

# Join fips onto counties
counties <- left_join(counties, fips)

# Get the most popular item in a set of data
get_mode <- function(vector){
  suppressWarnings(
    names(table(vector))[table(vector)==max(table(vector))]
  )
}

# Gets the county that encloses the passed in latitude and longitude (numeric),
# and is inside the passed in state (a string)
get_county <- function(lat, long, state){
  # Filter down counties to counties inside the relevant state
  state_counties <- counties %>% 
    filter(region == tolower(state))
  
  # Add a county_group id to the counties df
  state_counties$county_group <- rep(1:4, each = 4,len = nrow(state_counties))
  
  # Use each entries county_group id to create ranges of lattitude within each
  # county
  sum <- state_counties %>% 
    group_by(region, subregion, county_group) %>%
    arrange(desc(lat)) %>% 
    summarize("high_long" = max(long), "high_lat" = max(lat),
              "low_long" = min(long), "low_lat" = min(lat)) %>% 
    arrange(region, subregion, desc(high_lat))
  
  if(!is.null(lat) | !is.null(long)) {
    # Get the indexes of counties that contain the longitude
    # in their range of longitudes
    long_index <- which(sum$low_long < long
                        & long < sum$high_long)
    
    # Get the names of those counties
    long_counties <- sum[long_index, ]$subregion
    
    # Get the indexes of counties that contain the latitude
    # in their range of latitudes
    lat_index <- which(sum$low_lat < lat 
                       & lat < sum$high_lat)
    
    # Get the names of those counties
    lat_counties <- sum[lat_index, ]$subregion
    
    # Find the counties that exist in both lists
    matches <- intersect(long_counties, lat_counties)

    # Extrat each of those matches
    matches_vector <- c(long_counties[which(long_counties %in% matches)],
                        lat_counties[which(lat_counties %in% matches)])
    
    # Get the mode of those matches, this is the county the latitude
    # and longitude reside in
    mode <- get_mode(matches_vector)
    # If that mode is equal to two (2nd most common length besides 1, anything
    # above 2 is a outlier and does not need to be accounted for), break the tie
    # by which county has the closest longitude to the passed in value
    if(length(mode) == 2) {
      county1 <- state_counties %>% filter(subregion == mode[1])
      county2 <- state_counties %>% filter(subregion == mode[2])
      
      min_long_1 <- county1[which.min(abs(county1$long - long)), ]$long
      min_long_2 <- county2[which.min(abs(county2$long - long)), ]$long
      if(min_long_1 > min_long_2) {
        mode[1]
      } else {
        mode[2]
      }
    } else {
      # Return the found county
     mode
    }
    
  } else {
    print('This is not working')
  }
}
