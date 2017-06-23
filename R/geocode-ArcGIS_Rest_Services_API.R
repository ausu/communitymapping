# Geocode with ArcGIS Rest Services API
# Modified from: http://www.rpubs.com/cengel248/177198
# Created: June 23, 2017

# New Jersey State: http://geodata.state.nj.us/arcgis/rest/services/Tasks
# Info: https://njgin.state.nj.us/NJ_NJGINExplorer/jviewer.jsp?pg=Address

# New York State: http://gisservices.dhses.ny.gov/arcgis/rest/services/Locators
# Info: http://gis.ny.gov/gisdata/inventories/details.cfm?DSID=1278

# NJ State
geocodeNJ <- function(address) {
  # Load libraries
  require(httr)
  require(jsonlite)
  
  # Service URL: Composite locator service that will geocode street addresses in NJ. (includes subaddresses and intersections). The locator will attempt to match against subaddress points, address points and road centerline ranges. Addresses are sourced from the NJ Road Centerlines data model and the NJ Address Point data model. Date of last update: 6/5/2017
  gserver <-
    "http://geodata.state.nj.us/arcgis/rest/services/Tasks/Addr_NJ_cascade/GeocodeServer/"
  
  # Single Line Format
  open <- '{"records":[{"attributes":{"OBJECTID":1,"SingleLine":"'
  close <- '"}}]}'
  
  # Generate a Valid URL
  url <- URLencode(
    paste0(
      gserver,
      "geocodeAddresses?addresses=",
      open,
      address,
      close,
      "&outSR=4269",
      "&f=pjson"
    )
  )
  
  # Submit request
  request <- GET(url)
  
  # Parse JSON for Content
  results <- content(request, "parsed", "application/json")
  
  # Results
  resultsDF <- with(results$locations[[1]], {
    data.frame(
      matchAdr = attributes$Match_addr,
      status = attributes$Status,
      score = attributes$Score,
      loc = attributes$Loc_name,
      lat = location$y,
      lon = location$x,
      yCoord = attributes$Y,
      xCoord = attributes$X,
      side = attributes$Side,
      county = attributes$County_fld,
      muni = attributes$Muni_fld
    )
  })
  
  return(resultsDF)
  
}

#njA1 <- nj[1, c("SingleLine")]
#njSub <- nj[1:5,]

nj <- read.csv(file.choose(), header = T, stringsAsFactors = F)
nj$Zip <- stringr::str_pad(as.character(nj$Zip.Code), 5, "left", 0)
nj$SingleLine <- with(nj, paste(Street, City, State, Zip, sep = ", "))

njCoords <-
  do.call("rbind",
          sapply(nj$SingleLine, function(x)
            geocodeNJ(x), simplify = FALSE))


# NY State
geocodeNY <- function(address) {
  # Load libraries
  require(httr)
  require(jsonlite)
  
  # Service URL: The first composite locator (Street_and_Address_Composite) is made up of the following set of locators [1A_SAM_AP_ZipName, 1B_SAM_AP_CTName, 1C_SAM_AP_PlaceName, 3A_SS_ZipName, 3B_SS_CTName, 3C_SS_PlaceName] which are most likely to return a high quality hit. The locators are listed in the order in which they will be accessed along with a brief description of the locator's source data. These six locators will generate the majority of the results when geocoding addresses.
  gserver <-
    "http://gisservices.dhses.ny.gov/arcgis/rest/services/Locators/Street_and_Address_Composite/GeocodeServer/"
  
  # Single Line Format
  open <- '{"records":[{"attributes":{"OBJECTID":1,"SingleLine":"'
  close <- '"}}]}'
  
  # Generate a Valid URL
  url <- URLencode(
    paste0(
      gserver,
      "geocodeAddresses?addresses=",
      open,
      address,
      close,
      "&outSR=4269",
      "&f=pjson"
    )
  )
  
  # Submit request
  request <- GET(url)
  
  # Parse JSON for Content
  results <- content(request, "parsed", "application/json")

  
  # Results
    resultsDF <- with(results$locations[[1]], {
    data.frame(
      lat = location$y,
      lon = location$x,
      matchAdr = attributes$Match_addr,
      status = attributes$Status,
      score = attributes$Score,
      loc = attributes$Loc_name
    )
  })

  return(resultsDF)
  
}

ny <- read.csv(file.choose(), header = T, stringsAsFactors = F)
ny$SingleLine <- with(ny, paste(Street, City, State, Zip.Code, sep = ", "))

nyCoords <-
  do.call("rbind",
          sapply(ny$SingleLine, function(x)
            geocodeNY(x), simplify = FALSE))
