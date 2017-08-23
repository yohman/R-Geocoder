#load up the ggmap library
library(ggmap)

library(RJSONIO)
library(RCurl)

getGeoData <- function(location,key){
  print('in getgeodata function...')
  location <- gsub(' ','+',location)
  geo_data <- getURL(paste("https://maps.googleapis.com/maps/api/geocode/json?address=",location,"&key=",key, sep=""))
  geo_reply <- fromJSON(geo_data)
  
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, lng=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location['lat']
  answer$lng <- geo_reply$results[[1]]$geometry$location['lng']
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
  
}


#setting working directory 
setwd("/Users/yohkawano/Box Sync/IDRE/geocoder")

# keys
keys = c("AIzaSyCzlOtcG6jSWDCjSgNtu_SyS55lD6OVyiA","AIzaSyDoLPlrxrTOpULEmdE0A6m9h-X7kJs0IQE","AIzaSyDNd-drPbs8fATbZZfJVQudNxzcWZzKJOk","AIzaSyCDtc47lhYBCiivZmbuA-miRh-5jsVzR5k")

#loading in data 
infile <- "input"

data <- read.csv("CCPDnoTransients100.csv", header=TRUE, stringsAsFactors=FALSE)

data$lat <- NA
data$lng <- NA

cat("found ", nrow(data) , " addresses...")

startnum <- function()
{ 
  n <- readline(prompt="Start geocoding from record #: ")
  return(as.integer(n))
}

print(startnum())

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(startnum(), nrow(data))){
  if(ii > nrow(data))
  {
    print("No more records to geocode...")
  }
  else
  {
    print(paste("Working on address ", ii, "of", nrow(data)))
    address <- as.character(data[ii,2])
    print(address)
    #query the google geocoder - this will pause here if we are over the limit.
    
    result = getGeoData(address) 
    print(result)
    data$lat[ii] <- result$lat
    data$lng[ii] <- result$lng
    data$status[ii] <- result$status
    data$address_type[ii] <- result$address_type
    
    #result$index <- ii
    #append the answer to the results file.
    #geocoded <- rbind(geocoded, address)
    #geocoded <- rbind(geocoded, result)
    #save temporary results as we are going along
    #saveRDS(geocoded, tempfilename)
  }
}

print(data)
### Alternatively
write.csv(data, file = "foo.csv")
read.csv("foo.csv", row.names = 1)
