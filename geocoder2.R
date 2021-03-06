####loading in packages and data####
library(ggmap)

#setting working directory 
setwd("/Users/yohkawano/Box Sync/IDRE/geocoder")

#loading in data 
tourist_dest<-read.csv("tourist_destinations_forR.csv")
sdata<-read.csv("sampledata_forR.csv")
data<-read.csv("CCPDnoTransients10.csv")


####creating a addresses from tourist list####
infile <- "input"
addresses<-as.character(data$FullAddress)#making sure that addresses are treated as strings or as characters
print(addresses)

#define a function that will process Google's server responses for us.
getGeoDetails <- function(address){   
  #use the gecode function to query Google servers
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
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
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}

#initialize a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1
#if a temp file exists - load it up and count the rows!
tempfilename <- paste0(infile, '_temp_geocoded_ccpd.rds')
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocoded <- readRDS(tempfilename)
  startindex <- nrow(geocoded)
  print(startindex)
}

startindex <- 1

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(startindex, length(addresses))){
  print(paste("Working on index", ii, "of", length(addresses)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getGeoDetails(addresses[ii]) 
  print(result$status)     

  result$ID <- data[ii,1]
  #result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(geocoded, result)
  #save temporary results as we are going along
  saveRDS(geocoded, tempfilename)
}

# Checking out what our results

print(data)
print(geocoded)

#merging back together
data2 <- merge(data, geocoded, by =("ID"), all=F)

#quick maps
##making map based on Google map of Los Angeles
map_LA<- get_map(location='Los Angeles', zoom=12, maptype = "terrain", source='google',color='color') #might have to alter zoom 
ggmap(map_LA)+ geom_point(aes(x=long, y=lat), data=data2, na.rm = T)#plotting points from Sdata 


####saving data frame as a csv####
write.csv(data2, file = "ccpd_geocoded.csv")
