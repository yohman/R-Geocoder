#####################
#					#
#	Setup			#
#					#
#####################

#load up the ggmap library
library(ggmap)
library(RJSONIO)
library(RCurl)


# setting working directory 
setwd("/Users/yohkawano/Box Sync/IDRE/geocoder")

# keys
keys <- c("AIzaSyCzlOtcG6jSWDCjSgNtu_SyS55lD6OVyiA","AIzaSyDoLPlrxrTOpULEmdE0A6m9h-X7kJs0IQE","AIzaSyDNd-drPbs8fATbZZfJVQudNxzcWZzKJOk","AIzaSyCDtc47lhYBCiivZmbuA-miRh-5jsVzR5k")
keynum <- 1


#####################
#					#
#	Data			#
#					#
#####################

# get the data
datatogeocode <- read.csv("CCPDnoTransients.csv", header=TRUE, stringsAsFactors=FALSE)

# start row

startrow <- 1

# how many found
cat("Found ", nrow(datatogeocode) , " addresses...")

logmsg <- paste(Sys.time(),": Found ", nrow(datatogeocode) , " addresses...")
write.table(logmsg, file = "log.csv", sep = ",", append = TRUE, quote = TRUE,col.names = FALSE, row.names = FALSE)


#####################
#					#
#	Calling Google	#
#					#
#####################

getGeoData <- function(location)
{

	# cat('Sending ',location,' to google...')
	location <- gsub(' ','+',location)

	# send request to google
	geo_data <- getURL(paste("https://maps.googleapis.com/maps/api/geocode/json?address=",location,"&key=",keys[keynum], sep=""))

	# convert json reply to R readable format
	geo_reply <- fromJSON(geo_data)

	# use a new key if quota is reached
	if(geo_reply$status == "OVER_QUERY_LIMIT")
	{
		logmsg <- paste(Sys.time(),": ",geo_reply$status," (",location,")")
		write.table(logmsg, file = "log.csv", sep = ",", append = TRUE, quote = TRUE,col.names = FALSE, row.names = FALSE)

		keynum <<- keynum + 1

		if(keynum > length(keys))
		{
			cat("All ",length(keys)," keys used. Unable to geocode any more records...")
			write.table("All keys used up...", file = "log.csv", sep = ",", append = TRUE, quote = TRUE,col.names = FALSE, row.names = FALSE)

		}
		else
		{
			msg <- paste("...Query limit reached for key #: ",keynum,"\n")
			print(msg)

			write.table("Using next key...", file = "log.csv", sep = ",", append = TRUE, quote = TRUE,col.names = FALSE, row.names = FALSE)
			# (re)send request to google
			geo_data <- getURL(paste("https://maps.googleapis.com/maps/api/geocode/json?address=",location,"&key=",keys[keynum], sep=""))

			# convert json reply to R readable format
			geo_reply <- fromJSON(geo_data)			
		}
	}

	#now extract the bits that we need from the returned list
	answer <- data.frame(lat=NA, lng=NA,formatted_address=NA, address_type=NA, status=NA)
	answer$status <- geo_reply$status

	#return Na's if we didn't get a match:
	if (geo_reply$status != "OK")
	{
		logmsg <- paste(Sys.time(),": ",geo_reply$status," (",location,")")
		write.table(logmsg, file = "log.csv", sep = ",", append = TRUE, quote = TRUE,col.names = FALSE, row.names = FALSE)

		return(answer)
	}
	#else, extract what we need from the Google server reply into a dataframe:
	answer$lat <- geo_reply$results[[1]]$geometry$location['lat']
	answer$lng <- geo_reply$results[[1]]$geometry$location['lng']
	answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
	answer$formatted_address <- geo_reply$results[[1]]$formatted_address

	return(answer)
  
}

#####################
#					#
#	Geocode			#
#					#
#####################

geocode <- function(startrow)
{

	for (ii in seq(startrow, nrow(datatogeocode)))
	{
		if(ii > nrow(datatogeocode))
		{
			print("No more records to geocode...")
		}
		else
		{
			print(paste("Working on address ", ii, "of", nrow(datatogeocode)))
			address <- as.character(datatogeocode[ii,2])
			print(address)
			#query the google geocoder - this will pause here if we are over the limit.

			result = getGeoData(address) 
			# print(result)
			cat(result$lat,",",result$lng,"\n")

			# add original fields to the result data frame
			result$ID <- datatogeocode$ID[ii]
			result$FullAddress <- datatogeocode$FullAddress[ii]

			result <- result[c("ID","FullAddress","formatted_address","lat","lng","address_type","status")]

			write.table(result, file = "geocoded_test.csv", sep = ",", append = TRUE, quote = TRUE,col.names = FALSE, row.names = FALSE)
			#result$index <- ii
			#append the answer to the results file.
			#geocoded <- rbind(geocoded, address)
			#geocoded <- rbind(geocoded, result)
			#save temporary results as we are going along
			#saveRDS(geocoded, tempfilename)
		}
	}

}

geocode(startrow)



#####################
#					#
#	End...			#
#					#
#####################

logmsg <- paste(Sys.time(),": end... ")
write.table(logmsg, file = "log.csv", sep = ",", append = TRUE, quote = TRUE,col.names = FALSE, row.names = FALSE)

