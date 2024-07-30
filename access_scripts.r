# Now load the package
library(otpr)
library(dplyr)

#install.packages("opentripplanner") # Install Package
#library(opentripplanner)            # Load Package

#otpcon <- otp_connect(timezone = "US/Pacific")
#otpcon <- otp_connect(hostname =  "localhost",
#                     router = "default",
#                    port = 8080)

#route <- otp_plan(otpcon, 
#                 fromPlace = c(44.97317, -93.27156),
#                toPlace =   c(44.97357, -93.23516)
#)

# Call otpConnect() to define a connection called otpcon
otpcon <-
  otp_connect(
    hostname = "localhost",
    router = "default",
    port = 8080,
    #tz = "US/Chicago",
    ssl = FALSE
  )

# Call otp_get_times to get attributes of an itinerary
otp_get_times(
  otpcon,
  fromPlace = c(44.97317, -93.27156),
  toPlace =   c(44.97357, -93.23516),
  mode = 'TRANSIT',
  date = '06-11-2024',
  time = '23:00:00',
  maxWalkDistance = 1600, # allows nominal 800m at both ends of journey
  walkReluctance = 5,
  minTransferTime = 600, # allows at least 10 minutes (600 seconds) for transfers
  detail =TRUE
)

county_codes <- c(Hennepin=53, Ramsey=123, Dakota=37, Anoka=3, Washington=163, Scott=139, Carver=19)
bg_pop_centroid <- read.csv('CenPop2020_Mean_BG27.txt')
filtered_bg_pop_centroid <- bg_pop_centroid %>%
  filter(COUNTYFP %in% county_codes[c("Hennepin", "Ramsey")])

#twin cities bounding box
bounding_box <- c(-93.319453,44.887998,-92.998103,45.056582)
filtered_origins <- filtered_bg_pop_centroid %>%
  filter(
    LATITUDE >= bounding_box[2] & LATITUDE <= bounding_box[4] &
      LONGITUDE >= bounding_box[1] & LONGITUDE <= bounding_box[3]
  )
write.csv(filtered_origins, "filtered_origins.csv")
clinics <- read.csv("urgentcare_nodes.csv")

# WALK
travel_time_walk <- data.frame(COUNTYFP=double(),
                              TRACTCE=double(), 
                              BLKGRPCE=double(), 
                              fromLat=double(), 
                              fromLon=double(),
                              dest=double(),
                              toLat=double(), 
                              toLon=double(), 
                              travel_time=double())
for (i in 1:nrow(filtered_origins)) {
  for (j in 1:nrow(clinics)) {
    origin <- filtered_origins[i, ]
    fromPlace <- c(unlist(origin["LATITUDE"]), unlist(origin["LONGITUDE"]))
    dest <- clinics[j, ]
    toPlace <- c(unlist(dest["lat"]), unlist(dest["lon"]))
    
    response <- otp_get_times(
      otpcon,
      fromPlace = fromPlace,
      toPlace =   toPlace,
      mode = 'WALK',
      date = '06-11-2024',
      time = '15:00:00',
      maxWalkDistance = 10000000 #arbitrarily large number for car journeys 
      #maxWalkDistance = 1600, # allows nominal 800m at both ends of journey
      #walkReluctance = 5,
      #minTransferTime = 600 # allows at least 10 minutes (600 seconds) for transfers
    )
    
    tryCatch(
      {travel_time_walk[nrow(travel_time_walk) + 1,] = c(origin$COUNTYFP, 
                                                       origin$TRACTCE, 
                                                       origin$BLKGRPCE, 
                                                       origin$LATITUDE, 
                                                       origin$LONGITUDE, 
                                                       dest$id, 
                                                       dest$lat, 
                                                       dest$lon, 
                                                       response$duration)
      },
      error = function(e) {
        print(response$errorMessage)
        duration <- 99999
        travel_time_walk[nrow(travel_time_walk) + 1,] = c(origin$COUNTYFP, 
                                                        origin$TRACTCE, 
                                                        origin$BLKGRPCE, 
                                                        origin$LATITUDE, 
                                                        origin$LONGITUDE, 
                                                        dest$id, 
                                                        dest$lat, 
                                                        dest$lon, 
                                                        duration)
        print(travel_time_walk[nrow(travel_time_walk), ])
      }
    )
  }
}
write.csv(travel_time_walk, "travel_time_walk.csv")

# CAR
travel_time_car <- data.frame(COUNTYFP=double(),
                               TRACTCE=double(), 
                               BLKGRPCE=double(), 
                               fromLat=double(), 
                               fromLon=double(),
                               dest=double(),
                               toLat=double(), 
                               toLon=double(), 
                               travel_time=double())

for (i in 1:nrow(filtered_origins)) {
  for (j in 1:nrow(clinics)) {
    origin <- filtered_origins[i, ]
    fromPlace <- c(unlist(origin["LATITUDE"]), unlist(origin["LONGITUDE"]))
    dest <- clinics[j, ]
    toPlace <- c(unlist(dest["lat"]), unlist(dest["lon"]))
    
    response <- otp_get_times(
      otpcon,
      fromPlace = fromPlace,
      toPlace =   toPlace,
      mode = 'CAR',
      date = '06-11-2024',
      time = '15:00:00',
      maxWalkDistance = 10000000 #arbitrarily large number for car journeys 
      #maxWalkDistance = 1600, # allows nominal 800m at both ends of journey
      #walkReluctance = 5,
      #minTransferTime = 600 # allows at least 10 minutes (600 seconds) for transfers
    )
    
    tryCatch(
      {travel_time_car[nrow(travel_time_car) + 1,] = c(origin$COUNTYFP, 
                                                         origin$TRACTCE, 
                                                         origin$BLKGRPCE, 
                                                         origin$LATITUDE, 
                                                         origin$LONGITUDE, 
                                                         dest$id, 
                                                         dest$lat, 
                                                         dest$lon, 
                                                         response$duration)
      },
      error = function(e) {
        print(response$errorMessage)
        duration <- 99999
        travel_time_car[nrow(travel_time_car) + 1,] = c(origin$COUNTYFP, 
                                                          origin$TRACTCE, 
                                                          origin$BLKGRPCE, 
                                                          origin$LATITUDE, 
                                                          origin$LONGITUDE, 
                                                          dest$id, 
                                                          dest$lat, 
                                                          dest$lon, 
                                                          duration)
        print(travel_time_car[nrow(travel_time_car), ])
      },
      finally = {
        print(travel_time_car[nrow(travel_time_car), ] )
      }
    )
  }
}
write.csv(travel_time_car, "travel_time_car.csv")



# TRANSIT
travel_time_transit <- data.frame(COUNTYFP=double(),
                               TRACTCE=double(), 
                               BLKGRPCE=double(), 
                               fromLat=double(), 
                               fromLon=double(),
                               dest=double(),
                               toLat=double(), 
                               toLon=double(), 
                               travel_time=double())
for (i in 1:nrow(filtered_origins)) {
  for (j in 1:nrow(clinics)) {
    origin <- filtered_origins[i, ]
    fromPlace <- c(unlist(origin["LATITUDE"]), unlist(origin["LONGITUDE"]))
    dest <- clinics[j, ]
    toPlace <- c(unlist(dest["lat"]), unlist(dest["lon"]))
    
    response <- otp_get_times(
      otpcon,
      fromPlace = fromPlace,
      toPlace =   toPlace,
      mode = 'TRANSIT',
      date = '06-11-2024',
      time = '15:00:00',
      #maxWalkDistance = 10000000 #arbitrarily large number for car journeys 
      maxWalkDistance = 1600, # allows nominal 800m at both ends of journey
      walkReluctance = 5,
      minTransferTime = 600 # allows at least 10 minutes (600 seconds) for transfers
    )
    
    tryCatch(
      {travel_time_transit[nrow(travel_time_transit) + 1,] = c(origin$COUNTYFP, 
                                                         origin$TRACTCE, 
                                                         origin$BLKGRPCE, 
                                                         origin$LATITUDE, 
                                                         origin$LONGITUDE, 
                                                         dest$id, 
                                                         dest$lat, 
                                                         dest$lon, 
                                                         response$duration)
      },
      error = function(e) {
        print(response$errorMessage)
        duration <- 99999
        travel_time_transit[nrow(travel_time_transit) + 1,] = c(origin$COUNTYFP, 
                                                          origin$TRACTCE, 
                                                          origin$BLKGRPCE, 
                                                          origin$LATITUDE, 
                                                          origin$LONGITUDE, 
                                                          dest$id, 
                                                          dest$lat, 
                                                          dest$lon, 
                                                          duration)
        print(travel_time_transit[nrow(travel_time_transit), ])
      },
      finally = {
        print(travel_time_transit[nrow(travel_time_transit), ] )
      }
    )
  }
}

write.csv(travel_time_transit, "travel_time_transit.csv")
