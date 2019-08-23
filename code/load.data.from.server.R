################################################################################
# Load the Lynx Collar Data from the I&M Server and Save it Locally            #
#                                                                              #
# Author: McCrea Cobb <mccrea_cobb@fws.gov>                                    #
# Date created: 9/7/2018                                                       #
# Date edited: 3/8/2019                                                        #
#                                                                              #
# Changed the output to a RData file in an attempt to remove all data clean up #
# functions from the Shiny app.                                                #
################################################################################


#-------------------------------------------------------------------------------
## Update the flat.file on the server
##    Make sure that the server drive is mapped first! (->FWS Tools to Go->Map Drives)

## Process all QFP location data

setwd("s:/InvMon/biometrics/_projects/refuge/_regional_projects/lynx/code")

source("./functions/batch.flat.gps.R")

batch.flat.gps(iridium_csv.dir="s:/im_archive/boreal_lynx/products/iridium_csv",
               save.file=TRUE,
               save.dir="f:/active_projects/shiny/lynx/data",
               returnx=FALSE,
               pattern="Complete")
## Process all mortality notification data
source("s:/InvMon/biometrics/_projects/refuge/_regional_projects/lynx/functions/batch.flat.mort.R")
batch.flat.mort(iridium_csv.dir="s:/im_archive/boreal_lynx/products/iridium_csv",
                save.file=TRUE,
                save.dir="s:/InvMon/biometrics/_projects/refuge/_regional_projects/lynx/data/derived_data/gps_collar/telonics",
                returnx=FALSE,
                pattern="Complete")

#-------------------------------------------------------------------------------
## The Shiny app does not allow us to publish to shinyapps.io with a remote
## file path in the script. So, my workaround is to pull out the function for
## updating the data from the server and saving it locally. Then, the shiny script
## works from the local copy



GetServerData <- function(){
  require(tidyverse)
  require(dplyr)
  # Loads the updated flat.file from the I&M server and saves it locally:
  
  # Set working directory:
  setwd("f:/active_projects/Shiny/lynx")
  

  message("Importing collar data from the server...")
  
  # Load the data:
  dat <- "s:/InvMon/biometrics/_projects/refuge/_regional_projects/lynx/data/derived_data/gps_collar/telonics/flat.gps.gzip"
  dat <- get(load(dat))
  dat.collar <- read.csv("s:/InvMon/biometrics/_projects/refuge/_regional_projects/lynx/data/raw_data/capture/capture_data.csv")
  dat.collar$id <- dat.collar$CTN
  
  message("Formatting data...")
  dat$date <- as.Date(dat$fixtime)
  dat$month <- as.numeric(format(dat$date, "%m"))
  dat <- dat[complete.cases(dat$lat), ]   # Remove NA rows from lat/lon
  
  # Make datecapture a POSIXct:
  dat.collar$datecapture <- as.POSIXct(strptime(as.character(dat.collar$DateCapture), format="%m/%d/%Y"))
  
  dat <- merge(dat, dat.collar, by="id") ; rm(dat.collar)
  
  # Convert all factors to characters:
  i <- sapply(dat, is.factor)
  dat[i] <- lapply(dat[i], as.character) ; rm(i)
  
  # Subset out columns not needed for the app to reduce file size:
  dat <- dat %>% dplyr::select(-c(utmzone, utmy, utmx, alt, fixtype, hdop, 
                                  nsats, step, angle, deploy.site, Primary, CTN,
                                  Aux1, Aux2, Aux3, Code, ID, DateCapture))
  
  # Remove duplicate fixes:
  dat <- dat %>% distinct(id, fixtime, .keep_all = T)
  
  # 700521A (KNI): remove fixes after it was trapped:
  dat <- dat %>% filter(!(id == "700521A" & date > as.Date("2018-10-28")))
  
  # 700552A (YF): remove fixes after it died:
  dat <- dat %>% filter(!(id == "700552A" & date > as.Date("2018-06-18")))
  
  # 700534A (Kanuti): remove fixes after it died (? or something):
  dat <- dat %>% filter(!(id == "700534A" & date > as.Date("2018-05-04")))
  
  # Save it:
  save(dat, file="./data/collar_data.RData")
  
  message("Done!")
  
}


GetServerData()
