################################################################################
# Load the Lynx Collar Data from the I&M Server and Save it Locally
#
# Author: McCrea Cobb <mccrea_cobb@fws.gov>
# Date created: 9/7/2018
################################################################################


#-------------------------------------------------------------------------------
## Update the flat.file on the server:

setwd("s:/InvMon/Biometrics/_Projects/Refuge/_RegionalProjects/Lynx/Scripts")
## Process all QFP location data
source("./Functions/batch.flat.gps.R")
batch.flat.gps(report.dir="../Data/RawData/GPSCollar/telonics/iridium_csv",
               save.file=TRUE,
               save.dir="../Data/DerivedData/GPSCollar/telonics",
               returnx=FALSE,
               pattern="Complete")
## Process all mortality notification data
source("./Functions/batch.flat.mort.R")
batch.flat.mort(report.dir="../Data/RawData/GPSCollar/telonics/iridium_csv",
                save.file=TRUE,
                save.dir="../Data/DerivedData/GPSCollar/telonics",
                returnx=FALSE,
                pattern="Complete")

#-------------------------------------------------------------------------------
## The Shiny app does not allow us to publish to shinyapps.io with a remote
## file path in the script. So, my workaround is to pull out the function for
## updating the data from the server and saving it locally. Then, the shiny script
## works from the local copy

setwd("k:/Shiny/borealLynx/borealLynx")


# Loads the updated flat.file from the I&M server and saves it locally:

dat <- "s:/InvMon/Biometrics/_Projects/Refuge/_RegionalProjects/Lynx/Data/DerivedData/GPSCollar/telonics/flat.gps.gzip"
dat <- get(load(dat)) ; rm(x)

dat.collar <- read.csv("s:/InvMon/Biometrics/_Projects/Refuge/_RegionalProjects/Lynx/Data/RawData/Capture/CaptureData.csv")


# Format the data:
dat$fixtime <- as.POSIXct(strptime(dat$fixtime, format = "%Y-%m-%d %H:%M"))
dat$date <- as.Date(dat$fixtime)
dat$month <- as.numeric(format(dat$date, "%m"))
dat$lon <- as.numeric(dat$lon)
dat$lat <- as.numeric(dat$lat)
dat$CTN <- dat$id
dat <- dat[complete.cases(dat$lat), ]   # Remove NA rows from lat/lon
i <- sapply(dat, is.factor)
dat[i] <- lapply(dat[i], as.character) ; rm(i)

dat.collar$DateCapture <- as.POSIXct(strptime(as.character(dat.collar$DateCapture), format = "%m/%d/%Y"))

dat <- merge(dat, dat.collar, by = "CTN") ; rm(dat.collar)

write.csv(dat, file="CollarData.csv")
