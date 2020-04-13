
#' fetch_remote_data
#' 
#' @description A function to import, merge and clean gps collar and demographic data. The Lynx Shiny app does not allow us to publish to shinyapps.io with a remote file path in the script. So, my workaround is to pull out the function for updating the data from the server and saving it locally. Then, the shiny script works from the local copy
#'
#' @param flat.gps The file path to a gzip file containing the merged and formatted GPS collar data, output from \code{batch.flat.gps}.
#' @param dat.collar The file path to an Rdata file containing demographic data for each collared individual in flat.gps.
#' @param clean Clean the output? If \code{clean=TRUE}, it will remove fields not needed for shiny, remove some erroneous fixes, remove duplicate fields.
#' @param save Save the output? If \code{save=TRUE}, output will be saved to \code{savedir}.
#' @param savedir If \code{save=TRUE}, the directory path to the saved the output.
#' @param returnit If \code{return=TRUE}, the output be returned to the R environment.
#' 
#'
#' @return A dataframe of lynx GPS fixes merged with demographic info collected a capture. Some erroneous fixes are censored. Data are saved locally.
#' @export
#'
#' @examples fetch_remote_data(flat.gps = "s:/InvMon/biometrics/_projects/refuge/_regional_projects/lynx/data/derived_data/gps_collar/telonics/flat.gps.gzip", 
#'                         dat.collar = "s:/InvMon/biometrics/_projects/refuge/_regional_projects/lynx/data/raw_data/capture/capture_data.RData", 
#'                         clean = TRUE,
#'                         save = TRUE,
#'                         savedir = "./data/collar_data.RData",
#'                         returnit=FALSE)


fetch_remote_data <- function(flat.gps, dat.collar, clean, save, savedir, returnit){
  
  # required packages:
  require(tidyverse)
  require(dplyr)
  
  message("Importing collar data from the server...")
  
  # Load the data:
  dat <- get(load(flat.gps))
  dat.collar <- get(load(dat.collar))
  rm(x)
  
  # Reformat the telemetry data
  message("Formatting data...")
  
  dat.collar$id <- dat.collar %>%
    mutate(id = ctn) %>%
    mutate(site = c(rep("TET", 45), rep("WIS", 11), rep("KAN", 20), rep("YKF", 18), rep("KOY", 28)))  # Add a site variable to capture data (temporary fix)
  
  dat$date <- dat %>%
    mutate(date = as.Date(fixtime),
           month = as.numeric(format(date, "%m")))
  dat <- dat[complete.cases(dat$lat), ]   # Remove NA rows from lat/lon
  
  # Merge telemetry and capture datasets:
  dat <- merge(dat, dat.collar, by = "id", all.x = TRUE)
  rm(dat.collar)
  
  # Convert all factors to characters:
  i <- sapply(dat, is.factor)
  dat[i] <- lapply(dat[i], as.character) ; rm(i)
  
  if (clean == TRUE) {
    dat <- dat %>% 
      dplyr::select(-c(utmzone, utmy, utmx, alt, fixtype, hdop,  # Subset out columns not needed for the app to reduce file size:
                                    nsats, step, angle, deploy.site)) %>%
      distinct(id, fixtime, .keep_all = T) %>%  # Remove duplicate fixes
      filter(!(id == "700521A" & date > as.Date("2018-10-28")),      # Remove fixes after 700521A (KNI) was trapped
             id == "700552A" & date > as.Date("2018-06-18"),         # Remove fixes after 700552A (YF) died
             !(id == "700534A" & date > as.Date("2018-05-04"))) %>%  # Remove fixes after 700534A (Kanuti) died (? or went off air..)
      arrange(ctn, fixtime)
  }
  
  if (save == TRUE) {
    # Save it:
    save(dat, file = savedir)
  }
  
  message("Done!")
  
  if (returnit == TRUE) {
    return(dat)
  }
  
}
