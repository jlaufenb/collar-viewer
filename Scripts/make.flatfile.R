
## Ignore - something I have to do for Emacs
##.libPaths("C:/Users/jlaufenberg/Documents/R/win-library/3.4")
##.libPaths("C:/Users/jlaufenberg/Documents/R/win-library/3.3")

setwd("./Scripts")
source("./Funcs/flat.format.R")
source("./Funcs/batch.flatfile.R")
## load collar schedule reference data
scheds <- read.csv("../Data/GPS/Telonics/FixSchedules.csv")
collprogs <- read.csv("../Data/GPS/Telonics/TelonicsCollarPrograms.csv")
colnames(collprogs) <- c("CTN","Primary","Auxiliary 1","Auxiliary 2","Auxiliary 3","Code")
collprogs[,2:5] <- lapply(collprogs[,2:5], factor, levels(scheds$Schedule))
## list LiNC/metOcean accounts
accounts <- c("TetlinWiseman", "Kanuti", "YukonFlatsKNI")

## batch process and aggregate Condensed.csv files for all GPS collars

########################################
## McCrea, take a look at this object ##
########################################
CollarData <- batch.flatfile(accounts)
str(CollarData)

save(CollarData, file = "../OutputData/CollarData.RData")
write.csv(tmp, file = "../OutputData/CollarData.csv")

CollarData <- read.csv("./OutputData/CollarData.csv", header=T)
CollarPrograms <- read.csv("./Data/GPS/Telonics/TelonicsCollarPrograms.csv")
