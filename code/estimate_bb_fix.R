load("./data/collar_dat_demo.Rdata")

dat.sub <- dat %>%
  filter(id %in% c("KL009", "KL010"))  # KL010 is the problem


df <- xy_conv(dat.sub)  # Adds projected x and y coordinates to the df used for distance and speed calculations 

dat.move <- df %>%
  arrange(fixtime) %>%
  group_by(id) %>%
  mutate(Distance = move_dist(x, y),  # sourced from global.R
         sigDist = cumsum(Distance),
         NSD = move_nsd(x, y), # sourced from global.R
         dTime = move_dt(fixtime),  # sourced from global.R
         Speed = move_speed(Distance, dTime),  # sourced from global.R
         Year = lubridate::year(fixtime),
         Month = lubridate::month(fixtime),
         Day = lubridate::day(fixtime),
         Hour = lubridate::hour(fixtime)) %>%
  ungroup()

pct_contour <- c(50, 95)

df <- as.data.frame(dat.move)

# Brownian Bridge

bb <- to_ltraj(df)  # sourced from global.R
bbnew <- estimate_bbmm(bb)  # sourced from global.R
bb <- bb_fix(bb)  # sourced from global.R

## Here lies the problem with KL010
hr <- lapply(bb, function(x) get_contours(x, pct_contour))  # get_contours() sourced from global.R


foo <- get_contours(bb[[1]], 95)

for (i in seq_along(bb)) {
  hr[[i]]@proj4string <- sp::CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  hr[[i]] <- sp::spTransform(hr[[i]], CRS('+proj=longlat'))
}




estimate_bbmm <- function(traj) {
  sig1 <- adehabitatHR::liker(traj, sig2 = 40, rangesig1 = c(0, 10), plotit = FALSE)
  bb <- lapply(1:length(traj), function(x) {
    adehabitatHR::kernelbb(traj[x], sig1[[x]]$sig1, 40, grid = 100)
  })
  names(bb) <- lapply(1:length(traj), function(x) {
    attr(traj[[x]], "id")
  })
  return(bb)
}

bbnew <- estimate_bbmm(bb)  # sourced from global.R

