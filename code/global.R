################################################################################
# Global functions used in collar-viewer shiny app
# McCrea Cobb <mccrea_cobb@fws.gov>
################################################################################


#----
## Aesthetics

# Color palatte used in the application
color_pal <- c("#3366CC", "#DC3912", "#FF9900", "#109618", "#990099", "#0099C6",
               "#DD4477", "#66AA00", "#B82E2E", "#316395", "#994499", "#22AA99",
               "#AAAA11", "#6633CC", "#E67300", "#8B0707", "#651067", "#329262",
               "#5574A6", "#3B3EAC")


#----
## Mapping functions


#' collar_map
#' 
#' @description 
#' 
#' @author McCrea Cobb <mccrea_cobb@fws.gov
#'
#' @param gps_collar A dataframe of GPS collar fixes... 
#'
#' @return A leaflet map of fix locations, subsetted to one daily fix.
#' @export
#'
#' @examples collar_map(dat)
#' 
collar_map <- function(gps_collar) {  # Map collar data. One point per day for lines. First and last points. Used in the Interactive Map tab.
  df <- gps_collar %>%
    filter(!(is.na(lon) | is.na(lat))) %>%
    arrange(id, fixtime) %>%
    group_by(id, lubridate::as_date(fixtime)) %>%
    slice(1) %>%  # takes the first fix of the day for each collar
    ungroup()
  
  ids <- unique(df$id)
  pal <- rep_len(color_pal, length(ids))

  map <- leaflet() %>%
    addProviderTiles("Esri.WorldTopoMap", options = providerTileOptions(attribution = NA)) %>%
    addMeasure(primaryLengthUnit="kilometers", secondaryLengthUnit="kilometers") %>%
    addScaleBar(position="bottomleft") %>%
    setView(lng = -150, lat = 65, zoom = 4)
    
  
  for (i in seq_along(ids)) {
    d <- df %>% filter(id == ids[i])
    dp <- d[c(1, nrow(d)), ]
    map <- addPolylines(map, lng = d$lon, lat = d$lat,
                        weight = 1,
                        color = pal[i],
                        opacity = .4)
    map <- addCircleMarkers(map, lng = dp$lon, lat = dp$lat,
                            stroke = FALSE,
                            radius = 4,
                            color = pal[i],
                            fillOpacity = 1,
                            popup = paste(sep = "<br>",
                                          paste("<b>Collar ID:<b>", ids[i]),
                                          paste("<b>Study site:<b>", d$site),
                                          paste("<b>Last fix:<b> ", d$fixtime)))
  }
  return(map)
}




#----
## Functions used to create dat.move() in the Home Range tab

#' xy_conv
#'
#' @description Adds x and y projected coordinates columns to the dataframe 
#' containing lat and long columns. Used to create maps of home ranges.
#' 
#' @param df 
#' @param xy 
#' @param CRSin 
#' @param CRSout 
#'
#' @return
#' @export
#'
#' @examples
#' 
xy_conv <- function(df, xy = c('lon', 'lat'), CRSin = '+proj=longlat',
                   CRSout = "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") { # Alaska Albers
  
  df <- df[complete.cases(df[, xy]), ]
  conv <- sp::SpatialPoints(coordinates(cbind('x' = df[, xy[1]],
                                              'y' = df[, xy[2]])),
                            proj4string = CRS(CRSin))
  conv <- sp::spTransform(conv, CRS(CRSout))
  conv <- data.frame(conv)
  colnames(conv) <- c('x', 'y')
  df <- cbind(df, conv)
  
  return(df)
}


#' move_dist
#'
#' @description Output is used by moveSpeed().
#' 
#' @param x A vector of projected x coordinates. 
#' @param y A vector of projected y coordinates. 
#'
#' @return A vector of movement distances (meters).
#' @export
#'
#' @examples
#' 
move_dist <- function(x, y) {  # 
  dist <- c(0, sqrt((x[-1] - x[-length(x)])**2 +
                      (y[-1] - y[-length(y)])**2))
  return(dist)  # same unit as input (meters)
}

#' move_nsd
#' 
#' @description 
#'
#' @param x A vector of projected x coordinates.
#' @param y A vector of projected y coordinates.
#'
#' @return A vector of net square displacement values. 
#' @export
#'
#' @examples
#' 
move_nsd <- function(x, y) {
  r2n <- (x - x[1])**2 + (y - y[1])**2
  r2n <- (r2n - min(r2n))/(max(r2n) - min(r2n))
  return(r2n)
}

#' move_dt
#'
#' @description 
#' 
#' @param time 
#'
#' @return The time (seconds) between consecutive time values in a vector of 
#' fixtimes. Output is used by moveSpeed().
#' @export
#'
#' @examples
#' 
move_dt <- function(time) {
  dt <- c(0, unclass(time[-1]) - unclass(time[-length(time)]))
  return(dt/3600) # seconds
}

#' move_speed
#' 
#' @description 
#' 
#' @param dist Movement distances (meters) output from moveDist().
#' @param time Movement time (seconds) output from moveDt().
#'
#' @return The movement speed (meters/second) between consecutive GPS collar fixes.
#' @export
#'
#' @examples
#' 
move_speed <- function(dist, time) {  # 
  speed <- (dist/1000)/time
  speed[is.nan(speed)] <- 0
  return(speed)
}


#' map_pts
#' 
#' @description Creates a leaflet map object of all GPS collar fixes, with 
#' points and lines. Used on the Home Range tab.
#'
#' @param map 
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
#' 
map_pts <- function(map, df) {
  ids <- unique(df$id)
  pal <- rep_len(color_pal, length(ids))
  layers <- list()

  for(i in seq_along(ids)) {
    dat <- df %>% filter(id == ids[i])
    map <- addPolylines(map, lng = dat$lon, lat = dat$lat,
                        group = as.character(ids[i]),
                        color = pal[i], weight = 1)
    map <- addCircleMarkers(map, lng = dat$lon, lat = dat$lat,
                            group = as.character(ids[i]), color = pal[i],
                            radius = 3, stroke = FALSE, fillOpacity = .3,
                            popup = paste(sep = "<br>",
                                          paste("<b>Collar ID:</b> ", ids[i]),
                                          paste("<b>Date:</b> ", dat$fixtime)))
    layers <- c(layers, as.character(ids[i]))
  }
  map <- addLayersControl(map, overlayGroups = layers)
  return(map)
}


#' map_polygons
#' 
#' @description Adds geoJSON polygons to a leaflet map. Used on the Home Range tab.
#'
#' @param map 
#' @param geojson 
#'
#' @return 
#' @export
#'
#' @examples
#' 
map_polygons <- function(map, geojson) {
  pal <- rep_len(color_pal, length(geojson))
  for (i in seq_along(geojson)) {
    map <- addGeoJSON(map, geojson[[i]], color = pal[i],
                      weight = 1, group = names(geojson)[i])
  }
  return(map)
}





#----
## Brownian Bridge home range functions

#' to_ltraj
#' 
#' @description 
#'
#' @param dat A dataframe containing a column of projected x and y coordinates.
#'
#' @return A ltraj object.
#' @export
#'
#' @examples
#' 
to_ltraj <- function(dat) {
  dat <- as.data.frame(dat)
  dat$fixtime <- as.POSIXct(dat$fixtime, format = '%Y-%m-%d %H:%M:%S')
  dat <- dat[complete.cases(dat[, c("x", "y", "fixtime")]), ]

  traj <-  adehabitatLT::as.ltraj(dat[, c("x", "y")], date = dat$fixtime, id = dat$id)
  return(traj)
}

#' estimate_bbmm
#' 
#' @description 
#'
#' @param traj 
#'
#' @return Brownian Bridge contours from a traj object and plot Brownian Bridge HR.
#' @export
#'
#' @examples
#' 
estimate_bbmm <- function(traj) {
  sig1 <- liker(traj, sig2 = 40, rangesig1 = c(0, 10), plotit = FALSE)
  bb <- kernelbb(traj, sig1[[1]]$sig1, 40, grid = 100)
  return(bb)
}


#' get_ud
#' 
#' @description Used in get_mud().
#'
#' @param ud 
#' @param pct_contour 
#'
#' @return A geoJSON of contours. 
#' @export
#'
#' @examples
#' 
get_ud <- function(ud, pct_contour) {
  ud_list <- list(length(pct_contour))
  print('----- Entering contour loop')
  print(paste('-----', pct_contour))
  for (i in seq_along(pct_contour)) {
    print(paste('-----', pct_contour[[i]]))
    ctr <- getverticeshr(x = ud, percent = pct_contour[[i]])
    ctr@proj4string <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
    ctr <- sp::spTransform(ctr, CRS('+proj=longlat'))
    ctr <- geojson_list(ctr)
    ud_list[[i]] <- ctr
  }
  geojson <- geojson_json(Reduce(`+`, ud_list))
  return(geojson)
}


#' bb_fix
#' 
#' @description 
#'
#' @param bb 
#'
#' @return
#' @export
#'
#' @examples
#' 
bb_fix <- function(bb) {  # A fix for Brownian bridge calculations
  if (class(bb) == 'estUDm') {
    v <- bb
  } else {
    v <- list(bb)
  }
  return(v)
}



#----
## For maps of kernel and Brownian Bridge home ranges

#' get_contours
#' 
#' @description 
#'
#' @param ud 
#' @param pct 
#'
#' @return A spatial polygon dataframe of kernel utilization density 
#' contours. Used to create maps of kernel and Brownian Bridge home ranges.
#' @export
#'
#' @examples
#' 
get_contours <- function(ud, pct) {  # 
  
  ids <- as.character(pct)
  x <- adehabitatHR::getvolumeUD(ud)
  xyma <- coordinates(x)
  xyl <- list(x = unique(xyma[, 1]), y = unique(xyma[, 2]))
  z <- as.image.SpatialGridDataFrame(x[, 1])$z
  
  cl <- lapply(pct, function(x) {
    contourLines(x = xyl$x, y = xyl$y, z, nlevels = 1, levels = x)
  })
  
  plys <- lapply(seq_along(cl), function(i) {
    Polygons(lapply(seq_along(cl[[i]]), function(j) {
      m <- cl[[i]][[j]]
      ply <- cbind(m$x, m$y)
      ply <- rbind(ply, ply[1, ])
      Polygon(ply)
    }), ID = ids[i])
  })
  
  plys <- lapply(plys, function(x) maptools::checkPolygonsHoles(x))
  splys <- SpatialPolygons(plys)
  dff <- data.frame(id = ids)
  row.names(dff) <- ids
  spdf <- SpatialPolygonsDataFrame(splys, dff)
  return(spdf)
}
