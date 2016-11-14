#CCD AND CENSUS SHAPEFILES NEEDED
#JOSE M HERNANDEZ
#10/17/16
library(sp)
library(rgdal)
library(maps)
library(maptools)
library(foreign)
library(dplyr)

rm(list=ls())

## STEP1: Get your data ready
data <- readRDS("~/Google Drive/CRPE/KauffmanKC/Data/ccd/ccd_2010_14.Rda" )

##We only want MO 
MO_data <- data.frame(filter(data, fipst==29))

## STEP 2: Load geographic data
#read in Census Places polygons
MO <- readOGR("/Users/crpeadmin/Google Drive/CRPE/KauffmanKC/Data/shapefiles/shp_MO", "tl_2011_29_place")

MO <- readOGR("/Users/crpeadmin/Google Drive/CRPE/KauffmanKC/tl_2016_29_unsd", "tl_2016_29_unsd")
#combine all states' shapefiles 
summary(MO$NAME)
plot(MO)
##first, need to change the polygon IDs so that they are not duplicated across shapefile sets
MO1 <- spChFIDs(MO, as.character(MO$GEOID))

## STEP 3: Attach city onto state data
#set missing lon/lat to 0, & set lon/lat to coordinates
MO_data$latcod[is.na(MO_data$latcod)] <- 0
MO_data$loncod[is.na(MO_data$loncod)] <- 0
coordinates(MO_data) <- c("loncod", "latcod")

#tell R that school coordinates are in the same lat/long reference system as the places data
proj4string(MO_data) <- proj4string(MO1)

#combine is.na() with over() to do the containment test (note that we need to "demote" places to a SpatialPolygons object first)
inside.place <- !is.na(over(MO_data, as(MO1, "SpatialPolygons")))

#use "over" again, this time with places as a SpatialPolygonsDataFrame object, to determine which places (if any) contains each school, and store the place name as attribute of the schools data
MO_data$city <- rep(NA, nrow(MO_data))
MO_data$city <- over(MO_data, MO1)$NAMELSAD

#write the augmented state dataset to new .dta file
schools <- as.data.frame(MO_data)
####
saveRDS(schools, "~/Google Drive/CRPE/KauffmanKC/Data/ccd/ccd_2010_2014_city.rda")
####

tl_2016_29_unsd.shp