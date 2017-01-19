# bcmap.r
#
# generates topographic map of BC
#
# 2015-Mar-27 RDM
############################################################################

rm(list = ls())

library(maps)
library(sp)
library(raster)
library(maptools)
library(rgdal)


# get Canadian elevation data (raster format)
gad = getData("alt", country = "Canada")
gadbc.ext = matrix(nrow = 2, byrow = TRUE, c(-150, -110, 48, 60))
gadbc.ras = crop(gad, gadbc.ext)

# convert from long/lat to BC Albers
alb.par = "+proj=aea +lat_0=45 +lat_1=50 +lat_2=58.5 +lon_0=-126 +y_0=0 +x_0=1000000 +ellps=WGS84"
gadbc.alb = projectRaster(from = gadbc.ras, crs = alb.par, res = 1000)

# get Canada coast and provincial boundary polygons
gd = getData("GADM", country = "Canada", level = 1)

# convert SpatialPolygons to SpatialLines
gd = as(gd, "SpatialLines")

# convert from long/lat to BC Albers
gd.alb = spTransform(gd, CRS("+proj=aea +lat_0=45 +lat_1=50 +lat_2=58.5 +lon_0=-126 +y_0=0 +x_0=1000000 +ellps=WGS84"))


# plot raster map of elevation and add BC coast and borders 
plot(gadbc.alb, box = FALSE, xlim = c(1500000, 2300000), ylim = c(400000, 1200000)) # default palette
lines(gd.alb)

scalebar(400000, xy = c(400000, 400000), 
  divs = 4,
  label = c("0", "200", "400"),
  type = "bar"
)

