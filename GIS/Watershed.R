###################################################################
# 					WATERSHED DELINEATION 						  #
###################################################################
# This code takes a DEM of the place of interest,
# fills sinks, creates a channel system (and produces 
# a shapefile of streams), and then takes a table of 
# locations (POIs in UTM format), and delineates watersheds
# based on those points. It then combines them all, and 
# produces a shapefile with all watershed delineations.

# This tool relies on having a working copy of SAGA GIS
# installed on your system. On a Mac, see these instructions:
# http://www.nickrobison.com/2013/07/03/compile-saga-for-mac-os-x/
# tl;dr is run this code in Terminal: 
# 		brew tap osgeo/osgeo4mac
# 		brew install saga-gis --with-app --with python
# (homebrew is required...)
#
###################################################################
# 					Matt Chernos - September 2015				  #
###################################################################
rm(list = ls())

# Required Libraries
library(RSAGA)
library(raster)
library(maptools)
library(rgdal)
source('gdal_polygonizeR.R')		# Needed to polygonize rasters

# Set the Initiation Value (see "Channel Network" below)
# Too low and you will have too many small streams, 
# too large, and you won't have enough complexity...
init_value = 5000000			# "my" default = 10000000


# Load DEM and prepare for SAGA Analyses
dem = raster('permanent_data/dem.tif')
# writeRaster(dem, "dem_saga.sdat", "SAGA", overwrite = T)

# # Demonstrative...
# rsaga.env()
# rsaga.get.modules("ta_channels")

# # Fill Sinks (takes a long time... zzz... get coffee... zzz...)
# 		 	- Use method = 'xxl.wang.liu.2006' for "BIG" data
# rsaga.fill.sinks('dem_saga.sgrd', 'dem_sinkfilled.sgrd', method = "xxl.wang.liu.2006")
dem_filled = raster('permanent_data/dem_sinkfilled.sdat')
projection(dem_filled) = projection(dem)

# Catchment Area (get flow pathways grid)
rsaga.parallel.processing('permanent_data/dem_sinkfilled.sgrd', out.carea = 'catchment_area.sgrd')
# catch_area = raster('catchment_area.sdat')

# Channel Network
rsaga.geoprocessor(lib = "ta_channels", module = 8,
					param = list(
 					ELEVATION = 'permanent_data/dem_sinkfilled.sgrd',		# dem
  					# CHNLNTWRK = 'channel_network.sgrd',
 					# CHNLROUTE = 'channel_route.sgrd',
 					SHAPES = 'channel_ntwrk.shp',
 					INIT_GRID = 'catchment_area.sgrd',		# generated above
 					INIT_METHOD = 2,						# 2 -> greater than
 					INIT_VALUE =init_value) ) 
channel_shape = readOGR('channel_ntwrk.shp', 'channel_ntwrk')
# channel_network = raster('channel_network.sdat')


###################################################################
# 	To modify as required (i.e. add your spatial points here)	  #
###################################################################
# Ensure that if there is overlap, you have them ordered from largest-->smallest
# i.e. a later watershed should only overlap with 1 previous watershed
# Create/Read POIs
POIs = readOGR('permanent_data/watershed_pois.kml', 'Watersheds')
POIs = spTransform(POIs, crs(dem))
# poi_dat = data.frame(X = c(639930,639768), Y = c(5484783,5484518), NAME = c('Fernie', 'CoalCr'))
# POIs = SpatialPointsDataFrame(coordinates(poi_dat[,-3]), poi_dat[,-3], proj4string = crs(dem))
writeOGR(POIs,'POIs.shp',layer = 'POIs', driver = 'ESRI Shapefile', overwrite = T)
###################################################################

# # Snap Points to Lines (shapes_points, 19)
# snap catchment points to stream network
rsaga.geoprocessor(lib = 'shapes_points', 19,
				param = list(
					INPUT = 'POIs.shp', 						# Catchment POIs
					SNAP = 'channel_ntwrk.shp',				# Channel Network (created above)
					OUTPUT = 'snapped_points.shp',
					DISTANCE = 500) )						# Search distance
snapped_points = readOGR('snapped_points.shp', 'snapped_points')

# Upslope Area
for (i in 1:length(snapped_points)){
	rsaga.geoprocessor(lib = 'ta_hydrology', 4,
					param = list(TARGET_PT_X = coordinates(snapped_points)[i,1],
								 TARGET_PT_Y = coordinates(snapped_points)[i,2],
								 ELEVATION = 'permanent_data/dem_sinkfilled.sgrd',
								 AREA = paste(i,'catch.sgrd',sep = ''),
								 METHOD = 0)	) 		# 0 = Deterministic 8
					}

# For Loop to combine Polygonize the Sub basins
catch = raster('1catch.sdat')
values(catch) = ifelse(values(catch)> 0, 1, values(catch))	 # Need to change coding so no overlap
subb = gdal_polygonizeR(catch)[1,]					# Only need first polygon (2nd is square from dem)
for (i in 2:length(snapped_points)){
	temp = raster(paste(i,'catch.sdat', sep = ''))
	values(temp) = ifelse(values(temp)> 0, i, values(temp))
	temp = gdal_polygonizeR(temp)[1,]				# Only need first polygon (2nd is square from dem)
	subb = bind(subb, temp)
}

# Add Proper names to Sub Basins
subb$Name = as.character(snapped_points$Name)

# Now plot that shit
plot(dem, col = terrain.colors(100))
plot(subb, add = T)
plot(channel_shape, add = T, col = 'blue')
text(subb, subb$Name, cex = 0.6, font = 2) # Add labels for subbasins 

# Write the output, dummy!
writeOGR(subb,'permanent_data/watersheds_final.shp',layer = 'watersheds_final', driver = 'ESRI Shapefile', overwrite = T)
###################################################################
######### 					END CODE 						#######
###################################################################

x = readOGR('permanent_data/watersheds_final.shp', 'watersheds_final')
plot(x)
text(x, x$Name, cex = 0.6, font = 3)
data.frame(Watershed = x$Name, Area.km2 = area(x)/10^6)