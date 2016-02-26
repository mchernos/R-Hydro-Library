##################################
#								 #
# 	HRU Classification Routine	 #
#								 #
##################################
#	   		      -----				       #
# 	   M. Chernos ~ Nov 2015     #
#	   		      -----				       #
##################################
# This code uses land-use and elevation data to create hydrologic response units (HRUs).
# HRUs are useful for spatially aggregating details to reduce computation time for 
# hydrologic model input. 
# This code is designed to make HRU data to be input into Raven. 
#
# The code also has some neat tricks w/ spatial data:
# 	- uses focal() to spatially smooth (reduce the noise) of HRU raster
#	- converts shapefiles and rasters to each other - magic!
#	- extracts values to make nice summary table.
#	- also now uses sub-basins to help define HRUs (July 21-2015)
#	- updated to add Corbin subbasins (to correspond w/ gauge stn.) - Aug 20-2015
##############
library('maptools')
library('raster')
library('rgeos')
library('rgdal')

################
# HRU ROUTINE
################				
rez = 200		# Raster Resolution (m)
# Vegetation/Land-use Classification
#---------------------------------------------------------------------#
#             Description   GRIDCODE	Replaced w/		New Class.	      #
#                 Conifer      3			100			    Con. Forest	        #
#  Cutblock non-vegetated      10			200			    Grass		            #	
#   Cutblock Regenerating      5			200			    Shrub		            #
#               Deciduous      2			100		    	D. forest	          #
#                   Grass      8			200		    	Grass		            #	
#                    Mine      12			600		    	Mine			          #
#                   Shrub      6			200		    	Shrub		            #
#      Unvegetated / Rock      11			800		    	Rock			          #	
#                   Water      13			900		    	Water		            #
#---------------------------------------------------------------------#
# land_use = raster("landuse_temp.grd")
# values(land_use)[values(land_use) == 0 ] = NA
# land_use = aggregate(land_use, fact = rez/10, fun = modal, expand = F)
# writeRaster(land_use, 'land_use100m', 'raster')# write 100m raster 

# USED
land_use = raster('land_use100m')
land_use = aggregate(land_use, fact = rez/100, fun = modal, expand = F)

# Replace GRIDCODE w/ useful values
values(land_use)[values(land_use) == 13] = 900 	# Water 
values(land_use)[values(land_use) == 11] = 800 	# Unveg
values(land_use)[values(land_use) == 6 ] = 200 	# Shrub 
values(land_use)[values(land_use) == 12] = 600 	# Mine 
values(land_use)[values(land_use) == 8 ] = 200 	# Grass
values(land_use)[values(land_use) == 2 ] = 100 	# Decid.
values(land_use)[values(land_use) == 5 ] = 200 	# C. Regen
values(land_use)[values(land_use) == 10] = 200 	# C. non-veg
values(land_use)[values(land_use) == 3 ] = 100 	# Conifer

# hist(land_use, xaxt = 'n')
# axis(1, labels = c('CForest', 'Grass', 'Shrub', 'DForest 'Mine', 'Rock', 'Water'), 				
#					  at=c(100,200,300,400,600,800,900))


# lu2 = focal(land_use, w = matrix(1, 3, 3), fun = modal)
# values(lu2)[is.na(values(lu2))==T] = values(land_use)[is.na(values(lu2))==T]
# lu3 = focal(land_use, w = matrix(1, 5, 5), fun = modal)
# replace edge removal?
# values(lu3)[is.na(values(lu3))==T] = values(land_use)[is.na(values(lu3))==T]
# land_use = lu3


###########
### DEM ###
# Read in 10m DEM and reprocess (write to reduce comp. time next time)
# dem = raster('clipped_dem.grd')
# projection(dem) = projection(land_use)			# same projection
# dem = crop(dem,extent(land_use))				# Crop first (makes it easier to fix)
# extent(dem) = extent(land_use)					# Need to have same extents
# dem = aggregate(dem, fact = rez/10, fun = mean, expand = F)
# dem = mask(dem,land_use)							# Mask out study area
# writeRaster(dem, 'dem_100m', 'raster')

# USED
dem = raster('dem_100m')
dem = aggregate(dem, fact = rez/100, fun = mean, expand = F)

# Surface Slope Classification
slope = terrain(dem, opt = 'slope', neighbours = 8, unit = 'degrees')           
values(slope) = cut(values(slope), breaks = c(0, 20, 45, 90), labels = 1:3)	# <20, <45, >45

# Aspect Classification
aspect = terrain(dem, opt = 'aspect', neighbours = 8, unit = 'degrees')         # Aspect
# N, E, S, W, N
values(aspect) = cut(values(aspect), breaks = c(0, 45, 135, 225, 315, 360), labels = 1:5) 
values(aspect)[values(aspect)==5] = 1				# Combine north aspects

# Elevation Bands
dem_r = dem
values(dem_r) = round(values(dem_r),-2)			# round to closest 100 m 

######################
#### HRU addition ####
######################
# Add HRUs but not subcatchments yet
#		10 000  +    10     +    1  +  100 
HRU = dem_r*10 + aspect*10 + slope + land_use

# Focal Filter to reduce noise in HRU
HRU = focal(HRU, w = matrix(1, 5, 5), fun = modal, na.rm = T)

########################
# Catchement shapefile #
########################
catch = readShapePoly('michelcr_watershed/michelcr_watershed.shp', 
                      proj4string = CRS('+proj=utm +zone=11 +north +datum=NAD83'))
catch_rast = rasterize(catch, dem_r, 1)

sub_catch = readShapePoly('CanAus_Wshd_Final', 
                          proj4string = CRS('+proj=utm +zone=11 +north +datum=NAD83'))
subc = rasterize(sub_catch, dem_r, as.numeric(sub_catch$GRIDCODE)+1)
values(subc)[is.na(values(subc))==T] = 98

corbin = readOGR('michel_above_corbin.kml','michel_above_corbin.kml')
corbin = spTransform(corbin, CRS('+proj=utm +zone=11 +north +datum=NAD83'))
corbin_r = rasterize(corbin, dem_r, 20)
values(corbin_r)[is.na(values(corbin_r))==T] = 0

# Add sub-catchment components
subcatch = subc + catch_rast + corbin_r

HRU = HRU + subcatch*100000
HRU3 = mask(HRU, catch)		# Mask for proper catchment size. 

# Convert to Polygons
source('gdal_polygonizeR.R')
HRU_Poly = gdal_polygonizeR(HRU3)
writeOGR(HRU_Poly, paste('HRU_Poly',Sys.time(),rez,'m'), layer = 'HRU_Poly', 
         driver = 'ESRI Shapefile', overwrite = T)

# par(mfrow = c(2,2), mar = c(3,3,1,3))
# plot(slope, main = 'Slope')
# plot(aspect, main = 'Aspect')
# plot(dem_r, main = 'Elevation Bands')
# plot(land_use, main = 'Land-Use')

###########################
# BUILD DATAFRAME OF HRUs #
###########################

sub_catch = readShapePoly('CanAus_Wshd_Final', 
                          proj4string = CRS('+proj=utm +zone=11 +north +datum=NAD83'))
subc = rasterize(sub_catch, dem_r, as.numeric(sub_catch$GRIDCODE))
corbin = readOGR('michel_above_corbin.kml','michel_above_corbin.kml')
corbin = spTransform(corbin, CRS('+proj=utm +zone=11 +north +datum=NAD83'))
corbin_r = rasterize(corbin, dem_r, 20)
values(corbin_r)[is.na(values(corbin_r))==T] = 0

subc = subc + corbin_r

# Fresh slope/aspect data
slope = terrain(dem, opt = 'slope', neighbours = 8, unit = 'degrees')           
aspect = terrain(dem, opt = 'aspect', neighbours = 8, unit = 'degrees')         # Aspect
# N, E, S, W, N
values(aspect) = cut(values(aspect), breaks = c(0, 45, 135, 225, 315, 360), labels = 1:5) 
values(aspect)[values(aspect)==5] = 1				# Combine north aspects

# Extract Data Table of values from each HRU polygon
x = Sys.time()
asp = unlist(lapply(extract(aspect, HRU_Poly), mean, na.rm = T))		# mean aspect of HRU (dir.)
slp = unlist(lapply(extract(slope, HRU_Poly), mean, na.rm = T))		# mean slope of HRU (deg)
elv = unlist(lapply(extract(dem, HRU_Poly), mean, na.rm = T))		# mean elevation of HRU (m)
luse = unlist(lapply(extract(land_use, HRU_Poly), modal, na.rm = T))# land use class. 
area_a = area(HRU_Poly)/10^6											# km^2
x_y = coordinates(HRU_Poly)											# UTM (m)
basin = extract(subc, x_y)											# basin ID (sub_catch)
# basin = unlist(lapply(extract(subc, HRU_Poly), modal, na.rm = T))

Sys.time()-x

# Cut to re-label columns
asp = cut(asp, breaks = 0:4, labels = c(0, 90, 180, 270))
luse = cut(luse, 	breaks = c(0,100,200,600,800,900), 	
					labels = c('Forest', 'Grass','Mine', 'Rock', 'Water'))


# AREA, ELEVATION, LATITUDE,LONGITUDE,BASIN_ID, LAND_USE_CLASS, VEG_CLASS, SOIL_PROFILE*?, AQUIFER_PROFILE*, TERRAIN_CLASS*, SLOPE, ASPECT
HRU_data = data.frame(area_a, elv, x_y, basin_ID = basin, luse, slp, asp)
colnames(HRU_data) = c('area', 'elevation', 'X', 'Y', 'basin_ID', 'land_use', 'slope', 'asp')
HRU_data = HRU_data[is.na(HRU_data$slope) == F,]
write.csv(HRU_data, 'HRU_data.csv')

##############
# Processing #
##############
library(rgdal)

# This section is just to re-label things for Raven formatting

hru = read.csv('HRU_data.csv')
xy = SpatialPoints(cbind(hru$X,hru$Y), proj4string = CRS('+proj=utm +zone=11'))
temp = data.frame(spTransform(xy, CRS('+proj=longlat')))
hru$Y = round(temp[,1],4)
hru$X = round(temp[,2],4)
hru$elevation = round(hru$elevation)

temp = apply(data.frame(land_use = hru[,7], 
                        veg_class = paste(hru$land_use,'veg', sep = '_'),
                        soil = paste(hru$land_use,'soil',sep = '_')), 
             2, toupper)

newdat = data.frame(hru[,2:6], temp, aquifer = '[NONE]', 
                    terrain = '[NONE]', round(hru[,8:9]) )

newdat$basin_ID = ifelse(is.na(newdat$basin_ID), 99, newdat$basin_ID)

newdat$soil = ifelse(as.character(newdat$soil) == 'WATER_SOIL', 
                     'LAKE', as.character(newdat$soil))

newdat$basin_ID = ifelse(newdat$basin_ID == 0, 16, newdat$basin_ID)

# newdat$basin_ID = 99				# For aggregated version
# newdat$basin_ID = ifelse(newdat$basin_ID %in% c(1,2,3,4,99), newdat$basin_ID, 1)

write.csv(newdat, paste('HRU_processed', Sys.Date(),'.csv', sep =''), row.names = T)
# write.csv(newdat, 'HRU_processed_aggregated.csv', row.names = T)

# Make unique LOOP HRUs
data = read.csv('HRU_processed2015-11-11.csv')
data$land_use = ifelse(data$basin_ID %in% c(4,5,6,14), 
                       'STANDARD', as.character(data$land_use))
data$soil = ifelse(data$basin_ID %in% c(4,5,6,14), 
                       'LOOP_SOIL', as.character(data$soil))
write.csv(data,paste('HRU_Looped', Sys.Date(),'.csv', sep =''), row.names = F)
############################## END ##############################