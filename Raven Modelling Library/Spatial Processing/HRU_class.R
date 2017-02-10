##################################
#								 #
# 	HRU Classification Routine	 #
#								 #
##################################
#	   		      -----				       #
# 	   M. Chernos ~ Jan 2017     #
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
# - updated raster projection to nearest-neighbour to stop values averaging - Sept 2016
##############
rm(list = ls())
library('maptools')
library('raster')
library('rgeos')
library('rgdal')

################
# HRU ROUTINE
################				
rez = 500		# Raster Resolution (m)

# Add Shapefile to Clip
sub_catch = readShapePoly('permanent_data/watersheds_final.shp', 
                          proj4string = CRS('+proj=utm +zone=11 +north +datum=NAD83'))
# sub2 = spTransform(sub_catch[1,], CRS('+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))
# writeOGR(sub2, 'StMarys.shp', 'StMarys', 'ESRI Shapefile')

###########
### DEM ###
###########
dem = raster('permanent_data/dem_sinkfilled.sdat')
dem = aggregate(dem, fact = rez/100, fun = mean, expand = F)
projection(dem) = '+proj=utm +zone=11 +north +datum=NAD83'

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

# Vegetation/Land-use Classification
land_use = raster('permanent_data/Adams_LU.grd')
land_use = aggregate(land_use, fact = rez/100, fun = modal, expand = F)
land_use = projectRaster(land_use, dem, res = rez, crs = projection(dem), method = "ngb" ) # Nearest Neighbour needed to keep values from averaging
values(land_use)[values(land_use) < 1] = NA # Remove non-classified parts...
land_use = mask(land_use, sub_catch)

# Replace GRIDCODE w/ useful values
# Raster Value	Classification
# 1	Human Dominated (probably Mines)
# 2	Wetlands
# 3	Glaciers
# 4	Grasslands
# 5	Alpine
# 6	Water
# 7	Forest & Other
values(land_use) = 100 * values(land_use) # Multiply for proper magnitude 

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
subc = rasterize(sub_catch, dem_r, as.numeric(sub_catch$DN))

HRU = HRU + subc * 100000
HRU = mask(HRU, sub_catch)		# Mask for proper catchment size. 

# Convert to Polygons
source('gdal_polygonizeR.R')
HRU_Poly = gdal_polygonizeR(HRU)
writeOGR(HRU_Poly, paste('HRU_Poly',Sys.Date(),rez,'m'), layer = 'HRU_Poly', 
         driver = 'ESRI Shapefile', overwrite = T)

par(mfrow = c(2,2), mar = c(3,3,1,3))
plot(slope, main = 'Slope')
plot(aspect, main = 'Aspect')
plot(dem_r, main = 'Elevation Bands')
plot(land_use, main = 'Land-Use')

###########################
# BUILD DATAFRAME OF HRUs #
###########################

# sub_catch = readShapePoly('CanAus_Wshd_Final', 
#                           proj4string = CRS('+proj=utm +zone=11 +north +datum=NAD83'))
# subc = rasterize(sub_catch, dem_r, as.numeric(sub_catch$GRIDCODE))
# corbin = readOGR('michel_above_corbin.kml','michel_above_corbin.kml')
# corbin = spTransform(corbin, CRS('+proj=utm +zone=11 +north +datum=NAD83'))
# corbin_r = rasterize(corbin, dem_r, 20)
# values(corbin_r)[is.na(values(corbin_r))==T] = 0
# 
# subc = subc + corbin_r

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
luse = unlist(lapply(extract(land_use, HRU_Poly), modal, na.rm = T)) # land use class. 
area_a = area(HRU_Poly)/10^6											# km^2
x_y = coordinates(HRU_Poly)											# UTM (m)
basin = extract(subc, x_y)											# basin ID (sub_catch)
# basin = unlist(lapply(extract(subc, HRU_Poly), modal, na.rm = T))

Sys.time()-x

# Cut to re-label columns
asp = cut(asp, breaks = 0:4, labels = c(0, 90, 180, 270))
luse = cut(luse, 	breaks = c(0,100,200,300,400,500,600,701), 	
					labels = c('Disturbed','Wetland','Glacier','Grassland','Alpine', 'Lake','Forest'))
# 1	Human Dominated (probably Mines)
# 2	Wetlands
# 3	Glaciers
# 4	Grasslands
# 5	Alpine
# 6	Water
# 7	Forest & Other

# AREA, ELEVATION, LATITUDE,LONGITUDE,BASIN_ID, LAND_USE_CLASS, VEG_CLASS, SOIL_PROFILE*?, AQUIFER_PROFILE*, TERRAIN_CLASS*, SLOPE, ASPECT
HRU_data = data.frame(area_a, elv, x_y, basin_ID = basin, luse, slp, asp)
colnames(HRU_data) = c('area', 'elevation', 'X', 'Y', 'basin_ID', 'land_use', 'slope', 'asp')
HRU_data = HRU_data[complete.cases(HRU_data),] # Remove NAs (on edge or outside catchment)
write.csv(HRU_data, 'HRU_data.csv')

##############
# Processing #
##############
library('rgdal')

# This section is just to re-label things for Raven formatting

hru = read.csv('HRU_data.csv')
xy = SpatialPoints(cbind(hru$X,hru$Y), proj4string = CRS('+proj=utm +zone=11'))
temp = data.frame(spTransform(xy, CRS('+proj=longlat')))
hru$Y = round(temp[,1],4)
hru$X = round(temp[,2],4)
hru$elevation = round(hru$elevation)

temp = apply(data.frame(land_use = hru[,7], 
                        veg_class = ifelse( hru$land_use %in% c('Lake', 'Glacier'), as.character(hru$land_use),
                                              paste(hru$land_use,'veg', sep = '_') ),
                        soil = ifelse( hru$land_use %in% c('Lake', 'Glacier'), as.character(hru$land_use),
                                       paste(hru$land_use,'soil',sep = '_')) ), 
             2, toupper)
newdat = data.frame(hru[,2:6], temp, aquifer = '[NONE]', 
                    terrain = '[NONE]', round(hru[,8:9]) )


write.csv(newdat, paste('HRU_processed', Sys.Date(),'.csv', sep =''), row.names = T)
# write.csv(newdat, 'HRU_processed_aggregated.csv', row.names = T)
############################## END ##############################