############################
#
#      FILL AWS DATA
#
# M. Chernos - March 2015
#
############################
# For CanAus Project using Coleman, Fernie, and Sparwood Wx Stns. 
# Fills data using regressions from most highly correlated Wx Stns. 
# Makes a concerted effort (for the most part) to not fill Stn. with 
# other Stns. that will be used in Raven. 
#
# Regression Fill Tree: 
#
#	Crowsnest: 	1) Coleman		2) Beaver Mines	3) Sparwood 			4) Fernie (only for P)
#	Sparwood:	1) Sparwood CS* 	2) Fernie 		3) Beaver Mines (only for P)
#	Fernie:		1) Elko* 		2) Sparwood CS 	3) Sparwood 			4) Beaver Mines (only for P)
#	 * = 1:1 (no regression function for Precipitation filling)
############################
rm(list=ls())
#########################
### Read in Functions ###
#########################
source('aws_functions.R')
# mergeT() - compares T (min/max/mean) for two sites using elevation
# mergeP() - compares P (rain/snow/total) for two sites
# annual.temp() - summarizes T data + shows # of missing days
# annual.precip() - summarizes P data + missing days
# read.aws.data() - reads AWS data, replaces 'trace' precip w/ 0.1 mm, allows 'estimated'
# fill.T() - uses first dataset T data to fill 2nd dataset NAs using linear regression
# fill.T() - same but for P; can choose direct replacement (instead of linear reg.)
#########################

# READ IN THE DATAS									# Elevation (m)
coleman = read.aws.data('coleman70_97')				# 	1341 m 
crowsnest = read.aws.data('crowsnest93_14')			# 	1303 m
sparwood = read.aws.data('sparwood80_14')			# 	1138 m
kaser = read.aws.data('kaser70_80')					# same as sparwood
sparwoodcs = read.aws.data('sparwoodcs92_14')		# same as sparwood
fernie = read.aws.data('fernie70_14'	)				#	1001 m
elko = read.aws.data('elko70_95')					#	 931 m
beaver = read.aws.data('beavermines70_14')			#	1201 m
corbin = read.aws.data('corbin77_93')				# 	1572 m

# Merge Kaser and Sparwood AWS (only 500 m apart)
sparwood = merge(fill.T(kaser, sparwood), fill.P(kaser, sparwood), by = 'date')

# Check length of datasets:
# length(seq(as.Date('1970-01-01'), as.Date('2014-12-31'), by = 'day')) 
# = 16436

#########################################
#### BUILD CROWSNEST/COLEMAN DATASET ####
#########################################

### Built temperature record ###
# mergeT(coleman, crowsnest, 1341, 1303, 'Coleman', 'Crowsnest')
# mergeT(beaver, coleman, 1201, 1341, 'Beaver Mines', 'Coleman')
# mergeT(sparwood, crowsnest, 1138, 1303, 'Sparwood', 'Coleman')

# Beaver Mines fills most of gap
# Sparwood only fills in 1 point
ccT = fill.T(sparwood, fill.T(beaver, fill.T(coleman, crowsnest) ))		

### Build Precip set ###
#
# Beaver Fills everything up to 2007 (no data '13 -'14)
# Sparwood Fills most of 2007-present (except 2008 chunk)		
# Fernie Leaves 8 NA days. (3 have precip_mm = 0, rest in winter)
#	
# mergeP(annual.precip(crow_precip), annual.precip(sparwood),'Coleman', 'Sparwood') 
# mergeP(annual.precip(crowsnest), annual.precip(sparwood),'Coleman', 'Sparwood') 
# mergeP(annual.precip(coleman), annual.precip(sparwood),'Coleman', 'Sparwood') 	# Sparwood and Coleman best fit
# mergeP(annual.precip(crow_precip), annual.precip(fernie),'Coleman', 'Fernie') 

crow_precip = fill.P(coleman, crowsnest, linear = F)					
crow4 = fill.P(fernie, fill.P(sparwood, fill.P(beaver, crow_precip)	))						
xtemp = apply(crow4[,2:4], 2, function(x) ifelse(is.na(x), 0, as.numeric(x)))		# Replace NAs with 0
ccP = data.frame(date = crow4$date, xtemp)

# Combine temp and precip datasets for Coleman/Crowsnest
crowsnest_data = merge(ccT,ccP, by = 'date') 

################################
#### BUILD SPARWOOD DATASET ####
################################
#
## Build Temperature Record ##
#
# sparwood is mostly filled by nearby Sparwood CS
# Fernie fills in the rest (all stations have very high correlations)
#
# mergeT(sparwood,sparwoodcs, 1138, 1338, 'Sparwood', 'Sparwood CS')
# mergeT(sparwood,fernie, 1138, 1001, 'Sparwood', 'Fernie')
sparT = fill.T(fernie, fill.T(sparwoodcs, sparwood))

## Build Precipitation Record ##
#
# mergeP(sparwood, sparwoodcs, 'sparwood','sparwoodcs') 	# only fills precip_mm (no partition)
# mergeP(sparwood, fernie, 'sparwood', 'fernie')			# 
# mergeP(sparwood, beaver, 'sparwood','beaver')			# 
spar2 = fill.P(sparwoodcs, sparwood, linear = F)

# Partition net precip (if Tmean<0 = snow) 
# (only applied if theres net precip data but no partition)
for (i in 1:length(spar2$date)){
	if (is.na(spar2$rain_mm[i]) | is.na(spar2$snow_cm[i]) ) {
		if ( is.na(spar2$precip_mm[i])==F ){ 
			if(sparT$meanT[i] > 0){
				spar2$rain_mm[i] = spar2$precip_mm[i]
				spar2$snow_cm[i] = 0 } else {
				spar2$rain_mm[i] = 0
				spar2$snow_cm[i] = spar2$precip_mm[i]		}	}	}	}
spar4 = fill.P(beaver, fill.P(fernie, spar2))
xtemp = apply(spar4[,2:4], 2, function(x) ifelse(is.na(x), 0, as.numeric(x)))		# Replace NAs with 0
sparP = data.frame(date = spar4$date, xtemp)
sparwood_data = merge(sparT,sparP, by = 'date') 

##################################
#### BUILD FERNIE, AB DATASET ####
##################################
#
### Build Temperature dataset
#
# mergeT(elko, fernie, 931, 1001, 'Sparwood', 'Fernie')
# mergeT(corbin, fernie, 1572, 1001, 'Corbin', 'Fernie')
# mergeT(sparwoodcs, fernie, 1138, 1001, 'Sparwood CS', 'Fernie')
# mergeT(sparwood, fernie, 1138, 1001, 'Sparwood', 'Fernie')
fernT = fill.T(sparwood, fill.T(sparwoodcs, fill.T(corbin, fill.T(elko, fernie) )))

### Build Precipitation dataset
#
# mergeP(fernie, elko, 'fernie','elko')		# 1:1 better than fit. 
# mergeP(fernie, sparwoodcs, 'fernie','sparwoodcs')
# mergeP(fernie, sparwood, 'fernie','sparwood')
# mergeP(fernie, corbin, 'fernie','corbin')

fernie3 = fill.P(sparwoodcs, fill.P(elko, fernie, linear = F))

# Partition net precip (if Tmean<0 = snow) 
# (only applied if theres net precip data but no partition
for (i in 1:length(fernie3$date)){
	if (is.na(fernie3$rain_mm[i]) | is.na(fernie3$snow_cm[i]) ) {
		if ( is.na(fernie3$precip_mm[i])==F ){ 
			if(fernT$meanT[i] > 0){
				fernie3$rain_mm[i] = fernie3$precip_mm[i]
				fernie3$snow_cm[i] = 0 } else {
				fernie3$rain_mm[i] = 0
				fernie3$snow_cm[i] = fernie3$precip_mm[i]		}	}	}	}
				
fernie4 = fill.P(beaver,fill.P(sparwood, fernie3))
xtemp = apply(fernie4[,2:4], 2, function(x) ifelse(is.na(x), 0, as.numeric(x)))		# Replace NAs with 0
fernP = data.frame(date = fernie4$date, xtemp)

fernie_data = merge(fernT,fernP, by = 'date')

############################
### Write Processed Data ###
############################
write.csv(fernie_data, 'processed/fernie.csv', row.names = F)
write.csv(sparwood_data, 'processed/sparwood.csv', row.names = F)
write.csv(crowsnest_data, 'processed/crowsnest.csv', row.names = F)