###########################
# Cranbrook Water Balance #
###########################
# Check correlations between Cranbrook Farm site AWS 
# and Cranbrook Airport (YXC) AWS to fill in missing 
# Farm data. 
#
# Prepared by M. Chernos
###########################
rm(list=ls())

# read in Farm Data and make date class
farm = read.csv('2014farm_daily.csv')
farm$Day = strptime(farm$Day, format = '%d-%b-%Y')

# read in YXC Data
yxc = read.csv('yxc_daily2014.csv')
# yxc$Date.Time = strptime(yxc$Date.Time, format = '%y-%m-%d')

# Read in hourly YXC data from each month (for humidity and windspeed measurements)
yxchour = read.csv('hourly_months/yxc1.csv')
for (i in 2:12){
	temp = read.csv(paste('hourly_months/yxc', i, '.csv', sep = ''), header = F)
	colnames(temp) = colnames(yxchour)
	yxchour = rbind(yxchour,temp)
}
yxchour$Date.Time = strptime(yxchour$Date.Time, format = '%y-%m-%d %H:%M')

# Cloudiness from 'weather' obsv.
weather_level = levels(yxchour$Weather)
#Full sun =  'clear'(2), "Mainly Clear"(7), - use binary 1=cloud, 0 = sun
yxchour$weather = ifelse(yxchour$Weather == weather_level[c(2,7)], 0, 1)

# Aggregate relevant variables (RH, Windspeed) into daily means
relvars = c('Rel.Hum....', 'Wind.Spd..km.h.', 'weather')
yxchour2 = aggregate(yxchour[relvars], list(day = cut(yxchour$Date.Time, breaks = 'day')), 
						mean, na.rm = T)
colnames(yxchour2) = c('Day', 'rh', 'u', 'weather')

############################
# Regression Plot function #
############################
regplot = function(x,y, vartitle, ylab = 'Farm', xlab = 'YXC'){
	plot(x,y,main = vartitle, ylab = ylab, xlab = xlab)
	abline(0,1, col = 'grey')
	fit = lm(y~x)
	abline(fit, col = 'red')
	mtext(paste('r.squared = ', round(summary(fit)$r.squared,4)), cex = 0.6)
}

# re-written for intercept of 0
regplot0 = function(x,y, vartitle, ylab = 'Farm', xlab = 'YXC'){
	plot(x,y,main = vartitle, ylab = ylab, xlab = xlab)
	abline(0,1, col = 'grey')
	fit = lm(y~x + 0)
	abline(fit, col = 'red')
	mtext(paste('r.squared = ', round(summary(fit)$r.squared,4)), cex = 0.6)
}
###################################################################
# Function that changes NAs by applying a linear regression model #
###################################################################
regsub = function(x,y){
	fit = lm(y~x)
	# y = mx+b
	ifelse(is.na(y) == TRUE, fit$coefficient[2]*x + fit$coefficient[1], y)
}

# re-written for intercept of 0
regsub0 = function(x,y){
	fit = lm(y~x + 0)
	# y = mx+b
	ifelse(is.na(y) == TRUE, fit$coefficient[1]*x, y)
}

###############
# Temperature #
###############
# Add more logical naming (YXC)
yxc_maxT = yxc$Max.Temp..C.
yxc_minT = yxc$Min.Temp..C.
yxc_meanT = yxc$Mean.Temp..C.

# Add more logical naming (Farm)
farm_maxT = farm$TMax_Raw_
farm_minT = farm$TMin_Raw_
farm_meanT = farm$Temp_Raw_

# Plot Relationships and apply linear model
par(mfrow = c(1,3))
regplot(yxc_maxT, farm_maxT, 'Max Temp')
maxT = regsub(yxc_maxT, farm_maxT)
# points(yxc_maxT, regsub(yxc_maxT, farm_maxT), col = 'red')

regplot(yxc_minT, farm_minT, 'Min Temp')
minT = regsub(yxc_minT, farm_minT)
# points(yxc_minT, regsub(yxc_minT, farm_minT), col = 'red')

regplot(yxc_meanT, farm_meanT, 'Mean Temp')
meanT = regsub(yxc_meanT, farm_meanT)
# points(yxc_meanT, regsub(yxc_meanT, farm_meanT), col = 'red')
##########################

#############
#  Humidity #
#############
# par(mfrow = c(1,3))
regplot(yxchour2$rh, farm$Rh_Raw_, 'RH%')
RH = regsub(yxchour2$rh, farm$Rh_Raw_)
RH = ifelse(RH>100, 1, RH/100)  # Stops model from predicting RH > 100%
# points(yxchour2$rh, RH, col = 'red')

#############
# Windspeed #
#############
regplot(yxchour2$u, farm$Wspd_Raw_, 'Mean Windspeed')
windspeed = regsub(yxchour2$u, farm$Wspd_Raw_)/3.6     # convert from km/h to m/s
# points(yxchour2$u, windspeed, col = 'red')

#################
# Net Radiation #
#################
# Not sure about this...
require("EcoHydRology")
lat = 49.5*pi/180

net_radiation = NetRad(lat, 1:365, Tx = maxT, Tn = ifelse(minT > maxT, maxT, minT), 
						albedo = 0.3, airtemp = meanT)#, 
#						cloudiness = yxchour2$weather)
plot(net_radiation)

############
# Rainfall #
############
yxc_rain = yxc$Total.Rain..mm./1000         # convert mm to m
# farm_rain = farm$Rn_1_Raw_/10    # convert to m
# regplot0(yxc_rain,farm_rain, 'Rainfall (mm/day)')
# rainfall = regsub0(yxc_rain,farm_rain)
# # points(yxc_rain, rainfall, col = 'red')

rainfall = yxc_rain # Use YXC data for rainfall
rainfall[is.na(rainfall)==T] = 0  # Change 2 NA values to 0 m rain
############
# Snowfall #
############
fresh_swe = 0.1             			# 10% of snow depth - fresh (density of 100cm/g3)
old_swe = 0.2
ground_snow_swe = yxc$Snow.on.Grnd..cm. * 10 * old_swe    # COnvert to mm (w.e.) - old snow
snowfall_swe = yxc$Total.Snow..cm. * 10 * fresh_swe    # Convert to mm (w.e.) - new snow

# Calculate melt factor
ground_snow_swe[63]/sum(meanT[63:68])
###########################
# Clean up and Export Data #
############################

date = farm$Day
data  = data.frame(date, maxT, minT, meanT, RH, windspeed, net_radiation, rainfall, ground_snow_swe,snowfall_swe)
write.csv(data, 'filled_farm2014.csv')


