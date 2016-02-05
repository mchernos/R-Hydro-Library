#######################
#
# Functions to Read in AWS data and aggregate, make plots
#
# Useful to figure out correlations/relationships between AWS sites.
# Works using Environment Canada data scraped using environment_canada_aws_scraping_tool.R
#
# by M. Chernos - March 2015.
###############################
# mergeT() - compares T (min/max/mean) for two sites using elevation
# mergeP() - compares P (rain/snow/total) for two sites
# annual.temp() - summarizes T data + shows # of missing days
# annual.precip() - summarizes P data + missing days
# read.aws.data() - reads AWS data, replaces 'trace' precip w/ 0.1 mm, allows 'estimated'
# fill.T() - uses first dataset T data to fill 2nd dataset NAs using linear regression
# fill.T() - same but for P; can choose direct replacement (instead of linear reg.)
#########################

# Remove flaggings function
remove.char = function(x,char, n = 2){
	# n--> 2 = character before #, 1 = after #
	x = as.character(x)
	if(length(grep(char,x)) < 1){return(as.numeric(x))} else{
	x[grep(char,x)] = matrix(unlist(strsplit(x[grep(char,x)], char)), ncol = 2, byrow = T)[,n]
	return(as.numeric(x))		}
}

# removes flagging for 'trace' precip
trace.remove = function(x,char, rpl){
	x = as.character(x)
	x = ifelse(x == char, rpl, x)
	as.factor(x)
}

# replaces NAs with linear relationship 
linear.rep = function(x,y){
	fit = lm(x~y)		# y = mx + b
	round(ifelse(is.na(x), fit$coefficient[2]*y + fit$coefficient[1], x),1)
}

# replaces NAs with linear relationship 
linear.rep0 = function(x,y){
  fit = lm(x~y + 0)		# y = mx + b
  round(ifelse(is.na(x), fit$coefficient[1]*y, x),1)
}
# Read in data and process
read.aws.data = function(x){
	temp = read.csv(paste(x,'csv',sep = '.'))
	date = strptime(paste(temp$day, temp$Month, temp$Year, sep = '-'), format = '%d-%m-%Y')
	data = data.frame(temp$maxT, temp$minT, temp$meanT, temp$total_rain_mm, temp$total_snow_cm, temp$total_precip_mm)
	
	# Remove 'Trace' Legend ('LegendTT')
	data = data.frame(sapply(data, trace.remove, char = 'LegendTT', rpl = 0.1))
	
	# Remove 'Legend' Flagging: allow EE (Estimates) and CC (uncertain) and AA (accumulated) 
	data = data.frame(date, sapply(data, remove.char, char = 'Legend', n = 1)) 
	colnames(data) = c('date', 'maxT', 'minT', 'meanT', 'rain_mm', 'snow_cm', 'precip_mm')
	data
}

# Find number of Missing data
cc = function(x){length(x[complete.cases(x)==F])}

# Fit function to paste on graph
fitfunc = function(x,y, date) {
	fit = lm(y~x)
	abline(fit, col = 'red')
	mtext(paste("R2 =", round(summary(fit)$r.squared,4) ) )
	abline(0,1, col = 'grey')
	mtext(paste('n =',length(x[is.na(x)==F & is.na(y) ==F])), 
			1, line = -3, adj = 1, cex = 0.8)
	mtext(paste('mean diff =', round(mean(y - x, na.rm = T),2) ), 
		3, line = -1.5, adj = 0, cex = 0.8)
	mtext(paste(date[1], date[length(date)], sep = ' - '), 1, line = -1, adj = 1, cex = 0.8)
}

# aggregates annual precip and missing days from AWS data.	
annual.precip = function(x){
	dates = strptime(x$date, format = '%Y-%m-%d')
	rain = aggregate(x[c('rain_mm', 'snow_cm','precip_mm')], 
						list(year = cut(dates, breaks = 'year')), sum, na.rm = T )
	missing = aggregate(x[c('rain_mm', 'snow_cm','precip_mm')], 
						list(year = cut(x$date, breaks = 'year')), cc)
	data = merge(rain, missing, by = 'year')
	colnames(data) = c('date','rain_mm', 'snow_cm','precip_mm','m_rain','m_snow','m_total')
	data
}
# Aggregates annual temperature means, missing days (produces yearly summaries)
annual.temp = function(x){
	dates = strptime(x$date, format = '%Y-%m-%d')
	temp = aggregate(x[c('maxT', 'minT','meanT')], 
						list(year = cut(dates, breaks = 'year')), mean, na.rm = T )
	missing = aggregate(x[c('maxT', 'minT','meanT')], 
						list(year = cut(x$date, breaks = 'year')), cc)
	data = merge(temp, missing, by = 'year')
	colnames(data) = c('date','maxT', 'minT','meanT','MmaxT', 'MminT','MmeanT')
	data
}

# Merges and plots rain, snow and total precip - couples w/ fitfunc() to give more info
mergeP = function(x,y,xname, yname){
	cc = merge(x, y, by = 'date')
	
	par(mfrow = c(1,3))
	plot(cc$rain_mm.x, cc$rain_mm.y, xlab = xname, ylab = yname, 
			pch = 21, bg = rgb(0,0,0,0.4), main = 'Rain (mm)')
	fitfunc(cc$rain_mm.x, cc$rain_mm.y, cc$date)	
	
	plot(cc$snow_cm.x, cc$snow_cm.y, xlab = xname, ylab = yname, 
			pch = 21, bg = rgb(0,0,0,0.4), main = 'Snow (cm)')
	fitfunc(cc$snow_cm.x, cc$snow_cm.y, cc$date)	
	
	plot(cc$precip_mm.x, cc$precip_mm.y, xlab = xname, ylab = yname, 
			pch = 21, bg = rgb(0,0,0,0.4), main = 'Total Precipitation (mm)')
	fitfunc(cc$precip_mm.x, cc$precip_mm.y, cc$date)
}

# same, but for Max, Min, Mean Temperatures
mergeT = function(x,y,xz,yz,xname, yname){
	cc = merge(x, y, by = 'date')	
	
	par(mfrow = c(1,3))
	maxT.yad = cc$maxT.y + (0.006*(yz-xz))
	plot(cc$maxT.x, maxT.yad, xlab = xname, ylab = yname, 
			pch = 21, bg = rgb(0,0,0,0.4), main = 'Maximum Temperature (C)')
	fitfunc(cc$maxT.x, maxT.yad, cc$date)	
	
	minT.yad = cc$minT.y + (0.006*(yz-xz))
	plot(cc$minT.x, minT.yad, xlab = xname, ylab = yname, 
			pch = 21, bg = rgb(0,0,0,0.4), main = 'Minimum Temperature (C)')
	fitfunc(cc$minT.x, minT.yad, cc$date)	
	
	meanT.yad = cc$meanT.y + (0.006*(yz-xz))
	plot(cc$meanT.x, meanT.yad, xlab = xname, ylab = yname, 
			pch = 21, bg = rgb(0,0,0,0.4), main = 'Mean Temperature (C)')
	fitfunc(cc$meanT.x, meanT.yad, cc$date)
}


# Linear fill for Temperature Data
fill.T = function(x,y, mergeby = 'date'){
	temp = merge(x,y, by = mergeby, all = T)
	if(length(which(duplicated(temp$date))) > 0 ) 
	  {temp = temp[-which(duplicated(temp$date)),]}
	maxT = linear.rep(temp$maxT.x, temp$maxT.y)
	minT = linear.rep(temp$minT.x, temp$minT.y)
	meanT = linear.rep(temp$meanT.x, temp$meanT.y)
	data = data.frame(date = temp$date, maxT, minT, meanT)
	data
}

# Linear fill for Precip. Data: can also directly replace (no relationship)
fill.P = function(x,y, mergeby = 'date', linear = T){
	temp = merge(x,y, by = mergeby, all = T)	
	if(length(which(duplicated(temp$date))) > 0 ) 
	  {temp = temp[-which(duplicated(temp$date)),]}
	if(linear == F){
		rain_mm = ifelse(is.na(temp$rain_mm.x), temp$rain_mm.y, temp$rain_mm.x) 
		snow_cm = ifelse(is.na(temp$snow_cm.x), temp$snow_cm.y, temp$snow_cm.x)
		precip_mm = ifelse(is.na(temp$precip_mm.x), temp$precip_mm.y, temp$precip_mm.x)
	}else{
		rain_mm = linear.rep0(temp$rain_mm.x, temp$rain_mm.y)
		snow_cm = linear.rep0(temp$snow_cm.x, temp$snow_cm.y)
		precip_mm = linear.rep0(temp$precip_mm.x, temp$precip_mm.y)
	}
	data = data.frame(date = temp$date, rain_mm, snow_cm, precip_mm)
	data
}