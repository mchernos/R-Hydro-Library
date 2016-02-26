###################################################
#		     Hydrology Aggregation Functions 
#						~~~
# 				 	Matt Chernos
# 					 April 2015
#						~~~
#
# Updated July 6 - added daily.flows() - improved performance/speed
#				 - added fit.plots() - added ACF/CCF functionality
# Updated Sept 14 - improved daily.flowQ() - improved performance/speed, 
#								added ability to test individual year.
									
###################################################
### Functions in this file:
# 		maf() --> calculates mean annual flow from WSC .csv file
# 		mmf() --> calculates mean monthly flow from WSC .csv file
# 		amf() --> calculates average monthly flow from mmf() output
#		daily.flow() --> plots average, max/min, Q10/Q90 discharges 
#						  for specified river from WSC daily data
# 		daily.flowQ() --> same but allows for non-read in data
#	  	daily.flows() --> allows for the comparison of 2 models
#		daily.anomaly()--> takes time series variable and calculates 
# 	 					how far it is above/below mean for that DOY
#		monthly.anomaly()--> takes time series variable and calculates
#		  				daily rate relative to monthly sum (as a %)
#	  	fit.plots()	-->	Plots model fit and returns relevant stats
# 						including NSE, RMSE, R2 and PBIAS.

library('lubridate')
library('hydroGOF')

# Counts NAs
cc = function(x){length(x[complete.cases(x)==F])}

#################		
# MAF Function
#
# Mean annual flow from WSC data
#################		
maf = function(filename, record.start = NULL){
	data = read.csv(filename, skip = 1)
	data = data[data$PARAM == 1 & year(data$Date) >= ifelse(is.null(record.start), 0, record.start),]
	date = strptime(data$Date, format = '%Y/%m/%d')
	
	# Aggregate Data
	miss = aggregate(data['Value'],  list(year = cut(date, breaks = 'year')), cc)
	data = aggregate(data['Value'], list(year = cut(date, breaks = 'year')), mean, na.rm = T)
	data = data.frame(data, miss$Value)
	colnames(data) = c('year', unlist(strsplit(filename,'_daily.csv')), 'missing_days')
	data$year = year(strptime(data$year, '%Y-%m-%d'))
	data
}
# elk_natal = maf('elknatal_daily.csv', record.start = 1979)

#################		
# MMF Function
#
# Mean monthly flow from WSC data
#################		
mmf = function(filename, record.start = NULL){
	data = read.csv(filename, skip = 1)
	data = data[data$PARAM == 1 & year(data$Date) >= ifelse(is.null(record.start), 0, record.start),]
	date = strptime(data$Date, format = '%Y/%m/%d')
	
	# Aggregate Data
	miss = aggregate(data['Value'],  list(year = cut(date, breaks = 'month')), cc)
	data = aggregate(data['Value'], list(year = cut(date, breaks = 'month')), mean, na.rm = T)
	data = data.frame(data, miss$Value)
	colnames(data) = c('month', unlist(strsplit(filename,'_daily.csv')), 'missing_days')
	data
}
# elk_natal = mmf('elknatal_daily.csv', record.start = 1979)

###############
# AMF Function
#
# Average monthly flow function using MMF function
###############
amf = function(x,time){
	amf = c()
	for (i in 1:12){
		temp = x[month(time) == i]
		mmf[i] = mean(temp, na.rm = T)	}
	amf
}
# temp = sapply(data1[2:6], amf, data1$month)

###############################
# Daily Average Discharge Plot
###############################
#
# Calculates and plots daily average discharge, Q90,Q10, max/min for Daily river discharge data
# Produces table with # of data for each day (n)
# daily.flowQ --> same thing but using pre-read Q values. 
###############
daily.flow = function(filename, title, record.start = NULL){

	# Read in Data
	data = read.csv(filename, skip = 1)
	data = data[data$PARAM == 1 & year(data$Date) >= ifelse(is.null(record.start), 0, record.start),]
	yrs = paste(year(data$Date[1]), year(data$Date[length(data$Date)]), sep = ' - ')
	data$DOY = yday(data$Date)

	# Stats for each DOY	
	Qmean = c()
	Q90 = c()
	Q10 = c()
	Qmax = c()
	Qmin = c()
	n = c()
	for (i in 1:365){
		subset = data[data$DOY == i,]
		Qmean[i] = mean(subset$Value, na.rm = T)
		Q90[i] = quantile(subset$Value,0.90, na.rm = T)
		Q10[i] = quantile(subset$Value,0.10, na.rm = T)
		Qmax[i] = max(subset$Value, na.rm = T)
		Qmin[i] = min(subset$Value, na.rm = T)
		n[i] = length(subset$Value[is.na(subset$Value)==F])
	}
	
	# New Data colation, plot data
	newdata = data.frame(date = strptime(1:365,'%j'), Qmean, Q90, Q10, Qmax, Qmin, n)
	par(mar = c(4,4.5,3,3))
	plot(newdata$date, Qmean, type = 'l', lwd = 2, ylim = c(0,max(Qmax)),
			xlab = '', ylab = expression(paste('Daily Average Discharge (',m^3,'/s)')), 
			main = title)
	mtext(yrs)
	lines(newdata$date, Q90, col = rgb(0,0,128/255, 0.75))
	lines(newdata$date, Q10, col = rgb(0,0,128/255, 0.75))
	lines(newdata$date, Qmax, col = 'grey60')
	lines(newdata$date, Qmin, col = 'grey60')
	
	legend('topright', c('Average', '10%, 90% Quantiles', 'Max/Min', 
				paste('n = ',length(unique(year(data$Date[is.na(data$Value)==F]))),' yrs', sep = '' ) ), 
			lwd = c(2,1,1,0), lty = 1, col = c('black', rgb(0,0,128/255, 0.75), 'grey60', 'white'),
			bty = 'n')
	newdata
}
# elk_temp = daily.flow('elkriver_fernie_daily.csv', 'Elk River At Fernie', record.start = 1970)

# Daily Flows 
daily.flowQ = function(Q, date, title, record.start = NULL, test.year = NULL){

	data = data.frame(date, Q)
	
	# Remove test year from averages (if selected)
	if(is.null(test.year)==F){
		testdata = subset(data, year(data$date) == test.year) 
		data = subset(data, year(data$date)!= test.year)
		}
	
	yrs = paste(year(data$date[1]), year(data$date[length(data$date)]), sep = ' - ')
	data$DOY = yday(data$date)

	# Stats for each DOY
	Qmean = tapply(data$Q, yday(data$date), mean, na.rm = T )
	Q90 = tapply(data$Q, yday(data$date), quantile, 0.90, na.rm = T)
	Q10 = tapply(data$Q, yday(data$date), quantile, 0.10, na.rm = T)
	# Qmax = tapply(Q, yday(date), max, na.rm = T)
	# Qmin = tapply(Q, yday(date), min, na.rm = T)
	
	# New Data colation, plot data
	newdata = data.frame(date = strptime(1:366,'%j'), Qmean, Q90, Q10)
	par(mar = c(4,4.5,3,3))
	plot(newdata$date, Qmean, type = 'l', lwd = 2, ylim = c(0,max(Q90,na.rm = T)),
			xlab = '', ylab = expression(paste('Daily Average Discharge (',m^3,'/s)')), 
			main = title)
	mtext(yrs)
	lines(newdata$date, Q90, col = rgb(0,0,128/255, 0.75))
	lines(newdata$date, Q10, col = rgb(0,0,128/255, 0.75))
	# lines(newdata$date, Qmax, col = 'grey60')
	# lines(newdata$date, Qmin, col = 'grey60')
	
	# Add Test Year 
	if(is.null(test.year)==F){
		lines(strptime(yday(testdata$date),"%j")[-365], testdata$Q[-365], lwd = 2, col = 'firebrick')
		
		# Add Legend
		legend('topright', c('Average', '10%, 90% Quantiles',  test.year),  
			lwd = c(2,1,2), lty = 1, col = c('black', rgb(0,0,128/255, 0.75), 'firebrick'),
			bty = 'n')
	}else{	
	legend('topright', c('Average', '10%, 90% Quantiles'),  
			lwd = c(2,1), lty = 1, col = c('black', rgb(0,0,128/255, 0.75)),
			bty = 'n')
	}
	# newdata
}
# daily.flowQ(flow$Value, flow$Date, 'Elk River Flows', test.year = 2015)

# Daily Flow for two different Models/Gauges
daily.flows =  function(Q1, Q2, date, title, stats = F){
	yrs = paste(year(date[1]), year(date[length(date)]), sep = ' - ')
	
	# Make the data:
	Qmean = tapply(Q1, yday(date), mean, na.rm = T )
	Qmean2 = tapply(Q2, yday(date), mean, na.rm = T)
	Q90 = tapply(Q1, yday(date), quantile, 0.90, na.rm = T)
	Q10 = tapply(Q1, yday(date), quantile, 0.10, na.rm = T)
	Q290 = tapply(Q2, yday(date), quantile, 0.90, na.rm = T)
	Q210 = tapply(Q2, yday(date), quantile, 0.10, na.rm = T)
	
	# Make Plot
	newdata = data.frame(date = strptime(1:length(Qmean),'%j'), Qmean, Q90, Q10, Qmean2, Q290, Q210)
	par(mar = c(4,4.5,3,3))
	plot(newdata$date, Qmean, type = 'l', lwd = 2, 
	     ylim = c(min(Qmean2-Qmean,na.rm = T), max(c(Q90,Q290),na.rm = T)),
	     xlab = '', ylab = expression(paste('Daily Average Discharge (',m^3,'/s)')), 
	     main = title, col = 'navy')
	lines(newdata$date, Qmean2, lwd = 2, col = 'firebrick')
	mtext(yrs)
	lines(newdata$date, Q90, col = rgb(0,0,128/255, 0.5))
	lines(newdata$date, Q10, col = rgb(0,0,128/255, 0.5))
	lines(newdata$date, Q290, col = rgb(178/255,0, 0, 0.5))
	lines(newdata$date, Q210, col = rgb(178/255,0, 0, 0.5))
	lines(newdata$date, Qmean2-Qmean, lwd = 2, col = 'grey60')
	abline(h=0, lty = 2)
	legend('topright', c('Observed', 'Modelled', '10%, 90% Quantiles', 'Difference' , 
				paste('n = ',length(unique(year(date))),' yrs', sep = '' ) ), 
			lwd = c(2,2,1,2,0), lty = 1, col = c('navy', 'firebrick', 'grey60','grey60', 'white'),
			bty = 'n')
	if(stats == T){
		sim = Q2
		obs = Q1
		fit= lm(sim~obs, data = data, na.action = na.exclude)
		mtext(paste(' R2 = ', round(summary(fit)$r.squared,2), 
		' \n NSE = ', round(NSE(sim, obs),2), 
		' \n RMSE = ', round(rmse(sim, obs),2),' m3/s',
		' \n PBIAS = ', round(pbias(sim,obs),2), '%', sep = ''), 
		cex = 0.9, line = -4, adj = 0, font = 3)
		}
	}
# daily.flows(data$Q, data$Q_HBV, data$date, 'Michel Creek', stats = T)

####################
# Anomaly Analysis #
####################
# Calculates how much a certain day was above/below 
# average value for that day of year/month of year. 
#
# Daily anomaly
daily.anomaly = function(x, date){
	data = data.frame(date, x)
	data$Mean = with(data, ave(x, yday(data$date)))
	with(data, x-Mean)
	# averages = aggregate(x, list(DOY = yday(date)), mean)
	# temp = merge(data.frame(x,date, DOY = yday(date)), averages, by = 'DOY')  
	# head(temp[order(temp$date),])
}
# anomaly.meanT = daily.anomaly(dates, fernie_aws$meanT)
# temp = apply(dataframe[,1:3], 2, daily.anomaly, dates)

# Monthly fraction (normals)
monthly.fraction = function(x, date){
	data = data.frame(date, x)
	data$sum_month = with(data, ave(data$x, mo_yr = paste(month(data$date), year(data$date)), FUN = sum))
	data$av_month = with(data, ave(data$sum_month, month(data$date) ) )
	with(data, x - av_month)
	# averages = aggregate(x, list(DOY = yday(date)), mean)
	# temp = merge(data.frame(x,date, DOY = yday(date)), averages, by = 'DOY')  
	# head(temp[order(temp$date),])
}
# temp = monthly.fraction(dataframe[,4], dates)	

##############################
# Model Performance Analysis #
##############################
# Model fit - can be customized to add autocorrelation plots
fit.plots = function(obs, sim, data, plot.title, autocorrelation = F){
	plot(obs, sim, pch = 19, col = rgb(0,0,0,0.4), main = plot.title, xlab = 'Observed', ylab = 'Modelled')
	abline(0,1, col = 'grey60')
	fit= lm(sim~obs, data = data, na.action = na.exclude)
	mtext(paste('R2 = ', round(summary(fit)$r.squared,2), 
		';  NSE = ', round(NSE(sim, obs),2), 
		'; RMSE = ', round(rmse(sim, obs),2),' m3/s',
		'; PBIAS = ', round(pbias(sim,obs),2), '%', sep = ''), cex = 0.5)
	if(autocorrelation == T){
		ccf(sim, obs,  na.action = na.pass, lwd = 2, 100)
		abline(v = 0, lty = 4, col = 'red')}
}
# fit.plots(subb$xMich13, subb$Mich13, subb, 'Mich 13.3')

################################
# CALCULATE MEAN MONTHLY FLOWS #
################################
# Read in data, produce average monthly flows for period of specified record
Mmmf = function(filename, record.start = NULL){
  data = read.csv(filename, skip = 1)
  data = data[data$PARAM == 1 & year(data$Date) >= ifelse(is.null(record.start),
                                                          0, record.start),]
  date = strptime(data$Date, format = '%Y/%m/%d')
  
  # Aggregate Data
  data = aggregate(data['Value'], list(month = cut(date, breaks = 'month')),  mean)
  data = data.frame(
    tapply(data$Value, month(data$month), mean, na.rm = T), 
    tapply(data$Value, month(data$month), cc)
  )
  colnames(data) = c(unlist(strsplit(filename,'_daily.csv')), 'Months_Sampled')
  row.names(data) = month.abb[1:12]
  data
}
# Example: Find files and make a large table of all data in directory
# files = list.files(pattern = '.csv')
# data <- do.call("cbind", lapply(files, function(x) Mmmf(x, 1970)))

# SECONDARY STEP:
# Calculate Metadata for each site
date.range = function(filename, record.start = 1970){
  data = read.csv(filename, skip = 1)
  data = data[data$PARAM == 1 & year(data$Date) >= ifelse(is.null(record.start),
                                                          0, record.start),]
  date = strptime(data$Date, format = '%Y/%m/%d')
  data.frame(
    Gauge = unlist(strsplit(filename,'_daily.csv')),
    ID = data$ID[1],
    Record = paste(min(year(date)),'-', max(year(date)) )
  )
}
# metadata = do.call('rbind', lapply(files, function(x) date.range(x)))

