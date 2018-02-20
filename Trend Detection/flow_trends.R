############################
#
#  Elk River Flood Trends
#
#
# M. Chernos - April 2015
#############################
# Examines how drivers of flooding in the Elk Valley have changed
# 
rm(list = ls())
source('Hydroagg_functions.R')

#####################
# DISCHARGE  TRENDS #
#####################
Q.trends = function(site, title, record.start = 0 ){
	# running.mean
		running.mean = function(x,span, ...){
		  y = c()
		  for (i in span:length(x)){
		    y[i] = mean(x[(i-span):i], na.rm = T)
		  }
		  y
		}
	# MAF trends?
	elk_fernie = maf(paste(site,'_daily.csv',sep= ''), record.start = record.start)
	
	# Annual Extremes Trends?	
	data = read.csv(paste(site,'_annualextremes.csv',sep = ''))
	data = data[data$PARAM == 1 & is.na(data$MAX) == F & data$Year >= record.start,] 	# remove level data (param = 2)
	
	par(mfrow = c(1,3))
	
	par(mar = c(4,4.5,3,3), oma = c(0,0,2,0))
	
	# MAF
	plot(elk_fernie$year, elk_fernie[,2], pch = 19, xlab = '', main = 'Mean Annual Flow', 
		ylab = expression(paste('Q (',m^3,'/s)')))
	fit = lm(elk_fernie[,2]~elk_fernie$year)
	abline(fit, col = 'red', lwd = 2)
	mtext(paste('Q = ',round(fit$coefficient[2],4),'t + ',round(fit$coefficient[1],4), sep = '') , line = -1, cex = 0.8)
	mtext(paste('R^2',' = ', round(summary(fit)$r.squared,4)), line = -2, cex = 0.8)	
	mtext(paste('p.value (slope) = ', round(summary(fit)$coefficients[8],4) ), line = -3, cex = 0.8)
	lines(elk_fernie$year, running.mean(elk_fernie[,2], 5), lwd = 2, col = 'grey60')

	# MAX
	plot(data$Year, data$MAX, pch = 19, xlab = '', main = 'Annual Maximum Daily Flow',
		ylab = expression(paste('Q (',m^3,'/s)')))
	fit = lm(data$MAX~data$Year)
	abline(fit, col = 'red', lwd = 2)
	mtext(paste('Q = ',round(fit$coefficient[2],4),'t + ',round(fit$coefficient[1],4), sep = '') , line = -1, cex = 0.8)
	mtext(paste('R^2',' = ', round(summary(fit)$r.squared,4)), line = -2, cex = 0.8)	
	mtext(paste('p.value (slope) = ', round(summary(fit)$coefficients[8],4) ), line = -3, cex = 0.8)
	lines(data$Year, running.mean(data$MAX, 5), lwd = 2, col = 'grey60')

	# MIN
	plot(data$Year, data$MIN, pch = 19, xlab = '', main = 'Annual Minimum Daily Flow',
		ylab = expression(paste('Q (',m^3,'/s)')))
	fit = lm(data$MIN~data$Year)
	abline(fit, col = 'red', lwd = 2)
	mtext(paste('Q = ',round(fit$coefficient[2],4),'t + ',round(fit$coefficient[1],4), sep = '') , line = -1, cex = 0.8)
	mtext(paste('R^2',' = ', round(summary(fit)$r.squared,4)), line = -2, cex = 0.8)	
	mtext(paste('p.value (slope) = ', round(summary(fit)$coefficients[8],4) ), line = -3, cex = 0.8)
	lines(data$Year, running.mean(data$MIN, 5), lwd = 2, col = 'grey60')

	mtext(title, outer = T, cex = 1.2, font = 2)
}
Q.trends('elkriver_fernie', 'Elk River at Fernie', 1970)
Q.trends('elkriver_natal', 'Elk River Below Natal', 1970)
Q.trends('linecreek', 'Line Creek at the Mouth', 1970)
Q.trends('fordingriver', 'Fording River at the Mouth', 1970)
