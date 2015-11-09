###########################
# CCDST Daily Weather #
###########################
# Aggregates hourly data from Canadian Climate Data Scraping Tool to
# daily means. 
###########################
rm(list=ls())

# Read in hourly output from CCDST for climate station 
yxc = read.csv('test_data.csv')
date = paste(yxc$YEAR,yxc$MONTH,yxc$DAY,yxc$TIME, sep = '-')
yxc$date = strptime(date, format = '%Y-%m-%d-%H:%M')
variable <- c('TMP_C', 'DEWP_TMP_C', 'RHUM_PER', 'WSPD_KMH')

daily = aggregate (yxc[variable], 	# subset of yxc data frame (which variables you want)
			list(day = 				# first 'day is what you want to call the new vector column (this argument creates a column for 'day').
				cut(yxc$date, breaks = 'day')), 	# breaking based on days
							mean, 	# you are calculating daily means (can also do sums, medians etc. )
								na.rm = T)	# additional arguments to be passed for 'mean'. This makes sure you remove NAs (otherwise, anytime there's an NA in the day, you get an NA for the daily mean)


yxc = read.csv('All_Cranbrook_A.csv')
yxc$Date.Time = strptime(yxc$Date.Time, format = '%y/%m/%d %H:%M')
variable <- c('TMP_C', 'DEWP_TMP_C', 'RHUM_PER', 'WSPD_KMH')
daily = aggregate (yxc[variable], list(day = cut(yxc$Date.Time, breaks = 'day')), 
						mean, na.rm = T)
colnames(daily)=c('DAY', 'Tm', 'Td', 'RH', 'WIND_MS')

# Export Daily Mean Data 

data  = data.frame(DAY, Tm, Td, RH, WIND_MS)
write.csv(data, 'All_Cranbrook_A_Daily.csv')

date = day
ta  = data.frame(DAY, Tm, Td, RH, WIND_MS)
write.csv(data, 'All_Cranbrook_A_Daily.csv')
