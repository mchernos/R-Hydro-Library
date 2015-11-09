# Sparwood Climate Record
#
# From Sparwood and Sparwood CS (Environment Canada)
#
# Scraped w/ R code 
#
cc = function(x){
	length(x[complete.cases(x)==F])
}

# Add Sparwood Stn.
spar = read.csv('sparwood_dly80_95.csv')
spar = spar[,-16]
spar2 = read.csv('sparwood_dly95_08.csv')
spar3 = read.csv('sparwood_dly09_14.csv')

spar$date = strptime(paste(spar$Year, spar$Month, spar$day, sep = '-'), format = '%Y-%m-%e')
spar2$date = strptime(paste(spar2$Year, spar2$Month, spar2$day, sep = '-'), format = '%Y-%m-%e')
spar3$date = strptime(paste(spar3$Year, spar3$Month, spar3$day, sep = '-'), format = '%Y-%m-%e')
spar = rbind(spar,rbind(spar2,spar3))

# Add Sparwood CS Stn.
sparcs = read.csv('sparwoodcs_dly95_07.csv')
spar2cs = read.csv('sparwoodcs_dly07_14.csv')

sparcs$date = strptime(paste(sparcs$Year, sparcs$Month, sparcs$day, sep = '-'), format = '%Y-%m-%e')
spar2cs$date = strptime(paste(spar2cs$Year, spar2cs$Month, spar2cs$day, sep = '-'), format = '%Y-%m-%e')
sparcs = rbind(sparcs,spar2cs)

# full_dates = data.frame(date = seq(spar$date[1],spar$date[length(spar$date)],'day'),dummy = 'NA')

# Add Fernie
fernie = read.csv('fernie80_14.csv')
fernie$date = strptime(paste(fernie$Year, fernie$Month, fernie$day, sep = '-'), format = '%Y-%m-%e')
fernie$total_precip_mm = as.numeric(as.character(fernie$total_precip_mm))

# Cases when Sparwood and Fernie have no hourly Total Precip. Data
fernie_precip = fernie$total_precip_mm[91:length(fernie$date)]
spar_rain = as.numeric(as.character(spar$total_precip_mm))
# length(spar_rain[is.na(spar_rain)==T & is.na(fernie_precip)==T])
# [1] 284

# Make matching 1995-2014 rain datas (Sparwood CS) - Gets rid of ~800 NAs
length(spar_rain[is.na(spar_rain)==T])
sparcs_rain = as.numeric(as.character(sparcs$total_precip_mm))
spar_rain[5479:length(spar$date)][is.na(spar_rain[5479:length(spar$date)])==T] = sparcs_rain[is.na(spar_rain[5479:length(spar$date)])==T]

# # length(spar_rain[is.na(spar_rain)==T & is.na(sparcs_rain)==T])
# # [1] 1048

comp = data.frame(ifelse(fernie_precip>0, fernie_precip,NA),
				ifelse(spar_rain>0, spar_rain,NA))
colnames(comp) = c('fernie', 'spar')				
plot(comp$spar, comp$fernie)
adj_factor = mean(comp$spar/comp$fernie, na.rm = T)

# Replace Sparwood rain NAs with Fernie data (adjusted for the fact Fernie rain ~1.5 Sparwood)
spar_rain[is.na(spar_rain)==T] = fernie_precip[is.na(spar_rain)==T] / adj_factor

# data = aggregate(data['Value'], list(year = cut(date, breaks = 'year')), mean, na.rm = T)


# ANNUAL AGGREGATION
annual_rain = aggregate(spar_rain, list(year = cut(spar$date, breaks = 'year')), sum, na.rm = T )
rain_missing = aggregate(spar_rain, list(year = cut(spar$date, breaks = 'year')), cc)

# MAKE SUMMARY TABLE
rain = merge(annual_rain,rain_missing, by = 'year')
rain$adj_rain = rain$x.x/((365-rain$x.y)/365)
colnames(rain) = c('year', 'raw_rain(mm)', 'missing days', 'adj_rain(mm)')
rain
rain$year = strptime(as.character(rain$year), format = '%Y-%m-%d')

# plot(rain$year, rain$adj_rain, type = 'h', lwd = 10,
		 # main = 'Sparwood 1980-2014',
		 # ylab = 'Total Precipitation (mm/yr)',
		 # xlab = '',
		 # ylim = c(0,1200))
# abline(h = mean(rain$adj_rain), lwd = 2, lty = 2)		 

barplot(rain$adj_rain, names.arg = as.character(1980:2014),
		 main = 'Sparwood 1980-2014',
		 ylab = 'Total Precipitation (mm/yr)',
		 xlab = '',
		 ylim = c(0,1200))
abline(h = mean(rain$adj_rain), lwd = 2, lty = 2)
mtext(paste('mean = ', round(mean(rain$adj_rain),0),'mm', sep = ''))		 
# #########
write.csv(rain, 'sparwood_annual_precip.csv')
# rainspar1  = as.numeric(as.character(spar$total_precip_mm[1:5479]))
# ann_rain1 = aggregate(rainspar1, list(year = cut(spar$date[1:5479], breaks = 'year')), sum, na.rm = T )
# rain1_missing = aggregate(rainspar1, list(year = cut(spar$date[1:5479], breaks = 'year')), cc)

# rain1 = merge(ann_rain1,rain1_missing, by = 'year')
# rain1$adj_rain = rain1$x.x/((365-rain1$x.y)/365)
# colnames(rain1) = c('year', 'raw_rain(mm)', 'missing days', 'adj_rain(mm)')
# rain1

# rain2 = rbind(rain1,rain)
# x = rain2[16,]
# rain2 = rain2[-16,]
# rain2[17,2:4] = rain2[17,2:4] + x[2:4]

# rain2