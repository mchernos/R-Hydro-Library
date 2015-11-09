#######################################################
####### ENVIRONMENT CANADA CLIMATE DATA SCRAPER #######
#######################################################
# By M. Chernos
#
# March 10, 2015
#######################################################
# Data is from climate.weather.gc.ca and is graciously 
# collected and made available at no cost by Environment 
# Canada. 
#
#
#-----------------
# Dependencies:
#-----------------
library(lubridate)
library(XML)

############################
# HOURLY WEATHER DOWNLOADS #
############################

# Location dependent URL
# Full URL looks lke this:
#
# http://climate.weather.gc.ca/climateData/hourlydata_e.html?timeframe=1&
# Prov=BC&StationID=51442&hlyRange=2013-06-11%7C2015-01-25    <--- This is the part to parse
# &Year=2014&Month=6&Day=26&cmdB1=Go
#
# http://climate.weather.gc.ca/climateData/hourlydata_e.html?timeframe=1&
# Prov=BC&StationID=51442&hlyRange=2013-06-11%7C2015-01-25    <--- This is the part to parse
# &Year=2014&Month=6&Day=26&cmdB1=Go
# Go to climate.weather.gc.ca and look up hourly site and input url up to "Year=" (need to press 'next day' once to get proper html...)

# location_url = 'Prov=AB&StationID=27211&hlyRange=2008-12-22%7C2015-01-25'   # YYC Airport
# location_url = 'Prov=NU&StationID=1754&hlyRange=1984-12-03%7C2015-01-23'    # Grise Fiord
# location_url = "Prov=BC&StationID=51442&hlyRange=2013-06-11%7C2015-01-25"   # YVR Airport
location_url = "Prov=BC&StationID=50818&hlyRange=2012-11-08%7C2015-01-27"   # YXC (Cranbrook)


day1 = '2013-10-01'
# day2 = '2014-10-03'
day2 = '2015-01-17'
start = strptime(day1, format = '%Y-%m-%d')
end = strptime(day2, format = '%Y-%m-%d')
days = seq(start, end, 'days')

weather_data = c()
for (i in 1:length(days)){
	year =  year(days[i])
	month = month(days[i])
	day = day(days[i])
	
	preamb = 'http://climate.weather.gc.ca/climateData/hourlydata_e.html?timeframe=1&'
	start_url = paste('http://climate.weather.gc.ca/climateData/hourlydata_e.html?timeframe=1&',location_url,'&cmdB1=Go&Year=', sep = '')
	mid_url = '&Month='
	mid2_url = '&Day='
	end_url = "&cmdB1=Go#"
	url = paste(start_url, year, mid_url, month, mid2_url, day, end_url, sep = '')

	
	data = readHTMLTable(url)
	data = data.frame(data[2])
	data = data[-1,]
	data$Year = year
	data$Month = month
	data$day = day
	colnames(data) = c("Time", 'TempC', 'DewTempC', 'RelH', 'WindDir', "WindSpdkm.h", "Visibilitykm","PresskPa", "Hmdx", "WindChill", "Weather","Year","Month","Day")
	
	# Fix dumb time 'Legend add-on
	data$Time = matrix(unlist(strsplit(as.character(data$Time), ':', fixed = T)),ncol = 2,byrow = T)[,1]

# Collate Data
	weather_data = rbind(weather_data,data)
}

# Write .csv
	write.csv(weather_data,'cranbrook_2014_15weather_dats.csv', row.names = F)
	
	
############################
# DAILY WEATHER DOWNLOADS #
############################
# Location dependent URL
# Full URL looks lke this:
#
# http://climate.weather.gc.ca/climateData/dailydata_e.html?timeframe=2&
# Prov=QC&StationID=5415&dlyRange=1941-09-01%7C2014-07-31   <--- This is the part to parse out
# &cmdB1=Go&Year=2014&Month=8&cmdB1=Go#
#
# Go to climate.weather.gc.ca and look up daily site and input url from "Prov=" to "&cmbB1="
# (need to press 'next day' once to get proper html...)


month1 = '1970-01-01'
month2 = '1995-05-31'
start = strptime(month1, format = '%Y-%m-%d')
end = strptime(month2, format = '%Y-%m-%d')
months = seq(start,end, 'months')

# location_url = "Prov=BC&StationID=50818&dlyRange=2012-11-08%7C2015-01-27" # YXC Cranbrook
# location_url = "Prov=BC&StationID=1207&dlyRange=1980-03-01%7C2015-01-27"    	# Sparwood, BC
location_url = "Prov=BC&StationID=6842&dlyRange=1992-11-01%7C2015-01-27"  # Sparwood CS, BC
# location_url = "Prov=BC&StationID=1204&dlyRange=1969-05-01%7C1980-03-31"	# NATAL KAISER RESOURCES (old Sparwood)
# location_url = "Prov=BC&StationID=1180&dlyRange=1913-12-01%7C2015-01-28"  # Fernie, BC
# location_url = "Prov=AB&StationID=2381&dlyRange=1912-10-01%7C1997-09-30" 	# Coleman, AB
# location_url = "Prov=&StationID=1170&dlyRange=1977-06-01%7C1993-07-31"		# Corbin, BC
# location_url = "Prov=&StationID=10890&dlyRange=1993-06-04%7C2015-03-04"	# Crowsnest, AB
# location_url = "Prov=AB&StationID=2366&dlyRange=1912-11-01%7C2012-03-31"	# Beaver Mines, AB (until 2012)
# location_url = "Prov=AB&StationID=50028&dlyRange=2012-02-07%7C2015-03-07"	# Beaver Mines, AB (2012-present)
location_url = "Prov=BC&StationID=1179&dlyRange=1923-10-01%7C1995-05-31&"	# Elko, BC

weather_data = c()
for (i in 1:length(months)){
	year =  year(months[i])
	month = month(months[i])	
	# Form URL
	start_url = paste("http://climate.weather.gc.ca/climateData/dailydata_e.html?timeframe=2&",location_url,"&cmdB1=Go&Year=",sep = '')
	mid_url = '&Month='
	end_url = "&cmdB1=Go#"
	url = paste(start_url, year, mid_url, month, end_url,sep = '')

# Scrape data from html pages
	data = readHTMLTable(url)
	data = data.frame(data[2])
	data = data[-1,]
	col.names = c('day','maxT', 'minT', 'meanT', 'heat_dd', 'cool_dd', 'total_rain_mm', 'total_snow_cm',
		 'total_precip_mm', 'snow_on_grnd_cm', 'dir_max_gust_deg', 'speed_max_gust_kmh')

	# Check to see If Data not available for month
	if( is.null(dim(data)) == F ){
		colnames(data) = col.names
		data = data[-which(data$day=='Sum'):-length(data$day),]
	} else {
		# If no data for the month, construct an empty month to bind data to
		nmonth = ifelse(month < 12, month+1, 1)
		nyear = ifelse(month == 12, year+1, year)
		nd = strptime(c(paste('01',month,year), paste('01',nmonth,nyear)), format = '%d %m %Y')
		new_dates =seq(nd[1], nd[2], by = 'day')
		new_dates = new_dates[-length(new_dates)]
		data = matrix(NA, ncol = length(col.names), nrow = length(new_dates))
		data = apply(data, 2, as.factor)
		data[,1] = as.factor(day(new_dates))
		colnames(data) = col.names
		data = data.frame(data)
		}	
	data$Year = year
	data$Month = month

	# data$day = matrix(unlist(strsplit(as.character(data$day), ' ', fixed = T)),ncol = 4,byrow = T)[,1]
# Collate Data
	weather_data = rbind(weather_data,data)
}
	write.csv(weather_data,'elko70_95.csv', row.names = F)

# # Keep estimates ('LegendEE')
# test = weather_data[,i]
# test[grep('+LegendEE',test)] = as.numeric(strsplit(as.character(test[grep('+LegendEE',test)]),'LegendEE', collapse = T))
# test[grep('+LegendEE',weather_data[,i])] = strsplit(as.character(sub),"LegendEE")


# by(weather_data,function(x) )
# Write .csv
	# write.csv(weather_data,'weather_dats.csv')

## END ##