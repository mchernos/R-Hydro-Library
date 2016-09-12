#######################################################
####### ENVIRONMENT CANADA CLIMATE DATA SCRAPER #######
#######################################################
# By M. Chernos
#
# March 10, 2015 - updated for new URLs Sept. 2016
#######################################################
# Data is from climate.weather.gc.ca and is graciously 
# collected and made available at no cost by Environment 
# Canada. 
#
#
#-----------------
# Dependencies:
#-----------------
library('lubridate')
library('XML')

############################
# DAILY WEATHER DOWNLOADS #
############################
# Location dependent URL
# Full URL looks lke this:
#
# location_url = 'http://climate.weather.gc.ca/climate_data/daily_data_e.html?hlyRange=1953-01-01%7C1995-03-31&dlyRange=1887-11-01%7C1995-03-31&mlyRange=1887-01-01%7C1995-03-01&StationID=2364&Prov=AB&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2016&selRowPerPage=25&Line=0&searchMethod=contains&txtStationName=banff&timeframe=2&Year='
# 1940
# '&Month='
# 1
# '#'

# Go to climate.weather.gc.ca and look up daily site and input url up to 'Year='
# (need to press 'next day' once to get proper html...)

scrape.daily.ec = function(month1, month2, location_url, sitename){
  
  # month1 = '1970-01-01'
  # month2 = '1995-05-31'
  # location_url = 'http://climate.weather.gc.ca/climate_data/daily_data_e.html?hlyRange=1953-01-01%7C1995-03-31&dlyRange=1887-11-01%7C1995-03-31&mlyRange=1887-01-01%7C1995-03-01&StationID=2364&Prov=AB&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2016&selRowPerPage=25&Line=0&searchMethod=contains&txtStationName=banff&timeframe=2&Year='
  
  # Set Date Range
  start = strptime(month1, format = '%Y-%m-%d')
  end = strptime(month2, format = '%Y-%m-%d')
  months = seq(start,end, 'months')
  
  # Loop through Monthly Pages
  weather_data = c()
  for (i in 1:length(months)){
    year =  year(months[i])
    month = month(months[i])	
    
    # Form URL
    
    url = paste(location_url,year, '&Month=', month,'#',sep = '')

    # Scrape data from html pages
    data = readHTMLTable(url)
    data = data.frame(data)
    data = data[-1,]
    col.names = c('day','maxT', 'minT', 'meanT', 'heat_dd', 'cool_dd', 'total_rain_mm', 'total_snow_cm',
                  'total_precip_mm', 'snow_on_grnd_cm', 'dir_max_gust_deg', 'speed_max_gust_kmh')
    
    # Check to see if Data not available for month
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
    
    # Collate Data
    weather_data = rbind(weather_data,data)
  }
  write.csv(weather_data, paste0(sitename, year(start), '_', year(end),'.csv'), row.names = F)
}
# Example:
# location_url = 'http://climate.weather.gc.ca/climate_data/daily_data_e.html?hlyRange=1953-01-01%7C1995-03-31&dlyRange=1887-11-01%7C1995-03-31&mlyRange=1887-01-01%7C1995-03-01&StationID=2364&Prov=AB&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2016&selRowPerPage=25&Line=0&searchMethod=contains&txtStationName=banff&timeframe=2&Year='
# scrape.daily.ec('1928-01-01', '1964-12-31', location_url, 'banff2')

############################
# HOURLY WEATHER DOWNLOADS #
############################

# Note: not updated since URLs changed!

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
# location_url = "Prov=BC&StationID=50818&hlyRange=2012-11-08%7C2015-01-27"   # YXC (Cranbrook)

# day1 = '2013-10-01'
# day2 = '2015-01-17'

scrape.hourly.ec = function(day1, day2, location_url, sitename){
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
    
    # Read the HTML table 
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
  write.csv(weather_data, paste0(sitename, year(start), '_', year(end),'hourly','.csv'), row.names = F)
}	
## END ##