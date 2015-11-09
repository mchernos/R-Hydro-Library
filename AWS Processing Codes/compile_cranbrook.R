# Compile YXC data

yxc = read.csv('cranbrook_2014_15weather_datshrly.csv')

hour = matrix(unlist(strsplit(as.character(yxc$Time), ':', fixed = T)),ncol = 2,byrow = T)[,1]
yxc$date = paste(yxc$Year, yxc$Month, yxc$Day, hour, sep = '-') 
yxc$date = strptime(yxc$date, format = "%Y-%m-%e-%H")
head(yxc$date)

# Make proper numeric class for all columns
numchar = function(x){
	as.numeric(as.character(x))
}

relchars = c("RelH", "WindSpdkm.h")
yxc[relchars] = apply(yxc[relchars],2, numchar)

# Daily Means (RH, Windspeed)
daily_mean = aggregate(yxc[relchars],
						list(date = cut(yxc$date, breaks = 'day')), mean, na.rm = T)
daily_mean$WindSpd = daily_mean$WindSpdkm.h/3.6
write.csv(daily_mean,'yxc_precip_wind14_15.csv')
