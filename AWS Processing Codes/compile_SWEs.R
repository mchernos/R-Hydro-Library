# Read in 2011-2014 Data
data = read.csv('Raw Snow Water Equivalent (mm) October 1 2011 to September 30 2014.csv')
# [29] "X2C09Q.Morrissey.Ridge.Snow.Pillow"       
# [30] "X2C10P.Moyie.Mountain.Snow.Pillow"     
# [31] "X2C14P.Floe.Lake.Snow.Pillow"           
data = data[,c(1,29,30,31)]
data$DATE..UTC. = strptime(data$DATE..UTC., format = '%Y-%m-%d %H:%M')
colnames(data) = c('date','morrissey', 'moyie', 'floe' )

# Remove Floe Lake >1250 mm swe
data$floe[data$floe>1250] = NA

plot(data$date, data$morrissey, type = 'l', ylim = c(0,1500))
lines(data$date, data$moyie, col = 'darkgreen')
lines(data$date, data$floe, col = 'navy')

#daily data
data2 = aggregate(data, list(day = cut(data$date, breaks = 'day')), median, na.rm = T)
data2$day = strptime(data2$day, format = '%Y-%m-%d %H:%M:%S')

# Plot
plot(data2$day, data2$morrissey, type = 'l', ylim = c(0,1500),
		ylab = 'SWE (mm)', xlab = '')
lines(data2$day, data2$moyie, col = 'darkgreen')
lines(data2$day, data2$floe, col = 'navy')

legend('topright', c('Morrissey Ridge', 'Moyie Mountain', 'Floe Lake'), 
		col = c('black', 'darkgreen', 'navy'), lwd = 2)
#


# write.csv(data2, 'daily_swe.csv')		
# Combine w/ historical data

#############
# Floe Lake #
#############
floe = read.csv('floelake_daily.csv', skip = 8)
floe = data.frame(floe$Date, floe$Snow.Water.Equivalent)
colnames(floe) = c('date', "floe")
floe$date = strptime(floe$date, '%Y-%m-%d')

##################
# Moyie Mountain #
##################
moyie = read.csv('moyie_daily.csv', skip = 8)
moyie = data.frame(moyie$Date, moyie$Snow.Water.Equivalent)
colnames(moyie) = c('date', "moyie")
moyie$date = strptime(moyie$date, '%Y-%m-%d')

###################
# Morrissey Ridge #
###################
morrissey1 = read.csv('Morrisey_Daily_1979-1983.csv', skip = 8)
morrissey2 = read.csv('Morrisey_Daily_1983-2011.csv', skip = 8)
morrissey1 = data.frame(morrissey1$Date, morrissey1$Snow.Water.Equivalent)
colnames(morrissey1) = c('date', "swe")
morrissey2 = data.frame(morrissey2$Date, morrissey2$Snow.Water.Equivalent)
colnames(morrissey2) = c('date', "swe")
morrissey = rbind(morrissey1,morrissey2)
colnames(morrissey) = c('date', "morrissey")
morrissey$date = strptime(morrissey$date, '%d/%m/%Y')

# Merge Datasets (Moyie>Morrissey>Floe)
swe_temp = merge(moyie,morrissey, by = 'date', all.x = T)
swe_temp = merge(swe_temp, floe, by = 'date', all.x = T)
swe_temp = swe_temp[-(length(swe_temp$date)-1):-length(swe_temp$date),]  # remove two NA stupid frames

# Prep data2 
data2 = data2[,-2]
colnames(data2) = colnames(swe_temp)
swe_data = rbind(swe_temp,data2)
# write.csv(swe_data, 'swe_data.csv')

# Plot
plot(swe_data$date, swe_data$morrissey, type = 'l', ylim = c(0,1500),
		ylab = 'SWE (mm)', xlab = '')
lines(swe_data$date, swe_data$moyie, col = 'darkgreen')
lines(swe_data$date, swe_data$floe, col = 'navy')

legend('top', c('Morrissey Ridge', 'Moyie Mountain', 'Floe Lake'), 
		col = c('black', 'darkgreen', 'navy'), lwd = 2, horiz = T)
#