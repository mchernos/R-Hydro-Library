#####################################
# POND V-A RELATIONSHIPS
# CRANBROOK SPRAY IRRIGATION SITE
#####################################
# by M. Chernos
#####################################

# Read in pond data (Elevation is in ft, Volume is Acres-ft, Acres = Area)
pond1 = read.csv('pond1.csv')
pond2 = read.csv('pond2.csv')

# Convert Elevation from ft to m (1 ft  = 0.3048 m)
pond1$Elevation = pond1$Elevation*0.3048
pond2$Elevation = pond2$Elevation*0.3048

# Convert Volume from Acres-ft to m^3 (1 Acre-ft = 1233.48184 m^3)
pond1$Ac.Ft = pond1$Ac.Ft*1233.48184
pond2$Ac.Ft = pond2$Ac.Ft*1233.48184

# Convert Area from Acres to m^2 (1 Acre = 4046.86 m^2)
pond1$Acres = pond1$Acres*4046.86
pond2$Acres = pond2$Acres*4046.86

###########################
# SMOOTHING RELATIONSHIPS
###########################
res = 0.1  # precision needed for water level data (m)

par(mfrow = c(2,2))
# Pond 1 Volume
plot(pond1$Ac.Ft, pond1$Elevation)
lo1 = loess(pond1$Ac.Ft~pond1$Elevation, span = 0.5)
z1 = seq(round(min(pond1$Elevation)),max(pond1$Elevation),res)
vol1 = predict(lo1, z1)
lines(vol1,z1,col = 'red')

# Pond 2 volume
plot(pond2$Ac.Ft, pond2$Elevation)
lo2 = loess(pond2$Ac.Ft~pond2$Elevation, span = 0.5)
z2 = seq(round(min(pond2$Elevation)),max(pond2$Elevation)+1,res)
vol2 = predict(lo2, z2)
lines(vol2,z2,col = 'red')

# Pond 1 Area
plot(pond1$Acres, pond1$Elevation)
lo1a = loess(pond1$Acres~pond1$Elevation, span = 0.5)
z1a = seq(round(min(pond1$Elevation)),max(pond1$Elevation),res)
area1 = predict(lo1a, z1a)
lines(area1,z1a,col = 'red')

# Pond 2 Area
plot(pond2$Acres, pond2$Elevation)
lo2a = loess(pond2$Acres~pond2$Elevation, span = 0.5)
z2a = seq(round(min(pond2$Elevation)),max(pond2$Elevation)+1,res)
area2 = predict(lo2a, z2a)
lines(area2,z2a,col = 'red')

#######################
# READ IN POND ELEVATION
#######################
pond_data = read.csv('pond_elevation.csv') # Already in (m)

pond1_volume = c()
for(i in 1:length(pond_data$Pond.1)){
	pond1_volume[i] = vol1[z1==pond_data$Pond.1[i]]
}

pond2_volume = c()
for(i in 1:length(pond_data$Pond2)){
	pond2_volume[i] = vol2[z2==pond_data$Pond2[i]]
}

pond1_area = c()
for (i in 1:length(pond_data$Pond.1)){
	pond1_area[i] = area1[z1a==pond_data$Pond.1[i]]
}

pond2_area = c()
for (i in 1:length(pond_data$Pond2)){
	pond2_area[i] = area2[z2a==pond_data$Pond2[i]]
}

# Create usable date class and a full year class
date = strptime(pond_data$Date, format = '%d-%b-%y')
# tempdate = strptime(c('1-Jan-14'), format = '%d-%b-%y') # date range
year = seq(date[1],date[length(date)], by = 86400) # seconds in a day

# Linear Interpolation (and remove Dec 30,31, 2013)
pond1_v = approx(date, pond1_volume, year)$y[-1:-2]
pond1_a = approx(date, pond1_area, year)$y[-1:-2]
pond2_v = approx(date, pond2_volume, year)$y[-1:-2]
pond2_a = approx(date, pond2_area, year)$y[-1:-2]
year  = year[-1:-2]

# Plot the datas
par(mfrow = c(2,2))
plot(date, pond1_volume)
lines(year, pond1_v)

plot(date, pond2_volume)
lines(year, pond2_v)

plot(date, pond1_area)
lines(year, pond1_a)

plot(date, pond2_area)
lines(year, pond2_a)

# Clean up and write file
pond_VA = data.frame(year, pond1_v, pond2_v, pond1_a, pond2_a)
write.csv(pond_VA, 'pond_VA.csv')