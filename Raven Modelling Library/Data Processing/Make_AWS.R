############################
#
#      FILL AWS DATA
#
# M. Chernos - January 2017
#
############################
#
# Regression Fill Tree: 1980 -- 2015
# Mica
# Blue River
# Vavenby
# Salmon Arm
# Kamloops
# Temperature
############################
rm(list=ls())
#########################
### Read in Functions ###
#########################
source('aws_functions.R')
library('lubridate')
# mergeT() - compares T (min/max/mean) for two sites using elevation
# mergeP() - compares P (rain/snow/total) for two sites
# annual.temp() - summarizes T data + shows # of missing days
# annual.precip() - summarizes P data + missing days
# read.aws.data() - reads AWS data, replaces 'trace' precip w/ 0.1 mm, allows 'estimated'
# fill.T() - uses first dataset T data to fill 2nd dataset NAs using linear regression
# fill.T() - same but for P; can choose direct replacement (instead of linear reg.)
missing.data = function(x){x[which(!complete.cases(x)),]}

#########################
# WEATHER DATA      		          						          # Elevation (m)

# Mica Dam 
mica = read.aws.data('raw/Mica1980_2015')               # 579 m

# Blue River
blueA = read.aws.data('raw/BlueRiverA1980_2015')        # 690 m
blueCS = read.aws.data('raw/BlueRiverCS1993_2015')      # 683 m

# Vavenby
vavenby = read.aws.data('raw/Vavenby1980_2015')         # 445 m

# Kamloops
kamlp = read.aws.data('raw/KamloopsA1970_2013')         # 350 m
kamlpB = read.aws.data('raw/KamloopsA2013_2015')
kamlp = kamlp[year(kamlp$date) > 1979 & as.Date(kamlp$date) < as.Date('2013-06-13'),]
kamlp = rbind(kamlp, kamlpB[-1:-12,])

# Salmon Arm
salm = read.aws.data('raw/SalmonArm1980_1986')          # 345 m
salm2 = read.aws.data('raw/SalmonArm1982_2013')
salm3 = read.aws.data('raw/SalmonArm1994_2015')

# Sun Peaks Mountain
# sunpeak = read.aws.data('SunPeaksMnt1980_2015')       # 1847 m
# sunpeak$date = as.Date(sunpeak$date)

########################################
####    BUILD Mica Dam DATASET    ####
########################################
# TEMPERATURE
# mergeT(mica, blueA, 579, 690, 'Mica', 'Blue River')
# mergeT(mica, blueCS, 579, 680, 'Mica', 'Vavenby')
mT = fill.T(mica, fill.T(blueA, blueCS))

# PRECIPITATION
# mergeP(mica, blueA, 'Mica', 'Blue River' )
# mergeP(mica, vavenby, 'Mica', 'Vavenby' )

mP = fill.P(mica, fill.P(blueA,blueCS, linear = F))
mP = fill.P(mP, vavenby)

mica_data = merge(mT,mP, by = 'date')
mica_data = check.max(mica_data)

########################################
####    BUILD Blue River DATASET    ####
########################################
# TEMPERATURE
# mergeT(blueA, blueCS, 690, 683, 'BlueR', 'BlueR')
# mergeT(blueA, mica, 690, 579, 'BlueR', 'Mica')
bT = fill.T(blueA, blueCS)
bT = fill.T(bT, mica)

# PRECIPITATION
# mergeP(blueA, blueCS, 'BlueR', 'BlueR')
# mergeP(blueA, mica, 'BlueR', 'Mica')
bP = fill.P(blueA, blueCS, linear = F)
bP = fill.P(bP, mica, linear = T)
bP = fill.P(bP, vavenby, linear = F)

blueriver_data = merge(bT,bP, by = 'date')
blueriver_data = check.max(blueriver_data)

########################################
####    BUILD Vavenby DATASET    ####
########################################
# TEMPERATURE
# mergeT(vavenby, blueA, 445, 683, 'BlueR', 'BlueR')
# mergeT(blueA, mica, 690, 579, 'BlueR', 'Mica')
vT = fill.T(vavenby, fill.T(blueA, blueCS))

# PRECIPITATION
# mergeP(vavenby, blueA, 'Vavenby', 'BlueR')
# mergeP(vavenby, kamlp, 'Vavenby', 'Kamloops')
# mergeP(vavenby, salm2, 'Vavenby', 'Salmon Arm')
vP = fill.P(vavenby, fill.P(blueA,blueCS, linear = F), linear = T)
vP = fill.P(vP, fill.P(salm2, salm3, linear = F), linear = T)
vP = fill.P(vP, kamlp, linear = T)

vavenby_data = merge(vT,vP, by = 'date')
vavenby_data = check.max(vavenby_data)

########################################
####    BUILD Salmon Arm DATASET    ####
########################################
# TEMPERATURE
# mergeT(salm2, kamlp, 350, 345, 'Salmon Arm', 'Kamloops')
# mergeT(salm2, vavenby, 350, 448, 'Salmon Arm', 'Vavenby')
sT = fill.T(salm, fill.T(salm2, salm3))
sT = fill.T(sT,  kamlp)
sT = fill.T(sT,  vavenby)

# PRECIPITATION
# mergeP(salm2, kamlp, 'Salmon Arm', 'Kamloops')
# mergeP(salm2, vavenby, 'Salmon Arm', 'Vavenby')
# mergeP(salm2, blueA, 'Salmon Arm', 'Blue River')
sP = fill.P(salm, fill.P(salm2,salm3, linear = F), linear = F)
sP = fill.P(sP, kamlp, linear = T)
sP = fill.P(sP, vavenby, linear = T)
sP = fill.P(sP, blueA, linear = T)

salmonarm_data = merge(sT,sP, by = 'date')
salmonarm_data = check.max(salmonarm_data)

########################################
####    BUILD Kamloops DATASET    ####
########################################
# TEMPERATURE
# mergeT(kamlp, vavenby, 350, 448, 'Kamloops', 'Vavenby')
kT = fill.T(kamlp, fill.T(salm, fill.T(salm2, salm3)))
kT = fill.T(kT,  vavenby)

# PRECIPITATION
# mergeP(salm2, kamlp, 'Salmon Arm', 'Kamloops')
# mergeP(salm2, vavenby, 'Salmon Arm', 'Vavenby')
# mergeP(salm2, blueA, 'Salmon Arm', 'Blue River')
kP = fill.P(kamlp, fill.P(salm, fill.P(salm2,salm3, linear = F), linear = F), linear = F)
kP = fill.P(kP, vavenby, linear = T)

kamloops_data = merge(kT,kP, by = 'date')
kamloops_data = check.max(kamloops_data)

############################
### Write Processed Data ###
############################
# TEMP_DAILY_MAX,TEMP_DAILY_MIN,TEMP_AVE,PRECIP
# vars = c('date','maxT', 'minT', 'meanT', 'precip_mm')
vars = c('maxT', 'minT', 'meanT', 'precip_mm')
write.csv(mica_data[,vars], 'processed/Mica.csv', row.names = F)
write.csv(blueriver_data[,vars], 'processed/BlueRiver.csv', row.names = F)
write.csv(vavenby_data[,vars], 'processed/Vavenby.csv', row.names = F)
write.csv(salmonarm_data[,vars], 'processed/SalmonArm.csv', row.names = F)
write.csv(kamloops_data[,vars], 'processed/Kamloops.csv', row.names = F)

# Make RVT files
# The function
make.rvt = function(file){
  
  # Make Pretty Name
  name = sub('.csv', '', file)
  
  # Make file
  x = readLines('Template.rvt')
  fileConn = file(paste0('AWS/',name,'.rvt') )
  writeLines(c(x[1:4], 
               gsub("\"", "", noquote(readLines(paste0('processed/',file)))[-1]), 
               x[6]), fileConn)
  close(fileConn)
}

# make.rvt('Mica.csv')
#  Find all .csv weather files in directory
files = list.files(path = 'processed',pattern = '*.csv', recursive = T)
lapply(files, make.rvt)

#################
# MAKE AWS LIST #
#################
library('tidyverse')
data = readxl::read_excel('../Metadata/Data Sources.xlsx', sheet = 'AWS') %>%
  mutate(Name = gsub(' ' ,'', gsub('.csv','',Name))) %>%
  filter(Name %in% gsub('.csv','', files))

# :Gauge Kananaskis
# :Latitude 51.03
# :Longitude -115.03
# :Elevation 1391
# :RedirectToFile temp/KananaskisAWS.rvt
# :EndGauge
make.list = function(x){
  c(paste0(':Gauge ',data$Name[x]),
    paste0(':Latitude ',data$Latitude[x]),
    paste0(':Longitude ',data$Longitude[x]),
    paste0(':Elevation ',data$Elevation[x]),
    paste0(':RedirectToFile AWS/',data$Name[x],'.rvt'),
    ':EndGauge ',
    '#')
}

# Write .rvt file list
x = c('# --------------','# Climate Stations List', '# --------------','#')
for(i in 1:length(data$Name)){x = c(x, make.list(i))}
fileConn = file('awslist.txt')
writeLines(x, fileConn)
close(fileConn)

