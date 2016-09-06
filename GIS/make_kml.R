# MAke kml file and summarise WSC data
# This R script takes the standard WSC table of data,
# and summarises it, and then shows how to convert lat-long 
# into a usable format and then make a kml file of all Stns.
# 
# 
#  Matt Chernos - Sept 2016

library('dplyr')
library('tidyr')
library('readxl')
library('rgdal')

# Read in and summarise Data
data = read_excel('SE_BC-AB_WSC_Sites.xlsx') %>%
  separate(Years, c('Start', 'End'), sep = '-', convert = T ) %>%
  mutate(n.years = End-Start) %>%
  arrange(Prov, -n.years, -End, -Start) 

# Write to file
write.csv(data, 'BC-AB_WSC_Sites.csv', row.names = F)

###################
# Write .kml file #
###################
# Fix coords. by removing degrees
lat = as.numeric(sp::char2dms(sub('°', 'd', as.character(data$Latitude))))
long = as.numeric(sp::char2dms(sub('°', 'd', as.character(data$Longitude))))

# Make Spatial Points data frame
x = SpatialPointsDataFrame(data.frame(long, lat), data.frame(data, long, lat), 
                           proj4string = CRS("+proj=longlat +datum=WGS84"))
# Write a kml file
writeOGR(x, 'SEBC.kml', 'SEBC', 'KML')


