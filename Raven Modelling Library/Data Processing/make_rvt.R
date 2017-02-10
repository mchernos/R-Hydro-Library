# This code makes .rvt files from AWS .csv data
# It's naming comes from a table that contains names, 
# lon-lat and elevation to make the table.
#
# Matt Chernos August 2016. 
#
rm(list = ls())
library('dplyr')

# The function
make.rvt = function(file){
  
  # Make Pretty Name
  name = gsub(' ','',gsub('PRISM/', '',gsub('.csv', '',file)))
  
  # Make file
  x = readLines('Template.rvt')
  fileConn = file(paste0('../../Athabasca Basin Hydrological Model/Headwaters/AWS/',name,'.rvt') )
  writeLines(c(x[1:4], 
               gsub("\"", "", noquote(readLines(paste0('processed/',file)))[-1]), 
               x[6]), fileConn)
  close(fileConn)
}


#  Find all .csv weather files in directory
files = list.files(path = 'processed',pattern = '*.csv', recursive = T)
lapply(files, make.rvt)


# Make table
data = read.csv('../PRISM/PRISM_Factors/ab_precip.csv')[,c(1,3,2,4)] %>% arrange(Elevation)
data$Name = gsub(' ','',data$Name)
data[,2:3] = apply(data[,2:3], 2, round, 2)


# :Gauge Kananaskis
# :Latitude 51.03
# :Longitude -115.03
# :Elevation 1391
# :RedirectToFile temp/KananaskisAWS.rvt
# :EndGauge
make.list = function(x){
  c(paste0(':Gauge ',data$Name[x]),
    paste0(':Latitude ',data$coords.x2[x]),
    paste0(':Longitude ',data$coords.x1[x]),
    paste0(':Elevation ',data$Elevation[x]),
    paste0(':RedirectToFile AWS/',data$Name[x],'.rvt'),
    ':EndGauge ',
    '#')
}

# Write .rvt file list
x = c('# --------------','# Climate Stations List', '# --------------','#')
for(i in 1:length(data$Name)){x = c(x, make.list(i))}
fileConn = file('../../Athabasca Basin Hydrological Model/Headwaters/Misc/awslist.txt')
writeLines(x, fileConn)
close(fileConn)



# write.csv(data, '../../Athabasca Basin Hydrological Model/Misc/AWS.csv', row.names = F)
# write.csv(data.frame(rnorm(10)), paste0('../../Athabasca Basin Hydrological Model/Headwaters/AWS/matt','.csv') )