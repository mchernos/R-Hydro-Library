# Make Summary table for Weather Data
rm(list = ls())
# list all files
files = list.files('CCDST')

cc = function(x){length(x[complete.cases(x)==F])}
percent.missing = function(x){length(x[complete.cases(as.numeric(x)) == F]) / length(x)}

# read in data
make.data.summary = function(file){
  temp = read.csv(paste0('CCDST/',file, '/All_', file, '.csv'), na.strings = c("NA", ""),
                  fileEncoding = 'latin1') # needs for random chars.
  
  # Remove some unncessary columns
  temp = temp[,c('YEAR', 'MONTH', 'DAY', 'MAXTMP_C', 'MINTMP_C', 'MEANTMP_C', 'HT_DEGDAY_C',
                 'CL_DEGDAY_C', 'TOT_RAIN_MM', 'TOT_SNW_CM', 'TOT_PRECIP_MM', 'SNW_GRND_CM',
                 'DIRMAXGUST_.10S_DEG.', 'SPDMAXGUST_KMH') ]
  
  # Return column of stats for site
  c( `Start Year` = min(temp$YEAR), 
     `End Year` = max(temp$YEAR),
     `Total Years` = length(unique(temp$YEAR) ),
     apply(temp[,!(names(temp)%in% c('YEAR','MONTH', 'DAY'))], 2, percent.missing ))
}



data = do.call('rbind', lapply(files, make.data.summary))
rownames(data) = files

write.csv(data, 'data_summary.csv')
