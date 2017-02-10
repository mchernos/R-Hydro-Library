# Read in all HRUs and get Differencing Sets
# M Chernos - January 25, 2017
#
rm(list = ls())
library('tidyverse')

# Function to read in HRU files
read.hrus = function(file){
  temp = read_csv(paste0('AO_HRUs/',file)) %>% 
    select(hru, COVER)
  colnames(temp) = c('hru', gsub('adams_hrus_', '', gsub('.csv', '',file)))
  temp
}


# Read in all HRU files
files = c('adams_hrus_Forecast_2010.csv',
          paste0('adams_hrus_Forecast_', seq(2020, 2060, 10), '_583.csv') )
x = Reduce(function(x,y){full_join(x,y)}, lapply(files, read.hrus))
types = unique(x$Forecast_2010)

# ALTERNATE VERSION
# Read in all HRU files
# files = c('adams_hrus_Forecast_wcc_2010.csv',
#           paste0('adams_hrus_Forecast_wcc_', seq(2020, 2060, 10), '_687.csv') )
# x = Reduce(function(x,y){full_join(x,y)}, lapply(files, read.hrus))
# types = unique(x$Forecast_wcc_2010)


# Work-horse function to find HRUs that change between decades
find.missing = function(old, new, year){
  if(year == '2020'){xi = 2; xi1 = 3}
  if(year == '2030'){xi = 3; xi1 = 4}
  if(year == '2040'){xi = 4; xi1 = 5}
  if(year == '2050'){xi = 5; xi1 = 6}
  if(year == '2060'){xi = 6; xi1 = 7}
  
  x$hru[x[,xi] == old & x[,xi1] == new]
}
# find.missing('FOREST', 'CUT', '2020')

# Get Changes in HRU Groups and write to File
get.changes = function(old, year){
  # old = 'FOREST'
  X = lapply(types[types != old], find.missing, old, year)
  Xnames = paste(types[types != old], 'TO', old, year, sep = '_')
  
  # Threshold to use change (must have at least 50 HRUs changing)
  xnum = which(lengths(X) > 49)
  
  # Make HRU Groups
  Tx = c()
  for(i in xnum){
   Tx =  c(Tx, 
           paste0(':HRUGroup ',Xnames[i] ),
           paste(X[[i]],collapse = ','),
           paste0(':EndHRUGroup'),
           '#')
   }
  write(Tx, paste0('HRUGroups/',year,'.txt'), append = T)
  
  # Make HRU Lists
  yr1 = ifelse(as.numeric(year)> 2039, as.numeric(year)-59, as.numeric(year)-30)
  # :LandUseChange GLACIER_2040 ALPINE 1986-01-02
  # :HRUTypeChange GLACIER_2040 STANDARD 1986-01-02
  Lx = c()
  for(i in xnum){
    Lx =  c(Lx, 
            paste(':LandUseChange',Xnames[i], 
                   gsub(paste0('_',year), '', unlist(strsplit(Xnames[i], 'TO_'))[2]), paste0(yr1, '-01-01')),
            paste(':HRUTypeChange ', Xnames[i], 
                   gsub(paste0('_',year), '', 'STANDARD'), paste0(yr1, '-01-01')),
            '#')
  }
  write(Lx, paste0('HRUGroups/RVPlist',year,'.txt'), append = T)

  
  # Define HRUs
  if(length(xnum)>0){
    Hx = paste(c(':DefineHRUGroups ', 
                 paste(Xnames[xnum], collapse = ', ')), 
               collapse = '')
    write(Hx, paste0('HRUGroups/HRUlist.txt'), append = T)
  }

}

# Generate HRUGroups and write to file for each year
lapply(types, get.changes, '2020')
lapply(types, get.changes, '2030')
lapply(types, get.changes, '2040')
lapply(types, get.changes, '2050')
lapply(types, get.changes, '2060')

