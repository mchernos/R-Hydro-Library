##############################################
# CALCULATE AVERAGE FROM MULTIPLE .CSV FILES #
##############################################
# Hi Teresa,
# 
# I've written a function called 'calculate.averages()'
# that will read in the .csv files for the given period, 
# and then write a file to your same working directory
# folder. The file should have a name that follows the 
# template '[startyear]_[end year]_AVERAGE.csv'. 
# All you should need to do to get this code to work is 
# to open this code in the same folder as your files, and 
# run the code. 
# 
# This solution should allow for some changes, if necessary:
# 
#   - if you need to modify the file names, that line of code is the
#    first within the function:
#   files = paste('ElkValley', yr_range,'CanESM2_RCP45r1i1p1_100m.csv', sep = '_')
#     which you can change to suit whatever files you'd like to summarize (maybe you 
#     have RCP8.5 as well?!)
# 
# The first 4 lines of code are simply to clean up your workspace and install
# the 'data.table' package, which makes it quicker to read .csv files.
#
# If you have any additional questions, I'm at mchernos@gmail.com
# 
# Cheers,
# 
# Matt Chernos - Nov 6, 2015
#
###########################################################################

# Remove all remnant data currently in R
rm(list = ls())

# Check for installed packages (install if need be)
packages = c('data.table')
x = lapply(packages, function(x){if (!require(x, character.only = T)) install.packages(x)})
x = lapply(packages, require, character.only = T)
rm(x, packages)

################
#   FUNCTION:  #
################
# reads in data over specified period, 
# and then calculates average over period,
# then write output to a file named '20XX_20XX_AVERAGE.csv', 
#   (where XX is the start and end years)

calculate.averages = function(yr_range){
  
  # Filenames for Period + Read in those data
#   files = paste('ElkValley', yr_range,'CanESM2_RCP45_r1i1p1_100m.csv', sep = '_')
#   data <- do.call("rbind", lapply(files, function(x) data.frame(fread(x)) ) )
  file = paste('ElkValley', yr_range[1], yr_range[length(yr_range)],
                'CanESM2_RCP45r1i1p1_100m.csv', sep = '_')
  non_variables = c('ID1', 'ID2', 'Latitude', 'Longitude', 'Elevation')
  data  = fread(file)
  
  # Aggregate Data for all years
  data = data[,lapply(.SD, mean, na.rm = T), by=non_variables, 
                  .SDcols = colnames(data)[-1:-6]]
  # data = aggregate(data[,-1:-length(non_variables)], data[,non_variables[-1]], mean)
  
  # Write output to file
  write.csv(data,
            paste(yr_range[1], yr_range[length(yr_range)], 'AVERAGE.csv', sep = '_'), 
            row.names = F)
} 
# End function


# Calculate averages for select periods 
calculate.averages(2015:2024) # Output written to file: '2015_2024_AVERAGE.csv'
calculate.averages(2025:2034) # Output written to file: '2025_2034_AVERAGE.csv'
calculate.averages(2035:2044) #...
calculate.averages(2045:2054) #...
calculate.averages(2055:2064) #...

# END CODE
# ~ ~ ~ ~