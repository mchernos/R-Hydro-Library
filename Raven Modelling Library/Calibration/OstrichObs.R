###########################################
# OstrichObs.R
#
# This file produces the Observation section for OSTRICH file inputs.
# It's used in Levenberg-Marqhardt sensitivity analyses
#
# Matt Chernos - Sept 2016

# Read in Discharge data
Q = read.csv('elkfernie_daily.csv', skip = 1)
Q = data.frame(date = strptime(Q$Date,'%Y/%m/%d'), Q = Q$Value)

# Pick data period/length
Q = Q[Q$date >= '2000-09-01',]
Q2 = Q$Q[1:3500] # Size of dataset used for calib.

# Make Table
data = data.frame(obs = paste0('obs',1:length(Q2)), # Name
                  Q2,                 # Streamflow Value 
                  1,                  # Weight (of observation); 1 is default
                  'Elk_Hydrographs.csv',  # File where sim val is located
                  ';',                # ?
                  'time',             # Keyword,'time' follows 1st column of Raven file
                  1:length(Q2),       # Line number (based on keyword 'time')
                  12,                 # Column number (6 Waip; 12 Black)
                  "','")              # Sep.
data = data[complete.cases(data),] # remove NAs
write.table(data, 'ostrich_fernie_streamflow.txt', sep = ' ', row.names = F, quote = F)
 