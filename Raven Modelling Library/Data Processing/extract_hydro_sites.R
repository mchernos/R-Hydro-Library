# Generates .rvt files for Raven for all streamflow observations
# Extracts and processes all hydrometric data from 40 sites used in ARB
#
# Matt Chernos - August 2016
rm(list = ls())
library('dplyr')
library('tidyr')
library('lubridate')

# Function to read in WSC streamflow data
read.data = function(filename, site, record.start = 1980){
  data = read.csv(filename, skip = 1)
  data$Date = strptime(data$Date, format = '%Y/%m/%d')
  data = data[data$PARAM == 1 & year(data$Date) >= ifelse(is.null(record.start), 0, record.start),]
  data = data.frame(date = data$Date, Value = data$Value)
  data
}

# Read in WSC gauge metadata table
sites = readxl::read_excel('../Metadata/Data Sources.xlsx', sheet = 'WSC Sites')
n=1

# Get Site Metadata
site = sites$WSC[n]
name = gsub(' ','_',sites$`Name`[n])
id = sites$`Hydrograph ID`[n]

# Read in data and process
data = read.data('adamssquilax_daily.csv', 'Adams R')
Q = ifelse(is.na(data$Value), -1.2345, data$Value)

# Write .rvt file
fileConn = file(paste0('Flow/', name,'.rvt' ))
writeLines(c(paste0(':ObservationData HYDROGRAPH ',id,' m3/s'),
             paste(data$date[1],'00:00:00 1.0', length(data$date), sep = ' '),
             Q,
             ':EndObservationData'),
           fileConn)
close(fileConn)

# Make List of sites
files = paste0('Flow/',list.files('Flow', pattern = '.rvt'))
command = paste0(':RedirectToFile ', 'Flow/',  files)

fileConn = file('Flow/temprvtfilelist.txt')
writeLines(command, fileConn)
close(fileConn)

#############################
# Generate Channel Profiles #
#############################
library('dplyr')
library('tidyr')

sites$CODENAME = ifelse(is.na(sites$WSC),
                       gsub(' ', '_', sites$`Name`),
                       sites$`WSC`)
data = sites
# :ChannelProfile MOUNTAIN
# :Bedslope 0.008
# :SurveyPoints
# # xsect	depth (m)
# 0.00    1.0
# 1.00    0.0
# 49.00    0.0
# 50.00    1.0
# :EndSurveyPoints
# :RoughnessZones
# # xsect  manning's n
# 0.00    0.50
# :EndRoughnessZones
# :EndChannelProfile

make.channel.hydraulics = function(i){
  Name =  data$`Name`[i]
  B = data$Slope[i]
  Survey_points = c(paste0(0.00, ' ', data$D[i]),
                    paste0(0.05*data$W[i], ' ', 0),
                    paste0(0.95*data$W[i], ' ', 0),
                    paste0(data$W[i], ' ', data$D[i]) )
  n = paste0('0 ', data$`Manning's N`[i] )                
  
  # Collate all factors
  c('# -------',
    paste0('# ', Name ),
    '# -------',
    paste0(':ChannelProfile ', data$CODENAME[i]),
    paste0(':Bedslope ', round(B,4)),
    ':SurveyPoints',
    Survey_points,
    ':EndSurveyPoints',
    ':RoughnessZones',
    n,
    ':EndRoughnessZones',
    ':EndChannelProfile'
  )
}

x = c()

for(i in 1:length(data$CODENAME)){
  temp = make.channel.hydraulics(i)
  x = c(x,temp)
}

# Make file
fileConn = file('Flow/channelprofiles.rvp')
writeLines(x, fileConn)
close(fileConn)

#################################################
# Make Q Reference and Initial Conditions Table #
#################################################

# sites = readxl::read_excel('Summary Table.xlsx', sheet = 'Methods Table')
# 
# # Function to find Q_Ref and Q_initial
# make.Qvalues = function(site){
#   fn = function(x){list.files(x, pattern = '_ddf.csv')[1]}
#   
#   # Read in data and write to file
#   dat = read.data(file.path(site, fn(site)))
#   ID = site
#   
#   # Mean June-July flow (approximates bankfull)
#   Qref = dat[month(dat$date) %in% 6:7,]$Value %>% mean(na.rm = T) %>% round()
#   
#   # mean August flow to be used if Sept 1, 1990 not available
#   Qinit = dat[month(dat$date) == 9,]$Value %>% mean(na.rm = T) %>% round()
#   Qinit2 = ifelse(length(dat$Value[as.Date(dat$date) == as.Date('1990-09-01')] ) > 0,
#                    dat$Value[as.Date(dat$date) == as.Date('1990-09-01')], NA) %>% round
#   Qinit = ifelse(is.na(Qinit2), Qinit, ifelse(Qinit2==0, 1, Qinit2))
#   
#   # Poackage up data for selected site
#   data.frame(ID, Qref, Qinit) 
# }
# # make.Qvalues(sites$`Station Code`[6])
# 
# # Find for all sites --> 1:39 are non-lake ones
# output = do.call('rbind', lapply(sites$`Station Code`[!is.na(sites$`Station Code`)], make.Qvalues))
# 
# # Append to get Hydrograph ID #s
# temp_data = sites %>%  
#   select(`Station Name`, `Hydrograph ID`, `Station Code`, `Temp_Flow`) %>%
#   left_join(output, by = c('Station Code' = 'ID')) %>% 
#   mutate(Qref = ifelse(is.na(Qref), Temp_Flow , Qref),
#          Qinit = ifelse(is.na(Qinit), Temp_Flow, Qinit)) %>%
#   filter(!is.na(Qref))
# 
# # Write data to file
# table.maker <- function(x, file, header,footer){
#   cat(header, '',  file = file)
#   write.table(rbind(x,footer), file, append = T, row.names = F, col.names = F, quote = F)
# }
# # Write Q_INITIAL
# table.maker(temp_data %>% select(`Hydrograph ID`, Qref),
#             'Flow/Q_REFERENCE.txt',
#             c(':SubBasinProperties \n',':Parameters Q_REFERENCE \n',':Units m3/s \n'),
#             c(':EndSubBasinProperties', ''))
# 
# 
# # Make Initial Conditions
# x = readLines('rvc_Template.txt')
# table.maker(temp_data %>% select(`Hydrograph ID`, Qinit),
#             'Flow/Athabasca.rvc',
#             paste0(x[1:9], '\n'),
#             c(x[11],''))
