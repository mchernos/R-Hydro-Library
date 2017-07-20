# Make Channel Profiles
rm(list = ls())
library('dplyr')
library('tidyr')

data = readxl::read_excel('../../../Data/WSC/Summary Table.xlsx', sheet = 'Methods Table' )

data$CODENAME = ifelse(is.na(data$`Station Code`),
                       gsub(' ', '_', data$`Station Name`),
                       data$`Station Code`)
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
  Name =  data$`Station Name`[i]
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
fileConn = file('channelprofiles.rvp')
writeLines(x, fileConn)
close(fileConn)
