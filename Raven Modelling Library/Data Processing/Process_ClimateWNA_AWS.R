# Process Climate WNA Outputs to generate future scenarios
rm(list = ls())
library('tidyverse')
library('lubridate')

# Function to read in and process ClimateWNA files
read.climateWNA = function(file){
  
  # Read in Data
  baseline = read_csv(file) 
  
  if('GCM' %in% colnames(baseline)){
    baseline = baseline %>%
      select(-ID1, -Latitude, -Longitude, -Elevation) %>%
      gather(Variable, Value, -ID2, -GCM) }else{
        baseline = baseline %>%
          select(-ID1, -Latitude, -Longitude, -Elevation) %>%
          gather(Variable, Value, -ID2) 
      }
  
  # Need to process data, separate Months from Variable...
  months_num = sprintf("%02s", 1:12)
  for(i in 1:12){
    baseline$Variable = gsub(months_num[i], paste0('-', months_num[i]), baseline$Variable)
  }
  
  baseline %>%
    separate(Variable, c('Variable','Month'),sep = '-') %>%
    mutate(Month = as.numeric(Month)) %>%
    filter(Variable %in% c('Tmax', 'Tmin', 'Tave', 'PPT'))
}

# Compile data
data = rbind(read.climateWNA('Adams_AWS_Normal_1981_2010AM.csv') %>%
               mutate(GCM = 'Baseline'),
             read.climateWNA(file = 'Adams_AWS_6GCMs_AMT.csv') ) %>%
  spread(GCM, Value)

################################ 
# Get Deltas for each scenario #
################################
# Precip
precip = data %>% 
  filter(Variable == 'PPT') %>%
  mutate(`15GCM-Ensemble_rcp45_2055` = `15GCM-Ensemble_rcp45_2055`/Baseline,
         `15GCM-Ensemble_rcp85_2055` = `15GCM-Ensemble_rcp85_2055`/Baseline,
         `CanESM2_rcp45_2025` = `CanESM2_rcp45_2025`/Baseline,
         `CanESM2_rcp85_2025` = `CanESM2_rcp85_2025`/Baseline,
         `CanESM2_rcp45_2055` = `CanESM2_rcp45_2055`/Baseline,
         `CanESM2_rcp85_2055` = `CanESM2_rcp85_2055`/Baseline) %>%
  select(-Baseline)

# Temperature
temp = data %>% 
  filter(Variable != 'PPT') %>%
  mutate(`15GCM-Ensemble_rcp45_2055` = `15GCM-Ensemble_rcp45_2055` - Baseline,
         `15GCM-Ensemble_rcp85_2055` = `15GCM-Ensemble_rcp85_2055` - Baseline,
         `CanESM2_rcp45_2025` = `CanESM2_rcp45_2025` - Baseline,
         `CanESM2_rcp85_2025` = `CanESM2_rcp85_2025` - Baseline,
         `CanESM2_rcp45_2055` = `CanESM2_rcp45_2055` - Baseline,
         `CanESM2_rcp85_2055` = `CanESM2_rcp85_2055` - Baseline) %>%
  select(-Baseline)

# Clean up labels
climate_data = rbind(precip, temp) %>%
  gather(Scenario, Value, -Month, -Variable, -ID2) 

climate_data$Variable = recode(climate_data$Variable, 
                               PPT = 'precip_mm', Tave = 'meanT', 
                               Tmax = 'maxT', Tmin = 'minT')
######################
# Quick Precip Plot  #
######################
climate_data %>%
  filter(Variable == 'precip_mm') %>%
  mutate(Month = cut(Month, 0:12, labels = month.abb[1:12])) %>%
  separate(Scenario, c('Model', 'Scenario', 'Period'), sep = '_') %>%
  filter(Model == 'CanESM2') %>%
  mutate(Period = ifelse(Period == '2025', '2011 - 2040', '2041 - 2070'),
         Model = paste(Model, Scenario, sep = ' '),
         Value = Value - 1) %>%
  group_by(Month, Model, Period) %>%
  summarise(Value = mean(Value)) %>%
  ggplot(aes(x = Month, y = Value, fill = Model)) +
  geom_col(position = 'dodge', colour = 'black') +
  facet_grid(.~Period) +
  scale_fill_brewer('', palette = 'Set1', direction = -1) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = 'top', axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = '', y = 'Change in Monthly Precipitation') +
  guides(fill = guide_legend(ncol = 2))
ggsave('Precip Changes.png', width = 6, height = 4)
# 
# Quick Temp Plot
climate_data %>%
  filter(Variable == 'meanT') %>%
  mutate(Month = cut(Month, 0:12, labels = month.abb[1:12])) %>%
  separate(Scenario, c('Model', 'Scenario', 'Period'), sep = '_') %>%
  filter(Model == 'CanESM2') %>%
  mutate(Period = ifelse(Period == '2025', '2011 - 2040', '2041 - 2070'),
         Model = paste(Model, Scenario, sep = ' ')) %>%
  group_by(Month, Model, Period) %>%
  summarise(Value = mean(Value)) %>%
  ggplot(aes(x = Month, y = Value, fill = Model)) +
  geom_col(position = 'dodge', colour = 'black') +
  facet_grid(.~Period) +
  scale_fill_brewer('', palette = 'Set1', direction = -1) +
  theme(legend.position = 'top', axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = '', y = 'Change in Monthly Average Temperature (*C)') +
  guides(fill = guide_legend(ncol = 2))
ggsave('Temp Changes.png', width = 6, height = 4)


#######################################
# Generate New Daily Data from Values #
#######################################

# Make Folders for each Scenario
lapply(unique(climate_data$Scenario), dir.create)

# site = 'BlueRiver'
# scenario = '15GCM-Ensemble_rcp45_2055'
write.model.aws = function(site, scenario){
  
  # Subset modelled Data
  model = climate_data %>% filter(Scenario == scenario) %>%
    select(-Scenario, Sim = Value)
  
  # Read in Site Data
  obs = read_csv(paste0('../processed/',site,'.csv')) %>%
    mutate(Date = seq(as.Date('1980-01-01'), as.Date('2015-12-31'), by = 'days'), 
           Month = month(Date) ) %>%
    gather(Variable, Observed, -Month, -Date)
  
  temp = left_join(obs, model %>% filter(ID2 == site)) %>%
    mutate(Value = ifelse(Variable == 'precip_mm', Observed*Sim, Observed + Sim)%>% round(2)) %>%
    select(Date, Variable, Value) %>%
    spread(Variable, Value) %>%
    select(maxT, minT, meanT, precip_mm) 
  
  # Ensure Max and mean always warmer then Min
  check.max = function(data){
    data$maxT = ifelse(data$minT > data$maxT, data$minT + 0.1, data$maxT)
    data$meanT = ifelse(data$minT > data$meanT, data$minT + 0.1, data$meanT)
    data
  } 
  temp = check.max(temp)
  
  write.csv(temp, paste0(scenario, '/', site, '.csv'), row.names = F)
}

write.em.all = function(sn){
  sites = unique(climate_data$ID2)
  lapply(sites, write.model.aws, scenario = sn)
}

# And do it!
scenarios = unique(climate_data$Scenario)
lapply(scenarios, write.em.all)

##################
# Make RVT files #
##################
# The function
make.rvt = function(file, sn){
  
  # Make Pretty Name
  name = sub('.csv', '', file)
  
  # Make file
  x = readLines('../Template.rvt')
  fileConn = file(paste0('../',sn,'/',name,'.rvt') )
  writeLines(c(x[1:4], 
               gsub("\"", "", noquote(readLines(paste0(sn,'/',file)))[-1]), 
               x[6]), fileConn)
  close(fileConn)
}

# Make Model directories
lapply(paste0('../',unique(climate_data$Scenario)), dir.create)

#  Find all .csv weather files in directory
make.em.rvts = function(sn){
  files = list.files(path = sn, pattern = '*.csv', recursive = T)
  lapply(files, make.rvt, sn)
}
lapply(scenarios, make.em.rvts)
