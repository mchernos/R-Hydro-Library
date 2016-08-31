#################################
### Adjust for Climate Change ###
#################################
# This code takes daily weather data and PRISM monthly totals  
# to generate daily data for PRISM monthly values. 
# Finally, it calls make_rvt.R to generate .rvt files for Raven.
# 
# Matt Chernos - August 2016

rm(list = ls())
library('dplyr')
library('tidyr')
library('lubridate')

# Read in Observed Datasets
jasper = read.csv('processed/Jasper.csv')
hinton = read.csv('processed/Hinton.csv')
whitecourt = read.csv('processed/Whitecourt.csv')
slavelake = read.csv('processed/SlaveLake.csv')
fortmac = read.csv('processed/FortMcMurray.csv')

# Read in Climate Scenarios
precip = read.csv('../PRISM/PRISM_Factors/ab_precip.csv')[,-2:-4]
precip$Variable = 'precip_mm'
tmean = read.csv('../PRISM/PRISM_Factors/ab_tmean.csv')[,-2:-4]
tmean$Variable = 'meanT'
tmax = read.csv('../PRISM/PRISM_Factors/ab_tmax.csv')[,-2:-4]
tmax$Variable = 'maxT'
tmin = read.csv('../PRISM/PRISM_Factors/ab_tmin.csv')[,-2:-4]
tmin$Variable = 'minT'
x = rbind(precip, tmean, tmax, tmin)


# Make Scaling Factors
make.scaling = function(selected_site, proxy){
  
  # Filter for selected sites
  temp = inner_join(x[x$Name %in% selected_site,] %>% gather(Month, Value, -Name, -Variable),
                    x[x$Name == proxy,] %>% gather(Month, Value, -Name, -Variable) %>% select(-Name), 
                    by = c('Variable', 'Month'))
  
  # Temoerature Factor
  temperatures = temp %>%
    filter(Variable != 'precip_mm') %>%
    mutate(Factor = Value.x - Value.y) %>% 
    select(-Value.x, -Value.y)
  
  # Precip factor
  precip = temp %>%
    filter(Variable == 'precip_mm') %>%
    mutate(Factor = Value.x/Value.y) %>% 
    select(-Value.x, -Value.y)
  
  # Return all factors
  rbind(temperatures, precip)
}
# jasper_scaling = make.scaling(c('Columbia Icefield', 'Brazeau Icefield', 'Scott Glacier', 'Snake Indian Basin'),
#                        proxy = 'Jasper')

# Make Scale data
month_date = strptime(seq(as.Date('1970-01-01'), as.Date('2015-12-01'), by = 'months'), '%Y-%m-%d')

# Make AWS 
gen.site = function(Site, template, scaling){
  
  # Extract Scaling Factors
  vars = filter(scaling, Name == Site)
  scale.var = function(x){spline(month_date, rep(x, 46) , n = 16801)$y}
  
  # Process Data
  temp_data = template
  temp_data$precip_mm = temp_data$precip_mm * scale.var(filter(vars, Variable == 'precip_mm')$Factor)
  temp_data$maxT = temp_data$maxT + scale.var(filter(vars, Variable == 'maxT')$Factor)
  temp_data$minT = temp_data$minT + scale.var(filter(vars, Variable == 'minT')$Factor)
  temp_data$meanT = temp_data$meanT + scale.var(filter(vars, Variable == 'meanT')$Factor)
  temp_data$maxT = ifelse(temp_data$maxT<temp_data$minT, temp_data$minT, temp_data$maxT)
  
  # Round data to clean up.
  temp_data = apply(temp_data, 2, round, 1) # row 1 is date
  
  # Make write to file
  write.csv(temp_data, paste0('processed/PRISM/', Site, '.csv'), row.names = F)
}
# gen.site('Columbia Icefield',jasper, jasper_scaling)


# Make Jasper Sites
jasper_scaling = make.scaling(c('Columbia Icefield', 'Brazeau Icefield', 
                                'Scott Glacier', 'Snake Indian Basin', 
                                'Yellowhead Pass', 'Cairn Pass Lake',
                                'Azure Lake', 'Cadomin'),
                       proxy = 'Jasper')
lapply(unique(jasper_scaling$Name), gen.site, jasper, jasper_scaling)

# Make Hinton Sites
scaling = make.scaling(c('Embarras'), proxy = 'Hinton')
lapply(unique(scaling$Name), gen.site, hinton, scaling)

# Make Whitecourt Sites
scaling = make.scaling(c('Edson', 'Drayton Valley', 'Athabasca','Fox Creek', 'Barrhead'), 
                       proxy = 'Whitecourt')
lapply(unique(scaling$Name), gen.site, whitecourt, scaling)

# Make Lesser Slave Sites
scaling = make.scaling(c('House Mountain Lookout', 'Rock Island Lake Lookout'), proxy = 'Slave Lake')
lapply(unique(scaling$Name), gen.site, slavelake, scaling)

# Make Fort Mac Sites
scaling = make.scaling(c('Birch Mountain Lookout', 'Conklin Lookout'), proxy = 'Fort McMurray')
lapply(unique(scaling$Name), gen.site, fortmac, scaling)

# Source script to make Raven Files --> writes to Headwaters model /AWS
source('make_rvt.R')