rm(list = ls())
library('lubridate')
library('hydroGOF')
library('tidyverse')

#####################################
#           COMPARE SWE             #
#####################################
read.swe = function(x){
  hinton = read.csv(paste0('../SWE/',x), skip = 8)
  hinton$Snow.Water.Equivalent = as.numeric(as.character(hinton$Snow.Water.Equivalent))
  hinton$Date = as.Date(hinton$Date)
  hinton
}

# Function
get.swe.site = function(site){
  model = read.csv(paste0('SNOW_Daily_Average_ByHRUGroup.csv'))[-1,c('HRUGroup.', site)]
  model$HRUGroup. = as.Date(model$HRUGroup.)
  model[,site] = as.numeric(as.character(model[,site]))
  colnames(model) = c('Date', site)
  model
}

# Grassland SWE
CM = inner_join(get.swe.site('CELISTA_MOUNTAIN'), 
              read.swe('1F06P.csv')) %>% 
  select(Date,Simulated = CELISTA_MOUNTAIN, Observed = Snow.Water.Equivalent ) %>%
  gather( Type,Value, -Date ) %>%
  mutate(Site = 'CELISTA_MOUNTAIN')

CC = inner_join(get.swe.site('COOK_CREEK'), 
                read.swe('1E14P.csv')) %>% 
  select(Date,Simulated = COOK_CREEK, Observed = Snow.Water.Equivalent ) %>%
  gather( Type,Value, -Date ) %>%
  mutate(Site = 'COOK_CREEK')

MC = inner_join(get.swe.site('MOUNT_COOK'), 
                read.swe('1F06P.csv')) %>% 
  select(Date,Simulated = MOUNT_COOK, Observed = Snow.Water.Equivalent ) %>%
  gather( Type,Value, -Date ) %>%
  mutate(Site = 'MOUNT_COOK')

AR = inner_join(get.swe.site('ADAMS_R'), 
               read_csv('../SWE/Adams_River_manualSWE.csv') %>%
                 select(Date = `Date of Survey`, Snow.Water.Equivalent = `Water Equiv. mm`) %>%
                 mutate(Date = as.Date(strptime(Date, format = '%y-%m-%d')))) %>% 
  select(Date,Simulated = ADAMS_R, Observed = Snow.Water.Equivalent ) %>%
  gather( Type,Value, -Date ) %>%
  mutate(Site = 'ADAMS_R')

# # Daily SWE
snow_data = rbind(MC, CC, CM, AR)
# # 
snow_data %>%
  ggplot(aes(x = as.Date(Date), y = Value, color = Type)) +
  geom_line() +
  geom_point(data = AR) +
  facet_grid(Site~., scales = 'free') +
  labs(x = '', y = 'SWE (mm)') +
  scale_color_manual('', values = c('navy', 'firebrick')) +
  theme(legend.position = 'top')
# ggsave('Boreal_SWE.png', height = 5, width = 4)

# Average SWE
# snow_data %>%
#   group_by(Site, Type, Date = yday(as.Date(Date))) %>%
#   summarise(Value = mean(Value, na.rm = T))%>%
#   spread(Type, Value)%>%
#   gather(Type, Value, -Date, -Site) %>%
#   ggplot(aes(x = as.Date(Date), y = Value, color = Type)) +
#   geom_point() +
#   facet_wrap(~Site) +
#   labs(x = '', y = 'SWE (mm)', title = 'Athabasca Foothills Average Snowpack 2003 - 2013') +
#   scale_x_date('', date_labels = "%b") +
#   theme_bw() +
#   scale_color_manual('', values = c('navy', 'firebrick')) +
#   theme(legend.position = 'bottom')

# 
# 
snow_data %>%
  spread(Type, Value) %>%
  ggplot(aes(x = Observed, y = Simulated)) +
  geom_point() +
  facet_wrap(~Site) +
  geom_abline(intercept = 0, slope = 1) +
  stat_smooth(method = 'lm', se = F) +
  labs(y = 'Simulated', x = 'Observed', title = 'Daily SWE (mm)')+
  coord_equal()

R2 = function(sim, obs){  summary(lm(obs~sim))$r.squared}

snow_data %>%
  spread(Type, Value) %>%
  group_by(Site) %>%
  summarise(NSE = NSE(Simulated, Observed) %>% round(2),
            PBIAS = pbias(Simulated, Observed),
            R2 = R2(Simulated, Observed)%>% round(2))
