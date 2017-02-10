# Foothills Streamflow Comparisons
##########################
# - plots against modelled data
#
# - M Chernos - JUNE 2016
#
library('tidyverse')
library('hydroGOF')
library('lubridate')

# Prep Data
data = read.csv('Hydrographs.csv')[,c(-1,-3,-4, -9)] %>%
  gather(Site, Value, -date) %>% 
  separate(Site, c('Site', 'Type'), sep = '[.].')  %>%
  mutate(Type = factor(ifelse(Type == 'm3', 'Simulated', 'Observed'), 
                       levels = c( 'Observed','Simulated'))) %>%
  mutate(Scenario = ifelse(year(date) < 2003, 
                           'Validation','Calibration'),
         Site = gsub('_', ' ', Site))

data = data[-1:-100,]
# Facet Plot of all Sites
data %>% 
  filter(Site == 'Adams River Near Squilax') %>%
  group_by(Site, date = yday(date), Type) %>% # This and next line make average daily streamflow
  summarise(Value = mean(Value, na.rm = T)) %>%
  ggplot(aes(x = as.Date(date, format = '%Y-%m-%d'), y = Value, colour = Type)) + 
  geom_line() + 
  theme_bw() + 
  theme(legend.position = 'bottom') +
  # facet_grid(Site~Scenario) + 
  scale_x_date(date_labels = '%b') + 
  scale_colour_manual('', values = c( 'blue3', 'firebrick')) + 
  labs(x = '', y = expression(Streamflow~(m^3/s)),
       title = 'Adams River Near Squilax',
       subtitle = '1981 - 2010')
ggsave('../Figures/AdamsStreamflow.png', width = 6, height = 4)
# Calc monthly NSE
x = data %>%
  spread(Type, Value) %>%
  group_by(Site, Scenario) %>%
  # group_by(Site, Scenario) %>%
  summarise(NSE = NSE(Simulated, Observed) %>% round(2),
            PBIAS = pbias(Simulated, Observed))
x
# write.csv(arrange(x, Scenario), 'Elk_Stats.csv', quote = F)
# 
# ggsave('Adams_Streamflow.png', width = 8, height = 4)
