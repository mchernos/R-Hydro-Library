# Water Valance
# Miscellaneous Scripts
library('tidyverse')
library('lubridate')
vars = c('date', 'snowfall [mm/d SWE]', 'rainfall [mm/day]','Glacier Ice [mm]',
         'Cum. Losses to Atmosphere [mm]','Soil Water[2] [mm]')

data = read_csv(paste0('WatershedStorage.csv'))[,vars]  %>%
    mutate(`Snowfall [mm/d SWE]` = as.numeric(`snowfall [mm/d SWE]`),
           `Rainfall [mm/day]` = as.numeric(`rainfall [mm/day]`),
           `Losses to Atmosphere [mm]` = c(NA, diff(as.numeric(`Cum. Losses to Atmosphere [mm]`))),
           `Glacier Melt [mm]` = c(NA, diff(`Glacier Ice [mm]`)*-1),
           `Soil Water [mm]` = as.numeric(`Soil Water[2] [mm]`),
           `Fraction of Precipitation as Snow` = `Snowfall [mm/d SWE]`/(`Snowfall [mm/d SWE]` + `Rainfall [mm/day]`) ) %>%
    select(date, `Snowfall [mm/d SWE]`,`Rainfall [mm/day]`, 
           `Losses to Atmosphere [mm]`,`Glacier Melt [mm]`, 
           `Soil Water [mm]`,`Fraction of Precipitation as Snow` ) %>%
    gather(Variable, Value, -date) %>%
    group_by(Variable, Date = yday(date)) %>%
    summarise(Value = mean(Value, na.rm = T))

# data = do.call('rbind', lapply(c('Baseline', '2020', '2050'), get.vars)) 

data$Variable = factor(data$Variable, levels = unique(data$Variable)[c(4,5,1,3,2,6)])
data %>%
  ggplot(aes(x = as.Date(strptime(Date, format = '%j')), y = Value)) + 
  geom_line(alpha = 0.25) +
  geom_smooth(span = 0.2, se = F) +
  scale_colour_brewer(palette = 'Set1') + 
  scale_x_date(date_labels = '%b') + 
  # theme_bw() + 
  labs(x = '', y = '', title = 'Adams River Water Balance Components',
       caption = '') + 
  facet_wrap(~Variable, scales = 'free_y') + 
  theme(legend.position = 'bottom')

ggsave('Boreal_WB.png', width = 8, height = 5)
