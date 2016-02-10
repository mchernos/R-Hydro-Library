# Trend Analysis for Michel Creek. 
rm(list = ls())
# source('Hydroagg_functions.R')
library('dplyr')
library('tidyr')
library('ggplot2')
library('lubridate')

# Read in Data and give it good labels
Qdata = read.csv('Michel_Hydrographs.csv')
Qdata = data.frame(Simulated = Qdata$Outside..m3.s.,
                   date = strptime(Qdata$date, '%Y-%m-%d') )
# Remove 1970 [incomplete year, and lots of 0s]
Qdata = Qdata[year(Qdata$date) >1970,]


Qdata$Season = cut(month(Qdata$date), breaks = c(1,3,6,9,12,13),
                   c('Winter', 'Spring', 'Summer', 'Fall', 'Winter1'), right = F )
Qdata$Season = ifelse(Qdata$Season == 'Winter1', 'Winter', as.character(Qdata$Season))
Qdata$Season  = factor(Qdata$Season, levels = c('Winter', 'Spring', 'Summer', 'Fall'))
Qdata$Year = year(Qdata$date)

# Add Observed Data from Michel Cr
obsQ = read.csv('~/Dropbox/Hydrology Consulting Work/CanAus Michel Creek EA/Q/Michel_Natal_daily.csv', 
                skip = 1)
obsQ = data.frame(date = strptime(obsQ$Date,'%Y/%m/%d'), 
                  Observed = obsQ$Value) 
# Join Dataframes
Obs = left_join(Qdata, obsQ) %>%
  group_by(Season, Year) %>%
  summarise(Mean = mean(Observed), Maximum = max(Observed), Minimum = min(Observed)) %>%
  gather(Variable, value = Observed, -Season, -Year)


# Make Mean Flow Trends
prodat = Qdata %>%
  group_by(Season, Year) %>%
  summarise(Mean = mean(Simulated), Maximum = max(Simulated), Minimum = min(Simulated)) %>%
  gather(Variable, value = Simulated, -Season, -Year)


# Join and Make the Plot
left_join(prodat, Obs) %>% 
  gather(Dataset, Q, - Season, -Year, -Variable)%>%
  ggplot(aes(x = Year, y = Q, colour = Dataset) ) + 
  geom_line(alpha = 0.5) + 
  stat_smooth(method = 'lm', lwd = 0.5) + 
  facet_grid(Season~Variable, scales = 'free_y', switch = 'y') + 
  scale_y_log10(breaks = c(1,2,5,10,20,50,100)) + 
  theme_bw() + 
  labs(y = expression(paste('Annual Streamflow (',m^3,'/s)')), x = '') + 
  scale_color_manual(name = NULL, values = c('firebrick', 'navy')) + 
  theme(legend.position = 'top')

# All Year trends (nothing interesting or noteworthy to report...)
# Qdata %>%
#   group_by(Year) %>%
#   summarise(Mean = mean(Q), Maximum = max(Q), Minimum = min(Q)) %>%
#   gather(Variable, value = Q, -Year) %>%
#   ggplot(aes(x = Year, y = Q)) + geom_line(color = 'grey40') + 
#   stat_smooth(method = 'lm') + 
#   facet_grid(.~Variable, scales = 'free_y') + 
#   theme_bw()

# What trends are significant> (shows all seasons and Variables)
library('Kendall')
fac = paste(prodat$Season, prodat$Variable)
MK = function(x){MannKendall(x)$sl}
tapply(prodat$Simulated, fac, MannKendall )
sl = tapply(prodat$Simulated, fac, MK )
cols = ifelse(sl < 0.051, 'Signif', 'Nope' )
# $`Spring Minimum`
# tau = 0.198, 2-sided pvalue =0.056449
# 
# $`Winter Mean`
# tau = 0.31, 2-sided pvalue =0.0024471
# 
# $`Winter Minimum`
# tau = 0.279, 2-sided pvalue =0.0063946

tapply(Obs$Observed, fac, MannKendall )
sl = tapply(Obs$Observed, fac, MK )
ifelse(sl < 0.051, 'Signif', 'Nope' )
# $`Spring Minimum`