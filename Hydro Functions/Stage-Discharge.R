# fits and plots a power-law rating curve.
# incorporates a simple method to find "good"
# starting values for the parameters
#
# NB - this code does not address back-transformation bias
#
# NB - requires package Hmisc to plot error bars - must be installed prior to running
#      this script
#
# 2014 Feb 6  RDM
# Updated by M. Chernos Sept 2016
#######################################################################
rm(list = ls())
# library('Hmisc')
library('dplyr')
library('tidyr')

# read data
discharge = read.csv('07BK001/Daily__Apr-20-2016_06_58_32PM__ddf.csv', skip = 1) %>%
  dplyr::filter(PARAM == 1) %>%
  select(Date, Q = Value) %>%
  mutate(Date = as.Date(Date))

stage = read.csv('07BJ006/Daily__Apr-20-2016_06_57_43PM__ddf.csv', skip = 1) %>%
  dplyr::filter(PARAM == 2) %>%
  select(Date, H = Value) %>%
  mutate(Date = as.Date(Date))

data = inner_join(stage, discharge) %>%
  filter(!is.na(H) & !is.na(Q)) # Remove NAs

# Split data based on weir level
high = data[data$H > 576.61,]
low = data[data$H < 576.61,]

stage = low$H
q = low$Q
logq = log10(q)

# First estimate of parameter values acheived using linear approx.
# determine good starting values for parameters by 
# setting h0 to a value slightly below minimum observed stage
hmin = min(stage)
hmax = max(stage)
cstart = hmin - 0.1*(hmax - hmin)
start.lm = lm( logq ~ log10(stage - cstart) )
astart = start.lm$coefficients[1]
bstart = start.lm$coefficients[2]

# use nls to determine optimal parameters for low data
low.fit = nls(logq ~ a + b*log10(stage - c),
              alg = "plinear",
              start = list(a=astart,b=bstart,c=cstart) )

# Add linear fit for higher water levels
high.fit = lm(Q~H, high)

# plot rating curve
xmin = min(stage)
xmax = max(stage)
dx = 0.01*(xmax - xmin)
x = seq(xmin,xmax,dx)
b = coef(low.fit)
qhat = 10^(b[1] + b[2]*log10(x - b[3]))
qplus = 1.1*q
qminus = 0.9*q

plot(data$H, data$Q,
  xlab = "Stage (m)", 
  ylab = expression("Q ("*m^3*s^{-1}*")"),
  main = "Lesser Slave Lake"
)
lines(x, qhat, col = "red")  
lines(seq(min(high$H), max(high$H), 0.01),
      predict(high.fit,  newdata = data.frame(H = seq(min(high$H), max(high$H), 0.01))),
      col = 'red')


# Generate Predictions
H = seq(575.25, 578.25, by = 0.05 )
high = predict(high.fit, data.frame(H), se.fit = T, interval = 'confidence')$fit
low = 10^predict(low.fit, data.frame(stage = H), se.fit = T, interval = 'confidence')

Qpred = ifelse(H < 576.61, low, high)

Q_pred = ifelse(H < 576.61, 
                10^(b[1] + b[2]*log10(H - b[3])), 
                predict(high.fit, data.frame(H))  )

lines(H, Qpred, col = 'red')


write.csv(data.frame(H, Q_pred), 'Lesser Slave QS.csv', row.names = F)
# plot(H, Q_pred)
# library('ggplot2')
# 
# data %>%
#   ggplot(aes(x = H, y = Q)) + 
#   geom_point() + 
#   stat_smooth()
