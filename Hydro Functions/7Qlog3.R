###############################################################
# Drought Frequency Analysis using Annual Minimum 7-day Flows
###############################################################
# Code adapts elements from R.D. Moore's Gumbel Code, 
# and M. Chernos' Gumbel_Log3_FF code for Extreme Floods.
#
# February 2015
#
#
# Code calculates 7-day moving averages from daily WSC data
# and then finds the minimum for each calendar year. 
# Extreme Frequency analysis done using Log-Pearson Type III. 
# Year is ommited if there is missing days of data. 
###############################################################

rm(list = ls())
library(lubridate)

# running.mean
running.mean = function(x,span, ...){
  y = c(rep(NA,span-1))
  for (i in span:length(x)){
    y[i] = mean(x[(i-span):i], na.rm = T)
  }
  y
}

# Read in data 
data = read.csv('daily/oldmanlethbridge_daily.csv', skip=1)
data = data[data$PARAM == 1,] 	# remove level data (param = 2)

# Find Minimum 7-day moving average from each year
Q7 = running.mean(data$Value, 7)
data = data.frame(date = data$Date, Q = data$Value, Q7)
data$date = strptime(data$date, format = '%Y/%m/%d')
data2 = aggregate(data['Q7'], list(year = cut(data$date, breaks = 'years')), min)
data2 = data2[is.na(data2$Q7) == F,]

# Find Date of lowest Q7 for each year
years = unique(year(data2$year))
min_date = c()
for (i in 1:length(years)){
	temp = data[year(data$date)==years[i],]
	min_date[i] = as.character(temp[which.min(temp$Q7),1])
}

# Consolidate data 
data = data.frame(min_date, Q7 = data2$Q7)

# Specify Q and Labels
Q = data$Q7
graphlab = 'Red Deer River Below Burnt Timber Creek'
yrs = paste(years[1], years[length(years)], sep = ' - ')
Qbreak = 10 								# sequence for horizontal bars

# Generate plotting positions
n = length(Q)
r = n + 1 - rank(-Q)  # lowest Q has rank r = 1
T = (n + 1)/r
return_period = c(1.010,2,5,10,25,50,100,200)


# Set up x axis tick positions and labels
Ttick = c(1.001, 1.01,1.1,1.5,2,3,4,5,6,7,8,9,10,11,12,13,14,15, 16,17,18,19,20,25,30, 35,40,45,50, 						60,70,80,90,100,200)
xtlab = c(1.001,1.01,1.1,1.5,2,NA,NA,5,NA,NA,NA,NA,10,NA,NA,NA,NA,15, NA,NA,NA,NA,20,NA,30,NA,NA,NA,						50,NA,NA,NA,NA,100, 200)
y = -log(-log(1 - 1/T))
ytick = -log(-log(1 - 1/Ttick))
xmin = min(min(y),min(ytick))
xmax = max(ytick)


# Fit a line by method of moments, along with 95% confidence intervals
# KTtick = -(sqrt(6)/pi)*(0.5772 + log(log(Ttick/(Ttick-1))))
# QTtick = mean(Q) + KTtick*sd(Q) 
nQ = length(Q)
# se = (sd(Q)*sqrt((1+1.14*KTtick + 1.1*KTtick^2)))/sqrt(nQ) 
# LB = QTtick - qt(0.975, nQ - 1)*se
# UB = QTtick + qt(0.975, nQ - 1)*se
# max = max(UB)
# # Qmax = max(QTtick)
Qmax = max(Q)

##################
# LOG III METHOD #
##################
logQ = log10(Q)
skew_c = nQ*sum((logQ - mean(logQ))^3)/((nQ-1) * (nQ-2) * (sd(logQ)^3))

# K values for Return Periods
# approximation of kT necessary (can't use table for intermediate values)
# http://www.hydrology.bee.cornell.edu/BEE473Homework_files/RiskAnalysis.pdf
# (Chin 2006)

P = 1 - (1/return_period) 			# non-exceedance probability 1 - 1/return period (stipulated Ts)
k = skew_c/6
w = (log(1/P^2))^0.5
z = w - ((2.515517 + 0.802853*w + 0.010328*w^2) /
 				(1 + 1.432788*w + 0.189269*w^2 + 0.001308*w^3))
kT = z + (z^2 -1)*k + (1/3) * (z^3 - 6*z)*k^2 - (z^2 - 1)*k^3 + z*k^4 + (1/3)*k^5

logQcalc = mean(logQ) + kT * sd(logQ)
ylog = -log(-log(P))
Qcalc = 10^logQcalc

Qmax_log = max(Qcalc)
Qmax = max(c(Qmax,Qmax_log))

###############
## PLOTTING ###
###############

# Plot peak flow series with Gumbel axis
plot(y, Q,
  ylab = '' ,
  xaxt = "n", xlab = "Return Period (yr)",
  ylim = c(0, Qmax),
  xlim = c(xmin, xmax),
  pch = 21, bg = "red",
  main = graphlab
)  
mtext(yrs)
par(cex = 0.65)
axis(1, at = ytick, labels = as.character(xtlab))
mtext(expression( "Annual Minimum 7-Day Average Flow ("*m^3*s^{-1}*")" ), 2, 2.5)

# Add fitted line and confidence limits
# lines(ytick, QTtick, col = "black", lwd = 2)  
# lines(ytick, LB, col = "black", lty = 3, lwd = 3)
# lines(ytick, UB, col = "black", lty = 3, lwd = 3)  


# Draw grid lines
abline(v = ytick, lty = 3)         
# abline(h = seq(0.5, floor(Qmax), 0.5), lty = 3)  
abline(h = seq(0,floor(Qmax), round(Qmax, -1)/Qbreak), lty = 3)           
par(cex = 1) 

# Plot Log III
lo = loess(Qcalc~ylog, span = 0.5)
ymod = seq(min(ylog), max(ylog), 0.01)
ymod2 = seq(min(ylog), max(ylog), 0.0001)
Qpred = predict(lo, ymod)
Qpred2 = predict(lo, ymod2)
lines(ymod, Qpred , col = 'blue', lwd = 2)

# Add Error bars (Standard Error)
se_log = (sd(Q)*sqrt((1+1.14*kT + 1.1*kT^2)))/sqrt(nQ) 
lo_se = loess(se_log~ylog, span = 0.5)

LB_log = Qpred - qt(0.975, nQ - 1)*predict(lo_se, ymod)
UB_log = Qpred + qt(0.975, nQ - 1)*predict(lo_se, ymod)

lines(ymod, LB_log, lwd = 3, lty = 3,  col = 'blue')
lines(ymod, UB_log, lwd = 3, lty = 3, col = 'blue')

# Print and write Summary Table
summary_table = data.frame(Return_Period = Ttick) 
						   # Gumbel_Flow = round(QTtick,1), 
						   # SE = round(se,1))
data_table = data.frame(summary_table[Ttick %in% return_period[-1],],
						   LogIII_Flow = round(Qcalc[-1],1),
						   SE_Log = round(se_log[-1],1)
						          )
data_table
# write.csv(data_table, paste(graphlab,yrs, '.csv', sep = ''), row.names = F)

###################################
#### Tests for Goodness of Fit ####
# Calculate expected values from return periods (T)

# GUMBEL METHOD observed, expected 
# KTexp = -(sqrt(6)/pi)*(0.5772 + log(log(T/(T-1))))
# Qgumb = mean(Q) + KTexp*sd(Q) 

# Kolmogorov-Smirnov Test
# 1 - ks.test (so p value < 0.05 = 95% confident that it is a 'good fit')

# LOG III METHOD observed, expected
P = 1-(1/T) 			# exceedance probability 1/return period
k = skew_c/6
w = (log(1/P^2))^0.5
z = w - ((2.515517 + 0.802853*w + 0.010328*w^2) /
 				(1 + 1.432788*w + 0.189269*w^2 + 0.001308*w^3))
kT_ob = z + (z^2 -1)*k + (1/3) * (z^3 - 6*z)*k^2 - (z^2 - 1)*k^3 + z*k^4 + (1/3)*k^5
logQob = mean(logQ) + kT_ob * sd(logQ)
Qlog3 = 10^logQob

# Gumbel Method
# gp = round(1 - ks.test(Q,Qgumb, alternative = 'two.sided', simulate.p.value = T)$p.value, 4)
# # round(ks.test(Q,Qgumb, alternative = 'two.sided')$p.value, 4)

# Log III Method
lp = round(1 - ks.test(Q,Qlog3, alternative = 'two.sided', simulate.p.value = T)$p.value, 4)

# Add Legend
legend('topright', c(paste('Log 3 (p = ',lp,')', sep = ''), 
					'Standard Error'), 
		col = c('blue'), lty = c(1,3),
		lwd = c(2,3), border = 'n', bg = 'white')
		
		
#################
### Quantiles ###
#################

# Type 8 => m = (p+1)/3. p[k] = (k - 1/3) / (n + 1/3). Then 
# p[k] =~ median[F(x[k])]. The resulting quantile estimates 
# are approximately median-unbiased regardless of the
# distribution of x. 
# Hyndman, R. J. and Fan, Y. (1996) 
# Sample quantiles in statistical packages, 
# American Statistician 50, 361–365.

lower_q = quantile(Q, c(0.25), type = 8)		
higher_q = quantile(Q, c(0.75), type = 8)	

# Using Log 3 (Qpred)
rounding = 3
y_lower = ymod2[round(Qpred2, rounding) == round(lower_q, rounding)]
y_higher = ymod2[round(Qpred2, rounding) == round(higher_q, rounding)]

# y = -log(-log(1 - 1/T))
P25 = mean((1 - exp(-exp(-y_lower)))) 
P75	= 1 - mean((1 - exp(-exp(-y_higher))))

# P2 = (1 - exp(-exp(-ymod2)) )
# plot(P,Qpred2, type = 'l', lwd = 2)
# abline(h = quantile(Q,seq(0.25,0.75,0.25)), col = 2:4)
# abline(v = c(P25, P75), col = c(2,4))

P25		# Non-Exceedance
P75		# Exceedance
summary(Q)
n
# data[order(data$Q7),]