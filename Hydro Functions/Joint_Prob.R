# JOINT PROBABILITIES
rm(list = ls())

# Mid- Sites (Lethbridge, Calgary, Drumheller)
read.water.data = function(x, name = 'MaxQ'){
	data = read.csv(paste('annualextremes/',x,'_AnnualExtremes.csv', sep = ''))
	data = data[data$PARAM == 1 & is.na(data$MAX) == F,] 	# remove level data (param = 2)
	data = data.frame(data$Year, data$MAX)
	colnames(data) = c('year', name)
	data
	}

regplot = function(x,y, labelname, vartitle = 'Maximum Annual Daily Flow (cms)', ylab = 'Farm', xlab = 'YXC'){
	plot(x,y,main = vartitle, ylab = ylab, xlab = xlab, pch = 21, bg = rgb(0,0,0,0.6))
	# abline(0,1, col = 'grey')
	fit = lm(y~x)
	abline(fit, col = 'red')
	mtext(paste('r.squared = ', round(summary(fit)$r.squared,4)), cex = 0.6)
	text(x,y, labelname)
}

Prob3 = function(Pa,Pb,Pc,corx){	 
	# Joint probability for all 3 correlated variables
	# uses Chain Rule for Probability 
	# P{abc} = P{c|ab}*P{a|b}*P{a}; 
	# where P{c|ab} = P{a|c}*P{b|c}
	rab = corx[1,2] 	# correlation matrix
	rac = corx[1,3]
	rbc = corx[2,3]
	Pa_b = Pa + rab*sqrt((Pa/Pb)*(1-Pa)*(1-Pb))
	Pa_c = Pa + rac*sqrt((Pa/Pc)*(1-Pa)*(1-Pc))
	Pb_c = Pb + rbc*sqrt((Pb/Pc)*(1-Pb)*(1-Pc))
	Pc_ab = Pa_c*Pb_c
	Pabc = Pc_ab * Pa_b * Pa
	Pabc
}

########################
# PRAIRIE RIVER GAUGES #
########################
# Read in data
leth = read.water.data('oldmanlethbridge', 'lethbridge')
yyc = read.water.data('bowcalgary', 'calgary')
drum = read.water.data('reddeerdrumheller', 'drumheller')
water = merge(leth, merge(yyc, drum, by = 'year', all = T), by = 'year', all = T)

# par(mfrow = c(1,3))

# regplot(water$lethbridge, water$calgary, water$year, ylab = 'Lethbridge', xlab = 'Calgary')
# regplot(water$lethbridge, water$drumheller,water$year, ylab = 'Lethbridge', xlab = 'Drumheller')
# regplot(water$calgary, water$drumheller, water$year, ylab = 'Calgary', xlab = 'Drumheller')

corx = cor(water[colnames(water)[-1]], use = "pairwise.complete.obs", method = 'spearman')
Prob3(0.261,0.261,0.183,corx)
Prob3(0.279,0.274,0.189,corx)

##########################
# HEADWATER RIVER GAUGES #
##########################
# Read in data
wald = read.water.data('oldmanwaldrons', 'waldrons')
banff = read.water.data('bowbanff', 'banff')
burnt = read.water.data('reddeerburnttimber', 'burnttimber')
headwater = merge(wald, merge(banff, burnt, by = 'year', all = T), by = 'year', all = T)

# # par(mfrow = c(1,3))

# regplot(headwater$wald, headwater$banff, headwater$year, ylab = 'Waldron\'s Corner', xlab = 'Banff')
# regplot(headwater$wald, headwater$burnt, headwater$year, ylab = 'Waldron\'s Corner', xlab = 'Burnt Timber')
# regplot(headwater$banff, headwater$burnt, headwater$year, ylab = 'Banff', xlab = 'Burnt Timber')

corx2 = cor(headwater[colnames(headwater)[-1]], use = "pairwise.complete.obs", method = 'spearman')
Prob3(0.218, 0.204, 0.278, corx2)
Prob3(0.216,0.239,0.269, corx2)
# http://www.real-statistics.com/correlation/multiple-correlation/
# use to get the multiple r


# whats the probability for 100 yr events in all 3 rivers?
P = 1/100 # 100 yr event

###########################
# CONFLUENCE RIVER GAUGES #
###########################
mouth = read.water.data('oldmanmouth', 'mouth')
bass = read.water.data('bowbassano', 'bassano')
bind = read.water.data('reddeerbindloss', 'bindloss')
headwater = merge(mouth, merge(bass, bind, by = 'year', all = T), by = 'year', all = T)
corx3 = cor(headwater[colnames(headwater)[-1]], use = "pairwise.complete.obs", method = 'spearman')
Prob3(0.239,0.28,0.214, corx3)
Prob3(0.253,0.274, 0.244, corx3)
corx3


# Conditional Probability Graph
Tp = seq(1,200,0.01)
P = 1/Tp
prairie = Prob3(P,P,P,corx)
headwaters = Prob3(P,P,P,corx2)
confl = Prob3(P,P,P,corx3)
x = log(log(Tp))

plot(x, prairie,type = 'l', xaxt = 'n', col = 'navy', 
	ylab = 'Conditional Probaility of Exceedance', xlab = 'Return Period (yrs)')
lines(x, headwaters, col = 'darkgreen')
lines(x, confl, col = 'brown')
name = c(1.01,1.1,2,5,10,20,50,100,200)
axis(1,at = log(log(name)), labels = as.character(name))
abline(v = log(log(name)), lty = 3, col = rgb(0,0,0,.5))
abline(h = seq(0.2,1,0.2), lty = 3,col = rgb(0,0,0,.5))
legend('topright', c('Headwaters', 'Mid-Plains/Prairie', 'Confluence'),
		col = c('darkgreen','navy','brown'), lwd = 2, bg = 'white')
title('Likelihood All 3 Rivers in Site-Type Exceed Threshold')
####################
# Q7S RIVER GAUGES #
####################
library(lubridate)

# running.mean
running.mean = function(x,span, ...){
  y = c(rep(NA,span-1))
  for (i in span:length(x)){
    y[i] = mean(x[(i-span):i], na.rm = T)
  }
  y
}
genQ7 = function(x, y){
	# Read in data 
	data = read.csv(paste('daily/',x,'_daily.csv', sep = ''), skip=1)
	data = data[data$PARAM == 1,] 	# remove level data (param = 2)
	
	# Find Minimum 7-day moving average from each year
	Q7 = running.mean(data$Value, 7)
	data = data.frame(date = data$Date, Q = data$Value, Q7)
	data$date = strptime(data$date, format = '%Y/%m/%d')
	data2 = aggregate(data['Q7'], list(year = cut(data$date, breaks = 'years')), min)
	data2 = data2[is.na(data2$Q7) == F,]
	colnames(data2) = c('year', y)
	data2
}

## Q7s in Prairie
lethQ7 = genQ7('oldmanlethbridge', 'lethbridge')
yycQ7 = genQ7('bowcalgary', 'calgary')
drumQ7 = genQ7('reddeerdrumheller', 'drumheller')
water2 = merge(lethQ7, merge(yycQ7, drumQ7, by = 'year', all = T), by = 'year', all = T)

corx = cor(water2[colnames(water2)[-1]], use = "pairwise.complete.obs", method = 'spearman')
Prob3(0.245,0.198, 0.175, corx)
Prob3(0.257,0.207,0.187,corx)
## Q7s in Headwaters
waldQ7 = genQ7('oldmanwaldron', 'waldrons')
banffQ7 = genQ7('bowbanff', 'banff')
btQ7 = genQ7('reddeerburnttimber', 'burnttimber')
water3 = merge(waldQ7, merge(banffQ7, btQ7, by = 'year', all = T), by = 'year', all = T)

corx2 = cor(water3[colnames(water3)[-1]], use = "pairwise.complete.obs", method = 'spearman')
Prob3(0.219,0.271,0.243, corx2)
Prob3(0.273,0.226,0.232, corx2)

P = 1/100
Prob3(P,P,P,corx)*100
Prob3(P,P,P,corx2)*100
