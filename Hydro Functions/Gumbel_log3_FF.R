###############################################################
# Flood Frequency Analysis using Extreme Annual Flows
###############################################################
#
# Code for plotting annual peak flow series on
# extreme-value (Gumbel) paper.
#
# The code also now does the Log-Pearson Type III Method :)
#
# This code illustrates how to customize graph axes, and 
# also how to use superscripts in axis labels.
#
# RDM 2014 Feb 6 - Adapted by M. Chernos, Feb 2015
# April 2015     - Turned into function (M. Chernos)
# May 2015		 - Added ability to weigh prob. of yearly event happening again
# Feb 2018 - updated to take a data.frame instead of a csv file
###############################################################
# [depreciated] data.csv   --> .csv filename (function takes annual extreme data from Water Survey of Canada)
# Q          --> Discharge values (max annual)
# Years      --> Years of record
# title      --> character title of plot
# breaks     --> numeric input, (number of breaks for horizontal lines on plot)
# write.table--> writes .csv output for return periods and standard errors for GEV and Log 3 predictions
# print.data --> returns data.frame or ordered peak flows for site with dates and years. 

# Example: 
# FFA('elknatal_annualextremes.csv', 'Elk River Near Natal', 6, write.table = T, print.data = T)
# FFA('elknatal_annualextremes.csv', 'Elk River Near Natal', 6, write.table = F, print.data = F, year.prob = 2005)
# -------------------------------------------------------
# rm(list = ls())
# Read in data
# data = read.csv(data.csv)
# data = data[data$PARAM == 1 & is.na(data$MAX) == F,] 	# remove level data (param = 2)
# Q = data$MAX
# Years = data$Year
# FFA(Q, Years, 'Matt Test', write.table = T)

FFA = function(Q, Years, title, breaks = 5, write.table = FALSE, 
               print.data = FALSE, year.prob = NULL){

	# Specify Q and Labels
	graphlab = title
	yrs = paste(min(Years, na.rm = T), max(Years, na.rm = T), sep = ' - ')
	Qbreak = breaks 								# sequence for horizontal bars
	
	# Generate plotting positions
	n = length(Q)
	r = n + 1 - rank(Q)  # highest Q has rank r = 1
	T = (n + 1)/r
	return_period = c(1.010,2,5,10,25,50,100,200)
	
	
	# Set up x axis tick positions and labels
	Ttick = c(1.001,1.01,1.1,1.5,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,50,60,70,80,90,100,200)
	xtlab = c(1.001,1.01,1.1,1.5,2,NA,NA,5,NA,NA,NA,NA,10,NA,NA,NA,NA,15,NA,NA,NA,NA,20,NA,30,NA,NA,NA,50,NA,NA,NA,NA,100, 200)
	y = -log(-log(1 - 1/T))
	ytick = -log(-log(1 - 1/Ttick))
	xmin = min(min(y),min(ytick))
	xmax = max(ytick)
	
	
	# Fit a line by method of moments, along with 95% confidence intervals
	KTtick = -(sqrt(6)/pi)*(0.5772 + log(log(Ttick/(Ttick-1))))
	QTtick = mean(Q) + KTtick*sd(Q) 
	nQ = length(Q)
	se = (sd(Q)*sqrt((1+1.14*KTtick + 1.1*KTtick^2)))/sqrt(nQ) 
	LB = QTtick - qt(0.975, nQ - 1)*se
	UB = QTtick + qt(0.975, nQ - 1)*se
	max = max(UB)
	# Qmax = max(QTtick)
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
	
	P = 1/return_period 			# exceedance probability 1/return period (stipulated Ts)
	k = skew_c/6
	w = (log(1/P^2))^0.5
	z = w - ((2.515517 + 0.802853*w + 0.010328*w^2) /
	 				(1 + 1.432788*w + 0.189269*w^2 + 0.001308*w^3))
	kT = z + (z^2 -1)*k + (1/3) * (z^3 - 6*z)*k^2 - (z^2 - 1)*k^3 + z*k^4 + (1/3)*k^5
	
	logQcalc = mean(logQ) + kT * sd(logQ)
	ylog = -log(-log(1 - P))
	Qcalc = 10^logQcalc
	
	Qmax = max(c(Qmax,max(Qcalc)))
	
	
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
	mtext(expression( "Peak Annual Flow ("*m^3*s^{-1}*")" ), 2, 2.5)
	
	# Add fitted line and confidence limits
	lines(ytick, QTtick, col = "black", lwd = 2)  
	lines(ytick, LB, col = "black", lty = 3, lwd = 3)
	lines(ytick, UB, col = "black", lty = 3, lwd = 3)  
	
	# Draw grid lines
	abline(v = ytick, lty = 3)         
	# abline(h = seq(0.5, floor(Qmax), 0.5), lty = 3)  
	if (Qmax >= 1) {
			abline(h = seq(0,floor(Qmax), round(Qmax, -(nchar(round(Qmax))-1))/Qbreak), lty = 3)}else{
			abline(h = seq(0,floor(Qmax), round(Qmax, 1)/Qbreak), lty = 3)   }       
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
	summary_table = data.frame(Return_Period = Ttick, 
							   Gumbel_Flow = round(QTtick,1), 
							   SE = round(se,1)						)
	data_table = data.frame(summary_table[Ttick %in% return_period[-1],],
							   LogIII_Flow = round(Qcalc[-1],1),
							   SE_Log = round(se_log[-1],1)			)
	
	if (write.table == TRUE){
		write.csv(data_table, paste(graphlab,yrs, '.csv', sep = ''), row.names = F)
	}
	
	###################################
	#### Tests for Goodness of Fit ####
	# Calculate expected values from return periods (T)
	
	# GUMBEL METHOD observed, expected 
	KTexp = -(sqrt(6)/pi)*(0.5772 + log(log(T/(T-1))))
	Qgumb = mean(Q) + KTexp*sd(Q) 
	
	# Kolmogorov-Smirnov Test
	# 1 - ks.test (so p value < 0.05 = 95% confident that it is a 'good fit')
	
	# LOG III METHOD observed, expected
	P = 1/T 			# exceedance probability 1/return period
	k = skew_c/6
	w = (log(1/P^2))^0.5
	z = w - ((2.515517 + 0.802853*w + 0.010328*w^2) /
	 				(1 + 1.432788*w + 0.189269*w^2 + 0.001308*w^3))
	kT_ob = z + (z^2 -1)*k + (1/3) * (z^3 - 6*z)*k^2 - (z^2 - 1)*k^3 + z*k^4 + (1/3)*k^5
	Qlog3 =  10^(mean(logQ) + kT_ob * sd(logQ))
	
	# Gumbel Method
	gp = round(1 - ks.test(Q,Qgumb, alternative = 'two.sided', simulate.p.value = T)$p.value, 4)
	# round(ks.test(Q,Qgumb, alternative = 'two.sided')$p.value, 4)
	
	# Log III Method
	lp = round(1 - ks.test(Q,Qlog3, alternative = 'two.sided', simulate.p.value = T)$p.value, 4)
	# round(ks.test(Q,Qlog3, alternative = 'two.sided')$p.value, 4)
	
	# Add Legend
	legend('topleft', c(paste('Gumbel (p = ',gp,')', sep = ''), 
						paste('Log 3 (p = ',lp,')', sep = ''), 
						'Standard Error'), 
			col = c('black', 'blue', 'grey60'), lty = c(1,1,3),
			lwd = c(2,2,3), border = 'n', bg = 'white')
			
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
	y_lower = ymod2[round(Qpred2,1) == round(lower_q,1)]
	y_higher = ymod2[round(Qpred2,1) == round(higher_q,1)]
	
	# y = -log(-log(1 - 1/T))
	P25 = 1 - mean((1 - exp(-exp(-y_lower)))) 
	P75	= mean((1 - exp(-exp(-y_higher))))
	
	# Year Probability (What's the likelihood of (e.g.) 2013 happening again?)
	if(is.null(year.prob) == F){ 
		Qyear_log3 = data$MAX[data$Year ==year.prob]
		y.year_log3 = ymod2[round(Qpred2,1) == Qyear_log3]
		Y.Year = mean((1 - exp(-exp(-y.year_log3))))
		}
	
	
	# Return Summary Data
	return(
		list(data_table, 	# Return period table
			paste(' Probability of 1Q Non - Exceedance =', round(P25, 4)),
			paste(' Probability of 3Q Exceedance =', round(P75,4)),
			summary(Q),
			paste('n =', n), 
			if (print.data == TRUE) {data[order(data$MAX),] },
			if (is.null(year.prob) == F){paste('% chance to exceed ', year.prob, ' = ', round(Y.Year, 4), sep = '')}
		)
	)
	
	# 					* end function *
}