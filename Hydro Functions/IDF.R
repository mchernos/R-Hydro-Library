##############################################################################################
#  
#  Intensity Duration Frequency Analysis  
#
# formerly: Nanaimo-idf.r
#
# code to conduct idf analysis of Nanaimo Airport annual extreme rainfall data
# generates two graphs:
#  - graphs of i vs T for each duration on a multi-panel plot
#  - plot of i vs. D for a range of return periods
#
# 2013-Sept-9 RDM
# updated/reformatted by M. Chernos - April 2015
##############################################################################################
rm(list = ls())           # clean out workspace
# Gumplotidf.r 
#
# function to generate Gumbel plot for idf analysis
#
# Thanks to Prof. Saeid Mousavi of SBU in Iran, who provided the code for
# computing the method of moments fit and the 95% confidence limits
#
# 2013-Sept-9 RDM
###############################################################

Gumplotidf = function(Duration, int, T, id) {

  ### Define parameters for axis plotting ###
  
  Ttick = c(1.001,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100, 200)
  xtlab = c(1.001,2,NA,NA,5,NA,NA,NA,NA,10,NA,NA,NA,50,NA,NA,NA,NA,100, 200)
  y = -log(-log(1 - 1/T))
  ytick = -log(-log(1 - 1/Ttick))
  xmin = min(min(y), min(ytick))
  xmax = max(ytick)
  
  ### Fit distribution by method of moments ###  
  
  KTtick = -(sqrt(6)/pi)*(0.5772 + log(log(Ttick/(Ttick-1))))
  intTtick = mean(int) + KTtick*sd(int) 
  nint = length(int)
  se = (sd(int)*sqrt((1+1.14*KTtick + 1.1*KTtick^2)))/sqrt(nint) 
  LB = intTtick - qt(0.975, nint-1)*se
  UB = intTtick + qt(0.975, nint-1)*se
  ymax = max(UB)
  
  plot( y, int,
        xlab = "", ylab = "",
        xaxt = "n", # yaxt = "n",
        xlim = c(xmin,xmax), ylim = c(0, ymax),
        pch = 21, bg = "red",
        main = ""
      )  

  if(id > 7) { # add x axes 
    axis(side = 1, at = ytick, labels = xtlab)
	mtext(side = 1, text = "T (yr)", line = 3)
  }
  if(id == 4) { # add y axis label
    mtext(side = 2, line = 3, text = "Intensity (mm/hr)")
  }
  if (Duration < 1) { # Duration in minutes
    legend("topleft", bty = "n", legend = paste("", Duration*60, "min"))
	} else { # Duration in hours
	  legend("topleft", bty = "n", legend = paste("", Duration, "hr"))
	}

  ### Add curve fitted by method of moments, along with 95% confidence intervals ###
  
  lines(ytick, intTtick, col = "black")  
  lines(ytick, LB, col = "black", lty = 3)
  lines(ytick, UB, col = "black", lty = 3)       

  return
  
}  ### end function Gumplotidf() ###


################################################################################
#  set up parameters for idf analysis
################################################################################

durations = c(5/60, 10/60, 15/60, 30/60, 1, 2, 6, 12, 24)   # durations in hr for idf analysis
nd = length(durations)                                      # number of durations		  		  	  
Tp = c(2, 5, 10, 25, 50, 100)                               # return periods in years for plotting idf curves
KTp = -(sqrt(6)/pi)*(0.5772 + log(log(Tp/(Tp-1))))          # frequency factors for method of moments fit
nTp = length(Tp)



###########################################################################################
# Read table of rainfall depths taken from annual extreme analysis by Environment Canada
#
#  Column headers for input data table from Environment Canada:
#
#         Year  5 min 10 min 15 min 30 min    1 h    2 h    6 h   12 h   24 h
###########################################################################################

# use scan() function to read data table, then use matrix() to shape into rows and columns
input = scan()
1985    2.8    5.3    8.1   10.9   13.7   14.4   24.2   28.0   30.4
1986    2.5    3.9    4.4    5.9    8.6   14.6   36.8   56.3   84.7
1987    1.5    2.5    3.2    5.5    9.9   17.7   33.8   43.2   65.3
1988    2.0    3.2    4.2    5.3    6.8   11.1   27.7   45.0   51.8
1989    3.0    4.3    5.2    6.9    9.3   15.2   30.0   45.6   50.9
1990    2.4    2.9    3.5    6.2   10.5   17.7   41.4   52.1   78.6
1991    2.6    3.6    4.8    6.4   10.7   17.4   36.0   66.4  100.9
1992    1.7    2.0    3.1    5.3    9.1   15.3   26.1   43.9   54.4
1993    2.8    4.0    4.5    7.4   10.8   15.8   27.2   38.2   64.9
1994    1.8    2.7    3.6    5.8   10.1   15.0   30.9   40.1   60.6
1995    2.5    3.2    4.1    5.9    9.4   14.4   33.7   50.7   82.6
1996    4.4    6.9    9.9   15.9   21.2   24.0   46.7   50.3   60.9
1997    3.1    3.6    4.3    6.7   10.5   15.9   38.8   54.8   65.2
1998    1.9    2.3    2.9    5.3    8.8   14.4   33.5   44.3   48.5
1999    2.0    2.5    3.5    6.0   10.8   17.4   35.9   48.0   59.4
2000    2.0    3.5    4.0    5.9    8.7   15.0   30.1   45.2   47.6
2001    2.9    4.0    4.2    5.5    7.8   13.2   23.2   36.2   45.6
2002    4.4    4.8    4.8    5.7    9.3   14.5   30.0   38.0   64.9
2003    2.3    4.2    5.6    8.1    8.7   11.8   29.1   45.5   72.6
2004    3.9    6.3    7.6    9.2   10.2   15.2   27.7   33.0   41.0
2005    3.2    5.0    6.5    8.5    9.8   13.9   24.1   34.5   43.7

# blank line above to signal end of scan()

# convert input to numeric values and reshape to a matrix with dimensions of original table 
# then extract matrix of depths (columns 2 to 10)                                           

depth.table = matrix(as.numeric(input), nrow = 21, ncol = 10, byrow = TRUE)
depths = depth.table[ , 2:(nd+1)]           


####################################################################################
#  Set up multi-panel plot to show distributions for each duration
####################################################################################

# set parameters for a multi-panel plot with 5 rows and 2 columns 
par(mfrow = c(5,2), mar = c(1,16,1,1)*0.25, oma = c(5, 5,1,1), cex = 1)

# generate blank plot in top left panel and then add legend 
plot(durations, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")  
legend("left", bty = "n", pch = c(21, NA, NA), pt.bg = c("red", NA, NA), lty = c(0, 1, 3), col = c("black", "black", "black"), 
        legend = c("observed", "fit", "95% conf. limit"), title = "Nanaimo A", title.adj = 0
      )
	  
# set character size for remaining graph panels 
par(cex = 0.75) 


####################################################################################
#  Loop through durations and conduct analysis; add new plot of distributions to
#  the multi-panel plot in each pass through the loop
####################################################################################

idf = matrix(nrow = nd, ncol = nTp)
for (id in 1:nd) {
  di = depths[, id]                        # extract id-th column from matrix
  ni = length(di)                          # length of series
  int = di/durations[id]                   # convert depths to intensities in mm/hr
  ri = ni + 1 - rank(int)                  # compute rank of each intensity, with ri = 1 for the largest
  Ti = (ni + 1)/ri                         # calculate plotting position
  Gumplotidf(durations[id], int, Ti, id)   # plot intensity vs. T on Gumbel axes
  idf[id, ] = mean(int) + KTp*sd(int)      # intensities at different return periods for current duration
}


####################################################################################
#  Generate idf plot
####################################################################################

# set up to use different plotting symbols for each return period 
psym = seq(1, nTp)

# open new window for this graph, set plotting parameters for a single graph panel 
windows()          
par(mfrow = c(1,1), mar = c(5, 5, 5, 5), cex = 1)

# set up custom axis labels and grid line locations
ytick = c(1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,
          200,300,400,500,600,700,800,900,1000)
yticklab = as.character(ytick)

xgrid = c(5,6,7,8,9,10,15,20,30,40,50,60,120,180,240,300,360,
          420,480,540,600,660,720,840,960,1080,1200,1320,1440)

xtick = c(5,10,15,20,30,60,120,360,720,1440)
xticklab = c("5","10","15","20","30","60","2","6","12","24")

ymax = max(idf)
durations = durations*60    # change durations to minutes

# plot i vs D for first return period  
plot(durations, idf[, 1], 
     xaxt="n",yaxt="n",
     pch = psym[1], log = "xy",
	 xlim = c(4, 24*60), ylim = c(1, 100),
	 xlab = "(min)          Duration          (hr)",
	 ylab = "Intensity (mm/hr)"
	)
	
# plot i vs D for remaining return periods, use different plotting symbols for each T
for (iT in 2:nTp) {
  points(durations, idf[, iT], pch = psym[iT])
}

# add best-fit regression lines for each return period  
for (iT in 1:nTp) {
  mod.lm = lm(log10(idf[, iT]) ~ log10(durations))
  b0 = mod.lm$coef[1]
  b1 = mod.lm$coef[2]
  yfit = 10^(b0 + b1*log10(durations))
  lines(durations, yfit, lty = psym[iT])
}

# add custom axes  
axis(1, xtick, xticklab)
axis(2, ytick, yticklab)

# add grid lines  
abline(v = xgrid, col = "gray")
abline(h = ytick, col = "gray")

# add legend to lower left corner  
par(cex = 0.75)
legend("bottomleft", bg = "white", pch = psym, lty = psym, legend = as.character(Tp), title = "Return period (yr)", title.adj = 0.5)
