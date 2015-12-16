# Get Discharge from a Salt Dilution 
#  ~ Constant (Slug) Injection ~
# 
#   ~ October 2015 ~
#  by: Matthew Chernos
# 

# Function to correct EC for Water Temperature (1.9%/C)
EC_corr = function(Cond, Tw, Tr = 25){Cond / (1 + 0.019 * (Tw - Tr))}

##################################
# Determining *k* by Calibration #
##################################
# EXAMPLE:
# EC = scan(n = 13)
# 330.0 360.7 384.6 408.2 430.9 453.2 475.1 496.4 518.0 543.0 565.0 587.0 605.0 
# 
# y = scan(n = length(EC))
# 10 10 10 10 10 10 10 10 10 10 10 10 10
# 
# k.calib(EC, y)

k.calib = function(EC, y, V_c = 1000, X = 10, V_o = 1000){
  
  # CALIBRATION METHOD VALUES:
  # V_c = 1000 # (mL) - volume of streamwater in calibration tank
  # X = 10 # (mL) - volume of injection solution
  # V_o = 1000 # (mL) - volume of streamwater in 2nd soln.
  RC_sec = X/(V_o + X)
  
  # If not corrected:
  # EC = EC_corr(EC, 8.7)
  #
  
  # 1) Create Secondary Solution (dilute injection soln.)
  #   - Mix X mL (5-10) of injection solution into 
  #     measured volume of streamwater.
  #   - Calculate Relative Concentration (RC_sec)
  
  
  # 2) Create Calibration Tank
  #   - Add set volume of streamwater into tank
  #   - Keep at water temp (Tw) (maybe make corral)
  #   - Measure EC_o, where RC = 0
  
  # 3) Add Secondary Solution
  #   - add 2 mL increments of secondary solution to
  #     calibration tank.
  #   - Record EC and Tw
  
  # y = 10 # mL
  # y = rep(y, length(EC))  # added 2nd soln. increments added
  RC = (RC_sec * cumsum(y)) / (V_c + cumsum(y))
  
  # 4) Continue above procedure until EC > EC_slug max
  # 5) Plot RC (y) vs. EC (x)
  #     - the slope of the line is k
  
  k.fit = lm(RC~EC)
  k = coef(k.fit)['EC']
  
  # See plot
  plot(EC, RC, pch = 19, col = rgb(0,0,0,0.6),
       ylab = 'Relative Concentration',
       xlab = 'Electrical Conductivity')
  abline(k.fit, col = 'red')
  mtext(paste('k = ', round(coef(k.fit)['EC'],8),
              '\nR2 = ', round(summary(k.fit)$r.squared,2), sep = ''), 
        line = -2, adj = 0, font = 3)
  
  k
}


##################
# Find Discharge #
##################

# Q = V / (k * dT * sum(EC - EC_bg))

# 1) Mix Injection Solution
#     - Needs to be known concentration 
#     - 17 % ideal ~ 1 kg in 6 L
# 2) Prepare Salt Slug
#     - pour 500 mL in 1 L bottle
# 3) Set aside 20 mL of salt soln for 2ndary soln.
# 4) Measure up/downstream EC and Tw at injection site
#     - start logging at downstream site (1 s interval)
# 5) Pour in salt slug
#     - monitor downstream site until it returns to background 
# 6) Calculate Discharge 
#     - ensure right units! (uS/cm), (m^3), (s)

# Read in Data
# data = read.csv('ec_data.csv')

# Sp_Cond = data$Sp..Cond...uS.cm.
# Tw = data$Temp..C.
##########

Q.salt = function(Cond, Tw, k, V_slug = 1000, dT = 1){
  # dT = 1                  # (s) - sampling resolution
  # V_slug = 1000           # mL
  V_slug = V_slug/1000^2  # mL/L (convert to m^3)
  # k = 5e-6                # From Above
  
  # 7) Correct EC for Tw
  EC = EC_corr(Cond, Tw)
  
  # EC_bg = min(EC) # uS/cm 
  EC_bg = EC[1]
  
  plot(seq(1,by = 1, length.out = length(EC)), EC,
       xlab = 'Time (s)', ylab = 'EC (uS/cm)',
       type = 'l')
  
  Q = V_slug / (k * dT * sum(EC - EC_bg))
  
  Q
}




