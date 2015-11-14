# Get Discharge from a Salt Dilution 
#  ~ Constant (Slug) Injection ~
# 
#   ~ October 2015 ~
#  by: Matthew Chernos
# 

# Function to correct EC for Water Temperature
EC_corr = function(EC_raw, Tw){EC_raw * (1 + 0.019 * (Tw - 25))}

##################################
# Determining *k* by Calibration #
##################################

# EC = measured for calibration
EC = scan(n = 14)
330.0 360.7 384.6 408.2 430.9 453.2 475.1 496.4 518.0 543.0 565.0 587.0 605.0 

# 232.5 252.9 272.6 292.5 311.9 331.2 350.0 368.8 387.3 405.4 423.5 441.0 458.8 476.2

# Michel Creek Calib.
# 284.9 335.2 359.3 383.8 407.9 431.6 454.3

# If not corrected:
# EC = EC_corr(EC, 8.7)
#

# 1) Create Secondary Solution (dilute injection soln.)
#   - Mix X mL (5-10) of injection solution into 
#     measured volume of streamwater.
#   - Calculate Relative Concentration (RC_sec)

X = 10 # (mL) - volume of injection solution
V_o = 1000 # (mL) - volume of streamwater in 2nd soln.
RC_sec = X/(V_o + X)

# 2) Create Calibration Tank
#   - Add set volume of streamwater into tank
#   - Keep at water temp (Tw) (maybe make corral)
#   - Measure EC_o, where RC = 0

# 3) Add Secondary Solution
#   - add 2 mL increments of secondary solution to
#     calibration tank.
#   - Record EC and Tw

V_c = 1000 # (mL) - volume of streamwater in calibration tank
y = 10 # mL
y = rep(y, length(EC))  # added 2nd soln. increments added
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
data = read.csv('ec_data.csv')

Sp_Cond = data$Sp..Cond...uS.cm.
Tw = data$Temp..C.
##########

dT = 1                  # (s) - sampling resolution
V_slug = 1000           # mL
V_slug = V_slug/1000^2  # mL/L (convert to m^3)
# k = 5e-6                # From Above
k = 3.82e-06
# 7) Correct EC for Tw

EC = Sp_Cond * (1 + 0.019 * (Tw - 25))

EC_bg = 232.8           # uS/cm 
EC_bg = min(EC)

plot(seq(1,by = 1, length.out = length(EC)), EC,
     xlab = 'Time (s)', ylab = 'EC (uS/cm)',
     type = 'l')

Q = V_slug / (k * dT * sum(EC - EC_bg))


Q




