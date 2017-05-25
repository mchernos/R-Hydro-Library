# Takes an Ostrich DDS file and makes an rvt from it
#
# All hate-mail should be sent to Ryan MacDonald. 
#
# Matt Chernos ~ May 2017.
####################################################
rm(list = ls())
library('tidyverse')

# Read in DDS-AU file
read.in.params = function(Run){
  # Read in an Ostrich DDS Run
  DDSAU = readLines(paste0('OstOutput0_DDS',Run,'.txt'))
  
  # Find List of Parameters and make data.frame
  behave = grep('Optimal Parameter Set', DDSAU) + 1
  end = grep('Summary of Constraints', DDSAU) - 2
  
  # Clean up the data into a nice table
  dat = tbl_df(DDSAU[behave:end]) %>%
    mutate(value = gsub(' ', '', value),
           Round = Run) %>%
    separate(value, c('Parameter', 'Value'), sep = ':') %>%
    filter(Parameter != 'ObjectiveFunction')
}

# Repeat step to compile all 10 Ostrich files/runs
dat = do.call('rbind', lapply(0:9, read.in.params))

# Read in .rvp file template
RVP = readLines('wampus.rvp.tpl')

# Function to replace place-holder in template file w/ Value
replace.value = function(parameter, value, dat){
  sub(parameter, value, dat)
}
# dat1 = dat %>% filter(Round == 0)
# replace.value(dat1$Parameter[1], dat1$Value[1], RVP)

# Replace values and write file
make.rvp = function(Run){
  # Get a temporary RVP file and isolate data for only that Run
  temp = RVP
  dat1 = dat %>% filter(Round == Run)
  
  # For loop to cycle through parameters and iteratively replace 'em all. 
  # (couldn't figure out how to vectorize this...)
  for(i in 1:length(dat1$Parameter)){
    temp = replace.value(dat1$Parameter[i], dat1$Value[i], temp)
  }
  
  # Write rvp to file, named Wampus and Run number
  fileConn = file(paste0('RVP/Wampus', Run, '.rvp'))
  writeLines(temp, fileConn)
  close(fileConn)
}

# make the rvp for all 10 Ostrich files
sapply(0:9, make.rvp)
