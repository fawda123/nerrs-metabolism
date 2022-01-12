
# Running BASEmetab model -------------------------------------------------

library(BASEmetab)
# devtools::load_all('../BASEmetab')
library(tidyverse)
library(lubridate)
library(R2jags)
library(foreach)
library(doParallel)
library(here)

# setup parallel backend
ncores <- detectCores()
cl <- makeCluster(ncores - 2)
registerDoParallel(cl)

results.dir <- here('output')
data.dir <- results.dir

# input data
dat_input <- read.csv(here('data-raw/apadb May 2018 WQ MET.csv')) %>% 
  mutate(
    DateTimeStamp = mdy_hm(DateTimeStamp, tz = 'America/Jamaica')
  ) %>% 
  separate(DateTimeStamp, c('Date', 'Time'), sep = ' ') %>% 
  select(Date, Time, tempC = Temp, DO.meas = DO_obs, atmo.pressure = BP, salinity = Sal)

write.csv(dat_input, here('output/dat_input.csv'))

#run model

# cat point bottom depth (mean of tidal vector plus 0.3m)
H <- 1.852841

# areal K, 0.80 is m/d mean wanninkhof for the year at apa 2012
Kareal <- 0.8040253

# volumetric
kvol <- Kareal / H

results <- bayesmetab(data.dir, results.dir, interval = 900, K.est = F, K.meas.mean = kvol, K.meas.sd = 1e-9, instant = T, update.chains = T)

# remove image files from output
file.remove(list.files(path = results.dir, pattern = '\\.jpg$', full.names = T))

