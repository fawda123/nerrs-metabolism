library(dplyr)
library(lubridate)
library(EBASE)
library(doParallel)
library(foreach)
library(here)

# prep apa data
apadatraw <- read.csv(here('data-raw/cp_ebase_911.csv'))
apadat <- apadatraw |> 
  mutate(
    DateTimeStamp = mdy_hm(DateTimeStamp, tz = 'America/Jamaica'), 
    Depth = Depth + 0.3
  ) |> 
  rename(
    Sal = Sal_ppt ,
    PAR = PAR_W.m2
  ) |> 
  arrange(DateTimeStamp)

# EBASE observed ------------------------------------------------------------------------------

tomod <- apadat |> 
  select(-DO_dtd)

# setup parallel backend
ncores <- detectCores()
cl <- makeCluster(ncores - 2)
registerDoParallel(cl)

# ebase
apadecobs <- ebase(tomod, interval = 900, Z = tomod$Depth, ndays = 7, progress = getwd(), n.chains = 4,
             bprior = c(0.251, 1e-6))

stopCluster(cl)

save(apadecobs, file = here('data/apadecobs.RData'))

# EBASE detided -------------------------------------------------------------------------------

tomod <- apadat |> 
  select(-DO_obs) |> 
  rename(DO_obs = DO_dtd)

# setup parallel backend
ncores <- detectCores()
cl <- makeCluster(ncores - 2)
registerDoParallel(cl)

# ebase
apadecdtd <- ebase(tomod, interval = 900, Z = tomod$Depth, ndays = 7, progress = getwd(), n.chains = 4,
                bprior = c(0.251, 1e-6))

stopCluster(cl)

save(apadecdtd, file = here('data/apadecdtd.RData'))