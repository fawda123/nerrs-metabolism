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

yrs <- unique(year(tomod$DateTimeStamp))

apadecobs <- NULL
for(yr in yrs){
  
  cat(yr, '\t')
  
  # setup parallel backend
  ncores <- detectCores()
  cl <- makeCluster(ncores - 2)
  registerDoParallel(cl)

  tomodsub <- tomod |> 
    filter(year(DateTimeStamp) == yr)
  
  # ebase
  res <- ebase(tomodsub, interval = 900, Z = tomodsub$Depth, ndays = 7, progress = NULL, n.chains = 4,
             bprior = c(0.251, 1e-6))

  stopCluster(cl)
  
  apadecobs <- rbind(apadecobs, res)

}

save(apadecobs, file = here('data/apadecobs.RData'))

# EBASE detided -------------------------------------------------------------------------------

tomod <- apadat |> 
  select(-DO_obs) |> 
  rename(DO_obs = DO_dtd)

yrs <- unique(year(tomod$DateTimeStamp))
yrs <- yrs[yrs > 2007]

apadecdtd <- NULL
for(yr in yrs){
  
  cat(yr, '\t')
  
  # setup parallel backend
  ncores <- detectCores()
  cl <- makeCluster(ncores - 2)
  registerDoParallel(cl)

  tomodsub <- tomod |> 
    filter(year(DateTimeStamp) == yr)
  
  # ebase
  res <- ebase(tomodsub, interval = 900, Z = tomodsub$Depth, ndays = 7, progress = NULL, n.chains = 4,
                  bprior = c(0.251, 1e-6))

  stopCluster(cl)
  
  apadecdtd <- rbind(apadecdtd, res)
  
}

save(apadecdtd, file = here('data/apadecdtd.RData'))