library(tidyverse)
library(EBASE)
library(doParallel)
library(foreach)
library(here)
library(plotly)
library(lubridate)

# apa cat point -------------------------------------------------------------------------------

# prep apacp data
apacpdatraw <- read.csv(here('data-raw/cp_ebase_911.csv'))
apacpdat <- apacpdatraw |> 
  mutate(
    DateTimeStamp = mdy_hm(DateTimeStamp, tz = 'America/Jamaica'), 
    Depth = Depth + 0.3
  ) |> 
  rename(
    Sal = Sal_ppt ,
    PAR = PAR_W.m2
  ) |> 
  arrange(DateTimeStamp)

##
# EBASE observed

tomod <- apacpdat |> 
  select(-DO_dtd)

yrs <- unique(year(tomod$DateTimeStamp))

apacpdecobs <- NULL
for(yr in yrs){
  
  cat(yr, '\t')
  
  # setup parallel backend
  ncores <- detectCores()
  cl <- makeCluster(ncores - 2)
  registerDoParallel(cl)

  tomodsub <- tomod |> 
    filter(year(DateTimeStamp) == yr)
  
  # fixed depth for the year
  depth <- tomodsub$Depth
  
  # ebase
  res <- try(ebase(tomodsub, interval = 900, Z = depth, ndays = 1, progress = NULL, n.chains = 4,
             bprior = c(0.251, 1e-6)), silent = T)

  stopCluster(cl)
  
  i <- 1
  while(inherits(res, 'try-error')){
    
    # ncores <- detectCores()
    cl <- makeCluster(ncores - 2)
    registerDoParallel(cl)
    
    cat('retrying...\t')
    
    # ebase
    res <- try(ebase(tomodsub, interval = 900, Z = depth, ndays = 1, progress = NULL, n.chains = 4,
                     bprior = c(0.251, 1e-6)), silent = T)
    
    stopCluster(cl)
    
    i <- i + 1
    if(i > 5) break()
    
  }
  if(i > 5) next()
  
  apacpdecobs <- rbind(apacpdecobs, res)

}

save(apacpdecobs, file = here('data/apacpdecobs.RData'))

##
# EBASE detided

tomod <- apacpdat |> 
  select(-DO_obs) |> 
  rename(DO_obs = DO_dtd)

yrs <- unique(year(tomod$DateTimeStamp))

apacpdecdtd <- NULL
for(yr in yrs){
  
  cat(yr, '\t')
  
  # setup parallel backend
  ncores <- detectCores()
  cl <- makeCluster(ncores - 2)
  registerDoParallel(cl)

  tomodsub <- tomod |> 
    filter(year(DateTimeStamp) == yr)
  
  # fixed depth for the year
  depth <- tomodsub$Depth
  
  # ebase
  res <- try(ebase(tomodsub, interval = 900, Z = depth, ndays = 1, progress = NULL, n.chains = 4,
                  bprior = c(0.251, 1e-6)), silent = T)

  stopCluster(cl)
  
  i <- 1
  while(inherits(res, 'try-error')){
    
    ncores <- detectCores()
    cl <- makeCluster(ncores - 2)
    registerDoParallel(cl)
    
    cat('retrying...\t')
    
    # ebase
    res <- try(ebase(tomodsub, interval = 900, Z = depth, ndays = 1, progress = NULL, n.chains = 4,
                     bprior = c(0.251, 1e-6)), silent = T)
    
    stopCluster(cl)
    
    i <- i + 1
    if(i > 5) break()
    
  }
  if(i > 5) next()
  
  apacpdecdtd <- rbind(apacpdecdtd, res)
  
}

save(apacpdecdtd, file = here('data/apacpdecdtd.RData'))

##
# view results

data(apacpdecobs)
data(apacpdecdtd)

ylab <- 'mmol O2 m-2 d-1'

apacpdec <- list(
  Observed = apacpdecobs, 
  Detided = apacpdecdtd
) |> 
  enframe(name = 'Type') |> 
  unnest(value) |> 
  select(Type, Date, P, R, D) |> 
  mutate(
    NEM = P - R
  ) |> 
  pivot_longer(cols = -matches('Date|Type')) |> 
  summarise(value = mean(value, na.rm = T), .by = c(Type, Date, name)) |> 
  pivot_wider(names_from = name, values_from = value)

p1 <- plot_ly(subset(apacpdec, Type == 'Observed'), x = ~Date, y = ~P, type = 'scatter', mode = 'lines', name = 'P') |>
  add_trace(y = ~-R, mode = 'lines', name = 'R') |>
  add_trace(y = ~D, mode = 'lines', name = 'D') |>
  add_trace(y = ~NEM, mode = 'lines', name = 'NEM') |>
  layout(xaxis = list(title = ''),
         yaxis = list(title = paste('Observed', ylab)))

p2 <- plot_ly(subset(apacpdec, Type == 'Detided'), x = ~Date, y = ~P, type = 'scatter', mode = 'lines', name = 'P') |>
  add_trace(y = ~-R, mode = 'lines', name = 'R') |>
  add_trace(y = ~D, mode = 'lines', name = 'D') |>
  add_trace(y = ~NEM, mode = 'lines', name = 'NEM') |>
  layout(xaxis = list(title = ''),
         yaxis = list(title = paste('Detided', ylab)))

subplot(p1, p2, nrows = 2, shareX = T, titleY = T)

# apa dry bar ---------------------------------------------------------------------------------

# prep apadb data
apadbdatraw <- read.csv(here('data-raw/db_ebase_911.csv'))
apadbdat <- apadbdatraw |> 
  mutate(
    DateTimeStamp = ymd_hms(DateTimeStamp, tz = 'America/Jamaica'), 
    Depth = Depth + 0.3
  ) |> 
  rename(
    Sal = Sal_ppt ,
    PAR = PAR_W.m2
  ) |> 
  arrange(DateTimeStamp)

##
# EBASE observed

tomod <- apadbdat |> 
  select(-DO_dtd)

yrs <- unique(year(tomod$DateTimeStamp))

apadbdecobs <- NULL
for(yr in yrs){
  
  cat(yr, '\t')
  
  # setup parallel backend
  ncores <- detectCores()
  cl <- makeCluster(ncores - 2)
  registerDoParallel(cl)
  
  tomodsub <- tomod |> 
    filter(year(DateTimeStamp) == yr)
  
  # fixed depth for the year
  depth <- tomodsub$Depth
  
  # ebase
  res <- try(ebase(tomodsub, interval = 900, Z = depth, ndays = 1, progress = NULL, n.chains = 4,
               bprior = c(0.251, 1e-6)), silent = T)
  
  stopCluster(cl)
  
  i <- 1
  while(inherits(res, 'try-error')){
    
    # ncores <- detectCores()
    cl <- makeCluster(ncores - 2)
    registerDoParallel(cl)
    
    cat('retrying...\t')
    
    # ebase
    res <- try(ebase(tomodsub, interval = 900, Z = depth, ndays = 1, progress = NULL, n.chains = 4,
                     bprior = c(0.251, 1e-6)), silent = T)
    
    stopCluster(cl)
    
    i <- i + 1
    if(i > 5) break()
    
  }
  if(i > 5) next()
  
  apadbdecobs <- rbind(apadbdecobs, res)
  
}

save(apadbdecobs, file = here('data/apadbdecobs.RData'))

##
# EBASE detided

tomod <- apadbdat |> 
  select(-DO_obs) |> 
  rename(DO_obs = DO_dtd)

yrs <- unique(year(tomod$DateTimeStamp))

apadbdecdtd <- NULL
for(yr in yrs){
  
  cat(yr, '\t')
  
  # setup parallel backend
  ncores <- detectCores()
  cl <- makeCluster(ncores - 2)
  registerDoParallel(cl)
  
  tomodsub <- tomod |> 
    filter(year(DateTimeStamp) == yr)
  
  # fixed depth for the year
  depth <- tomodsub$Depth
  
  # ebase
  res <- try(ebase(tomodsub, interval = 900, Z = depth, ndays = 1, progress = NULL, n.chains = 4,
               bprior = c(0.251, 1e-6)), silent = T)
  
  stopCluster(cl)
  
  i <- 1
  while(inherits(res, 'try-error')){
    
    ncores <- detectCores()
    cl <- makeCluster(ncores - 2)
    registerDoParallel(cl)
    
    cat('retrying...\t')
    
    # ebase
    res <- try(ebase(tomodsub, interval = 900, Z = depth, ndays = 1, progress = NULL, n.chains = 4,
                     bprior = c(0.251, 1e-6)), silent = T)
    
    stopCluster(cl)
    
    i <- i + 1
    if(i > 5) break()
    
  }
  if(i > 5) next()
  
  apadbdecdtd <- rbind(apadbdecdtd, res)
  
}

save(apadbdecdtd, file = here('data/apadbdecdtd.RData'))

##
# view results

data(apadbdecobs)
data(apadbdecdtd)

ylab <- 'mmol O2 m-2 d-1'

apadbdec <- list(
  Observed = apadbdecobs, 
  Detided = apadbdecdtd
) |> 
  enframe(name = 'Type') |> 
  unnest(value) |> 
  select(Type, Date, P, R, D) |> 
  mutate(
    NEM = P - R
  ) |> 
  pivot_longer(cols = -matches('Date|Type')) |> 
  summarise(value = mean(value, na.rm = T), .by = c(Type, Date, name)) |> 
  pivot_wider(names_from = name, values_from = value)

p1 <- plot_ly(subset(apadbdec, Type == 'Observed'), x = ~Date, y = ~P, type = 'scatter', mode = 'lines', name = 'P') |>
  add_trace(y = ~-R, mode = 'lines', name = 'R') |>
  add_trace(y = ~D, mode = 'lines', name = 'D') |>
  add_trace(y = ~NEM, mode = 'lines', name = 'NEM') |>
  layout(xaxis = list(title = ''),
         yaxis = list(title = paste('Observed', ylab)))

p2 <- plot_ly(subset(apadbdec, Type == 'Detided'), x = ~Date, y = ~P, type = 'scatter', mode = 'lines', name = 'P') |>
  add_trace(y = ~-R, mode = 'lines', name = 'R') |>
  add_trace(y = ~D, mode = 'lines', name = 'D') |>
  add_trace(y = ~NEM, mode = 'lines', name = 'NEM') |>
  layout(xaxis = list(title = ''),
         yaxis = list(title = paste('Detided', ylab)))

subplot(p1, p2, nrows = 2, shareX = T, titleY = T)

# apa east bay --------------------------------------------------------------------------------

# prep apaeb data
apaebdatraw <- read.csv(here('data-raw/east_ebase_911.csv'))
apaebdat <- apaebdatraw |> 
  mutate(
    DateTimeStamp = ymd_hms(DateTimeStamp, tz = 'America/Jamaica'), 
    Depth = Depth + 1
  ) |> 
  rename(
    Sal = Sal_ppt ,
    PAR = PAR_W.m2
  ) |> 
  arrange(DateTimeStamp)

##
# EBASE observed

tomod <- apaebdat |> 
  select(-DO_dtd)

yrs <- unique(year(tomod$DateTimeStamp))

apaebdecobs <- NULL
for(yr in yrs){
  
  cat(yr, '\t')
  
  # setup parallel backend
  ncores <- detectCores()
  cl <- makeCluster(ncores - 2)
  registerDoParallel(cl)
  
  tomodsub <- tomod |> 
    filter(year(DateTimeStamp) == yr)
  
  # fixed depth for the year
  depth <- tomodsub$Depth
  
  # ebase
  res <- try(ebase(tomodsub, interval = 900, Z = depth, ndays = 1, progress = NULL, n.chains = 4,
                   bprior = c(0.251, 1e-6)), silent = T)
  
  stopCluster(cl)
  
  i <- 1
  while(inherits(res, 'try-error')){
    
    ncores <- detectCores()
    cl <- makeCluster(ncores - 2)
    registerDoParallel(cl)
    
    cat('retrying...\t')
    
    # ebase
    res <- try(ebase(tomodsub, interval = 900, Z = depth, ndays = 1, progress = NULL, n.chains = 4,
                     bprior = c(0.251, 1e-6)), silent = T)
    
    stopCluster(cl)
    
    i <- i + 1
    if(i > 5) break()
    
  }
  if(i > 5) next()
  
  apaebdecobs <- rbind(apaebdecobs, res)
  
}

save(apaebdecobs, file = here('data/apaebdecobs.RData'))

##
# EBASE detided

tomod <- apaebdat |> 
  select(-DO_obs) |> 
  rename(DO_obs = DO_dtd)

yrs <- unique(year(tomod$DateTimeStamp))

apaebdecdtd <- NULL
for(yr in yrs){
  
  cat(yr, '\t')
  
  # setup parallel backend
  ncores <- detectCores()
  cl <- makeCluster(ncores - 2)
  registerDoParallel(cl)
  
  tomodsub <- tomod |> 
    filter(year(DateTimeStamp) == yr)
  
  # fixed depth for the year
  depth <- tomodsub$Depth
  
  # ebase
  res <- try(ebase(tomodsub, interval = 900, Z = depth, ndays = 1, progress = NULL, n.chains = 4,
                   bprior = c(0.251, 1e-6)), silent = T)
  
  stopCluster(cl)
  
  i <- 1
  while(inherits(res, 'try-error')){
    
    ncores <- detectCores
    cl <- makeCluster(ncores - 2)
    registerDoParallel(cl)
    
    cat('retrying...\t')
    
    # ebase
    res <- try(ebase(tomodsub, interval = 900, Z = depth, ndays = 1, progress = NULL, n.chains = 4,
                     bprior = c(0.251, 1e-6)), silent = T)
    
    stopCluster(cl)
    
    i <- i + 1
    if(i > 5) break()
    
  }
  if(i > 5) next()
  
  apaebdecdtd <- rbind(apaebdecdtd, res)
  
}

save(apaebdecdtd, file = here('data/apaebdecdtd.RData'))

##
# view results

data(apaebdecobs)
data(apaebdecdtd)

ylab <- 'mmol O2 m-2 d-1'

apaebdec <- list(
  Observed = apaebdecobs, 
  Detided = apaebdecdtd
) |> 
  enframe(name = 'Type') |> 
  unnest(value) |> 
  select(Type, Date, P, R, D) |> 
  mutate(
    NEM = P - R
  ) |> 
  pivot_longer(cols = -matches('Date|Type')) |> 
  summarise(value = mean(value, na.rm = T), .by = c(Type, Date, name)) |> 
  pivot_wider(names_from = name, values_from = value)

p1 <- plot_ly(subset(apaebdec, Type == 'Observed'), x = ~Date, y = ~P, type = 'scatter', mode = 'lines', name = 'P') |>
  add_trace(y = ~-R, mode = 'lines', name = 'R') |>
  add_trace(y = ~D, mode = 'lines', name = 'D') |>
  add_trace(y = ~NEM, mode = 'lines', name = 'NEM') |>
  layout(xaxis = list(title = ''),
         yaxis = list(title = paste('Observed', ylab)))

p2 <- plot_ly(subset(apaebdec, Type == 'Detided'), x = ~Date, y = ~P, type = 'scatter', mode = 'lines', name = 'P') |>
  add_trace(y = ~-R, mode = 'lines', name = 'R') |>
  add_trace(y = ~D, mode = 'lines', name = 'D') |>
  add_trace(y = ~NEM, mode = 'lines', name = 'NEM') |>
  layout(xaxis = list(title = ''),
         yaxis = list(title = paste('Detided', ylab)))

subplot(p1, p2, nrows = 2, shareX = T, titleY = T)