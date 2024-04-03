library(tidyverse)
library(EBASE)
library(doParallel)
library(foreach)
library(here)
library(plotly)

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

# view results --------------------------------------------------------------------------------

data(apadecobs)
data(apadecdtd)

ylab <- 'mmol O2 m-2 d-1)'

apadec <- list(
  Observed = apadecobs, 
  Detided = apadecdtd
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

p1 <- plot_ly(subset(apadec, Type == 'Observed'), x = ~Date, y = ~P, type = 'scatter', mode = 'lines', name = 'P') |>
  add_trace(y = ~-R, mode = 'lines', name = 'R') |>
  add_trace(y = ~D, mode = 'lines', name = 'D') |>
  add_trace(y = ~NEM, mode = 'lines', name = 'NEM') |>
  layout(xaxis = list(title = ''),
         yaxis = list(title = paste('Observed', ylab)))

p2 <- plot_ly(subset(apadec, Type == 'Detided'), x = ~Date, y = ~P, type = 'scatter', mode = 'lines', name = 'P') |>
  add_trace(y = ~-R, mode = 'lines', name = 'R') |>
  add_trace(y = ~D, mode = 'lines', name = 'D') |>
  add_trace(y = ~NEM, mode = 'lines', name = 'NEM') |>
  layout(xaxis = list(title = ''),
         yaxis = list(title = paste('Detided', ylab)))

subplot(p1, p2, nrows = 2, shareX = T, titleY = T)

