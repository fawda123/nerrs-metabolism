library(WtRegDO)
library(doSNOW)
library(foreach)
library(tidyverse)
library(patchwork)

# run grid models for wtreg -------------------------------------------------------------------

data(SAPDC)

# setup window width grid
n <- 5
grd <- expand.grid(days = seq(0.1, 12, length.out = n),
                   hrs  = seq(0.1, 12, length.out = n),
                   tide = seq(0.1, 1, length.out = n)
)

# metadata for the location
tz <- 'America/Jamaica'
lat <- 31.39
long <- -81.28

# setup parallel backend with doSNOW
ncores <- parallel::detectCores()
cl <- makeCluster(ncores - 1)
registerDoSNOW(cl)

# setup progress bar
pb <- txtProgressBar(max = nrow(grd), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# parallel loop using foreach with progress bar
res <- foreach(i = 1:nrow(grd),
               .options.snow = opts,
               .packages = c('WtRegDO', 'dplyr', 'tibble'),
               .export = c('SAPDC', 'tz', 'lat', 'long')
               ) %dopar% {

  # get windows
  wins <- list(grd$days[i], grd$hrs[i], grd$tide[i])

  # weighted regression - turn off internal parallelization
  wtreg_res <- wtreg(SAPDC, parallel = FALSE, wins = wins,
                    tz = tz, lat = lat, long = long)

  # estimate ecosystem metabolism using observed DO time series
  metab_obs <- ecometab(wtreg_res, DO_var = 'DO_obs', tz = tz,
                       lat = lat, long = long)

  # estimate ecosystem metabolism using detided DO time series
  metab_dtd <- ecometab(wtreg_res, DO_var = 'DO_nrm', tz = tz,
                       lat = lat, long = long)

  # return results for this iteration
  list(
      obs = metab_obs,
      dtd = metab_dtd
    ) |>
    enframe() |>
    bind_cols(grd[i,])

}

# clean up
close(pb)
stopCluster(cl)

save(res, file = '~/Desktop/res.RData')

# eval results --------------------------------------------------------------------------------

load(file = '~/Desktop/res.RData')

toeval <- do.call('rbind', res) |>
  group_by(days, hrs, tide) |>
  nest() |>
  mutate(
    objall = purrr::map(data, function(x){
      objfun(x$value[[1]], x$value[[2]])
    }),
    objnomean = purrr::map(data, function(x){
      objfun(x$value[[1]], x$value[[2]], vls = c('sdPg', 'anomPg', 'sdRt', 'anomRt'))
    }),
    objanom = purrr::map(data, function(x){
      objfun(x$value[[1]], x$value[[2]], vls = c('anomPg', 'anomRt'))
    }),
    objmean = purrr::map(data, function(x){
      objfun(x$value[[1]], x$value[[2]], vls = c('meanPg', 'meanRt'))
    }),
    objsd = purrr::map(data, function(x){
      objfun(x$value[[1]], x$value[[2]], vls = c('sdPg', 'sdRt'))
    })
  ) |>
  select(-data)

# plot objective fun results
objplo_fun <- function(toeval, obj, ttl){

  toplo <- toeval |>
    unnest(!!obj) |> 
    rename(obj = !!obj) |> 
    select(days, hrs, tide, obj) |> 
    ungroup()

  out <- ggplot(toplo, aes(x = days, y = hrs)) +
    geom_tile(aes(fill = obj), color = 'grey') +
    geom_tile(data = toplo |> filter(obj == min(obj)), fill = NA, color = 'red', width = 3, height = 3, linewidth = 1) +
    scale_fill_viridis_c() +
    scale_x_continuous(breaks = round(unique(toeval$days), 2), expand = c(0, 0)) +
    scale_y_continuous(breaks = round(unique(toeval$hrs), 2), expand = c(0, 0)) +
    facet_wrap(~ tide) +
    labs(title = paste('Objective function for metabolism from observed vs detided DO, ', ttl),
        subtitle = 'Grid search over window widths (days, hours, tide) for weighted regression',
        x = 'Days window', y = 'Hours window',
        fill = 'Objective\nfunction') +
    theme_minimal()
  
  return(out)
  
}

# get window width combo for minimum objective function, picks only one window comb if multiple min
minobj_fun <- function(res, toeval, obj){

  minobj <- toeval |>
    unnest(!!obj) |> 
    rename(obj = !!obj) |> 
    select(days, hrs, tide, obj) |> 
    ungroup() |> 
    pull(obj) |> 
    which.min()

  out <- res[[minobj[1]]]

  return(out)

}

# compare metab eval criteria for lowest obj, picks only one window comb if multiple min
checkeval_fun <- function(res, toeval, obj){

  tochk <- minobj_fun(res, toeval, obj)

  out <- list(
      obs = meteval(tochk[tochk$name == 'obs', 'value'][[1]][[1]])$cmp,
      dtd = meteval(tochk[tochk$name == 'dtd', 'value'][[1]][[1]])$cmp
    ) |> 
    enframe() |> 
    unnest('value')
  
  return(out)

}

# plot metab obs v dtd for lowest obj, picks only one window comb if multiple min
metplo_fun <- function(res, toeval, obj, ttl){

  tochk <- minobj_fun(res, toeval, obj)

  # observed DO plot
  p1 <- plot(tochk[tochk$name == 'obs', 'value'][[1]][[1]], by = 'days') + 
    labs(title = 'Observed DO metabolism')
  p2 <- plot(tochk[tochk$name == 'dtd', 'value'][[1]][[1]], by = 'days') + 
    coord_cartesian(ylim = range(p1$data$val, na.rm = T)) +
    labs(title = paste('Detided DO metabolism, ', ttl))

  out <- p1 + p2 + plot_layout(ncol = 1)

  return(out)

}

# all criteria
crit <- 'objall'
ttl <- 'all criteria'
objplo_fun(toeval, crit, ttl)
minobj_fun(res, toeval, crit)
checkeval_fun(res, toeval, crit)
metplo_fun(res, toeval, crit, ttl)

# anom and sd criteria
crit <- 'objnomean'
ttl <- 'no mean criteria'
objplo_fun(toeval, crit, ttl)
minobj_fun(res, toeval, crit)
checkeval_fun(res, toeval, crit)
metplo_fun(res, toeval, crit, ttl)

# anom criteria only
crit <- 'objanom'
ttl <- 'anomalous criteria only'
objplo_fun(toeval, crit, ttl)
minobj_fun(res, toeval, crit)
checkeval_fun(res, toeval, crit)
metplo_fun(res, toeval, crit, ttl)

# mean criteria only
crit <- 'objmean'
ttl <- 'mean criteria only'
objplo_fun(toeval, crit, ttl)
minobj_fun(res, toeval, crit)
checkeval_fun(res, toeval, crit)
metplo_fun(res, toeval, crit, ttl)

# sd criteria only
crit <- 'objsd'
ttl <- 'sd criteria only'
objplo_fun(toeval, crit, ttl)
minobj_fun(res, toeval, crit)
checkeval_fun(res, toeval, crit)
metplo_fun(res, toeval, crit, ttl)