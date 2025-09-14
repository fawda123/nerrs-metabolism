# setup ---------------------------------------------------------------------------------------

library(tidyverse)
library(EBASE)
library(doParallel)
library(foreach)
library(here)
library(plotly)
library(lubridate)
library(SWMPr)
library(here)
library(readxl)
library(purrr)
library(fuzzyjoin)
library(WtRegDO)
library(hms)

source(here('R/funcs.R'))

# prep raw data -------------------------------------------------------------------------------

# get raw
pth <- here('data-raw/965773.zip')
apaebmetraw <- import_local(pth, 'apaebmet', trace = T)
apacpraw <- import_local(pth, 'apacpwq', trace = T)
apadbraw <- import_local(pth, 'apadbwq', trace = T)
apaebraw <- import_local(pth, 'apaebwq', trace = T)

# for PAR conversion to W/m2
Jpmolph <-  0.2175e6 # 1 mol-photons = 0.2175e6 J for ave PAR wavelength of 550nm be Watts/m2 for EBASE

# qaqc apaebmet
# atemp in C
# bp in mb (for DOsat with Odum)
# wspd in m/s
# totpar in mmol/m2
apaebmet <- apaebmetraw |> 
  qaqc(qaqc_keep = c(0, 1, 2, 3, 4, 5)) |> 
  subset(select = c('datetimestamp', 'atemp', 'bp', 'wspd', 'totpar')) |> 
  setstep(timestep = 30) |>
  mutate(
    totpar = totpar * 2, # 15min to 30min total
    par_wm2 = totpar * Jpmolph * 1e-3 / 30 / 60, # convert to W / m2
  ) |> 
  select(
    datetimestamp, 
    atemp_c = atemp, 
    bp_mb = bp,
    wspd_ms = wspd,
    par_wm2
  )

# level correction data
levdat <- list(
    apacp = 'Cat Point', 
    apadb = 'Dry Bar',
    apaeb = 'East Bay Bottom'
  ) |> 
  enframe() |> 
  mutate(
    dat = map(value, ~ read_excel(
      path = here('data-raw/Level to Depth conversions.xlsx'), 
      sheet = .x, 
      skip = 1, 
      col_types = c('date', 'text', 'date', 'text', 'numeric'),
      na = '-'
      )
    )
  ) |> 
  select(-value) |> 
  unnest(dat) |> 
  rename(
    deploydt = `Deployment Date`, 
    deploytm = `Deployment Time`, 
    retrievedt = `Retrieval Date`, 
    retrievetm = `Retrieval Time`,
    levcrr = `Subtract from Level Values`
  ) |> 
  mutate(
    across(c(deploydt, retrievedt), as.Date),
    deploytm = as.character(as_hms(as.numeric(deploytm) * 86400)),
    retrievetm = as.character(as_hms(as.numeric(retrievetm) * 86400), '%H:%M'),
    deploytm = lubridate::ymd_hms(paste(deploydt, deploytm), tz = 'America/Jamaica'),
    retrievetm = lubridate::ymd_hms(paste(retrievedt, retrievetm), tz = 'America/Jamaica')
  ) |> 
  select(name, deploy = deploytm, retrieve = retrievetm, levcrr) |> 
  na.omit() |> 
  group_nest(name)

# addl processing
# qaqc filtering
# use level with correction after last datetime with depth
# add 0.3m to depth for offset
# combine with met
# save file
# fill missing where maximum gap is 4 records
list(
  apacp = apacpraw,
  apadb = apadbraw,
  apaeb = apaebraw
  ) |> 
  enframe() |> 
  left_join(levdat, by = 'name') |>
  pmap(function(name, value, data){
  
    cat(name, '\n')

    value |> 
      qaqc(qaqc_keep = c(0, 1, 2, 3, 4, 5)) |> 
      subset(select = c('datetimestamp', 'temp', 'do_mgl', 'sal', 'depth', 'level')) |> 
      setstep(timestep = 30) |> 
      comb(apaebmet, method = 'intersect', timestep = 30) |> 
      na.approx(maxgap = 4) |> 
      interval_left_join(
        data,
        by = c("datetimestamp" = "deploy", "datetimestamp" = "retrieve"),
        type = "within"
      ) |> 
      mutate(
        depth = case_when(
          is.na(depth) & !is.na(level) ~ level - levcrr,
          T ~ depth
        ), 
        depth = depth + 0.3,
        datetimestamp = format(datetimestamp, "%Y-%m-%dT%H:%M:%S")
      ) |> 
      select(
        datetimestamp, 
        temp_c = temp, 
        sal_ppt = sal, 
        do_mgl, 
        depth_m = depth, 
        atemp_c, 
        bp_mb, 
        wspd_ms, 
        par_wm2
        ) |> 
      write.csv(file = here(paste0('data-raw/', name, 'decraw.csv')), row.names = F)
  
    }
  )

# apa cat point detide ------------------------------------------------------------------------

ncores <- parallel::detectCores() - 2
registerDoParallel(cores = ncores)

tomod <- read.csv(here('data-raw/apacpdecraw.csv')) |> 
  mutate(
    datetimestamp = ymd_hms(datetimestamp, tz = 'America/Jamaica')
  ) |> 
  select(DateTimeStamp = datetimestamp, Sal = sal_ppt, DO_mgl = do_mgl, Tide = depth_m, 
         WSpd = wspd_ms, Temp = temp_c, BP = bp_mb, ATemp = atemp_c, PAR = par_wm2) |> 
  filter(!is.na(Tide))

lat <- 29.7021
long <- -84.8802

apacpdtd <- wtreg(tomod, DO_obs = "DO_mgl", wins = list(9, 1, 1),
             lat = lat, long = long, tz = 'America/Jamaica', progress = T, parallel = T)

save(apacpdtd, file = here('data/apacpdtd.RData'))

# apa dry bay detide --------------------------------------------------------------------------

ncores <- parallel::detectCores() - 2
registerDoParallel(cores = ncores)

tomod <- read.csv(here('data-raw/apadbdecraw.csv')) |> 
  mutate(
    datetimestamp = ymd_hms(datetimestamp, tz = 'America/Jamaica')
  ) |> 
  select(DateTimeStamp = datetimestamp, Sal = sal_ppt, DO_mgl = do_mgl, Tide = depth_m, 
         WSpd = wspd_ms, Temp = temp_c, BP = bp_mb, ATemp = atemp_c, PAR = par_wm2) |> 
  filter(!is.na(Tide))

lat <- 29.6747
long <- -85.0583

apadbdtd <- wtreg(tomod, DO_obs = "DO_mgl", wins = list(9, 1, 1),
                  lat = lat, long = long, tz = 'America/Jamaica', progress = T, parallel = T)

save(apadbdtd, file = here('data/apadbdtd.RData'))

# apa east bay detide -------------------------------------------------------------------------

ncores <- parallel::detectCores() - 2
registerDoParallel(cores = ncores)

tomod <- read.csv(here('data-raw/apaebdecraw.csv')) |> 
  mutate(
    datetimestamp = ymd_hms(datetimestamp, tz = 'America/Jamaica')
  ) |> 
  select(DateTimeStamp = datetimestamp, Sal = sal_ppt, DO_mgl = do_mgl, Tide = depth_m, 
         WSpd = wspd_ms, Temp = temp_c, BP = bp_mb, ATemp = atemp_c, PAR = par_wm2) |> 
  filter(!is.na(Tide))

lat <- 29.7858
long <- -84.8752

apaebdtd <- wtreg(tomod, DO_obs = "DO_mgl", wins = list(9, 1, 1),
                  lat = lat, long = long, tz = 'America/Jamaica', progress = T, parallel = T)

save(apaebdtd, file = here('data/apaebdtd.RData'))

# combine detided with observed data ----------------------------------------------------------

list(
    apacpdtd = 'apacpdecraw',
    apadbdtd = 'apadbdecraw',
    apaebdtd = 'apaebdecraw'
  ) |> 
  enframe() |> 
  pmap(function(name, value){
  
    cat(name, '\n')

    # actual
    dec <- read.csv(here(paste0('data-raw/', value, '.csv'))) |> 
      mutate(
        datetimestamp = ymd_hms(datetimestamp, tz = 'America/Jamaica')
      ) 
  
    # detided
    load(file = here(paste0('data/', name, '.RData')))
    dtd <- get(name) |> 
      select(
        datetimestamp = DateTimeStamp, 
        donrm_mgl = DO_nrm
        ) |> 
      mutate(
        donrm_mgl = pmax(donrm_mgl, 0) 
      )
  
    # join detided to actual and save
    dec |> 
      left_join(dtd, by = 'datetimestamp') |> 
      mutate(
        datetimestamp = format(datetimestamp, "%Y-%m-%dT%H:%M:%S")
      ) |> 
      write.csv(file = here(paste0('data-raw/', value, '.csv')), row.names = F)

  })

# odum ----------------------------------------------------------------------------------------

list(
    apacp = list(fl = 'apacpdecraw', lat = 29.7021, long = -84.8802),
    apadb = list(fl = 'apadbdecraw', lat = 29.6747, long = -85.0583),
    apaeb = list(fl = 'apaebdecraw', lat = 29.7858, long = -84.8752)
  ) |> 
  enframe() |> 
  pmap(function(name, value){
    
    cat(name, '\n')

    # actual
    dec <- read.csv(here(paste0('data-raw/', value$fl, '.csv'))) |> 
      mutate(
        datetimestamp = ymd_hms(datetimestamp, tz = 'America/Jamaica')
      ) |> 
      select(
        DateTimeStamp = datetimestamp, 
        Temp = temp_c, 
        Sal = sal_ppt, 
        DO_obs = do_mgl, 
        ATemp = atemp_c, 
        BP = bp_mb,
        WSpd = wspd_ms, 
        Tide = depth_m, 
        DO_nrm = donrm_mgl
      )
    
    cat('\tobserved...\n')
    odumobs <- WtRegDO::ecometab(
      dec, 
      DO_var = "DO_obs", 
      lat = value$lat, 
      long = value$long, 
      tz = 'America/Jamaica', 
      gasex = "Wanninkhof", 
      instant = TRUE
    )
    
    obsnm <- paste0(name, 'decodumobs')
    
    # too large for github
    write.csv(odumobs, file = here(paste0('~/Desktop/', obsnm, '.csv')), row.names = F)
    
    # save as binary
    assign(obsnm, odumobs)
    save(list = obsnm, file = here(paste0('data/', obsnm, '.RData')))
    
    cat('\tdetided...\n')
    odumdtd <- WtRegDO::ecometab(
      dec, 
      DO_var = "DO_nrm", 
      lat = value$lat, 
      long = value$long, 
      tz = 'America/Jamaica', 
      gasex = "Wanninkhof", 
      instant = TRUE
    )
    
    dtdnm <- paste0(name, 'decodumdtd')
    
    # too large for github
    write.csv(odumdtd, file = here(paste0('~/Desktop/', dtdnm, '.csv')), row.names = F)
    
    # save as binary
    assign(dtdnm, odumdtd)
    save(list = dtdnm, file = here(paste0('data/', dtdnm, '.RData')))
    
  
  })

# apa cat point ebase -------------------------------------------------------------------------

# prep apacp data
apacpdat <- ebsdatprp(here('data-raw/apacpdecraw.csv'))

ncores <- parallel::detectCores() - 2

##
# EBASE observed

tomod <- apacpdat |> 
  select(-DO_nrm)

apacpdecebaseobs <- ebase_years(tomod, Z = tomod$Depth, interval = 1800, ndays = 1, n.chains = 4,
                           bprior = c(0.251, 1e-6), ncores = ncores, quiet = F)

save(apacpdecebaseobs, file = here('data/apacpdecebaseobs.RData'))

##
# EBASE detided

tomod <- apacpdat |> 
  select(-DO_obs) |> 
  rename(DO_obs = DO_nrm)

apacpdecebasedtd <- ebase_years(tomod, Z = tomod$Depth, interval = 1800, ndays = 1, n.chains = 4,
                           bprior = c(0.251, 1e-6), ncores = ncores, quiet = F)

save(apacpdecebasedtd, file = here('data/apacpdecebasedtd.RData'))

##
# view results

data(apacpdecebaseobs)
data(apacpdecebasedtd)

ebsresplo(apacpdecebaseobs, apacpdecebasedtd)

# apa dry bar ebase ---------------------------------------------------------------------------

# prep apadb data
apadbdat <- ebsdatprp(here('data-raw/apadbdecraw.csv'))

ncores <- parallel::detectCores() - 2

##
# EBASE observed

tomod <- apadbdat |> 
  select(-DO_nrm)

apadbdecebaseobs <- ebase_years(tomod, Z = tomod$Depth, interval = 1800, ndays = 1, n.chains = 4,
                           bprior = c(0.251, 1e-6), ncores = ncores, quiet = F)

save(apadbdecebaseobs, file = here('data/apadbdecebaseobs.RData'))

##
# EBASE detided

tomod <- apadbdat |> 
  select(-DO_obs) |> 
  rename(DO_obs = DO_nrm)

apadbdecebasedtd <- ebase_years(tomod, Z = tomod$Depth, interval = 1800, ndays = 1, n.chains = 4,
                           bprior = c(0.251, 1e-6), ncores = ncores, quiet = F)

save(apadbdecebasedtd, file = here('data/apadbdecebasedtd.RData'))

##
# view results

data(apadbdecebaseobs)
data(apadbdecebasedtd)

ebsresplo(apadbdecebaseobs, apadbdecebasedtd)

# apa east bay ebase --------------------------------------------------------------------------

# prep apaeb data
apaebdat <- ebsdatprp(here('data-raw/apaebdecraw.csv'))

ncores <- parallel::detectCores() - 2

##
# EBASE observed

tomod <- apaebdat |> 
  select(-DO_nrm)

apaebdecebaseobs <- ebase_years(tomod, Z = tomod$Depth, interval = 1800, ndays = 1, n.chains = 4,
                           bprior = c(0.251, 1e-6), ncores = ncores, quiet = F)

save(apaebdecebaseobs, file = here('data/apaebdecebaseobs.RData'))

##
# EBASE detided

tomod <- apaebdat |> 
  select(-DO_obs) |> 
  rename(DO_obs = DO_nrm)

apaebdecebasedtd <- ebase_years(tomod, Z = tomod$Depth, interval = 1800, ndays = 1, n.chains = 4,
                           bprior = c(0.251, 1e-6), ncores = ncores, quiet = F)

save(apaebdecebasedtd, file = here('data/apaebdecebasedtd.RData'))

##
# view results

data(apaebdecebaseobs)
data(apaebdecebasedtd)

ebsresplo(apaebdecebaseobs, apaebdecebasedtd)

# save all ebase as csv -----------------------------------------------------------------------

load(file = here::here('data/apacpdecebaseobs.RData'))
load(file = here::here('data/apacpdecebasedtd.RData'))
load(file = here::here('data/apadbdecebaseobs.RData'))
load(file = here::here('data/apadbdecebasedtd.RData'))
load(file = here::here('data/apaebdecebaseobs.RData'))
load(file = here::here('data/apaebdecebasedtd.RData'))

# save all
list(
    apacpdecebaseobs = apacpdecebaseobs,
    apacpdecebasedtd = apacpdecebasedtd,
    apadbdecebaseobs = apadbdecebaseobs,
    apadbdecebasedtd = apadbdecebasedtd,
    apaebdecebaseobs = apaebdecebaseobs,
    apaebdecebasedtd = apaebdecebasedtd
  ) |> 
  enframe() |> 
  purrr::pmap(
    function(name, value) {
      cat(name, '\n')
      write.csv(value, file = here::here(paste0('~/Desktop/', name, '.csv')), row.names = F)
    }
  )
