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
  mutate(
    par_wm2 = totpar * Jpmolph * 1e-3 / 15/ 60, # convert to W / m2
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
      setstep(timestep = 15) |> 
      comb(apaebmet, method = 'intersect') |> 
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
    datetimestamp = ymd_hms(datetimestamp, tz = 'America/Jamaica'), 
    depth_m = depth_m + 0.3
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
    datetimestamp = ymd_hms(datetimestamp, tz = 'America/Jamaica'), 
    depth_m = depth_m + 0.3
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
    datetimestamp = ymd_hms(datetimestamp, tz = 'America/Jamaica'), 
    depth_m = depth_m + 0.3
  ) |> 
  select(DateTimeStamp = datetimestamp, Sal = sal_ppt, DO_mgl = do_mgl, Tide = depth_m, 
         WSpd = wspd_ms, Temp = temp_c, BP = bp_mb, ATemp = atemp_c, PAR = par_wm2) |> 
  filter(!is.na(Tide))

lat <- 29.7858
long <- -84.8752

apaebdtd <- wtreg(tomod, DO_obs = "DO_mgl", wins = list(9, 1, 1),
                  lat = lat, long = long, tz = 'America/Jamaica', progress = T, parallel = T)

save(apaebdtd, file = here('data/apaebdtd.RData'))

# combine detided do with input data for ebase ------------------------------------------------

# apa cat point odum --------------------------------------------------------------------------

# apa dry bar odum ----------------------------------------------------------------------------

# apa east bay odum ---------------------------------------------------------------------------

# apa cat point ebase -------------------------------------------------------------------------

# prep apacp data
apacpdatraw <- read.csv(here('data-raw/apacpdecraw.csv'))
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

ncores <- parallel::detectCores() - 2

##
# EBASE observed

tomod <- apacpdat |> 
  select(-DO_dtd)

apacpdecobs <- ebase_years(tomod, Z = tomod$Depth, interval = 900, ndays = 1, n.chains = 4,
                           bprior = c(0.251, 1e-6), ncores = ncores, quiet = F)

save(apacpdecobs, file = here('data/apacpdecobs.RData'))

##
# EBASE detided

tomod <- apacpdat |> 
  select(-DO_obs) |> 
  rename(DO_obs = DO_dtd)

apacpdecdtd <- ebase_years(tomod, Z = tomod$Depth, interval = 900, ndays = 1, n.chains = 4,
                           bprior = c(0.251, 1e-6), ncores = ncores, quiet = F)

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

# apa dry bar ebase ---------------------------------------------------------------------------

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

ncores <- parallel::detectCores() - 2

##
# EBASE observed

tomod <- apadbdat |> 
  select(-DO_dtd)

apadbdecobs <- ebase_years(tomod, Z = tomod$Depth, interval = 900, ndays = 1, n.chains = 4,
                           bprior = c(0.251, 1e-6), ncores = ncores, quiet = F)

save(apadbdecobs, file = here('data/apadbdecobs.RData'))

##
# EBASE detided

tomod <- apadbdat |> 
  select(-DO_obs) |> 
  rename(DO_obs = DO_dtd)

apadbdecdtd <- ebase_years(tomod, Z = tomod$Depth, interval = 900, ndays = 1, n.chains = 4,
                           bprior = c(0.251, 1e-6), ncores = ncores, quiet = F)

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

# apa east bay ebase --------------------------------------------------------------------------

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

ncores <- parallel::detectCores() - 2

##
# EBASE observed

tomod <- apaebdat |> 
  select(-DO_dtd)

apaebdecobs <- ebase_years(tomod, Z = tomod$Depth, interval = 900, ndays = 1, n.chains = 4,
                           bprior = c(0.251, 1e-6), ncores = ncores, quiet = F)

save(apaebdecobs, file = here('data/apaebdecobs.RData'))

##
# EBASE detided

tomod <- apaebdat |> 
  select(-DO_obs) |> 
  rename(DO_obs = DO_dtd)

apaebdecdtd <- ebase_years(tomod, Z = tomod$Depth, interval = 900, ndays = 1, n.chains = 4,
                           bprior = c(0.251, 1e-6), ncores = ncores, quiet = F)

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


# save all as csv -----------------------------------------------------------------------------

load(file = here::here('data/apacpdecobs.RData'))
load(file = here::here('data/apacpdecdtd.RData'))
load(file = here::here('data/apadbdecobs.RData'))
load(file = here::here('data/apadbdecdtd.RData'))
load(file = here::here('data/apaebdecobs.RData'))
load(file = here::here('data/apaebdecdtd.RData'))

# save all
list(
    apacpdecobs = apacpdecobs,
    apacpdecdtd = apacpdecdtd,
    apadbdecobs = apadbdecobs,
    apadbdecdtd = apadbdecdtd,
    apaebdecobs = apaebdecobs,
    apaebdecdtd = apaebdecdtd
  ) |> 
  enframe() |> 
  purrr::pmap(
    function(name, value) {
      cat(name, '\n')
      write.csv(value, file = here::here(paste0('~/Desktop/', name, '.csv')), row.names = F)
    }
  )


# compare monthly avg -------------------------------------------------------------------------

load(file = here::here('data/apacpdecobs.RData'))
load(file = here::here('data/apacpdecdtd.RData'))
load(file = here::here('data/apadbdecobs.RData'))
load(file = here::here('data/apadbdecdtd.RData'))
load(file = here::here('data/apaebdecobs.RData'))
load(file = here::here('data/apaebdecdtd.RData'))

toplo <- list(
  apacpdecobs = apacpdecobs,
  apacpdecdtd = apacpdecdtd,
  apadbdecobs = apadbdecobs,
  apadbdecdtd = apadbdecdtd,
  apaebdecobs = apaebdecobs,
  apaebdecdtd = apaebdecdtd
) |> 
  enframe() |> 
  unnest('value') |> 
  mutate(
    mo = month(Date), 
    NEM = P - R,
    R = -1 * R
  ) |> 
  select(name, mo, P, R, NEM) |> 
  pivot_longer(cols = c(P, R, NEM), names_to = 'type', values_to = 'value') |>
  summarise(
    ave = mean(value, na.rm = TRUE),
    std = sd(value, na.rm = TRUE),
    hiv = t.test(value, na.rm = T)$conf.int[2],
    lov = t.test(value, na.rm = T)$conf.int[1],
    .by = c(name, type, mo)
  ) |> 
  mutate(
    site = case_when(
      grepl('^apacp', name) ~ 'Cat Point',
      grepl('^apadb', name) ~ 'Dry Bar',
      grepl('^apaeb', name) ~ 'East Bay',  
    ), 
    dotyp = case_when(
      grepl('obs$', name) ~ 'Observed',
      grepl('dtd$', name) ~ 'Detided',
    ), 
    dotyp = factor(dotyp, levels = c('Observed', 'Detided'))
  )

p <- ggplot(toplo, aes(x = mo, y = ave, group = type, color = type)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_errorbar(aes(ymin = lov, ymax = hiv), width = 0.4) +
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  facet_grid(site ~ dotyp) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    legend.position = 'top'
  ) +
  labs(
    x = NULL, 
    y = 'mmol O2 m-2 d-1', 
    color = NULL
  )

png('~/Desktop/ebase_monthly.png', width = 8, height = 6, units = 'in', res = 300)
print(p)
dev.off()

# compare monthly avg across years ------------------------------------------------------------

load(file = here::here('data/apacpdecobs.RData'))
load(file = here::here('data/apacpdecdtd.RData'))
load(file = here::here('data/apadbdecobs.RData'))
load(file = here::here('data/apadbdecdtd.RData'))
load(file = here::here('data/apaebdecobs.RData'))
load(file = here::here('data/apaebdecdtd.RData'))

toplo <- list(
  apacpdecobs = apacpdecobs,
  apacpdecdtd = apacpdecdtd,
  apadbdecobs = apadbdecobs,
  apadbdecdtd = apadbdecdtd,
  apaebdecobs = apaebdecobs,
  apaebdecdtd = apaebdecdtd
) |> 
  enframe() |> 
  unnest('value') |> 
  mutate(
    mo = month(Date), 
    yr = year(Date),
    yrcat = cut(yr, breaks = c(2001, 2007, 2012, 2017, 2023), labels = c('2002-2007', '2008-2012', '2013-2017', '2018-2022')),
    yrcat = factor(yrcat, levels = c('2002-2007', '2008-2012', '2013-2017', '2018-2022')),
    NEM = P - R,
    R = -1 * R
  ) |> 
  select(name, yrcat, mo, yr, P, R, NEM) |> 
  pivot_longer(cols = c(P, R, NEM), names_to = 'type', values_to = 'value') |>
  summarise(
    ave = mean(value, na.rm = TRUE),
    lov = t.test(value, na.rm = T)$conf.int[1],
    hiv = t.test(value, na.rm = T)$conf.int[2],
    .by = c(name, type, yr, mo)
  ) |> 
  mutate(
    site = case_when(
      grepl('^apacp', name) ~ 'Cat Point',
      grepl('^apadb', name) ~ 'Dry Bar',
      grepl('^apaeb', name) ~ 'East Bay',  
    ), 
    dotyp = case_when(
      grepl('obs$', name) ~ 'Observed',
      grepl('dtd$', name) ~ 'Detided',
    ), 
    dotyp = factor(dotyp, levels = c('Observed', 'Detided')),
    date = ymd(paste(yr, mo, 1, sep = '-'))
  )

toplo1 <- toplo |> filter(type == 'R')

# use a continuous color palette
p <- ggplot(toplo1, aes(x = date, y = ave, group = yr, color = yr)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_errorbar(aes(ymin = lov, ymax = hiv), width = 0.5) +
  geom_point() +
  geom_line() +
  scale_x_date(breaks = unique(toplo1$date), labels = month.abb) +
  scale_color_manual(values = c('#FDE725FF', '#5DC863FF', '#21908CFF', '#440154FF')) +
  facet_grid(site ~ dotyp) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    legend.position = 'top'
  ) +
  labs(
    x = NULL,
    y = 'mmol O2 m-2 d-1', 
    color = NULL
  )

png('~/Desktop/ebase_monthly.png', width = 8, height = 6, units = 'in', res = 300)
print(p)
dev.off()

