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

