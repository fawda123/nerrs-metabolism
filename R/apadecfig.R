# setup ---------------------------------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(here)

source(here('R/funcs.R'))

# compare ebase monthly avg -------------------------------------------------------------------

load(file = here::here('data/apacpdecebaseobs.RData'))
load(file = here::here('data/apacpdecebasedtd.RData'))
load(file = here::here('data/apadbdecebaseobs.RData'))
load(file = here::here('data/apadbdecebasedtd.RData'))
load(file = here::here('data/apaebdecebaseobs.RData'))
load(file = here::here('data/apaebdecebasedtd.RData'))

toplo <- list(
  apacpdecebaseobs = apacpdecebaseobs,
  apacpdecebasedtd = apacpdecebasedtd,
  apadbdecebaseobs = apadbdecebaseobs,
  apadbdecebasedtd = apadbdecebasedtd,
  apaebdecebaseobs = apaebdecebaseobs,
  apaebdecebasedtd = apaebdecebasedtd
) |> 
  enframe() |> 
  unnest('value') |> 
  mutate(
    mo = month(Date), 
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

# compare odum monthly avg --------------------------------------------------------------------

load(file = here::here('data/apacpdecodumobs.RData'))
load(file = here::here('data/apacpdecodumdtd.RData'))
load(file = here::here('data/apadbdecodumobs.RData'))
load(file = here::here('data/apadbdecodumdtd.RData'))
load(file = here::here('data/apaebdecodumobs.RData'))
load(file = here::here('data/apaebdecodumdtd.RData'))

toplo <- list(
  apacpdecodumobs = apacpdecodumobs,
  apacpdecodumdtd = apacpdecodumdtd,
  apadbdecodumobs = apadbdecodumobs,
  apadbdecodumdtd = apadbdecodumdtd,
  apaebdecodumobs = apaebdecodumobs,
  apaebdecodumdtd = apaebdecodumdtd
) |> 
  enframe() |> 
  unnest('value') |> 
  mutate(
    mo = month(DateTimeStamp),
  ) |> 
  select(name, mo, P = Pg, R = Rt, NEM) |> 
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

png('~/Desktop/odum_monthly.png', width = 8, height = 6, units = 'in', res = 300)
print(p)
dev.off()

# compare ebase monthly avg across year categories --------------------------------------------

load(file = here::here('data/apacpdecebaseobs.RData'))
load(file = here::here('data/apacpdecebasedtd.RData'))
load(file = here::here('data/apadbdecebaseobs.RData'))
load(file = here::here('data/apadbdecebasedtd.RData'))
load(file = here::here('data/apaebdecebaseobs.RData'))
load(file = here::here('data/apaebdecebasedtd.RData'))

toplo <- list(
  apacpdecebaseobs = apacpdecebaseobs,
  apacpdecebasedtd = apacpdecebasedtd,
  apadbdecebaseobs = apadbdecebaseobs,
  apadbdecebasedtd = apadbdecebasedtd,
  apaebdecebaseobs = apaebdecebaseobs,
  apaebdecebasedtd = apaebdecebasedtd
) |> 
  enframe() |> 
  unnest('value') |> 
  mutate(
    mo = month(Date), 
    yr = year(Date),
    yrcat = cut(yr, breaks = c(2001, 2005, 2009, 2013, 2017, 2021, 2025), labels = c('2002-2005', '2006-2009', '2010-2013', '2014-2017', '2018-2021', '2022-2024')),
    yrcat = factor(yrcat, levels = c('2002-2005', '2006-2009', '2010-2013', '2014-2017', '2018-2021', '2022-2024')),
    NEM = P - R,
    R = -1 * R
  ) |> 
  select(name, yrcat, mo, yr, P, R, NEM) |> 
  pivot_longer(cols = c(P, R, NEM), names_to = 'type', values_to = 'value') |>
  summarise(
    ave = mean(value, na.rm = TRUE),
    lov = tryCatch(t.test(value, na.rm = T)$conf.int[1], error = function(e) NA),
    hiv = tryCatch(t.test(value, na.rm = T)$conf.int[2], error = function(e) NA),
    .by = c(name, type, yrcat, mo)
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
    dotyp = factor(dotyp, levels = c('Observed', 'Detided'))#,
    # date = ymd(paste(yr, mo, 1, sep = '-'))
  )

toplo1 <- toplo |> filter(type == 'R')

cols <- RColorBrewer::brewer.pal(length(levels(toplo1$yrcat)), 'Reds')

# use a continuous color palette
p <- ggplot(toplo1, aes(x = mo, y = ave, group = yrcat, color = yrcat)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_errorbar(aes(ymin = lov, ymax = hiv), width = 0.5) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_manual(values = cols) +
  facet_grid(site ~ dotyp) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(), 
    legend.position = 'top', 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  ) +
  labs(
    x = NULL,
    y = 'mmol O2 m-2 d-1', 
    color = NULL
  )

png('~/Desktop/ebase_monthlyyrcat.png', width = 8, height = 6, units = 'in', res = 300)
print(p)
dev.off()

# compare odum monthly avg across year categories ---------------------------------------------

load(file = here::here('data/apacpdecodumobs.RData'))
load(file = here::here('data/apacpdecodumdtd.RData'))
load(file = here::here('data/apadbdecodumobs.RData'))
load(file = here::here('data/apadbdecodumdtd.RData'))
load(file = here::here('data/apaebdecodumobs.RData'))
load(file = here::here('data/apaebdecodumdtd.RData'))

toplo <- list(
  apacpdecodumobs = apacpdecodumobs,
  apacpdecodumdtd = apacpdecodumdtd,
  apadbdecodumobs = apadbdecodumobs,
  apadbdecodumdtd = apadbdecodumdtd,
  apaebdecodumobs = apaebdecodumobs,
  apaebdecodumdtd = apaebdecodumdtd
) |> 
  enframe() |> 
  unnest('value') |> 
  mutate(
    mo = month(DateTimeStamp), 
    yr = year(DateTimeStamp),
    yrcat = cut(yr, breaks = c(2001, 2005, 2009, 2013, 2017, 2021, 2025), labels = c('2002-2005', '2006-2009', '2010-2013', '2014-2017', '2018-2021', '2022-2024')),
    yrcat = factor(yrcat, levels = c('2002-2005', '2006-2009', '2010-2013', '2014-2017', '2018-2021', '2022-2024'))
  ) |> 
  select(name, yrcat, mo, yr, P = Pg, R = Rt, NEM) |> 
  pivot_longer(cols = c(P, R, NEM), names_to = 'type', values_to = 'value') |>
  summarise(
    ave = mean(value, na.rm = TRUE),
    lov = tryCatch(t.test(value, na.rm = T)$conf.int[1], error = function(e) NA),
    hiv = tryCatch(t.test(value, na.rm = T)$conf.int[2], error = function(e) NA),
    .by = c(name, type, yrcat, mo)
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
    dotyp = factor(dotyp, levels = c('Observed', 'Detided'))#,
    # date = ymd(paste(yr, mo, 1, sep = '-'))
  )

toplo1 <- toplo |> filter(type == 'R')

cols <- RColorBrewer::brewer.pal(length(levels(toplo1$yrcat)), 'Reds')

# use a continuous color palette
p <- ggplot(toplo1, aes(x = mo, y = ave, group = yrcat, color = yrcat)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_errorbar(aes(ymin = lov, ymax = hiv), width = 0.5) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_manual(values = cols) +
  facet_grid(site ~ dotyp) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(), 
    legend.position = 'top', 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  ) +
  labs(
    x = NULL,
    y = 'mmol O2 m-2 d-1', 
    color = NULL
  )

png('~/Desktop/odum_monthlyyrcat.png', width = 8, height = 6, units = 'in', res = 300)
print(p)
dev.off()

# compare ebase monthly avg across year -------------------------------------------------------

load(file = here::here('data/apacpdecebaseobs.RData'))
load(file = here::here('data/apacpdecebasedtd.RData'))
load(file = here::here('data/apadbdecebaseobs.RData'))
load(file = here::here('data/apadbdecebasedtd.RData'))
load(file = here::here('data/apaebdecebaseobs.RData'))
load(file = here::here('data/apaebdecebasedtd.RData'))

toplo <- list(
  apacpdecebaseobs = apacpdecebaseobs,
  apacpdecebasedtd = apacpdecebasedtd,
  apadbdecebaseobs = apadbdecebaseobs,
  apadbdecebasedtd = apadbdecebasedtd,
  apaebdecebaseobs = apaebdecebaseobs,
  apaebdecebasedtd = apaebdecebasedtd
) |> 
  enframe() |> 
  unnest('value') |> 
  mutate(
    mo = month(Date), 
    yr = year(Date),
    NEM = P - R,
    R = -1 * R
  ) |> 
  select(name, mo, yr, P, R, NEM) |> 
  pivot_longer(cols = c(P, R, NEM), names_to = 'type', values_to = 'value') |>
  summarise(
    ave = mean(value, na.rm = TRUE),
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
    dotyp = factor(dotyp, levels = c('Observed', 'Detided'))
  ) |> 
  select(-name) |> 
  filter(!is.na(ave))

toplo1 <- toplo |> 
  filter(type == 'R') |> 
  filter(ave >= -400)

# use a continuous color palette
p <- ggplot(toplo1, aes(x = mo, y = yr)) +
  geom_tile(aes(fill = ave)) +
  # scale_color_manual(values = cols) +
  scale_fill_distiller(palette = 'Spectral', na.value = 'white') +
  scale_x_continuous(breaks = 1:12, labels = month.abb, expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  facet_grid(site ~ dotyp) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    legend.position = 'top', 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6)
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = 'mmol O2 m-2 d-1'
  )

png('~/Desktop/ebase_monthlyyrs.png', width = 6, height = 9, units = 'in', res = 300)
print(p)
dev.off()

# compare odum monthly avg across year --------------------------------------------------------

load(file = here::here('data/apacpdecodumobs.RData'))
load(file = here::here('data/apacpdecodumdtd.RData'))
load(file = here::here('data/apadbdecodumobs.RData'))
load(file = here::here('data/apadbdecodumdtd.RData'))
load(file = here::here('data/apaebdecodumobs.RData'))
load(file = here::here('data/apaebdecodumdtd.RData'))

toplo <- list(
  apacpdecodumobs = apacpdecodumobs,
  apacpdecodumdtd = apacpdecodumdtd,
  apadbdecodumobs = apadbdecodumobs,
  apadbdecodumdtd = apadbdecodumdtd,
  apaebdecodumobs = apaebdecodumobs,
  apaebdecodumdtd = apaebdecodumdtd
) |> 
  enframe() |> 
  unnest('value') |> 
  mutate(
    mo = month(DateTimeStamp), 
    yr = year(DateTimeStamp)
  ) |> 
  select(name, mo, yr, P = Pg, R = Rt, NEM) |> 
  pivot_longer(cols = c(P, R, NEM), names_to = 'type', values_to = 'value') |>
  summarise(
    ave = mean(value, na.rm = TRUE),
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
    dotyp = factor(dotyp, levels = c('Observed', 'Detided'))
  ) |> 
  select(-name) |> 
  filter(!is.na(ave))

toplo1 <- toplo |> 
  filter(type == 'R') |> 
  filter(ave >= -400) |> 
  filter(ave <= 0)

# use a continuous color palette
p <- ggplot(toplo1, aes(x = mo, y = yr)) +
  geom_tile(aes(fill = ave)) +
  # scale_color_manual(values = cols) +
  scale_fill_distiller(palette = 'Spectral', na.value = 'white') +
  scale_x_continuous(breaks = 1:12, labels = month.abb, expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  facet_grid(site ~ dotyp) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    legend.position = 'top', 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6)
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = 'mmol O2 m-2 d-1'
  )

png('~/Desktop/odum_monthlyyrs.png', width = 6, height = 9, units = 'in', res = 300)
print(p)
dev.off()

# compare ebase and odum ----------------------------------------------------------------------

load(file = here::here('data/apacpdecebaseobs.RData'))
load(file = here::here('data/apacpdecebasedtd.RData'))
load(file = here::here('data/apadbdecebaseobs.RData'))
load(file = here::here('data/apadbdecebasedtd.RData'))
load(file = here::here('data/apaebdecebaseobs.RData'))
load(file = here::here('data/apaebdecebasedtd.RData'))

toplo1 <- list(
    apacpdecebaseobs = apacpdecebaseobs,
    apacpdecebasedtd = apacpdecebasedtd,
    apadbdecebaseobs = apadbdecebaseobs,
    apadbdecebasedtd = apadbdecebasedtd,
    apaebdecebaseobs = apaebdecebaseobs,
    apaebdecebasedtd = apaebdecebasedtd
  ) |> 
  enframe() |> 
  unnest('value') |> 
  select(name, Date, P, R, NEM, D) |>
  pivot_longer(cols = c(P, R, NEM, D), names_to = 'type', values_to = 'value') |>
  summarise(
    value = mean(value, na.rm = T), 
    .by = c(name, Date, type)
  ) |>
  mutate(
    name = gsub('ebase', '', name),
    value = case_when(
      type == 'R' ~ -1 * value, 
      T  ~ value
    )
  )

load(file = here::here('data/apacpdecodumobs.RData'))
load(file = here::here('data/apacpdecodumdtd.RData'))
load(file = here::here('data/apadbdecodumobs.RData'))
load(file = here::here('data/apadbdecodumdtd.RData'))
load(file = here::here('data/apaebdecodumobs.RData'))
load(file = here::here('data/apaebdecodumdtd.RData'))

toplo2 <- list(
    apacpdecodumobs = apacpdecodumobs,
    apacpdecodumdtd = apacpdecodumdtd,
    apadbdecodumobs = apadbdecodumobs,
    apadbdecodumdtd = apadbdecodumdtd,
    apaebdecodumobs = apaebdecodumobs,
    apaebdecodumdtd = apaebdecodumdtd
  ) |> 
  enframe() |> 
  unnest('value') |> 
  select(name, Date = metab_date, P = Pg, R = Rt, NEM, D) |> 
  pivot_longer(cols = c(P, R, NEM, D), names_to = 'type', values_to = 'value') |>
  summarise(
    value = mean(value, na.rm = T), 
    .by = c(name, Date, type)
  ) |> 
  mutate(
    name = gsub('odum', '', name)
  )

toplo <- inner_join(toplo1, toplo2, by = c('name', 'Date', 'type'), suffix = c('.ebase', '.odum')) |> 
  mutate(
    type = factor(type, levels = c('P', 'R', 'NEM', 'D')),
    dotyp = ifelse(grepl('obs$', name), 'Observed', 'Detided'), 
    dotyp = factor(dotyp, levels = c('Observed', 'Detided')),
    name = case_when(
      grepl('^apacp', name) ~ 'Cat Point',
      grepl('^apadb', name) ~ 'Dry Bar',
      grepl('^apaeb', name) ~ 'East Bay',  
    )
  )

obsplo <- cmpplo_fun(toplo, 'Observed')
dtdplo <- cmpplo_fun(toplo, 'Detided')

png('~/Desktop/ebase_odum_obs.png', width = 10, height = 8, units = 'in', res = 300)
print(obsplo)
dev.off()

png('~/Desktop/ebase_odum_dtd.png', width = 10, height = 8, units = 'in', res = 300)
print(dtdplo)
dev.off()

