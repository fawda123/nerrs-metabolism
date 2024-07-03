library(WtRegDO)
library(tidyverse)
library(patchwork)

data(SAPDC)

# metadata for the location
tz <- 'America/Jamaica'
lat <- 31.39
long <- -81.28


# get instantaneous flux and daily WtRegDO metabolism estimates -------------------------------

# instantaneous metab
metab_flux <- ecometab(SAPDC, DO_var = 'DO_obs', tz = tz,
                  lat = lat, long = long, instant = T)

flux <- metab_flux |>
  dplyr::mutate(
    night_hrs = 24 - day_hrs
  ) |>
  dplyr::summarise(
    D_night = mean(night_hrs * D_n, na.rm = T),
    D_day = mean(day_hrs * D_d, na.rm = T),
    TROC_night = mean(night_hrs * DOF_n, na.rm = T),
    TROC_day = mean(day_hrs * DOF_d, na.rm = T),
    .by = c('metab_date')
  ) |>
  dplyr::mutate(
    NEM_day = TROC_day - D_day,
    NEM_night = TROC_night - D_night
  ) |>
  pivot_longer(c('D_night', 'D_day', 'TROC_night', 'TROC_day', 'NEM_night', 'NEM_day'),
               names_to = 'variable', values_to = 'value') |>
  mutate(
    variable = factor(variable,
                      levels = c('D_night', 'NEM_night', 'TROC_night', 'D_day', 'NEM_day', 'TROC_day'))
  )

# daily WtRegDO metab
metab_daily <- ecometab(SAPDC, DO_var = 'DO_obs', tz = tz,
                        lat = lat, long = long, instant = F)

daily <- metab_daily |>
  select(metab_date = Date, GPP = Pg, ER = Rt, NEP = NEM) |>
  pivot_longer(c('GPP', 'ER', 'NEP'), names_to = 'variable', values_to = 'value') |>
  mutate(
    variable = factor(variable, levels = c('NEP', 'GPP', 'ER'))
  )

# partially recreate figure 4 with select days ------------------------------------------------

toplo1 <- flux |>
  dplyr::filter(metab_date >= as.Date('2012-02-08') & metab_date <= as.Date('2012-02-15'))

p1 <- ggplot(toplo1) +
  geom_col(aes(x = metab_date, y = value, fill = variable), position = 'dodge') +
  scale_fill_manual(values = c("lightgray", "darkgray", "black", "gold", "orange", "tomato"), name = "",
                    labels=c("Night F", "Night NEP", "Night TROC", "Day F", "Day NEP", "Day TROC"),
                    guide = guide_legend(nrow = 1)) +
  theme_bw() +
  theme(legend.position = 'top') +
  labs(
    x = NULL,
    y = expression("mmol" ~O["2"]/~m^2/~d)
  )

toplo2 <- daily |>
  dplyr::filter(metab_date >= as.Date('2012-02-08') & metab_date <= as.Date('2012-02-15'))

p2 <- ggplot(toplo2, aes(x = metab_date, y = value, color = variable)) +
  geom_line(size = 1) +
  geom_abline(intercept = 0, slope = 0, linetype = 'solid') +
  geom_point() +
  theme_bw() +
  theme(legend.position = 'top') +  labs(
    x = NULL,
    y = expression("mmol" ~O["2"]/~m^2/~d),
    color = NULL
  )

p1 + p2 + plot_layout(ncol = 1)

# make sure daily nep + nightly nep is equal to WtRegDO NEP -----------------------------------

# all dates combine flux and daily metabolism
fluxnep <- flux |>
  pivot_wider(names_from = variable, values_from = value) |>
  mutate(
    NEP = NEM_day + NEM_night
  )
dailynep <- daily |>
  pivot_wider(names_from = variable, values_from = value) |>
  select(metab_date, NEP)

toplo3 <- left_join(fluxnep, dailynep, by = 'metab_date', suffix = c(" flux", " WtRegDO daily"))

ggplot(toplo3, aes(x = `NEP flux`, y = `NEP WtRegDO daily`)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = 'solid') +
  theme(legend.position = 'top')
