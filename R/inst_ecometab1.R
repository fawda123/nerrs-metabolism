# ecometab for 2002 to 2020 dry bar

library(tidyverse)
library(WtRegDO)
library(lubridate)
library(here)

# # input data for ecometab from WtRegDO
# paris mmol/m2 total for 15 minute obs, convert to umol/m2/s
# sal is ppt, should be ppt
# DO is mg/l, should be mg/l
# water temp is C, should be C
# air temp is C, should be C
# BP is mb, should be mb
# WSpd is m/s, should be m/s
# Tide is m, should be m
dat_input <- read.csv(here('data-raw/apadb 01202022.csv')) %>%
  mutate(
    DateTimeStamp = ymd_hms(DateTimeStamp, tz = 'America/Jamaica')
  ) %>%
  select(DateTimeStamp, Temp, Sal, DO_obs, ATemp, BP, WSpd, Tide)

lat <- 29.6747
lng <- -85.0583
res <- ecometab(dat_input, tz = 'America/Jamaica', DO_var = 'DO_obs', metab_units = 'grams', lat = lat, long = lng, instant = T)

toplo <- res %>% 
  mutate( 
    yr = year(Date), 
    NEM = NEM * 1 / 32 * 1000
  ) %>% 
  group_by(yr) %>% 
  filter(yr > 2001 & yr < 2021) %>% 
  summarise(
    NEPave = mean(NEM, na.rm = T),
    NEPhi = t.test(NEM)$conf.int[2], 
    NEPlo = t.test(NEM)$conf.int[1]
  )

ggplot(toplo, aes(x = yr, y = NEPave)) + 
  geom_point() +
  geom_errorbar(aes(ymin = NEPlo, ymax = NEPhi), width = 0.1) +
  theme_minimal() + 
  labs(
    y = 'O2 mmol/m2/d', 
    x = NULL, 
    color = NULL
  )


# 2012 only
toplo <- res %>% 
  mutate( 
    mo = month(Date),
    yr = year(Date), 
    NEM = NEM * 1 / 32 * 1000
  ) %>% 
  filter(yr == 2012) %>% 
  group_by(mo) %>% 
  summarise(
    NEPave = mean(NEM, na.rm = T),
    NEPhi = t.test(NEM)$conf.int[2], 
    NEPlo = t.test(NEM)$conf.int[1]
  )

ggplot(toplo, aes(x = mo, y = NEPave)) + 
  geom_point() +
  geom_errorbar(aes(ymin = NEPlo, ymax = NEPhi), width = 0.1) +
  theme_minimal() + 
  labs(
    y = 'O2 mmol/m2/d', 
    x = NULL, 
    color = NULL
  )
