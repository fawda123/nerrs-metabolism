# instant ecometab 2012 cat point

library(tidyverse)
library(WtRegDO)
library(lubridate)

# # input data for ecometab from WtRegDO
# paris mmol/m2 total for 15 minute obs, convert to umol/m2/s
# sal is ppt, should be ppt
# DO is mg/l, should be mg/l
# water temp is C, should be C
# air temp is C, should be C
# BP is atm, should be mb
# WSpd is m/s, should be m/s
# Tide is m, should be m

# input data for BASEmetab
dat_input <- read.csv('../BASEmetab_script/data/APNERR2012.csv') %>% 
  mutate(
    DateTimeStamp = mdy_hm(DateTimeStamp, tz = 'America/Jamaica')
  ) %>% 
  filter(minute(DateTimeStamp) == 0)

lat <- 29.6747
lng <- -85.0583

daily <- ecometab(dat_input, tz = 'America/Jamaica', DO_var = 'DO_obs', metab_units = 'mmol', lat = lat, long = lng, instant = F)
instant <- ecometab(dat_input, tz = 'America/Jamaica', DO_var = 'DO_obs', metab_units = 'mmol', lat = lat, long = lng, instant = T)


toplo1 <-daily %>% 
  mutate( 
    mo = month(Date)
  ) %>% 
  group_by(mo) %>% 
  summarise(
    NEPave = mean(NEM, na.rm = T),
    NEPhi = t.test(NEM)$conf.int[2], 
    NEPlo = t.test(NEM)$conf.int[1]
  )

p1b <- ggplot(toplo1, aes(x = factor(mo), y = NEPave)) + 
  geom_point() +
  geom_errorbar(aes(ymin = NEPlo, ymax = NEPhi), width = 0.1) +
  theme_minimal() + 
  labs(
    y = 'O2 mmol/m2/d', 
    x = NULL, 
    color = NULL, 
    subtitle = '(b) WtRegDO (ecometab), 2012 Cat Point NEM'
  )


toplo2 <- instant %>% 
  mutate( 
    mo = month(DateTimeStamp)
  ) %>% 
  group_by(mo) %>% 
  summarise(
    NEPave = mean(NEM, na.rm = T),
    NEPhi = t.test(NEM)$conf.int[2], 
    NEPlo = t.test(NEM)$conf.int[1]
  )

p2 <- ggplot(toplo2, aes(x = factor(mo), y = NEPave)) + 
  geom_point() +
  geom_errorbar(aes(ymin = NEPlo, ymax = NEPhi), width = 0.1) +
  theme_minimal() + 
  labs(
    y = 'O2 mmol/m2/d', 
    x = NULL, 
    color = NULL
  )

p1 + p1b + plot_layout(ncol = 1)
