# instant ecometab May 2022 at Dry Bar

library(tidyverse)
library(WtRegDO)
library(here)
library(lubridate)

# # input data for ecometab from WtRegDO
# paris mmol/m2 total for 15 minute obs, convert to umol/m2/s
# sal is ppt, should be ppt
# DO is mg/l, should be mg/l
# water temp is C, should be C
# air temp is C, should be C
# BP is mb, should be mb
# WSpd is m/s, should be m/s
# Tide is m, should be m
dat_input <- read.csv(here('data-raw/apadb 010121-060122 WQ MET.csv')) %>%
  mutate(
    DateTimeStamp = ymd_hms(DateTimeStamp, tz = 'America/Jamaica')
  ) %>% 
  select(DateTimeStamp, Temp, Sal, DO_obs, ATemp, BP, WSpd, Tide) %>%  
  filter(minute(DateTimeStamp) == 0) %>% 
  filter(month(DateTimeStamp) == 5) %>% 
  mutate(
    Tide = case_when(
      is.na(Tide) ~ mean(Tide, na.rm = T), 
      T ~ Tide
    )
  ) %>% 
  filter(year(DateTimeStamp) == 2022)

# add tide from 
lat <- 29.6747
lng <- -85.0583

daily <- WtRegDO::ecometab(dat_input, tz = 'America/Jamaica', DO_var = 'DO_obs', metab_units = 'mmol', lat = lat, long = lng, instant = F)
instant <- WtRegDO::ecometab(dat_input, tz = 'America/Jamaica', DO_var = 'DO_obs', metab_units = 'mmol', lat = lat, long = lng, instant = T)

# for inst_bayesmetab7.R
mean(instant$KL)

toplo1 <- daily %>% 
  select(Date, NEP = NEM, ER = Rt, GPP = Pg) %>% 
  mutate(
    Date = ymd(Date, tz = 'America/Jamaica')
  ) %>% 
  pivot_longer(-Date, names_to = 'var', values_to = 'val')

p1 <- ggplot(toplo1, aes(x = Date, y = val, color = var)) + 
  geom_line() + 
  geom_point() +
  theme_minimal() + 
  labs(
    y = 'O2 mmol/m2/d', 
    x = NULL, 
    color = NULL, 
    title = 'Dry Bar May 2022, Odum'
  )

toplo2 <- instant %>% 
  select(Date = DateTimeStamp, GPP = Pg, ER = Rt, NEP = NEM) %>% 
  pivot_longer(-Date, names_to = 'var', values_to = 'val')

p2 <- ggplot(toplo2, aes(x = Date, y = val, color = var)) + 
  geom_line() + 
  geom_point() +
  theme_minimal() + 
  labs(
    y = 'O2 mmol/m2/d', 
    x = NULL, 
    color = NULL
  )

p <- p1 + p2 + plot_layout(ncol = 1, guides = 'collect')

png('figs/apadbMay2022Odum.png', height = 5, width = 7, res = 300, units = 'in')
print(p)
dev.off()

dat <- toplo2 %>% 
  mutate(
    site = 'apadb'
  )

write.csv(dat, 'data/apadbMay2022hourlyOdum.csv', row.names = F)