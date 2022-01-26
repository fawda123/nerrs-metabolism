# instant ecometab Aug 2015 and Dec 2017 dry bar

library(tidyverse)
library(readxl)
library(WtRegDO)
library(here)
library(lubridate)
library(patchwork)

# Aug 2015 ----------------------------------------------------------------

# # input data for ecometab from WtRegDO
# par is mmol/m2 total for 15 minute obs, convert to umol/m2/s
# sal is ppt, should be ppt
# DO is mg/l, should be mg/l
# water temp is C, should be C
# air temp is C, should be C
# BP is mb, should be mb
# WSpd is m/s, should be m/s
# Tide is m, should be m
dat_input1 <- read_excel(here('data-raw/Best Aug and Dec data 012421.xlsx'), sheet = 'Aug 2015') %>% 
  mutate(
    DateTimeStamp = force_tz(DateTimeStamp, tz = 'America/Jamaica')
  ) %>% 
  mutate_if(is.character, as.numeric) %>% 
  select(DateTimeStamp, Temp, Sal, DO_obs, ATemp, BP, WSpd, Tide) %>%  
  mutate_if(anyNA, function(x) ifelse(is.na(x), mean(x, na.rm = T), x)) %>%
  filter(minute(DateTimeStamp) == 0) %>% 
  as.data.frame()

lat <- 29.6747
lng <- -85.0583

daily1 <- ecometab(dat_input1, tz = 'America/Jamaica', DO_var = 'DO_obs', metab_units = 'mmol', lat = lat, long = lng, instant = F)
instant1 <- ecometab(dat_input1, tz = 'America/Jamaica', DO_var = 'DO_obs', metab_units = 'mmol', lat = lat, long = lng, instant = T)

# Dec 2017 ----------------------------------------------------------------

# # input data for ecometab from WtRegDO
# par is mmol/m2 total for 15 minute obs, convert to umol/m2/s
# sal is ppt, should be ppt
# DO is mg/l, should be mg/l
# water temp is C, should be C
# air temp is C, should be C
# BP is mb, should be mb
# WSpd is m/s, should be m/s
# Tide is m, should be m
dat_input2 <- read_excel(here('data-raw/Best Aug and Dec data 012421.xlsx'), sheet = 'Dec 2017') %>% 
  mutate(
    DateTimeStamp = force_tz(DateTimeStamp, tz = 'America/Jamaica')
  ) %>% 
  mutate_if(is.character, as.numeric) %>% 
  select(DateTimeStamp, Temp, Sal, DO_obs, ATemp, BP, WSpd, Tide) %>%  
  mutate_if(anyNA, function(x) ifelse(is.na(x), mean(x, na.rm = T), x)) %>%
  filter(minute(DateTimeStamp) == 0) %>% 
  as.data.frame()

lat <- 29.6747
lng <- -85.0583

daily2 <- ecometab(dat_input2, tz = 'America/Jamaica', DO_var = 'DO_obs', metab_units = 'mmol', lat = lat, long = lng, instant = F)
instant2 <- ecometab(dat_input2, tz = 'America/Jamaica', DO_var = 'DO_obs', metab_units = 'mmol', lat = lat, long = lng, instant = T)

# combine metabolism estimates --------------------------------------------

# KL means
mean(instant1$KL)
mean(instant2$KL)

##
# WtRegDO

wrdaily <- bind_rows(daily1, daily2) %>% 
  select(Date, NEM, ER = Rt, GPP = Pg) %>% 
  mutate(
    Date = ymd(Date, tz = 'America/Jamaica'), 
    mo = month(Date, label = T)
  ) %>% 
  filter(mo %in% c('Aug', 'Dec')) %>% 
  pivot_longer(NEM:GPP, names_to = 'var', values_to = 'val')

p1 <- ggplot(wrdaily, aes(x = Date, y = val, color = var)) + 
  geom_line() + 
  geom_point() +
  facet_wrap(~mo, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(
    y = 'Daily, O2 mmol/m2/d', 
    x = NULL, 
    subtitle = '(a) WtRegDO (Odum) estimates',
    color = NULL
  )

wrinstant <- bind_rows(instant1, instant2) %>% 
  select(Date = DateTimeStamp, NEM, ER = Rt, GPP = Pg) %>% 
  mutate(
    Date = ymd_hms(Date, tz = 'America/Jamaica'), 
    mo = month(Date, label = T), 
    estimate = 'WtRegDO'
  ) %>% 
  filter(mo %in% c('Aug', 'Dec')) %>% 
  pivot_longer(NEM:GPP, names_to = 'var', values_to = 'val')

p2 <- ggplot(wrinstant, aes(x = Date, y = val, color = var)) + 
  geom_line() + 
  geom_point() +
  facet_wrap(~mo, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(
    y = 'Instantaneous, O2 mmol/m2/d', 
    x = NULL, 
    color = NULL
  )

p1 + p2 + plot_layout(ncol = 1, guides = 'collect')

##
# BASEmetab

bmdaily1 <- list.files('BASEmetab-res/output5', pattern = '^BASE_results', full.names = T) %>% 
  read.csv
bmdaily2 <- list.files('BASEmetab-res/output6', pattern = '^BASE_results', full.names = T) %>% 
  read.csv

bmdaily <- bind_rows(bmdaily1, bmdaily2) %>% 
  select(Date, GPP = GPP.mean, ER= ER.mean, NEM = NEP.mean) %>% 
  mutate(
    ER = -1 * ER, 
    Date = lubridate::ymd(Date), 
    ER = ER * 1 / 32 * 1000 * 1.5, 
    GPP = GPP * 1 / 32 * 1000 * 1.5, 
    NEM = NEM * 1 / 32 * 1000 * 1.5,
    mo = month(Date, label = T)
  ) %>% 
  pivot_longer(GPP:NEM, names_to = 'var', values_to = 'val')

p1 <- ggplot(bmdaily, aes(x = Date, y = val, color = var)) + 
  geom_line() + 
  geom_point() +
  facet_wrap(~mo, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(
    y = 'Daily, O2 mmol/m2/d', 
    x = NULL, 
    subtitle = '(b) BASEmetab estimates',
    color = NULL
  )

bminstant1 <- list.files('BASEmetab-res/output5', pattern = '^instantaneous', full.names = T) %>% 
  read.csv
bminstant2 <- list.files('BASEmetab-res/output6', pattern = '^instantaneous', full.names = T) %>% 
  read.csv 

bminstant <- bind_rows(bminstant1, bminstant2) %>% 
  rename(GPP = GPP.instant, ER = ER.instant) %>% 
  mutate(
    ER = -1 * ER, 
    hm = interval - 1, 
    NEM = GPP + ER, 
    ER = ER * 24 * 1 / 32 * 1000 * 1.5, 
    GPP = GPP  * 24 * 1 / 32 * 1000 * 1.5, 
    NEM = NEM * 24 * 1 / 32 * 1000 * 1.5
  ) %>% 
  unite('Date', Date, hm, sep = ' ') %>% 
  mutate(
    Date = ymd_h(Date, tz = 'America/Jamaica'), 
    mo = month(Date, label = T), 
    estimate = 'BASEmetab'
  ) %>% 
  select(Date, mo, estimate, GPP, ER, NEM) %>% 
  pivot_longer(GPP:NEM, names_to = 'var', values_to = 'val') 
  
p2 <- ggplot(bminstant, aes(x = Date, y = val, color = var)) + 
  geom_line() + 
  geom_point() +
  facet_wrap(~mo, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(
    y = 'Instantaneous, O2 mmol/m2/d', 
    x = NULL, 
    color = NULL
  )

p1 + p2 + plot_layout(ncol = 1, guides = 'collect')

##
# hourly ensemble

allest <- bind_rows(wrinstant, bminstant) %>% 
  mutate(
    hr = hour(Date),
    ) %>% 
  group_by(hr, mo, estimate, var) %>% 
  summarise(
    val = mean(val, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  mutate(
    val = case_when(
      var == 'ER' ~ -1 * val, 
      T ~ val
    ), 
    val = val / 24, 
    var = factor(var, levels= c('GPP', 'ER','NEM'), labels = c('GPP', 'R', 'NEM'))
  )

write.csv(allest, '~/Desktop/allest.csv', row.names = F)

ggplot(allest, aes(x = hr, y = val, colour = var)) +
  geom_line() + 
  geom_point(size = 3) +
  scale_color_manual(values = c('darkblue', 'darkorange', 'gray')) +
  # scale_x_continuous(breaks = seq(1, 24, by = 1)) +
  geom_hline(yintercept = 0) +
  facet_grid(estimate~mo) +
  theme_bw() + 
  theme(
    strip.background = element_blank(), 
    strip.text = element_text(size = 12), 
    panel.grid.minor = element_blank()
  ) + 
  labs(
    color = NULL,
    x = 'Hour of day', 
    y = expression('mmol '* O[2] * ' ' * m^{-2} * ' ' * hr^{-1})
  )

