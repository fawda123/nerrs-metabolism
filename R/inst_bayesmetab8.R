# BASEmetab May 2022 Pilot's Cove

# Running BASEmetab model -------------------------------------------------

library(BASEmetab)
# devtools::load_all('../BASEmetab')
library(tidyverse)
library(lubridate)
library(patchwork)
library(R2jags)
library(foreach)
library(doParallel)
library(here)

# setup parallel backend
ncores <- detectCores()
cl <- makeCluster(ncores - 2)
registerDoParallel(cl)

data.dir <- here('BASEmetab-res/input8')
results.dir <- here('BASEmetab-res/output8')

# # input data for BASEmetab
# paris mmol/m2 total for 15 minute obs, convert to umol/m2/s
# sal is ppt, should be ppt
# DO is mg/l, should be mg/l
# water temp is C, should be C
# BP is mb, should be atm
# WSpd is m/s, should be m/s
# last line is to fill NA (only 3 values) with last value
#
# note that NA values filled with means
# input data filtered to hours obs
dat_input <- read.csv(here('data-raw/apapc 010121-060122 WQ MET.csv')) %>%
  mutate(
    DateTimeStamp = ymd_hms(DateTimeStamp, tz = 'America/Jamaica'),
    DateTimeStamp = as.character(DateTimeStamp),
    Par = Par * 1000 / (15 * 60), # convert to umol and per second
    BP = BP / 1013 # mb to atm
  ) %>%
  filter(year(DateTimeStamp) == 2022) %>% 
  filter(month(DateTimeStamp) == 5) %>% 
  separate(DateTimeStamp, c('Date', 'Time'), sep = ' ') %>%
  select(Date, Time, I = Par, tempC = Temp, DO.meas = DO_obs, atmo.pressure = BP, salinity = Sal) %>% 
  mutate_if(anyNA, function(x) ifelse(is.na(x), mean(x, na.rm = T), x)) %>% 
  filter(gsub('^\\d\\d\\:|\\:\\d\\d$', '', Time) == "00")

# # four days only
# dat_input <- dat_input[1:96, ] 

write.csv(dat_input, here(paste0(data.dir, '/dat_input.csv')), row.names = F)

#run model,takes a few minutes

# 0.5246162 is average ecometab results at cat point, from inst_ecometab6, mean of KL from instant output
# 2.2 is mean height at pilot's cove
K.meas.mean <- 0.5246162 / 2.2
K.meas.sd <- 1e-9

results <- bayesmetab(data.dir, results.dir, interval = 3600, K.est = F, K.meas.mean = K.meas.mean, 
                      K.meas.sd = K.meas.sd, instant = T, update.chains = T)

# plot results, daily vs instant

daily <- list.files(results.dir, pattern = '^BASE_results', full.names = T) %>% 
  read.csv %>%                   
  select(Date, GPP = GPP.mean, ER= ER.mean, NEP = NEP.mean) %>% 
  mutate(
    ER = -1 * ER, 
    Date = lubridate::ymd(Date), 
    ER = ER * 1 / 32 * 1000 * 2.2, 
    GPP = GPP * 1 / 32 * 1000 * 2.2, 
    NEP = NEP * 1 / 32 * 1000 * 2.2
  ) %>% 
  pivot_longer(-Date, names_to = 'var', values_to = 'val')

p1 <- ggplot(daily, aes(x = Date, y = val, color = var)) + 
  geom_line() + 
  geom_point() +
  theme_minimal() + 
  labs(
    y = 'O2 mmol/m2/d', 
    x = NULL, 
    color = NULL
  )

instant <- list.files(results.dir, pattern = '^instantaneous', full.names = T) %>% 
  read.csv %>% 
  select(Date, GPP = GPP.instant, ER= ER.instant) %>% 
  mutate(
    ER = -1 * ER, 
    hm = rep(0:23, length(unique(Date))), 
    NEP = GPP + ER, 
    ER = ER * 24 * 1 / 32 * 1000 * 2.2, 
    GPP = GPP  * 24 * 1 / 32 * 1000 * 2.2, 
    NEP = NEP * 24 * 1 / 32 * 1000 * 2.2
  ) %>% 
  unite('Date', Date, hm, sep = ' ') %>% 
  mutate(
    Date = ymd_h(Date, tz = 'America/Jamaica')
  ) %>% 
  pivot_longer(-Date, names_to = 'var', values_to = 'val')

p2 <- ggplot(instant, aes(x = Date, y = val, color = var)) + 
  geom_line() + 
  geom_point() +
  theme_minimal() + 
  labs(
    y = 'O2 mmol/m2/d', 
    x = NULL, 
    color = NULL,
    title = "Pilot's Cove May 2022, BASemetab"
  )

p <- p1 + p2 + plot_layout(ncol = 1, guides = 'collect')

png('figs/apapcMay2022BASEmetab.png', height = 5, width = 7, res = 300, units = 'in')
print(p)
dev.off()

dat <- instant %>% 
  mutate(
    site = 'apapc'
  )

write.csv(dat, 'data/apapcMay2022hourlyBASemetab.csv', row.names = F)