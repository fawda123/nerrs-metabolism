# run 2012 apacp data using model estimated k

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

data.dir <- here('BASEmetab-res/input3')
results.dir <- here('BASEmetab-res/output3')

# input data for BASEmetab
load(file = '../BASEmetab_script/data/APNERR2012.RData')

dat_input <- APNERR2012 %>% 
  filter(grepl('00$', Time)) %>% 
  select(-WSpd)
write.csv(dat_input, paste0(data.dir, '/dat_input.csv'), row.names = F)

#run model,takes a few minutes

results <- bayesmetab(data.dir, results.dir, interval = 3600, K.est = T, instant = T, update.chains = T)

# plot results, daily vs instant

daily <- list.files(results.dir, pattern = '^BASE_results', full.names = T) %>% 
  read.csv %>%                   
  select(Date, NEP = NEP.mean) %>% 
  mutate( 
    Date = lubridate::mdy(Date), 
    mo = month(Date), 
    NEP = NEP * 1 / 32 * 1000 * 1.5
  ) %>% 
  group_by(mo) %>% 
  summarise(
    NEPave = mean(NEP, na.rm = T),
    NEPhi = t.test(NEP)$conf.int[2], 
    NEPlo = t.test(NEP)$conf.int[1]
  )

p1 <- ggplot(daily, aes(x = mo, y = NEPave)) + 
  geom_point() +
  geom_errorbar(aes(ymin = NEPlo, ymax = NEPhi), width = 0.1) +
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
    NEP = GPP + ER
  ) %>% 
  unite('Date', Date, hm, sep = ' ') %>% 
  mutate(
    Date = mdy_h(Date, tz = 'America/Jamaica')
  ) %>% 
  select(Date, NEP) %>% 
  mutate(  
    Date = as.Date(Date)
  ) %>% 
  group_by(Date) %>% 
  summarise(
    NEP = mean(24 * NEP, na.rm = T)
  ) %>% 
  mutate(
    mo = month(Date), 
    NEP = NEP * 1 / 32 * 1000 * 1.5
  ) %>%  
  group_by(mo) %>%
  summarise(
    NEPave = mean(NEP, na.rm = T),
    NEPhi = t.test(NEP)$conf.int[2], 
    NEPlo = t.test(NEP)$conf.int[1]
  )

p2 <- ggplot(instant, aes(x = mo, y = NEPave)) + 
  geom_point() +
  geom_errorbar(aes(ymin = NEPlo, ymax = NEPhi), width = 0.1) +
  theme_minimal() + 
  labs(
    y = 'O2 mmol/m2/d', 
    x = NULL, 
    color = NULL
  )

p1 + p2 + plot_layout(ncol = 1, guides = 'collect')
