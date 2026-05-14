# Fit DLM models to spring/summer Chinook data
# Author: Ryan N. Kinzer

# load libs
library(tidyverse)
library(MARSS)
source('./R/summarize_ModelFits.R')

# set parameters for script
yr <- 2025
spp <- 'Chinook salmon'
#spp <- 'Steelhead'
model_run <- 'multiple' #multiple' # or single

# load pre-processed data
#df <- readRDS(paste0('./data/input/',spp,'_data_', yr,'.rds'))

# load model fits
load(paste0('./data/output/',yr,'/',gsub(' ', '_', spp,),'_model_fits_',yr,'.rda'))

# summarize model results and select best model
mod_tbl <- summarize_ModelFits(mod_fit = mod_fit, model_grid)
best_fit_index <- mod_tbl$fit_index[which.min(mod_tbl$AICc)]
best_model <- mod_fit[[best_fit_index]]
summary(best_model)
diag(best_model$par$Q)
diag(best_model$par$R)
best_model$par$Q
best_model$par$R
#1 - exp(best_model$par$U)
#plot(best_model$logLik, type = "l")

boot <- MARSSboot(best_model, nboot = 10) #500
u_boot <- boot$boot.params["U.1",]
decline_boot <- 1 - exp(u_boot)
quantile(decline_boot, c(0.025, 0.5, 0.975))

# get CIs and extract pararameter estimates
fitCI <- MARSSparamCIs(best_model)

# Get model estimates
time_df <- tibble(spawningyear = as.numeric(colnames(mod_mat)),
                  t = 1:dim(mod_mat)[2])

xtT <- tsSmooth(fitCI, type = "xtT", interval = "confidence") %>%
  left_join(time_df)

ggplot(xtT, aes(x = spawningyear)) +
  geom_ribbon(aes(ymin = .conf.low, ymax = .conf.up), alpha = .25) +
  geom_line(aes(y = .estimate), linewidth = 1) +
  #geom_line(aes(y = .x), colour = 'firebrick', linewidth = 1) +
  scale_colour_brewer(palette = 'Dark2') +
  facet_wrap(~.rownames, scales = 'free_y') +
  theme_bw()

# autoplot(fitCI, plot.type = 'xtT')

ytT <- tsSmooth(fitCI, type = 'ytT', interval = 'confidence')

fitted.ytT <- fitted(best_model, type = 'ytT', interval = "confidence") %>%
  left_join(time_df) %>%
  left_join(tibble(.rownames = rownames(best_model$model$data),
                   pop_series = rownames(mod_mat))) %>%
  mutate(.rownames = pop_series) %>%
  select(-pop_series)

ggplot(fitted.ytT, aes(x = spawningyear)) +
  geom_ribbon(aes(ymin = .conf.low, ymax = .conf.up), alpha = .25) +
  geom_line(aes(y = .fitted), linewidth = 1) +
  geom_point(aes(y = y), colour = 'blue') +
  facet_wrap(~.rownames, scales = 'free_y') +
  theme_bw()

# autoplot(fitCI, plot.type = 'fitted.ytT')

best_mod_fits <- dat %>% #mod_inputs$dat %>%
  full_join(fitted.ytT, by = c('pop_series' = '.rownames', 'spawningyear')) %>%
  mutate(mpg = str_split(pop_series, ' - ', simplify = TRUE)[,2],
         pop = str_split(pop_series, ' - ', simplify = TRUE)[,3],
         method = str_split(pop_series, ' - ', simplify = TRUE)[,4],
         resids = logSA - .fitted)

best_mod_fits %>%
  #filter(mpg == 'South Fork Salmon River') %>%
  #filter(spawningyear >= 2020) %>%
  ggplot(aes(x = spawningyear, group = pop_series)) +
  geom_ribbon(aes(ymin = .conf.low, ymax = .conf.up), alpha = .25) +
  geom_point(aes(y = logSA, colour = method)) +
  geom_line(aes(y = .fitted, colour = method)) +
  scale_colour_brewer(palette = 'Dark2') +
  facet_wrap(~ pop, scales = 'free_y') +
  theme_bw()

best_mod_fits %>%
  #filter(mpg == 'South Fork Salmon River') %>%
  ggplot(aes(x = logSA, y = .fitted, colour = spawningyear)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  #facet_wrap(~ pop) +
  labs(x = 'Observed',
       y = 'Predicted') +
  theme_bw() +
  theme(legend.position = 'none')

best_mod_fits %>%
  filter(spawningyear >= 1980) %>%
  #filter(mpg == 'Grande Ronde / Imnaha') %>%
  ggplot(aes(x = spawningyear, y = resids)) +
  geom_abline(intercept = 0, slope = 0) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ pop_series) +
  labs(x = 'Spawn Year',
       y = 'Observation Residuals (Y_t - Z x̂_t + A)') +
  theme_bw() +
  theme(legend.position = 'none')

hist(best_mod_fits$resids)
qqnorm(best_mod_fits$resids)
abline(a = 0, b = 1)


resids <- residuals(best_model)
autoplot(resids, plot.type = 'all')


#fitCI <- MARSSparamCIs(best_model, method = 'parametric', nboot = 1000, silent = FALSE)

save(fitCI, xtT, best_mod_fits, file = paste0('./data/output/',yr, '/', gsub(' ','_',spp), '_best_fit_',yr,'.rda'))

best_mod_fits %>%
  filter(spawningyear >= 2010) %>%
  mutate(species = spp,
         abundance = round(nosaij),
         modeled = round(exp(.fitted))) %>%
  select(species, mpg, pop, method, spawningyear, abundance, modeled) %>%
  write_csv(file = paste0('./data/output/',yr,'/',gsub(' ','_',spp), '_best_fit_',yr,'.csv'))
