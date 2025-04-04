# Fit DLM models to spring/summer Chinook data
# Author: Ryan N. Kinzer

# load libs
library(tidyverse)
library(MARSS)


# set parameters for script
yr <- 2024
#spp <- 'Chinook salmon'
spp <- 'Steelhead'
model_run <- 'multiple' #multiple' # or single

# load pre-processed data
#df <- readRDS(paste0('./data/input/',spp,'_data_', yr,'.rds'))

# load model fits
load(paste0('./data/output/', spp, '_', model_run,'_trends_',yr,'.rda'))


# summarize model results and select best model
mod_results <- tibble(
  model_id = seq_along(mod_fit),
  logLik = sapply(mod_fit, function(x) logLik(x)),
  AICc = sapply(mod_fit, function(x) x$AICc),
  converged = sapply(mod_fit, function(x) x$convergence),
  n_samps = sapply(mod_fit, function(x) x$samp.size),
  n_params = sapply(mod_fit, function(x) x$num.params),
  U = sapply(mod_fit, function(x) length(x$par$U)),
  Q = sapply(mod_fit, function(x) length(x$par$Q)),
  A = sapply(mod_fit, function(x) length(x$par$A)),
  R = sapply(mod_fit, function(x) length(x$par$R))
) %>%
  arrange(AICc) %>%
  mutate(deltaAIC = AICc - min(AICc))

print(mod_results)
mod_results <- mod_results %>% filter(!(model_id %in% c(3, 4, 5, 6))) # a single process only has a single variance (no model runs for diag and unequal or equalvarcov)
# converged; 0 is good, 1 is not enough, 10 is bad

best_model_id <- mod_results$model_id[which.min(mod_results$AICc)]
best_model <- mod_fit[[best_model_id]]
summary(best_model)

#plot(best_model$logLik, type = "l")

# get CIs and extract pararameter estimates
fitCI <- MARSSparamCIs(best_model)
#fitCI <- MARSSparamCIs(best_model, method = 'parametric', nboot = 10)

# Get model estimates
time_df <- tibble(spawningyear = as.numeric(colnames(mod_inputs$mod_mat)),
                  t = 1:dim(mod_inputs$mod_mat)[2])

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
                   pop_series = rownames(mod_inputs$mod_mat))) %>%
  mutate(.rownames = pop_series) %>%
  select(-pop_series)

ggplot(fitted.ytT, aes(x = spawningyear)) +
  geom_ribbon(aes(ymin = .conf.low, ymax = .conf.up), alpha = .25) +
  geom_line(aes(y = .fitted), linewidth = 1) +
  geom_point(aes(y = y), colour = 'blue') +
  facet_wrap(~.rownames, scales = 'free_y') +
  theme_bw()

# autoplot(fitCI, plot.type = 'fitted.ytT')

best_mod_fits <- mod_inputs$dat %>%
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

save(fitCI, xtT, best_mod_fits, file = paste0('./data/output/',gsub(' ','_',spp), '_best_fit_',yr,'.rda'))

####----------- Extra - not needed ----------------#####

# Get parameters describing the state processes
mod_mat <- mod_inputs$mod_mat

ests <- fitCI$states
colnames(ests) <- colnames(mod_mat)
#rownames(ests) <- colnames(mod_inputs$Z_pop)
ests <- as_tibble(ests, rownames = 'process')
xt <- ests %>%
  pivot_longer(names_to = 'spawningyear', values_to = 'states', -process)

# extract states.se
ests <- fitCI$states.se
colnames(ests) <- colnames(mod_mat)
#rownames(ests) <- colnames(mod_inputs$Z_pop)
ests <- as_tibble(ests, rownames = 'process')
xt_se <- ests %>%
  pivot_longer(names_to = 'spawningyear', values_to = 'states_se', -process)

states_df <- inner_join(xt, xt_se, by = c('process', 'spawningyear')) %>%
  mutate(spawningyear = as.numeric(spawningyear),
         exp_states = exp(states)) %>%
  left_join(tibble(process = row.names(fitCI$par$x0),
                   x0 = as.numeric(fitCI$par$x0), 
                   U = as.numeric(fitCI$par$U))
  ) %>%
  group_by(process) %>%
  mutate(trend = x0 + cumsum(U))

states_df

# Get processes scaled to each time-series of observations

# get observed data and states
Xtt <- fitCI$states
# Extract the estimated A parameters from the MARSS model
A_estimates <- best_model$par$A  # Vector of estimated values

# Convert A_mpg to numeric values
A_mpg <- mod_inputs$A_mpg
A_numeric <- sapply(A_mpg, function(a) {
  if (a == "0") {
    return(0)  # Convert "0" to numeric 0
  } else if (a %in% rownames(A_estimates)) {
    return(A_estimates[a,])  # Replace "aStateX" with its estimated value
  } else {
    stop(paste("Missing parameter estimate for", a))
  }
})

# Convert to matrix format matching Z_mpg rows
A_numeric <- matrix(A_numeric, nrow = length(A_numeric), ncol = ncol(Xtt), byrow = FALSE)
rownames(A_numeric) <- row.names(mod_mat)
colnames(A_numeric) <- colnames(mod_mat)

scaled_xtT <- mod_inputs$Z_mpg %*% Xtt + A_numeric
colnames(scaled_xtT) <- colnames(mod_mat)
ests <- as_tibble(scaled_xtT, rownames = 'pop_series')

scaled_xtT <- ests %>%
  pivot_longer(names_to = 'spawningyear', values_to = 'scaled_xtT', -pop_series) %>%
  mutate(spawningyear = as.numeric(spawningyear),
         exp_scaled_xtT = exp(scaled_xtT),
         mpg = str_split(pop_series, '.-.', simplify = TRUE)[,2],
         pop = str_split(pop_series, '.-.', simplify = TRUE)[,3])

# extract ytT
ests <- fitCI$ytT
colnames(ests) <- colnames(mod_mat)
rownames(ests) <- row.names(mod_mat)
ests <- as_tibble(ests, rownames = 'pop_series')
ytT <- ests %>%
  pivot_longer(names_to = 'spawningyear', values_to = 'ytT', -pop_series)

# extract ytT.se
ests <- fitCI$ytT.se
colnames(ests) <- colnames(mod_mat)
rownames(ests) <- row.names(mod_mat)
ests <- as_tibble(ests, rownames = 'pop_series')
ytT_se <- ests %>%
  pivot_longer(names_to = 'spawningyear', values_to = 'ytT_se', -pop_series)
