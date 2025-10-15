# Extra code for summary.....Fit DLM models to spring/summer Chinook data
# Author: Ryan N. Kinzer

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
