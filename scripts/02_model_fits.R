# Fit DLM models to spring/summer Chinook data
# Author: Ryan N. Kinzer

# load libs
library(tidyverse)
library(MARSS)

source('./R/transform_data.R')
source('./R/Z_create.R')
source('./R/A_create.R')

# set parameters for script
yr <- 2025
#spp <- 'Chinook salmon'
spp <- 'Steelhead'

# select dataset: single time-series for each population or multiple for each
model_run <- 'multiple' #'multiple' #or multiple
timeseries_length <- 15
yr_start <- (yr - timeseries_length) + 1 

# load pre-processed data
df <- readRDS(paste0('./data/input/',yr,'/',spp,'_data_', yr,'.rds'))

# Create model variables, eliminate time-series with less than 5 observations, and truncate to years >= 1980

df <- df %>%
  filter(spawningyear >= yr_start) %>% #yr_start
  group_by(pop, estimatetype) %>%
  mutate(n_obs = n()) %>%
  filter(n_obs >= 5) %>%
  transform_data(pop, estimatetype) %>%
  mutate(method = ifelse(estimatetype == 'NOSA', 1, 2)) %>%
  mutate(pop_series = paste0('Snake Basin', ' - ', mpg, ' - ', pop, ' - ', method))

df %>%
ggplot(aes(x = spawningyear, y = c, linetype = estimatetype)) +
  geom_line(aes(group = pop_series)) +
  geom_hline(yintercept = 0, colour = 'dodgerblue')

# Select a single time-series method....fill in missing years with
df_single <- df %>%
  arrange(pop, spawningyear, desc(estimatetype)) %>%
  group_by(pop, spawningyear) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(method = '1',
         pop_series = paste0('Snake Basin', ' - ', mpg, ' - ', pop, ' - ', method))

if(model_run == 'single'){
dat <- df_single %>%
  arrange(mpg, pop_series)
}

if(model_run == 'multiple'){
  dat <- df %>%
    arrange(mpg, pop_series)
}

# format data frame for model



if(spp == 'Chinook salmon'){
  mod_df <- dat %>%
    filter(spawningyear >= yr_start) %>%  #1980
    select(pop_series, spawningyear, logSA) %>%
    mutate(logSA = as.numeric(logSA)) %>%
    ungroup() %>%
    complete(pop_series, spawningyear, fill = list(logSA = NA)) %>%
    arrange(pop_series, spawningyear) %>%
    pivot_wider(names_from = spawningyear, values_from = logSA) %>%
    inner_join(dat %>% 
                select(mpg, pop_series) %>%
                distinct(), by = 'pop_series') %>%
    arrange(mpg, pop_series) %>%
    select(mpg, pop_series, everything())
} else {
  mod_df <- dat %>%
    filter(spawningyear >= yr_start) %>%  #2010
    select(pop_series, spawningyear, logSA) %>%
    mutate(logSA = as.numeric(logSA)) %>%
    ungroup() %>%
    complete(pop_series, spawningyear, fill = list(logSA = NA)) %>%
    arrange(pop_series, spawningyear) %>%
    pivot_wider(names_from = spawningyear, values_from = logSA) %>%
    inner_join(dat %>% 
                 select(mpg, pop_series) %>%
                 distinct(), by = 'pop_series') %>%
    arrange(mpg, pop_series) %>%
    select(mpg, pop_series, everything())
}

## create observation matrix (y_t)
mod_mat <- as.matrix(mod_df[,-c(1,2)])
t_series <- mod_df$pop_series
n <- dim(mod_mat)[1]
obs_years <- colnames(mod_mat)
t <- dim(mod_mat)[2]
rownames(mod_mat) <- t_series

# original model run
#mod_list=list(Q="equalvarcov", R="diagonal and equal", U="unequal")
#fit <- MARSS(mod_mat, mod_list, control = list(maxit = 1500))

# Define model matrics and parameter list

# pop process
Z_pop <- Z_create(.ts = t_series, .states = unique(dat$pop))
A_pop <- A_create(.ts = t_series, .states = unique(dat$pop))  # incorrect for some....need to fix by hand; Bear Valley Creek, MIddle FOrk Salmon River Lower Mainstem, Middle Fork River Upper Mainstem, EFSFSR, Secesh

if(spp == 'Chinook salmon'){
  A_pop[which(grepl('Bear Valley Creek - 2', row.names(A_pop)))] <- 'aBear Valley Creek2'
  A_pop[which(grepl('East Fork South Fork Salmon River - 2', row.names(A_pop)))] <- 'aEast Fork South Fork Salmon River2'
  A_pop[which(grepl('Secesh River - 2', row.names(A_pop)))] <- 'aSecesh River2'
}

# mpg process
Z_mpg <- Z_create(.ts = t_series, .states = unique(dat$mpg))
A_mpg <- A_create(.ts = t_series, .states = unique(dat$mpg))

# basin process
Z_basin <- Z_create(.ts = t_series, .states = 'Snake Basin')
A_basin <- A_create(.ts = t_series, .states = 'Snake Basin')

# gather model list
Z_list <- list(basin = Z_basin, mpg = Z_mpg, pop = Z_pop)
A_list <- list(basin = A_basin, mpg = A_mpg, pop = A_pop)
Q_list <- c("equalvarcov", "diagonal and equal", "diagonal and unequal") #diagonal and unequal, unconstrained # equal variance from year to year; state process error matrix
R_list <- c("diagonal and equal", "diagonal and unequal") # observation error matrix, unequal
x0_list <- c("unequal") # starting point
U_list <- c("unequal", "equal") # constant drift "unequal", "equal", or set manually

model_grid <- crossing(
  Z  = names(Z_list),
  Q  = Q_list,
  R  = R_list,
  x0 = x0_list,
  U  = U_list
) %>%
  distinct() %>%
  mutate(
    n_states = map_int(Z, ~ ncol(Z_list[[.x]])),
    n_obs    = map_int(Z, ~ nrow(Z_list[[.x]])),
    
    Q = case_when(
      n_states == 1 ~ "equalvarcov",
      TRUE ~ Q
    ),
    
    R = case_when(
      n_obs == 1 ~ "diagonal and equal",
      TRUE ~ R
    ),
    
    U = case_when(
      n_states == 1 ~ "equal",
      TRUE ~ U
    ),
    
    model_id = paste(Z, Q, R, x0, U, sep = "_"),
    model_id = gsub("[^A-Za-z0-9_]+", "_", model_id)
  ) %>%
  distinct(model_id, .keep_all = TRUE) %>%
  mutate(
    output_file = file.path(
      "./data/output",
      as.character(yr),
      paste0(gsub(" ", "_", spp), "_", model_run, "_", model_id, ".rds")
    )
  )

dir.create(file.path("./data/output", as.character(yr)), recursive = TRUE, showWarnings = FALSE)

fit_marss_model <- function(Z, Q, R, x0, U, model_id, output_file, ...) {
  
  if (file.exists(output_file)) {
    message("Skipping completed model: ", model_id)
    return(readRDS(output_file))
  }
  
  message("Starting model: ", model_id)
  message(format(Sys.time(), usetz = TRUE))
  
  mod_list <- list(
    Z  = Z_list[[Z]],
    A  = A_list[[Z]],
    Q  = Q,
    R  = R,
    x0 = x0,
    U  = U
  )
  
  fit <- MARSS(
    mod_mat,
    model = mod_list,
    control = list(maxit = 5000)
  )
  
  saveRDS(fit, output_file)
  
  message("Finished model: ", model_id)
  message(format(Sys.time(), usetz = TRUE))
  
  fit
}

safe_fit_marss_model <- safely(fit_marss_model)

model_results <- pmap(
  model_grid,
  safe_fit_marss_model
)

mod_fit <- map(model_results, "result")
mod_errors <- map(model_results, "error")

source('./R/summarize_ModelFits.R')
mod_tbl <- summarize_ModelFits(mod_fit = mod_fit,
                               model_grid = model_grid)

print(mod_tbl, n = nrow(mod_tbl))

best_fit_index <- mod_tbl$fit_index[which.min(mod_tbl$AICc)]

best_model <- mod_fit[[best_fit_index]]

summary(best_model)

# growth rate :expected abundance each year is: exp(best_model$par$U) of the previous year
1 - exp(best_model$par$U)


save(
  dat,
  mod_mat,
  model_grid,
  mod_fit,
  mod_errors,
  file = file.path(
    "./data/output",
    as.character(yr),
    paste0(gsub(" ", "_", spp), "_model_fits_", yr, ".rda")
  )
)

# # old method
# mod_inputs <- list('dat' = dat,
#                    'mod_mat' = mod_mat,
#                    'A_pop' = A_pop,
#                    'A_mpg' = A_mpg,
#                    'A_basin' = A_basin,
#                    'Z_pop' = Z_pop,
#                    'Z_mpg' = Z_mpg,
#                    'Z_basin' = Z_basin)
# 
# ## fit the model with MARSS  
# mod_fit <- list()
# cnt = 0
# # loop over all combinations
# for (i in seq_along(Z_list)) {  # Z and A must be the same length
#   Z <- Z_list[[i]]
#   A <- A_list[[i]]  # Matching A
#   
#   for (q in seq_along(Q_list)) {
#     for (r in seq_along(R_list)) {
#       for (x in seq_along(x0_list)) {
#         for (u in seq_along(U_list)) {
#           cnt <- cnt + 1
#           
#           Q <- Q_list[q]
#           R <- R_list[r]
#           x0 <- x0_list[x]
#           U <- U_list[u]
#           
#           cat("Starting model run", cnt, "at:", "\n")
#           cat(format(Sys.time(),usetz = TRUE), "\n")
#           
#           # Define model list
#           mod_list <- list(Z = Z, x0 = x0, A = A, U = U, R = R, Q = Q)
#           
#           # Fit MARSS model
#           mod_fit[[cnt]] <- MARSS(mod_mat, mod_list, control = list(maxit = 5000))
#           save(mod_inputs, mod_fit, file = paste0('./data/output/',yr,'/',spp, '_', model_run,'_trends_',yr,'.rda'))
# 
#           cat("Ending at:", format(Sys.time(),usetz = TRUE), "\n")
#         }
#       }
#     }
#   }
# }
# 
# save(mod_inputs, mod_fit, file = paste0('./data/output/',yr,'/', gsub(' ','_', spp), '_model_fits_',yr,'.rda'))

