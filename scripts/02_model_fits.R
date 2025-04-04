# Fit DLM models to spring/summer Chinook data
# Author: Ryan N. Kinzer

# load libs
library(tidyverse)
library(MARSS)

source('./R/transform_data.R')
source('./R/Z_create.R')
source('./R/A_create.R')

# set parameters for script
yr <- 2024
#spp <- 'Chinook salmon'
spp <- 'Steelhead'

# select dataset: single time-series for each population or multiple for each
model_run <- 'multiple' #'multiple' #or multiple

# load pre-processed data
df <- readRDS(paste0('./data/input/',spp,'_data_', yr,'.rds'))

# Create model variables, eliminate time-series with less than 5 observations, and truncate to years >= 1980

df <- df %>%
  group_by(pop, method) %>%
  mutate(n_obs = n()) %>%
  filter(n_obs >= 5) %>%
  transform_data(pop, method) %>%
  mutate(pop_series = paste0('Snake Basin', ' - ', mpg, ' - ', pop, ' - ', method))

ggplot(data = df, aes(x = spawningyear, y = c, linetype = method)) +
  geom_line(aes(group = pop_series)) +
  geom_hline(yintercept = 0, colour = 'dodgerblue')

# Select a single time-series method....fill in missing years with
df_single <- df %>%
  arrange(pop, spawningyear, desc(method)) %>%
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
    filter(spawningyear >= 1980) %>%
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
    filter(spawningyear >= 2010) %>%
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

if(spp == 'Chinook'){
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
Z_list <- list(Z_basin, Z_mpg, Z_pop)
A_list <- list(A_basin, A_mpg, A_pop)
Q_list <- c("equalvarcov", "diagonal and equal", "diagonal and unequal") #diagonal and unequal, unconstrained # equal variance from year to year; state process error matrix
R_list <- c("diagonal and equal", "diagonal and unequal") # observation error matrix, unequal
x0_list <- c("unequal") # starting point
U_list <- c("unequal") # constant drift "unequal", "equal", or set manually

mod_inputs <- list('dat' = dat,
                   'mod_mat' = mod_mat,
                   'A_pop' = A_pop,
                   'A_mpg' = A_mpg,
                   'A_basin' = A_basin,
                   'Z_pop' = Z_pop,
                   'Z_mpg' = Z_mpg,
                   'Z_basin' = Z_basin)

## fit the model with MARSS  
mod_fit <- list()
cnt = 0
# loop over all combinations
for (i in seq_along(Z_list)) {  # Z and A must be the same length
  Z <- Z_list[[i]]
  A <- A_list[[i]]  # Matching A
  
  for (q in seq_along(Q_list)) {
    for (r in seq_along(R_list)) {
      for (x in seq_along(x0_list)) {
        for (u in seq_along(U_list)) {
          cnt <- cnt + 1
          
          Q <- Q_list[q]
          R <- R_list[r]
          x0 <- x0_list[x]
          U <- U_list[u]
          
          cat("Starting model run", cnt, "at:", "\n")
          cat(format(Sys.time(),usetz = TRUE), "\n")
          
          # Define model list
          mod_list <- list(Z = Z, x0 = x0, A = A, U = U, R = R, Q = Q)
          
          # Fit MARSS model
          mod_fit[[cnt]] <- MARSS(mod_mat, mod_list, control = list(maxit = 5000))
          save(mod_inputs, mod_fit, file = paste0('./data/output/', spp, '_', model_run,'_trends_',yr,'.rda'))

          cat("Ending at:", format(Sys.time(),usetz = TRUE), "\n")
        }
      }
    }
  }
}

save(mod_inputs, mod_fit, file = paste0('./data/output/', gsub(' ','_', spp), '_model_fits_',yr,'.rda'))

