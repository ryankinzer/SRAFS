# Author: Ryan N. Kinzer
# Purpose: Load and format Sp/sm Chinook data from CAX from CAX and other sources.
# Updated: 3/01/2025

library(tidyverse)

# set species
spp <- 'Chinook salmon' # 'Steelhead'
run <- 'Spring|Summer|Spring/summer'
yr <- 2024

# load TRT pop names
user_path <- Sys.getenv('OneDrive')
proj_path <- '/Projects/DFRM Projects/River_Mapping/data/'
spatial_files <- paste0(user_path, proj_path, 'polygons/SR_pops.rda')
load(spatial_files) ; rm(fall_pop, sth_pop)

trt_pops <- spsm_pop %>% 
  sf::st_set_geometry(NULL)

trt_pops <- trt_pops %>%
  mutate(pop = str_trim(str_remove(POP_NAME, " above.*|below.*"))) %>%  #remove everything after
         #extract = str_trim(str_extract(POP_NAME, ".*(?= above|below)"))) %>% # extract everything before
  mutate(pop = case_when(
    TRT_POPID == 'MFLMA' ~ 'Middle Fork Salmon River Lower Mainstem',
    TRT_POPID == 'MFUMA' ~ 'Middle Fork Salmon River Upper Mainstem',
    TRT_POPID == 'GRLOS' ~ 'Wallowa/Lostine Rivers',
    TRT_POPID == 'SFSMA' ~ 'South Fork Salmon River',
    TRUE ~ pop
  )) %>%
  mutate(pop = str_to_title(pop)) %>%
  select(mpg = MPG, pop, TRT_POPID)


# load raw NOAA data and subset for species
#dat <- read_csv('./data/noaa_cax_data.csv')

# load raw NOSA data from CAX
dat <- readxl::read_excel('./data/QET_data/ca-data-all 03-05-2025 16 24.xls',
                          sheet = 'NOSA')

names(dat) <- tolower(names(dat))

cax_df <- dat %>%
  filter(estimatetype == 'NOSA') %>%
  filter(popfit == 'Same') %>%
  filter(grepl('Snake River', esu_dps)) %>%
  filter(commonname == spp) %>%
  filter(run == run) %>%
  #filter(spawningyear >= 1980) %>%
  mutate(method = ifelse(is.na(metacomments),methodadjustments, metacomments)) %>%
  select(pop = locationname, spawningyear, nosaij, tsaij, nosaej, tsaej) %>%
  mutate(source = '2 - CAX')

# n_ests <- pop_df %>%
#   group_by(pop, spawningyear) %>%
#   count()

# Missing Data - 

idfg <- readxl::read_excel('./data/QET_data/IDFG_2024_ChinookNOSA.xlsx',
                          sheet = 'Sheet1') %>%
  mutate(Population = ifelse(grepl('above Indian', Population), 'Middle Fork Salmon River Upper Mainstem', Population),
         Population = ifelse(grepl('below Indian', Population), 'Middle Fork Salmon River Lower Mainstem', Population),
         Population = ifelse(PopID == 28, 'South Fork Salmon River', Population)) %>%
  mutate(Population = str_trim(str_remove(Population, " above.*|below.*"))) %>% 
  select(pop = Population, spawningyear = SpawnYear, nosaij = NOSAIJ, tsaij = TSAIJ)

wdfw <- readxl::read_excel('./data/QET_data/wdfw_NOSA_2024.xlsx',
                           sheet = 'Sheet1') %>%
  filter(COMMONNAME == spp) %>%
  filter(SPAWNINGYEAR %in% c(2023, 2024)) %>%
  select(pop = LOCATIONNAME, spawningyear = SPAWNINGYEAR, nosaij = NOSAIJ, tsaij = TSAIJ)

odfw <- readxl::read_excel('./data/QET_data/ChS_GRIMN_NOSA_TSA_forRK_20250319.xlsx') %>%  #sheet = 'qry_ChS_GRIMN_NOSA_TSA_forRK_20250319'
  select(pop = CommonPopName, spawningyear = SpawningYear, nosaij = NOSAIJ, tsaij = TSAIJ) %>%
  filter(pop != 'Lostine River spring Chinook') %>%
  mutate(pop = gsub(' spring Chinook| spring/summer Chinook', "", pop),
         pop = ifelse(pop == 'Upper Grande Ronde', 'Grande Ronde River Upper Mainstem', pop),
         pop = ifelse(pop == 'Wallowa-Lostine', 'Wallowa/Lostine Rivers', pop),
         pop = ifelse(pop == 'Imnaha River', 'Imnaha River Mainstem', pop))

#npt
npt <- readxl::read_excel('./data/QET_data/NPT_2024_NOSA.xlsx',
                           sheet = 'Sheet1')
  
names(npt) <- c('spawningyear', 'East Fork South Fork Salmon River', 'Secesh River', 'JC_weir')

npt <- npt %>%
  select(-JC_weir) %>%
  pivot_longer(-spawningyear, names_to = 'pop', values_to = 'nosaij')

new_df <- bind_rows(idfg, wdfw) %>%
  bind_rows(odfw) %>%
  bind_rows(npt) %>%
  mutate(source = '1 - Updated')

pop_df <- bind_rows(cax_df, new_df) %>%
  left_join(trt_pops,
            by = 'pop') %>%
  arrange(pop, spawningyear, source)

ggplot(data = pop_df, aes(x = spawningyear, y = nosaij, colour = source)) +
  geom_line() +
  geom_point() +
  facet_wrap(~pop)

# Create a single time-series using differing methods; prioritizes based on source
pop_df <- pop_df %>%
  arrange(pop, spawningyear, source) %>%
  group_by(pop, spawningyear) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(method = '1')

# NEW METHOD FOR MISSING DATA - USE DABOM ESTIMATES!!!

# combine dabom data

dabom <- readxl::read_excel('C://GitHub/SnakeRiverFishStatus/output/syntheses/LGR_Chinook_all_summaries_2025-01-31.xlsx', sheet = 'Pop_Tot_Esc')
site <- readxl::read_excel('C://GitHub/SnakeRiverFishStatus/output/syntheses/LGR_Chinook_all_summaries_2025-01-31.xlsx',
                           sheet = 'Site_Esc')

unique(dabom$popid)

# pull in populations with direct estimates
pops <- c('SNASO',
          'CRLOL','CRLOC',
          #'SEUMA/SEMEA/SEMOO',
          'SCLAW/SCUMA', 
          'GRCAT', 'GRUMA', 'GRLOS', 'GRWEN', 'GRMIN', 
          'IRBSH', 'IRMAI',
          'SFEFS', 'SFSEC', 'SFSMA',
          'MFBIG', 'MFBEA', 'MFMAR',
          'SRPAN', 'SRNFS', 'SRLEM',
          'SRVAL', 'SRYFS')

direct_ests <- dabom %>%
  filter(popid %in% pops) %>%
  mutate(popid = ifelse(popid == 'SCLAW/SCUMA', 'SCUMA', popid)) %>%
  select(spawn_yr, popid, nosaij = median)

ggplot(data = direct_ests, aes(x = spawn_yr, y = nosaij)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~popid)


#### NOT VALID FOR MODELING ####
# pull in and proportion out aggregated estimates to populations
# need to calculate: GRUMA = UGR - GRCAT, Selway pops = SEUMA/SEMEA/SEMOO * hab_p, SCUMA = SCLAW/SCUMA, Upper Salmon pops = SRLMA/SRPAH/SREFS/SRYFS/SRVAL/SRUMA * hab_p

# pop_exp <- load('C://GitHub/SnakeRiverIPTDS/output/available_habitat/snake_river_iptds_and_pop_available_habitat.rda')
# 
# # get selway populations
# selway_ests <- pop_avail_hab %>% 
#   filter(popid %in% c('SEMEA', 'SEMOO', 'SEUMA')) %>%
#   mutate(pop_sites = 'SW1',
#          method = 'QRF_p',
#          p = qrf_length_m/sum(qrf_length_m)) %>%
#   select(popid, pop_sites, method, p) %>%
#   inner_join(dabom %>%
#                filter(popid == 'SEUMA/SEMEA/SEMOO') %>%
#                select(spawn_yr, pop_sites, median),
#              by = 'pop_sites'
#   ) %>%
#   mutate(nosaij = p * median) %>%
#   filter(spawn_yr != 2022) %>%
#   select(spawn_yr, popid, nosaij)

# split aggregated estimates with upstream population DABOM estimates

# get upper salmon population estimates
# upper_sal <- bind_rows(
#   dabom %>%
#   filter(popid %in% c('SRVAL', 'SRYFS', 'SRLMA/SRPAH/SREFS/SRYFS/SRVAL/SRUMA')) %>%
#   select(spawn_yr, pop_sites, estimate = median),
#   
#   site %>%
#   filter(site %in% c('PAHH', 'SALEFT')) %>%
#   select(spawn_yr, pop_sites = site, estimate = median)
# ) %>%
#   pivot_wider(names_from = 'pop_sites', values_from = 'estimate') %>%
#   mutate(lwr_upr = USE - (VC2 + YFK + SALEFT + PAHH))
# 
# 
# salmon_ests <- bind_rows(
#     pop_avail_hab %>% 
#       filter(popid %in% c('SRLMA', 'SRUMA')) %>%
#       mutate(method = 'QRF_p',
#              p = qrf_length_m/sum(qrf_length_m)) %>%
#       select(popid, method, p) %>%
#       cross_join(upper_sal %>%
#                   select(spawn_yr, lwr_upr)) %>%
#       mutate(est = p * lwr_upr) %>%
#       select(spawn_yr, popid, nosaij = est),
#     
#     upper_sal %>%
#       select(spawn_yr, SALEFT, PAHH) %>%
#       pivot_longer(-spawn_yr, names_to = 'popid', values_to = 'nosaij') %>%
#       mutate(popid = ifelse(popid == 'SALEFT', 'SREFS', 'SRPAH'))
# )

# Get Lookingglass  
looking_ests <- site %>%
  filter(site == 'LGW') %>%
  mutate(popid = 'GRLOO') %>%
  select(spawn_yr, popid, nosaij = median)

# combine

dabom_df <- direct_ests %>% 
  #bind_rows(selway_ests) %>%
  #bind_rows(salmon_ests) %>%
  bind_rows(looking_ests) %>%
  rename(TRT_POPID = popid) %>%
  left_join(trt_pops,
            by = 'TRT_POPID') %>%
    mutate(source = '3 - PIT Array',
           method = '2') %>%
  rename(spawningyear = spawn_yr)

    # filter(!is.na(pop),
    #        nosaij != 0)

# Combine all estimates

full_df <- bind_rows(pop_df, dabom_df)

obj <- ls()
rm(list = obj[!grepl('full_df|yr|spp', obj)])

# n_ests <- full_df %>%
#   group_by(pop, spawningyear) %>%
#   count()

ggplot(data = full_df, aes(x = spawningyear, y = nosaij, colour = method)) +
  geom_line() +
  geom_point() +
  facet_wrap(~TRT_POPID)

# mgt_targets <- readxl::read_excel('./data/mgt_targets.xlsx',
#                                   sheet = 'pop_targets')
# df_sum <- sub_df %>%
#   group_by(pop) %>%
#   summarise(mu = mean(nosaij, na.rm = TRUE)) %>%
#   ungroup() %>%
#   left_join(mgt_targets, by = 'pop') %>%
#   mutate(diff = target - mu)
# 
# df_sum %>%
#   filter(CBP_goals == 'Low') %>%
#   ggplot() +
#   geom_col(aes(x = fct_reorder(pop, diff), y = diff, fill = as.factor(target))) + #diff or mu
#   coord_flip()

source('./R/transform_data.R')

df <- transform_data(full_df %>%
                       filter(spawningyear >= 1980), pop, method)
df %>%
  ggplot(aes(x = spawningyear, y = c)) +
    geom_line(aes(linetype = as.factor(method), group = paste0(pop, method))) +
    geom_smooth(method = 'loess', colour = 'firebrick') + #, span = .75)
    geom_hline(yintercept = 0, colour = 'black') +
    facet_wrap(~TRT_POPID, scales = 'free_y') +
    theme_bw()

df %>%
  ggplot(aes(x = spawningyear, y = c)) +
  geom_line(aes(linetype = as.factor(method), group = paste0(pop, method))) +
  #geom_smooth(method = 'loess', colour = 'firebrick') + #, span = .75)
  geom_hline(yintercept = 0, colour = 'black') +
  #facet_wrap(~TRT_POPID, scales = 'free_y') +
  theme_bw()

saveRDS(full_df, file = paste0('./data/input/',spp,'_data_', yr,'.rds'))
