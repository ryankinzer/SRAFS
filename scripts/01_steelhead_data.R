# Author: Ryan N. Kinzer
# Purpose: Load and format Steelhead data from CAX and other sources.
# Updated: 3/01/2025

library(tidyverse)

# set species
spp <- 'Steelhead'
run <- 'Summer'
yr <- 2025

# load TRT pop names
user_path <- Sys.getenv('OneDrive')
proj_path <- '/Projects/DFRM Projects/River_Mapping/data/'
spatial_files <- paste0(user_path, proj_path, 'polygons/SR_pops.rda')
load(spatial_files) ; rm(spsm_pop, fall_pop)

trt_pops <- sth_pop %>% 
  sf::st_set_geometry(NULL)

trt_pops <- trt_pops %>%
  mutate(POP_NAME = str_trim(str_remove(POP_NAME, " tributaries.*"))) %>%
  mutate(pop = case_when(
    TRT_POPID == 'MFBIG-s' ~ 'Middle Fork Salmon River Lower Mainstem',
    TRT_POPID == 'SRLSR-s' ~ 'Little Salmon River',
    TRUE ~ POP_NAME
  )) %>%
  mutate(pop = str_to_title(pop)) %>%
  select(mpg = MPG, pop, TRT_POPID)

# load raw NOAA data and subset for species
#dat <- read_csv('./data/noaa_cax_data.csv')

# load raw NOSA data from CAX
# dat <- readxl::read_excel('./data/input/ca-data-all 03-05-2025 16 24.xls',
#                           sheet = 'NOSA')
# 
# names(dat) <- tolower(names(dat))

raw_dat <- rCAX::rcax_hli("NOSA", qlist = list(limit = 10000))

dat <- raw_dat %>%
  filter(grepl('Snake River', esapopname)) %>%
  filter(species == spp) %>%
  filter(run == run) %>%
  mutate(
    method = case_when(
      !is.na(protmethname) ~ protmethname,
      !is.na(methodadjustments) ~ methodadjustments,
      !is.na(metacomments) ~ metacomments
    ),
    across(c('nosaij', 'nosaej', 'tsaij', 'tsaej'), as.integer)
  ) %>%
  select(
    species,
    run,
    pop = locationname,
    spawningyear,
    nosaij,
    tsaij,
    nosaej,
    tsaej,
    estimatetype,
    popfit,
    popfitnotes,
    method,
    agency,
    contains('protmeth'),
    methodadjustments,
    metacomments,
    popfit,
    bestvalue
  ) %>%
  left_join(trt_pops,
            by = 'pop') %>%
  select(mpg, pop, TRT_POPID, spawningyear, everything()) %>%
  arrange(pop, spawningyear, estimatetype)

cax_df <- dat %>%
  filter(!(popfit == 'Portion' & agency == 'NPT')) %>%
  filter(bestvalue == 'Yes') %>%
  filter(!grepl('Superpopulation', pop)) 
  # filter(locationname != 'Asotin Creek') %>% # Asotin Creek records are for each individual tribs.
  # mutate(method = ifelse(is.na(metacomments),methodadjustments, metacomments)) %>%
  # filter(!grepl('STADEM', method)) %>% # pull in DABOM ests from new repo
  # filter(!grepl('GSI', comments)) %>%
  # mutate(source = '2 - CAX',
  #        method = '2')


# check methods

methods_df <- cax_df %>%
  group_by(pop, popfit, estimatetype, agency, protmethname) %>%
  summarize(n = n(),
            min_yr = min(spawningyear),
            max_yr = max(spawningyear))


n_ests <- cax_df %>%
  group_by(pop, spawningyear, estimatetype) %>%
  count()

cax_df %>%
  #filter(pop == 'East Fork South Fork Salmon River') %>%
  ggplot(aes(x = spawningyear, y = nosaij, group = estimatetype, colour = estimatetype)) +
  geom_line() +
  geom_point() +
  facet_wrap(~pop)


# n_ests <- cax_df %>%
#   mutate(tmp = paste(submitagency, protmethname)) %>%
#   group_by(pop, spawningyear) %>%
#   count()

# NEW METHOD FOR MISSING DATA - USE DABOM ESTIMATES!!!

# combine dabom data

# dabom <- readxl::read_excel(paste0('C://GitHub/SnakeRiverFishStatus/output/syntheses/deprecated/LGR_',spp,'_all_summaries_2025-01-31.xlsx'), sheet = 'Pop_Tot_Esc')
# site <- readxl::read_excel(paste0('C://GitHub/SnakeRiverFishStatus/output/syntheses/deprecated/LGR_',spp,'_all_summaries_2025-01-31.xlsx'), sheet = 'Site_Esc')
# #load('C://GitHub/SnakeRiverIPTDS/output/available_habitat/snake_river_iptds_and_pop_available_habitat.rda')
# 
# #unique(dabom$popid)
# 
# # pull in populations with direct estimates
# 
# direct_ests <- dabom %>%
#   filter(!(popid == 'GRLMT-s' & spawn_yr == 2024)) %>%
#   filter(!(popid %in% c('CRSFC-s', 'SRLSR-s'))) %>%
#   mutate(popid = ifelse(popid == 'CRLMA-s/CRSFC-s','CRSFC-s',popid)) %>%
#   filter(!grepl('/', popid)) %>%
#   select(spawn_yr, popid, nosaij = median)
# 
# 
# sites_ests <- site %>%
#   filter(site %in% c('SALEFT', 'PAHH')) %>%
#   mutate(popid = case_when(
#     site == 'SALEFT' ~ 'SREFS-s',
#     site == 'PAHH' ~ 'SRPAH-s',
#     site == 'RAPH' ~ 'SRLSR-s')
#     )%>%
#   select(spawn_yr, popid, nosaij = median)
# 
# 
# dabom_df <- direct_ests %>% 
#   #bind_rows(selway_ests) %>%
#   #bind_rows(salmon_ests) %>%
#   bind_rows(sites_ests) %>%
#   rename(TRT_POPID = popid) %>%
#   left_join(trt_pops,
#             by = 'TRT_POPID') %>%
#   mutate(source = '3 - PIT Array',
#          method = '1') %>%
#   select(mpg, pop, TRT_POPID, spawningyear = spawn_yr, everything())
# 
# 
# ggplot(data = dabom_df, aes(x = spawningyear, y = nosaij)) +
#   geom_line() + 
#   geom_point() +
#   facet_wrap(~pop)
# 
# # combine estimates
# full_df <- bind_rows(cax_df, dabom_df)
# 
# n_ests <- full_df %>%
#   group_by(pop, spawningyear) %>%
#   count()
# 
# full_df %>%
#   #filter(TRT_POPID == 'CRLMA-s') %>%
# ggplot(aes(x = spawningyear, y = nosaij, colour = source)) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~TRT_POPID)


obj <- ls()
rm(list = obj[!grepl('cax_df|yr|spp', obj)])

source('./R/transform_data.R')

df <- transform_data(cax_df %>%
                       filter(spawningyear >= 1980), pop, method)
df %>%
  ggplot(aes(x = spawningyear, y = c)) +
  geom_line(aes(linetype = as.factor(estimatetype), group = paste0(pop, estimatetype))) +
  geom_smooth(method = 'loess', colour = 'firebrick') + #, span = .75)
  geom_hline(yintercept = 0, colour = 'black') +
  facet_wrap(~TRT_POPID, scales = 'free_y') +
  theme_bw()

df %>%
  ggplot(aes(x = spawningyear, y = z)) +
  geom_line(aes(linetype = as.factor(estimatetype), group = paste0(pop, estimatetype))) +
  #geom_smooth(method = 'loess', colour = 'firebrick') + #, span = .75)
  geom_hline(yintercept = 0, colour = 'black') +
  #facet_wrap(~TRT_POPID, scales = 'free_y') +
  theme_bw()

saveRDS(cax_df, file = paste0('./data/input/',yr,'/',spp,'_data_', yr,'.rds'))
