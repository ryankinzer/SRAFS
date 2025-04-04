# Author: Ryan N. Kinzer
# Purpose: Load and format Steelhead data from CAX and other sources.
# Updated: 3/01/2025

library(tidyverse)

# set species
spp <- 'Steelhead'
run <- 'Summer'
yr <- 2024

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
dat <- readxl::read_excel('./data/QET_data/ca-data-all 03-05-2025 16 24.xls',
                          sheet = 'NOSA')

names(dat) <- tolower(names(dat))

cax_df <- dat %>%
  filter(grepl('Snake River', esu_dps)) %>%
  filter(commonname == spp) %>%
  filter(run == run) %>%
  filter(!grepl('Superpopulation', locationname)) %>%
  filter(locationname != 'Asotin Creek') %>% # Asotin Creek records are for each individual tribs.
  mutate(method = ifelse(is.na(metacomments),methodadjustments, metacomments)) %>%
  filter(!grepl('STADEM', method)) %>% # pull in DABOM ests from new repo
  filter(!grepl('GSI', comments)) %>%
  select(pop = locationname, spawningyear, nosaij, tsaij, nosaej, tsaej, popfit, submitagency, protmethname, method) %>%
  mutate(source = '2 - CAX',
         method = '2') %>%
  left_join(trt_pops,
            by = 'pop') %>%
  select(mpg, pop, TRT_POPID, spawningyear, everything())

# check methods

methods_df <- cax_df %>%
  group_by(pop, popfit, submitagency, protmethname) %>%
  summarize(n = n(),
            min_yr = min(spawningyear),
            max_yr = max(spawningyear))


# n_ests <- cax_df %>%
#   mutate(tmp = paste(submitagency, protmethname)) %>%
#   group_by(pop, spawningyear) %>%
#   count()

# NEW METHOD FOR MISSING DATA - USE DABOM ESTIMATES!!!

# combine dabom data

dabom <- readxl::read_excel(paste0('C://GitHub/SnakeRiverFishStatus/output/syntheses/LGR_',spp,'_all_summaries_2025-01-31.xlsx'), sheet = 'Pop_Tot_Esc')
site <- readxl::read_excel(paste0('C://GitHub/SnakeRiverFishStatus/output/syntheses/LGR_',spp,'_all_summaries_2025-01-31.xlsx'), sheet = 'Site_Esc')
#load('C://GitHub/SnakeRiverIPTDS/output/available_habitat/snake_river_iptds_and_pop_available_habitat.rda')

#unique(dabom$popid)

# pull in populations with direct estimates

direct_ests <- dabom %>%
  filter(!(popid == 'GRLMT-s' & spawn_yr == 2024)) %>%
  filter(!(popid %in% c('CRSFC-s', 'SRLSR-s'))) %>%
  mutate(popid = ifelse(popid == 'CRLMA-s/CRSFC-s','CRSFC-s',popid)) %>%
  filter(!grepl('/', popid)) %>%
  select(spawn_yr, popid, nosaij = median)


sites_ests <- site %>%
  filter(site %in% c('SALEFT', 'PAHH')) %>%
  mutate(popid = case_when(
    site == 'SALEFT' ~ 'SREFS-s',
    site == 'PAHH' ~ 'SRPAH-s',
    site == 'RAPH' ~ 'SRLSR-s')
    )%>%
  select(spawn_yr, popid, nosaij = median)


dabom_df <- direct_ests %>% 
  #bind_rows(selway_ests) %>%
  #bind_rows(salmon_ests) %>%
  bind_rows(sites_ests) %>%
  rename(TRT_POPID = popid) %>%
  left_join(trt_pops,
            by = 'TRT_POPID') %>%
  mutate(source = '3 - PIT Array',
         method = '1') %>%
  select(mpg, pop, TRT_POPID, spawningyear = spawn_yr, everything())


ggplot(data = dabom_df, aes(x = spawningyear, y = nosaij)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~pop)

# combine estimates
full_df <- bind_rows(cax_df, dabom_df)

n_ests <- full_df %>%
  group_by(pop, spawningyear) %>%
  count()

full_df %>%
  #filter(TRT_POPID == 'CRLMA-s') %>%
ggplot(aes(x = spawningyear, y = nosaij, colour = source)) +
  geom_line() +
  geom_point() +
  facet_wrap(~TRT_POPID)


obj <- ls()
rm(list = obj[!grepl('full_df|yr|spp', obj)])

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
