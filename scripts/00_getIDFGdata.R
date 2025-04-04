# Purpose: Load and format IDFG historic Granite counts.
# Author: Ryan N. Kinzer
# Date: 7/26/21

# load pkgs
library(tidyverse)

# load data
# IDFG Historic Data----
# Data rec'd from Tim Copeland and Chris Sullivan on 7/15/21 and ~7/13/21: Metadata below 
# Window counts or estimates of adult returns of salmon and steelhead to the uppermost Snake River dam from USACE and FPC
# From Schrader - the steelhead and sp/su Chinook total returns are the window counts and were not expanded; the various splits (origin, ocean age) were estimated…
# Uppermost Dam: Ice Harbor 1962-68, Lower Monumental 1969, Little Goose 1970-74, Lower Granite 1975 - present.
# Totals match final SCOBI output as of 2009 per Camacho and Lawry reports.  NA equals Not Available.
# Schrader made a one-fish adjustment to the sp/su Chinook 2017 hatchery adult:jack split.  The 2017 hatchery chinook total stayed the same.
# As of 7-17-20, Total sp/su Chinook are identical to USACE (FPC) posted window counts for Lower Granite 1975 - present.

# idfg_chn_data <- readxl::read_excel(path = './data/IDFG_historic/MasterSalmonDamCountsForRyanK.xlsx',
#                         skip = 1,
#                         range = 'A2:K61')

# new data from Luciano around 3/17/25??? 
lgd_df <- readxl::read_excel(path = './data/IDFG_historic/LGDAbundance Live Link_3_17_25.xlsx',
                                    sheet = 'Master_Dam_Counts')

names(idfg_chn_data) <- gsub(' |\\r\\n','_', tolower(names(idfg_chn_data)))

idfg_chn_data <- idfg_chn_data %>%
  mutate(across(everything(), .fns=as.character),
         wildspsu_chinook_jacks = if_else(wildspsu_chinook_jacks!='NA', wildspsu_chinook_jacks, NA_character_),
         hatchery_spsu_chinook_jacks = if_else(hatchery_spsu_chinook_jacks!='NA', hatchery_spsu_chinook_jacks, NA_character_),
         across(.cols = c(chinook_run_year, wildspsu_chinook_adults, wildspsu_chinook_jacks, 
                          hatchery_spsu_chinook_adults, hatchery_spsu_chinook_jacks,
                          total_spsu_chinook_adults, total_spsu_chinook, hatchery_minimum_escapement_goal, 
                          wild_goal, delisting_minimum_abundance_threshold), .fns = as.integer)) %>%
  pivot_longer(cols = c(wildspsu_chinook_adults, wildspsu_chinook_jacks, hatchery_spsu_chinook_adults, 
                        hatchery_spsu_chinook_jacks, total_spsu_chinook_adults, total_spsu_chinook), 
               names_to = 'tmp_group', values_to = 'IDFG_total') %>%
  mutate(species = 'Chinook',
         run_cmb = 'Sp/sm',
         group = case_when(
           tmp_group == 'total_spsu_chinook_adults' ~ 'Total_Adults',
           grepl('adult', tmp_group) ~ 'Adult',
           grepl('jack', tmp_group) ~ 'Jack',
           TRUE ~ 'Total'),
         origin = case_when(
           grepl('hatchery', tmp_group) ~ 'Hatchery',
           grepl('wild', tmp_group) ~ 'Wild',
           TRUE ~ NA_character_),
         from_file = 'MasterSalmonDamCountsForRyanK.xlsx',
         TS_comments = 'See comments in R script.',
         dam = case_when(
           count_location == 'Ice Harbor Dam' ~ 'IHR',
           count_location == 'Lower Monumental Dam' ~ 'LMN',
           count_location == 'Little Goose Dam' ~ 'LGS',
           count_location == 'Lower Granite Dam' ~ 'LWG'
         )) %>%
  select(Year=chinook_run_year, dam, run_cmb, species, origin, group, IDFG_total) %>%#, from_file, TS_comments)
  mutate(dataset = 'old')

# load new data from IDFG - rec'd from Bekki on 4/22/2024

new_idfg_chn_data <- readxl::read_excel(path = './data/IDFG_historic/LGD_DBCounts.xlsx',
                                    sheet = 'Chinook')

names(new_idfg_chn_data) <- gsub(' |\\r\\n','_', tolower(names(new_idfg_chn_data)))

new_idfg_chn_data <- new_idfg_chn_data %>%
  select(sp:estimate) %>%
  mutate(size = case_when(
    size == 'Lg' ~ 'Adult',
    size == 'Sm' ~ 'Jack'
  )) %>%
  mutate(origin = case_when(
    origin == 'H' ~ 'Hatchery',
    origin == 'W' ~ 'Wild'
  )) %>% 
  group_by(sp, sy, size, origin) %>%
  summarise(estimate = sum(estimate)) %>%
  ungroup() %>%
  pivot_wider(names_from = c(origin, size), values_from = estimate) %>%
  mutate(Total_Adults = Wild_Adult + Hatchery_Adult,
         Total = Wild_Adult + Hatchery_Adult + Wild_Jack + Hatchery_Jack) %>%
  pivot_longer(cols = Hatchery_Adult:Total, names_to = 'group', values_to = 'IDFG_total') %>%
  separate(col = group, into = c('origin', 'group'), '_') %>%
  mutate(origin = ifelse(origin == 'Total', NA, origin),
         group = ifelse(group == 'Adults', 'Total_Adults', group),
         group = ifelse(is.na(group), 'Total', group)) %>%
  mutate(dam = 'LWG',
         run_cmb = 'Sp/sm') %>%
  select(Year = sy, dam, run_cmb, species = sp, origin, group, IDFG_total) %>%
  mutate(dataset = 'new')

yrs <- unique(new_idfg_chn_data$Year)

idfg_dat <- bind_rows(idfg_chn_data[!(idfg_chn_data$Year %in% yrs),], new_idfg_chn_data)

ggplot(data = idfg_dat, aes(x = Year, y = IDFG_total, colour = dataset)) +
  geom_line() +
  geom_point() +
  facet_grid(origin~group, scales = 'free_y')


idfg_dat <- idfg_dat %>%
  select(-dataset)

save(idfg_dat, file= './data/IDFG_historic/idfg_chn_data.rda')


# get steelhead----
idfg_std_data <- readxl::read_excel(path = './data/IDFG_historic/MasterSteelheadDamCounts4RyanK.xlsx',
                                    #skip = 1,
                                    range = 'A2:F61')

names(idfg_std_data) <- gsub(' |\\r\\n','_', tolower(names(idfg_std_data)))

idfg_std_data <- idfg_std_data %>%
  rename(Year = chinook_run_year, Wild = wild_steelhead_adults, Hatchery = hatchery_steelhead_adults, Total = total_steelhead_adults) %>%
  mutate(dam = case_when(
      count_location == 'Ice Harbor Dam' ~ 'IHR',
      count_location == 'Lower Monumental Dam' ~ 'LMN',
      count_location == 'Little Goose Dam' ~ 'LGS',
      count_location == 'Lower Granite Dam' ~ 'LWG')
      ) %>%
  select(Year, dam, Wild, Hatchery, Total) %>%
  pivot_longer(cols = Wild:Total, names_to = 'origin', values_to = 'IDFG_total')



# load new data from IDFG - rec'd from Bekki on 4/22/2024

new_idfg_std_data <- readxl::read_excel(path = './data/IDFG_historic/LGD_DBCounts.xlsx',
                                        sheet = 'Steelhead')

names(new_idfg_std_data) <- gsub(' |\\r\\n','_', tolower(names(new_idfg_std_data)))

new_idfg_std_data <- new_idfg_std_data %>%
  mutate(dam = 'LWG',
         origin = case_when(
           type == 'H' ~ 'Hatchery',
           type == 'W' ~ 'Wild')
         ) %>%
  select(Year = sy, dam, origin, IDFG_total = estimate) %>%
  group_by(Year, dam, origin) %>%
  summarise(IDFG_total = sum(IDFG_total))

yrs <- unique(new_idfg_std_data$Year)

idfg_std_data <- bind_rows(idfg_std_data[!(idfg_std_data$Year %in% yrs),], new_idfg_std_data) %>%
  mutate(species = 'Steelhead')

save(idfg_std_data, file= './data/IDFG_historic/idfg_std_data.rda')


# save data in spreadsheet.

chnk <- idfg_dat
sthd <- idfg_std_data

chnk_wide <- chnk %>%
  filter(!is.na(origin)) %>%
  mutate(group = paste0(origin, "_", group)) %>%
  select(-origin) %>%
  pivot_wider(names_from = group, values_from = IDFG_total) %>%
  mutate(Total_Adult = rowSums(across(c(Wild_Adult, Hatchery_Adult)), na.rm = TRUE),
         Total = rowSums(across(c(Wild_Adult, Wild_Jack, Hatchery_Adult, Hatchery_Jack)), na.rm = TRUE))


sthd_wide <- sthd %>%
  pivot_wider(names_from = origin, values_from = IDFG_total)

list('Chinook' = chnk_wide,
     'Steelhead' = sthd_wide) %>%
  writexl::write_xlsx(x = .,
                      path = paste0('./data/IDFG_historic/lgr_returns_',yr,'.xlsx'))
