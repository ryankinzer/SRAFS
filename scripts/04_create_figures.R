# Purpose: Plot spring/summer Chinook salmon data and model summaries
# Ryan N. Kinzer

library(tidyverse)
library(patchwork)
source('./R/theme_rk.R')

# set meta data for plots
version <- '24'
yr <- 2024

spp <- 'Chinook salmon'
run <- 'Spring-summer'

#spp <- 'Steelhead'
#run <- 'Summer'

fig_path <- here::here('figures',gsub(' ','_',spp))
# set figure dimensions
h <- 10
w <- 16
dp <- 600

#plot_title <- paste(run,spp) #'Spring-summer Chinook Salmon'
#model_run <- 'multiple' # or single
#data_source <- paste0('Source: StreamNet Coordinated Assessments (3/5/2025); missing observations obtained from personnal communication \n with co-managers or derived by the Nez Perce Tribe using PIT-tag observations.')


# Lower Granite Plots----

mgt_targets <- readxl::read_excel('./data/input/mgt_targets.xlsx',
                                  sheet = 'targets') %>%
  mutate(target = as.numeric(target))

origin_size <- readxl::read_excel(path = './data/input/LGDAbundance Live Link_3_17_25.xlsx',
                                  sheet = 'Origin_Size') %>%
  filter(SpeciesName == strsplit(spp, ' ')[[1]][1]) %>%
  group_by(SpawnYear, RearName, Size) %>%
  summarise(est = sum(Estimate)) %>%
  select(spawnyear = SpawnYear, origin = RearName, size = Size, est)

master_dam <- readxl::read_excel(path = './data/input/LGDAbundance Live Link_3_17_25.xlsx',
                                 sheet = 'Master_Dam_Counts')

if(spp == 'Chinook salmon'){
  master_dam <- master_dam %>%
  select(spawnyear = ChinookRunYear, Wild_Large = WildSpSuChinookAdults, Wild_Small = WildSpSuChinookJacks, Hatchery_Large = HatcherySpSuChinookAdults, Hatchery_Small = HatcherySpSuChinookJacks)
} else {
  master_dam <- master_dam %>%
    select(spawnyear = ChinookRunYear, Wild_Large = WildSteelheadAdults, Hatchery_Large = HatcherySteelheadAdults)
}
  
master_dam <- master_dam %>%
  pivot_longer(-spawnyear, names_to = 'grp', values_to = 'est') %>%
  separate(grp, into = c('origin', 'size'), sep = '_')

yrs <- unique(origin_size$spawnyear)

idfg_dat <- bind_rows(master_dam[!(master_dam$spawnyear %in% yrs),], origin_size) %>%
  mutate(size = case_when(
    size == 'Large' ~ 'Adult',
    size == 'Small' ~ 'Jack',
    TRUE ~ 'Adult')
  )

if(spp == 'Chinook salmon'){
  idfg_dat <- idfg_dat %>%
    filter(size == 'Adult')
} else {
  idfg_dat <- idfg_dat %>%
    group_by(spawnyear, origin) %>%
    summarise(est = sum(est, na.rm = TRUE))
}

# total at LGR

lgr_mgt_total <- mgt_targets %>%
  filter(species == spp,
         grepl('Spring/summer|Summer', run)) %>%
  filter(origin == 'Total') %>%
  mutate(grp = 'Total (Wild + Hatchery)')

t2 <- idfg_dat %>%
  mutate(grp = 'Total (Wild + Hatchery)') %>%
  ggplot(aes(x = spawnyear)) +
  geom_col(aes(y = est, fill = origin), colour = 'black') +
  scale_fill_manual(values = c('grey60', 'grey80'), breaks = c('Wild', 'Hatchery'), labels = c('Wild', 'Hatchery')) +
  geom_hline(data = lgr_mgt_total, 
             aes(yintercept = target, colour = goal_label), size = 1) +
  geom_label(data = lgr_mgt_total, 
             aes(x = 1970, y = target, label = goal_label, colour = goal_label)) +
  scale_color_manual(values = c('darkgreen'), guide = 'none') +
  scale_x_continuous(breaks = seq(1960, 2020, 5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 350000),
                     label = scales::comma,
                     breaks = c(lgr_mgt_total$target, seq(25000,325000, by = 100000))#,
                     # sec.axis = sec_axis(~./scaleFactor, name = 'Percent Hatchery',
                     #                     labels = function(b){paste0(round(b * 100,0),"%")},
                     #                     breaks = seq(0,1, by = .1))
  ) +
  facet_wrap(~grp) +
  labs(
    #title = 'Spring/Summer Chinook Salmon',
    #subtitle = 'Adult escapement at Lower Granite Dam',
    # caption = 'Data Source: Idaho Department of Fish and Game; Lawry et al. 2020',
    x = 'Spawn Year',
    y = 'Escapement',
    fill = 'Origin') +
  theme_rk() +
  theme(legend.position = c(0.10, 0.4), #.1, .25
        legend.background = element_rect(fill = "white", color = "black"))


# hatchery fraction
phos_line <- idfg_dat %>%
  group_by(spawnyear) %>%
  mutate(total = sum(est),
         p = est/total) %>%
  filter(origin == 'Wild') %>%
  ggplot(aes(x = spawnyear, y = p)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0,1), expand = c(0,0), label = scales::percent) +
  labs(subtitle = 'Percent Wild',
       x = '',
       y = '') +
  theme_rk() +
  theme(plot.subtitle = element_text(size = 10, face = 'plain', vjust = 2, hjust = 0),
        axis.title=element_text(size=8),
        axis.text = element_text(size=8),
        plot.margin = unit(c(0, 0, 0, 0), "points"))

t_phos <- t2 + inset_element(phos_line, left = .7, bottom = .36, right = .95, top = .9)

lgr_mgt_total <- mgt_targets %>%
  filter(species == spp,
         grepl('Spring/summer|Summer', run)) %>%
  filter(origin == 'Hatchery')

hat_fig <- idfg_dat %>%
  filter(origin == 'Hatchery') %>%
  ggplot(aes(x = spawnyear, y =est)) +
  geom_col(fill = 'grey80', colour = 'black') +
  geom_hline(data = lgr_mgt_total, 
             aes(yintercept = target, colour = goal_label), size = 1) +
  geom_label(data = lgr_mgt_total, 
             aes(x = c(1980,2010), y = target, label = goal_label, colour = goal_label, vjust = c(.5,0))) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 260000), label = scales::comma, breaks = lgr_mgt_total$target) + #c(235000, 90000,11638)) +
  scale_color_manual(values = c('firebrick', 'darkgreen')) +
  #scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  facet_wrap(~origin, ncol = 2) +
  labs(#title = 'Natural-origin Spring/Summer Chinook Salmon',
    #subtitle = 'Adult escapement at Lower Granite Dam',
    caption = 'Data Source: Idaho Department of Fish and Game',
    x = 'Spawn Year',
    y = 'Escapement') +
  theme_rk() +
  theme(legend.position = 'none')


lgr_mgt_total <- mgt_targets %>%
  mutate(origin = ifelse(origin == 'Wild/natural', 'Wild', origin)) %>%
  filter(species == spp,
         grepl('Spring/summer|Summer', run),
         origin == 'Wild') %>%
  filter(CBP_goals != 'Medium')
  

nat_fig <- idfg_dat %>%
  filter(origin == 'Wild') %>%
  #  filter(Year >= 1975) %>%
  ggplot(aes(x = spawnyear, y = est)) +
  geom_col(fill = 'grey60', colour = 'black') +
  #scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  geom_hline(data = lgr_mgt_total, 
             aes(yintercept = target, colour = goal_label), size = 1) +
  geom_label(data = lgr_mgt_total, 
             aes(x = 1980, y = target, label = goal_label, colour = goal_label), vjust = c(0,.5, 0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 260000), label = scales::comma, breaks = lgr_mgt_total$target) + #c(235000,43000,1850)
  scale_color_manual(values = c('firebrick', 'navy', 'darkgreen')) +  
  facet_wrap(~origin, ncol = 2) +
  labs(#title = 'Natural-origin Spring/Summer Chinook Salmon',
    #subtitle = 'Adult escapement at Lower Granite Dam',
    x = 'Spawn Year',
    y = 'Escapement'
  ) +
  theme_rk() +
  theme(legend.position = 'none')


lgr_plots <- t_phos / (nat_fig | hat_fig)

lgr_plots
ggsave(paste0(fig_path,'/',gsub(' ','_',spp) ,'_lgr_',yr,'.png'), width = w, height = h, dpi = dp)

# load QET data----
load(paste0('./data/output/',gsub(' ', '_', spp), '_best_fit_',yr,'.rda'))

# mpg_dat <- best_mod_fits %>%
#   select(mpg, pop, TRT_POPID) %>%
#   distinct()

# # organize pops for plotting
pop_names <- best_mod_fits %>%
  distinct(mpg, pop) %>%
  filter(!is.na(mpg)) %>%
  mutate(
    mpg = case_when(
      spp == "Chinook salmon" ~ factor(mpg, levels = c(
        "Lower Snake", "Dry Clearwater", "Wet Clearwater",
        "Grande Ronde / Imnaha", "South Fork Salmon River",
        "Middle Fork Salmon River", "Upper Salmon River"
      )),
      TRUE ~ factor(mpg, levels = c(
        "Lower Snake", "Clearwater River",
        "Grande Ronde River", "Imnaha River", "Salmon River"
      ))
    )
  ) %>%
  arrange(mpg, pop)

pop_levels <- pop_names %>%
  pull(pop)

#pop_names <- c("Tucannon River", "Asotin Creek", "Lapwai/Big Canyon", "Lolo Creek", "Upper South Fork Clearwater", "Lochsa River", "Wenaha River", "Minam River", "Wallowa/Lostine Rivers", "Lookingglass Creek", "Catherine Creek", "Grande Ronde River Upper Mainstem", "Imnaha River Mainstem", "Big Sheep Creek", "Little Salmon River", "Secesh River", "East Fork South Fork Salmon River", "South Fork Salmon River", "Big Creek", "Camas Creek", "Loon Creek", "Middle Fork Salmon River Lower Mainstem", "Middle Fork Salmon River Upper Mainstem", "Sulphur Creek", "Marsh Creek", "Bear Valley Creek", "Chamberlain Creek", "Panther Creek", "North Fork Salmon River", "Lemhi River", "Pahsimeroi River", "East Fork Salmon River", "Salmon River Lower Mainstem", "Salmon River Upper Mainstem","Yankee Fork", "Valley Creek")              

best_mod_fits$pop <- factor(best_mod_fits$pop, levels = pop_levels)

best_mod_fits <- best_mod_fits %>%
  mutate(method = case_when(
    spp == 'Chinook salmon' & method == '1' ~ 'Spawning Ground & Weir Observations',
    spp == 'Steelhead' & method == '2' ~ 'Spawning Ground, Weir, and Genetic Stock Methods',
    TRUE ~ 'PIT-tag Observations'
  ))


best_mod_fits %>%
  ggplot(aes(x = spawningyear, group = pop_series)) +
  geom_line(aes(y = c), colour = 'grey50') +
  #geom_smooth(aes(y = center), group = 1, colour = 'blue', span = .5, se = FALSE, alpha =.25) +
  geom_hline(yintercept = 0, linewidth = 1, colour = 'black', linetype = 2) +
  #geom_vline(xintercept = 1979, linewidth = 1, colour = 'blue', linetype = 2) +
  #annotate(geom = 'text', x = 1962, y = 5500, label = 'Dam Construction Phase', size = 8) +
  #annotate(geom = 'text', x = 2005, y = 5500, label = 'Post-Dam Construction', size = 8) +
  scale_y_continuous(limits = c(-2000,6000)) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  #scale_x_continuous(limits = c(1980,2020)) +
  #facet_wrap(~pop, scales = 'free') +
  theme_rk() +
  labs(
    #title = plot_title,
    #subtitle = 'Centered empirical natural-origin spawner abundance (NOSAij) is shown for Snake River Basin populations with grey lines.',
    #'The solid blue line \n shows a common across population trend.',
    #caption = data_source,    
    x = 'Spawn Year',
    y = expression(NOSAij - mean~(NOSAij))
    )

ggsave(paste0(fig_path,'/',gsub(' ','_',spp) ,'_centered_',yr,'.png'), width = w, height = h, dpi = dp)

# states
drift_df <- tibble(.rownames = rownames(fitCI$par$U),
         x0 = fitCI$par$x0[,],
         u = fitCI$par$U[,1]) %>%
  mutate(drift = exp(u),
         growth = ifelse(drift>=1, (drift-1)*100, (1-drift)*-100))

pop_cnt <- pop_names %>%
  group_by(mpg) %>%
  count() %>%
  mutate(strip_labels = paste0(mpg, ' (',n,' populations)'))


if(spp == 'Steelhead'){
  drift_df$.rownames = 'Snake Basin'
  
  pop_cnt <- pop_names %>%
    mutate(mpg = 'Snake Basin') %>%
    group_by(mpg) %>%
    count() %>%
    mutate(strip_labels = paste0('Snake Basin', ' (',n,' populations)'))
  
}


drift_df <- left_join(drift_df, pop_cnt, by = c('.rownames' = 'mpg'))

xtT %>%
  left_join(pop_cnt, by = c('.rownames' = 'mpg')) %>%
 ggplot(aes(x = t)) +
  geom_ribbon(aes(ymin = .conf.low, ymax = .conf.up), alpha = .25) +
  geom_line(aes(y = .estimate), linewidth = 1) +
  geom_abline(data = drift_df, aes(intercept = x0, slope = u), size = 1, linetype = 1, colour = 'blue') +
  geom_text(data = drift_df, aes(x = 0, y = -Inf, label = paste0("x0 = ", round(x0,2))),hjust = 0, vjust = -1.5) +
  geom_text(data = drift_df, aes(x = 0, y = -Inf, label = paste0("u = ", round(u,2))), hjust = 0, vjust = -.5) +
  scale_colour_brewer(palette = 'Dark2') +
  #scale_x_continuous(breaks = scales::pretty_breaks()) +
  facet_wrap(~strip_labels) +
  theme_rk() +
  labs(
    #title = plot_title,
    #subtitle = 'Estimated state processes from the best fitting model to Snake River Basin population abundance observatons.',
    x = 'Time',
    y = 'LN(Abundance)')

ggsave(paste0(fig_path,'/',gsub(' ','_',spp) ,'_states_',yr,'.png'), width = w, height = h, dpi = dp)

# plot spawner abundance

best_mod_fits %>%
  filter(.,
         if(spp == 'Chinook salmon'){
           spawningyear >= 1980
         } else {
           spawningyear >= 2010
         }
  ) %>%
  #filter(!is.na(TRT_POPID)) %>%
  #filter(pop == 'South Fork Salmon River') %>%
  ggplot(aes(x = spawningyear, group = pop_series, colour = method)) +
  #geom_ribbon(aes(ymin = .conf.low, ymax = .conf.up), alpha = .25) +
  geom_point(aes(y = nosaij, fill = method), size = 2, shape = 21, colour = 'black') +
  geom_line(aes(y = exp(.fitted)), linewidth = 1) + #exp(.fitted))
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  facet_wrap(~pop, scales = 'free_y', ncol = 4) +
  theme_rk() +
  labs(
    #title = plot_title,
    #subtitle = 'Empirical natural-origin abundance estimates for Snake River Basin populations (points) and modeled population trends (line).',
    #caption = data_source,
    colour = '',
    fill = '',
    x = 'Spawn Year',
    y = 'Abundance'
)

ggsave(paste0(fig_path,'/',gsub(' ','_',spp) ,'_observations_',yr,'.png'), width = w, height = h, dpi = dp)


mpg_sa <- best_mod_fits %>%
  filter(.,
         if(spp == 'Chinook salmon'){
           spawningyear >= 1980
         } else {
           spawningyear >= 2010
         }
  ) %>%
  group_by(pop) %>%
  filter(spawningyear >= min(spawningyear[!is.na(logSA)], na.rm = TRUE)) %>%
  mutate(mpg = gsub('/','-',mpg)) %>%
  group_by(mpg) %>%
  nest() %>%
  mutate(fig = purrr::map(data,
                          ~ggplot(data = ., aes(x = spawningyear, group = pop_series, colour = method)) +
                            geom_point(aes(y = nosaij, fill = method), size = 2, shape = 21, colour = 'black') +
                            geom_line(aes(y = exp(.fitted)), linewidth = 1) +
                            geom_hline(yintercept = 50, linetype = 2) +
                            scale_x_continuous(breaks = scales::pretty_breaks()) +
                            facet_wrap(~pop, scales = 'free_y') +
                            theme_rk() +
                            labs(
                              #title = plot_title,
                              #subtitle = 'Empirical natural-origin abundance estimates for Snake River Basin populations (points) and modeled population trends (line).',
                              #colour = '',
                              #fill = '',
                              #caption = data_source,
                              x = 'Spawn Year',
                              y = 'Abundance'
                              )
                          )
         )


map2(mpg_sa$fig, mpg_sa$mpg,
     ~ggsave(paste0(fig_path,'/',gsub(' ','_',spp) ,'MPG_', gsub(' ','_',.y), '_observations_',yr,'.png'), width = w, height = h, dpi = dp)
)


best_mod_fits %>%
  filter(.,
         if(spp == 'Chinook salmon'){
            spawningyear >= 1980
           } else {
             spawningyear >= 2010
             }
         ) %>%
  ggplot(aes(x = spawningyear, group = pop_series, colour = method)) +
  geom_point(aes(y = resids, fill = method), size = 2, shape = 21, colour = 'black') +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -2, linetype = 2) +
  geom_hline(yintercept = 2, linetype = 2) +
  facet_wrap(~pop, ncol = 4) +
  theme_rk() +
  labs(
    #title = plot_title,
    #subtitle = 'Residuals of empirical natural-origin abundance estimates minus modeled expected values (ytT).',
    colour = '',
    fill = '',
    x = 'Spawn Year',
    y = 'Residuals'
)

ggsave(paste0(fig_path,'/',gsub(' ','_',spp) ,'_residuals_',yr,'.png'), width = w, height = h, dpi = dp)


#  Population targets

mod_dat <- best_mod_fits %>%
  arrange(pop, spawningyear, source) %>%
  group_by(pop, spawningyear) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(exp_fit = exp(.fitted))

if(spp == 'Chinook salmon'){
  target_dat <- mod_dat %>%  
    filter(spawningyear >= 1980)
  
  mgt_targets <- readxl::read_excel('./data/input/mgt_targets.xlsx',
                                    sheet = 'pop_targets') %>%
    filter(grepl('Spring/summer', run)) %>%
    filter(pop %in% unique(target_dat$pop)) %>%
    filter(CBP_goals != 'Medium') %>%
    mutate(CBP_goals = case_when(
      CBP_goals == 'High' ~ 'Healthy and Harvestable',
      CBP_goals == 'Low' ~ 'Minimum Viable Abundance',
      CBP_goals == 'Critical' ~ 'Quasi-Extinction (50 spawners)')) %>%
    mutate(CBP_goals = factor(CBP_goals, levels = c('Healthy and Harvestable', 'Minimum Viable Abundance', 'Quasi-Extinction (50 spawners)')))
} else {
  target_dat <- mod_dat %>%  
    filter(spawningyear >= 2010)
  
  mgt_targets <- readxl::read_excel('./data/input/mgt_targets.xlsx',
                                    sheet = 'pop_targets') %>%
    filter(grepl('Summer', run)) %>%
    filter(pop %in% unique(target_dat$pop)) %>%
    filter(CBP_goals != 'Medium') %>%
    mutate(CBP_goals = case_when(
      CBP_goals == 'High' ~ 'Healthy and Harvestable',
      CBP_goals == 'Low' ~ 'Minimum Viable Abundance',
      CBP_goals == 'Critical' ~ 'Quasi-Extinction (50 spawners)')) %>%
    mutate(CBP_goals = factor(CBP_goals, levels = c('Healthy and Harvestable', 'Minimum Viable Abundance', 'Quasi-Extinction (50 spawners)')))
}
  

ggplot() +
  geom_line(data = target_dat, aes(x = spawningyear, y = exp_fit)) +
  geom_point(data = target_dat, aes(x = spawningyear, y = exp_fit), fill = 'red', size = 3, colour = 'black') +
  geom_hline(data = mgt_targets,
             aes(yintercept = target, colour = CBP_goals),
             size = 1) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c('darkgreen', 'navy', 'firebrick')) +
  #guides(colour = guide_legend(ncol = 1)) +
  facet_wrap(~pop, scales = 'free_y', drop = TRUE) +
  theme_rk() +
  theme(legend.position = c(.9,0),
        legend.justification = c(.9,0)) +
  labs(x = 'Spawn Year',
       y = 'Abundance',
       colour = 'Abundance Evaluation Thresholds')

ggsave(paste0(fig_path,'/',gsub(' ','_',spp) ,'_pop_thresholds_',yr,'.png'), width = w, height = h, dpi = dp)

# QET numbers

qet_df <- mod_dat %>%
  filter(spawningyear > yr-10 ) %>%
  mutate(Modeled = exp(.fitted)) %>%
  select(mpg, pop, Empirical = nosaij, Modeled, spawningyear) %>%
  pivot_longer(cols = c(Empirical, Modeled), names_to = 'type', values_to = 'ests') %>%
  #bind_rows(forecast_df) %>%
  mutate(QET = ests <= 50)

qet_value <- 'Modeled'
#qet_value <- 'Empirical'

pop_summs <- qet_df %>%
  filter(type == qet_value) %>%
  group_by(pop) %>%
  summarise(
    mu15 = mean(ests[spawningyear > yr-15]),
    mu10 = mean(ests[spawningyear > yr-10]),
    mu4 = mean(ests[spawningyear > yr-4]),
    geo15 = exp(mean(log(ests[spawningyear > yr-15]))),
    geo10 = exp(mean(log(ests[spawningyear > yr-15]))),
    QET = sum(ests[spawningyear > yr - 4] <= 50)
    )

pop_qet <- qet_df %>%
  filter(spawningyear > (yr - 4)) %>% #advanced a year from Sy20 to SY21 (need four years)
  group_by(pop, type, QET) %>%
  tally() %>%
  pivot_wider(names_from = QET, values_from = n, names_prefix = "QET_", values_fill = 0) %>%
  mutate(p = QET_TRUE/(QET_TRUE + QET_FALSE)) %>%
  arrange(desc(p)) %>%
  filter(type == qet_value)

qet_num <- 4
all_pops <- unique(pop_qet$pop)
npops <- length(all_pops)
qet_pops <- pop_qet$pop[pop_qet$QET_TRUE >= qet_num]
current_pops <- qet_pops
nqet <- nrow(pop_qet[pop_qet$QET_TRUE >= qet_num,])
qet_percent <- nqet/npops

qet_df %>%
  mutate(grp = paste0(pop,type)) %>%
  filter(type == qet_value) %>%
  complete(spawningyear, nesting(pop)) %>%
  ggplot(aes(x = as.integer(spawningyear), y = ests, group = grp)) +
  geom_line(colour = 'grey50') +
  geom_point(aes(fill = QET, shape = type), size = 3, colour = 'black') +
  #geom_point(aes(y = nosaij), shape = 1) +
  #geom_point(aes(y = nosaij)) +
  geom_hline(yintercept = 50, linetype = 2) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_shape_manual(values = c('Empirical' = 20, 'Modeled' = 21, 'Forecast' = 23)) +
  scale_fill_manual(values = c('FALSE'='grey50', 'TRUE'='red')) +
  facet_wrap(~pop, scales = 'free_y', ncol = 4) +
  theme_rk() +
  guides(fill = 'none') +
  guides(shape = 'none') +
  labs(
    #title = plot_title,
    #subtitle = paste0(qet_value,' natural-origin spawner abundance (NOSAij) estimates for Snake River Basin populations relative to the quasi-extinction threshold \n (QET; dashed line, 4 years below 50 NOSAij) for the last 10-years (', yr-9,'-',yr,'). During the last four consecutive years ', round(qet_percent*100),'% of the \n ',npops ,' populations had more than ',qet_num ,' years of abundances below the the QET.'),
    #caption = data_source,
    #fill = 'Below QET',
    #shape = 'Estimate Type',
    x = 'Spawn Year',
    y = 'Abundance'
)

ggsave(paste0(fig_path,'/',gsub(' ','_',spp) ,'_modeled_',yr,'.png'), width = w, height = h, dpi = dp)


# Is abundance decreasing equally for all pops?

sa_dat <- xtT %>% filter(spawningyear > (yr-10)) # need 10 years

sa_dat %>%
  ggplot(aes(x = spawningyear, y = .estimate, group = .rownames)) +
  geom_line(size = 1, colour = 'blue') +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  geom_point(shape = 21, fill = 'blue', colour = 'black') +
  geom_smooth(method = 'lm', colour = 'black', se = FALSE) +
  facet_wrap(~.rownames, scales = "free_y") +
  theme_rk() +
  labs(
    title = plot_title,
    subtitle = 'Modeled natural-origin spawner abundance (NOSAij) estimates for Snake River Basin MPG processes over the last 10-years \n (2015-2024; blue line) with a fitted linear regression model showing the 10-year trend (black line).',
    caption = data_source,
    x = 'Spawn Year',
    y = expression(log~(NOSAij))
)

# model abundance decrease

if(length(unique(xtT$.rownames))>1){
  sa_mod1 <-lm(.estimate ~ spawningyear + .rownames + .rownames:spawningyear, data = sa_dat)
  #summary(spsm_mod1)
  sa_mod2 <- lm(.estimate ~ spawningyear + .rownames, data = sa_dat)
  #summary(spsm_mod2)
  anova(sa_mod1, sa_mod2) # slopes are not different!
  
  grp_slope <- broom::tidy(sa_mod2) %>%
    filter(term == 'spawningyear')
  
  sa_slope <- sa_dat %>%
    nest(data = -.rownames) %>%
    mutate(
      fit = purrr::map(data, ~ lm(.estimate ~ spawningyear, data = .x)),
      tidied = purrr::map(fit, broom::tidy)
    ) %>%
    unnest(tidied) %>%
    filter(term == 'spawningyear')
  #mutate(grp_mod = ifelse(pop %in% exclude_pops, 0, 1)) %>%
  
  sa_slope %>%
    left_join(pop_cnt, by = c('.rownames' = 'mpg')) %>%
    ggplot(aes(x = fct_reorder(strip_labels, estimate), y = estimate)) +
    geom_pointrange(aes(y = estimate,
                        ymin = estimate - 1.96*std.error,
                        ymax = estimate + 1.96*std.error),
                    shape = 21, fill = 'blue', colour = 'blue') +
    geom_text(aes(label = round(estimate,2)), size = 6, vjust = -1) +
    #scale_fill_manual(values = c('white', 'black'), labels = c('Excluded', 'Included')) +
    geom_hline(data = grp_slope, aes(yintercept = estimate), colour = 'black') +
    geom_hline(data = grp_slope, aes(yintercept = estimate - 1.96*std.error), colour = 'black', linetype = 2) +
    geom_hline(data = grp_slope, aes(yintercept = estimate + 1.96*std.error), colour = 'black', linetype = 2) +
    scale_y_continuous(limits = c(-.3, .3), breaks = c(-.3, -.2, -.1, 0, .1, .2, .3)) +
    coord_flip() +
    theme_rk2() +
    labs(
      #title = plot_title,
      #subtitle = paste('Modeled natural-origin spawner abundance growth for last 10-years (',min(sa_dat$spawningyear),'-',max(sa_dat$spawningyear),') across Snake Basin major population groups. Population abundance on \n average declined by approximately',round((1-exp(grp_slope$estimate))*100),'% each year across the time period.'),
      #caption = data_source,
      x = '',
      y = 'Slope (log scale)'
      )
  
  } else {
    
    sa_slope <- sa_dat %>%
      nest(data = -.rownames) %>%
      mutate(
        fit = purrr::map(data, ~ lm(.estimate ~ spawningyear, data = .x)),
        tidied = purrr::map(fit, broom::tidy)
      ) %>%
      unnest(tidied) %>%
      filter(term == 'spawningyear')
    #mutate(grp_mod = ifelse(pop %in% exclude_pops, 0, 1)) %>%
    
    sa_slope %>%
      left_join(pop_cnt, by = c('.rownames' = 'mpg')) %>%
      ggplot(aes(x = fct_reorder(strip_labels, estimate), y = estimate)) +
      geom_pointrange(aes(y = estimate,
                          ymin = estimate - 1.96*std.error,
                          ymax = estimate + 1.96*std.error),
                      shape = 21, fill = 'blue', colour = 'blue') +
      geom_text(aes(label = round(estimate,2)), size = 6, vjust = -1) +
      #scale_fill_manual(values = c('white', 'black'), labels = c('Excluded', 'Included')) +
      #geom_hline(data = grp_slope, aes(yintercept = estimate), colour = 'black') +
      #geom_hline(data = grp_slope, aes(yintercept = estimate - 1.96*std.error), colour = 'black', linetype = 2) +
      #geom_hline(data = grp_slope, aes(yintercept = estimate + 1.96*std.error), colour = 'black', linetype = 2) +
      scale_y_continuous(limits = c(-.3, .3), breaks = c(-.3, -.2, -.1, 0, .1, .2, .3)) +
      coord_flip() +
      theme_rk2() +
      labs(
        #title = plot_title,
        #subtitle = paste('Modeled natural-origin spawner abundance growth for last 10-years (',min(sa_dat$spawningyear),'-',max(sa_dat$spawningyear),') across Snake Basin major population groups. Population abundance on \n average declined by approximately',round((1-exp(grp_slope$estimate))*100),'% each year across the time period.'),
        #caption = data_source,
        x = '',
        y = 'Slope (log scale)'
      )

    state_slope <- lm(.estimate ~ spawningyear, sa_dat)
    # summary(state_slope)
    # 
    # sa_dat %>%
    #     mutate(spawnngyear = as.integer(spawningyear)) %>%
    #     ggplot(aes(x = spawningyear, y = .estimate)) +
    #     geom_point(shape = 21, fill = 'blue', colour = 'blue') +
    #     geom_smooth(method = 'lm', se = FALSE) +
    #     scale_x_continuous(breaks = scales::pretty_breaks()) +
    #     theme_rk2() +
    #     labs(
    #       #title = plot_title,
    #       #subtitle = paste('Modeled natural-origin spawner abundance growth for last 10-years (',min(sa_dat$spawningyear),'-',max(sa_dat$spawningyear),') across Snake Basin major population groups. Population abundance on \n average declined by approximately',round((1-exp(coef(state_slope)[2]))*100),'% each year across the time period.'),
    #       #caption = data_source,
    #       x = 'Spawn Year',
    #       y = 'LN(Abundance)'
    #       )
  }

ggsave(paste0(fig_path,'/',gsub(' ','_',spp) ,'_slope_',yr,'.png'), width = w, height = h, dpi = dp)

# Projections

if(length(unique(xtT$.rownames))>1){
  new_dat <- sa_slope %>%
    select(mpg = .rownames, estimate) %>%
    distinct() %>%
    inner_join(tibble(mpg = rep(unique(qet_df$mpg),each = 5),
                      spawningyear = rep((yr+1):(yr+5), length(unique(xtT$.rownames))))
    ) %>%
    inner_join(qet_df %>%
                 filter(spawningyear == yr) %>%
                 filter(type == qet_value) %>%
                 mutate(last = log(ests)) %>%
                 select(mpg, pop, last)
    ) %>%
    group_by(pop) %>%
    mutate(ests = exp(last + cumsum(estimate)),
           QET = ests <= 50,
           type = 'Prediction')
  
  new_predicts <- bind_rows(qet_df %>%
                              filter(type == qet_value),
                            new_dat)
    
} else {
  new_dat <- tibble(
    spawningyear = rep((yr+1):(yr+5), length(unique(xtT$.rownames))),
    estimate = coef(state_slope)[2]
    ) %>%
    cross_join(qet_df %>%
                 filter(spawningyear == yr) %>%
                 filter(type == qet_value) %>%
                 mutate(last = log(ests)) %>%
                 select(mpg, pop, last)
               ) %>%
    group_by(pop) %>%
    mutate(ests = exp(last + cumsum(estimate)),
           QET = ests <= 50,
           type = 'Prediction')
  
  new_predicts <- bind_rows(qet_df %>%
                              filter(type == qet_value),
                            new_dat)
}


pop_qet <- new_predicts %>%
  filter(spawningyear > yr) %>%
  group_by(pop, type, QET) %>%
  tally() %>%
  pivot_wider(names_from = QET, values_from = n, names_prefix = "QET_", values_fill = 0) %>%
  mutate(p = QET_TRUE/(QET_TRUE + QET_FALSE))

qet_num <- 1
all_pops <- unique(pop_qet$pop)
npops <- length(all_pops)
qet_pops <- pop_qet$pop[pop_qet$QET_TRUE >= qet_num]
nqet <- nrow(pop_qet[pop_qet$QET_TRUE >= qet_num,])
qet_percent <- nqet/npops

new_predicts %>%
  #complete(spawningyear, nesting(pop)) %>%
  ggplot(aes(x = as.integer(spawningyear), y = ests, group = pop)) +
  geom_line(colour = 'grey50') +
  geom_point(aes(fill = QET, shape = type), size = 3, colour = 'black') +
  #geom_point(aes(y = nosaij), shape = 1) +
  #geom_point(aes(y = nosaij)) +
  geom_hline(yintercept = 50, linetype = 2) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_shape_manual(values = c('Empirical' = 23, 'Modeled' = 21, 'Prediction' = 24)) +
  scale_fill_manual(values = c('FALSE'='grey50', 'TRUE'='red')) +
  facet_wrap(~pop, scales = 'free_y', ncol = 4) +
  theme_rk() +
  guides(fill = 'none') +
  guides(shape = 'none') +
  labs(
    #title = plot_title,
    #subtitle = paste0('Future predictions of natural-origin spawner abundance (NOSAij) for Snake River Basin show ',nqet,' populations (',round(qet_percent*100),'%) will start to drop below the the \n quasi-extinction threshold (QET; dashed line; 50 spawners) within the next 5 years. Circle points are modeled estimates from annual data collection and triangles represent future predictions.'),
    #caption = data_source,
    #fill = 'Below QET',
    #shape = 'Estimate Type',
    x = 'Spawn Year',
    y = 'Abundance'
)

ggsave(paste0(fig_path,'/',gsub(' ','_',spp) ,'_predictions_',yr,'.png'), width = w, height = h, dpi = dp)  

# New predict figure by MPG ----
mpg_preds <- new_predicts %>%
  # mutate(area = case_when(
  #   mpg == 'Lower Snake' ~ 'Lower Snake',
  #   mpg == 'Grande Ronde / Imnaha' ~ 'Northeast Oregon',
  #   grepl('Clearwater', mpg) ~ 'Clearwater River',
  #   grepl('Salmon', mpg) ~ 'Salmon River')) %>%
  mutate(mpg = gsub('/','-',mpg)) %>%
  group_by(mpg) %>%
  nest() %>%
  mutate(fig = purrr::map(data,
                   ~ggplot(data = ., aes(x = as.integer(spawningyear), y = ests, group = pop)) +
                     geom_line(colour = 'grey50') +
                     geom_point(aes(fill = QET, shape = type), size = 3, colour = 'black') +
                     geom_hline(yintercept = 50, linetype = 2) +
                     scale_x_continuous(breaks = scales::pretty_breaks()) +
                     scale_shape_manual(values = c('Empirical' = 23, 'Modeled' = 21, 'Prediction' = 24)) +
                     scale_fill_manual(values = c('FALSE'='grey50', 'TRUE'='red')) +
                     facet_wrap(~pop, scales = 'free_y', ncol = 3) +
                     theme_rk() +
                     guides(fill = 'none') +
                     labs(
                       title = plot_title,
                       subtitle = paste0('Predictions of natural-origin spawner abundance (NOSAij) for ',mpg,' populations. Red points are annual returns below 50 spawners.'),
                       caption = data_source,
                       #fill = 'Below QET',
                       shape = 'Estimate Type',
                       x = 'Spawn Year',
                       y = 'Abundance'
                       )
  )
  )


map2(mpg_preds$fig, mpg_preds$mpg,
     ~ggsave(paste0(fig_path,'/',gsub(' ','_',spp) ,'MPG_', gsub(' ','_',.y), '_predictions_',yr,'.png'), width = w, height = h, dpi = dp)
)


# Maps
library(sf)
library(scales)
library(ggmap)
library(maps)
# Load datasets ----
# Get states

user_path <- Sys.getenv('OneDrive')
proj_path <- '/Projects/DFRM Projects/River_Mapping/data/'

spatial_files <- paste0(user_path, proj_path, 'polygons/SR_pops.rda')
load(spatial_files) ; rm(fall_pop)

mgt_targets <- readxl::read_excel('./data/input/mgt_targets.xlsx', sheet = 'pop_targets')

if(spp == 'Chinook salmon'){
  trt_pops <- spsm_pop %>%
    filter(!grepl('NC', TRT_POPID)) %>%
    mutate(pop = str_trim(str_remove(POP_NAME, " above.*|below.*"))) %>%  #remove everything after
    #extract = str_trim(str_extract(POP_NAME, ".*(?= above|below)"))) %>% # extract everything before
    mutate(pop = case_when(
      TRT_POPID == 'MFLMA' ~ 'Middle Fork Salmon River Lower Mainstem',
      TRT_POPID == 'MFUMA' ~ 'Middle Fork Salmon River Upper Mainstem',
      TRT_POPID == 'GRLOS' ~ 'Wallowa/Lostine Rivers',
      TRT_POPID == 'SFSMA' ~ 'South Fork Salmon River',
      TRT_POPID == 'CRPOT' ~ 'Potlatch River',
      TRUE ~ pop
    )) %>%
    mutate(pop = str_to_title(pop)) %>%
    select(mpg = MPG, pop, TRT_POPID) %>%
    left_join(mgt_targets <- mgt_targets %>%
                filter(species == 'Chinook salmon') %>%
                select(pop, CBP_goals, target) %>%
                pivot_wider(names_from = CBP_goals, values_from = target),
              by = 'pop')
  
  full_esu_dps <- sf::st_read(paste0(user_path, proj_path,'polygons/CHNK_SPSU_ALL/CHNK_SPSU_All.shp')) %>%
    filter(grepl('Snake', ESU_DPS)) %>%
    mutate(extant = case_when(
      grepl('Outside', ESU_DPS) ~ 'Extinct',
      grepl('North Fork Clearwater', NWR_NAME) ~ 'Extinct',
      TRUE ~ 'Extant')
    ) %>%
    group_by(extant) %>%
    summarize(shape_area = sum(SHAPE_AREA)) %>%
    st_transform(crs = 4326)

} else {
  trt_pops <- sth_pop %>%
    filter(!grepl('NFC', TRT_POPID)) %>%
    mutate(pop = str_trim(str_remove(POP_NAME, " above.*|below.*"))) %>%  #remove everything after
    mutate(pop = case_when(
      TRT_POPID == 'GRLMT-s' ~ 'Grande Ronde River Lower Mainstem',
      TRT_POPID == 'SRLSR-s' ~ 'Little Salmon River',
      TRT_POPID == 'MFBIG-s' ~ 'Middle Fork Salmon River Lower Mainstem',
      TRUE ~ pop
    )) %>%
    mutate(pop = str_to_title(pop)) %>%
    select(mpg = MPG, pop, TRT_POPID) %>%
    left_join(mgt_targets <- mgt_targets %>%
              filter(species == 'Steelhead') %>%
              select(pop, CBP_goals, target) %>%
              pivot_wider(names_from = CBP_goals, values_from = target),
            by = 'pop')
  
  
  full_esu_dps <- sf::st_read(paste0(user_path, proj_path,'polygons/STHD_SUWI_ALL/STHD_SUWI_All.shp')) %>%
    filter(grepl('Snake', ESU_DPS)) %>%
    mutate(extant = case_when(
      grepl('Outside', ESU_DPS) ~ 'Extinct',
      grepl('North Fork Clearwater', NWR_NAME) ~ 'Extinct',
      TRUE ~ 'Extant')
    ) %>%
    group_by(extant) %>%
    summarize(shape_area = sum(SHAPE_AREA)) %>%
    st_transform(crs = 4326)
  
}


states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

pnw <- states %>% filter(ID %in% c('idaho', 'oregon', 'washington')) %>%
  st_transform(crs = 4326)

# load polygons
load(paste0(user_path, proj_path, 'polygons/npt_boundaries.rda'))

 icc <- icc %>%
   st_transform(crs = 4326)

# Subset Pops/Remove NF Clearwater ----
# sth_pop <- sth_pop %>%
#   filter(TRT_POPID != 'CRNFC-s')

#plot(sth_pop %>% st_geometry())

# load rivers and trim
load(paste0(user_path, proj_path,"flowlines/large_rivers.rda"))
# load(paste0(user_path, proj_path,"flowlines/SR_streams.rda"))
# 
pnw_rivers <- st_intersection(pnw_rivers %>% 
                                 st_transform(crs = 4326), sth_pop)
# 
# snake_rivers <- st_intersection(snake_rivers %>%
#                                   st_transform(crs = 4326), sth_pop)

# Get basemap
# snk_basin <- as_Spatial(sth_pop) # convert to use sp package fun
#b <- sp::bbox(snk_basin) # get bounding box around polygons
# map_center <- apply(b,1,mean) # find center

# gather basemap from google

# register_google('AIzaSyAotfBYbjtFIPKSYtoCqMnLgvVSelwgbhc', write = TRUE)
# # 
# base_map <- ggmap(get_googlemap(center = map_center,
#                                 maptype = 'satellite',
#                                 zoom = 7,
#                                 scale = 4,
#                                 color = 'color'
#     )
#   )

#save(base_map, file = './data/base_map.rda')
#load('./data/base_map.rda')


# Chinook ggplot ----

map_qet <- left_join(new_predicts,
                 trt_pops %>%
                   select(pop, High, Low, Medium, Critical) %>%
                   st_drop_geometry(),
                 by = 'pop') %>%
  filter(spawningyear >= yr) %>%
  group_by(pop) %>%
  mutate(QET = case_when(
    pop %in% current_pops ~ 'Currently Below QET50',
    any(ests[spawningyear >= yr]) >= High ~ 'Above Healthy and Harvestable',
    any(ests[spawningyear >= yr]) >= Low ~ 'Above Minimum Abundance',
    ests[spawningyear == yr] <= Critical ~ paste0("Below 50 Spawners in ", yr),
    ests[spawningyear == yr + 5] > Critical ~ paste0("Predicted Above 50 in ", yr+5),
    ests[spawningyear == yr + 5] <= Critical ~ paste0("Predicted Below 50 by ", yr+5)
  )) %>%
  select(pop, QET) %>%
  distinct()

map_qet %>%
  ungroup() %>%
  count(QET)

# TRT model data pop names
# TRT spatial data pop names.

qet_levels <- c('Above Healthy and Harvestable',
                'Above Minimum Abundance',
                paste0("Predicted Above 50 in ", yr+5),
                paste0("Predicted Below 50 by ", yr+5),
                paste0("Below 50 Spawners in ", yr),
                'Currently Below QET50',
                'Not Modeled',
                'Extinct')

map_dat <- left_join(trt_pops, map_qet) %>%
  mutate(QET = ifelse(is.na(QET), 'Not Modeled', QET)) %>%
  arrange(QET) %>%
  mutate(QET = factor(QET, levels = qet_levels))

missing_levels <- setdiff(qet_levels, unique(map_dat$QET))

if (length(missing_levels) > 0) {
  dummy_sf <- sf::st_sf(
    QET = factor(missing_levels, levels = qet_levels),
    geometry = sf::st_sfc(lapply(missing_levels, function(x) sf::st_geometrycollection())),
    crs = sf::st_crs(map_dat)
  )
  
  # 5. Bind to map_dat
  map_dat <- bind_rows(map_dat, dummy_sf)
}


#col <- c(colorRampPalette(c("#FF5E5B", "#F5C33B"), alpha = TRUE)(4), 'grey80')
#col <- c('lightgreen', 'dodgerblue', colorRampPalette(c("#F5C33B", "#FF5E5B"), alpha = TRUE)(4), 'grey50', 'grey80')
col <- c('#99d594', '#3288bd', '#ffffbf', '#f3be2a', '#fc8d59', '#E31A1C', 'grey50', 'grey80')

scales::show_col(col)

b <- st_bbox(full_esu_dps)
buffer <- 0.1  # degrees; adjust to your need
xlim <- c(b$xmin - buffer, b$xmax + buffer)
ylim <- c(b$ymin - buffer, b$ymax + buffer)

map_fig <- ggplot() +
  #base_map +
  geom_sf(data = pnw, fill = NA, inherit.aes = FALSE) +
  geom_sf(data = full_esu_dps, fill = 'grey80', inherit.aes = FALSE) +
  geom_sf(data = map_dat, aes(fill = QET),
          colour = 'black', size = 1, inherit.aes = FALSE) +
  #geom_sf(data = snake_rivers, colour = 'cyan', inherit.aes = FALSE) +
  #geom_sf(data = icc, aes(colour = 'NPT ICC Boundary'), fill = NA, linewidth = 1.25, inherit.aes = FALSE) +
  coord_sf(xlim = xlim, ylim = ylim) +
  ggrepel::geom_label_repel(
    data = map_dat[map_dat$QET!='Not Modeled',],
    aes(label = TRT_POPID, geometry = geometry,
        fill = QET,
    ),
    #alpha = .75,
    size = 2,
    stat = "sf_coordinates",
    min.segment.length = 0,
    #colour = "white",
    #segment.colour = "magenta",
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  #geom_sf(data = SR_streams, colour = 'cyan', inherit.aes = FALSE) +
  #scale_fill_viridis_d() + 
  #scale_fill_manual(values = c('violetred3','violet', 'lightgreen', 'grey75')) +
  scale_fill_manual(values = col, drop = FALSE) +
  scale_color_manual(values = 'black') +
  #scale_fill_brewer(palette = 'YlOrRd', direction = -1) +
  #scale_fill_manual(values = c('firebrick', 'orange', 'yellow','limegreen', 'grey75'), drop = FALSE) +
  #geom_point(data = sites, aes(x = Longitude, y = Latitude)) +
  #geom_label(data = sites, aes(x = Longitude, y = Latitude, label = SiteID), size = 2) +
  labs(
      #title = plot_title,
      #subtitle = 'Snake River Basin Populations',
      fill = 'Population Status',
      colour = 'Snake River Basin'
      ) +
  guides(guide_legend(ncol = 1)) +
  theme_void() +
  theme(#plot.title=element_text(colour = 'white', hjust=.10, vjust=-1, face='bold', margin=margin(t=0,b=-40)),
    #plot.subtitle = element_text(colour = 'white', hjust=.10, vjust=-1, face='bold', margin=margin(t=0,b=-50)),
    legend.position = c(0.00,0.15),
    legend.key = element_blank(),
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing.y = unit(1, "pt"),
    #legend.background = element_rect(fill=scales::alpha('white', 0.4), colour = NA)
  ) # fill = 'transparent'

map_fig

library(cowplot)

inset <- ggplot() +
  geom_sf(data = pnw) +
  geom_sf(data = full_esu_dps, fill = 'grey80') +
  geom_sf(data = map_dat, aes(fill = QET),
          colour = 'black', size = 1, inherit.aes = FALSE) +
  scale_fill_manual(values = col, drop = FALSE) +
  scale_color_manual(values = 'black') +
  theme_nothing()

ggdraw(map_fig) +
  draw_plot(inset,
            x = .75,
            y = .6,
            width = .25,
            height = .35)

ggsave(paste0(fig_path,'/',gsub(' ','_',spp) ,'_map_',yr,'.png'), width = w, height = h, dpi = dp)



brbg <- RColorBrewer::brewer.pal(4, "BrBG")[c(1,2)]
show_col(brbg)


offices <- st_as_sf(
  tribble(~'city', ~'lon', ~'lat',
          'Lapwai', 46.399965, -116.802493,
          'Sweetwater', 46.372628, -116.796259,
          'McCall', 44.862112, -116.087774,
          'Joseph', 45.356404, -117.229461,
          'NPTH', 46.520427, -116.660615,
          'Kooskia', 46.129688, -115.946948,
          'Orofino', 46.464489, -116.234573,
          'Grangeville', 45.926067, -116.122625,
          'Powell', 46.509943, -114.712225),
  coords = c('lat', 'lon'),
  crs = st_crs(4326)
)


tmp <- ggplot() +
  geom_sf(data = pnw) +
  geom_sf(data = full_esu_dps,  aes(fill = extant)) +
  geom_sf(data = map_dat, fill = NA,
          colour = 'black', size = 1, inherit.aes = FALSE) +
  geom_sf(data = icc, fill = NA, linewidth = 1.25, color = 'black') +
  #geom_sf(data = npt1863, fill = NA, linewidth = 1, color = 'green') +
  geom_sf(data = pnw_rivers, inherit.aes = FALSE, color = 'blue') +
  geom_sf(data = offices, inherit.aes = FALSE, size = 2, color = 'black') +
  scale_color_manual(values = 'black') +
  scale_fill_manual(values = brbg) +
  theme_nothing()

tmp

ggsave(paste0(fig_path,'/','_basin_map_.png'), width = w, height = h, dpi = dp)


#saveRDS(map_dat, here::here("data","map_data","spsm_qet_map.rds"))




# 
# 
# # Productivity
# rs_fig <- spsm_mod_fits %>%
#   arrange(pop, spawningyear) %>%
#   group_by(pop) %>%
#   mutate(
#     R = lead(states, n = 4),
#     logRS = R - log(tsaij+1),
#     RS = exp(logRS),
#     pos = logRS >= 0) %>%
#   ggplot(aes(x =spawningyear, y = logRS, fill = pos)) +
#   geom_col(colour = 'black') +
#   scale_fill_manual(values = c('red3','green3')) +
#   facet_wrap(~pop, ncol = 4) +
#   theme_rk() +
#   theme(legend.position = 'none') +
#   labs(title = plot_title,
#        subtitle = 'Modeled natural-origin spawner productivity for Snake River Basin populations. Productivity is calculated on the log scale as returning natural-origin \n recruits (R) four years later minus total spawners (S).',
#        x = 'Spawn Year',
#        y = expression(log~(R[t+4]) - log~(S[t])),
#        caption = data_source)
# rs_fig
# 
# ggsave(paste0('./figures/',gsub(' ','_',spp),'_rs_fig_',yr,'.png'), plot = rs_fig, width = 16, height = 10, dpi = 500)
# 
# # SAR Graphic
# SAR <- tribble(
#   ~Socean, ~Shydro,
#   0.06, .8,
#   0.04, 0.55,
#   0.02, 0.4
# )
# 
# SAR <- expand.grid(SAR) %>%
#   mutate(SAR = (Socean * Shydro)*100) %>%
#   mutate(Ocean = case_when(
#     Socean == .06 ~ 'Good (6%)',
#     Socean == .04 ~ 'Average (4%)',
#     Socean == 0.02 ~ 'Poor (2%)'
#   )) %>%
#   mutate(Hydro = case_when(
#     Shydro == .8 ~ 'Historic (80%)',
#     Shydro == .55 ~ 'Current (55%)',
#     Shydro == 0.4 ~ 'Low (40%)'
#   )) %>%
#   mutate(Ocean = factor(Ocean, levels = c('Poor (2%)', 'Average (4%)', 'Good (6%)')),
#          Hydro = factor(Hydro, levels = c('Low (40%)', 'Current (55%)', 'Historic (80%)')))
# 
# SAR_fig_cat <- ggplot(SAR, aes(x = Hydro, y = Ocean, fill = as.factor(SAR))) +
#   scale_fill_brewer(palette = 'RdYlGn') +
#   geom_tile() +
#   geom_text(aes(label = paste0(round(SAR,2),'%')), size = 12) +
#   scale_x_discrete(expand = c(0,0)) +
#   scale_y_discrete(expand = c(0,0)) +
#   labs(x = 'In-river Survival',
#        y = 'Marine Survival',
#        fill = 'SAR Percent') +
#   theme_bw() +
#   theme(legend.position = 'none',
#         axis.title = element_text(size = 24, colour = 'black'),
#         axis.text = element_text(size = 24, colour = 'black'))
# 
# SAR_fig_cat
# 
# ggsave(paste0('./figures/SAR_fig.png'), plot = SAR_fig_cat, width = 16, height = 10, dpi = 500)
# 
# s_hydro = seq(.3,.8,by = .01)
# s_ocean = seq(.01,.15, by = .001)
# 
# SAR = matrix(nrow = length(s_hydro), ncol = length(s_ocean),
#              dimnames = list(s_hydro,
#                              s_ocean))
# 
# for(i in 1:length(s_hydro)){
#   for(j in 1:length(s_ocean)){
#     SAR[i,j] = s_hydro[i] * s_ocean[j]
#   }
# }
# 
# SAR <- as_tibble(SAR)
# names(SAR) <- paste0('Socean_',s_ocean)
# 
# SAR <- SAR %>%
#   mutate(Shydro = s_hydro) %>%
#   pivot_longer(-Shydro, names_to = 'Socean', names_prefix = 'Socean_', values_to = 'SAR') %>%
#   mutate(Socean = as.double(Socean)) %>%
#   mutate(SARbin = cut(SAR*100, breaks = c(0,2,4,6,8,15)))
# 
# sar_labs <- tribble(
#   ~Shydro, ~Socean, ~lab,
#   .35, .03, '0-2% SAR',
#   .46, .060, '2-4% SAR',
#   .57, .088, '4-6% SAR',
#   .65, .11, '6-8% SAR',
#   .75, .135, '8-15% SAR'
# )
# 
# fig_sar_cont <- ggplot() +
#   scale_fill_brewer(palette = 'RdYlGn') +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   geom_tile(data = SAR, aes(x = Shydro, y = Socean, fill = SARbin)) +
#   geom_text(data = sar_labs, aes(x = Shydro, y = Socean, label = lab), size = 14) +
#   labs(x = 'In-river Survival',
#        y = 'Marine Survival',
#        fill = 'SAR Percent') +
#   theme_bw() +
#   theme(legend.position = 'none',
#         axis.title = element_text(size = 24, colour = 'black'),
#         axis.text = element_text(size = 24, colour = 'black'))
# 
# fig_sar_cont
# 
# ggsave(paste0('./figures/SAR_cont_fig.png'), plot = fig_sar_cont, width = 16, height = 10, dpi = 500)

