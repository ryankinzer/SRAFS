# Purpose: Create draft graphics for a website
# Author: Ryan N. Kinzer
# Created: 26 October 2021

library(tidyverse)
#library(plotly)
library(patchwork)
source('./R/theme_rk.R')
yr <- 2024

# Run getIDFGdata.R ----


# Sp/sm Chinook Salmon ----
load(file='./data/IDFG_historic/idfg_chn_data.rda')



idfg_dat %>%
  ggplot(aes(x= spawnyear, y = est)) +
  geom_line() +
  geom_point() +
  facet_grid(origin ~ size)
  

# 5-year average
idfg_dat %>%
  filter(spawnyear >= (yr-4)) %>%
  filter(origin == 'Wild') %>%
  group_by(size) %>%
  summarise(min_yr = min(spawnyear),
            max_yr = max(spawnyear),
            n_yrs = n(),
            avg = mean(est))

idfg_dat %>%
  ggplot(aes(x = spawnyear, y = est)) +
  geom_col(aes(fill = origin))

# new_df <- tribble(~Year, ~species, ~run_cmb, ~origin, ~group, ~IDFG_total,
#                   2021, 'Chinook', 'Sp/sm', 'Wild', 'Adult', 6556,  #email to Jay Hesse on 1/31/2022 from Jonathan Ebel
#                   2021, 'Chinook', 'Sp/sm', 'Hatchery', 'Adult', 29957, 
#                   2021, 'Chinook', 'Sp/sm', NA, 'Total_Adults', 36513)#,                  
# #2022, 'Chinook', 'Sp/sm', 'Wild', 'Adult', 9739)
# 
# idfg_chn_data <- bind_rows(idfg_chn_data, new_df) %>%
#   mutate(estimate = ifelse(Year == 2022, 'Forcast', 'Estimate'))

mgt_targets <- readxl::read_excel('./data/mgt_targets.xlsx',
                                  sheet = 'targets') %>%
  mutate(target = as.numeric(target))

# total graph
idfg_chn_data <- idfg_dat %>%
  mutate(origin = ifelse(is.na(origin), 'Total', origin),
         group = ifelse(group == 'Total_Adults', 'Adult', group))

total_df <- idfg_chn_data %>%
  filter(species == 'Chinook',
         run_cmb == 'Sp/sm',
         group == 'Adult') %>%
  pivot_wider(names_from = origin, values_from = IDFG_total) %>%
  mutate(phos = Hatchery/Total,
         origin = 'Total (Wild + Hatchery)')

stack_df <- idfg_chn_data %>%
  filter(species == 'Chinook',
         run_cmb == 'Sp/sm',
         group == 'Adult') %>%
  filter(origin != 'Total') %>%
  mutate(grp = 'Total (Wild + Hatchery)')

spsm_mgt_total <- mgt_targets %>%
  filter(species == 'Chinook salmon',
         run == 'Spring/summer',
         grepl('Total', origin)) %>%
  mutate(origin = 'Total (Wild + Hatchery)')

#scaleFactor <- 345000 / max(total_df$phos)

t <- total_df %>%
  #  filter(Year >= 1975) %>%
  ggplot(aes(x = Year)) +
  geom_col(aes(y = Total), fill = 'grey75', colour = 'black') +
  #geom_line(aes(y = phos * scaleFactor)) +
  geom_hline(data = spsm_mgt_total, 
             aes(yintercept = target, colour = goal_label), size = 1) +
  geom_label(data = spsm_mgt_total, 
             aes(x = 1970, y = target, label = goal_label, colour = goal_label)) +
  scale_color_manual(values = c('darkgreen')) +
  scale_x_continuous(breaks = seq(1960, 2020, 5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 350000),
                     label = scales::comma,
                     breaks = seq(25000,325000, by = 100000)#,
                     # sec.axis = sec_axis(~./scaleFactor, name = 'Percent Hatchery',
                     #                     labels = function(b){paste0(round(b * 100,0),"%")},
                     #                     breaks = seq(0,1, by = .1))
  ) +
  facet_wrap(~origin) +
  labs(
    #title = 'Spring/Summer Chinook Salmon',
    #subtitle = 'Adult escapement at Lower Granite Dam',
    # caption = 'Data Source: Idaho Department of Fish and Game; Lawry et al. 2020',
    x = 'Spawn Year',
    y = 'Escapement'
  ) +
  theme_rk() +
  theme(legend.position = 'none')

t

t2 <- stack_df %>%
  ggplot(aes(x = Year)) +
  geom_col(aes(y = IDFG_total, fill = origin), colour = 'black') +
  scale_fill_manual(values = c('grey60', 'grey80'), breaks = c('Wild', 'Hatchery'), labels = c('Wild', 'Hatchery')) +
  geom_hline(data = spsm_mgt_total, 
             aes(yintercept = target, colour = goal_label), size = 1) +
  geom_label(data = spsm_mgt_total, 
             aes(x = 1970, y = target, label = goal_label, colour = goal_label)) +
  scale_color_manual(values = c('darkgreen'), guide = 'none') +
  scale_x_continuous(breaks = seq(1960, 2020, 5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 350000),
                     label = scales::comma,
                     breaks = seq(25000,325000, by = 100000)#,
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

t2


# hatchery fraction
phos_line <- total_df %>%
  ggplot(aes(x = Year, y = 1-phos)) +
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

phos_line

#t_phos <- t + inset_element(phos_line, left = .5, bottom = .53, right = .95, top = .92)
t_phos <- t2 + inset_element(phos_line, left = .7, bottom = .36, right = .95, top = .9)
t_phos

# hatchery graph

spsm_mgt_H <- mgt_targets%>%
  filter(species == 'Chinook salmon',
         run == 'Spring/summer',
         origin == 'Hatchery')

h <- idfg_chn_data %>%
  filter(species == 'Chinook',
         run_cmb == 'Sp/sm',
         origin == 'Hatchery',
         group == 'Adult') %>%
  #  filter(Year >= 1975) %>%
  ggplot(aes(x = Year, y =IDFG_total)) +
  geom_col(fill = 'grey80', colour = 'black') +
  geom_hline(data = spsm_mgt_H, 
             aes(yintercept = target, colour = goal_label), size = 1) +
  geom_label(data = spsm_mgt_H, 
             aes(x = c(1980,2010), y = target, label = goal_label, colour = goal_label)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 260000), label = scales::comma, breaks = c(235000, 90000,11638)) +
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

h

h_diff_df <- idfg_chn_data %>%
  ungroup() %>%
  filter(species == 'Chinook',
         run_cmb == 'Sp/sm',
         origin == 'Hatchery',
         group == 'Adult') %>%
  mutate(diff = IDFG_total - 90000,
         direct = as.factor(sign(diff)))

h_diff_df %>%
  ggplot(aes(x = '', y = direct, fill = direct)) +
  geom_bar(stat = 'identity') +
  coord_polar('y', start = 0)

h_diff <- h_diff_df %>%
  #  filter(Year >= 1975) %>%
  ggplot(aes(x = Year, y =diff)) +
  geom_col(aes(fill = direct)) +
  geom_hline(yintercept = mean(h_diff_df$diff), size = 2) +
  geom_label(x = 1980, y = mean(h_diff_df$diff),
             label = paste0('Average = ', round(mean(h_diff_df$diff)))) +
  scale_fill_manual(values = c('firebrick', 'navy')) +
  scale_y_continuous(labels = scales::comma) +
  #facet_wrap(~origin, ncol = 2) +
  labs(title = 'Difference in hatchery abundance from mitigation goals.',
       x = 'Spawn Year',
       y = 'Escapement') +
  theme_rk() +
  theme(legend.position = 'none')

h_diff
#ggsave('./figures/website/hatchery_diff.png', h_diff, width = 11, height = 8.5)



# natural graph

spsm_mgt_W <- mgt_targets%>%
  mutate(origin = ifelse(origin == 'Wild/natural', 'Wild', origin)) %>%
  filter(species == 'Chinook salmon',
         run == 'Spring/summer',
         origin == 'Wild') %>%
  filter(CBP_goals != 'Medium')

w <- idfg_chn_data %>%
  filter(species == 'Chinook',
         run_cmb == 'Sp/sm',
         origin == 'Wild',
         group == 'Adult') %>%
  #  filter(Year >= 1975) %>%
  ggplot(aes(x = Year, y =IDFG_total)) +
  geom_col(fill = 'grey60', colour = 'black') +
  #scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  geom_hline(data = spsm_mgt_W, 
             aes(yintercept = target, colour = goal_label), size = 1) +
  geom_label(data = spsm_mgt_W, 
             aes(x = 1980, y = target, label = goal_label, colour = goal_label), vjust = c(.5,.5, 0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 260000), label = scales::comma, breaks = c(235000,43000,1850)) +
  scale_color_manual(values = c('firebrick', 'navy', 'darkgreen')) +  
  facet_wrap(~origin, ncol = 2) +
  labs(#title = 'Natural-origin Spring/Summer Chinook Salmon',
    #subtitle = 'Adult escapement at Lower Granite Dam',
    x = 'Spawn Year',
    y = 'Escapement'
  ) +
  theme_rk() +
  theme(legend.position = 'none')

w

w_diff_df <- idfg_chn_data %>%
  ungroup() %>%
  filter(species == 'Chinook',
         run_cmb == 'Sp/sm',
         origin == 'Wild',
         group == 'Adult') %>%
  mutate(diff = IDFG_total - 43000,
         direct = as.factor(sign(diff)))

w_diff <- w_diff_df %>%
  #  filter(Year >= 1975) %>%
  ggplot(aes(x = Year, y =diff)) +
  geom_col(aes(fill = direct)) +
  geom_hline(yintercept = mean(w_diff_df$diff), size = 2) +
  geom_label(x = 1990, y = mean(w_diff_df$diff),
             label = paste0('Average = ', round(mean(w_diff_df$diff)))) +
  scale_fill_manual(values = c('firebrick', 'navy')) +
  scale_y_continuous(labels = scales::comma) +
  #facet_wrap(~origin, ncol = 2) +
  labs(title = 'Difference in wild abundance from ESA delisting thesholds.',
       x = 'Spawn Year',
       y = 'Escapement') +
  theme_rk() +
  theme(legend.position = 'none')

w_diff
#ggsave('./figures/website/wild_diff.png', w_diff, width = 11, height = 8.5)

# 5-year average
idfg_chn_data %>%
  filter(species == 'Chinook',
         run_cmb == 'Sp/sm',
         origin == 'Wild',
         group == 'Adult') %>%
  filter(Year >= 2019) %>%
  summarize(n = n(),
            avg = mean(IDFG_total))


# combine
spsm_plots <- t_phos / (w | h) + plot_annotation(
  title = 'Spring/summer Chinook Salmon at Lower Granite Dam'
)

spsm_plots
ggsave(paste0('./figures/',yr,'/chinook_lgr_spsm_plots.png'), spsm_plots, width = 11, height = 8.5)



# Population Graphics
load('./data/QET_data/spsm_mod_fits.rda')

spsm_mod_fits <- spsm_mod_fits %>%
  mutate(mpg = ifelse(mpg == 'South Fork Salmon River', 'South Fork Salmon', mpg))

mpg_df <- spsm_mod_fits %>%
  select(MPG = mpg, pop) %>%
  distinct() %>%
  filter(!is.na(MPG))

spsm_mod_fits <- spsm_mod_fits %>%
  left_join(mpg_df) %>%
  mutate(mpg = MPG) %>%
  select(-MPG)

pop_targets <- readxl::read_excel('./data/mgt_targets.xlsx',
                                  sheet = 'pop_targets') %>%
  mutate(target = as.numeric(target))

spsm_pop_df <- left_join(spsm_mod_fits %>%
                           select(mpg:tsaij, states, exp_states),
                         pop_targets %>%
                           pivot_wider(names_from = CBP_goals, values_from = target)) %>%
  mutate(below_critical = ifelse(exp_states <= Critical, TRUE, FALSE),
         above_esa = ifelse(exp_states >= Low, TRUE, FALSE),
         above_recovery = ifelse(exp_states >= 'Broad Sense Recovery', TRUE, FALSE))


# Summaries

cmb <- spsm_pop_df %>%
  filter(!is.na(exp_states)) %>%
  group_by(spawningyear) %>%
  summarise(pops = n_distinct(pop),
            c = sum(below_critical),
            d = sum(above_esa),
            r = sum(above_recovery)) %>%
  mutate(p_c = c/pops,
         p_d = d/pops,
         p_r = r/pops) %>%
  ggplot(aes(x = spawningyear)) +
  geom_line(aes(y = p_c, colour = 'Below Critical Threshold'), size = 1) +
  geom_line(aes(y = p_d, colour = 'Above ESA Delisting'), size = 1) +
  geom_line(aes(y = p_r, colour = 'Above Broad Sense Recovery'), size = 1) +
  geom_point(aes(y = p_c, fill = 'Below Critical Threshold'), shape = 21, size = 4) +
  geom_point(aes(y = p_d, fill = 'Above ESA Delisting'), shape = 21, size = 4) +
  geom_point(aes(y = p_r, fill = 'Above Broad Sense Recovery'), shape = 21, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(.01, 0)), labels = scales::percent, limits = c(0,1)) +
  scale_colour_manual(values = c('darkgreen', 'navy', 'firebrick')) +
  scale_fill_manual(values = c('darkgreen', 'navy', 'firebrick')) +
  labs(title = 'Percent of Snake River spring/summer Chinook Salmon populations exceeding recovery or critical thesholds',
       x = 'Spawning Year',
       y = 'Percent',
       colour = '',
       fill = '') +
  theme_rk()

cmb
#ggsave('./figures/website/combined_theshold_plots.png', cmb, width = 11, height = 8.5)

c <- spsm_pop_df %>%
  filter(!is.na(exp_states)) %>%
  group_by(spawningyear) %>%
  summarise(pops = n_distinct(pop),
            n = sum(below_critical)) %>%
  mutate(p = n/pops) %>%
  ggplot(aes(x = spawningyear, y = p)) +
  geom_line(colour = 'firebrick') +
  geom_point(size = 2, shape = 21, colour = 'black', fill = 'firebrick') +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(subtitle = 'Percent of populations below critical threshold (50 spawners)',
       x = 'Spawning Year',
       y = '') +
  theme_rk() +
  theme(#plot.background = element_rect(colour = 'black'),
    plot.subtitle = element_text(size = 8, face = 'plain', vjust = 2, hjust = 0),
    axis.title=element_text(size=8),
    axis.text = element_text(size=8))#,
#plot.margin = unit(c(0, 0, 0, 0), "points"))

crit_pop <- spsm_pop_df %>%
  filter(!is.na(exp_states)) %>%
  group_by(pop) %>%
  summarise(t = n_distinct(spawningyear),
            n = sum(below_critical)) %>%
  mutate(p = n/t) %>%
  ggplot(aes(x = fct_reorder(pop,p), y = p)) +
  geom_point(size = 4, shape = 21, colour = 'black', fill = 'firebrick') +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  #geom_point(colour = 'grey', size = 3) +
  #facet_grid(mpg ~ ., scales = 'free', space = 'free_y') +
  coord_flip() +
  labs(title = 'Percent of return years Snake River spring/summer Chinook salmon populations \n are below the critical threshold (50 spawners)',
       x = '',
       y = '') +
  theme_rk() +
  theme(panel.grid.major.y = element_line(colour = 'grey', linetype = 2),
        axis.text.y.left = element_text(vjust = .5))

crit_plots <- crit_pop + inset_element(c, left = .4, bottom = .05, right = .95, top = .45)
crit_plots
ggsave(paste0('./figures/',yr,'/chinook_crit_plots.png'), crit_plots, width = 11, height = 8.5)

d <- spsm_pop_df %>%
  filter(!is.na(exp_states)) %>%
  group_by(spawningyear) %>%
  summarise(pops = n_distinct(pop),
            n = sum(above_esa)) %>%
  mutate(p = n/pops) %>%
  ggplot(aes(x = spawningyear, y = p)) +
  geom_line( colour = 'navy') +
  geom_point(size = 2, shape = 21, colour = 'black', fill = 'navy') +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(subtitle = 'Percent of populations above ESA delisting criteria',
       x = 'Spawning Year',
       y = '') +
  theme_rk() +
  theme(#plot.background = element_rect(colour = 'black'),
    plot.subtitle = element_text(size = 8, face = 'plain', vjust = 2, hjust = 0),
    axis.title=element_text(size=8),
    axis.text = element_text(size=8))#,
#plot.margin = unit(c(0, 0, 0, 0), "points"))

esa_pop <- spsm_pop_df %>%
  filter(!is.na(exp_states)) %>%
  group_by(pop) %>%
  summarise(t = n_distinct(spawningyear),
            n = sum(above_esa)) %>%
  mutate(p = n/t) %>%
  ggplot(aes(x = fct_reorder(pop,p), y = p)) +
  geom_line() +
  geom_point(size = 4, shape = 21, colour = 'black', fill = 'navy') +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  #facet_grid(mpg ~ ., scales = 'free', space = 'free_y') +
  coord_flip() +
  labs(title = 'Percent of return years Snake River spring/summer Chinook salmon populations \n are above the ESA delisting criteria',
       x = '',
       y = '') +
  theme_rk() +
  theme(panel.grid.major.y = element_line(colour = 'grey', linetype = 2),
        axis.text.y.left = element_text(vjust = .5))

esa_plots <- esa_pop + inset_element(d, left = .4, bottom = .05, right = .95, top = .45)
esa_plots
ggsave(paste0('./figures/',yr,'/chinook_esa_plots.png'), esa_plots, width = 11, height = 8.5)

d/c/(esa_pop + crit_pop)

r <- spsm_pop_df %>%
  filter(!is.na(exp_states)) %>%
  group_by(spawningyear) %>%
  summarise(pops = n_distinct(pop),
            n = sum(above_recovery)) %>%
  mutate(p = n/pops) %>%
  ggplot(aes(x = spawningyear, y = p)) +
  geom_line() +
  geom_point() +
  labs(x = 'Spawning Year',
       y = 'Proportion Above Broad Sense Recovery') +
  theme_rk()

status_plot <- d/c
status_plot
#ggsave('./figures/website/status_target_timeseries.png', status_plot, width = 11, height = 8.5)

#percent of time below critical and percent of time above esa
# proportion of years above esa?
# proportion of abundance above esa

prop_esa <- spsm_pop_df %>%
  mutate(p_critical = (nosaij/Critical)-1,
         p_esa = (nosaij/Low)-1) %>%
  ggplot(aes(x = spawningyear, y = p_esa, group = pop)) +
  geom_line() +
  geom_hline(yintercept = 0, size = 1, linetype = 2) +
  facet_wrap(~pop) +
  labs(x = 'Spawn Year',
       y = 'Proportion',
       title = 'Annual return as proportion of ESA Delisting criteria.') +
  theme_rk()

prop_esa
#ggsave('./figures/website/prop_esa.png', prop_esa, width = 11, height = 8.5)


# bulls eye
risk_df <-spsm_pop_df %>%
  filter(!is.na(exp_states)) %>%
  mutate(diff = exp_states - Low,
         p_diff = diff/Low,
         direct = as.factor(sign(diff))) %>%
  group_by(mpg, pop) %>%
  summarise(y_years = n_distinct(spawningyear),
            avg_p_diff = mean(p_diff, na.rm = TRUE),
            bad = sum(below_critical),
            avg_bad = bad/y_years,
            good = sum(above_esa),
            best = sum(above_recovery))

tmp <- left_join(risk_df,
                 sa_slopes %>%
                   select(pop, estimate))

tmp_plot <- tmp %>%
  ggplot(aes(x = avg_p_diff, y = estimate)) +
  ggrepel::geom_text_repel(aes(label = pop, colour = mpg)) +
  scale_color_brewer(palette = 'Set1') +
  xlim(c(-1,0))  

tmp_plot


risk_plot <- risk_df %>%
  ggplot(aes(x = avg_p_diff, y = bad)) +
  ggrepel::geom_text_repel(aes(label = pop, colour = mpg)) +
  scale_color_brewer(palette = 'Set1') +
  xlim(c(-1,0)) +
  labs(x = 'Average proportional difference from ESA delisting',
       y = 'Number of years below critical threshold') +
  theme_rk()

risk_plot
#ggsave('./figures/website/risk_plot.png', risk_plot, width = 11, height = 8.5)


diff_plot <- spsm_pop_df %>%
  filter(!is.na(exp_states)) %>%
  mutate(est = nosaij - Low,
         direct = as.factor(sign(est))) %>%
  ggplot(aes(x = spawningyear, y = est)) +
  geom_col(aes(fill = direct)) +
  scale_fill_manual(values = c('firebrick', 'navy')) +
  facet_wrap(~pop) +
  labs(title = 'Annual return abundance difference from ESA delisting thresholds for Snake River spring/summer Chinook salmon populations.',
       x = 'Spawn Year',
       y = 'Abundance Difference') +
  theme_rk() +
  theme(legend.position = 'none')

diff_plot
#ggsave('./figures/website/difference_plot.png', diff_plot, width = 11, height = 8.5)

# treemap----
library(treemap)

tree_plot <- spsm_pop_df %>%
  filter(!is.na(exp_states)) %>%
  filter(spawningyear == 2020) %>%
  #group_by(mpg, pop) %>%
  #mutate(avg_est = mean(exp_states)) %>%
  treemap(index=c('mpg', 'pop'),
          vSize="exp_states", #avg_est",
          type="index",
          palette = "Set1",                        # Select your color palette from the RColorBrewer presets or make your own.
          title="Average Snake River sp/sm Chinook salmon population size",                      # Customize your title
          fontsize.title=12,
          draw = TRUE,
fontsize.labels=c(15,12),
fontcolor.labels=c("black","white"),    # Color of labels
fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
bg.labels=c("transparent"),              # Background color of labels
align.labels=list(
  c("center", "center"), 
  c("right", "bottom")
),                                   # Where to place labels in the rectangle?
overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
inflate.labels=F                        # If true, labels are bigger when rectangle is bigger.
)

tree_plot  

# library(ggridges)
# 
# ridge_plot <- spsm_pop_df %>%
#   mutate(p_critical = (nosaij/Critical)-1,
#          p_esa = (nosaij/`ESA Delisting`)-1) %>%
#   ggplot(aes(x = p_esa, y = fct_rev(pop), fill = mpg)) +
#   geom_density_ridges(stat="binline", bins=40) +
#   scale_x_continuous(labels = scales::percent) +
#   #facet_grid(mpg ~ ., scales = 'free') +
#   labs(x = 'Percent',
#        y = '',
#        title = 'Annual return frequency as a proportion of the ESA delisting criteria.') +
#   theme_rk() +
#   theme(legend.position = 'none')
# #facet_wrap(~pop)
# 
# ggsave('./figures/website/ridge_plot.png', ridge_plot, width = 11, height = 8.5)
# 
#  ridge_plot + 
#    annotate("segment", x = -.10, xend = -1, y = Inf, yend = Inf, colour = "firebrick", size=3, alpha=0.6, arrow=arrow())  +
#    annotate("segment", x = .10, xend = 1, y = Inf, yend = Inf, colour = "navy", size=3, alpha=0.6, arrow=arrow())
# #   theme(plot.margin = unit(c(1,1,2,1), "lines")) +
# #   annotation_custom('text_high',xmin=1,xmax=1,ymin=-0.07,ymax=-0.07) + 
# #   annotation_custom('text_low',xmin=5,xmax=5,ymin=-0.07,ymax=-0.07)#+
# #   #coord_cartesian(clip="off")



# Steelhead ----

load(file='./data/IDFG_historic/idfg_std_data.rda')


# 5-year average
idfg_std_data %>%
  filter(Year >= (yr-4)) %>%
  filter(origin == 'Wild') %>%
  summarise(min_yr = min(Year),
            max_yr = max(Year),
            n_yrs = n(),
            avg = mean(IDFG_total))

# # taken from Res23-06Baum2022Wild Adult Steelhead Chinook Salmon Abundance Composition TABLE2
# new_df <- tribble(~Spawn_Year, ~dam, ~origin, ~IDFG_total,
#                   2021, 'LWG', 'Wild', 15478,
#                   2021, 'LWG', 'Hatchery', 45837, 
#                   2021, 'LWG', 'Total',  59126,                  
#                   2022, 'LWG', 'Wild', 9807,  
#                   2022, 'LWG', 'Hatchery', 34914, 
#                   2022, 'LWG', 'Total', 42586)
# 
# idfg_std_data <- bind_rows(idfg_std_data, new_df) %>%
#   filter(dam == 'LWG') %>%
#   mutate(species = 'Steelhead',
#          run_cmb = 'Summer',
#          group = 'Adult',
#          Year = Spawn_Year)
# 
# rm(new_df)

mgt_targets <- readxl::read_excel('./data/mgt_targets.xlsx',
                                  sheet = 'targets') %>%
  mutate(target = as.numeric(target))

# total graph
# idfg_std_data <- idfg_std_data %>%
#   mutate(origin = ifelse(is.na(origin), 'Total', origin),
#          group = ifelse(group == 'Total_Adults', 'Adult', group))

total_df <- idfg_std_data %>%
  # filter(species == 'Chinook',
  #        run_cmb == 'Sp/sm',
  #        group == 'Adult') %>%
  pivot_wider(names_from = origin, values_from = IDFG_total) %>%
  mutate(tot = Wild + Hatchery,
         phos = Hatchery/tot,
         origin = 'Total (Wild + Hatchery)')

stack_df <- idfg_std_data %>%
  # filter(species == 'Chinook',
  #        run_cmb == 'Sp/sm',
  #        group == 'Adult') %>%
  filter(origin != 'Total') %>%
  mutate(grp = 'Total (Wild + Hatchery)')

std_mgt_total <- mgt_targets %>%
  filter(species == 'Steelhead',
         run == 'Summer',
         grepl('Total', origin)) %>%
  mutate(origin = 'Total (Wild + Hatchery)',
         goal_label = 'Broad Sense Recovery (277,400)')


t2 <- stack_df %>%
  ggplot(aes(x = Year)) +
  geom_col(aes(y = IDFG_total, fill = origin), colour = 'black') +
  scale_fill_manual(values = c('grey60', 'grey80'), breaks = c('Wild', 'Hatchery'), labels = c('Wild', 'Hatchery')) +
  geom_hline(data = std_mgt_total, 
             aes(yintercept = target, colour = goal_label), size = 1) +
  geom_label(data = std_mgt_total, 
             aes(x = 1980, y = target, label = goal_label, colour = goal_label)) +
  scale_color_manual(values = c('darkgreen'), guide = 'none') +
  scale_x_continuous(breaks = seq(1960, 2022, 5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 350000),
                     label = scales::comma,
                     breaks = seq(0, 300000, by = 100000)#,
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
        legend.background = element_rect(fill = "white", color = "black"))# +
# ggtitle('Total Steelhead at Lower Granite Dam')

t2


# hatchery fraction
phos_line <- total_df %>%
  ggplot(aes(x = Year, y = 1-phos)) +
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

phos_line

#t_phos <- t + inset_element(phos_line, left = .5, bottom = .53, right = .95, top = .92)
t_phos <- t2 + inset_element(phos_line, left = .78, bottom = .55, right = .99, top = .95)
t_phos
# ggsave(paste0('./data/nov23/figures/std_total_', Sys.Date(), '.png'), t_phos, width = 11, height = 8.5)

# hatchery graph

std_mgt_H <- mgt_targets %>%
  filter(species == 'Steelhead',
         origin == 'Hatchery')

std_mgt_H$goal_label = c('Mitigation Index', 'Broodstock Requirement')  # do these work??

h <- idfg_std_data %>%
  filter(species == 'Steelhead',
         origin == 'Hatchery'#,
         # group == 'Adult'
  ) %>%
  #  filter(Year >= 1975) %>%
  ggplot(aes(x = Year, y =IDFG_total)) +
  geom_col(fill = 'grey80', colour = 'black') +
  geom_hline(data = std_mgt_H, 
             aes(yintercept = target, colour = goal_label), size = 1) +
  geom_label(data = std_mgt_H, 
             aes(x = c(1980,1990), y = target, label = goal_label, colour = goal_label)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 290000), label = scales::comma, breaks = c(130100, 5285)) +
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

h


# natural graph

std_mgt_W <- mgt_targets%>%
  mutate(origin = ifelse(origin == 'Wild/natural', 'Wild', origin)) %>%
  filter(species == 'Steelhead',
         run == 'Summer',
         origin == 'Wild') %>%
  filter(CBP_goals != 'Medium')

std_mgt_W$goal_label = c('Healthy and Harvestable', 'ESA Delisting Index', 'Critical')  # do these work??


w <- idfg_std_data %>%
  filter(origin == 'Wild') %>%
  #  filter(Year >= 1975) %>%
  ggplot(aes(x = Year, y =IDFG_total)) +
  geom_col(fill = 'grey60', colour = 'black') +
  #scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  geom_hline(data = std_mgt_W, 
             aes(yintercept = target, colour = goal_label), size = 1) +
  geom_label(data = std_mgt_W, 
             aes(x = 1990, y = target, label = goal_label, colour = goal_label), vjust = c(.5,.5, 0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 155000), label = scales::comma, breaks = c(147300,30800,1200)) +
  scale_color_manual(values = c('firebrick', 'navy', 'darkgreen')) +  
  facet_wrap(~origin, ncol = 2) +
  labs(#title = 'Natural-origin Spring/Summer Chinook Salmon',
    #subtitle = 'Adult escapement at Lower Granite Dam',
    x = 'Spawn Year',
    y = 'Escapement'
  ) +
  theme_rk() +
  theme(legend.position = 'none')

w


# combine
std_plots <- t_phos / (w | h) + plot_annotation(
  title = 'Summer Steelhead at Lower Granite Dam'
)

std_plots
ggsave(paste0('./figures/',yr,'/steelhead_lgr_plots.png'), std_plots, width = 11, height = 8.5)
