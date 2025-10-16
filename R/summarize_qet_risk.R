# Purpose: Summarize QET metrics
# Ryan N. Kinzer
# Created: 10/1/2025

summarize_qet_risk <- function(df, label, yr) {
  
  horizon_year <- yr + 5

  per_pop <- df %>%
    group_by(pop) %>%
    arrange(spawningyear, .by_group = TRUE) %>%
    mutate(
      below50      = estimate < 50,

      roll4_below50 = slider::slide_int(below50, sum, .before = 3, .complete = TRUE),

      qet_at_year   = if_else(spawningyear == yr & roll4_below50 == 4, TRUE, FALSE, missing = FALSE)
    ) %>%
    summarise(

      .groups = "drop",
      n_pops = n_distinct(pop),
      # below MAT
      below_mat = any(spawningyear == yr & estimate < `Minimum Viable Abundance`, na.rm = TRUE),
      # currently below 50
      below_50 = any(spawningyear == yr & estimate < 50, na.rm = TRUE),
      # currently meeting QET
      qet = any(qet_at_year, na.rm = TRUE),
      # predicted to be <=50 by prediction horizon
      pred_below_50 = any(spawningyear > yr & spawningyear <= horizon_year & estimate < 50, na.rm = TRUE)
    ) %>%
    summarise(
      group = label,
      n_pops = sum(n_pops),
      
      n_below_mat = sum(below_mat),
      pct_below_mat = n_below_mat / n_pops,
      
      n_below_50 = sum(below_50),
      pct_below_50 = n_below_50 / n_pops,
      
      n_qet = sum(qet),
      pct_qet = n_qet / n_pops,
      
      n_pred_below_50 = sum(pred_below_50),
      pct_pred_below_50 = n_pred_below_50 / n_pops,
      
      .groups = "drop"
    )
  
  per_pop
}

pop_status <- function(df, yr){
  
  last_5 <- yr - 4
  
  mpg_levels <- c('Lower Snake',
                  'Dry Clearwater',
                  'Wet Clearwater',
                  'Grande Ronde / Imnaha',
                  'South Fork Salmon River',
                  'Middle Fork Salmon River',
                  'Upper Salmon River')
  
  qet_levels <- c(
                  'Above Healthy and Harvestable',
                  'Above Minimum Viable Abundance (MAT)',
                  'Critical Abundance (Below MAT)',
                  'Quasi-Extinction (<50 spawners)',
                  'Extinct'
                  )
  
  df <- df %>%
    filter(between(spawningyear,last_5,yr)) %>%
    mutate(pop_hat = ifelse(hatchery_program, paste0(pop,"*"), as.character(pop)),
           mpg = factor(mpg, levels = mpg_levels),
           abund_cat = factor(abund_cat, levels = qet_levels))

  pops <- df %>%
    filter(estimate <= 50) %>%
    distinct(pop) %>%
    pull()
  
  col_map <- c(
    'Above Healthy and Harvestable'           = '#99d594',
    'Above Minimum Viable Abundance (MAT)'    = '#3288bd',
    'Critical Abundance (Below MAT)'          = '#f3be2a',
    'Quasi-Extinction (<50 spawners)'         = '#fc8d59',
    'Extinct'                                 = '#E31A1C'
  )
  
  fig <- df %>%
    ggplot(aes(x = spawningyear, y = pop_hat, fill = abund_cat)) +
    geom_tile(colour = 'black') +
    facet_grid(mpg~., scales = 'free_y', space = 'free') +
    #facet_wrap(~mpg, scales = 'free_y', ncol = 1) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +
    scale_fill_manual(values = col_map,
                      #limits = qet_levels,
                      drop = TRUE) +
    guides(fill = guide_legend(nrow = 1)) +
    theme_bw() +
    theme(legend.position = 'bottom',
          strip.text.y = element_text(angle = 0)) +
    labs(x = 'Spawn Year',
         y = '',
         fill = '')

  fig
  return(list(pops, fig))
  
}
