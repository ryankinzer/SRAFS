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
