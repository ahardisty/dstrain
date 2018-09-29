get_impact <- function(diff_pct, diff, care_ind){
  # Determine the impact associated with difference in bills
  #
  # Args:
  #   diff_pct: Difference in bills expressed as column of percents
  #   diff: Difference in bills expressed as absoolute dollar value
  #
  # Returns:
  #   A column with high, medium, low, or saver depending on the bill impact

  x <- 'Not Sure'

  x <- ifelse((care_ind == 'Y' & (diff_pct < .10 & diff < 10)), 'low', x)

  x <- ifelse((care_ind == 'Y' & (diff_pct > .10 | diff > 10)), 'medium', x)

  x <- ifelse((care_ind == 'Y' & (diff_pct > .10 & diff > 10)), 'high', x)

  x <- ifelse((care_ind == 'N' & (diff_pct < .10 & diff < 20)), 'low', x)

  x <- ifelse((care_ind == 'N' & (diff_pct > .10 | diff > 20)), 'medium', x)

  x <- ifelse((care_ind == 'N' & (diff_pct > .10 & diff > 20)), 'high', x)

  x <- ifelse(diff < 0, 'savers', x)

  x <- factor(x, levels = c('savers', 'low', 'medium', 'high'))

  return(x)
}
