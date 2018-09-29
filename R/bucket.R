bucket <- function(x, type){
  # Break up billing differences into discrete buckets
  #
  # Args:
  #   x: the column/variable which is a continuous variable
  #   type: whether the column is in absolute ($) or relative terms (%)
  #a
  # Returns:
  #   A column with discrete values instead of continous values

  if(type == 'percent'){
    return(cut(x,
               breaks = c(min(x), .10, .20, .30, max(x)),
               labels = c('<10%','10% - 20%','20% - 30%','>30%')))
  }else if(type == 'absolute'){
    return(cut(x,
               breaks = c(min(x), 10 ,20, 30, max(x)),
               labels = c('<$10', '$10 - $20','$20 - $30', '>$30')))
  }
}
