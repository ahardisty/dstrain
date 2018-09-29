make_cols <- function(x, y, fun){
  # function to quickly paste values to make variables out of two lists
  # written to utilize the map2 function
  #
  # Args:
  #   x: vector
  #   y: list
  #   fun: function to apply over the two lists
  # Returns:
  #   a flattened list elements from two lists combined
  x1 <- (x)
  y1 <- list(y)
  x3 <- map2(x1, y1, fun)
  flatten_chr(x3)
}
