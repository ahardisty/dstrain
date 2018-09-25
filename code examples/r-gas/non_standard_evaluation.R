# http://dplyr.tidyverse.org/articles/programming.html
# https://github.com/tidyverse/dplyr/blob/master/vignettes/programming.Rmd

# This vignette has two goals:
#
# Show you how to use dplyr's pronouns and quasiquotation to write reliable functions
# that reduce duplication in your data analysis code.
#
# To teach you the underlying theory including quosures, the data structure that stores
# both an expression and an environment, and tidyeval, the underlying toolkit.


greet <- function(name) {
  "How do you do, name?"
}
greet("Hadley")


# https://www.rstudio.com/resources/videos/tidy-eval-programming-with-dplyr-tidyr-and-ggplot2/

