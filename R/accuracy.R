accuracy <- function(actual, prediction){
  # How often did it predict the correct class?
  return(sum(prediction == actual) / NROW(actual))
}
