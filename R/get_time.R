get_time <- function(print_message, start_time) {
  # Print out the time associated with a print message
  #
  # Args:
  #   print_message: Description of the task being timed as a string.
  #   start_time: Reference point for the timer.
  #
  # Returns:
  #   Print out of the ellapsed time along with a printed message.

  tm <- unname(((proc.time() - start_time) / 60)['elapsed'])
  print(paste(print_message, round(tm, 2), sep = ': '))
}
