set_zero_dt = function(DT) {
  # Identify NA values and set to zero
  #
  # Args:
  #   DT: data table
  #
  # Returns:
  #   data table with NA values replaced with zeros.


  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,0)
}
