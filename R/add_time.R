add_time <- function(dt){
  # add date time variables to a data table
  #
  # Args:
  #   dt: name of data table
  #   x:  date variable
  # Returns:
  #   year, month, year_day, month_day, season, day name and weekend/weekday indicator
  # dt <- copy(dt)
  dt[,`:=` (month = data.table::month(date))]
  # dt <- dt[!month %in% c(4:10)]
  # dt[, `:=` (season = ('winter')),]
  dt[,`:=` (year = data.table::year(date),
            year_day = data.table::yday(date),
            month_day = data.table::mday(date),
            month_name=month.abb[data.table::month(date)]),]
  dt[,season := ifelse(month %in% c(4:10),'summer', 'winter'),]
  dt[,season := ifelse(month %in% c(4:10),'summer', 'winter'),]
  dt
}
