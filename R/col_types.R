col_types <- function(dir, pattern, n=5, fctr_cols, sep = '', ...){
  # read in first five rows of a file to define class of variables
  #
  # Args:
  #   dir: directory of the file
  #   pat: pattern to match
  #   char_cols: columns to assign character class
  #   num_cols: columns to assign number class
  #   n: number of rows to read
  # Returns:
  #   column name and classes for increased fread speed

  read_data <- dir(path = dir, pattern = pattern, full.names = TRUE, ...)
  init_row <- read.table(read_data[[1]]
                         , na.strings = '?'
                         , header = TRUE
                         , stringsAsFactors = TRUE
                         , nrows = n
                         , check.names = FALSE
                         , sep = sep
  )
  init_row[,fctr_cols] <- sapply(init_row[,fctr_cols], as.factor)
  # init_row[,char_cols] <- sapply(init_row[,char_cols], as.character)
  # init_row[,num_cols] <- sapply(init_row[,num_cols], as.numeric)
  classes <- sapply(init_row, class)
  list('classes' = classes, files = read_data, sample = init_row, fctr_cols)
}
