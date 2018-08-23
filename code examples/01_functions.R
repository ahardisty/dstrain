
# PROCESS VARIABLES -------------------------------------------------------
USER <- 'A3HE' # c ange based on user running script
PWD <- 'Eat3onions!'

# PROCESS PACKAGE LIBRARY ------------------------------------------------
PACKAGES <- c('earth', 
              # 'lubridate',
              'data.table',
              'stringr',
              # 'RODBC',
              'RMySQL',
              'purrr',
              'xlsx',
              'tidyverse')

# DATA PREP FUNCTIONS ---------------------------------------------------------------

accuracy <- function(actual, prediction){
  # How often did it predict the correct class?
  return(sum(prediction == actual) / NROW(actual))
}

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

get_cdd <- function(temp, base){
  return(ifelse(temp - base > 0, round(temp - base, 1), 0))
}

get_hdd <- function(temp, base){
  return(ifelse(base - temp > 0, round(base - temp, 1), 0))
}

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

get_packages <- function(p){
  # Check for, install, and load required packages
  #
  # Args:
  #   p: individual package name as a character string.
  #
  # Returns:
  #   Returns loaded packages.
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE, lib = 'C:/Program Files/R/R-3.3.0/library' )
  library(p, character.only = TRUE, verbose = TRUE)
}


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


cleanLines <- function(x) {
  x = gsub("\t+", "", x, perl=TRUE); # remove all tabs
  x = gsub("^\\s+", "", x, perl=TRUE); # remove leading whitespace
  x = gsub("\\s+$", "", x, perl=TRUE); # remove trailing whitespace
  x = gsub("[ ]+", " ", x, perl=TRUE); # collapse multiple spaces to a single space
  x = gsub("^[--]+.*$", "", x, perl=TRUE); # destroy any comments
  x = gsub("^[/]+.*$", "", x, perl=TRUE); # destroy any comments
  return(x)}

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

precision <- function(actual, prediction, positive = 'high'){
  # When it predicts high impact, how often is it correct?
  return(sum(prediction == positive & actual == positive) / sum(prediction == positive))
}


rmse <- function(y, y_hat) {
  return(sqrt(sum((y - y_hat)^2) / length(y)))
}

r_squared <- function(y, y_hat){
  y_bar <- mean(y)
  SS_tot <- sum((y - y_bar)^2)
  SS_res <- sum((y - y_hat)^2)
  rsq <- 1 - (SS_res / SS_tot)
  return(rsq)
}

set_zero = function(DT) {
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

specificity <- function(actual, prediction, positive = 'high'){
  # When it's actually not high impact, how often does it predict not high impact?
  return(sum(prediction != positive & actual != positive) / sum(actual != positive))
}

# tier <- function(pred, base, lower, upper){
#   # Calculate the electricity usage in a given tier
#   #
#   # Args:
#   #   pred: The column of values being tiered
#   #   base: the baseline usage used for splitting out the tiers
#   #   lower: the lower limit for the tier of interest as a multiplier
#   #   upper: the upper limit for the tier of interest as a multiplier
#   #
#   # Returns:
#   #   The electricity usage in a given tier as a column of numeric values
#   
#   return(ifelse((pred < base * upper) & (pred > base * lower), 
#                 pred - base * lower,
#                 ifelse(pred >= upper * base,
#                        (upper - lower) * base, 
#                        0)))
# }

# true_positive <- function(actual, prediction, positive = 'high'){
#   # When it's actually high impact, how often does it predict high impact?
#   return(sum(prediction == positive & actual == positive) / sum(actual == positive))
# }

# TERADATA MODELING FUNCTIONS ------------------------------------------------------

EstimateModel <- function(dataList, rt_sched_cd = 'HETOUA') {
  # change this function only 
  # MAKE CHANGES ONLY TO THE FUNCTION IN functions.R 
  # print(paste('str(dataList): ',str(dataList)))
  
  # Convert the input list to a data frame
  chunkDF <- data.frame(dataList)
  
  # print(paste('head of dataList is', head(dataList)))
  # print(paste('head of chunkDF is', head(chunkDF)))
  # 
  # print(paste('dataList structure is', str(dataList)))
  # print(paste('chunkDF structure is', str(chunkDF)))
  # 
  # Estimate a MARS model on this chunk
  model <- earth(Y ~ X, data = chunkDF, nprune = 3)
  
  # print(paste('model element is', model))
  
  # Estimate a lm model on this chunk
  # model <- lm(Y ~ X, data = chunkDF)
  print(paste('model element is', str(model)))
  #print(paste('model qr nchar is', nchar(model$qr)))
  
  # Print statements can be useful for debugging, the .rx* vars are Revo specific
  print(' ----------')
  # print(paste('train_indicator: ',chunkDF[1,]$train_indicator))
  #print(paste('customer: ', chunkDF[1,]$customer))
  # print(paste('customer: ', chunkDF$customer))
  # print(paste('prem: ', chunkDF[1,]$prem_id))
  # print(paste('acct: ', chunkDF[1,]$acct_id))
  # print(paste('period: ', chunkDF[1,]$tou_cd))
  # print(paste('period: ', chunkDF$tou_cd))
  
  print(paste('this is chunkSize and nchar should be right around here: ',dim(chunkDF)))
  # print(paste('the summary of chunkDF is', summary(chunkDF)))
  
  
  # Remove unneeded parts of MARS model object, this is only to keep what gets stored in Teradata smaller
  model$residuals <- NULL
  model$fitted.values <- NULL
  model$leverages <- NULL
  model$bx <- NULL
  attr(model$terms, ".Environment") <- NULL
  # 
  # Remove unneeded parts of LM model object, this is only to keep what gets stored in Teradata smaller
  # print(paste('nchar(model):', nchar(model$qr[1])))
  # # print(paste('model length before remove is', nchar(modelString)))
  #  model$residuals <- NULL
  # # print(paste('model -residuals length is', nchar(modelString)))
  #  model$effects <- NULL
  # # print(paste('model -effects length is', nchar(modelString)))
  #  model$fitted.values <- NULL
  # # print(paste('model -fitted length is', nchar(modelString)))
  #  model$assign <- NULL
  # # print(paste('model -assign length is', nchar(modelString)))
  #  model$qr[[1]] <- NULL
  # # print(paste('model -qr length is', nchar(modelString)))
  #  attr(model$terms, ".Environment") <- NULL
  # # print(paste('model -terms length is', nchar(modelString)))
  
  # Convert the model to a character string
  modelString <- rawToChar(serialize(model, connect=NULL, ascii=TRUE))
  print(paste('model length is', nchar(modelString)))
  
  modelString <- paste( modelString,
                        paste(rep("X", 5000 - nchar(modelString)), collapse = ""),
                        sep="")
  
  resultDF <- data.frame(
    # train_indicator = chunkDF[1,]$train_indicator, # since we're doing a group by we're assuming all fields are the same in a chunk
    customer = chunkDF[1,]$customer,
    # acct = chunkDF[1,]$acct_id,
    rt_sched_cd = rt_sched_cd,
    tou_cd = chunkDF[1,]$tou_cd,
    model_id = chunkDF[1,]$model_id,
    model_id_sm = chunkDF[1,]$model_id_sm,
    model_date = format(Sys.Date(), '%Y-%m-%d'),
    MODEL = modelString )
  # MODEL = 'THIS IS A MODEL')
  return( resultDF )
  
  # print(paste('the summary of resultDF is', summary(resultDF)))  
  
}

ScoreChunk <- function(dataList)
{
  # MAKE CHANGES ONLY TO THE FUNCTION IN functions.R 
  chunkDF <- as.data.frame(dataList)
  
  #DEBUG HELPER
  print(' ----------') 
  print(paste('Colnames:',paste(colnames(chunkDF),collapse=',')))
  # print(paste('train_indicator: ',chunkDF[1,]$train_indicator))
  print(paste('customer: ', chunkDF[1,]$customer))
  print(paste('tou_cd: ', chunkDF[1,]$tou_cd))
  print(paste('chunkSize: ',nrow(chunkDF)))
  print(head(chunkDF))
  
  # Extract the model string for the first observation
  # and convert back to model object
  # All observations in this chunk have the same partition criteria,
  # so they all share the same model
  modelString <- as.character(chunkDF[1,]$MODEL)
  model <- unserialize( charToRaw( modelString ) )
  print(paste('MODEL:',model[1:2]))
  
  resultDF <- data.frame(
    # train_indicator = chunkDF[1,]$train_indicator, # since we're doing a group by we're assuming all fields are the same in a chunk
    usg_dt = chunkDF$usg_dt,
    customer = chunkDF[1,]$customer,
    tou_cd = chunkDF[1,]$tou_cd,
    model_date = chunkDF[1,]$model_date,
    pred_date = format(Sys.Date(), '%b%Y'),
    PREDICTED_VAL = predict(model, chunkDF)[,1])
  
  return( resultDF )
}

ScoreChunkTEST <- function(dataList)
{
  # MAKE CHANGES ONLY TO THE FUNCTION IN functions.R 
  chunkDF <- as.data.frame(dataList)
  
  #DEBUG HELPER
  print(' ----------') 
  print(paste('Colnames:',paste(colnames(chunkDF),collapse=',')))
  # print(paste('train_indicator: ',chunkDF[1,]$train_indicator))
  print(paste('customer: ', chunkDF[1,]$customer))
  print(paste('tou_cd: ', chunkDF[1,]$tou_cd))
  print(paste('chunkSize: ',nrow(chunkDF)))
  print(head(chunkDF))
  
  # Extract the model string for the first observation
  # and convert back to model object
  # All observations in this chunk have the same partition criteria,
  # so they all share the same model
  modelString <- as.character(chunkDF[1,]$MODEL)
  model <- unserialize( charToRaw( modelString ) )
  print(paste('MODEL:',model[1:2]))
  
  resultDF <- data.frame(
    # train_indicator = chunkDF[1,]$train_indicator, # since we're doing a group by we're assuming all fields are the same in a chunk
    usg_dt = chunkDF$usg_dt,
    customer = chunkDF[1,]$customer,
    rt_sched_cd = chunkDF[1,]$rt_sched_cd,
    tou_cd = chunkDF[1,]$tou_cd,
    model_date = chunkDF[1,]$model_date,
    pred_date = format(Sys.Date(), '%Y-%m-%d'),
    PRED_VAL_temp_normal = predict(model, chunkDF$X)[,1],
    # PRED_VAL_temp_high = predict(model, chunkDF$X+chunkDF$X_sd)[,1],
    PRED_VAL_temp_high = predict(model, chunkDF$X+(.85*chunkDF$X_sd))[,1],
    # PRED_VAL_temp_low = predict(model, chunkDF$X-chunkDF$X_sd)[,1],
    PRED_VAL_temp_low = predict(model, chunkDF$X-(.85*chunkDF$X_sd))[,1],
    PRED_VAL_temp_mid_high = predict(model, chunkDF$X+(.50*chunkDF$X_sd))[,1],
    PRED_VAL_temp_mid_low = predict(model, chunkDF$X-(.50*chunkDF$X_sd))[,1]
    )
  
  return( resultDF )
}

trim_model <- function(model){
  
  model$residuals <- NULL
  model$fitted.values <- NULL
  model$leverages <- NULL
  model$bx <- NULL
  attr(model$terms, ".Environment") <- NULL
  
  return(model)
  
}
# IN MEMORY MODELING FUNCTIONS --------------------------------------------



# BILL ANALYSIS FUNCTIONS -------------------------------------------------

# LOAD PACKAGES -----------------------------------------------------------

sapply(PACKAGES, get_packages)
