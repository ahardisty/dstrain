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

specificity <- function(actual, prediction, positive = 'high'){
  # When it's actually not high impact, how often does it predict not high impact?
  return(sum(prediction != positive & actual != positive) / sum(actual != positive))
}


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
