class_gbm <- function(df, predictorsNames, outcomeName = 'churn_yes') {
  # Stochastic Gradient Boosting
  # Type: Regression, Classification
  #
  # Tuning parameters:
  #
  #   n.trees (# Boosting Iterations)
  #     interaction.depth (Max Tree Depth)
  #     shrinkage (Shrinkage)
  #     n.minobsinnode (Min. Terminal Node Size)
  #     Required packages: gbm, plyr
  #
  #     A model-specific variable importance metric is available
  objControl = trainControl(method='cv', number=5, returnResamp='none', summaryFunction = twoClassSummary
                            , classProbs = TRUE, savePredictions = TRUE)
  train(df[,predictorsNames], df[,outcomeName],
        # method='gbm',
        method='gbm',
        trControl=objControl,
        metric = "ROC",
        preProc = c("center", "scale"))
}
