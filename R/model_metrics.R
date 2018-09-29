model_metrics <- function(df, model_version = 'v1', model_project = "chi", model_label = "Stochastic Gradient Boosting"){
  # model_metrics <- function(df, model_version = 'V1', model_project = "CHI"
  #                           , obj_location = MODEL_DATA_SOURCE, pattern = MODEL_IMPORT_TYPE, full.names = T ){
  # summarize adjusted predictions
  df <- df %>%
    mutate(pred = factor(pred), obs = factor(obs))

  df_summary <- df %>%
    select(model, model_pd_index, model_pd, prob, model_train_date) %>%
    group_by(model, model_pd_index, model_pd, prob, model_train_date) %>%
    summarise()
  # gather date information from previously created model objects
  # object_info <- file.info(dir(obj_location, pattern = pattern, full.names = full.names))

  # extract specific details from adjusted predictions
  results <- caret::confusionMatrix(df$pred, df$obs)$byClass %>%
    as.tibble() %>%
    tibble::rownames_to_column("metric") %>%
    dplyr::bind_rows(caret::prSummary(df, levels(df$obs)) %>%
                       as.tibble() %>%
                       tibble::rownames_to_column("metric") %>%
                       filter(metric == 'AUC')) %>%
    dplyr::bind_rows(caret::twoClassSummary(df, levels(df$obs)) %>%
                       as.tibble() %>%
                       tibble::rownames_to_column("metric") %>%
                       filter(metric == 'ROC')) %>%
    mutate(metric = stringr::str_to_lower(metric)) %>%
    mutate(metric = stringr::str_replace_all(pattern = " ",replacement = "_", metric)) %>%
    tidyr::spread(key = metric, value = value) %>%
    mutate(
      prec_recall_ratio = precision / recall,
      model_type = df_summary$model,
      prob = df_summary$prob,
      model_pd_index = df_summary$model_pd_index,
      model_pd = df_summary$model_pd,
      model_train_date = df_summary$model_train_date,
      model_version = model_version,
      model_project = model_project,
      model_label = model_label,
      model_train_date = df_summary$model_train_date
    )
}
