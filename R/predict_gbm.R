predict_gbm <- function(df, predictorsNames, object = modelObject_list, type='prob') {
  # Predict using Stochastic Gradient Boosting
  predict(object=object, df[,predictorsNames], type= type) %>%
    mutate(health_score = cut(Churn, breaks = 5
                              , include.lowest = TRUE
                              , right = TRUE
                              , labels = 5:1
                              , ordered.result = TRUE)) %>%
    mutate(health_score_bin = cut(Churn, breaks = 5
                                  , include.lowest = TRUE
                                  , right = TRUE
                                  , labels = NULL
                                  , ordered.result = TRUE)) %>%
    cbind(df %>% tibble::rownames_to_column('crm_account_id') %>%
            select(crm_account_id)) %>%
    dplyr::mutate(prob_60 = dplyr::case_when(Not_Churn > .60 ~ "Not_Churn",
                                             TRUE ~ "Churn")) %>%
    dplyr::mutate(prob_65 = dplyr::case_when(Not_Churn > .65 ~ "Not_Churn",
                                             TRUE ~ "Churn")) %>%
    dplyr::mutate(prob_70 = dplyr::case_when(Not_Churn > .70 ~ "Not_Churn",
                                             TRUE ~ "Churn")) %>%
    dplyr::mutate(prob_75 = dplyr::case_when(Not_Churn > .75 ~ "Not_Churn",
                                             TRUE ~ "Churn")) %>%
    dplyr::mutate(prob_80 = dplyr::case_when(Not_Churn > .80 ~ "Not_Churn",
                                             TRUE ~ "Churn")) %>%
    dplyr::mutate(prob_85 = dplyr::case_when(Not_Churn > .85 ~ "Not_Churn",
                                             TRUE ~ "Churn")) %>%
    gather(key = prob, value = pred
           , -c(crm_account_id
                # , crm_mrr_band_pd0
                , health_score
                , health_score_bin
                , Churn
                , Not_Churn)) %>%
    mutate(pred = factor(pred))
}
