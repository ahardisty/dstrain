add_model_specs <- function(object = object, model_version = MODEL_VERSION,
                            model_project = MODEL_PROJECT ){

  data.frame(model_method = object$method
             , date_scored = Sys.Date()
             , model_label = object$modelInfo$label
             , model_type = object$modelType
             , model_version = model_version
             , model_project = model_project)

}
