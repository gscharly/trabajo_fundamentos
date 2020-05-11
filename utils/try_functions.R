# Función que intenta leer un objeto CV de tidymodels. Si no existe, entrena.
try_cv_train <- function(path, folds, grid, params){
  out <- tryCatch({
    model_search = readRDS(path)
  },
  error=function(cond){
    message(cond)
    model_search <- tune_grid(rf_workflow, grid = grid, resamples=folds, param_info = params)
    saveRDS(model_search, path)
    return(model_search)
  }
  )
  return(out)
}

try_bayes_cv_train <- function(path, wkflow, bayes_folds, params){
  out <- tryCatch({
    model_search = readRDS(path)
  },
  error=function(cond){
    message(cond)
    model_search <- tune_bayes(wkflow,
                               resamples = bayes_folds,
                               param_info = params,
                               initial = 5,
                               iter = 10,
                               metrics = metric_set(roc_auc),
                               control = control_bayes(no_improve = 15, verbose = FALSE))
    saveRDS(model_search, path)
    return(model_search)
  }
  )
  return(out)
}

