# Función que intenta leer un objeto CV de tidymodels. Si no existe, entrena.
try_cv_train <- function(path, wkflow, folds, grid, params){
  out <- tryCatch({
    model_search = readRDS(path)
  },
  error=function(cond){
    message(cond)
    all_cores <- parallel::detectCores(logical = FALSE) - 2
    registerDoFuture()
    cl <- makeCluster(all_cores)
    plan(future::cluster, workers = cl)
    model_search <- tune_grid(wkflow, grid = grid, resamples=folds, param_info = params)
    stopCluster(cl)
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
    all_cores <- parallel::detectCores(logical = FALSE) - 2
    registerDoFuture()
    cl <- makeCluster(all_cores)
    plan(future::cluster, workers = cl)
    model_search <- tune_bayes(wkflow,
                               resamples = bayes_folds,
                               param_info = params,
                               initial = 5,
                               iter = 30,
                               metrics = metric_set(roc_auc),
                               control = control_bayes(no_improve = 15, verbose = FALSE))
    stopCluster(cl)
    saveRDS(model_search, path)
    return(model_search)
  }
  )
  return(out)
}

