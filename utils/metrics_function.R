# Función que calcula distintas métricas de clasificación
metrics_function <- function(prediccion, test, label, bool){
  
  if (bool==TRUE) {
    val0 = FALSE
    val1 = TRUE
    positive = "TRUE"
  }
  else{
    val0 = 0
    val1 = 1
    positive = 1
  }
  
  
  # Medidas de precisión
  
  accuracy = sum(prediccion == test[,label]) /nrow(test)
  error = 1-accuracy
  
  # Acierto sobre el total de las casas CARAS, sensitivity o recall
  sensitivity = sum(prediccion == test[,label] & test[,label] == val1) / sum(test[,label] == val1)
  recall = sensitivity
  
  # Acierto sobre el total de las casas BARATAS
  specificity =  sum(prediccion == test[,label] & test[,label] == val0) / sum(test[,label] == val0)
  
  # Acierto cuando el predicho es CARO
  precision = sum(prediccion == test[,label] & prediccion == val1) / sum(prediccion == val1)
  
  # Acierto cuando el predicho es BARATO
  npv = sum(prediccion == test[,label] & prediccion == val0) / sum(prediccion == val0)
  
  # F1_score
  f1score = 2*precision*recall /(precision+recall)
  conf_mat <- caret::confusionMatrix(table(prediccion, test[,label]), positive=positive)
  metrics <- c(accuracy = accuracy, error = error, sensitivity = sensitivity, specificity = specificity, precision = precision, npv = npv, f1=f1score)
  return(list(metrics, conf_mat))
  
}

# Función para el cáculo de las métricas
#Input necedario son las predicciones del modelo, el conjunto de datos y el nombre de la columna a predecir
metrics_function_num <- function(preds, test, label){
  accuracy = sum(preds == test[,label]) / nrow(test)
  recall = sum(preds == test[,label] & test[,label] == 1) / sum(test[,label] == 1)
  precision = sum(preds == test[,label] & test[,label] == 1) / sum(preds == 1)
  f1_score = 2*recall*precision/(recall+precision)
  specificity = sum(preds == test[,label] & test[,label] == 0) / sum(test[,label] == 0)
  metrics <- c(accuracy = accuracy, recall = recall, specificity = specificity, precision = precision, f1=f1_score)
  cm <- caret::confusionMatrix(preds, test[,label], positive = '1')
  return(list(metrics, cm))
}



perform_knn <- function(train, test, label, k, cols) {
  predictions <- knn(train[,cols], test[,cols], k=k, cl=train[,label])
  test_metrics <- metrics_function(predictions, test, label, bool=TRUE)
  return(test_metrics)
  
}


dt_metrics <- function(df, features, label){
  df_f <- df[c(features, label)]
  # Hay que factorizar el label para que se construya bien el árbol
  df_f[,label] <- as.factor(df[,label])
  dt_form <- as.formula(paste(paste(label), " ~ ", paste(features, collapse = " + "), sep = ""))
  dt = rpart(dt_form, data = df_f)
  rpart.plot(dt, type=4, fallen.leaves = FALSE, tweak =1.75)
  preds <- predict(dt, type = "class")
  table(pred = preds, obs = df[,label])
  metrics <- metrics_function(preds, df_f, 'price_label_high', bool=TRUE)
  return(metrics)
}

