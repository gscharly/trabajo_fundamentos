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

perform_knn_cv <- function(train, label, k, cols){
  predictions = knn.cv(train[,cols], k=k, cl=train[,label])
  train_metrics <- metrics_function(predictions, train, label, bool=TRUE)
}


dt_metrics <- function(df, features, label){
  df_f <- df[c(features, label)]
  # Hay que factorizar el label para que se construya bien el árbol
  df_f[,label] <- as.factor(df[,label])
  dt_form <- as.formula(paste(paste(label), " ~ ", paste(features, collapse = " + "), sep = ""))
  dt <- rpart(dt_form, data = df_f)
  rpart.plot(dt, type=4, fallen.leaves = FALSE, tweak =1.75)
  preds <- predict(dt, type = "class")
  table(pred = preds, obs = df[,label])
  metrics <- metrics_function(preds, df_f, label, bool=TRUE)
  return(list("dt"=dt,"metrics"= metrics))
}

opt_f1_function <- function(prob_values, df, label, x_lim=0.25){
  beta <- 1
  if (class(prob_values) == "matrix"){
    pred_dt <- prediction(prob_values[,2], df[, label])
  }
  else {
    pred_dt <- prediction(prob_values, df[, label])
  }
  
  perf_dt <- performance(pred_dt, "prec", "rec")
  
  f1_grid <- (1+beta^2)*perf_dt@y.values[[1]]*perf_dt@x.values[[1]]/(beta^2*perf_dt@y.values[[1]]+perf_dt@x.values[[1]]) # Se calcula la f1 score para todos los posibles thresholds
  
  df_f1 <- data.frame(perf_dt@alpha.values, f1_grid, perf_dt@x.values, perf_dt@y.values)
  colnames(df_f1) <- c("th", "f1", "recall", "precision")
  df_f1 <- df_f1 %>% drop_na()
  p1 <- df_f1 %>% ggplot(aes(x=precision, y=recall)) + geom_line() + xlim(x_lim, 1) + labs(title='Precision vs recall') + xlab("Precision") + ylab("Recall")
  
  optimo <- which.max(f1_grid) # Mejor f1 score para este modelo
  prec_opt=perf_dt@y.values[[1]][optimo]
  rec_opt=perf_dt@x.values[[1]][optimo]
  f1_measure_opt <- (1+beta^2)*prec_opt*rec_opt/(beta^2*prec_opt+rec_opt)
  threshold_optimo <- perf_dt@alpha.values[[1]][optimo]
  #print(c(f1_opt= f1_measure_opt, precision = prec_opt, recall = rec_opt, threshold = threshold_optimo))
  
  # Plot f1, recall, precision
  df_f1_long <- gather(df_f1, key="metric", value = "value", f1, precision, recall)
  p2 <- df_f1_long %>% ggplot(aes(x=th, y=value, colour=metric)) + geom_line() + geom_vline(xintercept=threshold_optimo, linetype='dashed', color=palette34[4]) + labs(title='F1 score, recall y precision (clase 1) en función del umbral') + theme(legend.position = "right") + scale_color_manual(values=palette34)
  
  
  return(list(f1_opt= f1_measure_opt, precision = prec_opt, recall = rec_opt, threshold = threshold_optimo, p1=p1, p2=p2))
}

opt_f1_function_v2 <- function(prob_values, df, label, x_lim=0.25){
  beta <- 1
  if (class(prob_values) == "matrix"){
    prob_values = prob_values[,2]
    pred_dt <- prediction(prob_values, df[, label])
  }
  else {
    pred_dt <- prediction(prob_values, df[, label])
  }
  
  perf_dt <- performance(pred_dt, "prec", "rec")
  
  f1_grid <- (1+beta^2)*perf_dt@y.values[[1]]*perf_dt@x.values[[1]]/(beta^2*perf_dt@y.values[[1]]+perf_dt@x.values[[1]]) # Se calcula la f1 score para todos los posibles thresholds
  
  pr <- PRROC::pr.curve(scores.class0 = prob_values[df$price_label_high == 1], scores.class1 = prob_values[df$price_label_high == 0], curve = T)
  p1 <- data.frame(pr$curve) %>% ggplot(aes(x = X1, y = X2)) +
           geom_line() + labs(x="Recall",y="Precision", title=paste("AUC=", format(pr$auc.integral,digits=3))) +
             theme_bw() + scale_color_manual(values=palette34) +
               annotate("text",label = paste("AUC =",sprintf("%.3f",pr$auc.integral)), hjust= 10, vjust = -5)
  
  df_f1 <- data.frame(perf_dt@alpha.values, f1_grid, perf_dt@x.values, perf_dt@y.values)
  colnames(df_f1) <- c("th", "f1", "recall", "precision")
  df_f1 <- df_f1 %>% drop_na()
  
  optimo <- which.max(f1_grid) # Mejor f1 score para este modelo
  prec_opt=perf_dt@y.values[[1]][optimo]
  rec_opt=perf_dt@x.values[[1]][optimo]
  f1_measure_opt <- (1+beta^2)*prec_opt*rec_opt/(beta^2*prec_opt+rec_opt)
  threshold_optimo <- perf_dt@alpha.values[[1]][optimo]
  #print(c(f1_opt= f1_measure_opt, precision = prec_opt, recall = rec_opt, threshold = threshold_optimo))
  
  # Plot f1, recall, precision
  df_f1_long <- gather(df_f1, key="metric", value = "value", f1, precision, recall)
  p2 <- df_f1_long %>% ggplot(aes(x=th, y=value, colour=metric)) + geom_line() + geom_vline(xintercept=threshold_optimo, linetype='dashed', color=palette34[4]) + labs(title='F1 score, recall y precision (clase 1) en función del umbral') + theme(legend.position = "right") + scale_color_manual(values=palette34)
  
  
  return(list(f1_opt= f1_measure_opt, precision = prec_opt, recall = rec_opt, threshold = threshold_optimo, p1=p1, p2=p2))
}

val_metrics <- function(model, df, features, label, type='prob'){
  df_f <- df[c(features, label)]
  preds <- predict(model, df_f, type = "class")
  table(pred = preds, obs = df[,label])
  metrics <- metrics_function(preds, df_f, label, bool=TRUE)
  prob_values <- predict(model, df_f, type=type)
  roc_dt <- roc(df$price_label_high, prob_values[,2])
  roc_plot <- ggplotROCCurve(roc_dt)
  opt_f1 <- opt_f1_function_v2(prob_values, df, 'price_label_high')
  return(list(metrics=metrics, roc_plot=roc_plot, opt_f1=opt_f1))
}


