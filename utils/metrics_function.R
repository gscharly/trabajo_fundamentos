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


# Funciones para dibujar matriz de confusión y curva ROC
#Input necesario es una confusionMatrix de caret
ggplotConfusionMatrix <- function(m){
  mytitle <- paste("Accuracy", percent_format()(m$overall[1]))
  p <-
    ggplot(data = as.data.frame(m$table) ,
           aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    theme(legend.position = "none") +
    ggtitle(mytitle)
  return(p)
}

# Input necesario es un pROC object
ggplotROCCurve <- function(roc_object, interval = 0.2, breaks = seq(0, 1, interval)){
  require(pROC)
  if(class(roc) != "roc")
    simpleError("Please provide roc object from pROC package")
  ggroc(roc_object) + 
    scale_x_reverse(name = "Specificity",limits = c(1,0), breaks = breaks, expand = c(0.001,0.001)) + 
    scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = breaks, expand = c(0.001, 0.001)) +
    geom_segment(aes(x = 0, y = 1, xend = 1,yend = 0), alpha = 0.01) + 
    theme_bw() + 
    theme(axis.ticks = element_line(color = "grey80")) +
    coord_equal() + 
    annotate("text",x=-Inf,y=-Inf, label = paste("AUC =",sprintf("%.3f",roc_object$auc)), hjust= 1.2, vjust = -7)
}

perform_knn <- function(train, test, label, k, cols) {
  predictions <- knn(train[,cols], test[,cols], k=k, cl=train[,label])
  test_metrics <- metrics_function(predictions, test, label, bool=TRUE)
  return(test_metrics)
  
}

plot_acc_f1_k <- function(train, label, cols){
  long = 15
  accuracy = rep(0,long)
  f1score = rep(0,long)
  recall = rep(0,long)
  precision = rep(0,long)
  for (i in 1:long)
  {
    prediccion_knn_cv =knn.cv(train[,cols], 
                              k=i, cl=train[,label])
    accuracy[i] = sum(prediccion_knn_cv == train[,label]) /nrow(train)
    recall[i] = sum(prediccion_knn_cv == train[,label] & train[,label] == TRUE) / sum(train[,label] == TRUE)
    precision[i] = sum(prediccion_knn_cv == train[,label] & prediccion_knn_cv == TRUE) / sum(prediccion_knn_cv == TRUE)
    f1score[i] = 2*precision[i]*recall[i]/(precision[i]+recall[i])
  }
  resultados_knn = as.data.frame(cbind(accuracy,f1score,precision,recall))
  resultados_knn = resultados_knn %>% mutate(index=as.factor(seq(1:long)))
  
  max(resultados_knn$f1score)
  which.max(resultados_knn$f1score)
  
  
  p1 <- ggplot(data=resultados_knn,aes(x=index,y=accuracy)) + 
    geom_col(colour="cyan4",fill="cyan3")+
    ggtitle("Accuracy")
  
  
  p2 <- ggplot(data=resultados_knn,aes(x=index,y=f1score)) + 
    geom_col(colour="orange4",fill="orange3") +
    ggtitle("F1_score values")
  
  plot_grid(p1, p2, rel_heights = c(1/2, 1/2))
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

