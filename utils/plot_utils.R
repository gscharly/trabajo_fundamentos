# Plot functions


bar_plot_target <- function(df, col, target){
  p1 <- df %>% ggplot() + geom_bar(aes(x=!!sym(col), fill=!!sym(target))) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_manual(values=palette34) + ylab("") 
  p2 <- df %>% ggplot() + geom_bar(aes(x=!!sym(col), fill=!!sym(target)), position='fill') + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_manual(values=palette34) + ylab("")
  
  # arrange the three plots in a single row
  prow <- plot_grid (p1 + theme(legend.position="none"),
                     p2 + theme(legend.position="none")
  )
  legend <- get_legend(p1 + theme(legend.direction = "horizontal"))
  
  plot_grid(prow, legend, ncol = 1, rel_heights = c(1, .07))
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

# Plot correlaciones
heatmap_corr <- function(df, numeric_cols){
  cormat <- round(cor(na.omit(df[,numeric_cols])), 2)
  
  # Get lower triangle of the correlation matrix
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  
  upper_tri <- get_upper_tri(cormat)
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "#1EA6A2", high = "#A61E22", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()
  
  ggheatmap + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
}
