pred_type_distribution <- function(df, threshold, label) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$pred >= threshold & df[,label] == 1, "TP", v)
  v <- ifelse(df$pred >= threshold & df[,label] == 0, "FP", v)
  v <- ifelse(df$pred < threshold & df[,label] == 1, "FN", v)
  v <- ifelse(df$pred < threshold & df[,label] == 0, "TN", v)
  
  df$pred_type <- v
  
  return(df)
}