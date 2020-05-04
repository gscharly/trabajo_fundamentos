# Script para generar datasets de train y test con transformaciones


# Lectura de datos --------------------------------------------------------

houses <- read.csv('datasets/melb_data.csv')


# Targets para regresión y clasificación ----------------------------------
# Regresión
houses <- houses %>% mutate(log_price = log10(Price))

# Clasificación
quantiles_price <- quantile(houses$Price)
houses <- houses %>% mutate(price_label = Price > median(Price), price_label_high = Price > quantiles_price[4])
houses %>% ggplot(aes(x=Price, color=price_label)) + geom_density()
houses %>% ggplot(aes(x=Price, color=price_label_high)) + geom_density()



# División de los datos ---------------------------------------------------

#set.seed(10)
#trainIndex <- createDataPartition(houses$Price, p = .7, list = FALSE, times = 3)
#housesTrain <- houses[ trainIndex,]
#housesTest <- houses[-trainIndex,]

numero_total = nrow(houses)

# Porcentajes de train, test y validation
w_train = .7
w_test = .15
w_validation = 1 - (w_train + w_test)

# Todos los índices
indices = seq(1:numero_total) 

# Muestreo
indices_train = sample(1:numero_total, numero_total * w_train)
indices_test = sample(indices[-indices_train], numero_total * w_test)
indices_validation = indices[-c(indices_train,indices_test)]

# Agrupamos

housesTrain = houses[indices_train,]
housesTest = houses[indices_test,]
housesVal = houses[indices_validation,]


# Transformaciones sobre train --------------------------------------------

# Filtros e imputaciones (YearBuilt categórica)
#housesTrainFinal <- housesTrain
housesTrain$YearBuilt[which(housesTrain$YearBuilt<1850)] <- NA
housesTrain$Landsize[which(housesTrain$Landsize == 0)] <- NA
imputationsLandsizeFinal <- housesTrain %>% select(Lattitude, Longtitude, Distance, Landsize) %>% VIM::kNN(variable='Landsize')
imputationsCarFinal <- housesTrain %>% select(Rooms, Distance, Car) %>% VIM::kNN(variable='Car') 

#Necesario para la función de construcción del dataset
housesTrain$LandsizeImp <- imputationsLandsizeFinal %>% select(Landsize) %>% unlist()
housesTrain$CarImp <- imputationsCarFinal %>% select(Car) %>% unlist()
imputationsLandsizeforTest <- housesTrain %>% select(Suburb, Address, Rooms, Type, Price, Method, SellerG, Date, Distance, Postcode, Bedroom2, Bathroom, Car, LandsizeImp, BuildingArea, YearBuilt, CouncilArea, Lattitude, Longtitude, Regionname, Propertycount, log_price, price_label, price_label_high) 
names(imputationsLandsizeforTest)[names(imputationsLandsizeforTest) == "LandsizeImp"] <- "Landsize"
imputationsCarforTest <- housesTrain %>% select(Suburb, Address, Rooms, Type, Price, Method, SellerG, Date, Distance, Postcode, Bedroom2, Bathroom, CarImp, Landsize, BuildingArea, YearBuilt, CouncilArea, Lattitude, Longtitude, Regionname, Propertycount, log_price, price_label, price_label_high) 
names(imputationsCarforTest)[names(imputationsCarforTest) == "CarImp"] <- "Car"

# Transformaciones de variables cuantitativas

sqrt_distance <- housesTrain %>% mutate(sqrt_distance = sqrt(Distance)) %>% select(sqrt_distance)
housesTrain$sqrt_distance <- unlist(sqrt_distance)
log_landsize <- imputationsLandsizeFinal %>% mutate(log_landsize=log10(Landsize)) %>% select(log_landsize)
housesTrain$log_landsize <- unlist(log_landsize)
log_price <- housesTrain %>% mutate(log_price = log10(Price)) %>% select(log_price)
housesTrain$log_price <- unlist(log_price)

# Discretización de variables cuantitativas
housesTrain$rooms_cat <- cut(housesTrain$Rooms,  breaks = c(1,3,4,10), labels = c("Pequeñas", "Medianas", "Grandes"), include.lowest = TRUE, right = FALSE)
housesTrain$year_built_cat <- factor(cut2(housesTrain$YearBuilt, g=2), labels = c("Antigua", "Moderna"))
levels(housesTrain$year_built_cat) <- c(levels(housesTrain$year_built_cat), 'Desconocido')
housesTrain <- housesTrain %>% mutate_at(vars(year_built_cat), ~replace(., is.na(.), 'Desconocido'))
# Hasta 1 baño
housesTrain$bath_cat <- factor(cut2(housesTrain$Bathroom, g=2), labels=c("Pocos_baños", "Muchos_baños"))
#De 0 a 3 dormitorios
housesTrain$bed_cat <- factor(cut2(housesTrain$Bedroom2, g=2), labels=c("Pocos_dormitorios", "Muchos_dormitorios"))
#0-1 y 2 o más
housesTrain$car_cat <-cut(imputationsCarFinal$Car, breaks = c(0,2,20), labels = c("Pocas_plazas", "Muchas_plazas"), include.lowest = TRUE, right = FALSE)

#El join solo se debe ejecutar una vez
suburbs_count <- housesTrain %>% group_by(Suburb) %>% tally() %>% arrange(desc(n))
housesTrain <- housesTrain %>% inner_join(suburbs_count, by='Suburb') %>% mutate(sell_rate_suburb = round(n/Propertycount*100,2))
# Hasta 1.47% de tasa de venta por barrio
housesTrain$sell_rate_cat <- factor(cut2(housesTrain$sell_rate_suburb, g=2), labels=c("Menos_populares", "Más_populares"))
# Vistas al mar
water_councils <- c('Wyndham', 'Hobsons Bay', 'Port Phillip', 'Bayside', 'Kingston', 'Frankston', 'Stonnington')
housesTrain$may_have_water <- factor(ifelse(housesTrain$CouncilArea %in% water_councils, TRUE, FALSE))
# Habitaciones por unidad de parcela
housesTrain <- housesTrain %>% mutate(log_room_land = log10(Rooms/LandsizeImp*1000))


# Estandarización de variables
num_vars <- c('sqrt_distance', 'log_landsize', 'Lattitude', 'Longtitude', 'log_room_land')
num_std_vars <- c('sqrt_distance_std', 'log_landsize_std', 'lattitude_std', 'longtitude_std', 'log_room_land_std')
cat_vars <- c('rooms_cat', 'year_built_cat', 'car_cat', 'Regionname', 'Type', 'Method', 'bath_cat', 'bed_cat', 'sell_rate_cat', 'may_have_water')
other_vars <- c('Suburb', 'Address', 'Rooms', 'Distance', 'Lattitude', 'Longtitude', 'SellerG', 'Date', 'Postcode', 'Bedroom2', 'Bathroom', 'Car', 'YearBuilt', 'CouncilArea')
housesTrainNum <- housesTrain %>% select(num_vars)
normParam <- preProcess(housesTrainNum)
housesTrainNumNorm <- predict(normParam, housesTrainNum)
colnames(housesTrainNumNorm) <- num_std_vars

housesTrainFinal <- data.frame(housesTrain[,other_vars], housesTrain[,c('CarImp', 'LandsizeImp')], housesTrainNumNorm[,num_std_vars], housesTrain[,cat_vars], housesTrain[, c('Price', 'log_price', 'price_label', 'price_label_high')])

housesTrainFinal$Regionname = factor(housesTrainFinal$Regionname, levels=c('Southern Metropolitan', 'Northern Metropolitan', 'Western Metropolitan', 'Eastern Metropolitan', 'South-Eastern Metropolitan', 'Eastern Victoria', 'Northern Victoria', 'Western Victoria'))
#Utilizo la librería plyr
housesTrainFinal$Regionname  = mapvalues(housesTrainFinal$Regionname, from = c('Southern Metropolitan', 'Northern Metropolitan', 'Western Metropolitan', 'Eastern Metropolitan','South-Eastern Metropolitan', 'Eastern Victoria', 'Northern Victoria', 'Western Victoria'), to = c('Southern_Metropolitan', 'Northern_Metropolitan', 'Western_Metropolitan', 'Eastern_Metropolitan','South_Eastern_Metropolitan', 'Eastern_Victoria', 'Northern_Victoria', 'Western_Victoria'))

housesTrainFinal$Method = factor(housesTrainFinal$Method, levels=c('S', 'SP', 'PI', 'VB', 'SA'))
housesTrainFinal$Type = factor(housesTrainFinal$Type, levels=c('h', 'u', 't'))
# Se elimina la variable Car
housesTrainFinal <- housesTrainFinal %>% select(-Car)



# Transformaciones sobre test -------------------------------------------------------

#Función que preprocesa y limpia un dataset dado 
final_dataset_construction_year <- function(dataset, standarizer, imputationsCarforTest, imputationsLandsizeforTest, imputationsYearforTest){
  
  #Preprocesado 
  dataset$YearBuilt[which(dataset$YearBuilt<1850)] <- NA
  dataset$Landsize[which(dataset$Landsize == 0)] <- NA
  datasetCarNARows <- dataset %>% filter(is.na(Car)) %>% nrow()
  datasetCarNA <- dataset %>% filter(is.na(Car))
  datasetLandsizeNARows <- dataset %>% filter(is.na(Landsize)) %>% nrow()
  datasetLandsizeNA <- dataset %>% filter(is.na(Landsize)) 
  datasetYearBuiltNA <- dataset %>% filter(is.na(YearBuilt))
  datasetYearBuiltNARows <- dataset %>% filter(is.na(YearBuilt)) %>% nrow()
  
  #Imputación para Car
  imputationsCarforTest <- rbind(imputationsCarforTest,datasetCarNA) 
  imputationsCarFinal <- imputationsCarforTest  %>% select(Rooms, Car) %>% VIM::kNN(variable='Car')
  imputationsCarFinal <- tail(imputationsCarFinal,datasetCarNARows)
  datasetCarNA$Car <- imputationsCarFinal %>% select(Car) %>% unlist()
  dataset <- dataset %>% drop_na(Car)
  dataset <- rbind(dataset,datasetCarNA)
  
  #Imputación para Landsize
  imputationsLandsizeforTest <- rbind(imputationsLandsizeforTest,datasetLandsizeNA) 
  imputationsLandsizeFinal <- imputationsLandsizeforTest  %>% select(Lattitude, Longtitude, Distance, Landsize) %>% VIM::kNN(variable='Landsize')
  imputationsLandsizeFinal <- tail(imputationsLandsizeFinal,datasetLandsizeNARows)
  datasetLandsizeNA$Landsize <- imputationsLandsizeFinal %>% select(Landsize) %>% unlist()
  dataset <- dataset %>% drop_na(Landsize)
  dataset <- rbind(dataset,datasetLandsizeNA)
  
  
  #Transformación de variables
  sqrt_distance <- dataset %>% mutate(sqrt_distance = sqrt(Distance)) %>% select(sqrt_distance)
  dataset$sqrt_distance <- unlist(sqrt_distance)
  dataset <- dataset %>% mutate(log_landsize=log10(Landsize))
  log_price <- dataset %>% mutate(log_price = log10(Price)) %>% select(log_price)
  dataset$log_price <- unlist(log_price)
  #El join solo se debe ejecutar una vez
  suburbs_count <- dataset %>% group_by(Suburb) %>% tally() %>% arrange(desc(n))
  dataset <- dataset %>% inner_join(suburbs_count, by='Suburb') %>% mutate(sell_rate_suburb = round(n/Propertycount*100,2))
  # Hasta 1.47% de tasa de venta por barrio
  dataset$sell_rate_cat <- factor(cut2(dataset$sell_rate_suburb, g=2), labels=c("Menos_populares", "Más_populares"))
  # Vistas al mar
  water_councils <- c('Wyndham', 'Hobsons Bay', 'Port Phillip', 'Bayside', 'Kingston', 'Frankston', 'Stonnington')
  dataset$may_have_water <- factor(ifelse(dataset$CouncilArea %in% water_councils, TRUE, FALSE))
  # Habitaciones por unidad de parcela
  dataset <- dataset %>% mutate(log_room_land = log10(Rooms/Landsize*1000))
  
  
  # Discretización de variables cuantitativas
  dataset$rooms_cat <- cut(dataset$Rooms,  breaks = c(1,3,4,10), labels = c("Pequeñas", "Medianas", "Grandes"), include.lowest = TRUE, right = FALSE)
  dataset$year_built_cat <- factor(cut2(dataset$YearBuilt, g=2), labels = c("Antigua", "Moderna"))
  levels(dataset$year_built_cat) <- c(levels(dataset$year_built_cat), 'Desconocido')
  dataset <- dataset %>% mutate_at(vars(year_built_cat), ~replace(., is.na(.), 'Desconocido'))
  dataset$car_cat <-cut(dataset$Car, breaks = c(0,2,20), labels = c("Pocas_plazas", "Muchas_plazas"), include.lowest = TRUE, right = FALSE)
  
  # Hasta 1 baño
  dataset$bath_cat <- factor(cut2(dataset$Bathroom, g=2), labels=c("Pocos_baños", "Muchos_baños"))
  #De 0 a 3 dormitorios
  dataset$bed_cat <- factor(cut2(dataset$Bedroom2, g=2), labels=c("Pocos_dormitorios", "Muchos_dormitorios"))
  
  #Estandarización de variables
  num_vars <- c('sqrt_distance', 'log_landsize', 'Lattitude', 'Longtitude', 'log_room_land')
  num_std_vars <- c('sqrt_distance_std', 'log_landsize_std', 'lattitude_std', 'longtitude_std', 'log_room_land_std')
  other_vars <- c('Suburb', 'Address', 'Rooms', 'Distance', 'Lattitude', 'Longtitude', 'SellerG', 'Date', 'Postcode', 'Bedroom2', 'Bathroom', 'YearBuilt', 'CouncilArea')
  cat_vars <- c('rooms_cat', 'year_built_cat', 'car_cat', 'Regionname', 'Type', 'Method', 'bath_cat', 'bed_cat', 'sell_rate_cat', 'may_have_water')
  datasetNum <- dataset %>% select(num_vars)
  datasetNumNorm <- predict(standarizer, datasetNum)
  colnames(datasetNumNorm) <- num_std_vars
  
  datasetFinal <- data.frame(dataset[,other_vars], dataset[,c('Car', 'Landsize')], datasetNumNorm[,num_std_vars], dataset[,cat_vars], dataset[, c('Price', 'log_price', 'price_label', 'price_label_high')])
  #datasetFinal <- data.frame(datasetNumNorm[,num_vars], dataset[,cat_vars], dataset[, c('Price', 'log_price')])
  
  datasetFinal$Regionname = factor(datasetFinal$Regionname, levels=c('Southern Metropolitan', 'Northern Metropolitan', 'Western Metropolitan', 'Eastern Metropolitan', 'South-Eastern Metropolitan', 'Eastern Victoria', 'Northern Victoria', 'Western Victoria'))
  #Utilizo la librería plyr
  datasetFinal$Regionname  = mapvalues(datasetFinal$Regionname, from = c('Southern Metropolitan', 'Northern Metropolitan', 'Western Metropolitan', 'Eastern Metropolitan','South-Eastern Metropolitan', 'Eastern Victoria', 'Northern Victoria', 'Western Victoria'), to = c('Southern_Metropolitan', 'Northern_Metropolitan', 'Western_Metropolitan', 'Eastern_Metropolitan','South_Eastern_Metropolitan', 'Eastern_Victoria', 'Northern_Victoria', 'Western_Victoria'))
  
  datasetFinal$Method = factor(datasetFinal$Method, levels=c('S', 'SP', 'PI', 'VB', 'SA'))
  datasetFinal$Type = factor(datasetFinal$Type, levels=c('h', 'u', 't'))
  
  # Se cambia el nombre de las columnas car y landsize para que cuadren con las de train
  colnames(datasetFinal)[14] <- 'CarImp'
  colnames(datasetFinal)[15] <- 'LandsizeImp'
  
  return(datasetFinal)
  
}

test <- final_dataset_construction_year(housesTest,normParam, imputationsCarforTest, imputationsLandsizeforTest, imputationsYearforTest)
val <- final_dataset_construction_year(housesVal, normParam, imputationsCarforTest, imputationsLandsizeforTest, imputationsYearforTest)


# Guardamos ---------------------------------------------------------------

train_path <- 'base_train.csv'
test_path <- 'base_test.csv'
val_path <- 'base_val.csv'

write.csv(housesTrainFinal, train_path, row.names = FALSE)
write.csv(test, test_path, row.names = FALSE)
write.csv(val, val_path, row.names = FALSE)