
#======================== FUNCIONES 
#*****Función para imputar NA: mediana para numéricas, moda para categóricas **
imputar_nas_generico <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.numeric(col)) {
      col[is.na(col)] <- median(col, na.rm = TRUE)
    } else if (is.character(col)) {
      moda <- names(sort(table(col), decreasing = TRUE))[1]
      col[is.na(col)] <- moda
      col <- as.factor(col)  # convertir a factor si era carácter
    } else if (is.factor(col)) {
      moda <- names(sort(table(col), decreasing = TRUE))[1]
      col[is.na(col)] <- moda
    }
    return(col)
  })
  return(df)
}
#**************************************** MODELO *****************************************
entrenar_random_forest <- function(train_data, test_data, target_col, n_trees = 500) {
  # 2. Asegurar que target_col esté en train_data
  if (!(target_col %in% names(train_data))) {
    stop("La columna objetivo no está en los datos de entrenamiento.")
  }
  
  # 3. Separar variable objetivo
  y <- train_data[[target_col]]
  X <- train_data %>% select(-all_of(target_col))
  
  # 4. Asegurar que solo se usen variables numéricas para randomForest
  X <- X %>% select(where(is.numeric))
  test_data <- test_data %>% select(where(is.numeric))
  
  # 5. Alinear columnas entre train y test
  common_cols <- intersect(names(X), names(test_data))
  X <- X[, common_cols]
  test_data <- test_data[, common_cols]
  
  print(length(common_cols))
 # print(common_cols)
  
  # 6. Entrenar modelo
  modelo <- randomForest(x = X, y = y, ntree = n_trees, importance = TRUE, mtry = floor(length(common_cols) / 3))
  print(modelo)                       
  
  # 7. Predecir
  if (nrow(test_data) == 0 || ncol(test_data) == 0) {
    stop("El conjunto de test está vacío o no tiene columnas comunes con train.")
  }
  
  predicciones <- predict(modelo, newdata = test_data)
  
  # Vector con el error OOB por cada árbol agregado
  oob_error <- modelo$mse
  plot(oob_error, type = "l",
       xlab = "Número de árboles",
       ylab = "Error OOB (MSE)",
       main = "Evolución del error OOB en Random Forest")
  
  
  # Crear archivo de submission para Kaggle
  submission <- data.frame(Id = test_data$Id, SalePrice = predicciones)
  write.csv(submission, "rf_submission_2.csv", row.names = FALSE)
  cat("Archivo 'rf_submission.csv' generado correctamente.\n")
  
  #evaluar el modelo 
  # 8. Evaluación si test_data incluye SalePrice
  if (target_col %in% colnames(train_data)) {
    reales <- train_data[[target_col]]
    mae_val <- mean(abs(reales - predicciones))
    mse_val <- mean((reales - predicciones)^2)
    rmse_val <- sqrt(mse_val)
    
    cat(paste0("MAE: ", round(mae_val, 2), "\n"))
    cat(paste0("MSE: ", round(mse_val, 2), "\n"))
    cat(paste0("RMSE: ", round(rmse_val, 2), "\n"))
  } else {
    cat("Advertencia: test_data no contiene la columna real '", target_col, "' para evaluar métricas.\n")
  }
  
  return(list(modelo = modelo, predicciones = predicciones))

}

getwd()

# ================================
# CARGA DE DATOS
# ================================
train_raw <- read.csv("C:/sexto_semestre/itinerario datos_1/datos_retoKaggle/house-prices-advanced-regression-techniques/train.csv")
test_raw <- read.csv("C:/sexto_semestre/itinerario datos_1/datos_retoKaggle/house-prices-advanced-regression-techniques/test.csv")

limpieza_datos <- function(train_raw, test_raw) {
  # Paquetes requeridos
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(randomForest)

# Crear columna log(SalePrice)
train_raw$logSalePrice <- log(train_raw$SalePrice)

# Añadir columna dummy a test
test_raw$SalePrice <- NA
test_raw$logSalePrice <- NA

# Unir datasets para limpieza conjunta
full_data <- bind_rows(train_raw, test_raw)

#************ CALCULO DE NULOS **************************
#NULOS
# Calcular número de NAs por columna
na_counts <- colSums(is.na(data.frame(train_raw)))
# Calcular total de filas
total_filas <- nrow(train_raw)
print(total_filas)
# porcentaje de nulos 
porcent_na <- total_filas * 0.6
print(porcent_na)
# Filtrar las columnas con más de 0 NAs
na_counts_filtered <- na_counts[na_counts > porcent_na ]
#columnas con un porcentaje mayor al 60% de nulos
print(na_counts_filtered)

# Convertir a data frame
na_df <- data.frame(
  feature = names(na_counts),
  na_count = as.numeric(na_counts)
)

# Filtrar solo columnas con al menos un NA (opcional)
na_df <- na_df %>% filter(na_count > 0)

# Crear gráfico de barras
ggplot(na_df, aes(x = reorder(feature, -na_count), y = na_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Número de NA por columna", x = "Columnas", y = "Cantidad de NAs") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#====================================
#eliminacion de columnas con 40% nulos 
#====================================
# Obtener los nombres de las columnas con más del porcentaje permitido de NA
cols_a_eliminar <- names(na_counts_filtered)
print(cols_a_eliminar)
full_data <- full_data %>%
  select(-all_of(cols_a_eliminar))

names(full_data)

# ================================
# LIMPIEZA
# ================================
# Convertir todos los character a factor
full_data <- full_data %>%
  mutate(across(where(is.character), as.factor))

# Imputar NAs
# Guardar y quitar temporalmente las columnas target
saleprice <- full_data$SalePrice
log_saleprice <- full_data$logSalePrice

# Imputar solo otras columnas
full_data <- imputar_nas_generico(full_data)

# Restaurar las columnas target
full_data$SalePrice <- saleprice
full_data$logSalePrice <- log_saleprice

# Confirmar limpieza
cat("Valores NA restantes aceptados, 2918. Actuales = :", sum(is.na(full_data)), "\n")

sapply(full_data, function(x) {
  if (is.character(x)) {
    mean(is.na(x) | x == "")
  } else {
    mean(is.na(x))
  }
})

# ================================
# SEPARAR TRAIN Y TEST NUEVAMENTE
# ================================
train_clean <- full_data[!is.na(full_data$SalePrice), ]
test_clean <- full_data[is.na(full_data$SalePrice), ]
test_clean$SalePrice <- NULL
test_clean$logSalePrice <- NULL
nrow(test_clean)

#************* ouliers ********************
#str(train_raw)
Q1 <- quantile(train_clean$SalePrice, 0.25)
Q3 <- quantile(train_clean$SalePrice, 0.75)
IQR <- Q3 - Q1

limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR

# Filtrar el data frame sin outliers
train_clean <- train_clean[train_clean$SalePrice >= limite_inferior & train_clean$SalePrice <= limite_superior, ]

return(list(train_clean = train_clean, test_clean = test_clean))

       
}




#*************************************************************************************************
#************************************************************************************************
#*#funcion de limpieza
resultado <- limpieza_datos(train_raw, test_raw)

train_clean <- resultado$train_clean
test_clean <- resultado$test_clean


# Llamar a la función
resultado <- entrenar_random_forest(train_clean, test_clean, target_col = "SalePrice")

# Ver el modelo entrenado
print(resultado$modelo)
# Vector con el error OOB por cada árbol agregado
#oob_error <- resultado$modelo$mse

#print(oob_error)




#==================================

entrenar_random_forest_cv <- function(train_data, target_col, n_trees = 500) {
  y <- train_data[[target_col]]
  X <- train_data %>% select(-all_of(target_col)) %>% select(where(is.numeric))
  
  modelo <- randomForest(x = X, y = y, ntree = n_trees,
                         importance = TRUE, mtry = floor(ncol(X) / 3))
  
  return(modelo)
}


validacion_cruzada_general <- function(data, target_col, model_func, k = 5, ...) {
  library(caret)
  set.seed(123)
  
  folds <- createFolds(data[[target_col]], k = k, list = TRUE, returnTrain = FALSE)
  rmse_scores <- c()
  
  for (i in seq_along(folds)) {
    cat("Fold", i, "\n")
    
    test_indices <- folds[[i]]
    test_data <- data[test_indices, ]
    train_data <- data[-test_indices, ]
    
    # Entrenar el modelo pasando parámetros adicionales
    modelo <- model_func(train_data, target_col, ...)
    
    # Preparar datos para predicción: solo numéricas igual que en train
    X_test <- test_data %>% select(-all_of(target_col)) %>% select(where(is.numeric))
    y_test <- test_data[[target_col]]
    
    # Variables usadas en el modelo
    model_vars <- rownames(modelo$importance)
    
    # Asegurar que las variables test estén en el orden y columnas correctas
    common_cols <- intersect(colnames(X_test), model_vars)
    if (length(common_cols) == 0) stop("No hay columnas comunes para predecir")
    
    X_test <- X_test[, common_cols, drop = FALSE]
    
    pred <- predict(modelo, newdata = X_test)
    
    rmse_scores <- c(rmse_scores, RMSE(pred, y_test))  # Nota: en caret es (pred, obs)
    
  }
  
  cat("RMSE promedio (", k, " folds): ", mean(rmse_scores), "\n")
  return(mean(rmse_scores))
}

# Llamada corregida, pasando n_trees
rmse_promedio <- validacion_cruzada_general(
  data = train_clean,
  target_col = "SalePrice",
  model_func = entrenar_random_forest_cv,
  k = 5,
  n_trees = 500
)





