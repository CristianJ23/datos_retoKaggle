# Paquetes requeridos
library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)

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
entrenar_random_forest <- function(train_data, test_data, target_col, n_trees = 400) {
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
 # oob_error <- modelo$mse
 # plot(oob_error, type = "l",
  #     xlab = "Número de árboles",
   #    ylab = "Error OOB (MSE)",
    #   main = "Evolución del error OOB en Random Forest")
  
  
  # Crear archivo de submission para Kaggle
  submission <- data.frame(Id = test_data$Id, SalePrice = predicciones)
  write.csv(submission, "rf_submission_2.csv", row.names = FALSE)
  cat("Archivo 'rf_submission.csv' generado correctamente.\n")
  
  return(list(modelo = modelo, predicciones = predicciones))

}

getwd()

# ================================
# CARGA DE DATOS
# ================================
train_raw <- read.csv("C:/sexto_semestre/itinerario datos_1/datos_retoKaggle/house-prices-advanced-regression-techniques/train.csv")
test_raw <- read.csv("C:/sexto_semestre/itinerario datos_1/datos_retoKaggle/house-prices-advanced-regression-techniques/test.csv")


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
porcent_na <- total_filas * 0.4
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

#************* ouliers ********************
str(train_raw)

#====================================
#eliminacion de columnas con 40% nulos 
#====================================
# Obtener los nombres de las columnas con más del porcentaje permitido de NA
cols_a_eliminar <- names(na_counts_filtered)
print(cols_a_eliminar)
full_data <- full_data %>%
  select(-all_of(cols_a_eliminar))

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
cat("Valores NA restantes:", sum(is.na(full_data)), "\n")

# ================================
# SEPARAR TRAIN Y TEST NUEVAMENTE
# ================================
train_clean <- full_data[!is.na(full_data$SalePrice), ]
test_clean <- full_data[is.na(full_data$SalePrice), ]
test_clean$SalePrice <- NULL
test_clean$logSalePrice <- NULL
nrow(test_clean)



#*************************************************************************************************
#************************************************************************************************
# Llamar a la función
resultado <- entrenar_random_forest(train_clean, test_clean, target_col = "SalePrice")

# Ver el modelo entrenado
print(resultado$modelo)
# Vector con el error OOB por cada árbol agregado
#oob_error <- resultado$modelo$mse

#print(oob_error)




