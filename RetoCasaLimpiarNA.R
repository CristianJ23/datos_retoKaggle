# Paquetes requeridos
library(dplyr)
library(tidyr)
library(ggplot2)

# Función para imputar NA: mediana para numéricas, moda para categóricas
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

# ================================
# LIMPIEZA
# ================================
# Convertir todos los character a factor
full_data <- full_data %>%
  mutate(across(where(is.character), as.factor))

# Imputar NAs
full_data <- imputar_nas_generico(full_data)

# Confirmar limpieza
cat("Valores NA restantes:", sum(is.na(full_data)), "\n")

# ================================
# SEPARAR TRAIN Y TEST NUEVAMENTE
# ================================
train_clean <- full_data[!is.na(full_data$SalePrice), ]
test_clean <- full_data[is.na(full_data$SalePrice), ]
test_clean$SalePrice <- NULL
test_clean$logSalePrice <- NULL


