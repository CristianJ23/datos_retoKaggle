# Paquetes requeridos
library(dplyr)
library(tidyr)

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
train_raw <- read.csv("train.csv")
test_raw <- read.csv("test.csv")

# Crear columna log(SalePrice)
train_raw$logSalePrice <- log(train_raw$SalePrice)

# Añadir columna dummy a test
test_raw$SalePrice <- NA
test_raw$logSalePrice <- NA

# Unir datasets para limpieza conjunta
full_data <- bind_rows(train_raw, test_raw)

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


