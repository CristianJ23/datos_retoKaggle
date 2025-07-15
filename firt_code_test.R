############################################################################
#****************** reto casas ********************************************
#*####################################################################
# Cargar librerías necesarias
library(randomForest)
library(dplyr)

# Leer los datos
test_data <- read.csv("../segundo bimestre/retoKaggle/house-prices-advanced-regression-techniques/test.csv")
train_data <- read.csv("../segundo bimestre/retoKaggle/house-prices-advanced-regression-techniques/train.csv")
sapply(train_data, function(x) mean(is.na(x) | !nzchar(x)))

# Ver nombres de columnas
names(test_data)
str(train_data)

# Seleccionar variables relevantes
train_data <- train_data %>%
  select(SalePrice, OverallQual, GrLivArea, GarageCars, GarageArea, TotalBsmtSF, X1stFlrSF, FullBath, YearBuilt, TotRmsAbvGrd, Fireplaces)

test_data_features <- test_data %>%
  select(OverallQual, GrLivArea, GarageCars, GarageArea, TotalBsmtSF, X1stFlrSF, FullBath, YearBuilt, TotRmsAbvGrd, Fireplaces)

# Evaluación del modelo (dividir train_data en 80/20 para validación)
set.seed(123)
train_index <- sample(1:nrow(train_data), 0.8 * nrow(train_data))
train_split <- train_data[train_index, ]
val_split <- train_data[-train_index, ]

# Entrenar el modelo con el 80% de los datos
set.seed(88)
rf_model <- randomForest(SalePrice ~ ., data = train_split, ntree = 100, mtry = 2)

# Evaluar el modelo en el 20% restante
val_pred <- predict(rf_model, val_split)

mae <- mean(abs(val_pred - val_split$SalePrice))
mse <- mean((val_pred - val_split$SalePrice)^2)
rmse <- sqrt(mse)

cat("Evaluación en conjunto de validación:\n")
cat("MAE:", round(mae, 2), "\n")
cat("MSE:", round(mse, 2), "\n")
cat("RMSE:", round(rmse, 2), "\n")

# Importancia de las variables
var_imp <- importance(rf_model)
print(var_imp)
varImpPlot(rf_model)

# Entrenar modelo final con todos los datos de entrenamiento
set.seed(88)
rf_final_model <- randomForest(SalePrice ~ ., data = train_data, ntree = 100, mtry = 2)

print(rf_final_model)
# Predecir valores para test_data (no tiene SalePrice)
rf_pred_test <- predict(rf_final_model, test_data_features)

# Crear archivo de submission para Kaggle
submission <- data.frame(Id = test_data$Id, SalePrice = rf_pred_test)
write.csv(submission, "rf_submission.csv", row.names = FALSE)

cat("Archivo 'rf_submission.csv' generado correctamente.\n")

sum(is.na(rf_pred_test))  # Debería dar 0

colSums(is.na(test_data_features))

test_data_features <- test_data_features %>%
  mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

rf_pred_test <- predict(rf_final_model, test_data_features)

sum(is.na(rf_pred_test))  # Ahora debe ser 0

submission <- data.frame(Id = test_data$Id, SalePrice = rf_pred_test)

write.csv(submission, "rf_submission.csv", row.names = FALSE)