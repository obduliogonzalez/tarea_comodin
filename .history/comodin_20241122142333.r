# Configurar el espejo de CRAN
options(repos = c(CRAN = "https://cran.r-project.org"))

# Instalar y cargar paquetes
if (!require(randomForest)) {
  install.packages("randomForest")
  library(randomForest)
} else {
  library(randomForest)
}

# Proporcionar la ruta completa al archivo CSV

file_path <- "C://Users//nestor.gonzalez//Documents//GitHub//tarea_comodin//MIGRACION_BDP.csv"

if (file.exists(file_path)) {
  migracion <- read.csv(file_path, header = TRUE, sep = ",")
  migracion <- na.omit(migracion)
  migracion <- migracion[, c("DEPARTAMENTO", "PEI3", "PEI4", "PEI5", "AREA")]
  print("Archivo cargado y procesado correctamente.")
} else {
  stop("El archivo no existe en la ruta especificada.")
}

migracion$DEPARTAMENTO <- as.factor(migracion$DEPARTAMENTO)

set.seed(100) # Semilla para reproducibilidad 
migracion <- migracion[sample(1:nrow(migracion)), ] # Desordenar datos

# Dividir los datos en conjuntos de entrenamiento y prueba
index <- sample(1:nrow(migracion), 0.7 * nrow(migracion)) # 70% de los datos para entrenamiento
train <- migracion[index, ] # Datos de entrenamiento
test <- migracion[-index, ] # Datos de prueba

# Entrenar el modelo de bosque aleatorio
bosque <- randomForest(DEPARTAMENTO ~ PEI3 + PEI4 + PEI5 + AREA, data = train, ntree = 100, mtry = 4)

# Predecir en el conjunto de prueba
predicciones <- predict(bosque, test)

dato_nuevo <- data.frame(PEI3 = 2, PEI4 = 40, PEI5 = 2000, AREA = 2)

# Predecir en un nuevo dato
prediccion_nueva <- predict(bosque, dato_nuevo)

# Mostrar las predicciones
# print(predicciones)

# Opcional: Ver las predicciones en una vista de datos (solo en RStudio o R GUI)
View(predicciones)
View(prediccion_nueva)



# Mostrar las tablas de datos
View(migracion)
View(train)
View(test)

# Importancia de las variables
importancia <- importance(bosque) # Calcula la importancia de las variables
barplot(importancia[, 1], main = "Importancia de las Variables", horiz = TRUE, col = "steelblue", las = 1)

# Gráfico de error del modelo
plot(bosque, main = "Error del Modelo vs Número de Árboles")

# Matriz de confusión
confusion_matrix <- table(test$DEPARTAMENTO, predicciones)
print(confusion_matrix)


# Gráfico de predicciones por clase
library(ggplot2)
pred_df <- data.frame(Real = test$DEPARTAMENTO, Predicho = predicciones)
ggplot(pred_df, aes(x = Real, fill = Predicho)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Distribución de Predicciones por Clase", x = "Clase Real", y = "Frecuencia")


library(pROC)
roc_curve <- roc(test$DEPARTAMENTO, as.numeric(predicciones))
plot(roc_curve, col = "darkorange", main = "Curva ROC", print.auc = TRUE)


# Generar datos nuevos y predecir
nuevo_df <- data.frame(PEI3 = seq(1, 10, by = 1), PEI4 = seq(20, 200, by = 20), PEI5 = seq(500, 2000, by = 150), AREA = 2)
predicciones_nuevas <- predict(bosque, nuevo_df)

# Gráfico de las predicciones
nuevo_df$Prediccion <- predicciones_nuevas
ggplot(nuevo_df, aes(x = PEI3, y = PEI5, color = Prediccion)) +
  geom_point(size = 4) +
  labs(title = "Predicciones en Datos Nuevos", x = "PEI3", y = "PEI5") +
  theme_minimal()

library(caret)
control <- trainControl(method = "cv", number = 5)
modelo_cv <- train(DEPARTAMENTO ~ PEI3 + PEI4 + PEI5 + AREA, data = migracion, method = "rf", trControl = control)
print(modelo_cv)

