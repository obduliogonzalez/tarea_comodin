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
