
# Establecer el directorio de trabajo donde se encuentra el archivo CSV
setwd("C:/Users/ASUS/Documents")

# Leer el archivo CSV sin modificar los nombres de las columnas
datos <- read.csv("Book1.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

# Verificar los nombres de las columnas
colnames(datos)

# Suponiendo que la columna de interés se llama "140.8"
# Separar los datos en dos grupos de 14 observaciones cada uno
grupo1 <- datos[1:14, , drop = FALSE]
grupo2 <- datos[15:28, , drop = FALSE]

# Calcular estadísticas para el Grupo 1
media1 <- mean(grupo1[["140.8"]], na.rm = TRUE)
sd1 <- sd(grupo1[["140.8"]], na.rm = TRUE)
var1 <- var(grupo1[["140.8"]], na.rm = TRUE)
cv1 <- (sd1 / media1) * 100

# Calcular estadísticas para el Grupo 2
media2 <- mean(grupo2[["140.8"]], na.rm = TRUE)
sd2 <- sd(grupo2[["140.8"]], na.rm = TRUE)
var2 <- var(grupo2[["140.8"]], na.rm = TRUE)
cv2 <- (sd2 / media2) * 100

# Crear un data frame con las estadísticas
summary_stats <- data.frame(
  Grupo = c("Grupo 1", "Grupo 2"),
  Media = c(media1, media2),
  Desviacion_Estandar = c(sd1, sd2),
  Varianza = c(var1, var2),
  Coeficiente_Variacion = c(cv1, cv2)
)

# Mostrar las estadísticas calculadas
print(summary_stats)

# Crear un data frame combinado para los gráficos
grupo1$Grupo <- "Grupo 1"
grupo2$Grupo <- "Grupo 2"
datos_combinados <- rbind(grupo1, grupo2)

# Boxplot comparativo
boxplot(datos_combinados[["140.8"]] ~ datos_combinados$Grupo,
        col = c("skyblue", "orange"),
        main = "Distribución de Valores en los Grupos",
        ylab = "Valor")

# Histogramas para cada grupo
par(mfrow = c(1, 2))  # Configura dos gráficos lado a lado
hist(grupo1[["140.8"]], col = "skyblue", main = "Histograma - Grupo 1", xlab = "Valor")
hist(grupo2[["140.8"]], col = "orange", main = "Histograma - Grupo 2", xlab = "Valor")
par(mfrow = c(1, 1))  # Restablece la configuración de gráficos


# Realizar la prueba t para muestras independientes
resultado_prueba <- t.test(grupo1[["140.8"]], grupo2[["140.8"]], var.equal = TRUE)

# Mostrar el resumen de la prueba
print(resultado_prueba)


