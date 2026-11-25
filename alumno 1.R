# ==============================================================================
# ANÁLISIS ESTADÍSTICO: CONSUMO DE ALCOHOL Y TEMPERATURA MEDIA ANUAL (2019)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PREPARACIÓN Y LIMPIEZA DE DATOS (Puntos 1, 2 y 3)
# ------------------------------------------------------------------------------
# Fijar el directorio de trabajo donde están los CSV (con barras normales /):
# setwd("C:/ruta/a/tu/carpeta")

# Lectura de los ficheros CSV en formato inglés (separador "," y decimal ".")
temp <- read.csv("average-annual-surface-temperature.csv")
alc  <- read.csv("total-alcohol-consumption-per-capita-litres-of-pure-alcohol.csv")

# Emparejar los países de ambos ficheros mediante el código ISO (Code)
pos <- match(temp$Code, alc$Code)

# Crear el data frame limpio con las variables de interés
datos <- data.frame(
  Pais        = temp$Entity,
  Codigo      = temp$Code,
  Temperatura = temp$Average.surface.temperature,
  Alcohol     = alc$Alcohol.consumption[pos]
)

# Eliminar las filas con valores faltantes (NA)
datos <- na.omit(datos)

# Guardar el conjunto de datos limpio (opcional) y visualizarlo
write.csv2(datos, "datos_limpios_alcohol_temperatura.csv")
# View(datos)

# Extraer las dos variables como vectores para trabajar cómodamente (como en Práctica 2)
Temperatura <- datos$Temperatura
Alcohol     <- datos$Alcohol

# Comprobar tamaño de la muestra (debe ser >= 30; se obtienen n = 175 países)
n <- length(Alcohol)
n


# ------------------------------------------------------------------------------
# 2. ANÁLISIS ESTADÍSTICO UNIDIMENSIONAL: CONSUMO DE ALCOHOL (Punto 4)
# ------------------------------------------------------------------------------
# Resumen general (mínimo, Q1, mediana, media, Q3 y máximo)
summary(Alcohol)

# Medidas de posición
media_alc   <- mean(Alcohol, na.rm = TRUE)
mediana_alc <- median(Alcohol, na.rm = TRUE)
cuart_alc   <- quantile(Alcohol, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
min_alc     <- min(Alcohol, na.rm = TRUE)
max_alc     <- max(Alcohol, na.rm = TRUE)

# Medidas de dispersión (cuasi y poblacionales según Práctica 1 Parte 1)
rango_alc    <- max_alc - min_alc
iqr_alc      <- IQR(Alcohol, na.rm = TRUE)
cuasivar_alc <- var(Alcohol, na.rm = TRUE)
cuasidt_alc  <- sd(Alcohol, na.rm = TRUE)
var_alc      <- ((n - 1) / n) * cuasivar_alc
dt_alc       <- sqrt(var_alc)
CV_alc       <- dt_alc / media_alc

# Mostrar medidas de alcohol
media_alc; mediana_alc; cuart_alc; min_alc; max_alc
rango_alc; iqr_alc; cuasivar_alc; cuasidt_alc; var_alc; dt_alc; CV_alc

# Países con valores máximo y mínimo en consumo de alcohol
datos[Alcohol == max(Alcohol), ]
datos[Alcohol == min(Alcohol), ]

# Agrupación en k = 5 intervalos y tabla de frecuencias (Práctica 1 Parte 2)
k_alc <- 5
A_alc <- diff(range(Alcohol)) / k_alc
L1_alc <- min(Alcohol)
L_alc <- L1_alc + A_alc * (0:k_alc)
marcas_alc <- (L_alc[1:k_alc] + L_alc[1:k_alc + 1]) / 2
alc_agrupada <- cut(Alcohol, breaks = L_alc, right = FALSE, include.lowest = TRUE)
fabs_alc <- table(alc_agrupada)
frel_alc <- prop.table(fabs_alc)
fabs_alc
frel_alc


# ------------------------------------------------------------------------------
# 3. ANÁLISIS ESTADÍSTICO UNIDIMENSIONAL: TEMPERATURA MEDIA ANUAL (Punto 5)
# ------------------------------------------------------------------------------
# Resumen general
summary(Temperatura)

# Medidas de posición
media_temp   <- mean(Temperatura, na.rm = TRUE)
mediana_temp <- median(Temperatura, na.rm = TRUE)
cuart_temp   <- quantile(Temperatura, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
min_temp     <- min(Temperatura, na.rm = TRUE)
max_temp     <- max(Temperatura, na.rm = TRUE)

# Medidas de dispersión (cuasi y poblacionales según Práctica 1 Parte 1)
rango_temp    <- max_temp - min_temp
iqr_temp      <- IQR(Temperatura, na.rm = TRUE)
cuasivar_temp <- var(Temperatura, na.rm = TRUE)
cuasidt_temp  <- sd(Temperatura, na.rm = TRUE)
var_temp      <- ((n - 1) / n) * cuasivar_temp
dt_temp       <- sqrt(var_temp)
CV_temp       <- dt_temp / media_temp

# Mostrar medidas de temperatura
media_temp; mediana_temp; cuart_temp; min_temp; max_temp
rango_temp; iqr_temp; cuasivar_temp; cuasidt_temp; var_temp; dt_temp; CV_temp

# Países con valores máximo y mínimo en temperatura
datos[Temperatura == max(Temperatura), ]
datos[Temperatura == min(Temperatura), ]

# Agrupación en k = 5 intervalos y tabla de frecuencias (Práctica 1 Parte 2)
k_temp <- 5
A_temp <- diff(range(Temperatura)) / k_temp
L1_temp <- min(Temperatura)
L_temp <- L1_temp + A_temp * (0:k_temp)
marcas_temp <- (L_temp[1:k_temp] + L_temp[1:k_temp + 1]) / 2
temp_agrupada <- cut(Temperatura, breaks = L_temp, right = FALSE, include.lowest = TRUE)
fabs_temp <- table(temp_agrupada)
frel_temp <- prop.table(fabs_temp)
fabs_temp
frel_temp


# ------------------------------------------------------------------------------
# 4. REGRESIÓN BIDIMENSIONAL: ALCOHOL FRENTE A TEMPERATURA (Punto 6)
# ------------------------------------------------------------------------------
# Variable independiente (x): Temperatura (°C)
# Variable dependiente   (y): Alcohol (litros per cápita)

# Ajustar el modelo de regresión lineal simple con lm() (Práctica 2)
(lmAT <- lm(Alcohol ~ Temperatura))

# Extraer los coeficientes a1 (ordenada en el origen) y b1 (pendiente)
lmAT$coefficients
(a1 <- lmAT$coefficients[1])
(b1 <- lmAT$coefficients[2])

# Resumen completo del modelo (incluye R^2: Multiple R-squared)
summary(lmAT)

# Coeficiente de correlación lineal de Pearson (Práctica 2)
cor(Temperatura, Alcohol)

# Predicciones de ejemplo con la recta de regresión (Práctica 2)
# Ejemplo: consumo estimado de alcohol para países con 10 °C, 15 °C y 25 °C
predict(lmAT, newdata = data.frame(Temperatura = c(10, 15, 25)))


# ------------------------------------------------------------------------------
# 5. GRÁFICOS CORRESPONDIENTES (Punto 7)
# ------------------------------------------------------------------------------
# Gráfico 1: Histograma y diagrama de caja y bigotes del Consumo de Alcohol
par(mfrow = c(1, 2))
hist(Alcohol, breaks = L_alc, right = FALSE, col = "lightblue",
     main = "Histograma: Consumo de Alcohol",
     xlab = "Litros de alcohol puro per cápita (2019)", ylab = "Frecuencia")
boxplot(Alcohol, xlab = "Alcohol (litros per cápita)",
        main = "Cajas y bigotes: Alcohol", range = 1.5)
par(mfrow = c(1, 1))

# Gráfico 2: Histograma y diagrama de caja y bigotes de la Temperatura Media
par(mfrow = c(1, 2))
hist(Temperatura, breaks = L_temp, right = FALSE, col = "orange",
     main = "Histograma: Temperatura Media",
     xlab = "Temperatura media anual (°C, 2019)", ylab = "Frecuencia")
boxplot(Temperatura, xlab = "Temperatura (°C)",
        main = "Cajas y bigotes: Temperatura", range = 1.5)
par(mfrow = c(1, 1))

# Gráfico 3: Diagrama de dispersión y recta de regresión lineal (Práctica 2)
plot(Temperatura, Alcohol, pch = "+", cex = 1.5,
     main = "Regresión lineal: Alcohol frente a Temperatura",
     xlab = "Temperatura media anual (°C)",
     ylab = "Consumo de alcohol per cápita (litros)")
abline(lmAT, col = "blue", lwd = 2)