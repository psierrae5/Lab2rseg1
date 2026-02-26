library(tidyverse)

# 1. Cargar el dataset
data("AirPassengers")

## Inspeccionar la estructura del dataset
print(class(AirPassengers))   # Verifica que es una serie temporal (ts)
print(summary(AirPassengers)) # Resumen estadístico
print(start(AirPassengers))   # Inicio de la serie
print(end(AirPassengers))     # Fin de la serie
print(frequency(AirPassengers)) # Frecuencia: 12 (mensual)
time(AirPassengers)
head(AirPassengers,60)
      
# 2. Exploración inicial
library(tidyverse)

df <- data.frame(
  date = time(AirPassengers),
  passengers = as.numeric(AirPassengers)
)
ggplot(df, aes(x = date, y = passengers)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal()+
  ggtitle("Número de Pasajeros a lo largo del tiempo")
          
cat("La media de la serie temporal AirPassengers es: ", mean(AirPassengers), "\n")
cat("La desviación típica de la serie temporal AirPassengers es: ", sd(AirPassengers))

# 3. Análisis de tendencia y estacionalidad
dec <- decompose(AirPassengers)
plot(dec)   

print("Graficamos la Tendencia")
plot(dec$trend, main = "Tendencia", col = "blue", lwd = 2)
print("Graficamos la Estacionalidad")
plot(dec$seasonal, main = "Estacionalidad", col = "red", lwd = 2)
print("Graficamos el Componente Aleatorio")
plot(dec$random, main = "Componente Aleatorio", col = "darkgreen", lwd = 2)

#4. Análisis de estacionariedad:

acf <- acf(AirPassengers, main = "ACF - Serie Original")
acf
pacf <- pacf(AirPassengers, main = "PACF - Serie Original")
pacf
library(tseries)
adf.test(AirPassengers)
diff_airp <- diff(AirPassengers)
diff_airp
adf.test(diff_airp)

#5. Detección de valores atípicos:

boxplot(AirPassengers,
        main = "Boxplot de AirPassengers",
        ylab = "Número de pasajeros (miles)",
        col = "lightblue",
        horizontal = TRUE)

# No hay atípicos, salvo picos estacionales normales. Nos fijamos en los meses
mes <- cycle(AirPassengers)
año <- floor(time(AirPassengers))
sett <- as.numeric(AirPassengers)
mensual <- boxplot(sett ~ mes,
                    main="Boxplot mensual de AirPassengers",
                   xlab="Mes", ylab="Número de pasajeros",
                    names=month.abb)
outliers <- boxplot.stats(sett)$out
outliers

#6. Interpretación de resultados:

cat("La serie muestra tendencia creciente y fuerte estacionalidad anual.\n")
cat("La variabilidad aumenta con el tiempo (mejor trabajar con log). \n")
cat("Tras la diferenciación, la serie se vuelve estacionaria.\n")
cat("Algunos valores marcados como outliers corresponden a picos estacionales normales.\n")
