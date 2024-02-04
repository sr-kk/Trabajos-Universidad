# librerias utilizadas
library(tidyverse)
library(lubridate)
library(stringr)
library(splines)

# CO --------------------------------------------------------------------

# Cargamos los datos
datos_co <- readr::read_delim("Tareas/Tarea 1/Datos/La Florida/LaFlorida-CO.csv", delim = ";")

colnames(datos_co) <- c("fecha", "hora", "validados", "preliminares", 
                        "no_validados", "vacio")

datos_co2 <- datos_co |> 
  select(-c(vacio, hora, preliminares)) |> 
  mutate(co = ifelse(is.na(validados), no_validados, validados)) |> 
  mutate(co = as.numeric(str_replace(co, ",", "."))) |> 
  mutate(fecha = lubridate::ymd(fecha)) |> 
  filter(!is.na(co)) |> 
  filter(year(fecha) < 2023)

datos_co2 |>
  ggplot(aes(fecha, co)) +
  geom_line() +
  geom_smooth() +
  theme_minimal()

# agregamos un indice de tiempo
datos_co2$tiempo <- as.numeric(time(datos_co2$co))

# grafico de CO vs tiempo
plot(datos_co2$tiempo, datos_co2$co, type = "l", bty = "n", las = 1, 
     ylab = "Niveles de CO", xlab = "Tiempo")

# vemos informacion de la serie del tiempo
acf(datos_co2$co, lag.max = 100)
pacf(datos_co2$co)

hist(datos_co2$co)

MASS::boxcox(lm(datos_co2$co ~ 1))


# Variables exogenas ------------------------------------------------------

## Humedad ----------------------------------------------------------------

# cargamos de humedad
humedad <- readr::read_delim("Tareas/Tarea 1/Datos/La Florida/LaFlorida-Humedad.csv",
                             delim = ";")

# cambiamos el nombre de las columnas
colnames(humedad) <- c("fecha", "hora", "valor", "vacio")

# los datos no vienen en el mismo formato que los datos de pm10 (promedio mensual)
# por lo que transformamos los
humedad <- humedad |> 
  # eliminamos la ultima variable que no contiene informacion
  select(-c(vacio)) |> 
  # creamos fecha2 como formato fecha 
  mutate(fecha2 = lubridate::ymd(fecha)) |> 
  # extraemos el año
  mutate(anio = year(fecha2)) |> 
  # extraemos el mes
  mutate(mes = month(fecha2)) |> 
  # obtenemos el promedio mensual (omitimos los na)y desagrupamos
  group_by(anio, mes) |> 
  summarise(promedio_mensual = mean(valor, na.rm = TRUE)) |> 
  ungroup() |> 
  # concatenamos la fecha (si el mes es menor que 10 se agrega un 0) 
  mutate(fecha = ifelse(mes / 10 >= 1, str_c(anio, mes, "01"),
                        str_c(anio, "0", mes, "01"))) |> 
  # transformamos la fecha al formato correcto
  mutate(fecha = ymd(fecha))


## Temperatura -------------------------------------------------------------

## HAY NA EN LA TEMPERATURA

# cargamos de temperatura
temperatura <- readr::read_delim("Tareas/Tarea 1/Datos/La Florida/LaFlorida-Temperatura.csv",
                                 delim = ";")

# cambiamos el nombre de las columnas
colnames(temperatura) <- c("fecha", "hora", "valor", "vacio")

# los datos no vienen en el mismo formato que los datos de pm10 (promedio mensual)
# por lo que transformamos los
temperatura <- temperatura |> 
  select(-c(vacio)) |> 
  mutate(fecha2 = lubridate::ymd(fecha)) |> 
  mutate(anio = year(fecha2)) |> 
  mutate(mes = month(fecha2)) |> 
  mutate(valor = as.numeric(str_replace(valor, ",", "."))) |> 
  group_by(anio, mes) |> 
  summarise(promedio_mensual = mean(valor, na.rm = TRUE)) |> 
  ungroup() |> 
  # concatenamos la fecha (si el mes es menor que 10 se agrega un 0) 
  mutate(fecha = ifelse(mes / 10 >= 1, str_c(anio, mes, "01"),
                        str_c(anio, "0", mes, "01"))) |> 
  # transformamos la fecha al formato correcto
  mutate(fecha = ymd(fecha)) |> 
  select(-c(mes, anio))

plot(temperatura$promedio_mensual, type = "l")
# Imputamos datos faltantes mediante interpolación lineal
# Encuentra los índices de los datos faltantes
indices_faltantes = which(is.na(temperatura$promedio_mensual))
# Imputa los valores faltantes utilizando interpolación lineal
temperatura$promedio_mensual[indices_faltantes] = approx(seq_along(temperatura$promedio_mensual),
                                                    temperatura$promedio_mensual,
                                                    xout = indices_faltantes)$y
plot(temperatura$promedio_mensual, type = "l")


## Viento ------------------------------------------------------------------

# cargamos de humedad
viento <- readr::read_delim("Tareas/Tarea 1/Datos/La Florida/LaFlorida-Viento.csv", delim = ";")

# cambiamos el nombre de las columnas
colnames(viento) <- c("fecha", "hora", "valor", "vacio")

# los datos no vienen en el mismo formato que los datos de pm10 (promedio mensual)
# por lo que transformamos los
viento <- viento |> 
  select(-c(vacio)) |> 
  mutate(fecha2 = lubridate::ymd(fecha)) |> 
  mutate(anio = year(fecha2)) |> 
  mutate(mes = month(fecha2)) |> 
  mutate(valor = as.numeric(str_replace(valor, ",", "."))) |> 
  group_by(anio, mes) |> 
  summarise(promedio_mensual = mean(valor, na.rm = TRUE)) |> 
  ungroup() |> 
  # concatenamos la fecha (si el mes es menor que 10 se agrega un 0) 
  mutate(fecha = ifelse(mes / 10 >= 1, str_c(anio, mes, "01"),
                        str_c(anio, "0", mes, "01"))) |> 
  # transformamos la fecha al formato correcto
  mutate(fecha = ymd(fecha)) |> 
  select(-c(mes, anio))


## Union de las bases ------------------------------------------------------


datos <- inner_join(datos_co2, humedad, by = "fecha") |> 
  rename(humedad = promedio_mensual) |> 
  select(-c(validados, no_validados)) |> 
  mutate(mes = as.factor(mes)) |> 
  mutate(anio = as.factor(anio))

datos <- inner_join(datos, temperatura, by = "fecha") |> 
  rename(temperatura = promedio_mensual)

datos <- inner_join(datos, viento, by = "fecha") |> 
  rename(viento = promedio_mensual)


plot(scale(datos$co), type = "l")
lines(scale(datos$humedad), type = "l", col = "red")
lines(scale(datos$temperatura), type = "l", col = "blue")
lines(scale(datos$viento), type = "l", col = "blue")

datos$tiempo <- time(datos$co)

# Modelos -----------------------------------------------------------------


# Modelo  pruebas -------------------------------------------------------

datos$co2 <- (datos$co)^(0.6)

datos <- datos |> mutate(mes2 = ifelse(mes %in% c("1", "2", "10", "11", "12", "9"), "verano", mes))

datos$mes2 <- as.factor(datos$mes2)

# GOOD
modelo <- lm(co2 ~ I(tiempo^2) + mes2 + temperatura * I(tiempo), 
             data = datos)

summary(modelo)

plot(datos$tiempo, datos$co2, type = "l")
lines(modelo$fitted.values ~ datos$tiempo, col = "red")

plot(modelo$residuals, type = "l")


####

grafico <- ggplot() + 
  geom_line(aes(x = datos$tiempo, y = modelo$residuals)) +
  theme_minimal()

plotly::ggplotly(grafico)

###

acf_modelo <- acf(modelo$residuals, lag.max = 100)

forecast::ggAcf(modelo$residuals, lag.max = 45) +
  scale_y_continuous(limits = c(-1,1)) +
  theme_minimal()

shapiro.test(modelo$residuals)
  
pacf(modelo$residuals, lag.max = 100)

ks.test(scale(modelo$residuals), pnorm)

lmtest::bptest(modelo)

bc <- MASS::boxcox(modelo)

bc$x[which.max(bc$y)]

aux <- lm(modelo$residuals ~ datos$tiempo, data = datos)

summary(aux)

# 400 x 230
LSTS::Box.Ljung.Test(modelo$residuals) +
  labs(y = "Valor-p", 
       title = "Valor-p para Test de Box-Ljung")

LSTS::periodogram(modelo$residuals)



###

# MODELO ARIMA

(modelo_arima <- forecast::auto.arima(modelo$residuals))

modelo_arima$residuals

modelo_arima$residuals |> acf(lag.max = 100)

hist(modelo_arima$residuals, breaks = 25, freq = F)
curve(dnorm(x, 
            mean = mean(modelo_arima$residuals), 
            sd = sd(modelo_arima$residuals)),
      from = -3, to = 3, add = TRUE) # no logré añadir la curva XD

qqnorm(modelo_arima$residuals)
qqline(modelo_arima$residuals)

shapiro.test(modelo_arima$residuals)

ks.test(scale(modelo_arima$residuals), pnorm)

plot(modelo_arima$residuals)

# MAPE: Mean Absoluto percentage Error
mape <- function(serie, pred){
  return( mean(abs( (serie - pred)/serie )  )  )
}
mape(datos$co2, modelo$fitted.values)
