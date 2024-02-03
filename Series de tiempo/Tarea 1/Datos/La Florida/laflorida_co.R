# librerias utilizadas
library(tidyverse)
library(lubridate)
library(stringr)
library(splines)
library(forecast)

source("Tareas/Tarea 1/Codigo/summary.arima.R")
source("Tareas/Tarea 1/Codigo/TS.diag.R")
source("Tareas/Tarea 1/Codigo/salida.arima.R")


# CO --------------------------------------------------------------------

# Cargamos los datos
CO <- readr::read_delim("Tareas/Tarea 1/Datos/La Florida/LaFlorida-CO.csv", delim = ";")

colnames(CO) <- c("fecha", "hora", "validados", "preliminares", 
                        "no_validados", "vacio")

CO <- CO |> 
  select(-c(vacio, hora, preliminares)) |> 
  mutate(CO = ifelse(is.na(validados), no_validados, validados)) |> 
  mutate(CO = as.numeric(str_replace(CO, ",", "."))) |> 
  mutate(fecha = lubridate::ymd(fecha)) |> 
  filter(!is.na(CO)) %>% 
  select(fecha, CO)

CO |>
  ggplot(aes(fecha, CO)) +
  geom_line() +
  geom_smooth() +
  theme_minimal()




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
  summarise(humedad = mean(valor, na.rm = TRUE)) |> 
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
  summarise(temperatura = mean(valor, na.rm = TRUE)) |> 
  ungroup() |> 
  # concatenamos la fecha (si el mes es menor que 10 se agrega un 0) 
  mutate(fecha = ifelse(mes / 10 >= 1, str_c(anio, mes, "01"),
                        str_c(anio, "0", mes, "01"))) |> 
  # transformamos la fecha al formato correcto
  mutate(fecha = ymd(fecha)) |> 
  select(-c(mes, anio))



plot(temperatura$temperatura, type = "l")
# Imputamos datos faltantes mediante interpolación lineal
# Encuentra los índices de los datos faltantes
indices_faltantes = which(is.na(temperatura$temperatura))
# Imputa los valores faltantes utilizando interpolación lineal
temperatura$temperatura[indices_faltantes] = approx(seq_along(temperatura$temperatura),
                                                         temperatura$temperatura,
                                                         xout = indices_faltantes)$y
plot(temperatura$temperatura, type = "l")

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
  summarise(viento = mean(valor, na.rm = TRUE)) |> 
  ungroup() |> 
  # concatenamos la fecha (si el mes es menor que 10 se agrega un 0) 
  mutate(fecha = ifelse(mes / 10 >= 1, str_c(anio, mes, "01"),
                        str_c(anio, "0", mes, "01"))) |> 
  # transformamos la fecha al formato correcto
  mutate(fecha = ymd(fecha)) |> 
  select(-c(mes, anio))


# Correlaciones -----------------------------------------------------------

datos = cbind(CO %>% filter((fecha >= "2003-12-01") & (fecha <= "2022-12-01")),
              humedad %>% filter(fecha <= "2022-12-01") %>% select(humedad),
              viento %>% filter(fecha <= "2022-12-01") %>% select(viento),
              temperatura %>% filter(fecha <= "2022-12-01") %>% select(temperatura))

datos$m = as.factor(month(datos$fecha))

datos$CO2 <- (datos$CO)^(0.6)

datos <- datos |> mutate(mes = ifelse(m %in% c("1", "2", "10", "11", "12", "9"), "verano", m))

datos$mes <- as.factor(datos$mes)

Y = ts(datos$CO, start = c(2003, 12), end = c(2022, 12), frequency = 12)
t = as.numeric(time(Y))

dim(datos)
cor(cbind(datos[,2:5], t))

summary(lm(datos$CO ~ 1))


plot(datos$CO ~ datos$humedad)
plot(datos$CO ~ datos$viento)
plot(datos$CO ~ datos$temperatura)

plot(datos$CO, type = "l")
plot(datos$humedad, type = "l")
plot(datos$viento, type = "l")
plot(datos$temperatura, type = "l")




plot(datos$CO ~ t, type = "l", ylab = "Niveles de CO", xlab = "",  
     lwd = 2, las = 1, xaxt = "n", cex.lab = 1.5)
axis(1, at = seq(2003, 2022, 1), labels = seq(2003, 2022, 1))

acf(datos$CO, lag = 100, main = "", cex.lab = 1.5, lwd = 2)



modelo <- lm(CO2 ~ I(t^2) + mes + temperatura * I(t), 
             data = datos)

summary(modelo)


plot(modelo$residuals, type = "l")
abline(h = 0, lty = 2, lwd = 2)

plot(cummean(CO$CO), type = "l")
abline(h = 0, lty = 2, lwd = 2)


plot(datos$CO ~ t, type = "l", ylab = "Niveles de CO", xlab = "",
     lwd = 2, las = 1, xaxt = "n", cex.lab = 1.5)
axis(1, at = seq(2003, 2022, 1), labels = seq(2003, 2022, 1))
lines(modelo$fitted.values^(10/6) ~ t, col = "#1C86EE", lwd = 2)
legend("topright", 
       legend = c("CO", "Valores ajustados"), 
       col = c("black", "#1C86EE"), 
       lwd = 2, 
       horiz = T)

## Supuestos
# 1) Linealidad

plot(modelo$fitted.values ~ modelo$residuals, pch = 19)
# No se visualiza un patron especial en el grafico. Se loga
# la linealidad


# 2) Normalidad
qqnorm(modelo$residuals, main="Gráfico Q-Q Normal")
qqline(modelo$residuals, col="red")
shapiro.test(modelo$residuals)
# Hay normalidad
ks.test(scale(modelo$residuals), pnorm)


# 3) Homocedasticidad
lmtest::bptest(modelo)
## Hay homocedasticidad

# 4) Incorrelacion
acf(modelo$residuals, lag = 100)
pacf(modelo$residuals, lag = 100)
## Hay correlacion estacional


fit = auto.arima(modelo$residuals)
salida.arima(modelo$residuals, fit)
TS.diag(fit$residuals)
par(mfrow = c(1,1))

Box.Ljung.Test(fit$residuals, lag = 100, )

# MAPE: Mean Absoluto percentage Error
mape <- function(serie, pred){
  return( mean(abs( (serie - pred)/serie )  )  )
}


datos2 = cbind(rbind(CO %>% filter(fecha >= "2023-01-01"), c("2023-10-01", NA)),
              temperatura %>% filter(fecha >= "2023-01-01") %>% select(temperatura))
datos2$CO = as.numeric(datos2$CO)
datos2$m = as.factor(month(datos2$fecha))

datos2 = datos2 %>% mutate(CO2 = ifelse(!is.na(CO), CO^(0.6), CO),
                           mes = ifelse(m %in% c("1", "2", "10", "11", "12", "9"), 
                                        "verano", m))

Y2 = ts(datos$temperatura, start = c(2023, 01), end = c(2023, 09), frequency = 12)
t2 = as.numeric(time(Y2))

datos2 = datos2[1:9,]

newdata = data.frame(t = t2,
                     mes = datos2$mes,
                     temperatura = datos2$temperatura)
predicciones = predict(modelo, newdata = newdata)


mape(datos$CO2, modelo$fitted.values)
mape(datos2$CO2, predicciones)


####

aux = CO %>% filter(fecha >= "2003-12-01")
Y3 = ts(aux$CO, start = c(2003, 12), frequency = 12)
t3 = as.numeric(time(Y3))

plot(aux$CO ~ t3, type = "l", ylab = "Niveles de CO", xlab = "",
     lwd = 2, las = 1, xaxt = "n", cex.lab = 1.5, xlim = c(2003, 2024))
axis(1, at = seq(2003, 2024, 1), labels = seq(2003, 2024, 1))
lines(modelo$fitted.values^(10/6) ~ t3[1:229], col = "#3388FF", lwd = 3)
lines(predicciones^(10/6) ~ t3[230:238], col = "#D02090", lwd = 3)
legend("topright", 
       legend = c("CO", "Valores ajustados", "Predicción"), 
       col = c("black", "#3388FF", "#D02090"), 
       lwd = 2, 
       horiz = F)

#


### splines

library(splines)
mod = lm(CO ~ bs(t, knots = c(2003.917, 2009, 2014, 2020, 2022.917), degree = 10), datos)
plot(datos$CO ~ t, type = "l")
lines(mod$fitted.values ~ t, col = "red", lwd = 2)
plot(mod$residuals, type = "l")
abline(h = 0, lty = 2, lwd = 2)

grafico <- ggplot() + 
  geom_line(aes(x = t, y = datos$CO)) +
  geom_line(aes(x = t, y = mod$fitted.values)) +
  theme_minimal() 

plotly::ggplotly(grafico)

#

# ALONSO ------------------------------------------------------------------

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

plot(datos$co, datos$temperatura)

#datos$tiempo <- time(datos$co)


# Modelos Alonso -----------------------------------------------------------------
## Modelo de regresion simple ----------------------------------------------

BoxCox.lambda(datos$co, method = "guerrero")
BoxCox.lambda(datos$co, method = "loglik")
MASS::boxcox(lm(datos_co2$co ~ 1))

datos$co2 <- log(datos$co)
modelo_full <- lm(co2 ~ viento + humedad + temperatura, data = datos)

mod_step = step(modelo_full, direction = "backward")
summary(mod_step)

modelo_lm = lm(co2 ~ viento + temperatura, data = datos)
summary(modelo_lm)


plot(datos$tiempo, datos$co2, type = "l")
lines(mod_step$fitted.values ~ datos$tiempo, col = "red")

acf(modelo_lm$residuals, lag.max = 100)

shapiro.test(modelo_lm$residuals)


## Modelo con cuadratico -------------------------------------------------------

datos$co2 <- (datos$co)^(0.5)

modelo_c <- lm(co2 ~ tiempo + I(tiempo^2) + humedad * tiempo + viento + mes, 
                    data = datos)

summary(modelo_c)

plot(datos$tiempo, datos$co2, type = "l")
lines(modelo_c$fitted.values ~ datos$tiempo, col = "red")


acf(modelo_c$residuals, lag.max = 100)

shapiro.test(modelo_c$residuals)

bc <- MASS::boxcox(modelo_c)

bc$x[which.max(bc$y)]


## Modelo con spline -------------------------------------------------------

datos$co2 <- (datos$co)^(0.5)

# Modelo con spline
modelo_spline <- lm(co2 ~ tiempo *  I(tiempo^2) + humedad + viento + mes, 
                    data = datos)

summary(modelo_spline)

plot(datos$tiempo, datos$co2, type = "l", xlim = c(80, 317))
lines(modelo_spline$fitted.values ~ datos$tiempo, col = "red")
abline(v= 204)


acf(modelo_spline$residuals, lag.max = 100)

shapiro.test(modelo_spline$residuals)

bc <- MASS::boxcox(modelo_spline)

bc$x[which.max(bc$y)]

###

## Modelo  pruebas -------------------------------------------------------

datos$co2 <- (datos$co)^(0.5)

# Modelo con spline
modelo <- lm(co2 ~ humedad * I(tiempo^2) + viento * I(tiempo^2) + mes + tiempo + I(tiempo^2) , 
                    data = datos)

# Modelo con spline
modelo <- lm(co2 ~ humedad + I(tiempo^2) + tiempo + viento + temperatura + mes, 
             data = datos)


?cor
summary(modelo)

plot(datos$tiempo, datos$co2, type = "l")
lines(modelo$fitted.values ~ datos$tiempo, col = "red")

plot(datos$temperatura, type = "l")
plot(scale(datos$viento), type = "l")
plot()

plot(modelo$residuals, type = "l")
abline(v = 60)
abline(v = 126)
abline(v = 140)
plot(diff(modelo$residuals, lag = 1), type = "l")

acf(modelo$residuals, lag.max = 100)

shapiro.test(modelo$residuals)

ks.test(scale(modelo$residuals), pnorm)

lmtest::bptest(modelo)

bc <- MASS::boxcox(modelo)

bc$x[which.max(bc$y)]


a <- diff(modelo$residuals, lag = 1)

acf(a)
shapiro.test(a)
ks.test(scale(a), pnorm)

lmtest::bptest(lm(a ~ 1))


hist(a, breaks = 30)

LSTS::periodogram(a)

###

# MODELO ARIMA

modelo_arima <- forecast::auto.arima(datos$co, 
                                     xreg = as.matrix(select(datos, c(humedad, temperatura))),
                                     lambda = 0)

modelo_arima <- forecast::auto.arima(modelo_spline$residuals)

modelo_arima$residuals

modelo_arima$residuals |> acf(lag.max = 100)

hist(modelo_arima$residuals)
curve(dnorm(x), from = -3, to = 3, add = TRUE)

qqnorm(modelo_arima$residuals)
qqline(modelo_arima$residuals)

shapiro.test(modelo_arima$residuals)

ks.test(scale(modelo_arima$residuals), pnorm)



