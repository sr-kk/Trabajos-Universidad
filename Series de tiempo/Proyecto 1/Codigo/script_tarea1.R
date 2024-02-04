# librerias utilizadas
library(tidyverse)
library(lubridate)
library(stringr)
library(forecast)

source("Tareas/Tarea 1/Codigo/summary.arima.R")
source("Tareas/Tarea 1/Codigo/TS.diag.R")
source("Tareas/Tarea 1/Codigo/salida.arima.R")

# O2 --------------------------------------------------------------------

# Cargamos los datos
O2 <- readr::read_delim("Tareas/Tarea 1/Datos/ParqueOHiggins-O2.csv", delim = ";")

#skimr::skim(datos)

# Seleccionamos las variables de interés
O2 <- O2 %>% 
  mutate(O2 = case_when(!is.na(`Registros validados`) ~ as.numeric(gsub(",", ".", `Registros validados`)),
                        TRUE ~ NA)) %>% 
  rename(fecha = `FECHA (YYMMDD)`) %>% 
  mutate(fecha = lubridate::ymd(fecha)) %>% 
  select(fecha, O2) %>% 
  filter(fecha <= "2022-12-01")


# Imputamos datos faltantes mediante interpolación lineal

# Encuentra los índices de los datos faltantes
indices_faltantes = which(is.na(O2$O2))

# Imputa los valores faltantes utilizando interpolación lineal
O2$O2[indices_faltantes] = approx(seq_along(O2$O2), O2$O2, xout = indices_faltantes)$y


## Creamos objeto ts()
Y = ts(O2$O2, start = c(1997, 05), end = c(2022, 12), frequency = 12)
plot.ts(Y)
t = as.numeric(time(Y))
#

# Eliminamos tendencias ---------------------------------------------------

## Regresion lineal
mod1 = lm(Y ~ t)
summary(mod1) # beta 1 significativo

plot.ts(Y)
lines(mod1$fitted.values ~ t, col = "red", lwd = 2) 

res1 = mod1$residuals
plot(res1, type = "l")
abline(h = 0, lty = 2, lwd = 2)

## Regresion polinomial
mod2 = lm(Y ~ t + I(t^2))
summary(mod2)

plot.ts(Y)
lines(mod2$fitted.values ~ t, col = "red", lwd = 2) 

res2 = mod2$residuals
plot(res2, type = "l")
abline(h = 0, lty = 2, lwd = 2)




## Regresion spline
library(splines)
mod31 = lm(Y ~ bs(t)) # Ajusta por defecto un polinomio cubico
mod32 = lm(Y ~ bs(t, knots = c(1997, 2016.917, 2022.917)))
mod33 = lm(Y ~ bs(t, knots = c(1997, 2016.917, 2022.917), degree = 2))

plot.ts(Y)
lines(mod31$fitted.values ~ t, col = "red", lwd = 2)
lines(mod32$fitted.values ~ t, col = "blue", lwd = 2)
lines(mod33$fitted.values ~ t, col = "green3", lwd = 2)


AIC(mod1) 
AIC(mod2) # Mejor modelo reg cuadratica
AIC(mod31)
AIC(mod32)
AIC(mod33)

plot(res2, type = "l")



# Corregimos estacionaLidad -----------------------------------------------

acf(res2)
pacf(res2)

# Miramos estacionaLidad
per <- LSTS::periodogram(Y)
index = which(per$periodogram  == max(per$periodogram))
d_p = round((2*pi)/per$lambda[index]) # d = 12

acf(res2, lag = 100)
abline(v = c(1:5)*d_p, col = "red")

######-

d = ndiffs(Y) # se debe diferenciar con respecto al anterior inmediato
D = nsdiffs(Y) # y con respecto al del año anterior

lambda1 <- BoxCox.lambda(Y, method = "guerrero")
lambda2 <- BoxCox.lambda(Y, method = "loglik")
MASS::boxcox(lm(Y ~ 1))

fit = auto.arima(Y, lambda = 0, d = d, D = D)
salida.arima(Y, fit)
TS.diag(fit$res)
acf(c(fit$res), lag.max = 24, main = "")

 
# lags 6, 10, 11 sobresalen en ACF y sar1 no es significativo
fixed = c(NA, rep(0,5), NA, rep(0, 3), NA, NA, NA, NA)
fit = Arima(Y, order = c(1,1,11), seasonal = c(1,1,1), lambda = lambda1, fixed = fixed)
salida.arima(Y, fit)
TS.diag(fit$res)

# Significativo -> ar1, ma6, ma11, sma1




#




## Vemos si necesita transformación
lambda1 = forecast::BoxCox.lambda(Y, method = "guerrero")
lambda2 = forecast::BoxCox.lambda(Y, method = "loglik")




par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(forecast::BoxCox(Y, lambda = lambda1), col = "steelblue", lwd = 2, 
     xaxt = "n", ylab = "", main = "Transformacionn Box Cox - PM10 Estación Parque O'Higgins", 
     xlab = "")
axis(1, 1997:2022)


## Ordenes de ingtegración
d = forecast::ndiffs(res2)
D = forecast::nsdiffs()

## forecast::auto.arima
fit = forecast::auto.arima(Y, lambda = lambda)
summary.arima(fit)

TS.diag(fit$residuals)



# Variables exógenas ------------------------------------------------------

## Humedad -----------------------------------------------------------------

# cargamos de humedad
humedad <- readr::read_delim("Tareas/Tarea 1/Datos/Humedad.csv", delim = ";")

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

# Fecha inicio : Diciembre 2003
# Fecha termino : Octubre 2023

plot(humedad$humedad ~ humedad$fecha, type = "l")

skimr::skim(humedad)

## Viento -----------------------------------------------------------------

# cargamos de humedad
viento <- readr::read_delim("Tareas/Tarea 1/Datos/viento.csv", delim = ";")

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

# Fecha inicio : Diciembre 2003
# Fecha termino : Octubre 2023

plot(viento$viento ~ viento$fecha, type = "l")

# Imputamos datos faltantes mediante interpolación lineal
# Encuentra los índices de los datos faltantes
indices_faltantes = which(is.na(viento$viento))
# Imputa los valores faltantes utilizando interpolación lineal
viento$viento[indices_faltantes] = approx(seq_along(viento$viento), viento$viento, xout = indices_faltantes)$y

plot(viento$viento ~ viento$fecha, type = "l")

skimr::skim(humedad)

## Temperatura -----------------------------------------------------------------

# cargamos de temperatura
temperatura <- readr::read_delim("Tareas/Tarea 1/Datos/Temperatura.csv", delim = ";")

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

# Fecha inicio : Diciembre 2003
# Fecha termino : Octubre 2023

plot(temperatura$temperatura ~ temperatura$fecha, type = "l")

skimr::skim(temperatura)

# Correlaciones -----------------------------------------------------------



datos = cbind(O2 %>% filter(fecha >= "2003-12-01"),
              humedad %>% filter(fecha <= "2022-12-01") %>% select(humedad),
              viento %>% filter(fecha <= "2022-12-01") %>% select(viento),
              temperatura %>% filter(fecha <= "2022-12-01") %>% select(temperatura))

dim(datos)
cor(datos[,2:5])

plot(datos$O2 ~ datos$humedad)
plot(datos$O2 ~ datos$viento)
plot(datos$O2 ~ datos$temperatura)

plot(datos$O2, type = "l")
plot(datos$humedad, type = "l")
plot(datos$viento, type = "l")
plot(datos$temperatura, type = "l")

Y = ts(datos$O2, start = c(2003, 12), end = c(2022, 12), frequency = 12)
t = as.numeric(time(Y))


mod1 = lm(O2 ~ humedad + viento + temperatura, datos)
summary(mod1)

mod_step = step(mod1, direction = "backward")
summary(mod_step)

plot(datos$O2 ~ t, type = "l")
lines(mod_step$fitted.values ~ t, col = "red")

TS.diag(mod_step$residuals)


d = ndiffs(mod_step$residuals)
D = nsdiffs(mod_step$residuals)

fit = auto.arima(mod_step$residuals)
salida.arima(mod_step$res, fit)
TS.diag(fit$residuals)
par(mfrow = c(1,1))

#linealidad
plot(mod_step, 1)

#normalidad
nortest::lillie.test(mod_step$residuals)





# Transformación de los datos ---------------------------------------------

# cambiamos el nombre de la variable para hacer joins
datos_pm2 <- datos_pm |> 
  select(-fecha) |> 
  rename(fecha = fecha2) 

# realizamos join con humedad, cambiamos el nombre de la variable, el mes lo
# transformamos a factor y creamos una columna de estaciones
datos <- inner_join(datos_pm2, humedad, by = "fecha") |> 
  rename(humedad = promedio_mensual) |> 
  mutate(mes = as.factor(mes))|> 
  mutate(estacion = case_when(mes %in% c("4", "5", "6") ~ "otonio",
                              mes %in% c("7", "8", "9") ~ "invierno",
                              mes %in% c("10", "11", "12") ~ "primavera",
                              TRUE ~ "verano"))

# realizamos join con fecha y cambiamos el nombre de la variable
datos <- inner_join(datos, temperatura, by = "fecha") |> 
  rename(temperatura = promedio_mensual)

# realizamos join conv viento y cambiamos el nombre de la variable
datos <- inner_join(datos, viento, by = "fecha") |> 
  rename(viento = promedio_mensual)

## DE AQUI EN ADELANTE FUI PROBANDO COSAS

# grafico de dispersion respuesta viento
plot(datos$pm10, datos$viento)

# AQUI AJUSTO MODELOS LM PARA PROBAR
modelo_lm <- lm(pm10 ~ mes, data = datos)

summary(modelo_lm)

plot(datos$fecha, modelo_lm$residuals, type = "l")

hist(modelo_lm$residuals)
# COMENTARIO: LOS RESIDUOS SE COMPORTAN RARO

datos |> 
  ggplot() +
  geom_histogram(aes(y = pm10, group = mes, fill = mes))

smooth_spline <- smooth.spline(y = datos$pm10, x = cbind(time(datos$fecha)),
              cv = TRUE)

plot(datos$fecha, datos$pm10 - smooth_spline$y, type = "l")

hist(datos$pm10 - smooth_spline$y)
  
a <- datos |> 
  mutate(aux = fecha - month(1)) |> 
  mutate(aux = ymd(ifelse(month(aux) / 10 >= 1, str_c(year(aux), month(aux), "01"),
                      str_c(year(aux),"0", month(aux), "01"))))

plot(datos$pm10, lag(datos$viento))

lag(humedad)


# Test --------------------------------------------------------------------


MASS::boxcox(modelo_lm) # pasa bien

shapiro.test(modelo_lm$residuals) # no pasa

ks.test(scale(modelo_lm$residuals), pnorm) # no pasa

acf(modelo_lm$residuals) # se comporta más o menos bien

# observaciones rechaza normalidad
# hay una tendencia en la media
# ver periodogramas

