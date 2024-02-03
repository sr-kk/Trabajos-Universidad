# librerias
library(tidyverse)

# cargamos las funciones de R
source("Tareas/Tarea 2/Bartlett.R")
source("Tareas/Tarea 2/summary.arima.R")
source("Tareas/Tarea 2/Durbin_Levinson.R")
source("Tareas/Tarea 2/TS.diag.R")

source("Tarea/Tarea2/funciones_print.R")


# Una vez cargados los datos realizamos los diagnósticos usuales
datos <- chil013.crn.noaa

# 1607

datos <- chil013.crn.noaa |> filter(age_CE > 1605)

par(mfrow = c(3,1))
plot(datos$age_CE, datos$trsgi, type = "l", las = 1,xlab = "Año", ylab = "ICR", lwd =2)
acf(datos$trsgi, main = "", lwd =2)
pacf(datos$trsgi, main = "", lwd = 2)

# fitted escala original


# Analisis Exploratorio ---------------------------------------------------



## Media constante ---------------------------------------------------------

# No se observa una tendencia, para estar más seguros se aplica una regresión
# suaviazada

g1 <- datos |> 
  ggplot(aes(age_CE, trsgi)) +
  geom_line() +
  geom_smooth()

plotly::ggplotly(g1)

# se obsreva que existe una tendencia al final de la serie utilizaremos
# un test para estar + seguros

fit <- lm(trsgi ~ age_CE, datos)

summary(fit)

# el año no es significativo como predictor por lo que se puede decir que la media
# es constante


# Varianza constante ------------------------------------------------------

lmtest::bptest(fit)

# observamos que la varianza es constante

# también realizamos el método de boxcox
bc <- MASS::boxcox(fit)
bc$x[which.max(bc$y)]

# El método sugiere una transformacion raiz

datos$trsgi2 <- (datos$trsgi)^0.5

# Repetimos el test breuch-pagan
fit2 <- lm(trsgi2 ~ age_CE, datos)
lmtest::bptest(fit2)
# Vemos que disminuye el valor-p pero se mantiene la conclusion

plot(datos$trsgi2 ~ datos$age_CE, type = "l")


## Normalidad --------------------------------------------------------------

shapiro.test(datos$trsgi2)
ks.test(scale(datos$trsgi2), "pnorm")

qqnorm(datos$trsgi2)
qqline(datos$trsgi2)

car::qqPlot(datos$trsgi2)

# Modelo ARMA --------------------------------------------------------------


MASS::boxcox(lm(datos$trsgi  ~ datos$age_CE))

(lambda <- forecast::BoxCox.lambda(datos$trsgi, method = "guerrero"))

forecast::BoxCox.lambda(datos$trsgi, method = "loglik")

## Ordenes de integración
d <- forecast::ndiffs(datos$trsgi) 
D <- forecast::nsdiffs(datos$trsgi) 

d 

# ajustamos los datos con auto.arima
fit_ts <- forecast::auto.arima(datos$trsgi,
                               lambda = 0.5
                               )

summary(fit_ts)
# entrega un ARMA(3, 2)

summary.arima(fit_ts)

TS.diag(fit_ts$residuals)
# ljung-box se empieza a caer en 5 y 7 

# veamos los graficos para mejorar los resultados
par(mfrow = c(1,1))
plot(fit_ts$residuals ~ datos$age_CE, type = "l")
acf(fit_ts$residuals, lag.max = 100)
pacf(fit_ts$residuals, lag.max = 100)

# opciones de mejora 5 y 7
fixed_ts <- c(NA,   # AR
              0, 0 , 0, 0, NA, NA, NA # MA
              )

fit2_ts <- forecast::Arima(datos$trsgi, 
                           order = c(1,0,7), 
                           fixed = fixed_ts, 
                           lambda = 0.5,
                           include.mean = F)

summary_arima(fit2_ts, fixed_ts)
TS.diag(fit2_ts$residuals)


fixed_ts <- c(NA,   # AR
              rep(0,6), NA # MA
)

fit3_ts <- forecast::Arima(datos$trsgi, 
                           order = c(1,0,7), 
                           fixed = fixed_ts, 
                           lambda = 0.5,
                           include.mean = F)

summary_arima(fit3_ts, fixed_ts)
TS.diag(fit3_ts$residuals)

par(mfrow = c(1,1))
plot(fit3_ts$residuals ~ datos$age_CE, type = "l")
acf(fit3_ts$residuals, lag.max = 100)
pacf(fit3_ts$residuals, lag.max = 100)


fixed_ts <- c(NA, rep(0, 47), NA,   # AR
              rep(0,6), NA, rep(0, 22), NA # MA
)

fit4_ts <- forecast::Arima(datos$trsgi, 
                           order = c(49,0,30), 
                           fixed = fixed_ts, 
                           lambda = 0.5,
                           include.mean = F)

summary_arima(fit4_ts, fixed_ts)
TS.diag(fit4_ts$residuals)

par(mfrow = c(1,1))
plot(fit4_ts$residuals ~ datos$age_CE, type = "l")
acf(fit4_ts$residuals, lag.max = 100)
pacf(fit4_ts$residuals, lag.max = 100)


fixed_ts <- c(NA,  # AR
              rep(0,6), NA, rep(0, 28), NA # MA
)

fit5_ts <- forecast::Arima(datos$trsgi, 
                           order = c(1,0,36), 
                           fixed = fixed_ts, 
                           lambda = 0.5,
                           include.mean = F)

summary_arima(fit5_ts, fixed_ts)
TS.diag(fit5_ts$residuals)


## Significancia -----------------------------------------------------------

summary_arima(fit4_ts, fixed = fixed_ts)

summary_coef(summary_arima(fit2_ts, fixed = fixed_ts),
             caption = "Tabla de resumen coeficientes modelo ARMA(1,7)")


## Estacionariedad ---------------------------------------------------------

lm(fit5_ts$residuals ~ datos$age_CE) |> summary()

datos$age_CE <- as.numeric(datos$age_CE)

plot(fit4_ts$residuals ~ datos$age_CE, type = "l", 
     xlab = "Año", ylab = "Residuos")


## Invertibilidad ----------------------------------------------------------

plot(fit4_ts)


## Blancura ----------------------------------------------------------------

LSTS::Box.Ljung.Test(fit4_ts$residuals, lag = 100)
#LSTS::Box.Ljung.Test(fit_ts$residuals, lag = 20)

## Homocedasticidad --------------------------------------------------------

#lmtest::bptest(lm(fit_ts$residuals ~ datos$age_CE))

lmtest::bptest(lm(fit5_ts$residuals ~ datos$age_CE))


## Normalidad --------------------------------------------------------------

shapiro.test(fit5_ts$residuals)

hist(fit5_ts$residuals)

ks.test(scale(fit5_ts$residuals), "pnorm")

qqnorm(fit2_ts$residuals)
qqline(fit2_ts$residuals)

car::qqPlot(fit5_ts$residuals)

moments::kurtosis(fit5_ts$residuals)

## ACF empirico v/s estimado -----------------------------------------------

n <- length(datos$trsgi)

acf(datos$trsgi, ylim = c(-1,1), lag.max = 40)

acf_teo <- ARMAacf(ar = fit4_ts$coef[1:49], ma = fit4_ts$coef[50:79], lag.max = 40)

lines(acf_teo ~ c(0:40), col = "orange", type = "p", pch = 20)

w <- c(0, Bartlett(ar = fit4_ts$coef[1:49], ma = fit4_ts$coef[50:79], lag.max = 40))

li <- acf_teo - qnorm(0.975) * sqrt(w/n)
ls <- acf_teo + qnorm(0.975) * sqrt(w/n)

lines(li ~ c(0:40), lty = 2, col = "red")
lines(ls ~ c(0:40), lty = 2, col = "red")

## Periodograma vs Densidad Espectral --------------------------------------

aux <- LSTS::periodogram(datos$trsgi)

plot((aux$periodogram ~ aux$lambda), type = "l", col = "gray", xlab = expression(lambda), ylab = expression(I(lambda)), xlim = c(0,pi), xaxt = "n")
axis(1, seq(0,pi,pi/4), c(0,expression(pi/4),expression(pi/2),expression(3*pi/4),expression(pi)))

f <- LSTS::spectral.density(ar = fit4_ts$coef[1:49], ma = fit4_ts$coef[50:79] , sd = sqrt(fit4_ts$sigma2), lambda = aux$lambda)
lines(f ~ aux$lambda)

aux2 <- LSTS::smooth.periodogram(datos$trsgi, spar = 0.8, plot = F)
lines(aux2$smooth.periodogram ~ aux2$lambda, col = "black", lwd = 1, lty = 2)


## Durbin Levison ---------------------------------------------------------

# hay problemas con el durbin levinson debería ajustar en la escala original

fit_dl <- DurbinLevinson(serie = (datos$trsgi) - mean((datos$trsgi)), ar = fit2_ts$coef[1], 
                         ma = fit2_ts$coef[2:8])

aux <- forecast::Arima(datos$trsgi, order = c(1,0,7), lambda = lambda, fixed = fixed_ts)

plot(datos$trsgi ~ datos$age_CE, type = "l")
plot(fit_dl$fitted ~ datos$age_CE, col = "blue")
lines(forecast::InvBoxCox( fit2_ts$fitted, lambda = lambda) ~ datos$age_CE, type = "l", col = "red")

plot(datos$trsgi ~ datos$age_CE, type = "l")
lines((datos$trsgi - fit_dl$fitted )~ datos$age_CE, col = "red")

lines(fit2_ts$fitted ~ datos$age_CE, col = "blue")
lines(forecast::InvBoxCox(fit_dl$fitted, lambda = lambda) ~ datos$age_CE, col = "red")
lines(fit_dl$fitted + mean(datos$trsgi) ~ datos$age_CE, col = "red")


dl.loglik = function(x, serie, p = 1, q = 1){
  ar  = c(x[1], rep(0, 47), x[2])
  ma = c(rep(0,6), x[3], rep(0, 22), x[4])
  fit   = DurbinLevinson(serie = serie, ar = ar, ma = ma)
  e     = serie-fit$fitted
  nu    = fit$nu 
  aux = 0.5*(sum(log(fit$nu)) +  sum(e^2/fit$nu))
  if(is.nan(aux)){aux = Inf}
  aux
}


initial_parms = c(0.6, -0.1, 0.21, -0.11)
nlminb(start = initial_parms, 
       objective = dl.loglik, 
       serie = datos$trsgi^0.5 - mean(datos$trsgi^0.5),
       p = 49, q = 30)
summary_arima(fit4_ts, fixed_ts)

## Whittle -----------------------------------------------------------------

whittle.loglik = function(x, serie, p = 1, q = 1){
  Y = serie
  n = length(Y)
  #ar  = x[1:2]
  #ma  = x[3:4]
  ar  = c(x[1], rep(0, 47), x[2])
  ma = c(rep(0,6), x[3], rep(0, 22), x[4])
  #print(paste("Parámetros actuales:", "ar:", ar, "ma7:", ma[1], "ma30:", ma[2], "sigma:", sigma))
  sigma = x[5]
  aux = LSTS::periodogram(Y, plot = F)
  I = aux$periodogram
  lambda = aux$lambda
  f = LSTS::spectral.density(ar = ar, ma = ma, sd = sigma, lambda = lambda)
  aux = 0.5*(sum(log(f)) + sum(I/f))/n
  aux
}

initial_parms = c(0.6, -0.1, 0.21, -0.11, 0.14)
nlminb(start = initial_parms, 
       objective = whittle.loglik, 
       serie = datos$trsgi^0.5 - mean(datos$trsgi^0.5),
       p = 49, q = 30)$par

summary_arima(fit4_ts, fixed_ts)[,1]
sqrt(summary(fit4_ts)$sigma2) # sigma




