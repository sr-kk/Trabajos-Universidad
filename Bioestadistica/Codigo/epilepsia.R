# Librerias
library(tidyverse)
library(dplyr)
library(lattice)
library(kableExtra)

library(readr)
epilepsia_final = read_csv("Desktop/Bioestadistica/Proyecto/epilepsia_hd.csv")
View(epilepsia_final)
require(reshape)
epilepsia_final = rename(epilepsia_final, c('VAR1' = 'ID'))


# Analisis MCAR -----------------------------------------------------------

## Analisis grafico --------------------------------------------------------

### Proporcion tratamiento en ambas bases de datos
library(reshape)
df = data.frame(Fuente = c('Datos completos', 'Datos perdidos'),
                tratamiento = c(0.53, 0.5),
                placebo = c(0.47, 0.5))
dfp1 <- melt(df)

dfp1 %>% ggplot(aes(x=Fuente, y = value, fill=variable)) +
  geom_bar(stat='identity', position = 'dodge') +
  ggtitle('Proporción de tratados y no tratados por cada fuente de datos')+
  ylab('Porcentaje') 


### Comparacion variable base
plot.new()
# Fondo gris claro
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
     col = "#ebebeb")

# Añadimos un grid blanco
grid(nx = NULL, ny = NULL, col = "white", lty = 1,
     lwd = par("lwd"), equilogs = TRUE)
# Boxplot
par(new = TRUE)

boxplot(datos_completos$base, datos_faltantes$base,
        col = c('#009ACD', '#00688B'),
        xlab = "Fuente de datos",  # Etiqueta eje X
        ylab = "Ataques",  # Etiqueta eje Y
        main = "Número de ataques previo al estudio para datos 
        completos y faltantes" # Título
)
legend("topright", legend = c("Datos completos", "Datos perdidos"), # Posición y título
       fill = c('#009ACD', '#00688B'),  # Color
       inset = c(0.03, 0.05), # Cambiamos los márgenes
       bg = "white") # Color de fondo de la leyenda


### Comparacion variable age
plot.new()
# Fondo gris claro
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
     col = "#ebebeb")

# Añadimos un grid blanco
grid(nx = NULL, ny = NULL, col = "white", lty = 1,
     lwd = par("lwd"), equilogs = TRUE)
# Boxplot
par(new = TRUE)
boxplot(datos_completos$age, datos_faltantes$age,
        col = c('#009ACD', '#00688B'),
        xlab = "Fuente de datos",  # Etiqueta eje X
        ylab = "Edad",  # Etiqueta eje Y
        main = "Edad de los sujetos para cada fuente de datos" # Título
)
legend("topright", legend = c("Datos completos", "Datos perdidos"), # Posición y título
       fill = c('#009ACD', '#00688B'),  # Color
       inset = c(0.03, 0.05), # Cambiamos los márgenes
       bg = "white") # Color de fondo de la leyenda


summary(datos_completos[,6:9])
summary(datos_faltantes[,6:9])


## Analisis estadistico ----------------------------------------------------

# Resumen variables para datos completos y faltantes
k = matrix(c(0, 0, 1, 0.533, 1, 1, 
             1.792, 2.565, 3.178, 3.202, 3.714, 5.017,
             18, 24, 29, 29.42, 35, 57,
             0, 0, 0.5, 0.5, 1, 1,
             1.946, 2.420, 2.943, 3.003, 3.707, 4.025,
             19, 21.5, 25.5, 27, 30, 42),
           ncol = 6)
rownames(k) = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")
colnames(k) = c('trt', 'log base', 'age', 'trt', 'log base', 'age')

kable(k, format = 'latex', longtable = T, booktabs = T, caption = "Resumen datos") %>%  
  add_header_above(c(" ", "Datos completos" = 3, "Datos faltantes" = 3)) %>%
  kable_styling(latex_options = c('striped' ,"repeat_header"))



## Analisis modelamiento  --------------------------------------------------

epilepsia = epilepsia_final %>% 
  mutate(obs = ifelse(is.na(y1)|is.na(y2)|is.na(y3)|is.na(y4), 1, 0))

View(cor(epilepsia))

epilepsia$obs = as.factor(epilepsia$obs)

base_model = glm(obs ~ 1, data = epilepsia, family = binomial(link = "cloglog"))
full_model = glm(obs ~ trt+age+base, data = epilepsia, family = binomial(link = 'cloglog'))
step(base_model, 
     scope=list(lower=formula(base_model),upper=formula(full_model)),
     direction="both", trace = T)

# Mediante la funcion step se concluye que no existe ninguna variable significativa que 
# logre explicar el comportamiento de los datos perdidos a nivel general

# A continuacion se estudiara si existe alguna relacion entre los datos perdidos en cada
# semana con las variables que se han estado utilizando


datos_faltantes2 = datos_faltantes

datos_faltantes2$y1[!is.na(datos_faltantes2$y1)] = 0
datos_faltantes2$y1[is.na(datos_faltantes2$y1)] = 1

datos_faltantes2$y2[!is.na(datos_faltantes2$y2)] = 0
datos_faltantes2$y2[is.na(datos_faltantes2$y2)] = 1

datos_faltantes2$y3[!is.na(datos_faltantes2$y3)] = 0
datos_faltantes2$y3[is.na(datos_faltantes2$y3)] = 1

datos_faltantes2$y4[!is.na(datos_faltantes2$y4)] = 0
datos_faltantes2$y4[is.na(datos_faltantes2$y4)] = 1

datos_faltantes2$y1 = as.factor(datos_faltantes2$y1)
datos_faltantes2$y2 = as.factor(datos_faltantes2$y2)
datos_faltantes2$y3 = as.factor(datos_faltantes2$y3)
datos_faltantes2$y4 = as.factor(datos_faltantes2$y4)
datos_faltantes2$trt = as.factor(datos_faltantes2$trt)

datos_faltantes2 = datos_faltantes2 %>% select(-ID, -logbase)

# Estudiamos los modelos para cada semana
summary(glm(y1~trt+base+age, data=datos_faltantes2, family = binomial(link = "cloglog")))
summary(glm(y2~trt+base+age, data=datos_faltantes2, family = binomial(link = "cloglog")))
summary(glm(y3~trt+base+age, data=datos_faltantes2, family = binomial(link = "cloglog")))
summary(glm(y4~trt+base+age, data=datos_faltantes2, family = binomial(link = "cloglog")))

# Resultados: 
# - En ningun caso se tuvo alguna variable significativa que lograra explicar los datos faltantes

# Conclusion: Estamos bajo el supuesto de que los datos son MCAR




# Analisis metodos de imputacion ----------------------------------------------

## Funciones utiles --------------------------------------------------------
distancia_min = function(row, cluster){
  na = which(is.na(row)) # posicion variable con dato faltante
  dist_min = 1000
  
  for (i in 1:nrow(cluster)){
    dist_temp = abs(row$logbase - cluster$logbase[i]) + abs(row$age - cluster$age[i])
    
    if (dist_temp < dist_min){
      dist_min = dist_temp
      row_cluster = cluster[i,]
    }
  }
  
  new_value = row_cluster[,na]
  
  return(new_value)
}

input = function(data, method){
  # Funcion que se encarga de imputar los valores faltantes
  # Argumentos:
  #   data : dataframe con datos observados y no observados
  #   method : metodo que se quiere para imputar los datos
  #
  # Retorna un dataframe con los datos imputados
  
  datos_faltantes = data %>% 
    filter(is.na(y1) | is.na(y2) | is.na(y3) | is.na(y4))
  
  datos_completos = data %>% filter(!ID %in% datos_faltantes$ID)
  
  sd_base = 1 # valor tentativo
  sd_age = 5 # valor tentativo
  
  if (method == "mean"){
    datos_faltantes$y1[is.na(datos_faltantes$y1)] = round(mean(datos_completos$y1), 0)
    datos_faltantes$y2[is.na(datos_faltantes$y2)] = round(mean(datos_completos$y2), 0)
    datos_faltantes$y3[is.na(datos_faltantes$y3)] = round(mean(datos_completos$y3), 0)
    datos_faltantes$y4[is.na(datos_faltantes$y4)] = round(mean(datos_completos$y4), 0)
  }
  if (method == "median"){
    datos_faltantes$y1[is.na(datos_faltantes$y1)] = round(median(datos_completos$y1), 0)
    datos_faltantes$y2[is.na(datos_faltantes$y2)] = round(median(datos_completos$y2), 0)
    datos_faltantes$y3[is.na(datos_faltantes$y3)] = round(median(datos_completos$y3), 0)
    datos_faltantes$y4[is.na(datos_faltantes$y4)] = round(median(datos_completos$y4), 0)
  }
  if (method == "hotdeckal"){
    for (i in 1:nrow(datos_faltantes)){
      datos = datos_faltantes[i,] # fila con dato faltante
      na = which(is.na(datos)) # posicion variable con dato faltante
      
      cluster = datos_completos %>% 
        filter(trt == datos$trt, 
               logbase <= datos$logbase + sd_base & logbase >= datos$logbase - sd_base,
               age <= datos$age + sd_age & age >= datos$age - sd_age)
      
      rndm_row = cluster[sample(nrow(cluster), size = 1), ]
      datos_faltantes[i,na] = rndm_row[na]
    }
  }
  if (method == "hotdeckmin"){
    for (i in 1:nrow(datos_faltantes)){
      datos = datos_faltantes[i,] # fila con dato faltante
      na = which(is.na(datos)) # posicion variable con dato faltante
      
      cluster = datos_completos %>% 
        filter(trt == datos$trt, 
               logbase <= datos$logbase + sd_base & logbase >= datos$logbase - sd_base,
               age <= datos$age + sd_age & age >= datos$age - sd_age)
      
      input_value = distancia_min(datos, cluster)
      datos_faltantes[i,na] = input_value
    }
  }
  if (method == "hotdeckmean"){
    for (i in 1:nrow(datos_faltantes)){
      datos = datos_faltantes[i,] # fila con dato faltante
      na = which(is.na(datos)) # posicion variable con dato faltante
      
      cluster = datos_completos %>% 
        filter(trt == datos$trt, 
               logbase <= datos$logbase + sd_base & logbase >= datos$logbase - sd_base,
               age <= datos$age + sd_age & age >= datos$age - sd_age)
      
      input_value = round(mean(t(cluster[,na])))
      datos_faltantes[i,na] = input_value
    }
    
  }
  
  epilepsia_final = rbind(datos_completos, datos_faltantes)
  
  return(epilepsia_final)
}



# Medias y varianzas de los datos completos
y_sum = apply(datos_completos[,2:5], 1, sum) 
means = c(mean(datos_completos$y1), mean(datos_completos$y2), 
          mean(datos_completos$y3), mean(datos_completos$y4),
          mean(y_sum))

sds = c(sd(datos_completos$y1), sd(datos_completos$y2), 
        sd(datos_completos$y3), sd(datos_completos$y4),
        sd(y_sum))

# Medias y varianzas de los datos faltantes
new.data1 = input(epilepsia_final, "mean")
y_sum = apply(new.data1[,2:5], 1, sum) 

means1 = c(mean(new.data1$y1), mean(new.data1$y2), 
           mean(new.data1$y3), mean(new.data1$y4),
           mean(y_sum))
sds1 = c(sd(new.data1$y1), sd(new.data1$y2), 
         sd(new.data1$y3), sd(new.data1$y4),
         sd(y_sum))

new.data2 = input(epilepsia_final, "median")
y_sum = apply(new.data2[,2:5], 1, sum) 

means2 = c(mean(new.data2$y1), mean(new.data2$y2), 
           mean(new.data2$y3), mean(new.data2$y4),
           mean(y_sum))
sds2 = c(sd(new.data2$y1), sd(new.data2$y2), 
         sd(new.data2$y3), sd(new.data2$y4),
         sd(y_sum))

new.data3 = input(epilepsia_final, "hotdeckal")
y_sum = apply(new.data3[,2:5], 1, sum) 

means3 = c(mean(new.data3$y1), mean(new.data3$y2), 
           mean(new.data3$y3), mean(new.data3$y4),
           mean(y_sum))
sds3 = c(sd(new.data3$y1), sd(new.data3$y2), 
         sd(new.data3$y3), sd(new.data3$y4),
         sd(y_sum))

new.data4 = input(epilepsia_final, "hotdeckmin")
y_sum = apply(new.data4[,2:5], 1, sum) 

means4 = c(mean(new.data4$y1), mean(new.data4$y2), 
           mean(new.data4$y3), mean(new.data4$y4),
           mean(y_sum))
sds4 = c(sd(new.data4$y1), sd(new.data4$y2), 
         sd(new.data4$y3), sd(new.data4$y4),
         sd(y_sum))

new.data5 = input(epilepsia_final, "hotdeckmean")
y_sum = apply(new.data5[,2:5], 1, sum) 

means5 = c(mean(new.data5$y1), mean(new.data5$y2), 
           mean(new.data5$y3), mean(new.data5$y4),
           mean(y_sum))
sds5 = c(sd(new.data5$y1), sd(new.data5$y2), 
         sd(new.data5$y3), sd(new.data5$y4),
         sd(y_sum))



summary.means<-matrix(c(means, means1, means2, means3, means4, means5),ncol=6)

colnames(summary.means) = c("Media observada", "mean", "median",
                            "hotdeck al", 'hotdeck min', 'hotdeck mean')
rownames(summary.means) = c( 'Semana 1', 'Semana 2', 'Semana 3', 'Semana 4', 'Total')
summary.means

summary.sd = matrix(c(sds, sds1, sds2, sds3, sds4, sds5), ncol=6)
colnames(summary.sd) = c("Desv. est observada", "mean", "median",
                         "hotdeck al", 'hotdeck min', 'hotdeck mean')
rownames(summary.sd) = c( 'Semana 1', 'Semana 2', 'Semana 3', 'Semana 4', 'Total')
summary.sd

k = cbind(summary.means, summary.sd)

# Funcion para convertir una tabla a latex 
kable(summary.means, format = 'latex', booktabs = T, caption = "Comparación de medias para datos imputados") %>%  
  add_header_above(c(" ", " ", "Media datos imputados" = 5)) %>%
  kable_styling(latex_options = c('striped' ,"repeat_header", 'scale_down', 'hold_position'))


kable(summary.sd, format = 'latex', booktabs = T, caption = "Comparación desv.estandar para datos imputados") %>%  
  add_header_above(c(" ", " ", "Desv. estandar datos imputados" = 5)) %>%
  kable_styling(latex_options = c('striped' ,"repeat_header", 'scale_down', 'hold_position'))


# promedios de las diferencias de cada metodo

dif.means = round(c(mean(abs(means - means1)), mean(abs(means - means2)), mean(abs(means - means3)), 
                    mean(abs(means - means4)), mean(abs(means - means5))), 3)
dif.means
dif.sd = round(c(mean(abs(sds - sds1)), mean(abs(sds - sds2)), mean(abs(sds - sds3)),
                 mean(abs(sds - sds2)), mean(abs(sds - sds5))), 3)
dif.sd

matrix.dif = matrix(c(dif.means, dif.sd), nrow = 2, byrow = T)
matrix.dif

rownames(matrix.dif) = c('Media', 'Desv. estandar')
colnames(matrix.dif) = c('mean', 'median', 'hotdeck al', 'hotdeck min', 'hotdeck mean')
matrix.dif

kable(matrix.dif, format = 'latex', booktabs = T, caption = "Promedio entre la distancia del estimador y el parámetro") %>%  
  kable_styling(latex_options = c('hold_position'))


# Analisis, graficos y mas -----------------------------------------------

#imputacion por la media, mediana o hotdeck

# Aqui se puede elegir cualquier tipo de imputacion
epilepsia_final = input(epilepsia_final, method = "hotdeckmean")

View(epilepsia_final)
epilepsia_final[,3:6] #y1, y2, y3 ,y4

y_sum = apply(epilepsia_final[,3:6], 1, sum) 

y_mean = apply(epilepsia_final[,3:6], 1, mean) 

logbase4 = log(epilepsia_final$base)
logage_all = log(epilepsia_final$age)



# base Recuento inicial de convulsiones de 8 semanas
#modificamos nuestra tabla, para que nos quede por filas nuestra respuesta
#la respuesta es y1, y2, y3, y4
response = as.vector(t(epilepsia_final[,3:6])) # response vector

 #son 59 observaciones, y son 5 por individuos
n_obs = 59
#repetimos 4 veces cada item
ID = rep(epilepsia_final$ID, rep(4, n_obs))



trt = rep(epilepsia_final$trt, rep(4, n_obs)) 

semana = rep(1:4, n_obs) #indicador de en que semna se encuentr
semana = as.numeric(semana)
semana4 = rep(c(0, 0, 0, 1), n_obs) #indicador de la ultima semana

base = rep(epilepsia_final$base, rep(4, n_obs)) #dividimos 4 veces la base,
#pues es la cuenta total

age = rep(epilepsia_final$age, rep(4, n_obs)) #repetimos 5 veces

logbase = log(base) #aplicamos log para asegurar convergencia
logage = log(age) 
logbase4 = log(base/4)
#creamos un boxplot para verificar todo lo anterior


# Graficos de caja ---------------------------------------------------------

#boxplot por sujeto

boxplot(log(y_sum)~ epilepsia_final$trt, names = c('Placebo', 'Tratamiento'),
        ylab = 'Total (log)',
        xlab = 'Tratamiento',
        main = 'Ataques en un periodo de 8 semanas',
        col = 29)

#boxplot por media de sujeto
boxplot(y_mean ~ epilepsia_final$trt, names = c('Placebo', 'Tratamiento'),
        ylab = 'Media de ataques en un periodo de 8 semanas')


#boxplot por semana

boxplot(response ~ semana, names=c('semana1', 'semana2', 'semana3', 
                                   'semana4'),
        ylab= 'Numero de combulsiones por semana') 



library(lattice)
trt = as.factor(trt)
semana = as.factor(semana)

ltime = rep(c(2, 2, 2, 2), n_obs)
log_time = log(ltime)

semana_nva = as.numeric(semana)

data = data.frame(ID, response, semana,
                  semana4 , age, trt, logage, log_time, 
                  ltime, base, logbase4, semana_nva, logbase)
glimpse(data)
data <- data %>%
  mutate(trt_n = recode(trt, `0` = 'placebo', `1` = 'tratamiento'))

data = data %>% 
  mutate(
    # Create categories
    age_group = dplyr::case_when(
      age <= 26           ~ "18-26",
      age > 26 & age <= 32 ~ "27-32",
      age > 32 & age <= 39 ~ "33-39",
      age > 37             ~ "+40"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("18-26", "27-32","33-39", "+40")
    )
  )


# Graficos de linea  ------------------------------------------------------


data %>% 
  group_by(semana_nva, trt_n) %>% 
  summarise(mean_trt = mean(response, na.rm = TRUE)) %>% 
  ggplot(aes(x = semana_nva, y = mean_trt, color = trt_n)) +
  geom_line() +
  geom_point() +
  ggtitle('Valor medio por semana con y sin tratamiento') +
  xlab("semanas") +
  ylab("Media ataques de epilepsia")


data %>% 
  group_by(semana_nva) %>% 
  summarise(mean_trt = mean(response, na.rm = TRUE)) %>% 
  ggplot(aes(x = semana_nva, y = mean_trt)) +
  geom_line() +
  geom_point() +
  ggtitle('Valor medio por semana con y sin tratamiento') +
  xlab("semanas") +
  ylab("Media ataques de epilepsia")


# Graficos por edad --------------------------------------------------------

data %>% 
  ggplot(aes(x = semana,
             y = log(response + 1))) +
  geom_boxplot() +
  theme_bw() +
  facet_grid(.~ age_group) 

data %>% 
  ggplot(aes(x = semana,
             y = log(response + 1))) +
  geom_line(aes(group = ID)) +
  theme_bw() +
  facet_grid(.~ age_group) 
  

data %>% 
  ggplot(aes(x = semana,
             y = log(response + 1))) +
  geom_boxplot() +
  theme_bw() 

# Grafico de caja por tratamiento -----------------------------------------

#Sin normalizar
data %>% 
  ggplot(aes(x = semana,
             y = response)) +
  geom_boxplot() +
  theme_bw() +
  facet_grid(.~ trt_n)

#Normalizando
data %>% 
  ggplot(aes(x = semana,
             y = log(response + 1))) +
  geom_boxplot(color = "#0071BC", fill = "52DEFF") +
  theme_gray() +
  facet_grid(.~ trt_n) +
  ggtitle('Boxplot según estado del tratamiento') +
  xlab("semanas") +
  ylab("Ataques en escala log")

data %>% 
  ggplot(aes(x = semana,
             y = log(response + 1))) +
  geom_boxplot(color = "#0071BC", fill = "52DEFF") +
  theme_gray() +
  facet_grid(.~ trt_n) +
  ggtitle('Boxplot según estado del tratamiento') +
  xlab("semanas") +
  ylab("Ataques en escala log")


data %>% 
  ggplot(aes(
             y = log(response + 1))) +
  geom_boxplot(color = "#0071BC", fill = "52DEFF") +
  theme_gray() +
  facet_grid(.~ trt_n) +
  ggtitle('Ataques en un periodo de 8 semanas') +
  ylab("Ataques en escala log")

#responde = seziure
bwplot(response ~ semana|trt)


# Grafico de lineas  ------------------------------------------------------


matplot(1:4, t(epilepsia_final[,3:6]), type = 'l',
        lty = epilepsia_final$trt + 1, #se suma 1 pq es 0 o 1 
        xlab = 'Semana' , ylab = 'Número de ataques',
        main = 'Perfiles de sujetos durante 4 semanas ') 


matplot(1:4, log(t(epilepsia_final[,3:6])), type = 'l',
        lty = epilepsia_final$trt + 1, #se suma 1 pq es 0 o 1 
        xlab = 'Semana' , ylab = 'Número de ataques',
        main = 'Perfiles de sujetos durante 4 semanas utilizando
        la transformación') 

#grafico segun sujeto y si recibio o no tratamiento
data %>% 
  ggplot(aes(x = semana,
             y = log(response + 1))) +
  geom_line(aes(group = ID, color = ID)) +
  theme_gray() +
  facet_grid(.~ trt_n) +
  ggtitle('Perfiles de sujetos durante 4 semanas') +
  xlab("Semanas") +
  ylab("Ataques en escala log")


#resumen
library(foreign)
library(xtable)
library(stargazer)
data %>% 
  dplyr::group_by(trt_n, semana) %>% 
  dplyr::summarise(N = n(),
                   M = mean(response),
                   VAR = var(response),
                   SD = sd(response)) %>% 
  pander::pander()

tabla = data %>% 
  dplyr::group_by(trt_n, semana) %>% 
  dplyr::summarise(N = n(),
                   M = mean(response),
                   VAR = var(response),
                   SD = sd(response))



inter <- interaction(data$trt_n, data$semana)
tapply(data$response, inter, mean)
tapply(data$response, inter,  var)

#Algunas de las varianzas son considerablemente mayores que las medias 
#correspondientes, que para una variable de Poisson puede sugerir que 
#la dispersión excesiva puede ser un problema.
#por lo que tendremos que probar distintos modelos.

# Modelos -----------------------------------------------------------------

#ajustamos un modelo de regresión de Poisson a los datos
#asumiendo independencia 

#realizamos una regresion de poisson 
model_glm <- glm(response ~ base + age + trt_n + offset(log_time),
                 data = data,
                 family = poisson(link = "log"))

summary(model_glm)

#Ecuaciones de estimación generalizadas (GEE)
#Adaptarse a una estructura de independencia, seguido de una 
#estructura intercambiable 
#GEE = PROC GENMOD with repeat
model_gee <- gee::gee(response ~ base + age + trt_n + offset(log_time),
                      data = data,
                      family = poisson(link = "log"),
                      id = ID,
                      corstr = "independence",
                      scale.fix = TRUE,
                      scale.value = 1)
summary(model_gee)
# Las mediciones repetidas no están correlacionadas.
#Los naive SE’s son mucho más pequeños (la mitad) que los robust SE  
#(sándwich), lo que sugiere un mal ajuste.

#cambiando la estructura de la correlación
model_gee2 <- gee::gee(response ~ base + age + trt_n +offset(log_time),
                       data = data,
                       family = poisson(link = "log"),
                       id = ID,
                       corstr = "exchangeable",
                       scale.fix = TRUE,
                       scale.value = 1)
summary(model_gee2)

model_gee3 <- gee::gee(response ~ base + age + trt_n +offset(log_time),
                       data = data,
                       family = poisson(link = "log"),
                       id = ID,
                       corstr = "exchangeable",
                       scale.fix = FALSE)

summary(model_gee3)
#Los SE NAIVA están mucho más cerca de los SE robustos.
#Estimated Scale Parameter es mayor a 1.
model_gee4 <- gee::gee(response ~ base + age + trt_n +offset(log_time),
                       data = data,
                       family = poisson(link = "log"),
                       id = ID,
                       corstr = "independence",
                       scale.fix = TRUE)






# Estimaciones Importantes ------------------------------------------------------------

texreg::knitreg(list(model_glm, 
                     model_gee, 
                     model_gee2, 
                     model_gee3),
                custom.model.names = c("GLM",
                                      "GEE-Indep(1)",
                                      "GEE-Exchg(1)",
                                      "GEE-Exchg(free)"),
                caption = "Estimates on Logit Scale",
                digits = 3)




#creamos un modelo para ver la incidencia que tiene el tratamiento a lo
#largo de las semanas

modelo_prueba <- geepack::geeglm(response ~ trt_n + semana_nva,
                                  data = data,
                                  family = poisson(link = "log"),
                                  id = ID,
                                  corstr = "exchangeable")


interactions::interact_plot(model = modelo_prueba,
                            pred = semana_nva,
                            modx = trt_n)
summary(modelo_prueba)
#La conclusión
#No existe evidencia de que el tratamiento afecte la tasa semanal de ataques 
#epilépticos de manera diferente al placebo.

#Si el tratamiento no es efectivo, que variable puede producir más ataques?


glm_para_base <- glm(response ~ age,
                       data = data,
                       family = poisson(link = "log"))

library(gmodels)
fit.contrast(glm_para_base, "trt_n", c(0, 1))
exp(0.02753449) #exp(Estimate) ->estimated ratio
summary(glm_para_base)

#el grupo con tratamiento empieza peor que el grupo placebo


# Modelo pruebas ----------------------------------------------------------

#con intercepto aleatorio
model_glmer1 <- lme4::glmer(response ~ trt_n + semana_nva + age + (1|ID),
                     data = data,
                     family = poisson(link = "log"))

summary(model_glmer1)

model_glmer11 <- lme4::glmer(response ~ trt_n + base +  semana4 + age + (1|ID),
                            data = data,
                            family = poisson(link = "log"))

summary(model_glmer11)

#tratamiento no  es significativo a nivel del sujeto

model_subject <- lme4::glmer(response ~ trt_n + (1|ID),
                            data = data,
                            family = poisson(link = "log"))
summary(model_subject)

#modelo sin intercepto aleatorio y solo considerando el tratamiento

glm_model_1 <- glm(response ~ trt_n + offset(log_time),
                   data = data,
                   family = poisson(link = "log"))
summary(glm_model_1)


glm_model_1 <- glm(response ~ trt_n + offset(log_time),
                   data = data,
                   family = bin(link = "log"))
summary(glm_model_1)
#considerando solo el tratamiento no es significativo

#sin intercepto aleatorio

glm_model_2 <- glm(response ~ trt_n + semana + age + base + offset(log_time),
                    data = data,
                    family = poisson(link = "log"))
summary(glm_model_2)

#teniendo en cuenta la base, el tratamiento se significativo, y solo en la semana 4

#modificamos el modelo solo considerando la semana 4

glm_model_3 <- glm(response ~ trt_n + semana4 + age + base + semana4+offset(log_time),
                   data = data,
                   family = poisson(link = "log"))
summary(glm_model_3)
#todo es significativo

#Ahora, que interacción puede tener la base y el tratamiento?
#use las transformaciones para ver si mejora el modelo

glm_model_transform1 = glm(response ~ logbase*trt_n + logage + semana4 + offset(log_time),
                      data = data,
                      family = poisson(link = "log"))
summary(glm_model_transform1)
#el término de interacción en la regresión le informa sobre el efecto de 
#logbase y trt juntos
#la base y el trt poseen alto impacto


#que interaccion puede tener la ultima semana y el tratamiento?

glm_model_transform2 = glm(response ~ semana4*trt_n + logage + logbase + offset(log_time),
                    data = data,
                    family = poisson(link = "log"))
summary(glm_model_transform2)

#semana4 y trt no poseen mayor impacto en la regresion

# resumen modelos ---------------------------------------------------------
texreg::knitreg(list(glm_model_1,
                     glm_model_2, 
                     glm_model_3,
                     glm_model_transform1,
                     glm_model_transform2),
                custom.model.names = c("glm_model_1",
                                       "glm_model_2",
                                       "glm_model_3",
                                       "glm_model_transform1",
                                       "glm_model_transform2"),
                caption = "Estimates on GLM",
                digits = 3)

#Ahora de todos nos quedamos con glm_model_2, mayor AIC

#no estamos permitiendo que las respuestas dentro de los sujetos estén correlacionada

model_gee1 <- gee::gee(response ~ trt_n + semana4 + age + base + semana4+offset(log_time),
                       data = data,
                       family = poisson(link = "log"),
                       id = ID,
                       corstr = "independence",
                       scale.fix = FALSE)

model_gee2 <- gee::gee(response ~ trt_n + semana4 + age + base +semana4+ offset(log_time),
                       data = data,
                       family = poisson(link = "log"),
                       id = ID,
                       corstr = "independence",
                       scale.fix = TRUE,
                       scale.value = 1)

#modelo con una estructura de correlación intercambiable. 
#Esto dice que todos los pares de respuestas dentro de un sujeto están 
#igualmente correlacion
model_gee3 <- gee::gee(response ~ trt_n + semana4 + age + base + semana4+offset(log_time),
                       data = data,
                       family = poisson(link = "log"),
                       id = ID,
                       corstr = "exchangeable",
                       scale.value = 1)

model_gee4 <- gee::gee(response ~ trt_n + semana4 + age + base + semana4+ offset(log_time),
                       data = data,
                       family = poisson(link = "log"),
                       id = ID,
                       corstr = "exchangeable")


#muestra una correlación de 0.2959 entre sujetos.
#ahora hayq ue considerar si son intependientes o no.

#las correlaciones de las medidas tomadas más juntas sean más altas que las 
#tomadas más separadas

# GEE es una extensión de modelos lineales generalizados 
#(GLM) para el análisis de datos longitudinales

##mejor response ~ logbase*trt_n + logage + semana4 + offset(log_time) -> glm_model_transform1
#modelo 3 
texreg::knitreg(list(glm_model_3,
                     model_gee2, 
                     model_gee3,
                     model_gee4),
                custom.model.names = c("GLM",
                                       "GEE-Indep(1)",
                                       "GEE-Exchg(1)",
                                       "GEE-Exchg(free)"),
                caption = "Estimates on Logit Scale",
                digits = 3)


# Modelo gee considerando la transformacion y el impacto de dos va --------
model_gee1_transform <- gee::gee(response ~ logbase * trt_n + logage + semana4 + 
                         offset(log_time),
                       data = data,
                       family = poisson(link = "log"),
                       id = ID,
                       corstr = "independence",
                       scale.fix = TRUE)

model_gee2_transform <- gee::gee(response ~ logbase * trt_n + logage + semana4 + 
                                   offset(log_time),
                       data = data,
                       family = poisson(link = "log"),
                       id = ID,
                       corstr = "independence",
                       scale.fix = TRUE,
                       scale.value = 1)

#modelo con una estructura de correlación intercambiable. 
#Esto dice que todos los pares de respuestas dentro de un sujeto están 
#igualmente correlacion

model_gee3_transform <- gee::gee(response ~ logbase * trt_n + logage + semana4 + 
                                   offset(log_time),
                       data = data,
                       family = poisson(link = "log"),
                       id = ID,
                       corstr = "exchangeable",
                       scale.fix = TRUE,
                       scale.value = 1)

model_gee4_transform <- gee::gee(response ~ logbase * trt_n + logage + semana4 + 
                                   offset(log_time),
                       data = data,
                       family = poisson(link = "log"),
                       id = ID,
                       corstr = "exchangeable",
                       scale.fix = T)

## resumen

table = texreg::knitreg(list(glm_model_transform1,
                     model_gee2_transform, 
                     model_gee3_transform,
                     model_gee4_transform),
                custom.model.names = c("GLM",
                                       "GEE-Indep(1)",
                                       "GEE-Exchg(1)",
                                       "GEE-Exchg(free)"),
                caption = "Estimates on Logit Scale",
                digits = 3)

table

# Resolviendo el problema de la dispersion --------------------------------

glm_model_transform1 <- glm(response ~ logbase * trt_n + logage + semana4 + 
                    offset(log_time),
                    data = data,
                    family = poisson(link = "log"))

summary(glm_model_transform1)



dp = sum(residuals(glm_model_3,type ="pearson")^2)/glm_model_3$df.residual
dp
library(AER)
dispersiontest(glm_model_transform1)

summary(glm_model_transform1, dispersion = dp)


#solucion de la dispersion
#Permitir estimación de dispersión

modelo_qpoisson = glm(response ~logbase*trt_n + semana4 + logage + offset(log_time),
               family=quasipoisson, data)
summary(modelo_qpoisson)

#en donde la semana 4 no es significativa
#parametro de dispersion es de 4.67

modelo_qpoisson = glm(response ~logbase*trt_n + logage + offset(log_time),
                      family=quasipoisson(link = "log"), data)
summary(modelo_qpoisson)

#Reemplace Poisson con Binomial Negativo
#en el que la varianza es mayor que la media .

bin_neg = MASS::glm.nb(response ~logbase*trt_n + logage + semana4 + offset(log_time),
                       data = data)
summary(bin_neg)

#semana 4 sigue sinedo no significativa

bin_neg = MASS::glm.nb(response ~logbase*trt_n + logage + offset(log_time),
                       data = data)
summary(bin_neg)

#no se ajusta bien pues la relacion entre 

qpoi_mod = lme4::glmer(response ~logbase * trt_n + logage + offset(log_time) + (1|ID),
                family=quasipoisson, data)
        

# Modelos con efectos aleatorios ------------------------------------------

#lme4 (aproximación de Laplace)
library(lme4)
glmerLaplace = glmer(response ~ logbase*trt_n + logage + (1 + semana| ID)
                     + offset(log_time),
                     data = data,
                     family = poisson(link = "log"),
                     nAGQ = 1)
summary(glmerLaplace)  


dp = blmeco::dispersion_glmer(glmerLaplace)

summary(glmerLaplace, dispersion = dp)


#glmmML (cuadratura gaussiana adaptativa)

glmmML1 = glmmML::glmmML(response ~ logbase * trt_n + logage
                         + offset(log_time) ,
                 family = poisson,
                 data = data,
                 cluster = ID)
summary(glmmML1)

#glmmPQL (cuasi-verosimilitud penalizada)
#Este método no convergió.
glmmPQL1 = MASS::glmmPQL(response ~ logbase*trt_n + logage + semana
        ,random=~ 1 | ID, family=poisson,
        data = data) 

summary(glmmPQL1) #peor


#MCMCglmm (Markov Chain Monte Carlo a través de CSparse)

MCMCglmm1 <- MCMCglmm::MCMCglmm(response ~ logbase * trt_n + logage
                      + offset(log_time),
                      ## us() also estimate the covariance, but it failed to run.
                      ## idh() does not estimate the covariance.
                      random = ~ idh(1 + semana):ID,
                      family = "poisson",
                      data = data)
summary(MCMCglmm1)

#response ~ logbase*trt_n + logage + (1 + semana| ID) + offset(log_time)

glmerLaplace = glmer(response ~ logbase*trt_n + logage + (1 + age| ID)
                     + offset(log_time),
                     data = data,
                     family = poisson(link = "log"),
                     nAGQ = 1)
summary(glmerLaplace) 





# Supuestos del modelo escogido -------------------------------------------

modelo_qpoisson = glm(response ~logbase*trt_n + semana4 + logage + offset(log_time),
                      family=quasipoisson(link = "log"), data)
summary(modelo_qpoisson)

plot(glmerLaplace, 1) #Podemos verificar la linealidad de los datos 
#observando el gráfico Residual vs ajustado . 

durbinWatsonTest(modelo_qpoisson) #a forma más sencilla de comprobar el 
#supuesto de independencia es mediante el test de Durbin Watson
#La hipótesis nula establece que los errores no se autocorrelacionan entre 
#sí (son independientes). Por tanto, si conseguimos un valor de p > 0,05, 
#no podremos rechazar la hipótesis nula. ¡Esto nos daría suficiente evidencia 
#para afirmar que se cumple nuestra suposición de independencia!

plot(modelo_qpoisson, 3) #los errores residuales tienen una varianza constante

plot(modelo_qpoisson, 2)   #normalidad



