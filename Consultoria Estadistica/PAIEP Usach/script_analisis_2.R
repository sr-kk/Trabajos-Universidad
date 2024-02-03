library(readxl)
library(dplyr)
library(ggplot2)
library(skimr)

data = read_xls("Exam2023.xls")
View(data)



# Estudiantes bajo innovación -> BAP = 1
# Estudiantes bajo status quo -> BAP = 0



# ANÁLISIS EXPLORATORIO ---------------------------------------------------

#### 1. Puntajes ranking y PSU ####

data %>% 
  select(PTJE_RANKING, PTJE_LYC, PTJE_MAT) %>% 
  skim()

###########-

### Histograma ptje ranking ###
data %>% 
  ggplot(aes(PTJE_RANKING, fill = factor(BAP))) +
  geom_histogram(color = "white", position = "dodge") +
  facet_wrap(~BAP) 
# Se observa una mayor cantidad de estudiantes bajo status quo

data %>% 
  group_by(BAP) %>% summarise(freq = n(), freq.rel = freq/nrow(data))
# 89.4% bajo status quo
# 10.6% bajo innovacion
#
# Se debe tener en consideración la gran diferencia que hay entre la cantidad
# de estudiantes bajo innovación y status quo

### Box-plot ptje ranking ###
data %>% 
  ggplot(aes(y=PTJE_RANKING, fill = factor(BAP))) +
  geom_boxplot()
# Estudiantes bajo innovación parecen tener puntajes ranking más altos (recordar 
# la diferencia entre las cantidades)

data %>% 
  ggplot(aes(x = factor(BAP), y = PTJE_RANKING, fill = factor(VAI))) +
  geom_boxplot()
# De los estudiantes bajo innovacion, los que ingresan mediante vía regular tienden
# a tener puntajes más altos que los que ingresan mediante vía inclusiva. Caso contario
# del los estudiantes bajo status quo.

###########-

### Histograma PSU lenguaje ###
data %>% 
  ggplot(aes(PTJE_LYC, fill = factor(BAP))) +
  geom_histogram(color = "white", position = "dodge") +
  facet_wrap(~BAP)

### Box-plot PSU lenguaje ###
data %>% 
  ggplot(aes(y=PTJE_LYC, fill = factor(BAP))) +
  geom_boxplot()
# Estudiantes bajo status quo parecen tener puntajes más altos (recordar la diferencia
# entre las cantidades)

data %>% 
  ggplot(aes(x = factor(BAP), y = PTJE_LYC, fill = factor(VAI))) +
  geom_boxplot()
# Tanto en estudiantes bajo innovación como en status quo, los que ingresan mediante
# via regular tienden a tener puntajes más altos

###########-

### Histograma PSU matematicas ###
data %>% 
  ggplot(aes(PTJE_MAT, fill = factor(BAP))) +
  geom_histogram(color = "white", position = "dodge") +
  facet_wrap(~BAP)

### Box-plot PSU matematicas ###
data %>% 
  ggplot(aes(y=PTJE_MAT, fill = factor(BAP))) +
  geom_boxplot()
# Estudiantes bajo status parecen tener puntajes más altos (recordar la diferencia
# entre las cantidades)

data %>% 
  ggplot(aes(x = factor(BAP), y = PTJE_MAT, fill = factor(VAI))) +
  geom_boxplot()
# Tanto en estudiantes bajo innovación como status quo, los que ingresan mediante
# via regular tienden a tener puntajes más altos

###########-

### Box plot puntajes para el total de estudiantes ###
aux1 = data %>% transmute(Puntaje = PTJE_RANKING, Tipo = "Ptje. Ranking")
aux2 = data %>% transmute(Puntaje = PTJE_LYC, Tipo = "PSU Lenguaje")
aux3 = data %>% transmute(Puntaje = PTJE_MAT, Tipo = "PSU Matematicas")

rbind(aux1, aux2, aux3) %>% 
  ggplot(aes(Tipo, Puntaje, fill = Tipo)) + 
  geom_boxplot() 



#### 2. Promedios primer y segundo semestre #####

data %>% 
  select(Prom__20161, Prom__20162) %>% 
  skim()

View(data %>% filter(is.na(Prom__20161)))
View(data %>% filter(is.na(Prom__20162)))


# Existen 161 NA's para los promedios del primer semestre, de los cuales 17 son
# estudiantes BAP, no obstante, no otorgan ninguna información. Incluso, si fuesen 
# estudiantes que entran en segundo semestre no aportan ninguna información sobre 
# el impacto del programa.
#
# Existen 495 NA para los promedios del segundo semestre, de los cuales 80 no 
# tienen informacion sobre su promedio del primer semestre. Es decir, estos 
# estudiantes no aportan ninguna información al impacto del tratamiento.
# Luego, el resto de NA's se puede explicar por lo siguiente:
#   - Se fueron de la carrera
#   - No siguieron en el programa de innovacion
#
# Existen además 80 estudiantes que no entregan información para ambos semestres



#### Análisis 1° semestre ####

# Dado lo anterior, no se considerarán a los 161 estudiantes que no aportan información
# sobre el impacto del tratamiento

data2 = data %>% 
  filter(!is.na(Prom__20161)) # nueva data sin los 161 estudiantes

data2 %>% select(Prom__20161, Prom__20162) %>% skim()

data2 %>% group_by(BAP) %>% summarise(freq = n(), freq.rel = n()/nrow(data2))
# se mantienen las tasas de estudiantes BAP


### Histograma promedios primer semestre ###
data2 %>% 
  ggplot(aes(Prom__20161, fill = factor(BAP))) +
  geom_histogram(color = "white", position = "dodge") +
  facet_wrap(~BAP)
# Existe una cantidad no menor de estudiantes con promedio bajo 4

data2 %>% filter(Prom__20161 < 4) %>% count() # 637 siendo específicos


### Box-plot promedios primer semestre ###
data2 %>% 
  ggplot(aes(y=Prom__20161, fill = factor(BAP))) +
  geom_boxplot()
# Estudiantes bajo innovación tienden a tener promedio más bajo en el primer semestre

data2 %>% 
  ggplot(aes(x = factor(BAP), y = Prom__20161, fill = factor(VAI))) +
  geom_boxplot()
# De los estudiantes BAP, los que ingresaron mediante via regular mantienen una 
# distribución similar pero menos dispersa en comparacion a los que ingresaron 
# mediante via inclusiva.
#
# De los estudiantes bajo status quo, se puede nota una gran diferencia en los 
# promedios entre los que ingresaron mediante via regular e inclusiva, teniendo
# el primero puntajes superiores.
#
# Es importante mencionar que los estudiantes VAI bajo innovacion resultan tener
# promedios superiores a los estudiantes VAI bajo status quo
# 
data2 %>% filter(BAP == 1) %>% select(Prom__20161) %>%
  summarise(freq = n(),
            min = min(Prom__20161), 
            C1 = quantile(Prom__20161, 0.25),
            C2 = quantile(Prom__20161, 0.5),
            C3 = quantile(Prom__20161, 0.75),
            max = max(Prom__20161))
data2 %>% filter(BAP == 0) %>% select(Prom__20161) %>% 
  summarise(freq = n(),
            min = min(Prom__20161), 
            C1 = quantile(Prom__20161, 0.25),
            C2 = quantile(Prom__20161, 0.5),
            C3 = quantile(Prom__20161, 0.75),
            max = max(Prom__20161))


## Es de interés determinar una variable respuesta para este análisis ##
#
# Para ello veremos si existe alguna relación entre los promedios y la tasa
# de aprobación de cursos, pues una propuesta puede llegar a ser el éxito
# de aprobación en el semestre.

data2 %>% 
  ggplot(aes(Tasa_aprob_20161, Prom__20161)) +
  geom_point(aes(colour = factor(BAP)))
# Vemos que todos quienes aprueban la totalidad de sus cursos logran un promedio
# superior a 4, pero aquello no implica necesariamente aprobar todos los cursos.



###########-


### Analisis asistencia a tutorias ###

data2 %>% select(Tut_20161) %>% skim() 
# Se obervan 3311 estudiantes que no asistieron a las tutorias o que no fueron
# parte de ella (corresponden a los NA's)

data2 %>% filter(BAP == 0) %>% count() # se confirma que no fueron parte del programa


### Estudiaremos la correlación entre el promedio y la asistencia ###

data2 %>% filter(BAP == 1) %>% 
  ggplot(aes(Tut_20161, Prom__20161)) +
  geom_point()
# Se puede observar un leve patrón de crecimiento en el promedio a medida que 
# aumenta la cantidad de tutorias. Sin embargo, tambien se puede ver una 
# heterocedasticidad a lo largo del gráfico.

data2 %>% filter(BAP == 1) %>% select(Tut_20161, Prom__20161) %>% cor()


## Tabla resumen sobre el efecto de las tutorias sobre el promedio ##
tabla1 = data2 %>% filter(BAP == 1) %>% group_by(Tut_20161) %>% 
  summarise(min = min(Prom__20161),
            median = median(Prom__20161),
            mean = mean(Prom__20161),
            max = max(Prom__20161),
            freq = n())
View(tabla1)


## Análisis sólo a estudiantes intervenidos ##
data2 %>% filter(BAP == 1, Tut_20161 >= 10) %>% 
  ggplot(aes(Tut_20161, Prom__20161)) +
  geom_point()
# No se puede observar una tendencia clara entre ambas variables
data2 %>% filter(BAP == 1, Tut_20161 >= 10) %>% 
  select(Tut_20161, Prom__20161) %>% cor()



#### Análisis 2° semestre ####

# No se considerarán los estudiantes que no poseen informacion tanto del promedio
# del primer y segundo semestre

aux4 = data %>% filter(is.na(Prom__20161), is.na(Prom__20162))

data3 = setdiff(data, aux4)

data3 %>% group_by(BAP) %>% summarise(freq = n(), freq.rel = n()/nrow(data3))
# se mantienen las tasas

### Histograma promedios segundo semestre ###
data3 %>% 
  ggplot(aes(Prom__20162, fill = factor(BAP))) +
  geom_histogram(color = "white", position = "dodge") +
  facet_wrap(~BAP)

### Box-plot promedios segundo semestre ###
data3 %>% 
  ggplot(aes(y=Prom__20162, fill = factor(BAP))) +
  geom_boxplot()


data3 %>% 
  ggplot(aes(x = factor(BAP), y = Prom__20162, fill = factor(VAI))) +
  geom_boxplot()


data3 %>% 
  ggplot(aes(Tasa_aprob_20162, Prom__20162)) +
  geom_point(aes(colour = factor(BAP)))


### Analisis asistencia a tutorias ###

data3 %>% select(Tut_20162) %>% skim() 
# Se obervan 3392 estudiantes que no asistieron a las tutorias o que no fueron
# parte de ella (corresponden a los NA's)

data3 %>% filter(BAP == 0) %>% count() 


### Estudiaremos la correlación entre el promedio y la asistencia ###

data3 %>% filter(BAP == 1) %>% 
  ggplot(aes(Tut_20162, Prom__20162)) +
  geom_point()
# Se puede observar un leve patrón de crecimiento en el promedio a medida que 
# aumenta la cantidad de tutorias. Sin embargo, tambien se puede ver una 
# heterocedasticidad a lo largo del gráfico.

data3 %>% filter(BAP == 1) %>% select(Tut_20162, Prom__20162) %>% cor()




#### 3. Correlación puntajes psu y promedios acumulados ####

data2 %>% 
  filter(!is.na(PTJE_MAT), !is.na(PTJE_LYC), !is.na(PTJE_RANKING)) %>% 
  select(Prom__20161, PTJE_MAT, PTJE_LYC, PTJE_RANKING) %>% 
  GGally::ggpairs()








