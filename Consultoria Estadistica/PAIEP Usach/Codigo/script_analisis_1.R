library(readxl)
library(dplyr)
library(ggplot2)

data = read_xls("Exam2023.xls")

data2 = data %>% 
  filter(!is.na(Prom__20161))


# Z_k = 1, el estudiante asiste al menos a k tutorias el 1er semestre
# Z_k = 0, el estudiante asiste a menos de k tutorias el 1er semestre

# BAP = 1, si es estudiante BAP
# BAP = 0, si no

# Y = 1, si al menos logra el rendimiento esperado en el 1er semestre
# Y = 0, si no


N = data2 %>% filter(BAP == 1) %>% count()

k = 15
Nota = 4

aux1 = data2 %>% 
  filter(BAP == 1, Prom__20161 >= Nota, Tut_20161 == k) %>% count()/N # P(Y=1,Zk=1)
aux2 = data2 %>% 
  filter(BAP == 1, Prom__20161 >= Nota, Tut_20161 < k) %>% count()/N # P(Y=1,Zk=0)
aux3 = data2 %>% 
  filter(BAP == 1, Prom__20161 < Nota, Tut_20161 == k) %>% count()/N # P(Y=0,Zk=1)
aux4 = data2 %>% 
  filter(BAP == 1, Prom__20161 < Nota, Tut_20161 < k) %>% count()/N # P(Y=0,Zk=0)

M = matrix(c(aux1$n, aux2$n, aux3$n, aux4$n),2,2, byrow = T)
sum(M)
M

#zk1 = sum(M[,1]) # P(Zk=1)
#y1.zk1 = M[1,1] / zk1 # P(Y=1 | Zk=1)
#zk0 = sum(M[,2]) # P(Zk = 0)
#y1.zk0 = M[1,2] / zk0 # P(Y=1 | Zk=0)
sum(M[1,]) # P(Y = 1)


# Proceso automatizado ----------------------------------------------------

func_prob = function(data, Nota, k){
  N = data %>% filter(BAP == 1) %>% count()
  out1 = vector(length = length(k), mode = "numeric")
  out2 = vector(length = length(k), mode = "numeric")
  for(i in 1:length(k)){
    aux1 = data %>% 
      filter(BAP == 1, Prom__20161 >= Nota, Tut_20161 >= k[i]) %>% count()/N # P(Y=1,Zk=1)
    aux2 = data %>% 
      filter(BAP == 1, Prom__20161 >= Nota, Tut_20161 < k[i]) %>% count()/N # P(Y=1,Zk=0)
    aux3 = data %>% 
      filter(BAP == 1, Prom__20161 < Nota, Tut_20161 >= k[i]) %>% count()/N # P(Y=0,Zk=1)
    aux4 = data %>% 
      filter(BAP == 1, Prom__20161 < Nota, Tut_20161 < k[i]) %>% count()/N # P(Y=0,Zk=0)
    
    M = matrix(c(aux1$n, aux2$n, aux3$n, aux4$n),2,2, byrow = T)
    zk1 = sum(M[,1]) # P(Zk=1)
    y1.zk1 = M[1,1] / zk1 # P(Y=1 | Zk=1)
    y1 = sum(M[1,]) # P(Y=1)
    out1[i] = y1.zk1
    out2[i] = y1
  }
  out = data.frame(Prob1 = out1, Prob2 = out2)
  return(out)
  
}

F_emp = function(datos, quantile){
  n = length(datos)
  F = 1:n / n
  datos = sort(datos)
  return(datos[sum(F <= quantile)])
}

aux = data2 %>% filter(BAP == 0) 
data.frame(cuartil_1 = F_emp(aux$Prom__20161, 0.25),
           cuartil_2 = F_emp(aux$Prom__20161, 0.5),
           cuartil_3 = F_emp(aux$Prom__20161, 0.75))


prob_nota_c1 = func_prob(data2, 4.42, 1:48) # prob. de tener promedio sobre el c1 de quienes no son BAP
prob_nota_c2 = func_prob(data2, 5.04, 1:48)
prob_nota_c3 = func_prob(data2, 5.48, 1:48)


data_probs = data.frame(k = 1:48,
                        Nota_c1 = prob_nota_c1$Prob1,
                        Nota2_c1 = prob_nota_c1$Prob2,
                        Nota_c2 = prob_nota_c2$Prob1,
                        Nota2_c2 = prob_nota_c2$Prob2,
                        Nota_c3 = prob_nota_c3$Prob1,
                        Nota2_c3 = prob_nota_c3$Prob2)

data_probs %>% 
  ggplot(aes(x = k)) +
  geom_line(aes(y = Nota_c1, color = "Nota 4.42")) +
  geom_line(aes(y = Nota2_c1), color = "red", linetype = "dashed") +
  geom_line(aes(y = Nota_c2), color = "blue") +
  geom_line(aes(y = Nota2_c2), color = "blue", linetype = "dashed") +
  geom_line(aes(y = Nota_c3), color = "darkgreen") +
  geom_line(aes(y = Nota2_c3), color = "darkgreen", linetype = "dashed") +
  scale_color_manual(name = "Leyenda",
    breaks=c('Nota 4.42', 'Nota 5.04', 'Nota 5.48'),
                     values=c('Nota 4.42'='red', 'Nota 5.04'='blue', 'Nota 5.48'='darkgreen')) +
  labs(y = "Probabilidad de éxito", title = "Probabilidad de éxito estudiantes BAP para distintos valores de k")
  
# Se observa que para un k = 35 aprox. ya se logra tener la prob. más alta dado
# la cantidad de estudiantes (que vienen siendo 14).


# Pregunta 2 --------------------------------------------------------------

data3 = data %>% select(BAP, VAI, Prom__20161, Prom__20162, Tut_20161, Tut_20162)
skimr::skim(data3)

# Existen alumnos sin promedios el primer semestre, por lo que automáticamente
# no se podrá evaluar su progreso durante el año.
#
# Podemos ver alumnos sin información del promedio del segundo semestre, por lo 
# cual no se puede inferir nada sobre estos sujetos acerca de cómo hubiesen 
# reaccionado asistiendo al programa.
#
# Dado lo anterior es que se realizarán supuestos sobre esos individuos

# Eliminamos los sujetos sin promedios el primer semestre y reemplazaremos
# los NA´s por ceros en el promedio de segundo semestre y en la cantidad
# de tutorias del primer y segundo semestre

data3 = data3 %>% 
  filter(!is.na(Prom__20161)) %>% 
  mutate(Prom__20162 = case_when(!is.na(Prom__20162) ~ Prom__20162,
                                 is.na(Prom__20162) ~ 0),
         Tut_20161 = case_when(!is.na(Tut_20161) ~ Tut_20161,
                               is.na(Tut_20161) ~ 0),
         Tut_20162 = case_when(!is.na(Tut_20162) ~ Tut_20162,
                               is.na(Tut_20162) ~ 0))
skimr::skim(data3)

# Se consideran sólo los estudiantes VAI o BAP del segundo semestre
# Se crea la variable respuesta Y = 1 como los estudiantes que mejoran o igualan
# su promedio de un semestre a otro
# Se crea la variable Z = 1 si el alumno ha asistido al menos a k tutorias

k = 20

data3 = data3 %>% 
  filter(Tut_20162 > 0 | VAI == 1) %>% 
  mutate(Z = ifelse(Tut_20162 >= k, 1, 0),
         Y = ifelse(Prom__20162 >= Prom__20161, 1, 0))


#####-

# Interesa comparar
# P(Y1 = 1) y P(Y0 = 1)

# P(Y1 = 1) = P(Y1 = 1| Z = 1)P(Z = 1) + P(Y1 = 1| Z = 0)P(Z = 0)
#                                          (desconocido)
#
#           = P(Y = 1, Z = 1) + P(Y1 = 1| Z = 0)P(Z = 0)

# P(Y0 = 1) = P(Y0 = 1| Z = 1)P(Z = 1) + P(Y0 = 1| Z = 0)P(Z = 0)
#               (desconocido)
#
#           = P(Y0 = 1| Z = 1)P(Z = 1) + P(Y = 1, Z = 0)

## Creamos la tabla 2x2 ## 
N = nrow(data3)
aux1 = data3 %>% filter(Y == 1, Z == 1) %>% count()/N # P(Y=1, Z=1)
aux2 = data3 %>% filter(Y == 1, Z == 0) %>% count()/N # P(Y=1, Z=0)
aux3 = data3 %>% filter(Y == 0, Z == 1) %>% count()/N # P(Y=0, Z=1)
aux4 = data3 %>% filter(Y == 0, Z == 0) %>% count()/N # P(Y=0, Z=0)

M = matrix(c(aux1$n, aux2$n, aux3$n, aux4$n),2,2, byrow = T)
colnames(M) = c("Z=1", "Z=0")
rownames(M) = c("Y=1", "Y=0")
M
sum(M)


### CASO 1 ####
# Supuesto: 0 <= P(Y1 = 1| Z = 0) <= 1
# 
# -> P(Y = 1, Z = 1) <= P(Y1 = 1) <= P(Y = 1, Z = 1) + P(Z = 0)

(IC_1_Y1 = c(M[1,1], M[1,1] + sum(M[,2]))) # IC para P(Y1 = 1)

# Si asumimos que a un estudiante que pertenece al status quo, su probabilidad
# de haber mejorado al asistir al programa fuese nula, entonces la probabilidad 
# de éxito del programa es de al menos un 3.9%
#
# En cambio, si su probabilidad de mejorar fuese de un 100%, entonces la probabilidad
# de éxito del programa llega hasta un 93.6%
  

# Supuesto: 0 <= P(Y0 = 1| Z = 1) <= 1
# 
# -> P(Y = 1, Z = 0) <= P(Y0 = 1) <= P(Z = 1) + P(Y = 1, Z = 0)

(IC_1_Y0 = c(M[1,2], M[1,2] + sum(M[,1]))) # IC para P(Y0 = 1)

# Si asumimos que a un estudiante que pertenece al progrma, su probabilidad
# de haber mejorado al no asistir al programa fuese nula, entonces la probabilidad 
# de éxito al no asistir al programa es de al menos un 24.6%
#
# En cambio, si su probabilidad de mejorar fuese de un 100%, entonces la probabilidad
# de éxito al no asistir al programa llega hasta un 34.8%



### CASO 2 ####
# Supuesto: Siempre es mejor aplicar la innovacion que no aplicarla
#
# Supuesto : P(Y1 = 1 | Z = 0) >= P(Y0 = 1 | Z = 0)    
#
# P(Y = 1) <= P(Y1 = 1) <= P(Y = 1) + P(Y = 0, Z = 0) 

(IC_2_Y1 = c(sum(M[1,]), sum(M[1,]) + M[2,2])) # IC para P(Y1 = 1)

# Si asumimos que un estudiante que pertenece al status quo, su probabilidad
# de haber mejorado al asistir al programa fuese mayor o igual que al no haber ido, entonces
# la probabildiad de éxito del programa es de al menos un 28.5%
#
# Por otro lado, si su probabilidad de mejorar fuese de un 100%, entonces la 
# probabilidad de éxito del programa llega hasta un 93.6%


# Supuesto : P(Y1 = 1 | Z = 1) >= P(Y0 = 1 | Z = 1)   
#
# P(Y = 1, Z = 0) <= P(Y0 = 1) <= P(Y = 1)

(IC_2_Y0 = c(M[1,2], sum(M[1,]))) # IC para P(Y0 = 1)

# Si asumimos que un estudiante que pertenece al programa, su probabilidad
# de haber mejorado al no haber asistido fuese cero, entonces la probabilidad
# de éxito al no asistir al programa es de al menos un 24.6%
#
# Por otro lado, si su probabilidad de mejorar fuese menor o igual que al haber 
# ido, entonces la probabildiad de éxito al no asistir al programa es de a lo 
# más un 28.5%



### CASO 3 ###
# Supuesto: Es mejor lo observado que lo contrafactual
#
# Supuesto : P(Y1 = 1 | Z = 0) <= P(Y0 = 1 | Z = 0) 
#
# P(Y = 1, Z = 1) <= P(Y1 = 1) <= P(Y = 1)

(IC_3_Y1 = c(M[1,1], sum(M[1,]))) # IC para P(Y1 = 1)

# Si asumimos que un estudiante que no pertenece al programa, su probabilidad
# de haber mejorado al haber asistido fuese cero, entonces la probabilidad
# de éxito del programa es de al menos un 3.9%
#
# Por otro lado, si su probabilidad de mejorar fuese menor o igual que no haber
# asistido, la probabilidad de éxito del programa es a lo más un 28.5%

# Supuesto : P(Y1 = 1 | Z = 1) >= P(Y0 = 1 | Z = 1) 
#
# P(Y = 1, Z = 0) <= P(Y0 = 1) <= P(Y = 1)

(IC_3_Y0 = c(M[1,2], sum(M[1,]))) # IC para P(Y0 = 1)

# Si asumimos que un estudiante que pertenece al programa, su probabilidad
# de haber mejorado al no haber asistido fuese cero, entonces la probabilidad
# de éxito al no asistir al programa es al menos un 24.6%
#
# Por otro lado, si su probabilidad de mejorar fuese menor o igual que haber 
# asistido, la probabilidad de éxito al no asistir al programa es a lo más
# un 28.5%



### CASO 4 ###
# Supuesto: Lo contrafactual es mejor que lo observado
#
# Supuesto : P(Y1 = 1 | Z = 0) >= P(Y0 = 1 | Z = 0)
#
# P(Y = 1) <= P(Y1 = 1) <= P(Y = 1) + P(Y=0, Z=0)

(IC_4_Y1 = c(sum(M[1,]), sum(M[1,]) + M[2,2])) # IC para P(Y1 = 1)

# Si suponemos que un estudiante que no es parte del programa, su probabilidad
# de mejorar al asistir al programa es menor o igual que no haber asistido, entonces
# la probabilidad de éxito del programa es al menos un 29.5%
#
#

# Supuesto : P(Y0 = 1 | Z = 1) >= P(Y1 = 1 | Z = 1)
# P(Y1 = 1) <= P(Y0 = 1) <= P(Z = 1) + P(Y=1, Z=0)

(IC_4_Y0 = c(sum(M[1,]), sum(M[,1]) + M[1,2])) # IC para P(Y0 = 1)














