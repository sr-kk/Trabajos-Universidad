library(skimr)
library(dplyr)
library(readxl)
library(ggplot2) 
library(scales)

# Se cargan los datos
datos = read_excel("ESAVI_V4.xlsx")

# Primer acercamiento a los datos
# Usamos skim()
skim(datos)

# Se analizan las variables con nombre poco intuitivo
#
# VARIABLE : 
#
# Archivo
cbind(datos$Archivo[1:6])
unique(datos$Archivo)
# Variable que no entrega info relevante
#
# Fuente
cbind(datos$Fuente[1:6])
unique(datos$Fuente)
# Variable poco relevante, sería más útil si tuvieramos más información sobre ella.
# Sería interesante redefinir la variable y condicionar por gravedad, severidad, 
# causalidad, desenlace.
#
# Numero_notificacion
cbind(datos$Numero_Notificacion[1:6])
unique(datos$Numero_Notificacion)
# Variable totalmente irrelevante
#
# Centro
cbind(datos$Centro[1:6])
unique(datos$Centro)
# Sería interesante ver si existe un mayor numero de casos de algún atributo de 
# las variables gravedad, severidad, causalidad, desenlace. HAY MUCHO TRABAJO
# CON RECODIFICAR LAS VARIABLES.

# IDEAS
#
# 1. Con las variables Fecha_Vacunacion y Fecha_Notificación se podría ajustar un 
# modelo del tiempo hasta que ocurre una notificación. Luego se podrían 
# los datos por alguna de las siguientes variables: gravedad, severidad, 
# causalidad, desenlace.
#
# 

#### VARIABLES A CONSIDERAR ####

# 1. Fecha_Notificacion
# 2. Edad
# 3. Sexo
# 4. Nombre_Vacuna
# 5. Fecha_Vacunacion
# 6. Fecha_Inicio_ESAVI          
# 7. Fecha_Final_ESAVI
# 8. Desenlace_ESAVI
# 9. Gravedad
# 10. Severidad
# 11. Causalidad 
# 12. AESI


# Recodificacion de variables ---------------------------------------------

datos2 = datos %>% 
  select(Fecha_Notificacion, 
         Edad, 
         Sexo, 
         Nombre_Vacuna, 
         Fecha_Vacunacion, 
         Fecha_Inicio_ESAVI, 
         Fecha_Final_ESAVI, 
         Desenlace_ESAVI, 
         Gravedad, 
         Severidad, 
         Causalidad, 
         AESI)

skim(datos2)


decod_fecha = function(fechas){
  # fechas : vector de variables tipo character para transformar en 
  #fechas_return = data.frame(fecha = rep(NA, length(fechas)))
  #fechas_return$fecha = as.Date(fechas_return$fecha)
  
  fechas_return = vector(mode = "character", length = length(fechas))
  
  for(i in 1:length(fechas)){
  #for(i in 1:100){
    aux1 = unlist(gregexpr("-", fechas[i])) # identifica "-"
    aux2 = unlist(gregexpr("/", fechas[i])) # identifica "/"
    #cat(i, "\n")
    
    fecha = NA
    fechas_return[i] = fecha
    
    if(is.na(fechas[i])){next}
                 
    if (length(aux1) == 2){
      aux3 = unlist(strsplit(fechas[i],split = "-")) # Separa por "-"
      if (aux3[1] == "00"){next}
      if (nchar(aux3[3]) > 5){next}
      if (nchar(aux3[2]) > 2){next}
    
      if (nchar(aux3[3]) == 2){aux3[3] = paste("20", aux3[3], sep = "")}
      
      aux4 = unlist(strsplit(aux3[1], split = ""))
      if (aux4[1] == "O"){aux3[1] = paste(0, aux4[2], sep = "")}
      
      if (aux3[2] == "02" & aux3[1] == "29"){next}
      
      fecha = paste(aux3[c(3,2,1)], collapse = "-" )
      
    }
    
    if (length(aux2) == 2){
      aux3 = unlist(strsplit(fechas[i],split = "/")) # Separa por "/"
      if (nchar(aux3[3]) > 4){next}
      if (aux3[1] == "00" | aux3[2] == "00"){next}
      if (nchar(aux3[3]) == 2){aux3[3] =  paste("20", aux3[3], sep = "")}
      fecha = paste(aux3[c(3,2,1)], collapse = "-" )  
    }
    
    if (aux1[1] == -1 & aux2[1] == -1 & nchar(fechas[i]) == 5){
      f = as.numeric(fechas[i])
      fecha = as.character(as.Date(f,origin = "1899-12-30"))
      #cat(i, fecha, "\n")
  }
    
    fechas_return[i] = fecha
  }
  return(fechas_return)
}

decod_nombre_vacuna = function(variable){
  nombre_vacuna = vector(mode = "character", length = length(variable))

  for(i in 1:length(variable)){
    nombre_vacuna[i] = NA
    if (grepl("PFIZER", variable[i])){nombre_vacuna[i] = "Pfizer"}
    if (grepl("ASTRAZENECA", variable[i])){nombre_vacuna[i] = "Astrazeneca"}
  }
  return(nombre_vacuna)
}


datos2 = datos2 %>% 
  mutate(Fecha_Notificacion = decod_fecha(Fecha_Notificacion),
         Nombre_Vacuna = decod_nombre_vacuna(Nombre_Vacuna),
         Fecha_Vacunacion = decod_fecha(Fecha_Vacunacion),
         Fecha_Inicio_ESAVI = decod_fecha(Fecha_Inicio_ESAVI),
         Fecha_Final_ESAVI = decod_fecha(Fecha_Final_ESAVI),
         Severidad = ifelse(Severidad == "NA", NA, Severidad)
         )

datos2$Fecha_Notificacion = as.Date(datos2$Fecha_Notificacion)
datos2$Fecha_Vacunacion = as.Date(datos2$Fecha_Vacunacion)
datos2$Fecha_Inicio_ESAVI = as.Date(datos2$Fecha_Inicio_ESAVI)
datos2$Fecha_Final_ESAVI = as.Date(datos2$Fecha_Final_ESAVI)

datos2$Edad = as.numeric(datos2$Edad)
datos2$Sexo = as.factor(datos2$Sexo)
datos2$Gravedad = as.factor(datos2$Gravedad)
datos2$Causalidad = as.factor(datos2$Causalidad)
datos2$Severidad = as.factor(datos2$Severidad)
datos2$AESI = as.factor(datos2$AESI)

saveRDS(datos2, file = "Data_consultoria3.rds")


# ANALISIS EXPLORATORIO ---------------------------------------------------

skim(datos2)

# Existe un dato sin información de la vacuna. Se ignora.
datos3 = datos2 %>% filter(!is.na(Nombre_Vacuna))

datos3 %>% 
  group_by(Sexo) %>% 
  summarise(n())



# DESENLACE SEGUN VACUNA
datos3 %>% 
  ggplot(aes(Nombre_Vacuna, fill = Desenlace_ESAVI)) +
  geom_bar(position = "dodge", width = 0.5) +
  scale_y_continuous(breaks = seq(0,3000, by = 500))

datos3 %>% 
  filter(Desenlace_ESAVI %in% c("MORTAL", 
                                "NO RECUPERADO/ NO RESUELTO",
                                "RECUPERADO/RECUPERADO CON SECUELAS")) %>% 
  ggplot(aes(Nombre_Vacuna, fill = Desenlace_ESAVI)) +
  geom_bar(position = "dodge", width = 0.5) +
  scale_y_continuous(breaks = seq(0,500, by = 50)) +
  labs(title = "Número de personas con desenlace perjudicial y/o pendiente según vacuna ")


# SEXO SEGUN VACUNA
datos3 %>% 
  ggplot(aes(Sexo, fill = Nombre_Vacuna)) +
  geom_bar(aes(y = ..count../sum(..count..)),
    position = "dodge", width = 0.5 ) +
  labs(title = "Porcentaje de vacunas administradas según sexo",
       y = "Porcentaje") +
  scale_y_continuous(labels = percent)
 
# SEXO SEGUN DESENLACE
datos3 %>% 
  ggplot(aes(Sexo, fill = Desenlace_ESAVI)) +
  geom_bar(position = "dodge", width = 0.5) +
  scale_y_continuous(breaks = seq(0,5000, by = 500))

# SEXO Y GRAVEDAD
datos3 %>% 
  ggplot(aes(Sexo, fill = Gravedad)) +
  geom_bar(aes(y = ..count../sum(..count..)),
           position = "dodge", width = 0.5 ) +
  labs(title = "Porcentaje atributos de Gravedad según sexo",
       y = "Porcentaje") +
  scale_y_continuous(labels = percent)

datos3 %>% 
  filter(Gravedad == "GRAVE") %>% 
  ggplot(aes(Gravedad, fill = Sexo)) +
  geom_bar(aes(y = ..count../sum(..count..)),
           position = "dodge", width = 0.5 ) +
  labs(title = "Porcentaje de casos graves según sexo",
       y = "Porcentaje") +
  scale_y_continuous(labels = percent)




# CAUSALIDAD SEGUN VACUNA
datos3 %>% 
  ggplot(aes(Causalidad, fill = Nombre_Vacuna)) +
  geom_bar(aes(y = ..count../sum(..count..)),
           position = "dodge", width = 0.5 ) +
  labs(title = "Porcentaje atributos de Causalidad según vacuna",
       y = "Porcentaje") +
  scale_y_continuous(labels = percent)

datos3 %>% group_by(Desenlace_ESAVI) %>% summarise(n())

# CAUSALIDAD PROBABLE Y POSIBLE SEGUN DESENLACE
datos3 %>% 
  filter(Causalidad %in% c("PROBABLE", "POSIBLE")) %>% 
  ggplot(aes(Causalidad, fill = Desenlace_ESAVI)) +
  geom_bar(position = "dodge", width = 0.5)

# POR SEXO
datos3 %>% 
  filter(Causalidad %in% c("PROBABLE", "POSIBLE")) %>% 
  ggplot(aes(Sexo, fill = Causalidad )) +
  geom_bar(position = "dodge", width = 0.5) 

# CAUSALIDAD SEGUN DESENLACE MORTAL, NO RECUPERADO Y RECUPERADO CON SECUELAS
datos3 %>% 
  filter(Desenlace_ESAVI %in% c("MORTAL", 
                                "NO RECUPERADO/ NO RESUELTO",
                                "RECUPERADO/RECUPERADO CON SECUELAS")) %>% 
  ggplot(aes(Causalidad, fill = Desenlace_ESAVI)) +
  geom_bar(position = "dodge", width = 0.5) +
  scale_y_continuous(breaks = seq(0,700, by = 50)) +
  labs(title ="Número de personas con desenlace perjudicial y/o pendiente según tipo de causalidad")


# GRAVEDAD SEGUN VACUNA
datos3 %>% 
  filter(Gravedad == "GRAVE") %>%
  ggplot(aes(Gravedad, fill = Nombre_Vacuna)) +
  geom_bar(aes(y = ..count../sum(..count..)),
           position = "dodge", width = 0.5 ) +
  labs(title = "Porcentaje atributos de Gravedad según vacuna",
       y = "Porcentaje") +
  scale_y_continuous(labels = percent)

# GRAVEDAD SEGUN DESENLACE
datos3 %>% 
  ggplot(aes(Gravedad, fill = Desenlace_ESAVI)) +
  geom_bar(position = "dodge", width = 0.5) +
  scale_y_continuous(breaks = seq(0,5000, by = 500))

# DESENLACE DE CASOS GRAVES
datos3 %>% 
  filter(Gravedad == "GRAVE") %>% 
  ggplot(aes(Gravedad, fill = Desenlace_ESAVI)) +
  geom_bar(position = "dodge", width = 0.5) +
  scale_y_continuous(breaks = seq(0,50, by = 5)) +
  labs(title = "Desenlace de los casos graves")
  
# GRAVEDADD Y CAUSALIDAD
datos3 %>% 
  ggplot(aes(Gravedad, fill = Causalidad)) +
  geom_bar(position = "dodge", width = 0.5) +
  labs(title = "Gravedad de la situación por tipo de causalidad")

datos3 %>% 
  filter(Gravedad == "GRAVE") %>% 
  ggplot(aes(Gravedad, fill = Causalidad)) +
  geom_bar(position = "dodge", width = 0.5) +
  labs(title = "Causalidad de los casos graves")

# EDAD #
  
datos3 %>% 
  filter(Sexo %in% c("F", "M")) %>% 
  ggplot(aes(Edad, fill = Sexo)) + 
  geom_histogram(alpha = 1, position = "dodge")

datos3 %>% 
  filter(Desenlace_ESAVI %in% c("MORTAL", 
                                "NO RECUPERADO/ NO RESUELTO",
                                "RECUPERADO/RECUPERADO CON SECUELAS")) %>% 
  ggplot(aes(Edad, fill = Desenlace_ESAVI)) + 
  geom_histogram(alpha = 1, position = "dodge")


datos3 %>% 
  filter(Desenlace_ESAVI %in% c("MORTAL", 
                                "RECUPERADO/RECUPERADO CON SECUELAS")) %>% 
  ggplot(aes(Edad, fill = Desenlace_ESAVI)) + 
  geom_histogram(alpha = 1, position = "dodge")

skim(datos3)
