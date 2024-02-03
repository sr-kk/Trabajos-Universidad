# Utilizo la variable datos2 del script "script"

# Existe un dato sin información de la vacuna. Se ignora.
datos2 = datos2 %>% filter(!is.na(Nombre_Vacuna))


skim(datos2)


# Principal interes --> ver si implementar un sist. de farmaco-vigilancia
#
# Farmaco-vigilancia : detección, evaluación, comprensión y prevención 
#                      de los efectos adversos asociados al uso de los 
#                      medicamentos.


########################################-
#### TIPO DE DESENLACE SEGUN VACUNA ####
########################################-

tabla_vacuna.desenlace = table(datos2$Nombre_Vacuna, datos2$Desenlace_ESAVI)

# P(Desconocido|Pfizer)
tabla_vacuna.desenlace["Pfizer","DESCONOCIDO"] / sum(tabla_vacuna.desenlace["Pfizer",])
# P(Desconocido|Astrazeneca)
tabla_vacuna.desenlace["Astrazeneca", "DESCONOCIDO"] / sum(tabla_vacuna.desenlace["Astrazeneca",])


# P(Recuperacion|Pfizer)
tabla_vacuna.desenlace["Pfizer","EN RECUPERACION/ EN RESOLUCION"] / sum(tabla_vacuna.desenlace["Pfizer",])
# P(Recuperacion|Astrazeneca)
tabla_vacuna.desenlace["Astrazeneca", "EN RECUPERACION/ EN RESOLUCION"] / sum(tabla_vacuna.desenlace["Astrazeneca",])


# P(Muerte|Pfizer)
tabla_vacuna.desenlace["Pfizer","MORTAL"] / sum(tabla_vacuna.desenlace["Pfizer",])
# P(Muerte|Astrazeneca)
tabla_vacuna.desenlace["Astrazeneca","MORTAL"] / sum(tabla_vacuna.desenlace["Astrazeneca",])


# P(no recuperado|Pfizer)
tabla_vacuna.desenlace["Pfizer","NO RECUPERADO/ NO RESUELTO"] / sum(tabla_vacuna.desenlace["Pfizer",])
# P(no recuperado|Astrazeneca)
tabla_vacuna.desenlace["Astrazeneca","NO RECUPERADO/ NO RESUELTO"] / sum(tabla_vacuna.desenlace["Astrazeneca",])


# P(Recuperado|Pfizer)
tabla_vacuna.desenlace["Pfizer","RECUPERADO/ RESUELTO"] / sum(tabla_vacuna.desenlace["Pfizer",])
# P(Recuperado|Astrazeneca)
tabla_vacuna.desenlace["Astrazeneca","RECUPERADO/ RESUELTO"] / sum(tabla_vacuna.desenlace["Astrazeneca",])


# P(Secuelas|Pfizer)
tabla_vacuna.desenlace["Pfizer","RECUPERADO/RECUPERADO CON SECUELAS"] / sum(tabla_vacuna.desenlace["Pfizer",])
# P(Secuelas|Astrazeneca)
tabla_vacuna.desenlace["Astrazeneca","RECUPERADO/RECUPERADO CON SECUELAS"] / sum(tabla_vacuna.desenlace["Astrazeneca",])



###############################################-
#### TIPO DE DESENLACE SEGUN VACUNA Y SEXO ####
###############################################-

# No nos interesan las personas con sexo NI
datos3 = datos2 %>% filter(Sexo %in% c("F", "M"))

total_sexo = table(datos3$Sexo)
tabla_sexo.vacuna = table(datos3$Sexo, datos3$Nombre_Vacuna)
tabla_sexo.vacuna.desenlace = table(datos3$Desenlace_ESAVI, 
                                    datos3$Nombre_Vacuna,
                                    datos3$Sexo) 

# P(Desconocido | Hombre, Pfizer) = P(Desconocido, Hombre, pfizer) / P(Hombre, Pfizer)
(tabla_sexo.vacuna.desenlace["DESCONOCIDO", "Pfizer","M"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["M","Pfizer"] / sum(tabla_sexo.vacuna))

# P(Desconocido | Hombre, Astrazeneca) = P(Desconocido, Hombre, Astrazeneca) / P(Hombre, Astrazeneca)
(tabla_sexo.vacuna.desenlace["DESCONOCIDO", "Astrazeneca","M"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["M","Astrazeneca"] / sum(tabla_sexo.vacuna))

# P(Desconocido | Mujer, Pfizer) = P(Desconocido, Mujer, pfizer) / P(Mujer, Pfizer)
(tabla_sexo.vacuna.desenlace["DESCONOCIDO", "Pfizer","F"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["F","Pfizer"] / sum(tabla_sexo.vacuna))

# P(Desconocido | Mujer, Astrazeneca) = P(Desconocido, Mujer, Astrazeneca) / P(Mujer, Astrazeneca)
(tabla_sexo.vacuna.desenlace["DESCONOCIDO", "Astrazeneca","F"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["F","Astrazeneca"] / sum(tabla_sexo.vacuna))



# P(Recuperacion | Hombre, Pfizer) = P(Recuperacion, Hombre, pfizer) / P(Hombre, Pfizer)
(tabla_sexo.vacuna.desenlace["EN RECUPERACION/ EN RESOLUCION", "Pfizer","M"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["M","Pfizer"] / sum(tabla_sexo.vacuna))

# P(Recuperacion | Hombre, Astrazeneca) = P(Recuperacion, Hombre, Astrazeneca) / P(Hombre, Astrazeneca)
(tabla_sexo.vacuna.desenlace["EN RECUPERACION/ EN RESOLUCION", "Astrazeneca","M"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["M","Astrazeneca"] / sum(tabla_sexo.vacuna))

# P(Recuperacion | Mujer, Pfizer) = P(Recuperacion, Mujer, pfizer) / P(Mujer, Pfizer)
(tabla_sexo.vacuna.desenlace["EN RECUPERACION/ EN RESOLUCION", "Pfizer","F"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["F","Pfizer"] / sum(tabla_sexo.vacuna))

# P(Recuperacion | Mujer, Astrazeneca) = P(Recuperacion, Mujer, Astrazeneca) / P(Mujer, Astrazeneca)
(tabla_sexo.vacuna.desenlace["EN RECUPERACION/ EN RESOLUCION", "Astrazeneca","F"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["F","Astrazeneca"] / sum(tabla_sexo.vacuna))



# P(Muerte | Hombre, Pfizer) = P(Muerte, Hombre, pfizer) / P(Hombre, Pfizer)
(tabla_sexo.vacuna.desenlace["MORTAL", "Pfizer","M"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
(tabla_sexo.vacuna["M","Pfizer"] / sum(tabla_sexo.vacuna))

# P(Muerte | Hombre, Astrazeneca) = P(Muerte, Hombre, Astrazeneca) / P(Hombre, Astrazeneca)
(tabla_sexo.vacuna.desenlace["MORTAL", "Astrazeneca","M"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["M","Astrazeneca"] / sum(tabla_sexo.vacuna))

# P(Muerte | Mujer, Pfizer) = P(Muerte, Mujer, pfizer) / P(Mujer, Pfizer)
(tabla_sexo.vacuna.desenlace["MORTAL", "Pfizer","F"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["F","Pfizer"] / sum(tabla_sexo.vacuna))

# P(Muerte | Mujer, Astrazeneca) = P(Muerte, Mujer, Astrazeneca) / P(Mujer, Astrazeneca)
(tabla_sexo.vacuna.desenlace["MORTAL", "Astrazeneca","F"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["F","Astrazeneca"] / sum(tabla_sexo.vacuna))



"AQUI SERIA INTERESANTE SABER EL TIEMPO ENTRE QUE SE VACUNA Y QUE AÚN NO REPORTA MEJORA"
# P(No recuperado | Hombre, Pfizer) = P(No recuperado, Hombre, pfizer) / P(Hombre, Pfizer)
(tabla_sexo.vacuna.desenlace["NO RECUPERADO/ NO RESUELTO", "Pfizer","M"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["M","Pfizer"] / sum(tabla_sexo.vacuna))

# P(No recuperado | Hombre, Astrazeneca) = P(No recuperado, Hombre, Astrazeneca) / P(Hombre, Astrazeneca)
(tabla_sexo.vacuna.desenlace["NO RECUPERADO/ NO RESUELTO", "Astrazeneca","M"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["M","Astrazeneca"] / sum(tabla_sexo.vacuna))

# P(No recuperado | Mujer, Pfizer) = P(No recuperado, Mujer, pfizer) / P(Mujer, Pfizer)
(tabla_sexo.vacuna.desenlace["NO RECUPERADO/ NO RESUELTO", "Pfizer","F"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["F","Pfizer"] / sum(tabla_sexo.vacuna))

# P(No recuperado | Mujer, Astrazeneca) = P(No recuperado, Mujer, Astrazeneca) / P(Mujer, Astrazeneca)
(tabla_sexo.vacuna.desenlace["NO RECUPERADO/ NO RESUELTO", "Astrazeneca","F"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["F","Astrazeneca"] / sum(tabla_sexo.vacuna))



# P(Recuperado | Hombre, Pfizer) = P(Recuperado, Hombre, pfizer) / P(Hombre, Pfizer)
(tabla_sexo.vacuna.desenlace["RECUPERADO/ RESUELTO", "Pfizer","M"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["M","Pfizer"] / sum(tabla_sexo.vacuna))

# P(Recuperado | Hombre, Astrazeneca) = P(Recuperado, Hombre, Astrazeneca) / P(Hombre, Astrazeneca)
(tabla_sexo.vacuna.desenlace["RECUPERADO/ RESUELTO", "Astrazeneca","M"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["M","Astrazeneca"] / sum(tabla_sexo.vacuna))

# P(Recuperado | Mujer, Pfizer) = P(Recuperado, Mujer, pfizer) / P(Mujer, Pfizer)
(tabla_sexo.vacuna.desenlace["RECUPERADO/ RESUELTO", "Pfizer","F"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["F","Pfizer"] / sum(tabla_sexo.vacuna))

# P(Recuperado | Mujer, Astrazeneca) = P(Recuperado, Mujer, Astrazeneca) / P(Mujer, Astrazeneca)
(tabla_sexo.vacuna.desenlace["RECUPERADO/ RESUELTO", "Astrazeneca","F"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["F","Astrazeneca"] / sum(tabla_sexo.vacuna))



# P(Secuelas | Hombre, Pfizer) = P(Secuelas, Hombre, pfizer) / P(Hombre, Pfizer)
(tabla_sexo.vacuna.desenlace["RECUPERADO/RECUPERADO CON SECUELAS", "Pfizer","M"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["M","Pfizer"] / sum(tabla_sexo.vacuna))

# P(Secuelas | Hombre, Astrazeneca) = P(Secuelas, Hombre, Astrazeneca) / P(Hombre, Astrazeneca)
(tabla_sexo.vacuna.desenlace["RECUPERADO/RECUPERADO CON SECUELAS", "Astrazeneca","M"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["M","Astrazeneca"] / sum(tabla_sexo.vacuna))

# P(Secuelas | Mujer, Pfizer) = P(Secuelas, Mujer, pfizer) / P(Mujer, Pfizer)
(tabla_sexo.vacuna.desenlace["RECUPERADO/RECUPERADO CON SECUELAS", "Pfizer","F"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["F","Pfizer"] / sum(tabla_sexo.vacuna))

# P(Secuelas | Mujer, Astrazeneca) = P(Secuelas, Mujer, Astrazeneca) / P(Mujer, Astrazeneca)
(tabla_sexo.vacuna.desenlace["RECUPERADO/RECUPERADO CON SECUELAS", "Astrazeneca","F"] / 
    sum(tabla_sexo.vacuna.desenlace) ) /
  (tabla_sexo.vacuna["F","Astrazeneca"] / sum(tabla_sexo.vacuna))




datos4 = readRDS("datos_covid.rds")

datos4 %>% filter(Nombre_Vacuna == "AZTRAZENECA") %>% 
  group_by(Esavi_1) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  print(n=5)


datos4 %>% filter(Nombre_Vacuna == "PFIZER") %>% 
  group_by(Esavi_1) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  print(n=5)



datos4 %>% 
  filter(Esavi_1 %in% c("CEFALEA", "DOLOR", "FIEBRE", "MIALGIA", "ESCALOFRIO", "MALESTAR")) %>% 
  ggplot(aes(Esavi_1, fill = Nombre_Vacuna)) +
  geom_bar(position = "dodge", colour = "black") +
  labs(x = "ESAVI", y = "Frecuencia", 
       title = "Eventos más comunes reportados según tipo de vacuna")

unique(datos4$Desenlace_ESAVI)





