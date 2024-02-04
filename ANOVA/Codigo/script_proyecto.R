
# cargamos las librerias
library(tidyverse)
library(gridExtra)
library(cowplot)
library(missForest)
library(ggpubr)
library(rstatix)

# cargamos los datos
# datos <- readxl::read_excel("Proyecto/choques_2.xlsx", na = "-")
datos <- readxl::read_excel("choques_2.xlsx")

# cambiaremos el nombre de las variables
nombres <- names(datos) #extraemos el nombre por variable
nombres[c(4,5,6,7,8, 9)] <- c("carID_year", "Head_IC", "Chest_decel", "L_leg",
                           "R_leg", "DP") # nombres que cambiamos
colnames(datos) <- nombres # agregamos los nuevos nombres

# resumen de los datos
skimr::skim(datos)

# En los datos no aparecen nulos, esto se debe a la funcion de Excel

table(datos$make) # todo bien
table(datos$Model) |> as.data.frame()       # todo bien
table(datos$carID) |> as.data.frame()       # todo bien
table(datos$carID_year) |> as.data.frame()  # todo bien
table(datos$Head_IC) |> as.data.frame()     # hay datos nd (que podria interpretarse como no determinado)
table(datos$Chest_decel) |> as.data.frame() # tambien hay nd
table(datos$L_leg) |> as.data.frame()       # tambien hay nd
table(datos$R_leg) |> as.data.frame()       # tambien hay nd
table(datos$DP) |> as.data.frame()          # todo bien
table(datos$Protection) |> as.data.frame()  # todo bien
table(datos$Doors) |> as.data.frame()       # "-" = nulo
table(datos$Year) |> as.data.frame()        # todo bien
table(datos$Wt) |> as.data.frame()          # todo bien
table(datos$Size) |> as.data.frame()        # todo bien


# Transformación NA -------------------------------------------------------

# Cambiamos los valores nd por NA y los valores "-"
datos <- datos |> 
  mutate(Head_IC = ifelse(Head_IC == "nd", NA, Head_IC)) |>
  mutate(Chest_decel = ifelse(Chest_decel == "nd", NA, Chest_decel)) |> 
  mutate(L_leg = ifelse(L_leg == "nd", NA, L_leg)) |>
  mutate(R_leg = ifelse(R_leg == "nd", NA, R_leg)) |>
  mutate(Doors = ifelse(Doors == "-", NA, Doors))

table(datos$make) # todo bien
table(datos$Model) |> as.data.frame()                          # todo bien
table(datos$carID) |> as.data.frame()                          # todo bien
table(datos$carID_year) |> as.data.frame()                     # todo bien
table(datos$Head_IC, useNA = "ifany") |> as.data.frame()       # todo bien
table(datos$Chest_decel, useNA = "ifany") |> as.data.frame()   # todo bien
table(datos$L_leg, useNA = "ifany") |> as.data.frame()         # todo bien
table(datos$R_leg, useNA = "ifany") |> as.data.frame()         # todo bien                                                                         ien hay nd
table(datos$DP) |> as.data.frame()                          # todo bien
table(datos$Protection) |> as.data.frame()                     # todo bien
table(datos$Doors, useNA = "ifany") |> as.data.frame()         # todo bien
table(datos$Year) |> as.data.frame()                           # todo bien
table(datos$Wt) |> as.data.frame()                             # todo bien
table(datos$Size) |> as.data.frame()                           # todo bien

# Transformacion de los datos al tip correspondiente

glimpse(datos)

datos$Head_IC <- as.numeric(datos$Head_IC)
datos$Chest_decel <- as.numeric(datos$Chest_decel)
datos$L_leg <- as.numeric(datos$L_leg)
datos$R_leg <- as.numeric(datos$R_leg)
datos$Doors <- as.numeric(datos$Doors)


# Análisis datos faltantes ------------------------------------------------

skimr::skim(datos)

# veamos los datos que tienen datos faltantes segun variables


## Head IC -----------------------------------------------------------------

# Porcentaje Missing
12/352
# 3.4 %

missing_1 <- datos |> 
  filter(is.na(Head_IC))

# Notamos que no hay un patron claro. Solo que con auto mayoritariamente donde
# estaba en el asiento del pasajero y autos del año 90 o 91.

## Chest_decel -------------------------------------------------------------

# Porcentaje Missing
11/352
# 3.1 %

missing_2 <- datos |> 
  filter(is.na(Chest_decel))

# No se ve un patron claro


## L-leg -------------------------------------------------------------------

# Porcentaje Missing
9/352
# 2.5 %

missing_3 <- datos |> 
  filter(is.na(L_leg))

# Todos son cinturones 


## R-leg -------------------------------------------------------------------

# Porcentaje Missing
11/352
# 3.1 %

missing_4 <- datos |> 
  filter(is.na(R_leg))

# No se ven patrones

## Doors -------------------------------------------------------------------

# Porcentaje Missing
66/352
# 18.75 %

missing_5 <- datos |> 
  filter(is.na(Doors))

# todas son pu o van

# comp: compacto (2.8–3.1 m3)
# mid: mid-size (3.1–3.4 m3)

a <- datos |> 
  filter(Size == "van") |> 
  select(carID_year, Wt) |> 
  unique()




# Imputación --------------------------------------------------------------

## Imputación puertas autos ------------------------------------------------

# Como conocemos marca modelo y año podemos buscar en fuentes externas la
# cantidad de puertas que tiene un modelo

# Chevrolet c-1500: dos puertas
# https://en.wikipedia.org/wiki/Chevrolet_C/K_(fourth_generation)

# Chevrolet s10 pick-up y s10 pick-up 4x4: dos puertas
# https://bibipedia.info/es/tech_harakteristiki/chevrolet/s10_pickup/s10_pickup_1987_-_1993

# Dodge D-150: Hay versiones de 4 o 2 puertas

# Dodge Dakota 87: Hay versiones de 4 o 2 puertas

# Ford F-150 88: Dos puertas

# Ford Ranger 87: Hay versiones de 4 o 2 puertas

# Ford Ranger 90: Hay versiones de 4 o 2 puertas

# Isuzu Spacecab 87: cuatro puertas

# Isuzu Spacecab 88: cuatro puertas

# Jeep comanche 87: dos puertas

# Nissan NL Xev Pickup 88: dos puertas

# Nissan Pickup 87: dos puertas

# Nissan Pickup 89: dos puertas

# Toyota Pickup 87: dos puertas

# Toyota Pickup 89: dos puertas

# Toyota Pickup 4x4 91: dos o cuatro puertas

## Vans

# Chevrolet Astro 88:

# Chevrolet Astro 89:

lista <- unique(missing_5['carID_year'])
datos <- datos %>%  mutate(puertas =
                             case_when(carID_year == c(lista[1, ]) ~ 3,
                                       carID_year == c(lista[2, ]) | 
                                         carID_year == c(lista[4, ]) |
                                         carID_year == c(lista[5, ]) |
                                         carID_year == c(lista[11, ]) |
                                         carID_year == c(lista[19, ]) |
                                         carID_year == c(lista[25, ]) |
                                         carID_year == c(lista[26, ]) |
                                         carID_year == c(lista[27, ]) |
                                         carID_year == c(lista[31, ]) |
                                         carID_year == c(lista[32, ]) |
                                         carID_year == c(lista[33, ]) ~ 3,
                                       carID_year == c(lista[3, ]) |
                                         carID_year == c(lista[20, ]) |
                                         carID_year == c(lista[21, ]) |
                                         carID_year == c(lista[22, ]) ~ 4,
                                       carID_year == c(lista[6, ]) |
                                         carID_year == c(lista[7, ]) |
                                         carID_year == c(lista[8, ]) |
                                         carID_year == c(lista[9, ]) |
                                         carID_year == c(lista[10, ]) |
                                         carID_year == c(lista[12, ]) |
                                         carID_year == c(lista[13, ]) |
                                         carID_year == c(lista[14, ]) |
                                         carID_year == c(lista[15, ]) |
                                         carID_year == c(lista[16, ]) |
                                         carID_year == c(lista[17, ]) |
                                         carID_year == c(lista[18, ]) |
                                         carID_year == c(lista[23, ]) |
                                         carID_year == c(lista[24, ]) |
                                         carID_year == c(lista[28, ]) |
                                         carID_year == c(lista[29, ]) |
                                         carID_year == c(lista[30, ]) ~ 2,
                                       TRUE ~ Doors))



## Imputación en daño ------------------------------------------------------

# Ahora analizaremos los efectos de imputar


### Eliminar datos faltantes ------------------------------------------------

datos_missingnt <- datos |> 
  filter(complete.cases(datos))

datos_missingnt |> 
  select(c(Head_IC, Chest_decel, L_leg, R_leg, Wt)) |> 
  psych::describe()


# Miss Forest -------------------------------------------------------------

# elinamos modelo porque no soporta muchas variables categoricas
data <- datos |> 
  select(-c(Doors, carID_year, carID, Model))

data$make <- as.factor(data$make)
data$DP <- as.factor(data$DP)
data$Protection <- as.factor(data$Protection)
data$Size <- as.factor(data$Size)

set.seed(1234)
data_missforest <- missForest(as.data.frame(data), maxiter = 50)$ximp

datos |> 
  filter(!is.na(Head_IC)) |> 
  psych::describe()

data_missforest |> 
  select(c(Head_IC, Chest_decel, L_leg, R_leg, Wt)) |> 
  psych::describe()



# Elección de la variable respuesta ---------------------------------------

datos2 <- data_missforest

damage2 <- datos2 |> 
  select(c("Head_IC", "Chest_decel", "L_leg", "R_leg"))
damage2

damage_normal <- scale(damage2)
# Determinacion variable respuesta ----------------------------------------

# escalamos los datos, tienen diferente unidades de medida, por lo que es 
# "mas" justo estandarizadar las variables, de lo contrario la variable
# Chest_decel no tendría influencia en la definición de la nueva variable

damage_est <- scale(damage2) |> as.data.frame()

var_damage <- damage_est$Head_IC * 0.4 + damage_est$Chest_decel * 0.4 + damage_est$L_leg * 0.1 + damage_est$R_leg * 0.1

datos2 <- data_missforest

datos2 <- datos2 |> 
  mutate(damage = var_damage) |> 
  mutate(modelo = datos$Model, .after = 1)

datos4 <- datos2 |> 
  mutate(damage = log(damage + 2)) %>% 
  select(-c(Head_IC, Chest_decel, L_leg, R_leg, modelo))


g1 = ggplot(datos2, aes(x = damage)) + 
  geom_histogram(fill="salmon", alpha = 0.5) +
  labs(title= "Sin transformación",x="Daño", y = "Frecuencia") +
  theme(plot.title = element_text(hjust = 0.5))

g2 = ggplot(datos2, aes(x = (log(damage + 2)))) + 
  geom_histogram(fill="salmon", alpha = 0.5) +
  labs(title= "Con transformación",x="Daño", y = "Frecuencia") +
  theme(plot.title = element_text(hjust = 0.5))



datos4 %>%
  group_by(DP, Size) %>%
  summarise(Daño = mean(damage), n())

datos4 %>%
  ggplot() +
  aes(x = Wt, y = damage) +
  geom_point(color = 'salmon') +
  labs(title = 'Grafico de dispersón Peso vs Daño') +
  theme(plot.title = element_text(hjust = 0.5))

cor(datos4$Wt, datos4$damage)

datos4 %>%
  group_by(Protection, puertas) %>%
  summarise(Daño = mean(damage)) -> tips2

Pp <- tips2 %>%
  ggplot() +
  aes(x = Protection, y = Daño, color = puertas) +
  geom_line(aes(group = puertas)) +
  geom_point() +
  labs(title = 'Grafico interacción Puertas y Proteccion') +
  theme(plot.title = element_text(hjust = 0.5))


TP <- tips2 %>%
  ggplot() +
  aes(x = Protection, y = Daño, color = Size) +
  geom_line(aes(group = Size)) +
  geom_point() +
  labs(title = 'Grafico interacción Tamaño y Protección') +
  theme(plot.title = element_text(hjust = 0.5))


Td <- tips2 %>%
  ggplot() +
  aes(x = DP, y = Daño, color = Size) +
  geom_line(aes(group = Size)) +
  geom_point() +
  labs(title = 'Grafico interacción Tamaño y DP') +
  theme(plot.title = element_text(hjust = 0.5))


pd <- tips2 %>%
  ggplot() +
  aes(x = DP, y = Daño, color = Protection) +
  geom_line(aes(group = Protection)) +
  geom_point() +
  labs(title = 'Grafico interacción Protección y DP') +
  theme(plot.title = element_text(hjust = 0.5))


Dp <- tips2 %>%
  ggplot() +
  aes(x = DP, y = Daño, color = puertas) +
  geom_line(aes(group = puertas)) +
  geom_point() +
  labs(title = 'Grafico interacción Puertas y DP') +
  theme(plot.title = element_text(hjust = 0.5))

sp <- tips2 %>%
  ggplot() +
  aes(x = make, y = Daño, color = puertas) +
  geom_line(aes(group = puertas)) +
  geom_point() +
  labs(title = 'Grafico interacción Puertas y Tamaño') +
  theme(plot.title = element_text(hjust = 0.5))


p = plot_grid(Pp, TP, Td , pd, Dp, sp, ncol = 3, nrow = 2)
title = ggdraw() + draw_label("Gráficos interacción", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))



### Selección forward -------------------------------------------------------


# creamos un modelo add que será el que cambiará para el modelo add1
modelo_add <- lm(damage ~ 1, data = datos4)

# ajustamos el modelo completo para utilizar la formula
modelo_completo <- lm(damage ~ . -make, data = datos4)

drop1(modelo_completo, test = "F")
# realizamos el add1
#add1(modelo_add, scope = formula(modelo_completo), test = "F")

modelo_drop <- lm(damage ~ .-Wt-make, datos4)

drop1(modelo_drop, test = "F")

modelo_drop <- lm(damage ~ .-Wt-make-Year, datos4)
drop1(modelo_drop, test = "F")


aov(damage ~ .-Wt-make-Year, datos4) %>% summary()
modelo_drop %>% summary()

ggqqplot(residuals(modelo_drop))
shapiro_test(residuals(modelo_drop))

library(DescTools)
PlotQQ(residuals(modelo_drop))
car::qqPlot(residuals(modelo_drop), main = 'Gráfico cuantil-cuantil',
            xlab= "Quantiles", ylab = 'Residuos')
?qqPlot
datos4 %>%
  group_by(DP) %>%
  shapiro_test(damage)

datos4 %>%
  group_by(Size) %>%
  shapiro_test(damage)

datos4 %>%
  group_by(Protection) %>%
  shapiro_test(damage)

datos4 %>%
  group_by(Protection) %>%
  summarise(shapiro_test(damage), n())

datos4 %>%
  group_by(Protection) %>%
  summarise(statistic = shapiro.test(damage)$statistic,
            p.value = shapiro.test(damage)$p.value)

acs <-
  datos4 |> 
  mutate(yhat = fitted(modelo_drop),
         res_sqrt = sqrt(abs(rstandard(modelo_drop))))

ggplot(acs, aes(yhat, res_sqrt)) +
  geom_point() +
  geom_smooth()
datos4 = datos4 %>%
  mutate(g = ifelse(Protection == 'd&p airbags',
                    4, Protection))

# Perform Breusch-Pagan test to check for homoscedasticity
bartlett.test(damage ~ DP, data = datos4)
bartlett.test(damage ~ Protection, data = datos4)
bartlett.test(damage ~ puertas, data = datos4)
bartlett.test(damage ~ Size, data = datos4)

kruskal.test(damage ~ DP, data = datos4)
kruskal.test(damage ~ Protection, data = datos4)
kruskal.test(damage ~ puertas, data = datos4)
kruskal.test(damage ~ Size, data = datos4)

hmctest(damage ~ DP, data = datos4)
hmctest(damage ~ Protection, data = datos4)
hmctest(damage ~ puertas, data = datos4)
hmctest(damage ~ Size, data = datos4)

kruskal.test(weight ~ group, data = my_data)


library(lmtest)
bptest(modelo_drop)

library(nortest)
lillie.test(residuals(modelo_drop))


bx_size <- ggplot(datos4, aes(x = Size, y = damage, fill = Size)) +
  geom_boxplot(alpha=0.5) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")

datos4$puertas <- as.factor(datos4$puertas)

bx_puertas <- ggplot(datos4, aes(x = puertas, y = damage, fill = puertas)) +
  geom_boxplot(alpha=0.5) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")

bx_dp <- ggplot(datos4, aes(x = DP, y = damage, fill = DP)) +
  geom_boxplot(alpha=0.5) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")


# Protection

bx_sp <- ggplot(datos4, aes(x = Protection, y = damage, fill = Size)) +
  geom_boxplot(alpha=0.5) +
  scale_fill_brewer(palette="Dark2") +
  labs(title = 'Protection y Size')+
  theme(plot.title = element_text(hjust = 0.5))

bx_pp <- ggplot(datos4, aes(x = Protection, y = damage, fill = puertas)) +
  geom_boxplot(alpha=0.5) +
  scale_fill_brewer(palette="Dark2")+
  labs(title = 'Protection y Puertas')+
  theme(plot.title = element_text(hjust = 0.5))

bx_ppdp <- ggplot(datos4, aes(x = Protection, y = damage, fill = DP)) +
  geom_boxplot(alpha=0.5) +
  scale_fill_brewer(palette="Dark2")+
  labs(title = 'Protection y DP') +
  theme(plot.title = element_text(hjust = 0.5))

#puertas
bx_ps <- ggplot(datos4, aes(x = puertas, y = damage, fill = Size)) +
  geom_boxplot(alpha=0.5) +
  scale_fill_brewer(palette="Dark2")+
  labs(title = 'Puertas y Size')+
  theme(plot.title = element_text(hjust = 0.5))

bx_pdp <- ggplot(datos4, aes(x = puertas, y = damage, fill = DP)) +
  geom_boxplot(alpha=0.5) +
  scale_fill_brewer(palette="Dark2") +
  labs(title = 'Puertas y DP') +
  theme(plot.title = element_text(hjust = 0.5))

# Size
bx_dps <- ggplot(datos4, aes(x = DP, y = damage, fill = Size)) +
  geom_boxplot(alpha=0.5) +
  scale_fill_brewer(palette="Dark2") +
  labs(title = 'Size y DP') +
  theme(plot.title = element_text(hjust = 0.5))



p = plot_grid(bx_sp, bx_pp, bx_ppdp, bx_ps, bx_pdp, bx_dps, ncol = 3, nrow = 2)
title = ggdraw() + draw_label("Gráficos Boxplot Interacción", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

p

datos4 <- datos4 |> 
  mutate(Size2 = case_when(Size == "comp" ~ "liviano",
                           Size == "hev" ~ "liviano",
                           Size == "lt" ~ "liviano",
                           Size == "med" ~ "liviano",
                           Size == "mini" ~ "liviano",
                           TRUE ~ Size)) |> 
  mutate(Protection2 = case_when(Protection == "d airbag" ~ "airbag",
                                 Protection == "d&p airbags" ~ "airbag",
                                 TRUE ~ "belts"))


modelo <- lm(datos4$damage ~ Size2 + Protection2 + DP + puertas + Size2:puertas, data = datos4)








