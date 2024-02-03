# librerias utilizadas
library(skimr)
library(tidyr)
library(dplyr)
library(lubridate)
library(survival)
library(ggplot2)
library(ggfortify)
library(survminer)
library(kableExtra)
library(cowplot)

# cargamos la base de datos

survival <- readRDS("proyecto/survival.rds")

# realizamos un resumen
skim(survival)


# Analisis de las variables -----------------------------------------------

# GRAFICOS DE TORTA

genero = data.frame(Sexo = c("Femenino", 'Masculino', 'NA'),
                    n = c(3590, 1326, 84))
g1 = genero %>%  ggplot(aes(x = '', y = n/sum(n), fill = Sexo , 
                            label = scales::percent(n/sum(n)))) +
  geom_bar(stat='identity', color = 'white') +
  ggtitle('Porcentajes de cada sexo') +
  geom_text(vjust = -1, colour = "black") +
  coord_polar(theta = "y") +
  theme_void()

g1

comorbilidad = data.frame(Comorbilidad = c("Si", 'No'),
                          n = c(11, 1326))
g2 = comorbilidad %>%  ggplot(aes(x = '', y = n/sum(n), fill = Comorbilidad , 
                                  label = scales::percent(n/sum(n)))) +
  geom_bar(stat='identity', color = 'white') +
  ggtitle('Porcentajes comorbilidad') +
  geom_text(position=position_stack(vjust=0.5), colour = "black") +
  scale_fill_manual(values=c("orange","steelblue")) +
  coord_polar(theta = "y") +
  theme_void()
g2


# Histograma edades
survival %>%  ggplot(aes(x = edad)) +
  geom_histogram(aes(fill=..count..), col='black', bins = 30) +
  ggtitle("Histograma edades")

grupo_edad = data.frame(Rango = c("21-30", "30-40","40-50", "+50", "NA"),
                        n = c(796, 1728, 1207, 1224, 45))

g3 = grupo_edad %>%  ggplot(aes(x = '', y = n/sum(n), fill = Rango , 
                                label = scales::percent(n/sum(n)))) +
  geom_bar(stat='identity', color = 'white') +
  ggtitle('Porcentajes por rango de edad') +
  geom_text(position=position_stack(vjust=0.5), colour = "black") +
  coord_polar(theta = "y") +
  theme_void()
g3

plot_grid(g1, g2)


obs = data.frame(censura = c("Si", 'No'),
                 n = c(4855, 145))
g4 = obs %>%  ggplot(aes(x = censura, y = n/sum(n), fill = censura , 
                         label = scales::percent(n/sum(n)))) +
  geom_bar(stat='identity', color = 'white') +
  ggtitle('Nivel de censura del estudio') +
  geom_text(vjust = -1, colour = "black") +
  ylim(0, 1.1) +
  ylab('Frecuencia relativa')
g4

# Missing data ------------------------------------------------------------


## Eliminación valores nulos ----------------------------------------------

data_drop <- survival %>% drop_na(c(edad, sexo))


## Imputación por la media de los datos completos -------------------------

surv_imp <- data.frame(survival)

surv_imp$edad[is.na(surv_imp$edad)] = round(mean(data_drop$edad, na.rm = T), 0)

table(surv_imp$sexo)
surv_imp$sexo[is.na(surv_imp$sexo)] = "F"


## Imputacion por la media ------------------------------------------------

surv_imp2 <- data.frame(survival)

surv_imp2$edad[is.na(surv_imp2$edad)] = round(mean(survival$edad, na.rm = T), 0)

aux <- survival[!is.na(survival$sexo),]
surv_imp2$sexo[is.na(surv_imp2$sexo)] = "F"


# Hot deck ----------------------------------------------------------------

# en primer lugar se prepara la base de datos para analizar en SAS
surv_hd <- survival[!is.na(survival$edad),]

inicio = ymd('2020-04-04')
final = ymd('2021-08-01')
surv_hd = surv_hd %>% 
  mutate(
    hospitalizacion = ifelse(is.na(fecha),
                             as.numeric(final - inicio),
                             as.numeric(fecha - inicio)))

surv_hd <- surv_hd %>% mutate(status  = ifelse(hospitalizacion == 484,0,1))

#creamos la base de datos que utilizará sas
ruta <- "definir la ruta/surv_hd.csv"
write.csv(surv_hd, ruta)

# cargamos la base de datos que se creo en SAS
surv_hd <- rio::import("proyecto/surv_hotdeck.csv")
surv_hd <- surv_hd %>% select(c(-UnitID, -ImpIndex))

# creamos los datos censurados
datosSurv <- Surv(time = surv_hd$hospitalizacion, event = surv_hd$status)



# Kaplan Meier ------------------------------------------------------------


## Sexo --------------------------------------------------------------------


sexo_km <- survfit(datosSurv  ~ sexo, data = surv_hd, type="kaplan-meier",
                   conf.type="log-log", conf.int=0.95) 

autoplot(sexo_km)

ggsurvplot(sexo_km, conf.int = TRUE, risk.table = "absolute",
           legend.labs=c("F", "M"), 
           legend = c(0.95, 0.85), break.time.by = 50,
           legend.title = "Sexo",
           censor.shape = "|", xlab = "Días", ylab = "Sobrevida",
           ylim = c(0.95,1))

test_sexo <-survdiff(datosSurv ~ sexo, data = surv_hd)
test_sexo


## Comorbilidades ---------------------------------------------------------

comorbilidad_km <- survfit(datosSurv  ~ comorbilidades,
                           data = surv_hd, type="kaplan-meier",
                           conf.type="log-log", conf.int=0.95) 

autoplot(comorbilidad_km)

ggsurvplot(comorbilidad_km, conf.int = TRUE, risk.table = "absolute",
           legend.labs=c("Sin comorbilidad", "Con comorbilidad"), 
           legend = c(0.1, 0.25), break.time.by = 44,
           legend.title = "Tipo de Comorbilidad",
           censor.shape = "|", xlab = "Días", ylab = "Sobrevida")

test_comorbilidad <-survdiff(datosSurv ~ comorbilidades, data = surv_hd)
test_comorbilidad


## Edad ------------------------------------------------------------------

surv_hd$edad <- as.numeric(surv_hd$edad)
surv_hd = surv_hd %>% 
  mutate(
    # Create categories
    age_group = dplyr::case_when(
      edad <= 30            ~ "21 - 30",
      edad > 30 & edad <= 40 ~ "30 - 40",
      edad > 40 & edad <= 50 ~ "40 - 50",
      edad > 50             ~ "+ 50"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("21 - 30", "30 - 40","40 - 50", "+ 50")
    )
  )


edad_km <- survfit(datosSurv  ~ age_group, data = surv_hd,
                   type="kaplan-meier", conf.type="log-log", conf.int=0.95)

autoplot(edad_km)

ggsurvplot(edad_km, conf.int = TRUE, risk.table = "absolute",
           legend.labs=c("21-30", "31-40","41-50","+50"), 
           legend = c(0.07, 0.35), break.time.by = 44,
           legend.title = "Rango edad",
           censor.shape = "|", xlab = "Días", ylab = "Sobrevida",
           ylim = c(0.96,1))

test_edad <-survdiff(datosSurv ~ age_group, data = surv_hd)
test_edad


# Modelo de Cox -----------------------------------------------------------

# por si se quiere cambiar la estimacion del genero por mujer
# surv_hd <- surv_hd %>% mutate(genero = ifelse(sexo == "F", 1, 0))

# modelo_cox <-
#   coxph(Surv(time = surv_hd$hospitalizacion, event = surv_hd$status) ~
#           edad + sexo + comorbilidades, data = surv_hd)

surv_hd$comorbilidades <- as.factor(surv_hd$comorbilidades)
modelo_cox <-
  coxph(Surv(time = surv_hd$hospitalizacion, event = surv_hd$status) ~
          edad + sexo + comorbilidades, data = surv_hd)

summary(modelo_cox)

cox.zph(modelo_cox)

ggcoxzph(cox.zph(modelo_cox))
