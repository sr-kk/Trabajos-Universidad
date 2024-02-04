library(rio)
library(dplyr)
library(tidyverse)
library(skimr)
library(GGally)
library(ggplot2)

##### Parte a) #####


datos <- rio::import(file = "UNData.csv")


# Analisis exploratorio ---------------------------------------------------

skimr::skim(datos)

tabla <- as.data.frame(describe(datos))
class(tabla)
tabla <- tabla %>% select(c(mean, sd, median, min, max)) %>% slice(4:n()) %>% round(2)

colnames(tabla) <- c("Media", "Desviación Estándar", "Mediana", "Mínimo", "Máximo")

tabla %>%
  knitr::kable(align = "lllll", format = "latex", longtable = T, booktabs = T, escape = FALSE, caption = "hola")



## Box-plot ----------------------------------------------------------------


### Fertility ---------------------------------------------------------------

# hacer eje y mas grande

bpf1 <- datos %>% ggplot(aes(y = fertility, fill = region)) +
  theme_minimal() + 
  theme(axis.title.y = element_text(size = 15),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "top") +
  geom_boxplot() +
  scale_fill_discrete(name = "Región", labels=c("África", "Asia", "Caribe", "Europa", 
                                                "América Latina", "Amética del Norte", 
                                                "Atlántico Norte", "Oceanía")) +
  labs(title = "Box-plot de Fertilidad según región", 
       y = "Fertilidad (tasa de natalidad)")


bpf2 <- datos %>% ggplot(aes(y = fertility, fill = group)) +
  theme_minimal() + 
  theme(axis.title.y = element_text(size = 15),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "top") +
  geom_boxplot() +
  scale_fill_discrete(name = "Grupo", labels=c("África", "OECD", "Otros")) +
  labs(title = "Box-plot de Fertilidad según grupo", 
       y = "Fertilidad (tasa de natalidad)")

grid.arrange(bpf1, bpf2, ncol=2)

### Producto nacional bruto -------------------------------------------------

bppp1 <- datos %>% ggplot(aes(y = ppgdp, fill = region)) +
  theme_minimal() + 
  theme(axis.title.y = element_text(size = 15),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "top") +
  geom_boxplot() +
  scale_fill_discrete(name = "Región", labels=c("África", "Asia", "Caribe", "Europa", 
                                                "América Latina", "Amética del Norte", 
                                                "Atlántico Norte", "Oceanía")) +
  labs(title = "Box-plot Producto Nacional Bruto según región", 
       y = "Producto nacional bruto por persona")


bppp2 <-datos %>% ggplot(aes(y = ppgdp, fill = group)) +
  theme_minimal() + 
  theme(axis.title.y = element_text(size = 15),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "top") +
  geom_boxplot() +
  scale_fill_discrete(name = "Grupo", labels=c("África", "OECD", "Otros")) +
  labs(title = "Box-plot Producto Nacional Bruto según grupo", 
       y = "Producto nacional bruto por persona")

grid.arrange(bppp1, bppp2, ncol=2)

### Esperanza de vida -------------------------------------------------------

bpl1 <- datos %>% ggplot(aes(y = lifeExpF, fill = region)) +
  theme_minimal() + 
  theme(axis.title.y = element_text(size = 15),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "top") +
  geom_boxplot() +
  scale_fill_discrete(name = "Región", labels=c("África", "Asia", "Caribe", "Europa", 
                                                "América Latina", "Amética del Norte", 
                                                "Atlántico Norte", "Oceanía")) +
  labs(title = "Box-plot Esperanza de vida según región", 
       y = "Esperanza de Vida")

bpl2 <-datos %>% ggplot(aes(y = lifeExpF, fill = group)) +
  theme_minimal() + 
  theme(axis.title.y = element_text(size = 15),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "top") +
  geom_boxplot() +
  scale_fill_discrete(name = "Grupo", labels=c("África", "OECD", "Otros")) +
  labs(title = "Box-plot Esperanza de vida según grupo", 
       y = "Esperanza de Vida")

grid.arrange(bpl1, bpl2, ncol=2)


### Porcentaje de poblacion urbana ------------------------------------------

bppc1 <- datos %>% ggplot(aes(y = pctUrban, fill = region)) +
  theme_minimal() + 
  theme(axis.title.y = element_text(size = 15),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "top") +
  geom_boxplot() +
  scale_fill_discrete(name = "Región", labels=c("África", "Asia", "Caribe", "Europa", 
                                                "América Latina", "Amética del Norte", 
                                                "Atlántico Norte", "Oceanía")) +
  labs(title = "Box-plot Porcentaje de poblacion urbana según región", 
       y = "Porcentaje de población urbana")

bppc2 <- datos %>% ggplot(aes(y = pctUrban, fill = group)) +
  theme_minimal() + 
  theme(axis.title.y = element_text(size = 15),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "top") +
  geom_boxplot() +
  scale_fill_discrete(name = "Grupo", labels=c("África", "OECD", "Otros")) +
  labs(title = "Box-plot Porcentaje de poblacion urbana según grupo", 
       y = "Porcentaje de población urbana")

grid.arrange(bppc1, bppc2, ncol=2)

## Graficos de dispersion --------------------------------------------------

datos %>% select(-c(localities)) %>% 
  ggpairs()

datos2 <- datos %>% select(-c(localities, region, group))

cor(datos2)

pairs(datos2)

###

c1 <- datos %>% ggplot(aes(x = fertility, y = ppgdp)) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 13),
        legend.position = "top") +
  geom_point() +
  labs(title = "Fertilidad vs. Producto Nacional Bruto", 
       x = "Fertilidad",
       y = "PBN")

c2 <- datos %>% ggplot(aes(x = fertility, y = lifeExpF)) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 13),
        legend.position = "top") +
  geom_point() +
  labs(title = "Fertilidad vs. Esperanza de Vida", 
       x = "Fertilidad",
       y = "Esperanza de Vida")

c3 <- datos %>% ggplot(aes(x = fertility, y = pctUrban)) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 13),
        legend.position = "top") +
  geom_point() +
  labs(title = "Fertilidad vs. Porcentaje de poblacion urbana", 
       x = "Fertilidad",
       y = "Porcentaje poblacion urbana")

c4 <- datos %>% ggplot(aes(x = lifeExpF, y = ppgdp)) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 13),
        legend.position = "top") +
  geom_point() +
  labs(title = "Esperanza de Vida vs. Producto Nacional Bruto", 
       x = "Esperanza de Vida",
       y = "PBN")

c5 <- datos %>% ggplot(aes(x = lifeExpF, y = pctUrban)) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 13),
        legend.position = "top") +
  geom_point() +
  labs(title = "Esperanza de Vida vs. Porcentaje de poblacion urbana", 
       y = "Porcentaje poblacion urbana",
       x = "Esperanza de Vida")

c6 <- datos %>% ggplot(aes(x = ppgdp, y = pctUrban)) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 13),
        legend.position = "top") +
  geom_point() +
  labs(title = "PBN vs. Porcentaje de poblacion urbana", 
       x = "PBN",
       y = "Porcentaje poblacion urbana")

grid.arrange(c3, c2, c1, c5, c4, c6, ncol=2)


## Histogramas -------------------------------------------------------------


### Fertilidad --------------------------------------------------------------

datos$group <- as.factor(datos$group)
datos$group <- relevel(datos$group, "other")

h1 <- datos %>% ggplot(aes(x = fertility)) +
  geom_histogram(color='#e9ecef',
                 position = "identity", bins = 10, fill = "royalblue4") +
  theme_minimal() +
  labs(title = "Histograma variable Fertilidad", 
       x = "Fertilidad (tasa de natalidad)",
       y = "Frecuencia")


### Producto nacional bruto -------------------------------------------------

h2 <- datos %>% ggplot(aes(x = ppgdp)) +
  geom_histogram(color='#e9ecef',
                 position = "identity", bins = 20, fill = "royalblue4") +
  theme_minimal() +
  labs(title = "Histograma variable Producto nacional bruto", 
       x = "Producto nacional bruto",
       y = "Frecuencia")

### Esperanza de vida -------------------------------------------------------

h3 <- datos %>% ggplot(aes(x = lifeExpF)) +
  geom_histogram(color='#e9ecef',
                 position = "identity", bins = 20, fill = "royalblue4") +
  theme_minimal() +
  labs(title = "Histograma variable Esperanza de Vida", 
       x = "Esperanza de Vida",
       y = "Frecuencia")

### Porcentaje de poblacion urbana ------------------------------------------

h4 <- datos %>% ggplot(aes(x = pctUrban)) +
  geom_histogram(color='#e9ecef',
                 position = "identity", bins = 20, fill = "royalblue4") +
  theme_minimal() +
  labs(title = "Histograma variable Porcentaje de poblacion urbana", 
       x = "Porcentaje de poblacion urbana",
       y = "Frecuencia")
# sugerencia hacer los histogramas en general.

grid.arrange(h1, h2, h3, h4, ncol=2)


# Grafico de Barras -------------------------------------------------------

region <- table(datos$region) %>% as.data.frame()

r <- region %>% ggplot(aes(x = Var1, y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal() + 
  theme(axis.title.y = element_text(size = 15),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 14), 
        title = element_text(size = 20),
        axis.ticks.x=element_blank(),
        legend.position = "top") +
  scale_x_discrete(labels=c("África", "Asia", "Caribe", "Europa", 
                            "América\nLatina", "Amética\ndel Norte", 
                            "Atlántico\nNorte", "Oceanía")) + 
  scale_fill_discrete(name = "Región", labels=c()) +
  labs(title = "Número de países por región", 
       y = "Frecuencia")

grupo <- table(datos$group) %>% as.data.frame()

g <- grupo %>% ggplot(aes(x = Var1, y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal() + 
  theme(axis.title.y = element_text(size = 15),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 15), 
        title = element_text(size = 20),
        axis.ticks.x=element_blank(),
        legend.position = "top") +
  scale_x_discrete(labels=c("África", "OECD", "Otros")) + 
  scale_fill_discrete(name = "Región", labels=c()) +
  labs(title = "Número de países por grupo", 
       y = "Frecuencia")

region$Freq
grid.arrange(g, r, ncol=2)


##### Parte b) ####

data = import("UNData.csv")

# Matriz de respuestas
Y = cbind(data$fertility, data$lifeExpF)
n = nrow(Y)

### UTILIZANDO TODOS LOS PREDICTORES ###
# Matriz de información
#X = cbind(data$region, data$group, data$ppgdp, data$pctUrban)
aux1 = data %>% mutate(Asi = ifelse(region == "Asia",1,0),
                      Carib = ifelse(region == "Caribbean",1,0),
                      Eur = ifelse(region == "Europe",1,0),
                      Lat = ifelse(region == "Latin Amer",1, 0),
                      Amer = ifelse(region == "North America",1,0),
                      Atl = ifelse(region == "NorthAtlantic",1,0),
                      Ocea = ifelse(region == "Oceania",1,0),
                      group_oecd = ifelse(group == "oecd",1,0),
                      group_afr = ifelse(group == "africa",1,0)) %>% 
  select(-localities, -region, -group, -fertility, -lifeExpF)

X = as.matrix(aux1)

# Se crea la función qqnorm multivariado
qqnorm_multivariado = function(Y, X, title){
  n = nrow(Y)
  # Estimador Beta
  B = solve(t(X)%*%X) %*% t(X) %*% Y
  # Estimador Sigma
  Sigma.Y = t(Y - X %*% B) %*% (Y - X %*% B) / n 
  
  delta = c()
  for(i in 1:dim(Y)[1]){
    delta[i] = t(Y[i,] - t(B) %*% X[i,]) %*% 
      solve(Sigma.Y) %*% (Y[i,] - t(B) %*% X[i,]) 
  }
  p = 2
  z = ( (delta/p)^(1/3) - (1 - 2/(9*p)) ) / sqrt(2/(9*p)) 
  qqnorm(z, main = title, xlab = "Cuantiles teóricos", ylab = "Cuantiles empíricos")
  abline(0,1)
  
  shapiro.test(z)
  
}

# Verificamos supuesto de normalidad con todas los predictores
qqnorm_multivariado(Y, X, "QQ-plot modelo completo")
# Se rechaza normalidad

# Eliminamos la variable group
aux2 = aux1 %>% select(-group_oecd, -group_afr)
X = as.matrix(aux2)
qqnorm_multivariado(Y, X, "QQ-norm modelo sin group")
# Se rechaza normalidad

# Eliminamos la variable region
aux3 = aux2 %>% select(ppgdp, pctUrban)
X = as.matrix(aux3)
qqnorm_multivariado(Y, X, "QQ-norm modelo con ppgdp y pctUrban")
# No se rechaza la normalidad

# Consideramos la variable group
aux4 = aux1 %>% select(ppgdp, pctUrban, group_oecd, group_afr)
X = as.matrix(aux4)
qqnorm_multivariado(Y, X, "QQ-norm modelo sin region")
# Se rechaza normalidad

par(mfrow=c(1,2))
qqnorm_multivariado(Y, as.matrix(aux2), "QQ-plot modelo sin group")
qqnorm_multivariado(Y, as.matrix(aux4), "QQ-plot modelo sin region")
par(mfrow=c(1,1))

# Modelo propuesto (fertility, lifeExp) ~ (ppgdp, pctUrban)
X = as.matrix(aux3)


(B = solve(t(X)%*%X) %*% t(X) %*% Y) # EMV de B
(Sigma.Y = t(Y - X %*% B) %*% (Y - X %*% B) / n) #EMV de Sigma


# Revisamos los VIFs asociados para descartar colinealidad
R = cor(X)
diag(solve(R)) # VIFs

# Número de condición
lambda.min = min(eigen(R)$values)
lambda.max = max(eigen(R)$values)
sum(eigen(R)$values) # la suma debe ser igual a p : cantidad de predictores
(k = sqrt(lambda.max / lambda.min)) # n° de condicionamiento


r = qr(X)$rank # rango de X

# Estudiamos la significancia de los predictores 

### pctUrban ###
X1 = X[,1]
B1 = solve(t(X1)%*%X1) %*% t(X1) %*% Y
Sigma1.Y =  t(Y - X1 %*% B1) %*% (Y - X1 %*% B1) / n 
trv = n * log(det(Sigma1.Y) / det(Sigma.Y))
trv > qchisq(0.95, r)

### ppgdp ###
X2 = X[,2]
B2 = solve(t(X2)%*%X2) %*% t(X2) %*% Y
Sigma2.Y =  t(Y - X2 %*% B2) %*% (Y - X2 %*% B2) / n 
trv = n * log(det(Sigma2.Y) / det(Sigma.Y))
trv > qchisq(0.95, r)


 
### test por coeficiente
(S = t(Y - X %*% B) %*% (Y - X %*% B) / (n - 2) )
(C = solve( t(X) %*% X) )
B

(t11 <- B[1,1]/ (sqrt(S[1,1] * C[1,1])) )
(t12 <- B[1,2]/ (sqrt(S[2,2] * C[1,1])) )
(t21 <- B[2,1]/ (sqrt(S[1,1] * C[2,2])) )
(t22 <- B[2,2]/ (sqrt(S[2,2] * C[2,2])) )

# test
abs(t11) > qt(0.975, n - 2)
abs(t12) > qt(0.975, n - 2)
abs(t21) > qt(0.975, n - 2)
abs(t22) > qt(0.975, n - 2)

# intervalos para los coeficientes
i11 <- c(B[1,1] - qt(0.975,n-2) * sqrt(S[1,1] * C[1,1]),
         B[1,1] + qt(0.975,n-2) * sqrt(S[1,1] * C[1,1]))
i12 <- c(B[1,2] - qt(0.975,n-2) * sqrt(S[2,2] * C[1,1]),
         B[1,2] + qt(0.975,n-2) * sqrt(S[2,2] * C[1,1]))
i21 <- c(B[2,1] - qt(0.975,n-2) * sqrt(S[1,1] * C[2,2]),
         B[2,1] + qt(0.975,n-2) * sqrt(S[1,1] * C[2,2]))
i22 <- c(B[2,2] - qt(0.975,n-2) * sqrt(S[2,2] * C[2,2]),
         B[2,2] + qt(0.975,n-2) * sqrt(S[2,2] * C[2,2]))

i11;i12;i21;i22

