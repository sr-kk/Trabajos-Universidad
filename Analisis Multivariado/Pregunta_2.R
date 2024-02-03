#librerias utilizadas
library(dplyr)
library(skimr)
library(GGally)
library(corrplot)
library(MVN) 


load("Casen2017.RData")



# Pre-procesamiento -------------------------------------------------------

aux = casen2017 %>% 
  filter(provincia == 131) %>% 
  select(y1, v17, v19, edad, yaut, ytot, ytoth, ypch, comuna)

summary(aux$v17)
summary(aux$v19)
summary(aux$y1)
summary(aux$yaut)
summary(aux$ytot)
summary(aux$ytoth)
summary(aux$ypch)


y1 = aux %>% 
  select(y1, comuna) %>% 
  na.omit %>% filter(y1 != 0, y1 != 99) %>% 
  group_by(comuna) %>% summarise(prom = mean(log(y1)))
v17 = aux %>% 
  select(v17, comuna) %>% 
  na.omit %>% filter(v17 != 0, v17 != 99) %>% 
  group_by(comuna) %>% summarise(prom = mean(log(v17)))
v19 = aux %>% 
  select(v19, comuna) %>% 
  na.omit %>% filter(v19 != 0, v19 != 99) %>% 
  group_by(comuna) %>% summarise(prom = mean(log(v19)))
edad = aux %>% 
  select(edad, comuna) %>% 
  na.omit %>% 
  group_by(comuna) %>% summarise(prom = mean(edad))
yaut = aux %>% 
  select(yaut, comuna) %>% 
  na.omit %>% 
  group_by(comuna) %>% summarise(prom = mean(log(yaut)))
ytot = aux %>% 
  select(ytot, comuna) %>% 
  na.omit %>% 
  group_by(comuna) %>% summarise(prom = mean(log(ytot)))
ytoth = aux %>% 
  select(ytoth, comuna) %>% 
  na.omit %>% filter(ytoth != 0) %>% 
  group_by(comuna) %>% summarise(prom = mean(log(ytoth)))
ypch = aux %>% 
  select(ypch, comuna) %>% 
  na.omit %>% filter(ypch != 0) %>% 
  group_by(comuna) %>% summarise(prom = mean(log(ypch)))


X = cbind(y1$prom, v17$prom, v19$prom, edad$prom, 
          yaut$prom, ytot$prom, ytoth$prom, ypch$prom)
colnames(X) = c("y1", "v17","v19","edad","yaut","ytot","ytoth","ypch")
comunas = c("Santiago", "Cerrillos", "Cerro Navia", "Conchalí", "El bosque",
            "Estación Central", "Huechuraba", "Independencia", "La Cisterna",
            "La Florida", "La Granja", "La Pintana", "La Reina", "Las Condes",
            "Lo Barnechea", "Lo Espejo", "Lo Prado", "Macul", "Maipú", "Ñuñoa",
            "Pedro Aguirre Cerda", "Peñalolén", "Providencia", "Pudahuel",
            "Quilicura", "Quinta Normal", "Recoleta", "Renca", "San Joaquín",
            "San Miguel", "San Ramón", "Vitacura")
rownames(X) = comunas


#### Supuesto de normalidad ####

######## Código utilizado en ayudantía 5 ###############-
dist.mahalanobis = function(X, mu, S){
  
  p = ncol(X); N = nrow(X)
  sigma.inv = solve(S)
  
  ## Mahalanobis
  dist.mah = c()
  for( i in 1:N){
    diff = as.matrix(X[i,] - mu)
    dist.mah[i] = t(diff)%*%sigma.inv%*%(diff)
    
  }
  
  ## z_i normal estandar
  a = (dist.mah/p)^(1/3); c = 2/(9*p)
  dist.norm = (a - (1 - c))/sqrt(c)
  return(data.frame(delta_i = dist.mah, z_i = dist.norm))
  
}

p = ncol(X); N = nrow(X)
distancias = dist.mahalanobis(X,
                              colMeans(X),
                              cov(X))

qq1 = ggplot(distancias) + aes(sample = delta_i) + 
  geom_qq_line(size = 1,color = "darkred", distribution = stats::qchisq, dparams = list(df = p)) + 
  geom_qq(size = 1.5, distribution = stats::qchisq, dparams = list(df = p)) + 
  labs(x = "Cuantiles teóricos dist. ChiCuadrado", y = "Cuantiles muestrales", 
       title ="Gráfico \"QQ\" Delta_i")

qq2 = ggplot(distancias) + aes(sample = z_i) + geom_qq_line(size = 1, color = "darkred") + geom_qq(size = 1.5) + 
  labs(x = "Cuantiles teóricos dist. normal", y = "Cuantiles muestrales", 
       title ="Gráfico \"QQ\" Z_i")

gridExtra::grid.arrange(
  qq1,
  qq2,
  nrow=1)

########################################-

mvn(data = X, mvnTest = "mardia")$multivariateNormality

# Vemos que el test de mardia nos indica que la normalidad no se rechaza, por lo
# tanto, en base al gráfico y al test podemos asumir normalidad 



# Análisis de componentes principales -------------------------------------


R = cor(X)

# Análisis de correlación
corrplot(R, method = "number", type = "upper")

L = eigen(R)$values # valores propios
G = eigen(R)$vectors # vectores propios

L
round(G,3)

# Restricciones : 
# 1. Vectores propios de largo = 1
# 2. Vectores propios ortogonales

for(i in 1:nrow(G)){
  cat("Largo vector propio", i, "=", sum(G[,i]^2), "\n")
}

round(t(G) %*% G, 10) #ortogonales

# Además, como estamos usando la matriz R, su traza debe ser igual a la 
# cantidad de predictores
sum(diag(R))
sum(L)

##### Analisis de componentes principales #####
prop.var = L / sum(L)
cum.prop = cumsum(prop.var)

# tabla componentes principales
tabla1 = rbind(L, prop.var, cum.prop)
colnames(tabla1) = seq(1,8)
rownames(tabla1) = c("varianza", "Prop. varianza", "Prop. acumulada")
round(tabla1,4)



#### Estandarizamos X  antes de transformar
#### usando las componentes principales

n = dim(X)[1]
medias = apply(X, MARGIN = 2,FUN = mean)
Sxx = apply(X,MARGIN = 2,FUN = var)*(n - 1)

Xs = X # Matriz estandarizada
for(j in 1:dim(X)[2]){
  
  Xs[,j] = (X[,j] - medias[j])/sqrt(Sxx[j])
  
}

# Obtenemos las CP
Z = Xs %*% G
colnames(Z) = c("CP1", "CP2", "CP3", "CP4", "CP5", "CP6", "CP7", "CP8")

round(t(Z) %*% Z, 10) # columnas ortogonales

# Además esperamos que la correlación sea cero
corrplot(cor(Z), method = "number", type = "upper")


# Se plantea la hipótesis
#
# H_0 : (l3+l4+l5+l6+l7+l8) / tr(R) > 0.05 

f_l = sum(L[3:8]) / sum(L)
n = dim(X)[1] 
var_fl = 2 * (0.05/sum(L))^2 * sum(L[1:2]^2) + 2 * (0.95/sum(L))^2 * sum(L[3:8]^2)

sqrt(n-1) * (f_l - 0.05) < -1.65 * sqrt(var_fl)
#No se rechaza H_0

# Intervalo de confianza para c_0
l_i = f_l - 1.65*sqrt(var_fl/(n-1))
l_s = f_l + 1.65*sqrt(var_fl/(n-1))
l_i;l_s



sector = c("Poniente", "Poniente", "Poniente", "Poniente", "Poniente",
           "Poniente", "Poniente", "Poniente", "Poniente", "Oriente", "Poniente",
           "Poniente", "Oriente", "Oriente", "Oriente", "Poniente", "Poniente", 
           "Oriente", "Poniente", "Oriente", "Poniente", "Oriente", "Oriente", 
           "Poniente", "Poniente", "Poniente", "Poniente", "Poniente", "Poniente", 
           "Poniente", "Poniente", "Oriente")

# Ahora consideramos las primeras 2 CP
Z2 = Z[,1:2]
Z2 = as.data.frame(Z2) %>% mutate(comunas = comunas, sector = sector)

qq3 = ggplot(Z2, aes(CP1, CP2)) +
  geom_point(aes(colour = factor(sector))) +
  xlim(-2,2)+ylim(-0.4,0.4)+
  ggtitle("Gráfico de dispersión entre las primeras 2 CP") +
  xlab("CP 1") + ylab("CP 2") 

qq4 = ggplot(Z2, aes(CP1, CP2, label = factor(comunas))) +
  xlim(-2,2)+ylim(-0.4,0.4)+
  geom_text(hjust=-0.5, vjust=1, aes(colour = factor(sector))) +
  ggtitle("Gráfico de dispersión entre las primeras 2 CP") +
  xlab("CP 1") + ylab("CP 2") 

gridExtra::grid.arrange(
  qq3,
  qq4,
  nrow=2)

# Correlaciones entre las variables y las 2 primeras CP
cor.ZX = matrix(rep(0,16), ncol=2)

for(j in 1:2){
  for(i in 1:8){
    cor.ZX[i,j] = G[i,j] * sqrt(L[j])
  } 
}
names = c("y1", "v17","v19","edad","yaut","ytot","ytoth","ypch")

cor.ZX = as.data.frame(cor.ZX)
cor.ZX = cor.ZX %>% mutate(sum_V1.V2 = V1^2 + V2^2, names = names) # tabla de correlaciones
cor.ZX


ggplot(cor.ZX, aes(V1, V2, label = names)) +
  xlim(-1, 1) +
  geom_function(fun =~ sqrt(1-.x^2)) +
  geom_function(fun =~ -sqrt(1-.x^2)) +
  geom_point(colour="blue") +
  geom_text(hjust=-0.5, vjust=1) +
  ggtitle("Gráfico de correlación entre las variables y las 2 primeras CP") +
  xlab("CP 1") + ylab("CP 2")
