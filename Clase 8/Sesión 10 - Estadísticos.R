# EJEMPLOS DE OBTENCIÓN DE ALGUNOS ESTADÍSTICOS (univariados y multivariados)

library(dplyr)

# Cargamos un data.frame que se encuentra disponible en R

data(iris)
class(iris)

# Obtenemos algunos indicadores de localización para todo el dataset 

summary(iris)

# Vamos a obtener los nombres de todos los atributos presentes en el dataset, 
# es decir, el nombre de las columnas 

names(iris)

# ESTADÍSTICA UNIVARIADA

# Vamos a trabajar solo con la variable llamada Sepal.Width

iris$Sepal.Width


# Medidas de Tendencia Central

# La media
mean(iris$Sepal.Width)

# La mediana

median(iris$Sepal.Width)

# La moda

table(iris$Sepal.Width) %>% 
  which.max() %>% 
  names()

# Media Truncada
# el parametro trim indica el porcentaje de datos que será omitido entre 
# los valores más bajos y los valores más altos

mean(iris$Sepal.Width, trim = 0.3)

# Medidas de Dispersión

# Tasa de Variación

freq <- iris$Sepal.Width %>%
  table() %>% 
  sort(decreasing = T)

freq_modal <- freq[1] / sum(freq)
TV <- 1 - freq_modal

# Varianza

var(iris$Sepal.Width)

# Desviación Estándar

sd(iris$Sepal.Width)

# Descición Media

library(DescTools)
MeanAD(iris$Sepal.Width)

# MEDA
mad(iris$Sepal.Width,constant = 1)

# Coeficiente de Variación

sd(iris$Sepal.Width)/mean(iris$Sepal.Width)


# Quartiles

quantile(iris$Sepal.Width,probs = 0.25)
quantile(iris$Sepal.Width,probs = 0.5)
quantile(iris$Sepal.Width,probs = 0.75)

quantile(iris$Sepal.Width)

IQR(iris$Sepal.Width)

boxplot(iris$Sepal.Width)

# k-esimo Momento Muestral

x <- iris$Sepal.Width
k <- 2

mean(x^2)

# k-esimo Momento Muestral centrado

x <- iris$Sepal.Width
k <- 2

m1 <- mean(x)
mean((x - m1)^k)

# Indice de Simetría

Q <- quantile(iris$Sepal.Width, probs = c(0.25,0.5,0.75))

IS <- (Q[1]+Q[3]-2*Q[2])/(Q[3]-Q[1])              


# Coeficiente de Fisher

x <- iris$Sepal.Width
m1 <- mean(x)
m3c <- mean((x - m1)^3)

gamma1 <- m3c / (sd(x)^3)

hist(x)

# Curtosis

x <- iris$Sepal.Width
m1 <- mean(x)
m4c <- mean((x - m1)^4)

gamma2 <- (m4c / (sd(x)^4)) - 3

hist(x)


# Distribuciones de Probabilidad

?distributions


# Distribución Gamma

x <- seq(0,10,.1)
y <- dgamma(x,shape = 2,scale = 1)
plot(x,y, type='l')

# Distribución Beta

x <- seq(0,1,.01)
y <- dbeta(x,shape1 = 2, shape2 = 3)
plot(x,y, type='l')

# ESTADÍSTICA MULTIVARIADA

# Vamos a trabajar con las 4 variables númericas:
# "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  

names(iris)


# Vector de Medias
colMeans(select(iris,-Species))


# Matriz de Varianza y Covarianzas

var(select(iris,-Species))


# Matriz de Correlación

cor(select(iris,-Species))


# Grafico de Dispersión de a pares

plot(select(iris,-Species))


# Grafico de Caja

boxplot(select(iris,-Species))


# COMPONENTES PRINCIPALES

pca <- prcomp(select(iris,-Species), scale = TRUE)
names(pca)

## Vector de Medias de los datos Originales

pca$center

## Vector con las Desviaciones Estandar de los datos Originales

pca$scale

## Los loadings

pca$rotation


## Los datos transformados al nuevo espacio

pca$x
 
## Proporción de la Varianza explicada por cada Componente Principal
pca_var <- pca$sdev^2
prop_var <- pca_var/sum(pca_var)

plot(cumsum(prop_var), type='b')

