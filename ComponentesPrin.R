#########################  REDUCCIÓN DE DIMENSIÓN

######COMPONENTES PRINCIPALES

#Lectura de los datos
library(readxl)
base <- read_excel("base.xlsx")
head(base)

#Elmina la última columna porque no es numérica
base<-base[,-8]

#Matriz de dispersión
pairs(base,col="blue",main="Gráfica de dispersión",cex.main=0.8)


#Calcular las varianzas de los atributos 
eig<-eigen(cor(base))   #es una lista que tiene valores y vectores
eig$values              #muestra los  valores propios de la matriz de correlaciones
eig$vectors              #muestra los  vectores propios de la matriz de correlaciones



#Hacemos una tabla con
#la columna 1 el número del componente
#la columna 2 la desv estándar
#la columna 3 los eigen valores (que son las varianzas)
#la columna 4 el acumulado de los eigenvalores (suma de las varianzas como proporción)

resumen<-data.frame(componente=(1:length(eig$values)),std=sqrt(eig$values),
                    prop.var=eig$values/sum(eig$values),prop.acum=cumsum(eig$values/sum(eig$values)))

#Se obtienen las componentes principales de forma más directa, con las variables estandarizadas
componentes<-princomp(base,cor=T)

#Gráfica de sedimentación
screeplot(componentes,type="l",main="Gráfica de sedimentación" ,col="purple",lwd=2,cex.main=0.9)


#calculando los escalares de cada componente
componentes$loadings


#calculando la base transformada
componentes$scores

#biplot
biplot(componentes,cex=0.6,scale=0) 

