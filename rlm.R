library(readxl)
datarls1 <- read_excel("poblacion1.xlsx",sheet = 1,col_names = TRUE,na = "")
datarls2 <- read_excel("poblacion2.xlsx",sheet = 1,col_names = TRUE,na = "")
str(datarls1)
str(datarls2)
#podemos notar que las dimensiones de la poblacion1 es de 44 obs y 4 variables, en cambio
# las dimensiones de poblacion2 es de 40 obs y 7 variables 

#procedemos a unir los archivos leidos en un mismo objeto
poblacion <- merge(x = datarls1 ,y = datarls2) 
str(poblacion)
typeof(poblacion)


#Creamos un codigo que identifique la clase de cada variable y genere diagrama de cajas 
#para variables continuas y diagrama de barras para variables discretas

for(i in 1:(ncol(poblacion))){
  if(is.numeric(poblacion[i])==T){
    hist(poblacion[,i])
  }else{
    barplot(table(poblacion[,i]))
  }
}

#Creemos un codigo que calcule automaticamente el minimo,media,maximo, 
#desviacion est?ndar primer cuartil de cada variable numerica y frecuencia en el caso 
#de variables categoricas
for(i in 1:(ncol(poblacion))) {
  if(is.numeric(poblacion[i])==T){
    print(names(poblacion[i]))
    print(summary(poblacion)[4,i])
    print(summary(poblacion)[4,i])
    print(summary(poblacion)[6,i])
    print(sd(poblacion[,i]))
    print(summary(poblacion)[2,i])
    print("*******")
  } else {
    print(names(poblacion[i]))
    print(summary(poblacion)[4,i])
    print(summary(poblacion)[4,i])
    print(summary(poblacion)[6,i])
    print(sd(poblacion[,i]))
    print(summary(poblacion)[1,i])
    print("********")
  }
}
summary(poblacion)

#consideremos la variable categorica "serv.bas.compl" con una confiabilidad del 90%
#¿Puede asumirse que la media de la variable "poblacion" en el grupo "serv.bas.compl:SI"
#es distinta a la media del grupo "serv.bas.compl:NO" ?

#primero veamos si podemos asumir que las varianzas de las variables que se van a tomar 
# son iguales o no.
names(poblacion)
var1<-poblacion[,1]
var10<-poblacion[,10]
boxplot(var1~var2)
# gracias al diagrama de cajas podemos ver que en el grupo de "SI" existe mas variacion
# que en el grupo de "NO". Además comparando las varianzas de los 2 grupos tenemos:
var(var1[var2=="SI"])
var(var1[var2=="NO"])
#claramente vemos que las varianzas son distintas. Ahora procedemos a aceptar o rechazar 
# nuestra prueba de hipotesis de las medias
#procedamos a comprobar la hipotesis si las medias son iguales:
t.test(data_serv1~data_serv2,  mu=0, alternative = "two.sided", conf.level = 0.90,var.eq=F,paired=F)

t.test(var1[var2=="SI"], var1[var2=="NO"],conf.level = 0.90)

#Generemos el modelo de regresión lineal múltiple que mejor se ajuste a nuestros datos
var2<-poblacion[,2]
var3<-poblacion[,3]
var4<-poblacion[,4]
mod1<-lm(var1~var2+var4)
summary(mod1)
plot(mod1)

#interpretacion de R2
summary(mod1)[[8]]
