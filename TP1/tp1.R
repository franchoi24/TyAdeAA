setwd("C:/Users/fchoi/OneDrive - Centaurus/Escritorio/ITBA/TyAdeAA/TP1")
base = read.table("optdigits.tra",sep=",")
head(base, 1)
dim(base)
library(jpeg) #cargamos la librería 
vector=base[14,] #tomamos la fila 14 por separado
vector #visualizamos que la fila sigue siendo un data.frame con títulos de columnas
#visualice en la columna V65 qué número se va a dibujar 
vector=vector[-65] #sacamos la columna V65 (variable a predecir – valor 5) 
vector=as.numeric(vector) #transformamos el data.frame a vector numérico
vector #visualizamos que la fila ahora es un vector sin títulos de columnas
vector=vector/16
#transformamos los valores de (1:16) a (0:1)
vector
imagen=array(vector,dim=c(8,8))
#creamos la imagen de 8x8
imagen=t(imagen)
#rotamos la imagen
plot(as.raster(imagen))
plot(as.raster(1 - imagen))
#EJ 6
vector=base[814,] #tomamos la fila 14 por separado
vector #visualizamos que la fila sigue siendo un data.frame con títulos de columnas
#visualice en la columna V65 qué número se va a dibujar 
vector=vector[-65] #sacamos la columna V65 (variable a predecir – valor 5) 
vector=as.numeric(vector) #transformamos el data.frame a vector numérico
vector #visualizamos que la fila ahora es un vector sin títulos de columnas
vector=vector/16
#transformamos los valores de (1:16) a (0:1)
vector
imagen=array(vector,dim=c(8,8))
#creamos la imagen de 8x8
imagen=t(imagen)
#rotamos la imagen
plot(as.raster(imagen))

#EJ 7
set.seed(41824814)
for (i in 1:64){
  var=paste("V",i,sep="")
  nuevo=paste("Pixel",i,sep="")
  names(base)[names(base)==var]=nuevo}
names(base)[names(base)=="V65"]="Numero"
base$Numero=as.factor(base$Numero)
head(base,1) 

#EJ 8
plot(base$Numero,main="Gráfico de Francisco")
plot(base$Numero,main="Gráfico de Francisco",col="salmon")

#EJ 9
summary(base$Numero)
sum(summary(base$Numero))

#PARTE B
#EJ1
set.seed(41824814);partition=createDataPartition(base$Numero,p=0.75,list=F)
entreno = base[partition,]
testeo = base[-partition,]

#EJ2
head(entreno[,c(1:2,63:65)])
summary(entreno[,c(1:2,63:65)])
head(testeo[,c(1:2,63:65)])
summary(testeo[,c(1:2,63:65)])
(dim(entreno))
(dim(testeo))
summary(entreno$Numero)
summary(testeo$Numero)


#PARTE C
#EJ 1
set.seed(41824814);red=nnet(Numero~.,entreno,size=40,maxit=20000,MaxNWts=20000)

#EJ3
red

#EJ 5
library(NeuralNetTools)
plotnet(red)

#EJ 6
pred=predict(red,testeo,type="class")
confusionMatrix(factor(pred),testeo$Numero)

#EJ7
cm <- confusionMatrix(factor(pred),testeo$Numero)
conf_matrix <- cm$table
diagonal <- diag(conf_matrix)
acertados <- sum(diagonal)
registros <- dim(testeo[1])
acc <- acertados / registros
acc

#EJ9
nro <- base[14,]

#EJ 10
nuevo_predict = predict(red, nro, type="class")
nuevo_predict


#PARTE D
#EJ 1
set.seed(123);red=nnet(Numero~.,entreno,size=40,maxit=20000,MaxNWts=20000)
confusionMatrix(factor(pred),testeo$Numero)

