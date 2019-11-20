### G L O B A L###

###INSTALAR PAQUETES###
library(foreign)
library(ggplot2)
library(psych)

###IMPORTACION DE DATOS###
BASE<-read.delim("clipboard")
BASE

###MOSTRAR LAS VARIABLES###
names(BASE)

###MATRIZ DE DATOS###
(BASE)

###GRAFICA DE DISPERSION CON ETIQUETA###

###FRECUENCIA$ACUSTICNESS###
plot(frecc ~ acousticness, data = BASE, xlab = "Ac?stica", ylab = "Frecuencia", main="Diagrama de Dispersi?n")
MLatin <- lm(frecc ~ acousticness, data = BASE)
abline(MLatin, col = "red")

###FRECUENCIA$DANCEABILITY###
plot(danceability ~ frecc, data = BASE, xlab = "Bailable", ylab = "Frecuencia", main="Diagrama de Dispersi?n")
MLatin <- lm(danceability ~ frecc , data = BASE)
abline(MLatin, col = "red")

###FRECUENCIA$ENERGY###
plot(energy ~ frecc, data = BASE, xlab = "Energ?a", ylab = "Frecuencia", main="Diagrama de Dispersi?n")
MLatin <- lm(energy ~ frecc , data = BASE)
abline(MLatin, col = "red")

###FRECUENCIA$INSTRUMENTALNESS###
plot(instrumentalness ~ frecc, data = BASE, xlab = "Frecuencia", ylab = "Instrumentalizada", main="Diagrama de Dispersi?n")
MLatin <- lm(instrumentalness  ~ frecc , data = BASE)
abline(MLatin, col = "red")

###FRECUENCIA$LOUDNESS###
plot(loudness ~ frecc, data = BASE, xlab = "Frecuencia", ylab = "Vol?men", main="Diagrama de Dispersi?n")
MLatin <- lm(loudness ~ frecc , data = BASE)
abline(MLatin, col = "red")

###FRECUENCIA$SPEECHINESS###
plot(speechiness ~ frecc, data = BASE, xlab = "Frecuencia", ylab = "Habla", main="Diagrama de Dispersi?n")
MLatin <- lm(speechiness ~ frecc , data = BASE)
abline(MLatin, col = "red")

###FRECUENCIA$TEMPO###
plot(tempo~ frecc, data = BASE, xlab = "Frecuencia", ylab = "Tiempo", main="Diagrama de Dispersi?n")
MLatin <- lm(tempo ~ frecc , data = BASE)
abline(MLatin, col = "red")

###FRECUENCIA$VALENCE###
plot(valence ~ frecc, data = BASE, xlab = "Frecuencia", ylab = "Valencia", main="Diagrama de Dispersi?n")
MLatin <- lm(valence ~ frecc , data = BASE)
abline(MLatin, col = "red")

###energy ~ loudness###
plot(energy ~ loudness, data = BASE, xlab = "Volumen", ylab = "Energ?a", main="Diagrama de Dispersi?n")
MLatin <- lm(energy ~ loudness, data = BASE)
abline(MLatin, col = "red")

###energy ~ valence###
plot(energy ~ valence , data = BASE, xlab = "Valencia", ylab = "Energ?a", main="Diagrama de Dispersi?n")
MLatin <- lm(energy ~ valence, data = BASE)
abline(MLatin, col = "red")

###instrumentalness ~ loudness###
plot(instrumentalness ~ loudness, data = BASE, xlab = "Volumen", ylab = "Instrumentalidad", main="Diagrama de Dispersi?n")
MLatin <- lm(instrumentalness ~ loudness, data = BASE)
abline(MLatin, col = "red")

###instrumentalness ~ speechiness###
plot(instrumentalness ~ speechiness, data = BASE, xlab = "Habla", ylab = "Instrumentalidad", main="Diagrama de Dispersi?n")
MLatin <- lm(instrumentalness ~ speechiness, data = BASE)
abline(MLatin, col = "red")

###loudness ~ valence###
plot(loudness ~ valence, data = BASE, xlab = "Valencia", ylab = "Volumen", main="Diagrama de Dispersi?n")
MLatin <- lm(loudness ~ valence, data = BASE)
abline(MLatin, col = "red")

### N A C I O N A L###
###INSTALAR PAQUETES###
library(foreign)
library(ggplot2)
library(psych)


###IMPORTACION DE DATOS###
mexico<-read.delim("clipboard")
mexico

###MOSTRAR LAS VARIABLES###
names(mexico)

###MATRIZ DE DATOS###
(mexico)

###acousticness ~ frecc###
plot(acousticness ~ frecc, data = mexico, xlab = "Frecuencia", ylab = "Ac?stica", main="Diagrama de Dispersi?n")
MLatin <- lm(acousticness ~ frecc, data = mexico)
abline(MLatin, col = "red")

###valence ~ frecc###
plot(valence ~ frecc, data = mexico, xlab = "Frecuencia", ylab = "Valencia", main="Diagrama de Dispersi?n")
MLatin <- lm(valence~ frecc, data = mexico)
abline(MLatin, col = "red")

###streams ~ frecc###
plot(streams ~ frecc, data = mexico, xlab = "Frecuencia", ylab = "Streams", main="Diagrama de Dispersi?n")
MLatin <- lm(streams ~ frecc, data = mexico)
abline(MLatin, col = "red")

###energy ~ loudness###
plot(energy ~ loudness, data = mexico, xlab = "Volumen", ylab = "Energ?a", main="Diagrama de Dispersi?n")
MLatin <- lm(energy ~ loudness, data = mexico)
abline(MLatin, col = "red")

###instrumentalness ~ loudness###
plot(instrumentalness ~ loudness, data = mexico, xlab = "Volumen", ylab = "Instrumentalidad", main="Diagrama de Dispersi?n")
MLatin <- lm(instrumentalness ~ loudness, data = mexico)
abline(MLatin, col = "red")

###loudness ~ valence###
plot(loudness ~ valence, data = mexico, xlab = "Valencia", ylab = "Volumen", main="Diagrama de Dispersi?n")
MLatin <- lm(loudness ~ valence, data = mexico)
abline(MLatin, col = "red")


----------------------------------------------------------------------------------------
plot(streams ~ frecc, data = mexico, xlab = "Frecuencia", ylab = "Streams", main="Diagrama de Dispersi?n")
MLatin <- lm(streams ~ frecc, data = mexico)
abline(MLatin, col = "red")

##MATRIZDECORRELACION##
plot(BASE)

###llamar base###
mexico<-read.delim("clipboard")
mexico


### ANALISIS BIVARIADO ###
library(psych) 
pairs.panels(mexico, scale=F, pch=21,main="Matriz de Dispersi?n, Histograma y Correlaci?n")
pairs.panels(b1, scale=F, pch=21,main="Gr?fico 1.2: Matriz de Dispersi?n, Histograma y Correlaci?n")

---------------------------------------------------------------------------------
#CON MATRIZ DE COVARIANZA
(acp.cov <- prcomp(acp)) 

#CORRELACIONES ENTRE VARIABLES Y COMPONENETES 
diag(1/sqrt(diag(cov(acp)))) %% acp.cov$rotation %% diag(acp.cov$sdev)
------------------------------------------------------------------------
#MATRIZ DE CORRELACI?N
acp <- prcomp(acp, scale = TRUE)
acp
summary(acp)

#AUTOVALORES
acp$sdev^2

#Correlaciones entre Variables y Componentes
(corvar <- acp$rotation %*% diag(acp$sdev))
---------------------------------------------
#NUEVAS COORDENADAS
acp$x
--------------------------------
#GR?FICO DE PORCENTAJE DE VARIANZA TOTAL
barplot(summary(acp)$importance[2,])
#M?S DIRECTO
plot(acp)
#GR?FICO DE SEDIMENTACI?N
plot(acp, type = "lines", main = 'Gr?fico de sedimentaci?n') 
------------------------------------
#Correlaci?n entre Variables y CP1 y CP2
plot(-1:1, -1:1, type='n', asp=1, xlab='CP1', ylab='CP2')

abline(h=0, v=0, lty=2, col=8)

## Dibuja un c?rculo de centro (0,0) y radio 1
symbols(0, 0, 1, inches=F, add=T)
symbols(0, 0, sqrt(.5), inches=F, add=T)

## Dibuja los vectores y coloca los nombres
arrows(0, 0, corvar[,1], corvar[,2], length=.1)
text(corvar[,1], corvar[,2], colnames(b), pos=4, offset=.6, col=2, font=2)
------------------------------------------------------------------
#GR?FICPS DE INDIVIDUOS
plot(acp$x, pch = 19, , main= 'Gr?fico de individuos')
abline(h = 0, v = 0, lty = 2, col = 8)
#pch=19  puntos
#h los valores de y para l?neas horizontales
#v valores ?para  x verticales
#lty par?metro gr?ficolineas discontinuas del plano 
----------------------------------------------------
#BIPLOT
biplot(acp)

