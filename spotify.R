
install.packages("readr")
library(readr)

global<-read_csv("spotify.csv")
global

summary(global)



### BASE CON VAR. CUANTITATIVAS ###
b<-read.delim("clipboard")
head(b)
summary(b)
b<-(global[2:3])
b

# var con valores [0,1] (6)

a<-(global[8:9])
a
a1<-(global[11])
a1
a2<-(global[14])
a2
a3<-(global[17])
a3
a4<-(global[20])
a4

b1<-cbind(a,a1,a2,a3,a4)
b1

# var con valores [1,300] (2)
bb2<-global[10]
bbb2<-global[18]
b2<-cbind(bb2,bbb2)
head(b2)

# var con valores [>99900] (1)
b3<-global[4]
b3

# var con valores [<0] (1)
b4<-global[15]
b4

# var con valores [casi 0] (1)
b5<-global[12]
b5

#base 50
global[]
b50m<-read.delim("clipboard")
head(b50m)
summary(b50m)

b50g<-read.delim("clipboard")
head(b50g)
summary(b50g)

excel_spoty<-read.excel("")

### ANALISIS UNIVARIADO ###
boxplot(b1, ylab="Y", main="?nalisis univariado")
boxplot(b2, ylab="Y", main="?nalisis univariado")
boxplot(b3, ylab="Y", xlab="Streams", main="?nalisis univariado")
boxplot(b4, ylab="Y", xlab="Loudness", main="?nalisis univariado")

### ANALISIS BIVARIADO ###
library(psych) 
pairs.panels(b, scale=F, pch=21,main="Gr?fico 1.1: Matriz de Dispersi?n, Histograma y Correlaci?n")
pairs.panels(b1, scale=F, pch=21,main="Gr?fico 1.2: Matriz de Dispersi?n, Histograma y Correlaci?n")

### MATRIZ DE CORRELACIONES ###
library(corrplot)
m<-cor(b)
corrplot(m, method="number", main="Matriz de correlaciones")

#cluster k means
library(factoextra)
library(ggplot2)
bscale<-as.data.frame(scale(b50m[-1]))
set.seed(100)
cls<-kmeans(bscale, centers=3)
fviz_cluster(cls, data = bscale, main="Clustering K-means ~ M?xico")

bscale<-as.data.frame(scale(b50g[-1]))
set.seed(100)
cls<-kmeans(bscale, centers=2)
fviz_cluster(cls, data = bscale, main="Clustering K-means ~ Global")

### SERIES TEMP ###
library(astsa)
library(quantmod)
library(fma)
library(tseries)
library(forecast)

MX<-read.table("clipboard", header=T)
MX<-ts(MX)
MX<-as.zoo(MX)
head(MX)
tail(MX)
summary(MX)

GL<-read.table("clipboard", header=T)
GL<-ts(GL)
GL<-as.zoo(GL)
head(GL)
tail(GL)
summary(GL)

plot(MX, main="Variables M?xico", xlab="", col="red")
plot(GL, main="Variables Global", xlab="", col="blue")

#Barplot
GL<-read.delim("clipboard")
MX<-read.delim("clipboard")
names(GL)
barplot(MX$valence, main = "Valence", names.arg = MX$X, cex.names = .7, las=1, col = "pink", horiz=T)
barplot(GL$valence, main = "Valence", names.arg = GL$X, cex.names = .7, las=1, col = "pink", horiz=T)
abline(v=0.6)

#Plot enegy vs valence
GL<-read.delim("clipboard")
MX<-read.delim("clipboard")

name<-read.delim("clipboard")
head(name)

plot(energy~valence, data=GL, main="Diagrama de dispersi?n")
points(energy~valence, data=GL, pch = 20, col="red")
abline(h=0.5)
abline(v=0.5)
with(GL, text(energy~valence, labels = row.names(name), pos =3))

plot(energy~valence, data=MX, main="Diagrama de dispersi?n")
points(energy~valence, data=MX, pch = 20, col="blue")
abline(h=0.5)
abline(v=0.5)
with(MX, text(energy~valence, labels = row.names(name), pos =4))

#Pie Chart
GL<-read.delim("clipboard")
MX<-read.delim("clipboard")
head(MX)
pie(MX$frec, labels = MX$Artista, main="Artistas m?s escuchados de M?xico")
pie(GL$frec, labels = GL$Artista, main="Artistas m?s escuchados del mundo")