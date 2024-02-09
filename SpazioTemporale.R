getwd()
setwd("C:/Suz/Università/TESI/R programming")

# Importazione del dataset dal pacchetto 'FMCC' scaricabile da github
library(devtools)
#devtools::install_github("guidofioravanti/spde_spatio_temporal_pm10_modelling_italy/FMCC")
library(FMCC)
#data(pm10)
names(pm10)


# Importazione della mappa delle regioni italiane
require(rgdal)
map.spdf <- readOGR(
  dsn= paste0("C:/Suz/Università/Terzo anno/Epidemiologia Ambientale/Esame Epi Amb/Limiti01012022_g/Reg01012022_g") ,
  layer="Reg01012022_g_WGS84",
  verbose=FALSE
)
class(map.spdf)

library(lubridate)
library(dplyr)
library(geoR)

pm10$id.day <- yday(pm10$yymmdd)
class(pm10$x)
#pm10$x<-pm10$x/sd(pm10$x)
#pm10$y<-pm10$y/sd(pm10$y)

# Creo oggetto geodata gennaio
pm10.dayx.gd.gennaio <- as.geodata(pm10[pm10$id.day<32,],
                                   coords.col = 3:4,
                                   data.col=5)
summary(pm10.dayx.gd.gennaio) #13910 osservazioni
Gennaio.df <- as.data.frame(pm10.dayx.gd.gennaio)
Gennaio<-Gennaio.df%>%group_by(x,y)%>%summarise(data=mean(data))

# Creo oggetto geodata febbraio
pm10.dayx.gd.febbraio <- as.geodata(pm10[pm10$id.day<60 & pm10$id.day>31,],
                                    coords.col = 3:4,
                                    data.col=5)
summary(pm10.dayx.gd.febbraio) #12656
Febbraio.df <- as.data.frame(pm10.dayx.gd.febbraio)
Febbraio<-Febbraio.df%>%group_by(x,y)%>%summarise(data=mean(data))


# Creo oggetto geodata marzo
pm10.dayx.gd.marzo <- as.geodata(pm10[pm10$id.day<91 & pm10$id.day>59,],
                                 coords.col = 3:4,
                                 data.col=5)
summary(pm10.dayx.gd.marzo) #14174
Marzo.df <- as.data.frame(pm10.dayx.gd.marzo)
Marzo<-Marzo.df%>%group_by(x,y)%>%summarise(data=mean(data))

# Creo oggetto geodata aprile
pm10.dayx.gd.aprile <- as.geodata(pm10[pm10$id.day<121 & pm10$id.day>90,],
                                  coords.col = 3:4,
                                  data.col=5)
summary(pm10.dayx.gd.aprile)#13780
Aprile.df <- as.data.frame(pm10.dayx.gd.aprile)
Aprile<-Aprile.df%>%group_by(x,y)%>%summarise(data=mean(data))

# Creo oggetto geodata maggio
pm10.dayx.gd.maggio <- as.geodata(pm10[pm10$id.day<152 & pm10$id.day>120,],
                                  coords.col = 3:4,
                                  data.col=5)
summary(pm10.dayx.gd.maggio)#14461
Maggio.df <- as.data.frame(pm10.dayx.gd.maggio)
Maggio<-Maggio.df%>%group_by(x,y)%>%summarise(data=mean(data))

# Creo oggetto geodata giugno
pm10.dayx.gd.giugno <- as.geodata(pm10[pm10$id.day<182 & pm10$id.day>151,],
                                  coords.col = 3:4,
                                  data.col=5)
summary(pm10.dayx.gd.giugno) #13812
Giugno.df <- as.data.frame(pm10.dayx.gd.giugno)
Giugno<-Giugno.df%>%group_by(x,y)%>%summarise(data=mean(data))


# Creo oggetto geodata luglio
pm10.dayx.gd.luglio <- as.geodata(pm10[pm10$id.day<213 & pm10$id.day>181,],
                                  coords.col = 3:4,
                                  data.col=5)
summary(pm10.dayx.gd.luglio)#14031 
Luglio.df <- as.data.frame(pm10.dayx.gd.luglio)
Luglio<-Luglio.df%>%group_by(x,y)%>%summarise(data=mean(data))

# Creo oggetto geodata agosto
pm10.dayx.gd.agosto <- as.geodata(pm10[pm10$id.day<244 & pm10$id.day>212,],
                                  coords.col = 3:4,
                                  data.col=5)
summary(pm10.dayx.gd.agosto)#14393
Agosto.df <- as.data.frame(pm10.dayx.gd.agosto)
Agosto<-Agosto.df%>%group_by(x,y)%>%summarise(data=mean(data))

# Creo oggetto geodata settembre
pm10.dayx.gd.settembre <- as.geodata(pm10[pm10$id.day<274 & pm10$id.day>243,],
                                     coords.col = 3:4,
                                     data.col=5)
summary(pm10.dayx.gd.settembre)#13927
Settembre.df <- as.data.frame(pm10.dayx.gd.settembre)
Settembre<-Settembre.df%>%group_by(x,y)%>%summarise(data=mean(data))

# Creo oggetto geodata ottobre
pm10.dayx.gd.ottobre <- as.geodata(pm10[pm10$id.day<305& pm10$id.day>273,],
                                   coords.col = 3:4,
                                   data.col=5)
summary(pm10.dayx.gd.ottobre) #14295
Ottobre.df <- as.data.frame(pm10.dayx.gd.ottobre)
Ottobre<-Ottobre.df%>%group_by(x,y)%>%summarise(data=mean(data))

# Creo oggetto geodata novembre
pm10.dayx.gd.novembre <- as.geodata(pm10[pm10$id.day<335 & pm10$id.day>304,],
                                    coords.col = 3:4,
                                    data.col=5)
summary(pm10.dayx.gd.novembre) #13986
Novembre.df <- as.data.frame(pm10.dayx.gd.novembre)
Novembre<-Novembre.df%>%group_by(x,y)%>%summarise(data=mean(data))

# Creo oggetto geodata dicembre
pm10.dayx.gd.dicembre <- as.geodata(pm10[pm10$id.day>334,],
                                    coords.col = 3:4,
                                    data.col=5)
summary(pm10.dayx.gd.dicembre)#14554 
Dicembre.df <- as.data.frame(pm10.dayx.gd.dicembre)
Dicembre<-Dicembre.df%>%group_by(x,y)%>%summarise(data=mean(data))


library(dplyr)
library(readr)
joined.1 <- merge(Gennaio, Febbraio, by=c("x","y"), all.x = FALSE, all.y = FALSE)
colnames(joined.1)<-c("x","y","Gennaio","Febbraio")
joined.2 <- merge(joined.1, Marzo, by=c("x","y"), all.x = FALSE, all.y = FALSE)
colnames(joined.2)<-c("x","y","Gennaio","Febbraio","Marzo")
joined.3 <- merge(joined.2, Aprile, by=c("x","y"), all.x = FALSE, all.y = FALSE)
colnames(joined.3)<-c("x","y","Gennaio","Febbraio","Marzo","Aprile")
joined.4 <- merge(joined.3, Maggio, by=c("x","y"), all.x = FALSE, all.y = FALSE)
colnames(joined.4)<-c("x","y","Gennaio","Febbraio","Marzo","Aprile","Maggio")
joined.5 <- merge(joined.4, Giugno, by=c("x","y"), all.x = FALSE, all.y = FALSE)
colnames(joined.5)<-c("x","y","Gennaio","Febbraio","Marzo","Aprile","Maggio","Giugno")
joined.6 <- merge(joined.5, Luglio, by=c("x","y"), all.x = FALSE, all.y = FALSE)
colnames(joined.6)<-c("x","y","Gennaio","Febbraio","Marzo","Aprile","Maggio","Giugno",
                      "Luglio")
joined.7 <- merge(joined.6, Agosto, by=c("x","y"), all.x = FALSE, all.y = FALSE)
colnames(joined.7)<-c("x","y","Gennaio","Febbraio","Marzo","Aprile","Maggio","Giugno",
                      "Luglio","Agosto")
joined.8 <- merge(joined.7, Settembre, by=c("x","y"), all.x = FALSE, all.y = FALSE)
colnames(joined.8)<-c("x","y","Gennaio","Febbraio","Marzo","Aprile","Maggio","Giugno",
                      "Luglio","Agosto","Settembre")
joined.9 <- merge(joined.8, Ottobre, by=c("x","y"), all.x = FALSE, all.y = FALSE)
colnames(joined.9)<-c("x","y","Gennaio","Febbraio","Marzo","Aprile","Maggio","Giugno",
                      "Luglio","Agosto","Settembre","Ottobre")
joined.10 <- merge(joined.9, Novembre, by=c("x","y"), all.x = FALSE, all.y = FALSE)
colnames(joined.10)<-c("x","y","Gennaio","Febbraio","Marzo","Aprile","Maggio","Giugno",
                      "Luglio","Agosto","Settembre","Ottobre","Novembre")
Annuale <- merge(joined.10, Dicembre, by=c("x","y"), all.x = FALSE, all.y = FALSE)
colnames(Annuale)<-c("x","y","Gennaio","Febbraio","Marzo","Aprile","Maggio","Giugno",
                      "Luglio","Agosto","Settembre","Ottobre","Novembre","Dicembre")
summary(Annuale)

#Matrice 12x470 con i valori di PM10
data<-t(as.matrix(Annuale[,3:14]))
dim(data) #12x470

#Coordinata x
x<-(Annuale[,1])
#Coordinata y
y<-(Annuale[,2])
time<-seq(1,12,1)



#Coordinate riscalate
x.scaled<-x
y.scaled<-y
x.scaled<-x.scaled/sd(x)
y.scaled<-y.scaled/sd(y)

#Annuale.long <- melt(setDT(Annuale), id.vars = c("x","y"), variable.name = "Anno")
#head(Annuale.long)
#class(Annuale.long$x)
#Annuale.long$Anno<- as.numeric(Annuale.long$Anno)
#Annuale.long$Anno<- as.vector(Annuale.long$Anno)
#Annuale.long$x<- as.vector(Annuale.long$x)
#Annuale.long$y<- as.vector(Annuale.long$y)


library (CompRandFld)
library (RandomFields)
library (spam)

# Variogramma empirico
fit <- EVariogram(data,x.scaled,y.scaled,time)
fit$lenbins
fit$variogramt

# Variogramma Spaziale Empirico Marginale
plot(fit$centers, fit$variograms, xlab='u', ylab=expression(gamma(u)),
     ylim=c(0, max(fit$variograms)), xlim=c(0, max(fit$centers)),
     pch=20,main="Sample variogram spaziale",cex.axis=1, cex=2)

# Variogramma Temporale Empirico Marginale
plot(fit$bint, fit$variogramt, xlab='h', ylab=expression(gamma(t)),
     ylim=c(0, max(fit$variogramt)),xlim=c(0,max(fit$bint)),
     pch=20,main="Sample variogram temporale",cex.axis=1, cex=2)
#nel punto 1 ci sono quelli che distano 1, ecc. 
#manca il punto 11, in cui ho quelli che distano 11 (quindi gennaio e dicembre)
 

# Variogrammi spazio-temporali
st.vario <- matrix(fit$variogramst,length(fit$centers),length(fit$bint))
st.vario <- cbind(c(0,fit$variograms), rbind(fit$variogramt,st.vario))

# Variogrammi spazio-temporali 3d
require(scatterplot3d)
#st.grid <- expand.grid(c(0,fit$centers),c(0,fit$bint))
#scatterplot3d(st.grid[,1], st.grid[,2], c(st.vario),
#              highlight.3d=TRUE, xlab="h",ylab="t",
#              zlab=expression(gamma(h,t)), pch=20,
#              main="Space-time variogram",cex.axis=.7,
#              mar=c(2,2,2,2), mgp=c(0,0,0),
#              cex.lab=.7)

# Versione lisciata dei variogrammi spazio-temporali
persp(c(0,fit$centers), c(0,fit$bint), st.vario,
      xlab="h", ylab="u", zlab=expression(gamma(h,u)),
      ltheta=90, shade=0.75, ticktype="detailed", phi=30,
      theta=30,main="Variogramma spazio-temporale",cex.axis=.8,
      cex.lab=.8)

