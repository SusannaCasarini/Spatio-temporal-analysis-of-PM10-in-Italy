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
id.day <- 79 # 20 marzo
#View(pm10)
########################################################################################################
# Creo oggetti geodata mensili
# Creo oggetto geodata gennaio
pm10.dayx.gd.gennaio <- as.geodata(pm10[pm10$id.day<32,],
                                   coords.col = 3:4,
                                   data.col=5,
                                   covar.col = c("q_dem", "d_a1", "dust", "sp", "tp", "t2m"))#ho tolto alcune covariate
# Creo oggetto geodata febbraio
pm10.dayx.gd.febbraio <- as.geodata(pm10[pm10$id.day<60 & pm10$id.day>31,],
                                    coords.col = 3:4,
                                    data.col=5,
                                    covar.col = c("q_dem", "d_a1", "dust", "sp", "tp", "t2m"))#ho tolto alcune covariate
# Creo oggetto geodata marzo
pm10.dayx.gd.marzo <- as.geodata(pm10[pm10$id.day<91 & pm10$id.day>59,],
                                 coords.col = 3:4,
                                 data.col=5,
                                 covar.col = c("q_dem", "d_a1", "dust", "sp", "tp", "t2m"))#ho tolto alcune covariate
# Creo oggetto geodata aprile
pm10.dayx.gd.aprile <- as.geodata(pm10[pm10$id.day<121 & pm10$id.day>90,],
                                  coords.col = 3:4,
                                  data.col=5,
                                  covar.col = c("q_dem", "d_a1", "dust", "sp", "tp", "t2m"))#ho tolto alcune covariate
# Creo oggetto geodata maggio
pm10.dayx.gd.maggio <- as.geodata(pm10[pm10$id.day<152 & pm10$id.day>120,],
                                  coords.col = 3:4,
                                  data.col=5,
                                  covar.col = c("q_dem", "d_a1", "dust", "sp", "tp", "t2m"))#ho tolto alcune covariate
# Creo oggetto geodata giugno
pm10.dayx.gd.giugno <- as.geodata(pm10[pm10$id.day<182 & pm10$id.day>151,],
                                  coords.col = 3:4,
                                  data.col=5,
                                  covar.col = c("q_dem", "d_a1", "dust", "sp", "tp", "t2m"))#ho tolto alcune covariate
# Creo oggetto geodata luglio
pm10.dayx.gd.luglio <- as.geodata(pm10[pm10$id.day<213 & pm10$id.day>181,],
                                  coords.col = 3:4,
                                  data.col=5,
                                  covar.col = c("q_dem", "d_a1", "dust", "sp", "tp", "t2m"))#ho tolto alcune covariate
# Creo oggetto geodata agosto
pm10.dayx.gd.agosto <- as.geodata(pm10[pm10$id.day<244 & pm10$id.day>212,],
                                  coords.col = 3:4,
                                  data.col=5,
                                  covar.col = c("q_dem", "d_a1", "dust", "sp", "tp", "t2m"))#ho tolto alcune covariate
# Creo oggetto geodata settembre
pm10.dayx.gd.settembre <- as.geodata(pm10[pm10$id.day<274 & pm10$id.day>243,],
                                     coords.col = 3:4,
                                     data.col=5,
                                     covar.col = c("q_dem", "d_a1", "dust", "sp", "tp", "t2m"))#ho tolto alcune covariate
# Creo oggetto geodata ottobre
pm10.dayx.gd.ottobre <- as.geodata(pm10[pm10$id.day<305& pm10$id.day>273,],
                                   coords.col = 3:4,
                                   data.col=5,
                                   covar.col = c("q_dem", "d_a1", "dust", "sp", "tp", "t2m"))#ho tolto alcune covariate
# Creo oggetto geodata novembre
pm10.dayx.gd.novembre <- as.geodata(pm10[pm10$id.day<335 & pm10$id.day>304,],
                                    coords.col = 3:4,
                                    data.col=5,
                                    covar.col = c("q_dem", "d_a1", "dust", "sp", "tp", "t2m"))#ho tolto alcune covariate
# Creo oggetto geodata dicembre
pm10.dayx.gd.dicembre <- as.geodata(pm10[pm10$id.day>334,],
                                    coords.col = 3:4,
                                    data.col=5,
                                    covar.col = c("q_dem", "d_a1", "dust", "sp", "tp", "t2m"))#ho tolto alcune covariate
boxplot(pm10.dayx.gd.gennaio$data, pm10.dayx.gd.febbraio$data, pm10.dayx.gd.marzo$data,pm10.dayx.gd.aprile$data,pm10.dayx.gd.maggio$data,pm10.dayx.gd.giugno$data,pm10.dayx.gd.luglio$data,pm10.dayx.gd.agosto$data,pm10.dayx.gd.settembre$data,pm10.dayx.gd.ottobre$data,pm10.dayx.gd.novembre$data,pm10.dayx.gd.dicembre$data,
        names = c("Gennaio", "Febbraio","Marzo","Aprile","Maggio","Giugno","Luglio","Agosto","Settembre","Ottobre","Novembre","Dicembre"),
        ylab="Concentrazione giornaliera di PM10 𝜇g/m3")
#aggiungo la linea che indica il livello di esposizione giornaliera oltre il quale vi sono effetti sulla salute
abline(h = 45,
       lty = 2) 

#########################################################################
# Creo oggetto geodata per il 20 marzo
pm10.dayx.gd <- as.geodata(pm10[pm10$id.day==id.day,],
                           coords.col = 3:4,
                           data.col=5,
                           covar.col = c("q_dem", "d_a1", "sp", "tp", "t2m", "pbl00","pbl12","wspeed","i_surface","aod550"))
pm10.dayx.gd$data # 471 siti osservazionali
summary(pm10.dayx.gd$coords)
summary(pm10.dayx.gd$data)
summary(pm10.dayx.gd$dust)

# Coordinate riscalate per l'analisi del variogramma
# Coordinate originali divise per la deviaizone standard.
pm10.dayx.gd.scaledcoords <- pm10.dayx.gd
pm10.dayx.gd.scaledcoords$coords[,1] <- pm10.dayx.gd.scaledcoords$coords[,1] / sd(pm10.dayx.gd$coords[,1])
pm10.dayx.gd.scaledcoords$coords[,2] <- pm10.dayx.gd.scaledcoords$coords[,2] / sd(pm10.dayx.gd$coords[,2])

summary(pm10.dayx.gd.scaledcoords$coords)
summary(pm10.dayx.gd.scaledcoords$data)

# Mappa di pm10 divisione in quintili
points(pm10.dayx.gd, pt.div="quint")
plot(map.spdf, add=T)

# Ricerca di evidenza di un trend a partire dai dati originali
plot(pm10.dayx.gd, lowess=T)

# Analisi dei residui rispetto a diversi tipi di trend
# trend spaziale ordine 1
plot(pm10.dayx.gd,
     lowess=T,
     trend = ~ x + y)

# trend spaziale ordine 2
plot(pm10.dayx.gd, lowess=T,
     trend = ~ x + y + I(x^2) + I(y^2) + I(x*y))

# trend spaziale ordine 2 + covariate ambientali
plot(pm10.dayx.gd, lowess=T,
     trend = ~ x + y +  I(x^2) + I(y^2) + I(x*y) + d_a1 + t2m + tp + q_dem + sp + pbl00 + pbl12 + wspeed + i_surface + aod550)

# Fit del modello col trend in modo 'manuale' con la funzione lm (coordinate riscalate)
x <- pm10.dayx.gd.scaledcoords$coords[,1]
y <- pm10.dayx.gd.scaledcoords$coords[,2]
d_a1 <- pm10.dayx.gd$covariate[,'d_a1']
t2m <- pm10.dayx.gd$covariate[,'t2m']
q_dem <- pm10.dayx.gd$covariate[,'q_dem']
tp <- pm10.dayx.gd$covariate[,'tp']
sp <- pm10.dayx.gd$covariate[,'sp']
pbl00 <- pm10.dayx.gd$covariate[,'pbl00']
pbl12 <- pm10.dayx.gd$covariate[,'pbl12']
wspeed <- pm10.dayx.gd$covariate[,'wspeed']
i_surface <- pm10.dayx.gd$covariate[,'i_surface']
aod550 <- pm10.dayx.gd$covariate[,'aod550']


lm1 <- lm(pm10.dayx.gd$data ~  x + y + I(x^2) +
            I(y^2) + I(x*y) +
            d_a1 + t2m + tp +
            q_dem + sp +
            pbl00 + pbl12 + wspeed +
            i_surface + aod550)
summary(lm1)

# sample variogram sui dati pm10
vari1 <- variog(pm10.dayx.gd,
                option = "bin")
plot(vari1)

# variogram sui residui da un trend con covariate ambentali
vari1.covariates <- variog(pm10.dayx.gd.scaledcoords,
                           option = "bin",
                           trend = ~ x + y + I(x^2) +
                             I(y^2) + I(x*y) +
                             d_a1 + t2m + tp +
                             q_dem + sp +
                             pbl00 + pbl12 + wspeed +
                             i_surface + aod550)
plot(vari1.covariates)

# Variogramma empirico
vari1.covariates.cloud <- variog(pm10.dayx.gd.scaledcoords,
                                 option = "cloud",
                                 trend = ~ x + y + I(x^2) +
                                   I(y^2) + I(x*y) +
                                   d_a1 + t2m + tp +
                                   q_dem + sp +
                                   pbl00 + pbl12 + wspeed +
                                   i_surface + aod550)

plot(vari1.covariates.cloud$u, vari1.covariates.cloud$v,
     col='gray', type='p')
points(vari1.covariates$u, vari1.covariates$v, col=2, pch=16) # Sample variogram

# Lavoriamo sulla scala dell'asse verticale
plot(vari1.covariates.cloud$u, vari1.covariates.cloud$v,
     ylim=c(0,200),
     col='gray', type='p', xlab="distance", ylab="semivariance")
points(vari1.covariates$u, vari1.covariates$v, col=2, pch=16)

pract.range.circa <- 1
sigma2.ini <- 75
nugget.ini <- 50

# matern k=0.5
phi.ini <- pract.range.circa/3
ml.matern.k0.5 <- likfit(pm10.dayx.gd.scaledcoords,
                         trend = ~ x + y + I(x^2) +
                           I(y^2) + I(x*y) +
                           d_a1 + t2m + tp +
                           q_dem + sp +
                           pbl00 + pbl12 + wspeed +
                           i_surface + aod550,
                         ini.cov.pars = c(sigma2.ini,
                                          phi.ini),
                         cov.model="matern", kappa=0.5,
                         nugget=nugget.ini, fix.nugget = F)
# matern k=1.5
phi.ini <- pract.range.circa/4.75
ml.matern.k1.5 <- likfit(pm10.dayx.gd.scaledcoords,
                         trend = ~ x + y + I(x^2) +
                           I(y^2) + I(x*y) +
                           d_a1 + t2m + tp +
                           q_dem + sp +
                           pbl00 + pbl12 + wspeed +
                           i_surface + aod550,
                         ini.cov.pars = c(sigma2.ini,
                                          phi.ini),
                         cov.model="matern", kappa=1.5,
                         nugget=nugget.ini, fix.nugget = F)

# matern k=2.5
phi.ini <- pract.range.circa/5.92
ml.matern.k2.5 <- likfit(pm10.dayx.gd.scaledcoords,
                         trend = ~ x + y + I(x^2) +
                           I(y^2) + I(x*y) +
                           d_a1 + t2m + tp +
                           q_dem + sp +
                           pbl00 + pbl12 + wspeed +
                           i_surface + aod550,
                         ini.cov.pars = c(sigma2.ini,
                                          phi.ini),
                         cov.model="matern", kappa=2.5,
                         nugget=nugget.ini, fix.nugget = F)

ml.matern.k0.5
ml.matern.k1.5
ml.matern.k2.5

# Mappa delle stime ricalcolate con il modello
points.geodata(coords=pm10.dayx.gd$coords,
               data=fitted.values(ml.matern.k0.5),
               pt.divide = 'quintile')
plot(map.spdf, add=T)

points.geodata(coords=pm10.dayx.gd$coords,
               data=pm10.dayx.gd$data,
               pt.divide = 'quintile')
plot(map.spdf, add=T)

#Grafico dei dati stimati in funzione dei dati osservati
plot(pm10.dayx.gd$data, fitted.values(ml.matern.k0.5), xlab="Dati", ylab="Fitted values")
abline(1,1)
#I dati stimati sembrano molto simili ai dati misurat, soprattutto per valori di bassa concentrazione. 
# Per concentrazioni maggiori i valori stimati tendono ad essere minori dei valori misurati. 

# KRIGING
# Griglia con pred_grid (coordinate riscalate)
range(pm10.dayx.gd.scaledcoords$coords[,1])
range(pm10.dayx.gd.scaledcoords$coords[,2])
# Punti griglia per la previsione: long spanna da 1 a 6 e lat da 15 a 20
pred.locs.scaledcoords <- pred_grid(c(1,6), c(15,20),  
                                    by=0.01)
nlocs <- dim(pred.locs.scaledcoords)[1]
nlocs #251001 locazioni di previsione
plot(pred.locs.scaledcoords)

# SK: simple kriging
#Stimo il mio modello 2nd
pract.range.circa <- 1
sigma2.ini <- 75 
nugget.ini <- 50
phi.ini <- pract.range.circa/3
ml.matern.k0.5.trend2nd <- likfit(pm10.dayx.gd.scaledcoords,
                                  trend = "2nd",
                                  ini.cov.pars = c(sigma2.ini,
                                                   phi.ini),
                                  cov.model="matern", kappa=0.5,
                                  nugget=nugget.ini, fix.nugget = F)
ml.matern.k0.5.trend2nd #osservo che la ML è più bassa, perchè è un modello senza coviariate
summary(ml.matern.k0.5.trend2nd)$parameters.summary

# Questi valori del modello dovranno essere sostituiti al previsore puntuale con la funzione krig.conv 
# Stime e previsore
KC <- krige.control(type="SK",#simple kriging 
                    obj.mod=ml.matern.k0.5.trend2nd, #modello da cui prende i parametri
                    trend.d="2nd", #trend su cui ho fittato i dati
                    trend.l="2nd") # trend con cui voglio fare la previsore
sk <- krige.conv(pm10.dayx.gd.scaledcoords,
                 krige=KC,
                 locations=pred.locs.scaledcoords,# Location in cui voglio la previsione
                 output=output.control(signal=TRUE)) 
names(sk) #predict= previsioni(valore atteso di T|Y), krige.var= errore della previsione (varianza di T|Y)


# Mappa delle previsioni
library(raster)
# Pred.locs.scaledcoords trasformate nelle coordinate orginali
pred.locs <- matrix(NA, nlocs, 2)
pred.locs[,1] <- pred.locs.scaledcoords[,1]*sd(pm10.dayx.gd$coords[,1])
pred.locs[,2] <- pred.locs.scaledcoords[,2]*sd(pm10.dayx.gd$coords[,2])

# Raster dataframe con i valori di previsione
dfr <- rasterFromXYZ(data.frame(x=pred.locs[,1],
                                y=pred.locs[,2],
                                z=sk$predict))
# Selezione dei punti della griglia coincidenti con i punti della mappa dell'italia
dfr2 <- mask(dfr, map.spdf)

plot(dfr2)
points(pm10.dayx.gd$coords, pch=4, col=2, cex=0.5)

# Mappa degli errori (deviazioni standard)
dfr.prederr <- rasterFromXYZ(data.frame( x=pred.locs[,1],
                                         y=pred.locs[,2],
                                         z=sqrt(sk$krige.var)))
dfr2.prederr <- mask(dfr.prederr, map.spdf)
plot(dfr2.prederr)
points(pm10.dayx.gd$coords, pch=4, col=2, cex=0.5)
