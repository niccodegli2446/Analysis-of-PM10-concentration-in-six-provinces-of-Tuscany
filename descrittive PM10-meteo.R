########################### CARICAMENTO DATI #############################
rm(list=ls())
dev.off()
#### Caricamento dati e librerie modelli 
source("/Users/niccolodeglinnocenti/Desktop/Università/TESI/data_load.R")
#### Sostituzione missing 
source("/Users/niccolodeglinnocenti/Desktop/Università/TESI/fill_missing.R")

#### Caricamento dati no missing
#dati=read.table('/Users/niccolodeglinnocenti/Desktop/Università/TESI/PM10.csv',
#                header=T, sep=';', dec='.')
dati$DATE=as.Date(as.character(dati$DATE), format = "%Y-%m-%d")

### Caricamento dati meteo e funzioni utili
meteo=data.frame(cbind(
  rain.AR=meteo.AR$precipitazioni.mm, wind.AR=meteo.AR$vel_med_ms, 
  umi.AR=meteo.AR$umi_med, temp.AR=meteo.AR$temp_med, rangeT.AR=meteo.AR$diff_temp,
  rain.LI=meteo.LI$precipitazioni.mm, wind.LI=meteo.LI$vel_med_ms, 
  umi.LI=meteo.LI$umi_med, temp.LI=meteo.LI$temp_med, rangeT.LI=meteo.LI$diff_temp,
  rain.PO=meteo.PO$precipitazioni.mm, wind.PO=meteo.PO$vel_med_ms, 
  umi.PO=meteo.PO$umi_med, temp.PO=meteo.PO$temp_med, rangeT.PO=meteo.PO$diff_temp,
  rain.FI=meteo.FI$precipitazioni.mm, wind.FI=meteo.FI$vel_med_ms, 
  umi.FI=meteo.FI$umi_med, temp.FI=meteo.FI$temp_med, rangeT.FI=meteo.FI$diff_temp,
  rain.PI=meteo.PI$precipitazioni.mm, wind.PI=meteo.PI$vel_med_ms, 
  umi.PI=meteo.PI$umi_med, temp.PI=meteo.PI$temp_med, rangeT.PI=meteo.PI$diff_temp,
  rain.LU=meteo.LU$precipitazioni.mm, wind.LU=meteo.LU$vel_med_ms, 
  umi.LU=meteo.LU$umi_med, temp.LU=meteo.LU$temp_med, rangeT.LU=meteo.LU$diff_temp))

############################# FUNZIONI #########################################

fasce.invernali=function(){
  abline(v=as.Date("2020-03-20"), col='blue', lty=2)
  abline(v=as.Date("2020-12-21"), col='blue', lty=2)
  abline(v=as.Date("2021-03-20"), col='blue', lty=2)
  abline(v=as.Date("2021-12-21"), col='blue', lty=2)
  abline(v=as.Date("2022-03-20"), col='blue', lty=2)
  abline(v=as.Date("2022-12-21"), col='blue', lty=2)
  abline(v=as.Date("2023-03-20"), col='blue', lty=2)
  abline(v=as.Date("2023-12-21"), col='blue', lty=2)
}

#### correlazione PM10 di ogni città con il suo meteo ####
cor.PM10.meteo=function(dat,Meteo){
  var=c('rain','umi','temp','wind','rangeT')
  citta=c('AR','LI','PO','FI','PI','LU')
  m=matrix(NA,ncol=length(citta),nrow=length(var))
  for(j in 1:length(dat)){
    met=Meteo[paste(var,citta[j],sep='.')]
    x=NULL
    for(i in 1:length(met)){
      x=c(x,cor(dat[,j],met[,i]))
    }
    m[,j]<-x
  }
  dimnames(m)<-list(var,citta)
  m
}

draw.rect=function(vet, PM10, value, max){
  
  # value --> numero di giorni minimi consecutivi da prendere in considerazione
  # PM10 --> valore del PM10 preso in esame: PM10=40 prendo in considerazione almeno
  #          value giorni (es. almeno 5 giorni) in cui il PM10 è stato superiore di 40
  
  r=rle((vet>=PM10)*1)
  ends=cumsum(r$lengths)
  starts=ends - r$lengths + 1
  starts_1=starts[r$values==1]
  ends_1=ends[r$values==1]
  d=data.frame(start=starts_1, end=ends_1)
  Dat.frame=d[which(d$end-d$start>=(value-1)),]
  for(i in 1:NROW(Dat.frame)){
    if(PM10<=9) col=adjustcolor("#66FF00", alpha.f = 0.8)
    if(PM10>9 & PM10<=19) col=adjustcolor("#99FF00", alpha.f = 0.8)
    if(PM10>19 & PM10<=29) col=adjustcolor("#CCFF33", alpha.f = 0.8)
    if(PM10>29 & PM10<=39) col=adjustcolor("#FFFF66", alpha.f = 0.8)
    if(PM10>39 & PM10<50) col=adjustcolor("#FFCC33", alpha.f = 0.8)
    if(PM10>=50) col=adjustcolor("red", alpha.f = 0.8)
    rect(xleft = dati$DATE[Dat.frame[i,1]], ybottom = 0, xright = dati$DATE[Dat.frame[i,2]], 
         ytop = max, col = col, border = NA)
  }
}

draw.line=function(x){
  if(x<=9) abline(h=10, col="#66FF00")
  if(x>9 & x<=19) abline(h=20 , col="#99FF00")
  if(x>19 & x<=29) abline(h=30, col="#CCFF33")
  if(x>29 & x<=39) abline(h=30, col="#FFFF66")
  if(x>39 & x<50) abline(h=40, col='#FFCC33')
  if(x>=50) abline(h=50, col='red')
}

# cumulata su 5 giotni
diff5.cumulata=function(x, lag=4){
  c(rep(0,lag),diff(cumsum(x),lag))
}

#giorni favorevoli all'accumulo
giorni.fav=function(citta){
  var=c('rain','umi','temp','wind')
  nomi.meteo=paste(var,citta,sep='.')
  indR=meteo[,nomi.meteo][,1]<1
  indU=meteo[,nomi.meteo][,2]>=75
  indT=meteo[,nomi.meteo][,3]<15
  indW=meteo[,nomi.meteo][,4]<1
  IND=as.logical(indR*indT*indW*indU)
  IND
}

################################ mappa Toscana ##################################
########### mappa con punti rossi -> posizione stazione monitoraggio PM10
#############           punti blu -> posizione centraline meteo
toscana<-st_read("/Users/niccolodeglinnocenti/Desktop/Università/TESI/DATI/shapefile_df8408ad933eae021b1b07329ceab869/am_prov_multipart.shp")
st_crs(toscana) <- 3003
toscana <- st_transform(toscana, crs = 4326)

toscana$NOME[toscana$NOME=='MODENA']<-''

# coordinate PM10 UTM (EPSG:3003)
coordinate_PM10_old=data.frame(
  nome = c("AR.REPUBBLICA", "LI.CARDUCCI", "PO.FERRUCCI", "FI.MOSSE", 
           "PI.BORGHETTO", "LU.MICHELETTO"),
  N = c(4816110,4823183,4860034,4850406,4840980,4855539),
  E = c(1732681,1607354,1669108,1679502,1613586,1621515)
)
sf_utm <- st_as_sf(coordinate_PM10_old, coords = c("E", "N"), crs = 3003)
coordinate_PM10 <- st_transform(sf_utm, crs = 4326)

# coordinate meteo UTM (EPSG:4326)
coordinate_meteo=data.frame(
  nome=c("AR.Anghiari", "LI.Quercianella", "PO.Prato Università", 
         "FI.Firenze Università", "PI.Metato", "LU.Montecarlo"),
  lon = c(12.097,10.348,11.099,11.251,10.384,10.654),
  lat = c(43.559,43.480,43.886,43.799,43.771,43.843)
)
coordinate_meteo <- st_as_sf(coordinate_meteo, coords = c("lon", "lat"), crs = 4326)

ggplot(toscana) +
  geom_sf(aes(fill = NOME),color = "black", alpha=0.6) +
  geom_sf(data = coordinate_PM10, color = "red", size = 2) +
  geom_sf(data = coordinate_meteo, color = "blue", size = 2) +  
  geom_sf_text(aes(label = NOME), size = 2, color = "black") + 
  guides(fill = "none") + 
  labs(title = "", x="Longitudine", y="Latitudine") 

####### distanza tra centraline meteo e centraline PM10 in km per ogni provincia
citta=c('AR','LI','PO','FI','PI','LU')
punti_PM10=rbind(c(11.87586, 43.4616), c(10.32873, 43.55374), c(11.10441, 43.87388),
                 c(11.23045, 43.78481), c(10.4096, 43.71301), c(10.51128, 43.8428))
punti_meteo=rbind(c(12.097, 43.559), c(10.348, 43.48), c(11.099, 43.886),
                  c(11.251, 43.799), c(10.384, 43.771), c(10.654, 43.843))
distanza_m <- distHaversine(punti_PM10, punti_meteo)/1000
names(distanza_m)<-citta
distanza_m

###### distanza in km tra le varie centraline di PM10 delle varie città
AR=c(11.87586, 43.4616)
LI=c(10.32873, 43.55374)
PO=c(11.10441, 43.87388)
FI=c(11.23045, 43.78481)
PI=c(10.4096, 43.71301)
LU=c(10.51128, 43.8428)

dist=distHaversine(rbind(AR,AR,AR,AR,AR,LI,LI,LI,LI,PO,PO,PO,FI,FI,PI),
                   rbind(LI,PO,FI,PI,LU,PO,FI,PI,LU,FI,PI,LU,PI,LU,LU))/1000

matrice.distanze=matrix(c(0,dist[1:5],0,0,dist[6:9],0,0,0,dist[10:12],0,0,0,0,
                          dist[13:14],0,0,0,0,0,dist[15],rep(0,6)),ncol=6,byrow=T)
dimnames(matrice.distanze)=list(citta,citta)

matrice.distanze

######################## ACF province ########################################
par(mfrow=c(3,2))
Acf(dati[,2],lag.max = 35, main='AR.REPUBBLICA')
Acf(dati[,3],lag.max = 35, main='LI.CARDUCCI')
Acf(dati[,4],lag.max = 35, main='PO.FERRUCCI')
Acf(dati[,5],lag.max = 35, main='FI.MOSSE')
Acf(dati[,6],lag.max = 35, main='PI.BORGHETTO')
Acf(dati[,7],lag.max = 35, main='LU.MICHELETTO')

#ACF joint

datii=dati
names(datii)<-c('date','AR','LI','PO','FI','PI','LU')
acf.joint=Acf(datii[,-1], lag.max = 35, plot=FALSE)
plot(acf.joint, xlab='', ylab='', mar =  c(1.3, 1.4, 2, 0.3), 
     oma =  c(0.8, 1.2, 1, 1), 
     mgp =  c(1, 0.6, 0))
rm(datii)
######################## SFORAMENTI, MEDIE E QUANTILI ####################

#### numero di sforamenti per anno e per provincia ####
s=function(v){sum(v>=50, na.rm = T)}
mats=rbind(apply(dati[-1][dati$DATE<='2020-12-31' & dati$DATE>='2020-01-01',],2,s), #2020-2021
apply(dati[-1][dati$DATE<='2021-12-31' & dati$DATE>='2021-01-01',],2,s), #2021-2022
apply(dati[-1][dati$DATE<='2022-12-31' & dati$DATE>='2022-01-01',],2,s), #2022-2023
apply(dati[-1][dati$DATE<='2023-12-31' & dati$DATE>='2023-01-01',],2,s)) #2023-2024
row.names(mats)<-c("2020","2021","2022","2023")
mats
#in 4 anni
apply(dati[-1],2,s)

#### media per anno e per provincia ####
matm=rbind(apply(dati[-1][dati$DATE<='2020-12-31' & dati$DATE>='2020-01-01',],2,mean), #2020-2021
apply(dati[-1][dati$DATE<='2021-12-31' & dati$DATE>='2021-01-01',],2,mean), #2021-2022
apply(dati[-1][dati$DATE<='2022-12-31' & dati$DATE>='2022-01-01',],2,mean), #2022-2023
apply(dati[-1][dati$DATE<='2023-12-31' & dati$DATE>='2023-01-01',],2,mean)) #2023-2024
row.names(matm)<-c("2020","2021","2022","2023")
matm
#in 4 anni
apply(dati[-1],2,mean)

#### 330/365 quantile per anno e per provincia ####
matq=rbind(apply(dati[-1][dati$DATE<='2020-12-31' & dati$DATE>='2020-01-01',],2,quantile,330/365), #2020-2021
apply(dati[-1][dati$DATE<='2021-12-31' & dati$DATE>='2021-01-01',],2,quantile,330/365), #2021-2022
apply(dati[-1][dati$DATE<='2022-12-31' & dati$DATE>='2022-01-01',],2,quantile,330/365), #2022-2023
apply(dati[-1][dati$DATE<='2023-12-31' & dati$DATE>='2023-01-01',],2,quantile,330/365)) #2023-2024
row.names(matq)<-c("2020","2021","2022","2023")
matq
#in 4 anni
apply(dati[-1],2,quantile,330/365)

######################## RAPPRESENTAZIONI GRAFICHE PM10 #######################

#### serie del PM10 di tutte e 6 le città messe a confronto ####
# versione per tesi
par(mfrow=c(6,1),mar = c(0, 4.5, 0, 4.5), oma = c(3, 0, 2.5, 0))
ylim=range(c(dati$AR.REPUBBLICA,dati$LI.CARDUCCI,dati$PO.FERRUCCI,dati$FI.MOSSE,
             dati$PI.BORGHETTO,dati$LU.MICHELETTO))
plot(dati$DATE,dati$AR.REPUBBLICA, type='l',xaxt = "n", ylim=ylim, ylab="AR.REPUBBLICA")
fasce.invernali()
ind=dati$AR.REPUBBLICA>=50
points(dati$DATE[ind], dati$AR.REPUBBLICA[ind], col='red')
plot(dati$DATE,dati$LI.CARDUCCI, type='l',xaxt = "n",ylim=ylim, ylab="LI.CARDUCCI")
fasce.invernali()
ind=dati$LI.CARDUCCI>=50
points(dati$DATE[ind], dati$LI.CARDUCCI[ind], col='red')
plot(dati$DATE,dati$PO.FERRUCCI, type='l',xaxt = "n",ylim=ylim, ylab="PO.FERRUCCI")
fasce.invernali()
ind=dati$PO.FERRUCCI>=50
points(dati$DATE[ind], dati$PO.FERRUCCI[ind], col='red')
plot(dati$DATE,dati$FI.MOSSE, type='l',xaxt = "n",ylim=ylim, ylab="FI.MOSSE")
fasce.invernali()
ind=dati$FI.MOSSE>=50
points(dati$DATE[ind], dati$FI.MOSSE[ind], col='red')
plot(dati$DATE,dati$PI.BORGHETTO, type='l',xaxt = "n",ylim=ylim, ylab="PI.BORGHETTO")
fasce.invernali()
ind=dati$PI.BORGHETTO>=50
points(dati$DATE[ind], dati$PI.BORGHETTO[ind], col='red')
plot(dati$DATE,dati$LU.MICHELETTO, type='l',ylim=ylim, ylab="LU.MICHELETTO")
fasce.invernali()
ind=dati$LU.MICHELETTO>=50
points(dati$DATE[ind], dati$LU.MICHELETTO[ind], col='red')
mtext('PM10',outer = TRUE, cex = 1,  line = 1, font = 2)
#coppia di sforamenti in tutte e 6 le città in date "2020-03-28" e "2020-03-29"

#versione per presentazione
#pdf(paste("/Users/niccolodeglinnocenti/Desktop/Università/TESI/Grafici","ts_PM10_presentazione.pdf", sep='/'), width = 14, height = 6)
par(mfrow=c(2,3), mar=c(2,3,2,1), oma=c(3,0,2,0), cex.axis=0.7, cex.lab=0.8)
ylim=range(c(dati$AR.REPUBBLICA,dati$LI.CARDUCCI,dati$PO.FERRUCCI,dati$FI.MOSSE,
             dati$PI.BORGHETTO,dati$LU.MICHELETTO))
plot(dati$DATE,dati$AR.REPUBBLICA, type='l',ylim=ylim, main="AR")
fasce.invernali()
ind=dati$AR.REPUBBLICA>=50
points(dati$DATE[ind], dati$AR.REPUBBLICA[ind], col='red')
plot(dati$DATE,dati$LI.CARDUCCI, type='l',ylim=ylim, main="LI")
fasce.invernali()
ind=dati$LI.CARDUCCI>=50
points(dati$DATE[ind], dati$LI.CARDUCCI[ind], col='red')
plot(dati$DATE,dati$PO.FERRUCCI, type='l',ylim=ylim, main="PO")
fasce.invernali()
ind=dati$PO.FERRUCCI>=50
points(dati$DATE[ind], dati$PO.FERRUCCI[ind], col='red')
plot(dati$DATE,dati$FI.MOSSE, type='l',ylim=ylim, main="FI")
fasce.invernali()
ind=dati$FI.MOSSE>=50
points(dati$DATE[ind], dati$FI.MOSSE[ind], col='red')
plot(dati$DATE,dati$PI.BORGHETTO, type='l',ylim=ylim, main="PI")
fasce.invernali()
ind=dati$PI.BORGHETTO>=50
points(dati$DATE[ind], dati$PI.BORGHETTO[ind], col='red')
plot(dati$DATE,dati$LU.MICHELETTO, type='l',ylim=ylim, main="LU")
fasce.invernali()
ind=dati$LU.MICHELETTO>=50
points(dati$DATE[ind], dati$LU.MICHELETTO[ind], col='red')
#dev.off()

##### n.sformaneti contemporanei delle 6 città ####
m=matrix(rep(NA,36), ncol=6)
dimnames(m)[[1]]=names(dati)[-1]
dimnames(m)[[2]]=names(dati)[-1]
m[1,1]<-sum(dati$AR.REPUBBLICA>=50)
m[1,2]<-length(intersect(dati$DATE[dati$AR.REPUBBLICA>=50],dati$DATE[dati$LI.CARDUCCI>=50]))
m[1,3]<-length(intersect(dati$DATE[dati$AR.REPUBBLICA>=50],dati$DATE[dati$PO.FERRUCCI>=50]))
m[1,4]<-length(intersect(dati$DATE[dati$AR.REPUBBLICA>=50],dati$DATE[dati$FI.MOSSE>=50]))
m[1,5]<-length(intersect(dati$DATE[dati$AR.REPUBBLICA>=50],dati$DATE[dati$PI.BORGHETTO>=50]))
m[1,6]<-length(intersect(dati$DATE[dati$AR.REPUBBLICA>=50],dati$DATE[dati$LU.MICHELETTO>=50]))
m[2,2]<-sum(dati$LI.CARDUCCI>=50)
m[2,3]<-length(intersect(dati$DATE[dati$LI.CARDUCCI>=50],dati$DATE[dati$PO.FERRUCCI>=50]))
m[2,4]<-length(intersect(dati$DATE[dati$LI.CARDUCCI>=50],dati$DATE[dati$FI.MOSSE>=50]))
m[2,5]<-length(intersect(dati$DATE[dati$LI.CARDUCCI>=50],dati$DATE[dati$PI.BORGHETTO>=50]))
m[2,6]<-length(intersect(dati$DATE[dati$LI.CARDUCCI>=50],dati$DATE[dati$LU.MICHELETTO>=50]))
m[3,3]<-sum(dati$PO.FERRUCCI>=50)
m[3,4]<-length(intersect(dati$DATE[dati$PO.FERRUCCI>=50],dati$DATE[dati$FI.MOSSE>=50]))
m[3,5]<-length(intersect(dati$DATE[dati$PO.FERRUCCI>=50],dati$DATE[dati$PI.BORGHETTO>=50]))
m[3,6]<-length(intersect(dati$DATE[dati$PO.FERRUCCI>=50],dati$DATE[dati$LU.MICHELETTO>=50]))
m[4,4]<-sum(dati$FI.MOSSE>=50)
m[4,5]<-length(intersect(dati$DATE[dati$FI.MOSSE>=50],dati$DATE[dati$PI.BORGHETTO>=50]))
m[4,6]<-length(intersect(dati$DATE[dati$FI.MOSSE>=50],dati$DATE[dati$LU.MICHELETTO>=50]))
m[5,5]<-sum(dati$PI.BORGHETTO>=50)
m[5,6]<-length(intersect(dati$DATE[dati$PI.BORGHETTO>=50],dati$DATE[dati$LU.MICHELETTO>=50]))
m[6,6]<-sum(dati$LU.MICHELETTO>=50)
m[,1]<-m[1,]
m[,2]<-m[2,]
m[,3]<-m[3,]
m[,4]<-m[4,]
m[,5]<-m[5,]
m[,6]<-m[6,]
m

m.perc=m/diag(m)*100
m.perc[lower.tri(m.perc)]<-NA

pheatmap(m, display_numbers = TRUE, number_format = "%.0f", cluster_rows = FALSE,
         cluster_cols = FALSE, fontsize_number = 10, fontsize = 10,
         main = "")

pheatmap(m.perc, display_numbers = TRUE, number_format = "%.2f", cluster_rows = FALSE,
         cluster_cols = FALSE, fontsize_number = 10, fontsize = 10,
         main = "Percentuale di sforamenti contemporanei")

#### livello PM10 delle varie città ####
# range PM10 per stazioni di monitoraggio
D=data.frame(fascia=c('0-9','10-19','20-29','30-39','40-50','>50'),
         AR=c(sum(dati$AR.REPUBBLICA<10),sum(dati$AR.REPUBBLICA>=10 & dati$AR.REPUBBLICA<20),
              sum(dati$AR.REPUBBLICA>=20 & dati$AR.REPUBBLICA<30),
              sum(dati$AR.REPUBBLICA>=30 & dati$AR.REPUBBLICA<40),
              sum(dati$AR.REPUBBLICA>=40 & dati$AR.REPUBBLICA<50),
              sum(dati$AR.REPUBBLICA>=50)),
         LI=c(sum(dati$LI.CARDUCCI<10),sum(dati$LI.CARDUCCI>=10 & dati$LI.CARDUCCI<20),
              sum(dati$LI.CARDUCCI>=20 & dati$LI.CARDUCCI<30),
              sum(dati$LI.CARDUCCI>=30 & dati$LI.CARDUCCI<40),
              sum(dati$LI.CARDUCCI>=40 & dati$LI.CARDUCCI<50),
              sum(dati$LI.CARDUCCI>=50)),
         PO=c(sum(dati$PO.FERRUCCI<10),sum(dati$PO.FERRUCCI>=10 & dati$PO.FERRUCCI<20),
              sum(dati$PO.FERRUCCI>=20 & dati$PO.FERRUCCI<30),
              sum(dati$PO.FERRUCCI>=30 & dati$PO.FERRUCCI<40),
              sum(dati$PO.FERRUCCI>=40 & dati$PO.FERRUCCI<50),
              sum(dati$PO.FERRUCCI>=50)),
         FI=c(sum(dati$FI.MOSSE<10),sum(dati$FI.MOSSE>=10 & dati$FI.MOSSE<20),
              sum(dati$FI.MOSSE>=20 & dati$FI.MOSSE<30),
              sum(dati$FI.MOSSE>=30 & dati$FI.MOSSE<40),
              sum(dati$FI.MOSSE>=40 & dati$FI.MOSSE<50),
              sum(dati$FI.MOSSE>=50)),
         PI=c(sum(dati$PI.BORGHETTO<10),sum(dati$PI.BORGHETTO>=10 & dati$PI.BORGHETTO<20),
              sum(dati$PI.BORGHETTO>=20 & dati$PI.BORGHETTO<30),
              sum(dati$PI.BORGHETTO>=30 & dati$PI.BORGHETTO<40),
              sum(dati$PI.BORGHETTO>=40 & dati$PI.BORGHETTO<50),
              sum(dati$PI.BORGHETTO>=50)),
         LU=c(sum(dati$LU.MICHELETTO<10),sum(dati$LU.MICHELETTO>=10 & dati$LU.MICHELETTO<20),
              sum(dati$LU.MICHELETTO>=20 & dati$LU.MICHELETTO<30),
              sum(dati$LU.MICHELETTO>=30 & dati$LU.MICHELETTO<40),
              sum(dati$LU.MICHELETTO>=40 & dati$LU.MICHELETTO<50),
              sum(dati$LU.MICHELETTO>=50)))
D

D_long=pivot_longer(D, cols=-fascia, names_to="Città", values_to="Conteggio")
D_long$fascia = factor(D_long$fascia, 
                       levels = c('>50', '40-50', '30-39', '20-29', '10-19', '0-9'))
D_percent=mutate(group_by(D_long, Città),
  Percentuale=Conteggio/sum(Conteggio)*100)

ordine_citta=rev(c("AR", "LI", "PO", "FI", "PI","LU")) 
D_percent$Città=factor(D_percent$Città, levels=ordine_citta)

colori = c('red', '#FFCC33', '#FFFF66', '#CCFF33', '#99FF00', '#66FF00')
#colori= c('#66FF00','#99FF00','#CCFF33','#FFFF66','#FFCC33','red')
ggplot(D_percent, aes(x = Percentuale, y = Città, fill = fascia)) +
  geom_bar(stat = "identity") +
  labs(title = "",
       x = "Percentuale delle misurazioni",
       y = "Città") +
  theme_minimal() +
  scale_fill_manual(
    values = colori,
    labels = c(expression(">50"~mu*"g/m"^3),expression("40-50"~mu*"g/m"^3),
               expression("30-39"~mu*"g/m"^3),expression("20-29"~mu*"g/m"^3),
               expression("10-19"~mu*"g/m"^3),expression("0-9"~mu*"g/m"^3)),
    name = "Fasce PM10",
    guide = guide_legend(reverse = TRUE)) +
  scale_x_continuous(labels = function(x) paste0(x, "%"))

############################### PM10 VS METEO ################################## 
# medie meteo
var=c('rain','wind','umi','temp')
citta=c('AR','LI','PO','FI','PI','LU')
met=rbind(colMeans(meteo[,paste('rain',citta,sep='.')]),
          colMeans(meteo[,paste('wind',citta,sep='.')]),
          colMeans(meteo[,paste('umi',citta,sep='.')]),
          colMeans(meteo[,paste('temp',citta,sep='.')]))
dimnames(met)<-list(var,citta)
met

#### correlazione PM10 di ogni città con il suo meteo ####
cor.PM10.meteo(dati[-1],meteo)

#### confronto città per città e meteo ####
# evidenziati gli almeno 'days' giorni consecutivi in cui PM10>='PM10'

### AR REPUBBLICA
#PM10 vs meteo
days=5
PM10=40
par(mfrow=c(7,1),mar = c(0, 4.5, 0, 4.5), oma = c(3, 0, 2.5, 0))
plot(dati$DATE,dati$AR.REPUBBLICA, type='l',xaxt = "n", ylab="PM10")
fasce.invernali()
draw.rect(dati$AR.REPUBBLICA,PM10 = PM10, value = days, max=max(dati$AR.REPUBBLICA))
draw.line(PM10)
plot(dati$DATE,diff5.cumulata(meteo$rain.AR), type='l',xaxt = "n", ylab="cum. 5 rain")
draw.rect(dati$AR.REPUBBLICA,PM10 = PM10,value = days, max=max(diff5.cumulata(meteo$rain.AR)))
plot(dati$DATE,diff5.cumulata(meteo$wind.AR), type='l',xaxt = "n", ylab="cum. 5 wind")
draw.rect(dati$AR.REPUBBLICA,PM10 = PM10,value = days, max=max(diff5.cumulata(meteo$wind.AR)))
plot(dati$DATE, meteo$temp.AR, type='l',xaxt = "n", ylab='temp.')
draw.rect(dati$AR.REPUBBLICA,PM10 = PM10,value = days, max=max(meteo$temp.AR))
fasce.invernali()
plot(dati$DATE, meteo$wind.AR, type='l', xaxt='n', ylab='wind')
draw.rect(dati$AR.REPUBBLICA,PM10 = PM10,value = days, max=max(meteo$wind.AR))
plot(dati$DATE, meteo$rain.AR, type='l', xaxt='n', ylab='rain')
draw.rect(dati$AR.REPUBBLICA,PM10 = PM10,value = days, max=max(meteo$rain.AR))
plot(dati$DATE, meteo$umi.AR, type='l', ylab='humidity')
draw.rect(dati$AR.REPUBBLICA,PM10 = PM10,value = days, max=max(meteo$umi.AR))
#mtext(paste(names(dati[2]), paste(paste0("days = ",days), paste("PM10 = ", PM10),sep=' & ')), 
#      outer = TRUE, cex = 1,  line = 1, font = 2)

### LI CARDUCCI
#PM10 vs meteo
days=3
PM10=40
par(mfrow=c(7,1),mar = c(0, 4.5, 0, 4.5), oma = c(3, 0, 2.5, 0))
plot(dati$DATE,dati$LI.CARDUCCI, type='l',xaxt = "n", ylab="PM10")
fasce.invernali()
draw.rect(dati$LI.CARDUCCI,PM10 = PM10,value = days, max=max(dati$LI.CARDUCCI))
draw.line(PM10)
plot(dati$DATE,diff5.cumulata(meteo$rain.LI), type='l',xaxt = "n", ylab="cum. 5 rain")
draw.rect(dati$LI.CARDUCCI,PM10 = PM10,value = days, max=max(diff5.cumulata(meteo$rain.LI)))
plot(dati$DATE,diff5.cumulata(meteo$wind.LI), type='l',xaxt = "n", ylab="cum. 5 wind")
draw.rect(dati$LI.CARDUCCI,PM10 = PM10,value = days, max=max(diff5.cumulata(meteo$wind.LI)))
plot(dati$DATE, meteo$temp.LI, type='l',xaxt = "n", ylab='temp.')
draw.rect(dati$LI.CARDUCCI,PM10 = PM10,value = days, max=max(meteo$temp.LI))
fasce.invernali()
plot(dati$DATE, meteo$wind.LI, type='l', xaxt='n', ylab='wind')
draw.rect(dati$LI.CARDUCCI,PM10 = PM10,value = days, max=max(meteo$wind.LI))
plot(dati$DATE, meteo$rain.LI, type='l', xaxt='n', ylab='rain')
draw.rect(dati$LI.CARDUCCI,PM10 = PM10,value = days, max=max(meteo$rain.LI))
plot(dati$DATE, meteo$umi.LI, type='l', ylab='humidity')
draw.rect(dati$LI.CARDUCCI,PM10 = PM10,value = days, max=max(meteo$umi.LI))
# mtext(paste(names(dati[3]), paste(paste0("days = ",days), paste("PM10 = ", PM10),sep=' & ')), 
#       outer = TRUE, cex = 1,  line = 1, font = 2)

### PO FERRUCCI
#PM10 vs meteo
days=5
PM10=40
par(mfrow=c(7,1),mar = c(0, 4.5, 0, 4.5), oma = c(3, 0, 2.5, 0))
plot(dati$DATE,dati$PO.FERRUCCI, type='l',xaxt = "n", ylab="PM10")
fasce.invernali()
draw.rect(dati$PO.FERRUCCI ,PM10 = PM10,value = days, max=max(dati$PO.FERRUCCI))
draw.line(PM10)
plot(dati$DATE, diff5.cumulata(meteo$rain.PO), type='l',xaxt = "n", ylab="cum. 5 rain")
draw.rect(dati$PO.FERRUCCI ,PM10 = PM10,value = days, max=max(diff5.cumulata(meteo$rain.PO)))
plot(dati$DATE,diff5.cumulata(meteo$wind.PO), type='l',xaxt = "n", ylab="cum. 5 wind")
draw.rect(dati$PO.FERRUCCI,PM10 = PM10,value = days, max=max(diff5.cumulata(meteo$wind.PO)))
plot(dati$DATE, meteo$temp.PO, type='l',xaxt = "n", ylab='temp.')
draw.rect(dati$PO.FERRUCCI ,PM10 = PM10,value = days, max=max(meteo$temp.PO))
fasce.invernali()
plot(dati$DATE, meteo$wind.PO, type='l', xaxt='n', ylab='wind')
draw.rect(dati$PO.FERRUCCI ,PM10 = PM10,value = days, max=max(meteo$wind.PO))
plot(dati$DATE, meteo$rain.PO, type='l', xaxt='n', ylab='rain')
draw.rect(dati$PO.FERRUCCI ,PM10 = PM10,value = days, max=max(meteo$rain.PO))
plot(dati$DATE, meteo$umi.PO, type='l', ylab='humidity')
draw.rect(dati$PO.FERRUCCI ,PM10 = PM10,value = days, max=max(meteo$umi.PO))
# mtext(paste(names(dati[4]), paste(paste0("days = ",days), paste("PM10 = ", PM10),sep=' & ')), 
#       outer = TRUE, cex = 1,  line = 1, font = 2)

### FI MOSSE
#PM10 vs meteo
days=5
PM10=40
par(mfrow=c(7,1),mar = c(0, 4.5, 0, 4.5), oma = c(3, 0, 2.5, 0))
plot(dati$DATE,dati$FI.MOSSE, type='l',xaxt = "n", ylab="PM10")
fasce.invernali()
draw.rect(dati$FI.MOSSE ,PM10 = PM10,value = days, max=max(dati$FI.MOSSE))
draw.line(PM10)
plot(dati$DATE,diff5.cumulata(meteo$rain.FI), type='l',xaxt = "n", ylab="cum. 5 rain")
draw.rect(dati$FI.MOSSE ,PM10 = PM10,value = days, max=max(diff5.cumulata(meteo$rain.FI)))
plot(dati$DATE,diff5.cumulata(meteo$wind.FI), type='l',xaxt = "n", ylab="cum. 5 wind")
draw.rect(dati$FI.MOSSE,PM10 = PM10,value = days, max=max(diff5.cumulata(meteo$wind.FI)))
plot(dati$DATE, meteo$temp.FI, type='l',xaxt = "n", ylab='temp.')
draw.rect(dati$FI.MOSSE ,PM10 = PM10,value = days, max=max(meteo$temp.FI))
fasce.invernali()
plot(dati$DATE, meteo$wind.FI, type='l', xaxt='n', ylab='wind')
draw.rect(dati$FI.MOSSE ,PM10 = PM10,value = days, max=max(meteo$wind.FI))
plot(dati$DATE, meteo$rain.FI, type='l', xaxt='n', ylab='rain')
draw.rect(dati$FI.MOSSE ,PM10 = PM10,value = days, max=max(meteo$rain.FI))
plot(dati$DATE, meteo$umi.FI, type='l', ylab='humidity')
draw.rect(dati$FI.MOSSE ,PM10 = PM10,value = days, max=max(meteo$umi.FI))
# mtext(paste(names(dati[5]), paste(paste0("days = ",days), paste("PM10 = ", PM10),sep=' & ')), 
#       outer = TRUE, cex = 1,  line = 1, font = 2)

#### versione FI presentazione ####
days=5
PM10=40
par(mfrow=c(5,1),mar = c(0, 4.5, 0, 4.5), oma = c(3, 0, 2.5, 0))
plot(dati$DATE,dati$FI.MOSSE, type='l',xaxt = "n", ylab="PM10")
fasce.invernali()
draw.rect(dati$FI.MOSSE ,PM10 = PM10,value = days, max=max(dati$FI.MOSSE))
draw.line(PM10)
plot(dati$DATE, meteo$temp.FI, type='l',xaxt = "n", ylab='temp.')
draw.rect(dati$FI.MOSSE ,PM10 = PM10,value = days, max=max(meteo$temp.FI))
fasce.invernali()
plot(dati$DATE, meteo$wind.FI, type='l', xaxt='n', ylab='wind speed')
draw.rect(dati$FI.MOSSE ,PM10 = PM10,value = days, max=max(meteo$wind.FI))
plot(dati$DATE, meteo$rain.FI, type='l', xaxt='n', ylab='rain')
draw.rect(dati$FI.MOSSE ,PM10 = PM10,value = days, max=max(meteo$rain.FI))
plot(dati$DATE, meteo$umi.FI, type='l', ylab='humidity')
draw.rect(dati$FI.MOSSE ,PM10 = PM10,value = days, max=max(meteo$umi.FI))
mtext("FI", outer = TRUE, cex = 1,  line = 1, font = 2)
###########

### PI BORGHETTO
#PM10 vs meteo
days=5
PM10=40
par(mfrow=c(7,1),mar = c(0, 4.5, 0, 4.5), oma = c(3, 0, 2.5, 0))
plot(dati$DATE,dati$PI.BORGHETTO, type='l',xaxt = "n", ylab="PM10")
fasce.invernali()
draw.rect(dati$PI.BORGHETTO ,PM10 = PM10,value = days, max=max(dati$PI.BORGHETTO))
draw.line(PM10)
plot(dati$DATE,diff5.cumulata(meteo$rain.PI), type='l',xaxt = "n", ylab="cum. 5 rain")
draw.rect(dati$PI.BORGHETTO ,PM10 = PM10,value = days, max=max(diff5.cumulata(meteo$rain.PI)))
plot(dati$DATE,diff5.cumulata(meteo$wind.PI), type='l',xaxt = "n", ylab="cum. 5 wind")
draw.rect(dati$PI.BORGHETTO,PM10 = PM10,value = days, max=max(diff5.cumulata(meteo$wind.PI)))
plot(dati$DATE, meteo$temp.PI, type='l',xaxt = "n", ylab='temp.')
draw.rect(dati$PI.BORGHETTO ,PM10 = PM10,value = days, max=max(meteo$temp.PI))
fasce.invernali()
plot(dati$DATE, meteo$wind.PI, type='l', xaxt='n', ylab='wind')
draw.rect(dati$PI.BORGHETTO ,PM10 = PM10,value = days, max=max(meteo$wind.PI))
plot(dati$DATE, meteo$rain.PI, type='l', xaxt='n', ylab='rain')
draw.rect(dati$PI.BORGHETTO ,PM10 = PM10,value = days, max=max(meteo$rain.PI))
plot(dati$DATE, meteo$umi.PI, type='l', ylab='humidity')
draw.rect(dati$PI.BORGHETTO ,PM10 = PM10,value = days, max=max(meteo$umi.PI))
# mtext(paste(names(dati[6]), paste(paste0("days = ",days), paste("PM10 = ", PM10),sep=' & ')), 
#       outer = TRUE, cex = 1,  line = 1, font = 2)

### LU MICHELETTO
#PM10 vs meteo
days=5
PM10=40
par(mfrow=c(7,1),mar = c(0, 4.5, 0, 4.5), oma = c(3, 0, 2.5, 0))
plot(dati$DATE,dati$LU.MICHELETTO, type='l',xaxt = "n", ylab="PM10")
fasce.invernali()
draw.rect(dati$LU.MICHELETTO ,PM10 = PM10,value = days, max=max(dati$LU.MICHELETTO))
draw.line(PM10)
plot(dati$DATE,diff5.cumulata(meteo$rain.LU), type='l',xaxt = "n", ylab="cum. 5 rain")
draw.rect(dati$LU.MICHELETTO ,PM10 = PM10,value = days, max=max(diff5.cumulata(meteo$rain.LU)))
plot(dati$DATE,diff5.cumulata(meteo$wind.LU), type='l',xaxt = "n", ylab="cum. 5 wind")
draw.rect(dati$LU.MICHELETTO,PM10 = PM10,value = days, max=max(diff5.cumulata(meteo$wind.LU)))
plot(dati$DATE, meteo$temp.LU, type='l',xaxt = "n", ylab='temp.')
draw.rect(dati$LU.MICHELETTO ,PM10 = PM10,value = days, max=max(meteo$temp.LU))
fasce.invernali()
plot(dati$DATE, meteo$wind.LU, type='l', xaxt='n', ylab='wind')
draw.rect(dati$LU.MICHELETTO ,PM10 = PM10,value = days, max=max(meteo$wind.LU))
plot(dati$DATE, meteo$rain.LU, type='l', xaxt='n', ylab='rain')
draw.rect(dati$LU.MICHELETTO ,PM10 = PM10,value = days, max=max(meteo$rain.LU))
plot(dati$DATE, meteo$umi.LU, type='l', ylab='humidity')
draw.rect(dati$LU.MICHELETTO ,PM10 = PM10,value = days, max=max(meteo$umi.LU))
# mtext(paste(names(dati[7]), paste(paste0("days = ",days), paste("PM10 = ", PM10),sep=' & ')), 
#       outer = TRUE, cex = 1,  line = 1, font = 2)

########################### TABELLA RIASSUNTIVA ################################

# quartili del meteo
ra=c(0,1,4,15,256.40)
ve=c(0,1,2,4,15)
te=c(-1.20,10,15,22,34)
um=c(0,65,75,85,101)
var=c('rain','umi','temp','wind')
citta=c('AR','LI','PO','FI','PI','LU')

########### PM10=40 ############

PM10=40 # si prende in considerazione solo le misurazioni >= di questo valore

#tabella riassuntiva con divisione città
tab2=NULL
for(i in 1:length(citta)){
  nomi.meteo=paste(var,citta[i],sep='.')
  ind=dati[-1][,i]>=PM10
  r=cut(meteo[ind,nomi.meteo[1]],breaks = ra, right=F)
  levels(r)<-1:4
  u=cut(meteo[ind,nomi.meteo[2]],breaks = um, right=F)
  levels(u)<-1:4
  t=cut(meteo[ind,nomi.meteo[3]],breaks = te, right=F)
  levels(t)<-1:4
  w=cut(meteo[ind,nomi.meteo[4]],breaks = ve, right=F)
  levels(w)<-1:4
  li=split(dati[-1][ind,i],list(r,u,t,w))
  x=unlist(lapply(li,length))
  if(i!=1){names(x)=NULL}
  tab2=cbind(tab2,x)
}
tab2=data.frame(tab2)
names(tab2)=citta
tab2.perc=round(apply(tab2,2,function(vet){(vet/sum(vet))*100}),3)

## TABELLA RIASSUNTIVA FINALE 
# ultima colonna è il totale, non si considera la divisione in città
tab.40=cbind(tab2.perc,TOT=round((apply(tab2,1,sum)/sum(apply(tab2,1,sum)))*100,3))
# pioggia, umidità, temperatura, vento
#tab.40
#write.csv(tab,'/Users/niccolodeglinnocenti/Desktop/tab.riassuntiva')

# tabella riassuntiva eliminando zeri e senza divisione città
TAB=apply(tab2,1,sum)
ind=which(TAB!=0)
TAB=TAB[ind]
x.40=round((matrix(TAB)/sum(TAB))*100,3)
rownames(x.40)=row.names(tab.40)[ind]
colnames(x.40)=paste0('% PM10>=',PM10)
x.40

#'rain','umi','temp','wind'
#eliminando percentuali minori di 1
ind=x.40[,1]<1
a.40=data.frame(x.40[!ind])
rownames(a.40)=rownames(x.40)[!ind]
a.40


########### PM10=50
PM10=50 # si prende in considerazione solo le misurazioni >= di questo valore

#tabella riassuntiva con divisione città
tab2=NULL
for(i in 1:length(citta)){
  nomi.meteo=paste(var,citta[i],sep='.')
  ind=dati[-1][,i]>=PM10
  r=cut(meteo[ind,nomi.meteo[1]],breaks = ra, right=F)
  levels(r)<-1:4
  u=cut(meteo[ind,nomi.meteo[2]],breaks = um, right=F)
  levels(u)<-1:4
  t=cut(meteo[ind,nomi.meteo[3]],breaks = te, right=F)
  levels(t)<-1:4
  w=cut(meteo[ind,nomi.meteo[4]],breaks = ve, right=F)
  levels(w)<-1:4
  li=split(dati[-1][ind,i],list(r,u,t,w))
  x=unlist(lapply(li,length))
  if(i!=1){names(x)=NULL}
  tab2=cbind(tab2,x)
}
tab2=data.frame(tab2)
names(tab2)=citta
tab2.perc=round(apply(tab2,2,function(vet){(vet/sum(vet))*100}),3)

## TABELLA RIASSUNTIVA FINALE 
# ultima colonna è il totale, non si considera la divisione in città
tab.50=cbind(tab2.perc,TOT=round((apply(tab2,1,sum)/sum(apply(tab2,1,sum)))*100,3))
# pioggia, umidità, temperatura, vento
#tab.50
#write.csv(tab,'/Users/niccolodeglinnocenti/Desktop/tab.riassuntiva')

# tabella riassuntiva eliminando zeri e senza divisione città
TAB=apply(tab2,1,sum)
ind=which(TAB!=0)
TAB=TAB[ind]
x.50=round((matrix(TAB)/sum(TAB))*100,3)
rownames(x.50)=row.names(tab.50)[ind]
colnames(x.50)=paste0('% PM10>=',PM10)
x.50

#'rain','umi','temp','wind'
#eliminando percentuali minori di 1
ind=x.50[,1]<1
a.50=data.frame(x.50[!ind])
rownames(a.50)=rownames(x.50)[!ind]
a.50

#### pie-chart per presentazione ####
par(mfrow=c(1,2), mar=c(3.5,1.1,3,1), oma=c(5,0,0,0))  
ind=x.40[,1]<5
b.40=x.40[!ind]
b.40[4]<-100-sum(b.40)
names(b.40)=c(rownames(x.40)[!ind],'altro')
cols <- c("red", "tomato","#fc8d62", "#66c2a5" )
pct.40 <- paste0(round(b.40, 1), "%")
pie(b.40, labels=pct.40, col = cols[1:length(b.40)], 
    main = 'PM10>=40', cex.main = 0.8)
legend("bottomleft", legend=names(b.40), fill=cols[1:length(b.40)])

ind=x.50[,1]<5
b.50=x.50[!ind]
b.50[4]<-100-sum(b.50)
names(b.50)=c(rownames(x.50)[!ind],'altro')
pct.50 <- paste0(round(b.50, 1), "%")
pie(b.50, labels=pct.50, col = cols[1:length(b.50)], 
    main = 'PM10>=50', cex.main = 0.8)


dev.off()
#distribuzione delle precipitazioni/vento/temperatura/umidità nei giorni in PM10>=PM10
piog=NULL
vent=NULL
temp=NULL
umi=NULL
for(i in 1:length(citta)){
  nomi.meteo=paste(var,citta[i],sep='.')
  ind=dati[-1][,i]>=PM10
  piog=c(piog,meteo[ind,nomi.meteo[1]])
  vent=c(vent,meteo[ind,nomi.meteo[4]])
  temp=c(temp,meteo[ind,nomi.meteo[3]])
  umi=c(umi,meteo[ind,nomi.meteo[2]])
}

par(mfrow=c(2,2))
hist(piog, breaks=60, probability=T, main='Distribuzione delle precipitazioni', 
     xlab='mm', ylab='Densità', xaxt='n')
axis(1,at = seq(min(piog), max(piog), by = 1))
hist(vent, breaks=60, probability=T, main='Distribuzione della velocità del vento media', 
     xlab='m/s', ylab='Densità', xaxt='n')
axis(1,at = seq(min(vent), max(vent), by = 0.5))
hist(umi, breaks=50, probability=T, main="Distribuzione dell'umidità media", 
     xlab='%', ylab='Densità',  xaxt='n')
axis(1,at = seq(min(umi), max(umi), by = 5))
hist(temp, breaks=50, probability=T, main='Distribuzione della temperatura media', 
     xlab='°C', ylab='Densità')

#distribuzione delle precipitazioni/vento/temperatura/umidità nei giorni in PM10<PM10
piog1=NULL
vent1=NULL
temp1=NULL
umi1=NULL
for(i in 1:length(citta)){
  nomi.meteo=paste(var,citta[i],sep='.')
  ind=dati[-1][,i]<PM10
  piog1=c(piog,meteo[ind,nomi.meteo[1]])
  vent1=c(vent,meteo[ind,nomi.meteo[4]])
  temp1=c(temp,meteo[ind,nomi.meteo[3]])
  umi1=c(umi,meteo[ind,nomi.meteo[2]])
}

par(mfrow=c(2,2))
hist(piog, breaks=50, probability=T, main='Distribuzione delle precipitazioni', 
     xlab='Precipitazioni in mm', ylab='Densità', xaxt='n')
axis(1, at=seq(min(piog), max(piog), by = 5))
hist(vent, breaks=50, probability=T, main='Distribuzione del vento medio', 
     xlab='Vento medio in m/s', ylab='Densità', xaxt='n')
axis(1, at=seq(min(vent), max(vent), by = 1))
hist(umi, breaks=50, probability=T, main="Distribuzione dell'umidità media", 
     xlab='Umidità media percentuale', ylab='Densità', xaxt='n')
axis(1, at=seq(round(min(umi),0), max(umi), by = 5))
hist(temp, breaks=50, probability=T, main='Distribuzione della temperatura media', 
     xlab='Temperatura media in °C', ylab='Densità')

########################### BOX-PLOT PM10 vs METEO #############################
#condizioni favorevoli all'accumulo
citta <- c("AR", "LI", "PO", "FI", "PI", "LU")
colonne_dati <- c("AR.REPUBBLICA", "LI.CARDUCCI", "PO.FERRUCCI", 
                  "FI.MOSSE", "PI.BORGHETTO", "LU.MICHELETTO")

anni <- format(dati$DATE, "%Y")
df_plot <- data.frame()

for (i in seq_along(citta)) {
  nome_citta <- citta[i]
  nome_col <- colonne_dati[i]
  
  giorni_fav <- giorni.fav(nome_citta)
  
  valori_fav <- dati[[nome_col]][giorni_fav]
  valori_nofav <- dati[[nome_col]][!giorni_fav]
  
  anni_fav <- anni[giorni_fav]
  anni_nofav <- anni[!giorni_fav]
  
  df_plot <- rbind(df_plot,
                   data.frame(valore = valori_fav,
                              citta = nome_citta,
                              condizione = "Favorevole",
                              anno = anni_fav),
                   data.frame(valore = valori_nofav,
                              citta = nome_citta,
                              condizione = "Non favorevole",
                              anno = anni_nofav))
}

df_plot$citta <- factor(df_plot$citta, levels = c('AR','LI','PO','FI','PI','LU'))
df_plot$condizione <- factor(df_plot$condizione, levels = c("Favorevole", "Non favorevole"))
df_plot$anno <- factor(df_plot$anno, levels = sort(unique(df_plot$anno))) 

ggplot(df_plot, aes(x = citta, y = valore, fill = condizione)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  stat_summary(
    aes(group = condizione), fun = mean, geom = "point", shape = 21, size = 1.5,
    color = "black", fill = "white", position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 30, color = "red", linetype = "solid", linewidth = 0.3) + 
  facet_wrap(~ anno) +  coord_cartesian(ylim = c(0, 110)) +
  labs(title = "",
       x = "Città", y = "Valore osservato",
       fill = "Condizione") +
  scale_y_continuous(breaks = seq(0, max(df_plot$valore, na.rm = TRUE), by = 10)) +
  theme_minimal()

#######################################################################################
# analisi concentrazione PM10 confronto con cumulata su 5 giorni

cum5.rain.AR=diff5.cumulata(meteo$rain.AR)
cum5.wind.AR=diff5.cumulata(meteo$wind.AR)
cum5.rain.LI=diff5.cumulata(meteo$rain.LI)
cum5.wind.LI=diff5.cumulata(meteo$wind.LI)
cum5.rain.PO=diff5.cumulata(meteo$rain.PO)
cum5.wind.PO=diff5.cumulata(meteo$wind.PO)
cum5.rain.FI=diff5.cumulata(meteo$rain.FI)
cum5.wind.FI=diff5.cumulata(meteo$wind.FI)
cum5.rain.PI=diff5.cumulata(meteo$rain.PI)
cum5.wind.PI=diff5.cumulata(meteo$wind.PI)
cum5.rain.LU=diff5.cumulata(meteo$rain.LU)
cum5.wind.LU=diff5.cumulata(meteo$wind.LU)

mean.5days=function(x, lag){
  vet=NULL
  for(i in 5:length(x)){
    vet=c(vet,mean(x[(i-4):i]))
  }
  c(rep(mean(x),4),vet)
}

mean.cum.5=rbind(
c( mean(dati$AR.REPUBBLICA[cum5.rain.AR<1]), 
   mean(dati$AR.REPUBBLICA[cum5.rain.AR>=1]),
   mean(dati$AR.REPUBBLICA[cum5.wind.AR<5]),
   mean(dati$AR.REPUBBLICA[cum5.wind.AR>=5]) ),

c( mean(dati$LI.CARDUCCI[cum5.rain.LI<1]), 
   mean(dati$LI.CARDUCCI[cum5.rain.LI>=1]),
   mean(dati$LI.CARDUCCI[cum5.wind.LI<5]),
   mean(dati$LI.CARDUCCI[cum5.wind.LI>=5]) ),

c( mean(dati$PO.FERRUCCI[cum5.rain.PO<1]), 
   mean(dati$PO.FERRUCCI[cum5.rain.PO>=1]),
   mean(dati$PO.FERRUCCI[cum5.wind.PO<5]),
   mean(dati$PO.FERRUCCI[cum5.wind.PO>=5]) ),

c( mean(dati$FI.MOSSE[cum5.rain.FI<1]), 
   mean(dati$FI.MOSSE[cum5.rain.FI>=1]),
   mean(dati$FI.MOSSE[cum5.wind.FI<5]),
   mean(dati$FI.MOSSE[cum5.wind.FI>=5]) ),

c( mean(dati$PI.BORGHETTO[cum5.rain.PI<1]), 
   mean(dati$PI.BORGHETTO[cum5.rain.PI>=1]),
   mean(dati$PI.BORGHETTO[cum5.wind.PI<5]),
   mean(dati$PI.BORGHETTO[cum5.wind.PI>=5]) ),

c( mean(dati$LU.MICHELETTO[cum5.rain.LU<1]), 
   mean(dati$LU.MICHELETTO[cum5.rain.LU>=1]),
   mean(dati$LU.MICHELETTO[cum5.wind.LU<5]),
   mean(dati$LU.MICHELETTO[cum5.wind.LU>=5]) ))

dimnames(mean.cum.5)=list(c('AR','LI','PO','FI','PI','LU'),
  c('rain cum5<1','rain cum5>=1','wind cum5<5','wind cum5>=5'))

mean.cum.5

data_plot=data.frame(
  valori=c(dati$AR.REPUBBLICA,dati$AR.REPUBBLICA,
           dati$LI.CARDUCCI,dati$LI.CARDUCCI,
           dati$PO.FERRUCCI,dati$PO.FERRUCCI,
           dati$FI.MOSSE,dati$FI.MOSSE,
           dati$PI.BORGHETTO,dati$PI.BORGHETTO,
           dati$LU.MICHELETTO,dati$LU.MICHELETTO),
  citta=c(rep("AR",2922),rep("LI",2922),rep("PO",2922),rep("FI",2922),
          rep("PI",2922),rep("LU",2922)),
  fenomeno=c(rep('Pioggia',1461),rep('Vento',1461),rep('Pioggia',1461),rep('Vento',1461),
             rep('Pioggia',1461),rep('Vento',1461),rep('Pioggia',1461),rep('Vento',1461),
             rep('Pioggia',1461),rep('Vento',1461),rep('Pioggia',1461),rep('Vento',1461)),
  condizione=c(cum5.rain.AR<1,cum5.wind.AR<5,
               cum5.rain.LI<1,cum5.wind.LI<5,
               cum5.rain.PO<1,cum5.wind.PO<5,
               cum5.rain.FI<1,cum5.wind.FI<5,
               cum5.rain.PI<1,cum5.wind.PI<5,
               cum5.rain.LU<1,cum5.wind.LU<5))

data_plot <- data_plot %>%
  mutate(gruppo = case_when(
    fenomeno == "Pioggia" & condizione == "TRUE"  ~ "Precipitazioni 5gg < 1mm",
    fenomeno == "Pioggia" & condizione == "FALSE" ~ "Precipitazioni 5gg >= 1mm",
    fenomeno == "Vento"   & condizione == "TRUE"  ~ "Velocità vento 5gg < 5 m/s",
    fenomeno == "Vento"   & condizione == "FALSE" ~ "Velocità vento 5gg >= 5 m/s"))

data_plot$citta=factor(data_plot$citta, levels = c('AR','LI','PO','FI','PI','LU'))
data_plot$gruppo <- factor(data_plot$gruppo, levels = c("Precipitazioni 5gg < 1mm", "Precipitazioni 5gg >= 1mm", 
                                                       "Velocità vento 5gg < 5 m/s", "Velocità vento 5gg >= 5 m/s"))

#boxplot
ggplot(data_plot, aes(x = gruppo, y = valori, fill = gruppo)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_vline(xintercept = 2.5, linetype = "dashed", color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2.5, 
               color = "black", fill = "white", position = position_dodge(width = 0.75)) +
  facet_wrap(~ citta, scales = "fixed") + coord_cartesian(ylim = c(0, 75)) +
  scale_fill_manual(name = "Condizione",
    values = c(
      "Precipitazioni 5gg < 1mm" = "#1f77b4",  
      "Precipitazioni 5gg >= 1mm" = "#aec7e8",  
      "Velocità vento 5gg < 5 m/s"   = "#ff7f0e",  
      "Velocità vento 5gg >= 5 m/s"   = "#ffbb78"),
    labels = c("Precipitazioni 5gg < 1mm", "Precipitazioni 5gg >= 1mm", 
               "Velocità vento 5gg < 5 m/s", "Velocità vento 5gg >= 5 m/s")) +
  labs(title = "", y = "Valori") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        legend.position = "right")

# violin plot
ggplot(data_plot, aes(x = gruppo, y = valori, fill = gruppo)) +
  geom_violin(aes(x = gruppo, y = valori, fill = gruppo), trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, position = position_dodge(0.9), alpha = 0.3)  +
  #geom_vline(xintercept = 2.5, linetype = "dashed", color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, 
               color = "black", fill = "white", position = position_dodge(width = 0.75)) +
  facet_wrap(~ citta, scales = "fixed") +
  scale_fill_manual(name = "Condizione",
                    values = c(
                      "Precipitazioni 5gg < 1mm" = "#1f77b4",  
                      "Precipitazioni 5gg >= 1mm" = "#aec7e8",  
                      "Velocità vento 5gg < 5 m/s"   = "#ff7f0e",  
                      "Velocità vento 5gg >= 5 m/s"   = "#ffbb78"),
                    labels = c("Precipitazioni 5gg < 1mm", "Precipitazioni 5gg >= 1mm", 
                               "Velocità vento 5gg < 5 m/s", "Velocità vento 5gg >= 5 m/s")) +
  labs(title = "Distribuzione valori per città e condizione", y = "Valori") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        legend.position = "right")

#joy plot
mean_data <- data_plot %>%
  group_by(citta, gruppo) %>%
  summarise(media = mean(valori), .groups = 'drop')

ggplot(data_plot, aes(x = valori, y = gruppo, fill = gruppo)) +
  ggridges::geom_density_ridges(scale = 1) +
  facet_wrap(~ citta, scales = "fixed") +
  scale_fill_manual(name = "Condizione",
                    values = c(
                      "Precipitazioni 5gg < 1mm" = "#1f77b4",  
                      "Precipitazioni 5gg >= 1mm" = "#aec7e8",  
                      "Velocità vento 5gg < 5 m/s"   = "#ff7f0e",  
                      "Velocità vento 5gg >= 5 m/s"   = "#ffbb78"),
                    labels = c("Precipitazioni 5gg < 1mm", "Precipitazioni 5gg >= 1mm", 
                               "Velocità vento 5gg < 5 m/s", "Velocità vento 5gg >= 5 m/s")) +
  labs(title = "Distribuzione valori per città e condizione", y = "Valori") +
  theme_minimal() +
  theme(legend.position = "none")
