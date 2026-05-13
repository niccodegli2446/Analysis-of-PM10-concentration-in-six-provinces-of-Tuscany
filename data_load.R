#### Libraries and Functions
library(zoo)
library(forecast)
library(lmtest)      
library(tsoutliers)   
library(urca)         
library(FinTS)   
library(timeDate)
library(vars)
library(systemfit)
library(ggplot2)
library(sf)
library(tidyr)
library(dplyr)
library(pheatmap)
library(geosphere)
library(ggridges)
source("/Users/niccolodeglinnocenti/Desktop/Università/TESI/funzioni utili/DN-Functions-20250410.R")
source("/Users/niccolodeglinnocenti/Desktop/Università/TESI/funzioni utili/TSA-Predict-Student-Functions.R")
source("/Users/niccolodeglinnocenti/Desktop/Università/TESI/funzioni utili/CalendarEffects-Student-Functions.R")
source("/Users/niccolodeglinnocenti/Desktop/Università/TESI/funzioni utili/TSA-Useful-Functions.R")
source("/Users/niccolodeglinnocenti/Desktop/Università/TESI/funzioni utili/psGIRF-work.R")

# percosrso directory
myDir='/Users/niccolodeglinnocenti/Desktop/Università/TESI/DATI'

######################### IMPORTAZIONE DATI PM10 ##############################
###############################################################################

file.path=paste0(myDir,"/dati_PM10/")
stazioni=c('AR-REPUBBLICA','LI-CARDUCCI','PO-FERRUCCI','FI-MOSSE',
           'PI-BORGHETTO', 'LU-MICHELETTO')
anni=c('2020','2021','2022','2023')
dati=NULL
for(i in anni){
  for(j in stazioni){
    path=paste(paste(paste(paste0(file.path,j),'PM10', sep='_'),i,sep='_'),'csv', sep='.')
    dati=rbind(dati, read.table(file = path, header=T, sep=';', dec='.'))
  }
}
dati$DATA<-as.Date(as.character(dati$DATA), format = "%Y%m%d")

data=data.frame(unique(dati$DATA), dati$MEDIA.GIORNALIERA[dati$STAZIONE==stazioni[1]],
                                   dati$MEDIA.GIORNALIERA[dati$STAZIONE==stazioni[2]],
                                   dati$MEDIA.GIORNALIERA[dati$STAZIONE==stazioni[3]],
                                   dati$MEDIA.GIORNALIERA[dati$STAZIONE==stazioni[4]],
                                   dati$MEDIA.GIORNALIERA[dati$STAZIONE==stazioni[5]],
                                   dati$MEDIA.GIORNALIERA[dati$STAZIONE==stazioni[6]])
names(data)<-c('date',stazioni)

data$`AR-REPUBBLICA`[data$`AR-REPUBBLICA`=='NaN']<-rep(NA,sum(data$`AR-REPUBBLICA`=='NaN'))
data$`LI-CARDUCCI`[data$`LI-CARDUCCI`=='NaN']<-rep(NA,sum(data$`LI-CARDUCCI`=='NaN'))
data$`FI-MOSSE`[data$`FI-MOSSE`=='NaN']<-rep(NA,sum(data$`FI-MOSSE`=='NaN'))
data$`PO-FERRUCCI`[data$`PO-FERRUCCI`=='NaN']<-rep(NA,sum(data$`PO-FERRUCCI`=='NaN'))
data$`PI-BORGHETTO`[data$`PI-BORGHETTO`=='NaN']<-rep(NA,sum(data$`PI-BORGHETTO`=='NaN'))
data$`LU-MICHELETTO`[data$`LU-MICHELETTO`=='NaN']<-rep(NA,sum(data$`LU-MICHELETTO`=='NaN'))

nomi=c('DATE','AR.REPUBBLICA','LI.CARDUCCI','PO.FERRUCCI','FI.MOSSE',
       'PI.BORGHETTO','LU.MICHELETTO')
names(data)<-nomi

##################### IMPORTAZIONE DATI METEO (Meteo.it) ######################
###############################################################################

# file.path=paste0(myDir,"/dati_meteo_il.meteo/")
# citta=c('Firenze','Prato','Pisa','Livorno')
# anni=c('2020','2021','2022','2023')
# mesi=c('Gennaio','Febbraio','Marzo','Aprile','Maggio','Giugno','Luglio',
# 'Agosto','Settembre','Ottobre','Novembre','Dicembre')
# 
# datiM=NULL
# for(i in citta){
#   for(j in anni){
#     for(k in mesi){
#       path=paste0(file.path,paste(i,j,paste0(k,'.csv'), sep='-'))
#       datiM=rbind(datiM, read.table(path, header=T, sep=';', dec='.'))
#     }
#   }
# }
# names(datiM)<-c("LOCALITA","DATA", "TMEDIA", "TMIN",  "TMAX", 
#                 "PUNTORUGIADA", "UMIDITA", "VISIBILITA.km",
#                 "VENTOMEDIA.km.h", "VENTOMAX.km.h", "RAFFICA.km.h" , "PRESSIONESLM.mb",
#                  "PRESSIONEMEDIA.mb", "PIOGGIA.mm",  "FENOMENI")
# datiM$DATA<-as.Date(datiM$DATA, format='%d/%m/%Y')

################### IMPORTAZIONE DATI METEO (sir.toscana.it) ###################
################################################################################

file.path=paste0(myDir,"/dati_meteo_s.toscana/")
centraline=c('TOS01001096','TOS01001205','TOS11000001','TOS11000513',
             'TOS11000039','TOS11000031')
variabili=c('prec','anemo','igro','temp')

l=list(NULL)
j=1
for(i in centraline){
    for(k in variabili){
      path=paste0(file.path,paste0(paste(k,i,sep='_'),'.csv'))
      datim=read.table(path, header = T, sep=';', dec=',', na.strings = "",skip = 18)
      datim$gg.mm.aaaa<-as.Date(as.character(datim$gg.mm.aaaa),format='%d/%m/%Y')
      if(k=='temp'){
        datim$Min..Â.C.<-as.numeric(datim$Min..Â.C.)
        datim$Max..Â.C.<-as.numeric(datim$Max..Â.C.)
      }
      l[[j]]=datim[datim$gg.mm.aaaa>='2020-01-01' & datim$gg.mm.aaaa<='2023-12-31',]
      names(l)[j]=paste(k,i,sep='_')
      j=j+1
    }
  }

nomi.var=c('date','precipitazioni.mm','vel_med_ms',
           'vel_max_ms','umi_med','umi_min','umi_max','temp_max','temp_min',
           'temp_med','diff_temp')

meteo.FI=data.frame(l[[1]][-3],l[[2]][-c(1,3)],l[[3]][-1],l[[4]][-1])
ind=which(colSums(is.na(meteo.FI))!=0)
if(sum(ind)!=0){meteo.FI[,ind]=apply(meteo.FI[,ind],2,na.approx)}
meteo.FI[,length(meteo.FI)+1]=meteo.FI[,8]*0.5+meteo.FI[,9]*0.5
meteo.FI[,length(meteo.FI)+1]=meteo.FI[,8]-meteo.FI[,9]
names(meteo.FI)<-nomi.var

meteo.PO=data.frame(l[[5]][-3],l[[6]][-c(1,3)],l[[7]][-1],l[[8]][-1])
ind=which(colSums(is.na(meteo.PO))!=0)
if(sum(ind)!=0){meteo.PO[,ind]=apply(meteo.PO[,ind],2,na.approx)}
meteo.PO[,length(meteo.PO)+1]=meteo.PO[,8]*0.5+meteo.PO[,9]*0.5
meteo.PO[,length(meteo.PO)+1]=meteo.PO[,8]-meteo.PO[,9]
names(meteo.PO)<-nomi.var

meteo.PI=data.frame(l[[9]][-3],l[[10]][-c(1,3)],l[[11]][-1],l[[12]][-1])
ind=which(colSums(is.na(meteo.PI))!=0)
if(sum(ind)!=0){meteo.PI[,ind]=apply(meteo.PI[,ind],2,na.approx)}
meteo.PI[,length(meteo.PI)+1]=meteo.PI[,8]*0.5+meteo.PI[,9]*0.5
meteo.PI[,length(meteo.PI)+1]=meteo.PI[,8]-meteo.PI[,9]
names(meteo.PI)<-nomi.var

meteo.LI=data.frame(l[[13]][-3],l[[14]][-c(1,3)],l[[15]][-1],l[[16]][-1])
ind=which(colSums(is.na(meteo.LI))!=0)
if(sum(ind)!=0){meteo.LI[,ind]=apply(meteo.LI[,ind],2,na.approx)}
meteo.LI[,length(meteo.LI)+1]=meteo.LI[,8]*0.5+meteo.LI[,9]*0.5
meteo.LI[,length(meteo.LI)+1]=meteo.LI[,8]-meteo.LI[,9]
names(meteo.LI)<-nomi.var

meteo.AR=data.frame(l[[17]][-3],l[[18]][-c(1,3)],l[[19]][-1],l[[20]][-1])
ind=which(colSums(is.na(meteo.AR))!=0)
if(sum(ind)!=0){meteo.AR[,ind]=apply(meteo.AR[,ind],2,na.approx)}
meteo.AR[,length(meteo.AR)+1]=meteo.AR[,8]*0.5+meteo.AR[,9]*0.5
meteo.AR[,length(meteo.AR)+1]=meteo.AR[,8]-meteo.AR[,9]
names(meteo.AR)<-nomi.var

meteo.LU=data.frame(l[[21]][-3],l[[22]][-c(1,3)],l[[23]][-1],l[[24]][-1])
ind=which(colSums(is.na(meteo.LU))!=0)
if(sum(ind)!=0){meteo.LU[,ind]=apply(meteo.LU[,ind],2,na.approx)}
meteo.LU[,length(meteo.LU)+1]=meteo.LU[,8]*0.5+meteo.LU[,9]*0.5
meteo.LU[,length(meteo.LU)+1]=meteo.LU[,8]-meteo.LU[,9]
names(meteo.LU)<-nomi.var

rm(datim,l,dati,i,j,k,nomi.var,ind,anni,centraline,file.path,path,stazioni,variabili)
