rm(list=ls())
dev.off()
#### Caricamento dati, modelli per sostituzione missing e sostituzione missing
source("/Users/niccolodeglinnocenti/Desktop/Università/TESI/data_load.R")
source("/Users/niccolodeglinnocenti/Desktop/Università/TESI/fill_missing.R")

#### Caricamento dati no missing
#dati=read.table('/Users/niccolodeglinnocenti/Desktop/Università/TESI/PM10.csv',
#               header=T, sep=';', dec='.')
dati$DATE=as.Date(as.character(dati$DATE), format = "%Y-%m-%d")

####
colSums(is.na(dati))

#dati
y=log(dati[-1])

#regressori
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

.print.SUR=function(model){
  mat=matrix(nrow=29,ncol=6,0)
  dimnames(mat)=list(c("(Intercept)","AR.REPUBBLICA.l1", "LI.CARDUCCI.l1" , "PO.FERRUCCI.l1" ,
                       "FI.MOSSE.l1" , "PI.BORGHETTO.l1" , "LU.MICHELETTO.l1" ,
                       
                       "AR.REPUBBLICA.l2" , "LI.CARDUCCI.l2", "PO.FERRUCCI.l2" , 
                       "FI.MOSSE.l2" , "PI.BORGHETTO.l2" , "LU.MICHELETTO.l2",
                       
                       "rain", "wind",  "umi",  "temp", "rangeT", "fav",
                       
                       "Mon" , "Tue" , "Wed" , "Thu" , "Fri" , "Sat" , 
                       "pspl.1" , "pspl.2" , "pspl.3" , "pspl.4" ),
                     c('AR','LI','PO','FI','PI','LU'))
  
  coeff=summary(model)$coeff
  for(j in c('AR','LI','PO','FI','PI','LU')){
    for(k in c("rain", "wind",  "umi",  "temp", "rangeT", "fav")){
      dimnames(coeff)[[1]][dimnames(coeff)[[1]]==paste(paste(j,k,sep='_'),j, sep='.')]<-
        sub("\\..*", "", paste(paste(j,k,sep='_'),j, sep='.'))
    }
  }
  star=symnum(coeff[,4], corr = FALSE,
              cutpoints = c(0,  .001,.01,.05, .1, 1),
              symbols = c("***","**","*","."," "))
  list=strsplit(dimnames(coeff)[[1]],'_')
  for(i in 1:length(list)){
    col=list[[i]][1]
    row=list[[i]][2]
    mat[row,col]<-sprintf("%.4f%s", coeff[paste(col,row,sep='_'),][1], 
                          star[paste(col,row,sep='_')])
  }
  mat
}

################################################################################

#plot, acf e pacf
ylim=range(dati[,2], dati[,3], dati[,4], dati[,5], dati[,6], na.rm = T)
plot(dati$DATE, dati[,2], type='l', ylim=ylim, xlab='DATE', 
     ylab='PM10', main=names(dati)[2])
abline(h=50, col='red')
Acf(dati$AR.REPUBBLICA,lag.max = 83, na.action = na.pass,main=names(dati)[2])
Acf(dati$AR.REPUBBLICA,type='partial',lag.max = 83, 
    na.action = na.pass, main=names(dati)[2])
plot(dati$DATE, dati[,3], type='l', ylim=ylim, xlab='DATE', 
     ylab='PM10', main=names(dati)[3])
abline(h=50, col='red')
Acf(dati$LI.CARDUCCI,lag.max = 83, na.action = na.pass,main=names(dati)[3])
Acf(dati$LI.CARDUCCI,type='partial',lag.max = 83, 
    na.action = na.pass, main=names(dati)[3])
plot(dati$DATE, dati[,4], type='l', ylim=ylim, xlab='DATE', 
     ylab='PM10', main=names(dati)[4])
abline(h=50, col='red')
Acf(dati$PO.FERRUCCI,lag.max = 83, na.action = na.pass,main=names(dati)[4])
Acf(dati$PO.FERRUCCI,type='partial',lag.max = 83,
    na.action = na.pass, main=names(dati)[4])
plot(dati$DATE, dati[,5], type='l', ylim=ylim, xlab='DATE', 
     ylab='PM10', main=names(dati)[5])
abline(h=50, col='red')
Acf(dati$FI.MOSSE,lag.max = 83, na.action = na.pass,main=names(dati)[5])
Acf(dati$FI.MOSSE,type='partial',lag.max = 83, 
    na.action = na.pass, main=names(dati)[5])
plot(dati$DATE, dati[,6], type='l', ylim=ylim, xlab='DATE', 
     ylab='PM10', main=names(dati)[6])
abline(h=50, col='red')
Acf(dati$PI.BORGHETTO,lag.max = 83, na.action = na.pass, main=names(dati)[6])
Acf(dati$PI.BORGHETTO,type='partial',lag.max = 83, 
    na.action = na.pass, main=names(dati)[6])
plot(dati$DATE, dati[,7], type='l', ylim=ylim, xlab='DATE', 
     ylab='PM10', main=names(dati)[7])
abline(h=50, col='red')
Acf(dati$LU.MICHELETTO,lag.max = 83, na.action = na.pass, main=names(dati)[7])
Acf(dati$LU.MICHELETTO,type='partial',lag.max = 83, 
    na.action = na.pass, main=names(dati)[7])

#analisi preliminari
# unit root test
adf.AR <- ur.df(y = y[,1], type = "trend", lags = 21, selectlags = "AIC")
adf.LI <- ur.df(y = y[,2], type = "trend", lags = 21, selectlags = "AIC")
adf.PO <- ur.df(y = y[,3], type = "trend", lags = 21, selectlags = "AIC")
adf.FI <- ur.df(y = y[,4], type = "trend", lags = 21, selectlags = "AIC")
adf.PI <- ur.df(y = y[,5], type = "trend", lags = 21, selectlags = "AIC")
adf.LU <- ur.df(y = y[,6], type = "trend", lags = 21, selectlags = "AIC")
ADF=data.frame(t(adf.AR@teststat),t(adf.LI@teststat),t(adf.PO@teststat),
               t(adf.FI@teststat),t(adf.PI@teststat),t(adf.LU@teststat), adf.AR@cval)
names(ADF)=c(names(y)[1],names(y)[2],names(y)[3],names(y)[4],
             names(y)[5],names(y)[6], "1pct",  "5pct", "10pct")
ADF

adf.AR <- ur.df(y = y[,1], type = "drift", lags = 21, selectlags = "AIC")
adf.LI <- ur.df(y = y[,2], type = "drift", lags = 21, selectlags = "AIC")
adf.PO <- ur.df(y = y[,3], type = "drift", lags = 21, selectlags = "AIC")
adf.FI <- ur.df(y = y[,4], type = "drift", lags = 21, selectlags = "AIC")
adf.PI <- ur.df(y = y[,5], type = "drift", lags = 21, selectlags = "AIC")
adf.LU <- ur.df(y = y[,6], type = "drift", lags = 21, selectlags = "AIC")
ADFdrift=data.frame(t(adf.AR@teststat),t(adf.LI@teststat),t(adf.PO@teststat),
                    t(adf.FI@teststat),t(adf.PI@teststat),t(adf.LU@teststat), adf.AR@cval)
names(ADFdrift)=c(names(y)[1],names(y)[2],names(y)[3],names(y)[4],
                  names(y)[5],names(y)[6], "1pct",  "5pct", "10pct")
ADFdrift

adf.AR <- ur.df(y = y[,1], type = "none", lags = 21, selectlags = "AIC")
adf.LI <- ur.df(y = y[,2], type = "none", lags = 21, selectlags = "AIC")
adf.PO <- ur.df(y = y[,3], type = "none", lags = 21, selectlags = "AIC")
adf.FI <- ur.df(y = y[,4], type = "none", lags = 21, selectlags = "AIC")
adf.PI <- ur.df(y = y[,5], type = "none", lags = 21, selectlags = "AIC")
adf.LU <- ur.df(y = y[,6], type = "none", lags = 21, selectlags = "AIC")
ADFnone=data.frame(t(adf.AR@teststat),t(adf.LI@teststat),t(adf.PO@teststat),
                   t(adf.FI@teststat),t(adf.PI@teststat),t(adf.LU@teststat), adf.AR@cval)
names(ADFnone)=c(names(y)[1],names(y)[2],names(y)[3],names(y)[4],
                 names(y)[5],names(y)[6], "1pct",  "5pct", "10pct")
ADFnone
#assenza di radici unitarie --> serie tutte stazionarie

acf.joint=Acf(dati[,-1], lag.max = 35, plot=FALSE)
plot(acf.joint, xlab='', ylab='', mar =  c(1.3, 1.4, 2, 0.3), 
     oma =  c(0.8, 1.2, 1, 1),  mgp =  c(1, 0.6, 0))

###################################################################################
##################################### VAR #########################################
###################################################################################

# joint ACF
# par(mfrow=c(3,3))
# ccf(x, y) stima la correlazione tra x[t+k] and y[t]
# per k>0 => y prima di x => y guida x (la seconda guida la prima)
# per k<0 => x prima di y => x guida y (la prima guida la seconda)
# ccf(y[,1],y[,2], lag.max = 35, main='AR - LI')
# ccf(y[,1],y[,3], lag.max = 35, main='AR - PO')
# ccf(y[,1],y[,4], lag.max = 35, main='AR - FI')
# ccf(y[,1],y[,5], lag.max = 35, main='AR - PI')
# ccf(y[,1],y[,6], lag.max = 35, main='AR - LU')
# ccf(y[,2],y[,3], lag.max = 35, main='LI - PO')
# par(mfrow=c(3,3))
# ccf(y[,2],y[,4], lag.max = 35, main='LI - FI')
# ccf(y[,2],y[,5], lag.max = 35, main='LI - PI')
# ccf(y[,2],y[,6], lag.max = 35, main='LI - LU')
# ccf(y[,3],y[,4], lag.max = 35, main='PO - FI')
# ccf(y[,3],y[,5], lag.max = 35, main='PO - PI')
# ccf(y[,3],y[,6], lag.max = 35, main='PO - LU')
# ccf(y[,4],y[,5], lag.max = 35, main='FI - PI')
# ccf(y[,4],y[,6], lag.max = 35, main='FI - LU')
# ccf(y[,5],y[,6], lag.max = 35, main='PI - LU')


dd <- .xreg.daily.dummies(date = dati$DATE)
md <- .xreg.monthly.dummies(date = dati$DATE)
perspl <- .xreg.perspl(time = dati$DATE, knot = 5, degree = 3,
                       demean = TRUE)

dd=dd[,-which(colnames(dd)=='lh')]
dd=dd[,-which(colnames(dd)=='eh')]
dd=dd[,-which(colnames(dd)=='sh')]

# 12 dummy mensili
dummy=cbind(dd,md)
xreg=cbind(dummy,meteo) 
VARselect(y, lag.max = 10, type = "const",
          season = 7, exogen = xreg)

fit1=VAR(y = y, p = 2, type = 'const', exogen = xreg)

summary(fit1)
AIC(fit1)

res=residuals(fit1)
Acf(res, lag.max = 35)
#plot(fit1)

# spline
dummy=cbind(dd,perspl)
xreg=cbind(dummy,meteo) 
VARselect(y, lag.max = 10, type = "const",
          season = 7, exogen = xreg)

fit2=VAR(y = y, p = 2, type = 'const', exogen = xreg)

summary(fit2)
AIC(fit2)

res=residuals(fit2)
Acf(res, lag.max = 35)
#plot(fit2)

# FUNZIONI DI IMPULSO RISPOSTA
# ortogonali
irf=irf(fit2, impulse = NULL, response = NULL, n.ahead = 7, ortho = TRUE, 
        cumulative = FALSE, boot = TRUE, ci = 0.95, runs = 100, seed = NULL)
plot(irf)

# generalizzate
girf=girf.varest(fit2, impulse=NULL, response=NULL, n.ahead=7, ortho=FALSE, 
                 cumulative=FALSE, boot=TRUE, ci=0.95, runs=100, seed=NULL)
plot(girf)

# GIR=psGIRF(fit2,n.ahead = 7, orthog = FALSE, cumulative = FALSE)
# plot.gir(GIR, write=F)

###################################################################################
##################################### SUR #########################################
###################################################################################

.lag=function(x,k){
  mean=mean(x)
  c(rep(mean,k),x[1:(NROW(x)-k)]) 
}

Y=y
Y[,length(Y)+1]<-.lag(Y$AR.REPUBBLICA,1)
Y[,length(Y)+1]<-.lag(Y$LI.CARDUCCI,1)
Y[,length(Y)+1]<-.lag(Y$PO.FERRUCCI,1)
Y[,length(Y)+1]<-.lag(Y$FI.MOSSE,1)
Y[,length(Y)+1]<-.lag(Y$PI.BORGHETTO,1)
Y[,length(Y)+1]<-.lag(Y$LU.MICHELETTO,1)

Y[,length(Y)+1]<-.lag(Y$AR.REPUBBLICA,2)
Y[,length(Y)+1]<-.lag(Y$LI.CARDUCCI,2)
Y[,length(Y)+1]<-.lag(Y$PO.FERRUCCI,2)
Y[,length(Y)+1]<-.lag(Y$FI.MOSSE,2)
Y[,length(Y)+1]<-.lag(Y$PI.BORGHETTO,2)
Y[,length(Y)+1]<-.lag(Y$LU.MICHELETTO,2)

# Y[,length(Y)+1]<-.lag(Y$AR.REPUBBLICA,3)
# Y[,length(Y)+1]<-.lag(Y$LI.CARDUCCI,3)
# Y[,length(Y)+1]<-.lag(Y$PO.FERRUCCI,3)
# Y[,length(Y)+1]<-.lag(Y$FI.MOSSE,3)
# Y[,length(Y)+1]<-.lag(Y$PI.BORGHETTO,3)
# Y[,length(Y)+1]<-.lag(Y$LU.MICHELETTO,3)

names(Y)[7:length(Y)]<- 
  c("AR.REPUBBLICA.l1", "LI.CARDUCCI.l1" , "PO.FERRUCCI.l1" ,
    "FI.MOSSE.l1" , "PI.BORGHETTO.l1" , "LU.MICHELETTO.l1" ,
    
     "AR.REPUBBLICA.l2" , "LI.CARDUCCI.l2", "PO.FERRUCCI.l2" , 
     "FI.MOSSE.l2" , "PI.BORGHETTO.l2" , "LU.MICHELETTO.l2")
    # 
    # "AR.REPUBBLICA.l3" , "LI.CARDUCCI.l3", "PO.FERRUCCI.l3" , 
    # "FI.MOSSE.l3" , "PI.BORGHETTO.l3" , "LU.MICHELETTO.l3")

#dummy settimanali
dd <- .xreg.daily.dummies(date = dati$DATE)
md <- .xreg.monthly.dummies(date = dati$DATE)
perspl <- .xreg.perspl(time = dati$DATE, knot = 5, degree = 3,
                       demean = TRUE)

dd=dd[,-which(colnames(dd)=='lh')]
dd=dd[,-which(colnames(dd)=='eh')]
dd=dd[,-which(colnames(dd)=='sh')]

#giorni favorevoli e non
giorni.fav=function(citta){
  var=c('rain','umi','temp','wind')
  nomi.meteo=paste(var,citta,sep='.')
  indR=meteo[,nomi.meteo][,1]<=1
  indU=meteo[,nomi.meteo][,2]>=75
  indT=meteo[,nomi.meteo][,3]<=15
  indW=meteo[,nomi.meteo][,4]<=1
  IND=as.logical(indR*indT*indW*indU)
  IND
}

fav_no_fav=cbind(fav.AR=giorni.fav('AR')*1, fav.LI=giorni.fav('LI')*1, 
             fav.PO=giorni.fav('PO')*1, fav.FI=giorni.fav('FI')*1, 
             fav.PI=giorni.fav('PI')*1, fav.LU=giorni.fav('LU')*1)

Y=cbind(Y, const=rep(1,NROW(Y)), dd, perspl, meteo, fav_no_fav)

############################# modello completo ################################

eq1=AR.REPUBBLICA ~ AR.REPUBBLICA.l1 + AR.REPUBBLICA.l2 + LI.CARDUCCI.l1 + 
  LI.CARDUCCI.l2 + PO.FERRUCCI.l1 + PO.FERRUCCI.l2 + FI.MOSSE.l1 + FI.MOSSE.l2 + 
  PI.BORGHETTO.l1 + PI.BORGHETTO.l2 + LU.MICHELETTO.l1 +  LU.MICHELETTO.l2 +
  rain.AR + wind.AR + umi.AR + temp.AR + rangeT.AR + fav.AR +
  Mon + Tue + Wed + Thu + Fri + Sat  + 
  pspl.1 + pspl.2 + pspl.3 + pspl.4

eq2=LI.CARDUCCI ~ AR.REPUBBLICA.l1 + AR.REPUBBLICA.l2 + LI.CARDUCCI.l1 + 
  LI.CARDUCCI.l2 + PO.FERRUCCI.l1 + PO.FERRUCCI.l2 + FI.MOSSE.l1 + FI.MOSSE.l2 + 
  PI.BORGHETTO.l1 + PI.BORGHETTO.l2 + LU.MICHELETTO.l1 +  LU.MICHELETTO.l2 +
  rain.LI + wind.LI + umi.LI + temp.LI + rangeT.LI + fav.LI +
  Mon + Tue + Wed + Thu + Fri + Sat  + 
  pspl.1 + pspl.2 + pspl.3 + pspl.4

eq3=PO.FERRUCCI ~ AR.REPUBBLICA.l1 + AR.REPUBBLICA.l2 + LI.CARDUCCI.l1 + 
  LI.CARDUCCI.l2 + PO.FERRUCCI.l1 + PO.FERRUCCI.l2 + FI.MOSSE.l1 + FI.MOSSE.l2 + 
  PI.BORGHETTO.l1 + PI.BORGHETTO.l2 + LU.MICHELETTO.l1 +  LU.MICHELETTO.l2 +
  rain.PO + wind.PO + umi.PO + temp.PO + rangeT.PO + fav.PO +
  Mon + Tue + Wed + Thu + Fri + Sat  + 
  pspl.1 + pspl.2 + pspl.3 + pspl.4

eq4=FI.MOSSE ~ AR.REPUBBLICA.l1 + AR.REPUBBLICA.l2 + LI.CARDUCCI.l1 + 
  LI.CARDUCCI.l2 + PO.FERRUCCI.l1 + PO.FERRUCCI.l2 + FI.MOSSE.l1 + FI.MOSSE.l2 + 
  PI.BORGHETTO.l1 + PI.BORGHETTO.l2 + LU.MICHELETTO.l1 +  LU.MICHELETTO.l2 +
  rain.FI + wind.FI + umi.FI + temp.FI + rangeT.FI + fav.FI +
  Mon + Tue + Wed + Thu + Fri + Sat  + 
  pspl.1 + pspl.2 + pspl.3 + pspl.4

eq5=PI.BORGHETTO ~ AR.REPUBBLICA.l1 + AR.REPUBBLICA.l2 + LI.CARDUCCI.l1 + 
  LI.CARDUCCI.l2 + PO.FERRUCCI.l1 + PO.FERRUCCI.l2 + FI.MOSSE.l1 + FI.MOSSE.l2 + 
  PI.BORGHETTO.l1 + PI.BORGHETTO.l2 + LU.MICHELETTO.l1 +  LU.MICHELETTO.l2 +
  rain.PI + wind.PI + umi.PI + temp.PI + rangeT.PI + fav.PI +
  Mon + Tue + Wed + Thu + Fri + Sat  + 
  pspl.1 + pspl.2 + pspl.3 + pspl.4

eq6=LU.MICHELETTO ~ AR.REPUBBLICA.l1 + AR.REPUBBLICA.l2 + LI.CARDUCCI.l1 + 
  LI.CARDUCCI.l2 + PO.FERRUCCI.l1 + PO.FERRUCCI.l2 + FI.MOSSE.l1 + FI.MOSSE.l2 + 
  PI.BORGHETTO.l1 + PI.BORGHETTO.l2 + LU.MICHELETTO.l1 +  LU.MICHELETTO.l2 +
  rain.LU + wind.LU + umi.LU + temp.LU + rangeT.LU + fav.LU +
  Mon + Tue + Wed + Thu + Fri + Sat  + 
  pspl.1 + pspl.2 + pspl.3 + pspl.4

eq.system.complete=list(AR=eq1,LI=eq2,PO=eq3,FI=eq4,PI=eq5,LU=eq6)

fit.SUR.complete <- systemfit(eq.system.complete, method = "SUR", data = Y)
summary(fit.SUR.complete)
acf.res.complete=Acf(residuals(fit.SUR.complete), lag.max = 35, plot=FALSE)
plot(acf.res.complete, xlab='', ylab='', mar =  c(1.3, 1.4, 2, 0.3), 
     oma =  c(0.8, 1.2, 1, 1), 
     mgp =  c(1, 0.6, 0))

.print.SUR(fit.SUR.complete)

############################# selection model #################################

eq1=AR.REPUBBLICA ~ AR.REPUBBLICA.l1 + LI.CARDUCCI.l2 + PO.FERRUCCI.l2 +
  LU.MICHELETTO.l1 +  LU.MICHELETTO.l2 + 
  rain.AR + wind.AR + umi.AR + temp.AR + rangeT.AR + fav.AR +
  #rain.AR*umi.AR + rain.AR*temp.AR + 
  #umi.AR*wind.AR + temp.AR*wind.AR + umi.AR*temp.AR + rain.AR*wind.AR +
  Mon + Tue + Wed + Thu + Fri + Sat  + 
  pspl.1 + pspl.2 + pspl.3 + pspl.4 

eq2=LI.CARDUCCI ~ LI.CARDUCCI.l1  + FI.MOSSE.l1 +
  LU.MICHELETTO.l1 + 
  rain.LI + wind.LI + umi.LI + temp.LI + rangeT.LI + fav.LI +
  #rain.LI*umi.LI + rain.LI*temp.LI + 
  #umi.LI*wind.LI + temp.LI*wind.LI + umi.LI*temp.LI + rain.LI*wind.LI +
  Mon + Tue + Wed + Thu + Fri + Sat + 
  pspl.1 + pspl.2 + pspl.3 + pspl.4 

eq3=PO.FERRUCCI ~ AR.REPUBBLICA.l1 +  PO.FERRUCCI.l1 + 
  FI.MOSSE.l1 + PI.BORGHETTO.l1 + LU.MICHELETTO.l1 +
  AR.REPUBBLICA.l2 + 
  rain.PO + wind.PO + umi.PO + temp.PO + rangeT.PO + fav.PO +
  #rain.PO*umi.PO + rain.PO*temp.PO + 
  #umi.PO*wind.PO + temp.PO*wind.PO + umi.PO*temp.PO + rain.PO*wind.PO +
  Mon + Tue + Wed + Thu + Fri + Sat +
  pspl.1 + pspl.2 + pspl.3 + pspl.4 

eq4=FI.MOSSE ~ AR.REPUBBLICA.l1 +  PO.FERRUCCI.l1 +
  FI.MOSSE.l1 + PI.BORGHETTO.l1 + 
  AR.REPUBBLICA.l2  +  PO.FERRUCCI.l2 + PI.BORGHETTO.l2 +
  FI.MOSSE.l2 +  
  rain.FI + wind.FI + umi.FI + temp.FI + rangeT.FI + fav.FI +
  #rain.FI*umi.FI + rain.FI*temp.FI +  
  #umi.FI*wind.FI + temp.FI*wind.FI + umi.FI*temp.FI + rain.FI*wind.FI +
  Mon + Tue + Wed + Thu + Fri + Sat + 
  pspl.1 + pspl.2 + pspl.3 + pspl.4 

eq5=PI.BORGHETTO ~  LI.CARDUCCI.l1 + 
  FI.MOSSE.l1 + PI.BORGHETTO.l1 + LU.MICHELETTO.l1 +
  rain.PI + wind.PI + umi.PI + temp.PI + rangeT.PI + fav.PI +
  #rain.PI*umi.PI + rain.PI*temp.PI + 
  #umi.PI*wind.PI + temp.PI*wind.PI + umi.PI*temp.PI + rain.PI*wind.PI +
  Mon + Tue + Wed + Thu + Fri + Sat + 
  pspl.1 + pspl.2 + pspl.3 + pspl.4 

eq6=LU.MICHELETTO ~ AR.REPUBBLICA.l1+  PO.FERRUCCI.l1 +
  FI.MOSSE.l1 + LU.MICHELETTO.l1 + 
  AR.REPUBBLICA.l2  + 
  rain.LU + wind.LU + umi.LU + temp.LU + rangeT.LU + fav.LU +
  #rain.LU*umi.LU + rain.LU*temp.LU + 
  #umi.LU*wind.LU + temp.LU*wind.LU + umi.LU*temp.LU + rain.LU*wind.LU +
  Mon + Tue + Wed + Thu + Fri + Sat + 
  pspl.1 + pspl.2 + pspl.3 + pspl.4 

eq.system=list(AR=eq1,LI=eq2,PO=eq3,FI=eq4,PI=eq5,LU=eq6)
fit.SUR <- systemfit(eq.system, method = "SUR", data = Y)

summary(fit.SUR)
acf.res=Acf(residuals(fit.SUR), lag.max = 35, plot=FALSE)
plot(acf.res, xlab='', ylab='', mar =  c(1.3, 1.4, 2, 0.3), 
     oma =  c(0.8, 1.2, 1, 1), 
     mgp =  c(1, 0.6, 0))

.print.SUR(fit.SUR)



ar=linearHypothesis(fit.SUR, hypothesis.matrix = c('AR_pspl.1 = 0','AR_pspl.2 = 0',
                                                   'AR_pspl.3 = 0','AR_pspl.4 = 0'))
li=linearHypothesis(fit.SUR, hypothesis.matrix = c('LI_pspl.1 = 0','LI_pspl.2 = 0',
                                                   'LI_pspl.3 = 0','LI_pspl.4 = 0'))
po=linearHypothesis(fit.SUR, hypothesis.matrix = c('PO_pspl.1 = 0','PO_pspl.2 = 0',
                                                   'PO_pspl.3 = 0','PO_pspl.4 = 0'))
fi=linearHypothesis(fit.SUR, hypothesis.matrix = c('FI_pspl.1 = 0','FI_pspl.2 = 0',
                                                   'FI_pspl.3 = 0','FI_pspl.4 = 0'))
pi=linearHypothesis(fit.SUR, hypothesis.matrix = c('PI_pspl.1 = 0','PI_pspl.2 = 0',
                                                   'PI_pspl.3 = 0','PI_pspl.4 = 0'))
lu=linearHypothesis(fit.SUR, hypothesis.matrix = c('LU_pspl.1 = 0','LU_pspl.2 = 0',
                                                   'LU_pspl.3 = 0','LU_pspl.4 = 0'))

stars.Ftest=symnum(c(ar$'Pr(>F)'[2],li$'Pr(>F)'[2],po$'Pr(>F)'[2],
              fi$'Pr(>F)'[2],pi$'Pr(>F)'[2],lu$'Pr(>F)'[2]), corr = FALSE,
            cutpoints = c(0,  .001,.01,.05, .1, 1),
            symbols = c("***","**","*","."," "))
F.test=data.frame(provincia=c('AR','LI','PO','FI','PI','LU'), 
                  F.test=sprintf("%.4f%s", c(ar$F[2],li$F[2],po$F[2],
                                            fi$F[2],pi$F[2],lu$F[2]), 
                                stars.Ftest))
                  
F.test

#################### CONVERSIONE SUR IN OGGETTO varest ##########################
xreg=cbind(dd,perspl,meteo,fav_no_fav)
fit.var=VAR(y = y, p = 2, type = 'const', exogen = xreg)

nomi.meteo=names(meteo)
nomi.fav.nofav=dimnames(fav_no_fav)[[2]]
nomi.dummy.day=dimnames(dd)[[2]]
nomi.spline=dimnames(perspl)[[2]]
nomi.eq=names(eq.system)

fit.var$datamat<-Y[-c(1:2),]

fit.var$varresult$AR.REPUBBLICA$coefficients[
  c(nomi.meteo[nomi.meteo!=c("rain.AR","wind.AR","umi.AR","temp.AR","rangeT.AR")])]<-
  rep(0,25)
fit.var$varresult$AR.REPUBBLICA$coefficients[
  c(nomi.meteo[nomi.meteo==c("rain.AR","wind.AR","umi.AR","temp.AR","rangeT.AR")])]<-
  fit.SUR$coefficients[c('AR_rain.AR','AR_wind.AR','AR_umi.AR','AR_temp.AR','AR_rangeT.AR')]
  
fit.var$varresult$LI.CARDUCCI$coefficients[
  c(nomi.meteo[nomi.meteo!=c("rain.LI","wind.LI","umi.LI","temp.LI","rangeT.LI")])]<-
  rep(0,25)
fit.var$varresult$LI.CARDUCCI$coefficients[
  c(nomi.meteo[nomi.meteo==c("rain.LI","wind.LI","umi.LI","temp.LI","rangeT.LI")])]<-
  fit.SUR$coefficients[c('LI_rain.LI','LI_wind.LI','LI_umi.LI','LI_temp.LI','LI_rangeT.LI')]

fit.var$varresult$PO.FERRUCCI$coefficients[
  c(nomi.meteo[nomi.meteo!=c("rain.PO","wind.PO","umi.PO","temp.PO","rangeT.PO")])]<-
  rep(0,25)
fit.var$varresult$PO.FERRUCCI$coefficients[
  c(nomi.meteo[nomi.meteo==c("rain.PO","wind.PO","umi.PO","temp.PO","rangeT.PO")])]<-
  fit.SUR$coefficients[c('PO_rain.PO','PO_wind.PO','PO_umi.PO','PO_temp.PO','PO_rangeT.PO')]

fit.var$varresult$FI.MOSSE$coefficients[
  c(nomi.meteo[nomi.meteo!=c("rain.FI","wind.FI","umi.FI","temp.FI","rangeT.FI")])]<-
  rep(0,25)
fit.var$varresult$FI.MOSSE$coefficients[
  c(nomi.meteo[nomi.meteo==c("rain.FI","wind.FI","umi.FI","temp.FI","rangeT.FI")])]<-
  fit.SUR$coefficients[c('FI_rain.FI','FI_wind.FI','FI_umi.FI','FI_temp.FI','FI_rangeT.FI')]

fit.var$varresult$PI.BORGHETTO$coefficients[
  c(nomi.meteo[nomi.meteo!=c("rain.PI","wind.PI","umi.PI","temp.PI","rangeT.PI")])]<-
  rep(0,25)
fit.var$varresult$PI.BORGHETTO$coefficients[
  c(nomi.meteo[nomi.meteo==c("rain.PI","wind.PI","umi.PI","temp.PI","rangeT.PI")])]<-
  fit.SUR$coefficients[c('PI_rain.PI','PI_wind.PI','PI_umi.PI','PI_temp.PI','PI_rangeT.PI')]

fit.var$varresult$LU.MICHELETTO$coefficients[
  c(nomi.meteo[nomi.meteo!=c("rain.LU","wind.LU","umi.LU","temp.LU","rangeT.LU")])]<-
  rep(0,25)
fit.var$varresult$LU.MICHELETTO$coefficients[
  c(nomi.meteo[nomi.meteo==c("rain.LU","wind.LU","umi.LU","temp.LU","rangeT.LU")])]<-
  fit.SUR$coefficients[c('LU_rain.LU','LU_wind.LU','LU_umi.LU','LU_temp.LU','LU_rangeT.LU')]

fit.var$varresult$AR.REPUBBLICA$coefficients["const"]<-fit.SUR$coefficients["AR_(Intercept)"]
fit.var$varresult$LI.CARDUCCI$coefficients["const"]<-fit.SUR$coefficients["LI_(Intercept)"]
fit.var$varresult$PO.FERRUCCI$coefficients["const"]<-fit.SUR$coefficients["PO_(Intercept)"]
fit.var$varresult$FI.MOSSE$coefficients["const"]<-fit.SUR$coefficients["FI_(Intercept)"]
fit.var$varresult$PI.BORGHETTO$coefficients["const"]<-fit.SUR$coefficients["PI_(Intercept)"]
fit.var$varresult$LU.MICHELETTO$coefficients["const"]<-fit.SUR$coefficients["LU_(Intercept)"]

fit.var$varresult$AR.REPUBBLICA$coefficients[nomi.dummy.day]<-
  fit.SUR$coefficients[c("AR_Mon","AR_Tue","AR_Wed","AR_Thu","AR_Fri","AR_Sat")]
fit.var$varresult$LI.CARDUCCI$coefficients[nomi.dummy.day]<-
  fit.SUR$coefficients[c("LI_Mon","LI_Tue","LI_Wed","LI_Thu","LI_Fri","LI_Sat")]
fit.var$varresult$PO.FERRUCCI$coefficients[nomi.dummy.day]<-
  fit.SUR$coefficients[c("PO_Mon","PO_Tue","PO_Wed","PO_Thu","PO_Fri","PO_Sat")]
fit.var$varresult$FI.MOSSE$coefficients[nomi.dummy.day]<-
  fit.SUR$coefficients[c("FI_Mon","FI_Tue","FI_Wed","FI_Thu","FI_Fri","FI_Sat")]
fit.var$varresult$PI.BORGHETTO$coefficients[nomi.dummy.day]<-
  fit.SUR$coefficients[c("PI_Mon","PI_Tue","PI_Wed","PI_Thu","PI_Fri","PI_Sat")]
fit.var$varresult$LU.MICHELETTO$coefficients[nomi.dummy.day]<-
  fit.SUR$coefficients[c("LU_Mon","LU_Tue","LU_Wed","LU_Thu","LU_Fri","LU_Sat")]

fit.var$varresult$AR.REPUBBLICA$coefficients[nomi.spline]<-
  fit.SUR$coefficients[c("AR_pspl.1","AR_pspl.2","AR_pspl.3","AR_pspl.4")]
fit.var$varresult$LI.CARDUCCI$coefficients[nomi.spline]<-
  fit.SUR$coefficients[c("LI_pspl.1","LI_pspl.2","LI_pspl.3","LI_pspl.4")]
fit.var$varresult$PO.FERRUCCI$coefficients[nomi.spline]<-
  fit.SUR$coefficients[c("PO_pspl.1","PO_pspl.2","PO_pspl.3","PO_pspl.4")]
fit.var$varresult$FI.MOSSE$coefficients[nomi.spline]<-
  fit.SUR$coefficients[c("FI_pspl.1","FI_pspl.2","FI_pspl.3","FI_pspl.4")]
fit.var$varresult$PI.BORGHETTO$coefficients[nomi.spline]<-
  fit.SUR$coefficients[c("PI_pspl.1","PI_pspl.2","PI_pspl.3","PI_pspl.4")]
fit.var$varresult$LU.MICHELETTO$coefficients[nomi.spline]<-
  fit.SUR$coefficients[c("LU_pspl.1","LU_pspl.2","LU_pspl.3","LU_pspl.4")]

fit.var$varresult$AR.REPUBBLICA$coefficients[
  c(nomi.fav.nofav[nomi.fav.nofav!="fav.AR"])]<-rep(0,5)
fit.var$varresult$AR.REPUBBLICA$coefficients[
  c(nomi.fav.nofav[nomi.fav.nofav=="fav.AR"])]<-fit.SUR$coefficients["AR_fav.AR"]

fit.var$varresult$LI.CARDUCCI$coefficients[
  c(nomi.fav.nofav[nomi.fav.nofav!="fav.LI"])]<-rep(0,5)
fit.var$varresult$LI.CARDUCCI$coefficients[
  c(nomi.fav.nofav[nomi.fav.nofav=="fav.LI"])]<-fit.SUR$coefficients["LI_fav.LI"]

fit.var$varresult$PO.FERRUCCI$coefficients[
  c(nomi.fav.nofav[nomi.fav.nofav!="fav.PO"])]<-rep(0,5)
fit.var$varresult$PO.FERRUCCI$coefficients[
  c(nomi.fav.nofav[nomi.fav.nofav=="fav.PO"])]<-fit.SUR$coefficients["PO_fav.PO"]

fit.var$varresult$FI.MOSSE$coefficients[
  c(nomi.fav.nofav[nomi.fav.nofav!="fav.FI"])]<-rep(0,5)
fit.var$varresult$FI.MOSSE$coefficients[
  c(nomi.fav.nofav[nomi.fav.nofav=="fav.FI"])]<-fit.SUR$coefficients["FI_fav.FI"]

fit.var$varresult$PI.BORGHETTO$coefficients[
  c(nomi.fav.nofav[nomi.fav.nofav!="fav.PI"])]<-rep(0,5)
fit.var$varresult$PI.BORGHETTO$coefficients[
  c(nomi.fav.nofav[nomi.fav.nofav=="fav.PI"])]<-fit.SUR$coefficients["PI_fav.PI"]

fit.var$varresult$LU.MICHELETTO$coefficients[
  c(nomi.fav.nofav[nomi.fav.nofav!="fav.LU"])]<-rep(0,5)
fit.var$varresult$LU.MICHELETTO$coefficients[
  c(nomi.fav.nofav[nomi.fav.nofav=="fav.LU"])]<-fit.SUR$coefficients["LU_fav.LU"]



x=c(fit.SUR$eq[[1]]$coefficients[2],rep(0,4), fit.SUR$eq[[1]]$coefficients[5], 0,
    fit.SUR$eq[[1]]$coefficients[3],fit.SUR$eq[[1]]$coefficients[4],0,0,
    fit.SUR$eq[[1]]$coefficients[6])
fit.var$varresult$AR.REPUBBLICA$coefficients[1:12]<-x

x=c(0,fit.SUR$eq[[2]]$coefficients[2],0,fit.SUR$eq[[2]]$coefficients[3],0,
    fit.SUR$eq[[2]]$coefficients[4],rep(0,6))
fit.var$varresult$LI.CARDUCCI$coefficients[1:12]<-x


x=c(fit.SUR$eq[[3]]$coefficients[2],0,fit.SUR$eq[[3]]$coefficients[3],
    fit.SUR$eq[[3]]$coefficients[4],fit.SUR$eq[[3]]$coefficients[5],
    fit.SUR$eq[[3]]$coefficients[6],fit.SUR$eq[[3]]$coefficients[7],rep(0,5))
fit.var$varresult$PO.FERRUCCI$coefficients[1:12]<-x

x=c(fit.SUR$eq[[4]]$coefficients[2],0,fit.SUR$eq[[4]]$coefficients[3],
    fit.SUR$eq[[4]]$coefficients[4],fit.SUR$eq[[4]]$coefficients[5],
    0,fit.SUR$eq[[4]]$coefficients[6],0,fit.SUR$eq[[4]]$coefficients[7],
    fit.SUR$eq[[4]]$coefficients[9],fit.SUR$eq[[4]]$coefficients[8],0)
fit.var$varresult$FI.MOSSE$coefficients[1:12]<-x

x=c(0,fit.SUR$eq[[5]]$coefficients[2],0,fit.SUR$eq[[5]]$coefficients[3],
    fit.SUR$eq[[5]]$coefficients[4],fit.SUR$eq[[5]]$coefficients[5],rep(0,6))
fit.var$varresult$PI.BORGHETTO$coefficients[1:12]<-x

x=c(fit.SUR$eq[[6]]$coefficients[2],0,fit.SUR$eq[[6]]$coefficients[3],
    fit.SUR$eq[[6]]$coefficients[4],0,fit.SUR$eq[[6]]$coefficients[5],
    fit.SUR$eq[[6]]$coefficients[6], rep(0,5))
fit.var$varresult$LU.MICHELETTO$coefficients[1:12]<-x


fit.var$varresult$AR.REPUBBLICA$residuals<-fit.SUR$eq[[1]]$residuals
fit.var$varresult$LI.CARDUCCI$residuals<-fit.SUR$eq[[2]]$residuals
fit.var$varresult$PO.FERRUCCI$residuals<-fit.SUR$eq[[3]]$residuals
fit.var$varresult$FI.MOSSE$residuals<-fit.SUR$eq[[4]]$residuals
fit.var$varresult$PI.BORGHETTO$residuals<-fit.SUR$eq[[5]]$residuals
fit.var$varresult$LU.MICHELETTO$residuals<-fit.SUR$eq[[6]]$residuals

#####################FUNZIONI DI IMPULSO RISPOSTA #############################

# FUNZIONI DI IMPULSO RISPOSTA
# ortogonali
irf=girf.varest(fit.var, impulse=NULL, response=NULL, n.ahead=7, ortho=TRUE, 
                cumulative=FALSE, boot=TRUE, ci=0.95, runs=100, seed=NULL)
plot(irf)

# generalizzate
girf=girf.varest(fit.var, impulse=NULL, response=NULL, n.ahead=7, ortho=FALSE, 
                 cumulative=FALSE, boot=TRUE, ci=0.95, runs=1000, seed=NULL)
plot(girf)

#####################################################################
#####################################################################
#####################################################################

#log(y) vs spline
spline.vs.PM10=function(vet,name, date, spline.coeff, spline){
  x=date
  y=spline%*%spline.coeff
  y1=y-mean(y)+mean(vet)
  plot(x,vet, type='l', col='gray', ylab='', xlab='',
       main=name)
  legend('topright',legend=c(expression(log(PM10)), 'Spline'),
         col=c('gray','red'),lty=1,bty='n')
  lines(x,y1,col='red')
}

spline.vs.PM10(y[,1], 'AREZZO',
               date = dati$DATE,
               spline.coeff = fit.SUR$coefficients[c("AR_pspl.1","AR_pspl.2","AR_pspl.3","AR_pspl.4")],
               spline = perspl)

spline.vs.PM10(y[,2], 'LIVORNO',
               date = dati$DATE,
               spline.coeff = fit.SUR$coefficients[c("LI_pspl.1","LI_pspl.2","LI_pspl.3","LI_pspl.4")],
               spline = perspl)

spline.vs.PM10(y[,3], 'PRATO',
               date = dati$DATE,
               spline.coeff = fit.SUR$coefficients[c("PO_pspl.1","PO_pspl.2","PO_pspl.3","PO_pspl.4")],
               spline = perspl)

spline.vs.PM10(y[,4], 'FIRENZE',
               date = dati$DATE,
               spline.coeff = fit.SUR$coefficients[c("FI_pspl.1","FI_pspl.2","FI_pspl.3","FI_pspl.4")],
               spline = perspl)

spline.vs.PM10(y[,5], 'PISA',
               date = dati$DATE,
               spline.coeff = fit.SUR$coefficients[c("PI_pspl.1","PI_pspl.2","PI_pspl.3","PI_pspl.4")],
               spline = perspl)

spline.vs.PM10(y[,6], 'LUCCA',
               date = dati$DATE,
               spline.coeff = fit.SUR$coefficients[c("LU_pspl.1","LU_pspl.2","LU_pspl.3","LU_pspl.4")],
               spline = perspl)

#### spline per presentazione ####
ar.spline=as.vector(perspl%*%fit.SUR$coefficients[c("AR_pspl.1","AR_pspl.2","AR_pspl.3","AR_pspl.4")])[dati$DATE<="2020-12-31"]
li.spline=as.vector(perspl%*%fit.SUR$coefficients[c("LI_pspl.1","LI_pspl.2","LI_pspl.3","LI_pspl.4")])[dati$DATE<="2020-12-31"]
po.spline=as.vector(perspl%*%fit.SUR$coefficients[c("PO_pspl.1","PO_pspl.2","PO_pspl.3","PO_pspl.4")])[dati$DATE<="2020-12-31"]
fi.spline=as.vector(perspl%*%fit.SUR$coefficients[c("FI_pspl.1","FI_pspl.2","FI_pspl.3","FI_pspl.4")])[dati$DATE<="2020-12-31"]
pi.spline=as.vector(perspl%*%fit.SUR$coefficients[c("PI_pspl.1","PI_pspl.2","PI_pspl.3","PI_pspl.4")])[dati$DATE<="2020-12-31"]
lu.spline=as.vector(perspl%*%fit.SUR$coefficients[c("LU_pspl.1","LU_pspl.2","LU_pspl.3","LU_pspl.4")])[dati$DATE<="2020-12-31"]
ylim=range(ar.spline,li.spline,po.spline,fi.spline,pi.spline,lu.spline)
plot(dati$DATE[dati$DATE<="2020-12-31"], ar.spline, type='l', ylab='',
     xlab='', ylim=ylim, main='spline')
lines(dati$DATE[dati$DATE<="2020-12-31"], li.spline, col='red', ylim=ylim)
lines(dati$DATE[dati$DATE<="2020-12-31"], po.spline, col='green', ylim=ylim)
lines(dati$DATE[dati$DATE<="2020-12-31"], fi.spline, col='blue', ylim=ylim)
lines(dati$DATE[dati$DATE<="2020-12-31"], pi.spline, col='violet', ylim=ylim)
lines(dati$DATE[dati$DATE<="2020-12-31"], lu.spline, col='gray', ylim=ylim)
legend('top', legend = c('AR','LI', 'PO', 'FI', 'PI', 'LU'), 
       col = c('black','red','green','blue', 'violet','gray'),
       text.col =c('black','red','green','blue','violet','gray'),lty = 1)

#################### coeff. stimati dummy settimanali ##########################
giorni=c('Mon','Tue','Wed','Thu','Fri','Sat')

AR.coef=fit.SUR$coefficients[c('AR_Mon','AR_Tue','AR_Wed','AR_Thu','AR_Fri','AR_Sat')]
LI.coef=fit.SUR$coefficients[c('LI_Mon','LI_Tue','LI_Wed','LI_Thu','LI_Fri','LI_Sat')]
PO.coef=fit.SUR$coefficients[c('PO_Mon','PO_Tue','PO_Wed','PO_Thu','PO_Fri','PO_Sat')]
FI.coef=fit.SUR$coefficients[c('FI_Mon','FI_Tue','FI_Wed','FI_Thu','FI_Fri','FI_Sat')]
PI.coef=fit.SUR$coefficients[c('PI_Mon','PI_Tue','PI_Wed','PI_Thu','PI_Fri','PI_Sat')]
LU.coef=fit.SUR$coefficients[c('LU_Mon','LU_Tue','LU_Wed','LU_Thu','LU_Fri','LU_Sat')]

ylim=range(AR.coef,LI.coef,PO.coef,FI.coef,PI.coef,LU.coef)
plot(AR.coef, type='b', xaxt='n', ylim=ylim, xlab='giorni della settimana', 
     ylab='', main='dummy settimanali')
axis(1, at=1:6, labels=giorni)
lines(LI.coef, type='b', col='red')
lines(PO.coef, type='b', col='green')
lines(FI.coef, type='b', col='blue')
lines(PI.coef, type='b', col='violet')
lines(LU.coef, type='b', col='gray')

legend('topright', legend = c('AR','LI', 'PO', 'FI', 'PI', 'LU'), 
       col = c('black','red','green','blue', 'violet','gray'),
       text.col =c('black','red','green','blue','violet','gray'),lty = 1)

################## legame tra distanza province e correlazione ##################

###### distanza in km tra le varie centraline di PM10 delle varie città
citta=c('AR','LI','PO','FI','PI','LU')
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

cor_values=summary(fit.SUR)$residCor[upper.tri(summary(fit.SUR)$residCor)]
dist_values=matrice.distanze[upper.tri(matrice.distanze)]

# calcola la correlazione tra distanza e correlazione
# ipotesi nulla correlazione pari a 0
cor_test_result <- cor.test(dist_values, cor_values, method = "pearson")
cor_test_result

# visualizzazione grafica
plot(dist_values, cor_values, xlab = "Distanza [km]", ylab = "Correlazione",
     main = "",
     pch = 19, col = "blue")
abline(lm(cor_values ~ dist_values), col = "red")

#################################################################################
