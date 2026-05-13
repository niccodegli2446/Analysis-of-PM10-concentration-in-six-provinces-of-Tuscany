###############################################################################
############################# MODEL FOR MISSING  ##############################
###############################################################################

root.analysis=function(model){
  fit <- model
  #### Compute roots
  root <- .arma.roots(fit = fit)
  
  par(mfrow=c(1,1))
  .circle(win = 2.5, main = "All roots")
  points(root$root.long$ar, col = "red")
  points(root$root.long$ma, col = "blue")
  
  par(mfrow = c(1,2), mar = c(4,4,4,0.5))
  .circle(win = 2.5, main = "Non seasonal roots")
  points(root$root$ar, col = "red")
  points(root$root$ma, col = "blue")
  .circle(win = 2.5, main = "Seasonal roots")
  points(root$root$sar, col = "red")
  points(root$root$sma, col = "blue")
}

res=function(model){
  fit <- model
  ## Useful quantities
  res1   <- residuals(fit)                          ## Residuals
  resst1 <- scale(res1)                             ## Standardized residuals
  
  main <- "residuals"
  x1 <- res1
  par(mfrow=c(1,1))
  plot(x1, type = "l", main = main, xlab = "", ylab = "")
  Acf(x = x1, type = "partial",     lag.max = 83, na.action = na.pass, main = main)
  Acf(x = x1, type = "correlation", lag.max = 83, na.action = na.pass, main = main)
  
  npar1 <- NROW(fit$coef)                       ## Number of parameters
  fitdf1 <- 0                                   ## If we want to remove np from df
  lag1  <- fitdf1 + c(1, 2, 5, 7, 8, 10, 14, 15, 20, 21, 22)      ## lag
  lb <- mapply(FUN = Box.test, lag = lag1,
               MoreArgs = list(x = x1, type = "Ljung-Box", fitdf = 0))[1:3, , drop = FALSE]
  rbind(lag = lag1, lb)
  
}

#### Dummies (Sun and December are the references)
date=data$DATE
dd <- .xreg.daily.dummies(date = date)
md <- .xreg.monthly.dummies(date = date)
perspl <- .xreg.perspl(time = date, knot = 5, degree = 3,
                       demean = TRUE)

################################ AR-REPUBBLICA ################################
###############################################################################

y=data$AR.REPUBBLICA
start <- as.numeric(c(format(date[1], "%Y"), format(date[1], "%m"), 
                      format(date[1], '%d')))
g.transf <- "log"        
y <- ts( data = log(y), start = start, frequency = 1)

######### REGRESSORI #########
#### effetti di calendario giornalieri
calendar=cbind(dd,md)
calendar=calendar[,-which(dimnames(calendar)[[2]]=='eh')]
calendar=calendar[,-which(dimnames(calendar)[[2]]=='lh')]
#calendar=calendar[,-which(dimnames(calendar)[[2]]=='sh')]
#### meteo: pioggia (in mm), vento (velocità media m/s), temperatura (media max e min) e umidità media
reg.meteo=cbind(rain=meteo.AR$precipitazioni.mm,wind=meteo.AR$vel_med_ms,
                temp=meteo.AR$temp_med, umid=meteo.AR$umi_med)
#### drift
drift <- cbind(drift = 1 : NROW(y))

######### ARIMA #########
#### No outliers
xreg <- cbind(calendar,reg.meteo)
fit <- Arima(y = y,
             order = c(1, 0, 0), seasonal = list(order = c(0, 0, 0), period=7),
             xreg = xreg, include.constant = T)
#.print.arima(x = fit)
####
fit.AR.1 <- fit # calendar con solo sh
#.loglik(fit = fit.AR.1, g = g.transf)
xreg.AR=xreg

#### Si outliers
# calendar=calendar[,-which(dimnames(calendar)[[2]]=='sh')]
# xreg <- cbind(calendar,reg.meteo)
# fit <- tso(y = y, xreg = xreg, cval = 6, delta = 0.7,
#            types = c("AO"),
#            maxit = 10, maxit.iloop = 100, maxit.oloop = 10,
#            tsmethod = "arima",
#            args.tsmethod = list(order = c(1, 0, 0), 
#                                 seasonal = list(order = c(0, 0, 0), period=7)))
# plot(fit)     ## Use .plot.tso(fit) if plot(fit) stops with an error 
# 
# ## Extract settings 
# settings <- .Arima.settings(fit = fit$fit)
# 
# ## External regressors 
# oeff <- outliers.effects(mo = fit$outliers, n = NROW(y), pars = coef(fit$fit), 
#                          weights = FALSE)
# xreg <- cbind(oeff,xreg)
# fit <- Arima(y = y,
#              order = settings$order, seasonal = list(order = c(0, 0, 0), period=7),
#              xreg = xreg, include.constant = settings$include.constant)
# .print.arima(x = fit)
# fit.AR.2 <- fit # calendar senza eh,sh e lh
# .loglik(fit = fit.AR.2, g = g.transf)
#############################

### Root analysis
#root.analysis(fit.AR.1)

### Diagnostics on residuals
#res(fit.AR.1)
# fit.AR.1 meglio
rm(settings,oeff,fit, calendar,xreg,y, drift)

################################ LI-CARDUCCI ##################################
###############################################################################

y=data$LI.CARDUCCI
start <- as.numeric(c(format(date[1], "%Y"), format(date[1], "%m"), 
                      format(date[1], '%d')))
g.transf <- "log"        
y <- ts( data = log(y), start = start, frequency = 1)

######### REGRESSORI #########
#### effetti di calendario giornalieri
calendar=cbind(dd,md)
#calendar=calendar[,-which(dimnames(calendar)[[2]]=='eh')]
#calendar=calendar[,-which(dimnames(calendar)[[2]]=='lh')]
#calendar=calendar[,-which(dimnames(calendar)[[2]]=='sh')]
#### meteo: pioggia (in mm), vento (velocità media m/s), temperatura (media max e min) 
reg.meteo=cbind(rain=meteo.LI$precipitazioni.mm,wind=meteo.LI$vel_med_ms,
                temp=meteo.LI$temp_med)
#### drift
drift <- cbind(drift = 1 : NROW(y))

######### ARIMA #########
#### No outliers
# xreg <- cbind(calendar,reg.meteo) 
# fit <- Arima(y = y,
#              order = c(1, 0, 1), seasonal = list(order = c(0, 0, 0), period=7),
#              xreg = xreg, include.constant = T)
# .print.arima(x = fit)
# ####
# fit.LI.1 <- fit # calendar con tutto
# .loglik(fit = fit.LI.1, g = g.transf)

#### Si outliers
calendar=calendar[,-which(dimnames(calendar)[[2]]=='eh')]
calendar=calendar[,-which(dimnames(calendar)[[2]]=='lh')]
calendar=calendar[,-which(dimnames(calendar)[[2]]=='sh')]
xreg <- cbind(calendar,reg.meteo) 
fit <- tso(y = y, xreg = xreg, cval = 5, delta = 0.7, 
           types = c("AO"), 
           maxit = 10, maxit.iloop = 100, maxit.oloop = 10,
           tsmethod = "arima", 
           args.tsmethod = list(order = c(1, 0, 1), 
                                seasonal = list(order = c(0, 0, 0), period=7)))
#fit
#plot(fit)             ## Use .plot.tso(fit) if plot(fit) stops with an error 
#### Copy
fit1.o <- fit
#### Refit
## Extract settings so as to avoid to copy manually order and seasonal
settings <- .Arima.settings(fit = fit$fit)
#settings
## External regressors of outliers
oeff <- outliers.effects(mo = fit$outliers, n = NROW(y), pars = coef(fit$fit), 
                         weights = FALSE)
xreg <- cbind(oeff,xreg) 
fit <- Arima(y = y,
             order = settings$order, seasonal = list(order = c(0, 0, 0), period=7),
             xreg = xreg, include.constant = settings$include.constant)
#.print.arima(x = fit)
fit.LI.2 <- fit # calendar senza eh,lh e sh
#.loglik(fit = fit.LI.2, g = g.transf)
xreg.LI=xreg
#############################

### Root analysis
#root.analysis(fit.LI.2)

### Diagnostics on residuals
#res(fit.LI.2)
# fit.LI.2 meglio
rm(settings,oeff,fit, calendar,xreg,y)

################################ PO-FERRUCCI ###################################
################################################################################

y=data$PO.FERRUCCI
start <- as.numeric(c(format(date[1], "%Y"), format(date[1], "%m"), 
                      format(date[1], '%d')))
g.transf <- "log"        
y <- ts( data = log(y), start = start, frequency = 1)

######### REGRESSORI #########
#### effetti di calendario giornalieri
calendar=cbind(dd,md)
calendar=calendar[,-which(dimnames(calendar)[[2]]=='eh')]
calendar=calendar[,-which(dimnames(calendar)[[2]]=='lh')]
calendar=calendar[,-which(dimnames(calendar)[[2]]=='sh')]
#### meteo: pioggia (in mm), vento (velocità media m/s), temperatura (media max e min) e umidità media
reg.meteo=cbind(rain=meteo.PO$precipitazioni.mm,wind=meteo.PO$vel_med_ms,
                temp=meteo.PO$temp_med, umid=meteo.PO$umi_med)
#### drift
drift <- cbind(drift = 1 : NROW(y))
#############################

######### ARIMA #########
#### No outliers
xreg <- cbind(calendar, reg.meteo)
fit <- Arima(y = y,
             order = c(1, 0, 0), seasonal = list(order = c(0, 0, 1), period=7),
             xreg = xreg, include.constant = T)
#.print.arima(x = fit)
####
fit.PO.1 <- fit # calendar senza eh, sh e lh. 
#.loglik(fit = fit.PO.1, g = g.transf)
xreg.PO=xreg

### Si outliers (il tso non converge)
# xreg <- cbind(calendar, xreg)
# fit <- tso(y = y, xreg = xreg, cval = 6, delta = 0.7,
#            types = c("AO"),
#           maxit = 10, maxit.iloop = 100, maxit.oloop = 10,
#           tsmethod = "arima",
#           args.tsmethod = list(order = c(1, 0, 0),
#                                seasonal = list(order = c(0, 0, 0), period=7)))
# fit
# plot(fit)               ## Use .plot.tso(fit) if plot(fit) stops with an error
# ### Copy
# fit1.o <- fit
# ### Refit
# # Extract settings so as to avoid to copy manually order and seasonal
# settings <- .Arima.settings(fit = fit$fit)
# settings
# # External regressors of outliers
# oeff <- outliers.effects(mo = fit$outliers, n = NROW(y), pars = coef(fit$fit),
#                         weights = FALSE)
# xreg <- cbind(oeff,xreg)
# fit <- Arima(y = y,
#             order = settings$order, seasonal = list(order = c(0, 0, 0), period=7),
#             xreg = xreg, include.constant = settings$include.constant)
# .print.arima(x = fit)
# fit.PO.2 <- fit
# .loglik(fit = fit.PO.2, g = g.transf)
#############################

### Root analysis
#root.analysis(fit.PO.1) 

### Diagnostics on residuals
#res(fit.PO.1)
# fit.PO.1
rm(settings,oeff,fit, calendar,xreg,y)

################################ FI-MOSSE ######################################
################################################################################

y=data$FI.MOSSE
start <- as.numeric(c(format(date[1], "%Y"), format(date[1], "%m"), 
                      format(date[1], '%d')))
g.transf <- "log"        
y <- ts( data = log(y), start = start, frequency = 1)

######### REGRESSORI #########
#### effetti di calendario giornalieri
calendar=cbind(dd,md)
#calendar=calendar[,-which(dimnames(calendar)[[2]]=='eh')]
#calendar=calendar[,-which(dimnames(calendar)[[2]]=='lh')]
#calendar=calendar[,-which(dimnames(calendar)[[2]]=='sh')]
#### meteo: pioggia (in mm), vento (velocità media m/s), temperatura (media max e min) 
reg.meteo=cbind(rain=meteo.FI$precipitazioni.mm,wind=meteo.FI$vel_med_ms,
                temp=meteo.FI$temp_med)
#### drift
drift <- cbind(drift = 1 : NROW(y))
#############################

######### ARIMA #########
#### No outliers
xreg <- cbind(calendar,reg.meteo)
fit <- Arima(y = y,
             order = c(1, 0, 0), seasonal = list(order = c(0, 0, 0), period=7),
             xreg = xreg, include.constant = T)
#.print.arima(x = fit)
####
fit.FI.1 <- fit # calendar con tutto
#.loglik(fit = fit.FI.1, g = g.transf)
xreg.FI=xreg

#### Si outliers
# xreg <- cbind(drift,calendar,reg.meteo)
# fit <- tso(y = y, xreg = xreg, cval = 5, delta = 0.7,
#            types = c("AO"),
#            maxit = 10, maxit.iloop = 100, maxit.oloop = 10,
#            tsmethod = "arima",
#            args.tsmethod = list(order = c(1, 0, 0), 
#                                 seasonal = list(order = c(0, 0, 0), period=7)))
# #fit
# plot(fit)               ## Use .plot.tso(fit) if plot(fit) stops with an error
# #### Copy
# fit1.o <- fit
# #### Refit
# ## Extract settings so as to avoid to copy manually order and seasonal
# settings <- .Arima.settings(fit = fit$fit)
# #settings
# ## External regressors of outliers
# oeff <- outliers.effects(mo = fit$outliers, n = NROW(y), pars = coef(fit$fit),
#                          weights = FALSE)
# xreg <- cbind(oeff,xreg)
# fit <- Arima(y = y,
#              order = settings$order, seasonal = list(order = c(0, 0, 0), period=7),
#              xreg = xreg, include.constant = settings$include.constant)
# .print.arima(x = fit)
# fit.FI.2 <- fit
# .loglik(fit = fit.FI.2, g = g.transf)
#############################

### Root analysis
#root.analysis(fit.FI.1)

### Diagnostics on residuals
#res(fit.FI.1)
# fit.FI.1 meglio
rm(settings,oeff,fit, calendar,xreg,y)

################################ PI-BORGHETTO ##################################
################################################################################

y=data$PI.BORGHETTO
start <- as.numeric(c(format(date[1], "%Y"), format(date[1], "%m"), 
                      format(date[1], '%d')))
g.transf <- "log"        
y <- ts( data = log(y), start = start, frequency = 1)

######### REGRESSORI #########
#### effetti di calendario giornalieri
calendar=cbind(dd,md)
calendar=calendar[,-which(dimnames(calendar)[[2]]=='eh')]
calendar=calendar[,-which(dimnames(calendar)[[2]]=='lh')]
calendar=calendar[,-which(dimnames(calendar)[[2]]=='sh')]
#### meteo: pioggia (in mm), vento (velocità media m/s), temperatura (media max e min) e umidità media
reg.meteo=cbind(rain=meteo.PI$precipitazioni.mm,wind=meteo.PI$vel_med_ms,
                temp=meteo.PI$temp_med,umid=meteo.PI$umi_med)
#### drift
drift <- cbind(drift = 1 : NROW(y))
#############################

######### ARIMA #########
#### No outliers
xreg <- cbind(calendar,reg.meteo)
fit <- Arima(y = y,
             order = c(1, 0, 0), seasonal = list(order = c(1, 0, 0), period=7),
             xreg = xreg, include.constant = T)
#.print.arima(x = fit)
####
fit.PI.1 <- fit #calendar senza eh, lh e sh
#.loglik(fit = fit.PI.1, g = g.transf)
xreg.PI=xreg

#### Si outliers (non ci sono anomalie)
# xreg <- cbind(calendar,reg.meteo)
# fit <- tso(y = y, xreg = xreg, cval = 5, delta = 0.7,
#            types = c("AO"),
#            maxit = 10, maxit.iloop = 100, maxit.oloop = 10,
#            tsmethod = "arima",
#            args.tsmethod = list(order = c(1, 0, 0), 
#                                 seasonal = list(order = c(1, 0, 0), period=7)))
# #fit
# plot(fit)               ## Use .plot.tso(fit) if plot(fit) stops with an error
# #### Copy
# fit1.o <- fit
# #### Refit
# ## Extract settings so as to avoid to copy manually order and seasonal
# settings <- .Arima.settings(fit = fit$fit)
# #settings
# ## External regressors of outliers
# oeff <- outliers.effects(mo = fit$outliers, n = NROW(y), pars = coef(fit$fit),
#                          weights = FALSE)
# xreg <- cbind(oeff,xreg)
# fit <- Arima(y = y,
#              order = settings$order, seasonal = list(order = c(1, 0, 0), period=7),
#              xreg = xreg, include.constant = settings$include.constant)
# .print.arima(x = fit)
# fit.PI.2 <- fit
# .loglik(fit = fit.PI.2,g = g.transf)
#############################

### Root analysis
#root.analysis(fit.PI.1)

### Diagnostics on residuals
#res(fit.PI.1)

# fit.PI.1 meglio
rm(settings,oeff,fit, calendar,xreg,y)

################################ LU-MICHELETTO #################################
################################################################################

y=data$LU.MICHELETTO
start <- as.numeric(c(format(date[1], "%Y"), format(date[1], "%m"), 
                      format(date[1], '%d')))
g.transf <- "log"        
y <- ts( data = log(y), start = start, frequency = 1)

######### REGRESSORI #########
#### effetti di calendario giornalieri
calendar=cbind(dd,md)
calendar=calendar[,-which(dimnames(calendar)[[2]]=='eh')]
calendar=calendar[,-which(dimnames(calendar)[[2]]=='lh')]
calendar=calendar[,-which(dimnames(calendar)[[2]]=='sh')]
#### meteo: pioggia (in mm), vento (velocità media m/s), temperatura (media max e min) e umidità media
reg.meteo=cbind(rain=meteo.PI$precipitazioni.mm,wind=meteo.PI$vel_med_ms,
                temp=meteo.PI$temp_med,umid=meteo.PI$umi_med)
#### drift
drift <- cbind(drift = 1 : NROW(y))
#############################

######### ARIMA #########
#### No outliers
# xreg <- cbind(calendar,reg.meteo)
# fit <- Arima(y = y,
#              order = c(1, 0, 0), seasonal = list(order = c(0, 0, 0), period=7),
#              xreg = xreg, include.constant = T)
# .print.arima(x = fit)
# ####
# fit.LU.1 <- fit # calendar senza eh,lh e sh
# .loglik(fit = fit.LU.1, g = g.transf)

#### Si outliers
xreg <- cbind(calendar,reg.meteo)
fit <- tso(y = y, xreg = xreg, cval = 5, delta = 0.7, 
           types = c("AO"), 
           maxit = 10, maxit.iloop = 100, maxit.oloop = 10,
           tsmethod = "arima", 
           args.tsmethod = list(order = c(3, 0, 1), 
                                seasonal = list(order = c(0, 0, 0), period=7)))
#fit
#plot(fit)               ## Use .plot.tso(fit) if plot(fit) stops with an error 
#### Copy
fit1.o <- fit
#### Refit
## Extract settings so as to avoid to copy manually order and seasonal
settings <- .Arima.settings(fit = fit$fit)
#settings
## External regressors of outliers
oeff <- outliers.effects(mo = fit$outliers, n = NROW(y), pars = coef(fit$fit), 
                         weights = FALSE)
xreg <- cbind(oeff,xreg)
fit <- Arima(y = y,
             order = settings$order, seasonal = list(order = c(0, 0, 0), period=7),
             xreg = xreg, include.constant = settings$include.constant)
#.print.arima(x = fit)
fit.LU.2 <- fit #calendar senza eh, sh e lh
#.loglik(fit = fit.LU.2,g = g.transf)
xreg.LU=xreg
#############################

### Root analysis
#root.analysis(fit.LU.2)

### Diagnostics on residuals
#res(fit.LU.2)
# fit.LU.2 meglio
rm(settings,oeff,fit, calendar,xreg,y,root.analysis,res, 
   fit1.o,g.transf,reg.meteo,start,drift)


###############################################################################
################################ FILL MISSING  ################################
###############################################################################

dati=data.frame(DATE=data$DATE)

# AREZZO
vet=data$AR.REPUBBLICA
ind=which(is.na(vet))
fitted=.predict(object = fit.AR.1, n.ahead=1, t=0,
                y = NULL, xreg = xreg.AR, fixed.n.ahead = TRUE)
vet[ind]<-exp(fitted$pred$mean[ind])
dati[,length(dati)+1]<-vet

# LIVORNO
vet=data$LI.CARDUCCI
ind=which(is.na(vet))
fitted=.predict(object = fit.LI.2, n.ahead=1, t=0,
                y = NULL, xreg = xreg.LI, fixed.n.ahead = TRUE)
vet[ind]<-exp(fitted$pred$mean[ind])
dati[,length(dati)+1]<-vet

# PRATO
vet=data$PO.FERRUCCI
ind=which(is.na(vet))
fitted=.predict(object = fit.PO.1, n.ahead=1, t=0,
                y = NULL, xreg = xreg.PO, fixed.n.ahead = TRUE)
vet[ind]<-exp(fitted$pred$mean[ind])
dati[,length(dati)+1]<-vet

# FIRENZE
vet=data$FI.MOSSE
ind=which(is.na(vet))
fitted=.predict(object = fit.FI.1, n.ahead=1, t=0,
              y = NULL, xreg = xreg.FI, fixed.n.ahead = TRUE)
vet[ind]<-exp(fitted$pred$mean[ind])
dati[,length(dati)+1]<-vet

# PISA
vet=data$PI.BORGHETTO
ind=which(is.na(vet))
fitted=.predict(object = fit.PI.1, n.ahead=1, t=0,
                y = NULL, xreg = xreg.PI, fixed.n.ahead = TRUE)
vet[ind]<-exp(fitted$pred$mean[ind])
dati[,length(dati)+1]<-vet

# LUCCA
vet=data$LU.MICHELETTO
ind=which(is.na(vet))
fitted=.predict(object = fit.LU.2, n.ahead=1, t=0,
                y = NULL, xreg = xreg.LU, fixed.n.ahead = TRUE)
vet[ind]<-exp(fitted$pred$mean[ind])
dati[,length(dati)+1]<-vet

nomi=c('DATE','AR.REPUBBLICA','LI.CARDUCCI','PO.FERRUCCI','FI.MOSSE',
       'PI.BORGHETTO','LU.MICHELETTO')
names(dati)<-nomi

#write.table(dati,'/Users/niccolodeglinnocenti/Desktop/TESI/PM10.csv', sep=';', dec='.')

rm(xreg.AR,xreg.LU,xreg.PI,xreg.FI,xreg.PO,xreg.LI)
