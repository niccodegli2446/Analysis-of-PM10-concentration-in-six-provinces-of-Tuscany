# funzioni per calcolo e plot girf senza intervalli di confidenza.
# no bootstrap

psGIRF=function(x, n.ahead = 20, cumulative = TRUE, orthog = FALSE){
  
  y.names <- colnames(x$y)
  impulse <- y.names
  response <- y.names
  
  # Ensure n.ahead is an integer
  n.ahead <- abs(as.integer(n.ahead))
  
  # Create arrays to hold calculations     
  # [1:nlags, 1:nvariables, shocked variable ] 
  
  IRF_o  = array(data = 0, dim = c(n.ahead,x$K,x$K),
                 dimnames = list(NULL,y.names,y.names))       
  IRF_g  = array(data = 0, dim = c(n.ahead,x$K,x$K),
                 dimnames = list(NULL,y.names,y.names))
  IRF_g1 = array(data = 0, dim = c(n.ahead,x$K,x$K))
  
  # Estimation of orthogonalised and generalised IRFs
  SpecMA <- Phi(x, n.ahead)
  params <- ncol(x$datamat[, -c(1:x$K)])
  sigma.u <- crossprod(resid(x))/(x$obs - params)
  P <- t(chol(sigma.u))
  sig_jj <- diag(sigma.u)
  
  for (jj in 1:x$K){
    indx_      <- matrix(0,x$K,1)
    indx_[jj,1] <- 1
    
    for (kk in 1:n.ahead){  #kk counts the lag
      
      IRF_o[kk, ,jj] <- SpecMA[, ,kk]%*%P%*%indx_  # Peseran-Shin eqn 7 (OIRF)
      
      IRF_g1[kk, ,jj] <- SpecMA[, ,kk]%*%sigma.u%*%indx_
      IRF_g[kk, ,jj] <- sig_jj[jj]^(-0.5)*IRF_g1[kk, ,jj]  # Peseran-Shin eqn 10 (GIRF)
      
    }
  }
  
  if(orthog==TRUE){
    irf <- IRF_o
  } else if(orthog==FALSE) {
    irf <- IRF_g
  } else {
    stop("\nError! Orthogonalised or generalised IRF?\n")
  }
  
  idx <- length(impulse)
  irs <- list()
  for (ii in 1:idx) {
    irs[[ii]] <- matrix(irf[1:(n.ahead), response, impulse[ii]], nrow = n.ahead)
    colnames(irs[[ii]]) <- response
    if (cumulative) {
      if (length(response) > 1) 
        irs[[ii]] <- apply(irs[[ii]], 2, cumsum)
      if (length(response) == 1) {
        tmp <- matrix(cumsum(irs[[ii]]))
        colnames(tmp) <- response
        irs[[ii]] <- tmp
      }
    }
  }
  names(irs) <- impulse
  result <- irs
  return(result)
  
}

plot.gir=function(object, write=FALSE){
  #ylim=c(min(unlist(lapply(object, range))),max(unlist(lapply(object, range))))
  for(i in 1:length(object)){
    if(write==T){pdf(paste0(paste0("GIR_", names(object)[i]),".pdf"), 
                     width = 14, height = 10)}
    par(mfrow=c(round(length(object)/2),2), mar = c(0, 4.5, 0, 4.5), oma = c(3, 0, 2.5, 0))
    m=object[[i]]
    ylim=range(m)
    for(j in 1:ncol(m)){
      if(j<ncol(m)-1){
        plot(m[,j], type='l', ylim=ylim, main='', xlab='', 
             ylab=names(object)[j], xaxt = "n")
        abline(h=0, col='red', lty=2)
      }
      if(j>=ncol(m)-1){
        plot(m[,j], type='l', ylim=ylim, main='', xlab='', ylab=names(object)[j])
        abline(h=0, col='red', lty=2)
      }
    }
    mtext(paste("Impulse from: ",names(object)[i]), outer = TRUE, cex = 1, 
          line = 1, font = 2)
    if(write==T){dev.off()}
  }
}

