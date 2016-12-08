#kernel two sample test.
rkimu <- function(lambda,size,gamma){
  kekka <- rep(0,size)
  for(i in 1:size){
    z <- rnorm(length(lambda),mean=0,sd=sqrt(1/(gamma*(1-gamma))))
    kekka[i] <- sum(lambda*(z^2-1/(gamma*(1-gamma))))
  }
  return(kekka)
}
kernelTST <- function(X,Y){
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  s <- sigest(rbind(X,Y),scaled=FALSE)[2]
  rbf <- rbfdot(sigma = s)
  nX <- dim(X)[1]
  nY <- dim(Y)[1]
  N <- nX+nY
  Kx <- kernelMatrix(x = X,kernel = rbf)
  Ky <- kernelMatrix(x = Y,kernel = rbf)
  
  #get test statistic
  diag(Kx)<-0;diag(Ky)<-0
  term1 <- sum(Kx)/(nX*(nX-1))
  term2 <- sum(Ky)/(nY*(nY-1))
  term3 <- 2*sum(kernelMatrix(x=X,y=Y,kernel=rbf))/(nX*nY)
  Ustatistic <- term1+term2-term3
  TestStatistic <- N*Ustatistic
  
  
  #get critical values
  one <- matrix(1,nX,1)
  H <- diag(nX) - 1/nX*(one%*%t(one))
  K <- kernelMatrix(x = X,kernel = rbf)
  centerd_K <- H%*%K%*%H
  lambda <- 1/nX*eigen(centerd_K)$values
  lambda_pos <- lambda[lambda>0.001]
  kimusample <- rkimu(lambda=lambda_pos,size=1000,gamma=1/2)
  criticalvalue <- sort(kimusample)[0.95*length(kimusample)]
  
  
  kimu.plot <- ggplot(data.frame(kimubunpu = kimusample),aes(kimubunpu))+geom_histogram(bins=30)+
    geom_vline(xintercept=criticalvalue,col='red',show.legend=TRUE)+
    geom_vline(xintercept=TestStatistic,linetype='dashed',show.legend=TRUE)
  
  
  outcome <- list(CriticalValue=criticalvalue,TestStatistic=TestStatistic,NullHypothesis='Rejected',kimu.plot)
  if (criticalvalue>TestStatistic){
    outcome$NullHypothesis = 'Accepted'
  }
  else{
    outcome$NullHypothesis = 'Rejected'
  }
  return(outcome)
}