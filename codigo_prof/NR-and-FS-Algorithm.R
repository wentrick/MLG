
rm(list = ls())

## Log-likelihood function
lik.funct<- function(y,par){
  mu<-par[1]; 
  sig2<-par[2];
  n<- length(y);
  return(-(n/2)*log(2*pi*sig2)-(1/(2*sig2))*sum((y-mu)**2))
}

## Log-likelihood function
score.vec<- function(y,par){
  mu<-par[1];
  sig2<-par[2];
  n<- length(y)
  score<-double()
  ##
  partial.mu<- (1/sig2)*sum(y-mu)
  partial.sig2<- (-n/(2*sig2))+(1/(2*sig2**2))*sum((y-mu)**2)
  ##
  score[1]<-partial.mu
  score[2]<-partial.sig2
  ##
  return(score)
}

## Expected information matrix
expected.info<- function(y,par){
  mu<-par[1]
  sig2<-par[2]
  ##
  n<-length(y)
  info.mat<-matrix(NA,ncol=2,nrow=2)
  ##
  info.mat[2,1]<-info.mat[1,2]<-0
  info.mat[1,1]<-n/sig2
  info.mat[2,2]<-n/(2*sig2**2)
  ##
  return(info.mat)
}

##----------------------------------------##
## Fisher Scoring for Normal (mu, sigma2) ##
##----------------------------------------##
##
fs.normal<-function(y,par,max.iter=20,eps=1e-15){
  mu<-par[1]
  sig2<-par[2]
  ##
  logL.vec<-double()
  mu.vec<-double()
  sig2.vec<-double()
  tol.vec<-double()
  n<-length(y)
  ##
  par.vec<-matrix(NA,nrow = max.iter, ncol = 2)
  par.vec[1,]<-c(mu,sig2)
  contador<-1
  tol.vec[1]<-tol<-1
  par.anter<-par.vec[1,]
  logL.vec[1]<-lik.funct(y,par.anter)
  
  ##  
  while(TRUE){
    
    if(par.anter[2]>0){
      v<-solve(expected.info(y,par.anter),score.vec(y,par.anter))
      
      ## Etapa iterativa
      par.atual<- par.anter+v
    }
    else
      return(NA)
    
    ## Calculando a tolerância
    tol<- sum(abs(par.atual-par.anter)/abs(par.anter))
    ##tol<- abs(lik.funct(y,par.atual)-lik.funct(y,par.anter))
    contador<-contador+1
    
    ## Atualizando as quandidades 
    par.anter<- par.atual
    par.vec[contador,]<- par.atual
    tol.vec[contador]<-tol
    logL.vec[contador]<-lik.funct(y,par.atual)
    cat("Wait: the FS algorithm is running", "\r")
    
    if((eps>tol)|(contador>max.iter))break
    
  }
  res<-matrix(NA,nrow = contador,ncol = 4)
  colnames(res)<-c("mu.hat","sig2.hat","LogLik","Tolerance")
  res[,1]<-par.vec[1:contador,1]
  res[,2]<-par.vec[1:contador,2]
  res[,3]<-logL.vec
  res[,4]<-tol.vec
  
  ## Matriz de Variância-Covariância
  ## Cov.hats<-solve(expected.info(y,par.atual))
  
  return(list(res=res,par.atual=par.atual))
}

##------------------------------##
## Obtendo os resultados finais ##
##------------------------------##
data<-runif(50,0,2)
##
##score.vec(data,c(0,1))
##expected.info(data,c(0,1))
##
par0<-cbind(sample(-5:5,1),sample(0.01:5,1))
fs.normal(data,par0)
##
cbind(mean(data),sum((data-mean(data))**2)/(length(data)))
##
##
##
##
##
##
##
##----------------------------------##
## Fisher Scoring for Bernoulli (p) ##
##----------------------------------##

rm(list = ls())
##
lik.funct<- function(y,p){n<-length(y); return(sum(y)*log(p)+(n-sum(y))*log(1-p))}
score.vec<- function(y,p){n<-length(y); return(sum(y)/p-(n-sum(y))/(1-p))}
fisher.info<- function(y,p){n<-length(y); return(n/(p*(1-p)))}
##
##
##
fs.bernoulli<-function(y,p,max.iter=10,eps=1e-05){
  
  logL.vec<-double()
  p.vec<-double()
  tol.vec<-double()
  n<-length(y)
  ##
  p.vec[1]<-p.anter<-p
  logL.vec[1]<-lik.funct(y,p.anter)
  contador<-1
  tol.vec[1]<-tol<-1
  ##
  while(TRUE){
    
    if((p.anter>0)&(p.anter<1)){
      v<-solve(fisher.info(y,p.anter),score.vec(y,p.anter)) 
      
      ## Etapa iterativa
      p.atual<- p.anter+v
    }
    else
      return(NA)
    
    ## Calculando a tolerância
    ## tol<- abs(p.atual-p.anter)/p.anter
    tol<- abs(lik.funct(y,p.atual)-lik.funct(y,p.anter))
    ##
    contador<-contador+1
    
    ## Atualizando as quandidades 
    p.anter<- p.atual
    p.vec[contador]<-p.anter
    tol.vec[contador]<-tol
    logL.vec[contador]<-lik.funct(y,p.atual)
    cat("Wait: the FS algorithm is running", "\r")
    
    if((eps>tol)|(contador>max.iter))break
    
  }
  res<-matrix(NA,nrow = contador,ncol = 3)
  colnames(res)<-c("p.hat","LogLik","Tolerance")
  res[,1]<-p.vec
  res[,2]<-logL.vec
  res[,3]<-tol.vec
  ##
  var.phat<-solve(fisher.info(y,p.atual))
  
  return(list(res=res, var.phat=var.phat)) 
}

##------------------------------##
## Obtendo os resultados finais ##
##------------------------------##
data<-rbinom(50,1,0.5)
p0<- sample(0.01:0.1,1,replace=F)
##
fs.bernoulli(data,p0)
##
mean(data)
##
##
##
##
##
##
##
##-------------------------------##
## FS for the Poisson Regression ##
##-------------------------------##

rm(list = ls())
##--------------##
## Score vector ##
##--------------##
score.pois<- function(data,betas){
  
  y<-data$y                         # Response 
  X<-as.matrix(data[,2:ncol(data)]) # Design matrix
  ##
  betas<- as.matrix(betas)
  return(t(X)%*%(y-exp(X%*%betas)))
}


##---------------------------##
## Fisher information matrix ##
##---------------------------##

fisher.infopois<- function(data,betas){
  y<-data$y                    
  X<-as.matrix(data[,2:ncol(data)]) 
  ##
  betas<- as.matrix(betas)
  vector<- as.vector(exp(X%*%betas))
  G.betas<- diag(vector)
  ##
  return(t(X)%*%G.betas%*%X)
}


fs.poisReg<-function(data, betas,max.iter=15,eps=1e-08){
  
  n<-nrow(data)
  y<-data$y                    
  X<-as.matrix(data[,2:ncol(data)]) 
  betas<- as.matrix(betas)
  ##
  tol.vec<-double()
  betas.vec<- matrix(NA,nrow=max.iter, ncol=ncol(X))
  ##
  betas.vec[1,]<-betas.ant<-betas
  contador<-1
  tol.vec[1]<-tol<-1
  ##
  while(TRUE){
    
    v<-solve(fisher.infopois(data,betas.ant),score.pois(data,betas.ant)) 
    
    ## Iterative step (FS Algorithm)
    betas.atual<- betas.ant+v
    
    ## Computing the tolerance level
    tol<- sum(abs((betas.atual-betas.ant)))
    contador<-contador+1
    
    ## Updating the main results 
    betas.ant<- betas.atual
    betas.vec[contador,]<-betas.ant
    tol.vec[contador]<-tol
    cat("Wait: the FS algorithm is running", "\r")
    
    if((eps>tol)|(contador>max.iter))break
  }
  Results<-matrix(NA, nrow=contador, ncol=5)
  colnames(Results)<-c("b0","b1","b2","b3", "Tolerance")
  Results[1:contador,1:4]<- betas.vec[1:contador,]
  Results[1:contador,5]<- tol.vec
  ##
  Cov.Mat <- solve(fisher.infopois(data,betas.atual))
  se.betas<- sqrt(diag(Cov.Mat))
  ##
  return(list(Results=Results,Cov.Mat=Cov.Mat,se.betas=se.betas)) 
}

##----------------------##
## Artificial Data Sets ##
##----------------------##
n<-50
y<-rpois(n,4)
x0<-rep(1,n)
x1<-rnorm(n)
x2<-rbinom(n,1,0.5)
x3<-runif(n,2,4)
##
data.pois<-data.frame(y,x0,x1,x2,x3)

##------------------------------##
## Results FS and GLM R-package ##
##------------------------------##

##--------------##
## FS Algorithm ##
##--------------##
betas0<-rep(0,4)
obj<-fs.poisReg(data.pois,betas0)
obj$Results[10,]
obj$Cov.Mat

##---------------##
## GLM R-Package ##
##---------------##
fit.pois<-glm(y~x1+x2+x3, data = data.pois, family = poisson(link="log"))
summary(fit.pois)
