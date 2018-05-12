soft.th=function(lambda,x){
  return(sign(x)*pmax(abs(x)-lambda,0))
}

warm.start=function(X,y,lambda.max=100,standardize=TRUE){
  p=ncol(X)
  n=nrow(X)
  X=as.matrix(X)
  for(j in 1:p){
    X[,j]=X[,j]-mean(X[,j])
  }
  y=as.vector(y)
  y=y-mean(y)
  if(standardize==TRUE){
    scale=array(dim=p)
    for(j in 1:p){
      scale[j]=sqrt(covar(X[,j],X[,j]))
      X[,j]=X[,j]/scale[j]
    }
  }
  dec=round(lambda.max/50)
  lambda.seq=seq(lambda.max,1,-dec)
  r=length(lambda.seq)
  coef.seq=matrix(nrow=r,ncol=p)
  k=0
  for(lambda in lambda.seq){
    k=k+1
    beta.old=beta
    eps=1
    while(eps>0.01){
      for(j in 1:p){
        r=y-X[,-j]%*%beta[-j]
        beta[j]=soft.th(lambda,covar(r,X[,j]))/covar(X[,j],X[,j])
      }
      eps=max(abs(beta-beta.old))
      beta.old=beta
    }
    if(standardize==TRUE){
      for(j in 1:p){
        beta[j]=beta[j]/scale[j]
      }
      for(j in 1:p){
        coef.seq[k,j]=beta[j]
      }
    }
  }
  return(coef.seq)
}

warm.start(X,y)