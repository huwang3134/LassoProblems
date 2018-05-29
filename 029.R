linear=function(X,y){
  n=nrow(X)
  p=ncol(X)
  X=as.matrix(X)
  X.bar=array(dim=p)
  for(j in 1:p){
    X.bar[j]=mean(X[,j])
  }
  for(j in 1:p){
    X[,j]=X[,j]-X.bar[j]
  }
  y=as.vector(y)
  y.bar=mean(y)
  y=y-y.bar
  beta=as.vector(solve(t(X)%*%X)%*%t(X)%*%y)
  beta.0=y.bar-sum(X.bar*beta)
  return(list(beta=beta,beta.0=beta.0))
}

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
  lambda.seq=seq(200,0,-10)
  r=length(lambda.seq)
  coef.seq=matrix(nrow=r,ncol=p)
  L1.seq=NULL
  beta=array(0,dim=p)
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
    }
    for(j in 1:p){
      coef.seq[k,j]=beta[j]#???
      L1.seq=c(L1.seq,sum(abs(beta)))
    }
  }
  return(L1.seq)
}

crime=read.table("crime.txt")
X=crime[,3:7]
y=crime[,1]

warm.start(X,y)

L1=sum(abs(linear(X,y)$beta))
L1
beta
