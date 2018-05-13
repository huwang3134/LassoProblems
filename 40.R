soft.th=function(lambda,x){
  return(sign(x)*pmax(abs(x)-lambda,0))
}

warm.start=function(X,y,lambda.max=100,standardize=TRUE,alpha=1){
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
  beta=array(0,dim=p)
  k=0
  for(lambda in lambda.seq){
    k=k+1
    beta.old=beta
    eps=1
    while(eps>0.01){
      for(j in 1:p){
        r=y-X[,-j]%*%beta[-j]
        beta[j]=soft.th(lambda*alpha,covar(r,X[,j]))/(covar(X[,j],X[,j])+lambda*(1-alpha))
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
    }
  }
  return(coef.seq)
}

lambda.max=100
dec=round(lambda.max/50)
lambda.seq=seq(lambda.max,1,-dec)

crime=read.table("crime.txt")
X=crime[,3:7]
y=crime[,1]

par(mfrow=c(1,2))
#alpha=0.3
coef.seq=warm.start(X,y,300,alpha=0.3)
plot(log(lambda.seq),coef.seq[,1],
     xlab="log(lambda)",ylab="coeficient",ylim=c(min(coef.seq),max(coef.seq)))

for(j in 1:p){
  par(new)
  lines(log(lambda.seq),coef.seq[,j],col=j)
}

#alpha=1
coef.seq=warm.start(X,y,300)
plot(log(lambda.seq),coef.seq[,1],
     xlab="log(lambda)",ylab="coeficient",ylim=c(min(coef.seq),max(coef.seq)))

for(j in 1:p){
  par(new)
  lines(log(lambda.seq),coef.seq[,j],col=j)
}