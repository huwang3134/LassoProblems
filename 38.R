crime=read.table("crime.txt")
X=crime[,3:7]
y=crime[,1]

covar=function(x,y){
  return(sum(x*y)/length(x))
}

linear.lasso=function(X,y,lambda=0,standardize=TRUE,alpha=1){
  X=as.matrix(X)
  p=ncol(X)
  X.bar=array(dim=p)
  Scale=array(dim=p)
  for(j in 1:p){
    X.bar[j]=mean(X[,j])
    X[,j]=X[,j]-X.bar[j]
  }
  y.bar=mean(y)
  y=y-y.bar
  beta=array(0,dim=p)
  beta.old=array(0,dim=p)
  eps=1
  if(standardize==TRUE){
    for(j in 1:p){
      Scale[j]=sqrt(covar(X[,j],X[,j]))
      X[,j]=X[,j]/Scale[j]
    }
  }
  while(eps>0.001){
    for(j in 1:p){
      r=y-X[,-j]%*%beta[-j]
      beta[j]=soft.th(lambda*alpha,covar(r,X[,j]))/(covar(X[,j],X[,j])+lambda*(1-alpha))
    }
    eps=max(abs(beta-beta.old))
    beta.old=beta
  }
  if(standardize==TRUE){
    for(j in 1:p){
      beta[j]=beta[j]/Scale[j]
    }
  }
  beta.0=y.bar-X.bar[j]*beta[j]
  for(j in 1:p){
    beta.0=beta.0-X.bar[j]*beta[j]
    return(list(beta=beta,beta.0=beta.0))
  }
}

linear(X,y)
linear.lasso(X,y,alpha=0.5)