crime=read.table("crime.txt")
X=crime[,3:7]
y=crime[,1]

ridge=function(X,y,lambda,standardize=TRUE){
  n=nrow(X)
  p=ncol(X)
  X=as.matrix(X)
  X.bar=array(dim=p)
  Scale=array(dim=p)
  for(j in 1:p){
    X.bar[j]=mean(X[,j])
  }
  for(j in 1:p){
    X[,j]=X[,j]-X.bar[j]
  }
  y=as.vector(y)
  y.bar=mean(y)
  y=y-y.bar
  if(standardize==TRUE){
    for(j in 1:p){
      Scale[j]=sqrt(covar(X[,j],X[,j]))
      X[,j]=X[,j]/Scale[j]
    }
  }
  beta=as.vector(solve(t(X)%*%X+lambda*diag(p))%*%t(X)%*%y)
  if(standardize==TRUE){
    for(j in 1:p){
      beta[j]=beta[j]/Scale[j]
    }
  }
  beta.0=y.bar-sum(X.bar*beta)
  return(list(beta=beta,beta.0=beta.0))
}

linear.lasso(X,y,1,TRUE)
linear.lasso(X,y,1,FALSE)
ridge(X,y,1,TRUE)
ridge(X,y,1,FALSE)