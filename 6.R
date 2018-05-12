ridge=function(X,y,lambda){
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
  beta=as.vector(solve(t(X)%*%X+lambda*diag(p))%*%t(X)%*%y)
  beta.0=y.bar-sum(X.bar*beta)
  return(list(beta=beta,beta.0=beta.0))
}