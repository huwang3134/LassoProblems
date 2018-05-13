cv.linear.lasso=function(X,y,lambda.max=100,K=10){
  X=as.matrix(X)
  y=as.matrix(y)
  p=ncol(X)
  n=nrow(X)
  for(j in 1:p){
    X[,j]=X[,j]-mean(X[,j])
  }
  y=y-mean(y)
  dec=round(lambda.max/50)
  lambda.seq=seq(lambda.max,1,-dec)
  r=length(lambda.seq)
  S=array(0,dim=r)
  m=round(n/K)
  for(i in 1:K){
    test=seq(i,n,m)
    train=setdiff(1:n,test)
    coef.seq=warm.start(X[train,],y[train],lambda.max) #warmstart()->28
    for(h in 1:r){
      S[h]=S[h]+sum((y[test]-X[test,]%*%coef.seq[h,])^2)/m
    }
  }
  S=S/K
  S.min=Inf
  for(h in 1:r){
    if(S[h]<S.min){
      S.min=S[h]
      k=h
    }
  }
  coef.seq=warm.start(X,y,lambda.max)
  T0=array(0,dim=r)
  for(h in 1:r){
    T0[h]=sum((y-X%*%coef.seq[h,])^2)/n
  }
  return(list(lambda.min=lambda.seq[k],
              S.seq=S,T.seq=T0,lambda.seq=lambda.seq))
}

result=cv.linear.lasso(X,y)
S.seq=result$S.seq
T.seq=result$T.seq
lambda.seq=result$lambda.seq

plot(lambda.seq,S.seq,xlab="lambda",ylab="error at test",
     ylim=c(min(S.seq,T.seq),max(S.seq,T.seq)),col="red")

lines(lambda.seq,T.seq,col="blue")

