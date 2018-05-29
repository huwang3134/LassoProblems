crime=read.table("crime.txt")
X=crime[,3:7]
y=crime[,1]

X=as.matrix(X)
y=as.vector(y)
X=scale(X)
y=y-mean(y)
p=ncol(X)
beta=array(dim=p)

AIC.min=Inf
for(k in 1:p){
  A=combn(1:p,k) #pこの中からk個選ぶ全組
  q=ncol(A)
  for(h in 1:q){
    T0=A[,h]
    beta[T0]=solve(t(X[,T0])%*%X[,T0])%*%t(X[,T0])%*%y
    Z=0
    for(j in T0){
      Z=Z+X[,T0]*beta[j]
      S=sum((y-z)^2)/n
      AIC=log(S)+2*k
      print(c(S,k,AIC))
      if(AIC<AIC.min){
        AIC.min=AIC
        k.min=k
        T.min=T0
      }
    }
  }
}

k.min
T.min