lambda.max=100
M=100
dec=round(lambda.max/50)
lambda.seq=seq(lambda.max,1,-dec)
r=length(lambda.seq)
SS.seq=array(0,dim=r)
SS2.seq=array(0,dim=r)

for(i in 1:M){
  index=sample(1:n,n,replace = TRUE)
  S.seq=cv.linear.lasso(X[index,],y[index])$S.seq
  SS.seq=SS.seq+S.seq
  SS2.seq=SS2.seq+S.seq^2
}

mid=SS.seq/M
sgm=sqrt(SS2.seq/(M-1)-mid^2)

plot(log(lambda.seq),mid,xlab="log(lambda)",ylab="error at test",
     ylim=c(min(mid-sgm),max(mid+sgm)),type="n")
lines(log(lambda.seq),mid+sgm,col="blue")
lines(log(lambda.seq),mid-sgm,col="blue")
lines(log(lambda.seq),mid,col="red")