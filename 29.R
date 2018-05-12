L1=sum(abs(linear(X,y)$beta))
L1.seq=L1.seq/L1
plot(L1.seq,beta,xlim=c(0,1),ylim=c(-12,12))