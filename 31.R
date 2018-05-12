crime=read.table("crime.txt")
lm.fit=lm(V1~V3+V4+V5+V6+V7,data=crime)
summary(lm.fit)

X=as.matrix(crime[,3:7])
y=as.vector(crime[,1])
p=ncol(X)
for(j in 1:p){
  X[,j]=X[,j]-mean(X[,j])
}
y=y-mean(y)

library(glmnet)
glmnet(X,y,lambda=30)$beta
lm.fit=lm(V1~V3+V4+V5,data=crime)
summary(lm.fit)
