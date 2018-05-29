library(glmnet)
df=read.csv("nba.csv")
x=as.matrix(df[,2:20])
x=x[,-9]
y=as.vector(df[,21])
cv=cv.glmnet(x,y,familiy="binomial")
cv2=cv.glmnet(x,y,familiy="binomial",type.measure="class")
par(mfrow=c(1,2))
plot(cv)
plot(cv2)
par(mfrow=c(1,1))
lambda=cv$lambda.min
result=glmnet(x,y,lambda = lambda, family = "binomial")
beta=result$beta
beta.0=result$a0
f=function(x){
  beta.0+x%*%beta
}
g=function(x){
  exp(f(x))/(1+exp(f(x)))
}
z=array(dim=nrow(x))
for(i in 1:nrow(x)){
  z[i]=drop(g(x[i,]))
}
res=y==(z>0.5)
table(res)/length(res)