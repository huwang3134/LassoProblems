library(ElemStatLearn)
library(glmnet)
data(zip.train)
dim(zip.train)
index=NULL
set=c(2,7,9)
m=length(set)
for(i in set){
  index=c(index,which(zip.train[,1]==i))
}

x=as.matrix(zip.train[index,2:257])
y=as.vector(zip.train[index,1])

cv.glmnet.fit=cv.glmnet(x,y,family="multinomial")
plot(cv.glmnet.fit)
opt.lambda=cv.glmnet.fit$lambda.min

glmnet.fit=glmnet(x,y,lambda=opt.lambda, family="multinomial")
beta=array(dim=c(m,256))
for(i in 1:m){
  beta[i,]=abs(glmnet.fit$beta[[i]][1:256])
  beta[i, beta[i,]!=0]=1
}

image.obj=NULL
for(j in 1:m){
  image.obj=rbind(image.obj,c(j,beta[j,]))
}

par(mfrow=c(2,3))
for(j in 1:m){
  image(zip2image(image.obj,j),col=gray(1:0))
}