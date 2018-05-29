df=read.table("iris.txt",sep=",")
x=as.matrix(df[,1:4])
y=as.vector(as.numeric(df[,5]))
cv=cv.glmnet(x,y,family="multinomial")
cv2=cv.glmnet(x,y,family="multinomial",type.measure="class")
par(mfrow=c(1,2))
plot(cv)
plot(cv2)
par(mfrow=c(1,1))
lambda=cv$lambda.min
result=glmnet(x,y,lambda=lambda,family="multinomial")
beta=array(dim=c(3,4))
for(k in 1:3)beta[k,]=drop(result$beta[[k]])
beta.0=array(dim=3)
beta.0=drop(result$a0)
f=function(x,k) beta.0[k]+x%*%beta[k,]
z=array(dim=150)
for(i in 1:150){
max.k=1; max.value=-100 
for(k in 1:3)if(f(x[i,],k)>max.value){max.value=f(x[i,],k); max.k=k}
print(max.k)
z[i]=max.k
}

