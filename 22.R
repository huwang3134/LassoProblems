install.packages("glmnet")
library(glmnet)
X=as.matrix(X)
y=as.vector(y)
p=ncol(X)
X.bar=array(dim=p)
for (j in 1:p) {
  X.bar[j]=mean(X[,j])
  X[,j]=X[,j]-X.bar[j]
}
y.bar=mean(y)
y=y-y.bar
result=glmnet(X,y)
plot(result)