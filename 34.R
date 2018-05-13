X=as.matrix(X)
y=as.vector(y)
cv=cv.glmnet(X,y,grouped=F)
plot(cv)
#最上部の0〜5の数字は説明変数の数を表す