n=500
x=array(dim=c(6,n))
z=array(dim=c(2,n))

for(i in 1:2){
  z[i,]=rnorm(n)
}

y=3*z[1,]-1.5*z[2,]+2*rnorm(n)

for(j in 1:3){
  x[j,]=z[1,]+rnorm(n)/5
}

for(j in 4:6){
  x[j,]=z[2,]+rnorm(n)/5
}

par(mfrow=c(2,2))

glm.fit=glmnet(t(x),y,alpha=0)
plot(glm.fit)

legend("topleft",legend = c("X1","X2","X3","X4","X5","X6"),col=1:6,lwd=2,cex=0.8)

glm.fit=glmnet(t(x),y,alpha=0.3)
plot(glm.fit)

legend("topleft",legend = c("X1","X2","X3","X4","X5","X6"),col=1:6,lwd=2,cex=0.8)

glm.fit=glmnet(t(x),y,alpha=1)
plot(glm.fit)

legend("topleft",legend = c("X1","X2","X3","X4","X5","X6"),col=1:6,lwd=2,cex=0.8)
