soft.th=function(lambda,x){
  return(sign(x)*pmax(abs(x)-lambda,0))
}

linear.lasso.1=function(x,y,lambda){
  x.bar=mean(x)
  y.bar=mean(y)
  x.scale=scale(x)
  beta=soft.th(lammda,sum(x*y)/length(x))
  beta.0=y.bar-beta*x.bar
}
