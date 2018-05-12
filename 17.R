soft.th=function(lambda,x){
  return(sign(x)*pmax(abs(x)-lambda,0))
}
curve(soft.th(5,x),-10,10)