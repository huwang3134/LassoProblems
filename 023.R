df=read.table("crime.txt")
x=df[,3:7]
y=df[,1]
p=ncol(x)
lambda.seq=seq(0,170,22)
plot(log(lambda.seq),ylim=c(-1,10),type="n",col="red")
for(j in 1:p){
  coef.seq=NULL
  for(lambda in lambda.seq){
    coef.seq=c(coef.seq,linear.lasso(x,y,lambda)$beta[j])
  }
  par(new=TRUE)
  lines(log(lambda.seq),coef.seq,col=j)
}

legend("topright",legend=
         c("annual police funding",
           "% of people 25 years+ of high school",
           "% of 16 to 19 year-olds not in highschool and not highschool graduates",
           "% of 18 to 24 year-olds in college",
           "% of people 25 years+ with at least 4 years of college"),
       col=1:p, lwd=2, cex =.8)