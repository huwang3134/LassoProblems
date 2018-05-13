crime=read.table("crime.txt")
X=crime[,3:7]
y=crime[,1]
linear(X,y)
ridge(X,y,100)