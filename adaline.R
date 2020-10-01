points_plotting<-function(X,Y){
  plot(x=X[1:4],y=Y[1:4],type="p",xlab="X",ylab="Y",col="blue",pch=3,cex=2,xlim=c(0,4),ylim=c(0,4))
  points(x=X[5:8],y=Y[5:8],col="red",pch=4,cex=2)
  #abline(a=0,b=1,col="blue")
  title("data plot")  
}
sep_plotting<-function(w,color=1,lwd=1){
  if(w[3]==0){
    if(w[2]!=0){
      abline(v=-w[1]/w[2],col=color,lwd=lwd)
    }   
    #ici on doit traiter le cas au w[2]==0
  }else{
    abline(a=-w[1]/w[3], b=-w[2]/w[3],col=color,lwd=lwd)
  }
}
weights_update<-function(w,x,err){
  return(w+2*err*x)
}
activation_function<-function(w,x){
  sum<-0
  for(i in 1:3)
    sum<-sum+(w[i]-min(w))*(x[i]-min(x))/((max(w)-min(w))*(max(x)-min(x)))
  return(sum)
}
classification_function<-function(w,x){
  return(sign(activation_function(w,x)))
}
error<-function(w,input,y){
  sum<-0
  for(i in 1:nrow(input)){
    sum=sum+(y[i]-activation_function(w,input[i,]))^2
  }
  return(sum/nrow(input))
}
adaline<-function(w,input,y){
  Tmax=2000;
  t=0
  sep_plotting(w,2)
  tol=0
  print(paste("error",error(w,input,y)))
  #here we calculate the error
  while(t<=Tmax&&(error(w,input,y)>=tol)){
    err<-0
    for(i in 1:nrow(input)){
      err=y[i]-activation_function(w,input[i,])
      print(paste("err :",err))
      if(err!=0){
        w=weights_update(w,input[i,],err) 
        sep_plotting(w,t)
      }
    } 
    t=t+1
  }
  print(paste("nombre d'iterations",t))
  print(paste("le poids",w))
  return(w)
}
# introduction des données
w<-c(0,0.2,-0.5)
output<-c(1,1,1,1,0,0,0,0)
input<-matrix(nrow = 8,ncol=3,data=rbind(c(1,2,4),c(1,1,3),c(1,1,2),c(1,0.5,1.7),c(1,1,0.5),c(1,0,0.5),c(1,0.5,1.5),c(1,0,1)))
points_plotting(input[,2],input[,3])
w<-adaline(w,input,output)
sep_plotting(w,lwd=2)