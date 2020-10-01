points_plotting<-function(X,Y){
  plot(x=X[1:2],y=Y[1:2],type="p",xlab="X",ylab="Y",col="blue",pch=3,cex=2,xlim=c(0,4),ylim=c(0,4))
  points(x=X[3:4],y=Y[3:4],col="red",pch=4,cex=2)
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
weights_update<-function(w,x,y){
  return(w+x*y)
}
activation_function<-function(w,x){
  sum<-0
  for(i in 1:3)
    sum<-sum+w[i]*x[i]
  return(sum)
}
classification_function<-function(w,x){
  return(sign(activation_function(w,x)))
}
error<-function(w,input,y){
  nb_miss_class_points<-0
  for(i in 1:nrow(input)){
    if(classification_function(w,input[i,])!=y[i])
      nb_miss_class_points=nb_miss_class_points+1
  }
  return(nb_miss_class_points/nrow(input))
}
perceptron<-function(w,input,y){
  j=0
  c=1
  #here we calculate the error
  while(error(w,input,y)!=0){
    for(i in 1:nrow(input)){
      if(classification_function(w,input[i,])!=y[i]){
        w=weights_update(w,input[i,],y[i])
        c=c+1;
        sep_plotting(w,j)
        j=j+1
      }
    } 
  }
  print(c)
  return(w)
}

# introduction des données
w<-c(0,1,-1)
#output<-c(1,1,-1,-1)
#input<-matrix(nrow = 4,ncol=3,data=rbind(c(1,2,4),c(1,1,0.5),c(1,0.5,1.5),c(1,0,0.5)))
output<-c(1,1,1,1,-1,-1,-1,-1)
input<-matrix(nrow = 8,ncol=3,data=rbind(c(1,2,4),c(1,1,3),c(1,1,2),c(1,0.5,1.7),c(1,1,0.5),c(1,0,0.5),c(1,0.5,1.5),c(1,0,0.5)))
points_plotting(input[,2],input[,3])
w<-perceptron(w,input,output)
sep_plotting(w,lwd=2)