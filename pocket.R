points_plotting<-function(X,Y,O){
  plot(X[1],Y[1],type="p",xlab="X",ylab="Y",col="red",pch=3,cex=2,xlim=c(0,4),ylim=c(0,4))
  for(i in 2:length(O)){
    if(O[i]==O[1]){
      points(X[i],Y[i],col="red",type="p",pch=3,cex=2)
    }else{
      points(X[i],Y[i],col="blue",type="p",pch=3,cex=2)
    }
  }  
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
pocket<-function(w,input,y,tmax,tol){
  j=0
  c=0
  w_temp<-w
  sep_plotting(w,2)
  err=c()
  #here we calculate the error
  err[1]=error(w,input,y)
  cc=1
  for(t in 1:tmax){
    for(i in 1:nrow(input)){
      c=c+1;
      if(classification_function(w_temp,input[i,])!=y[i]){
        w_temp=weights_update(w_temp,input[i,],y[i])  
      }
      if(error(w_temp,input,y)<error(w,input,y)){
        w=w_temp
        sep_plotting(w,j)
        j=j+1
      }
      cc=cc+1
      err[cc]=error(w,input,y)
      if(err[cc]<tol){
        print(paste("nombre d'iterations",c))
        res=c(w,err)
        return(res)
      }
    }
  }
  res=c(w,err)
  print(paste("nombre d'iterations",c))
  return(res)
}
err_plot=function(err){
  plot(c(1:length(err)),err, type="o", col="red")
  title("Pocket Error")
}
