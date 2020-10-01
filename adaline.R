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
weights_update<-function(w,x,err){
  return(w+0.01*err*x)
}
activation_function<-function(w,x){
  sum<-0
  for(i in 1:3){
    sum<-sum+w[i]*x[i]
  }  
  return(sum)
}
classification_function<-function(w,x){
  return(sign(activation_function(w,x)))
}
error<-function(w,input,y){
  sum<-0
  for(i in 1:nrow(input)){
    x=input[i,]
    x=normalisation(x)
    sum=sum+(y[i]-activation_function(w,x))^2
  }
  return(sum/nrow(input))
}
normalisation=function(x){
  res=c()
  for(i in 1:length(x)){
    res[i]=(x[i]-min(x))/(max(x)-min(x))
  }
  return(res)
}
err_plot=function(err){
  plot(c(1:length(err)),err, type="o", col="red")
  title("Delta rule Error")
}
adaline<-function(w,input,y,Tmax,tol){
  w_min=w
  sep_plotting(w,2)
  err=c()
  err[1]=error(w,input,y)
  #here we calculate the error
  cc=1
  c=0
  couleur=3
  for(t in 1:Tmax){
    for(i in 1:nrow(input)){
      c=c+1
      e=y[i]-classification_function(w,input[i,])
      print(paste("err :",e))
      if(e!=0){
        xs=normalisation(input[i,])
        er=y[i]-activation_function(w,xs)
        w=weights_update(w,input[i,],er) 
        
      }
      cc=cc+1
      err[cc]=error(w,input,y)
      if(err[cc]<err[cc-1]){
        w_min=w
      }
      
      if(err[cc]<=tol){
        sep_plotting(w_min,couleur)
        print(paste("nombre d'itérations : ",c))
        res=c(w_min,err)
        return(res)
      }
    }
  }
  sep_plotting(w_min,couleur)
  print(paste("nombre d'iterations",c))
  res=c(w_min,err)
  return(res)
}


