source("perceptron.R")

# introduction des données sans bruits
w<-c(0,1,-1)
output<-c(1,1,-1,-1)
input<-matrix(nrow = 4,ncol=3,data=rbind(c(1,2,4),c(1,1,0.5),c(1,0.5,1.5),c(1,0,0.5)))
points_plotting(input[,2],input[,3],output)
res1<-perceptron(w,input,output)

w=res1[1:3]
w
err1=res1[4:length(res1)]
min(err1)
sep_plotting(w,lwd=2)
err_plot(err1)

# introduction des données avec bruits
w<-c(0,1,-1)
output<-c(1,1,1,1,-1,-1,-1,-1)
input<-matrix(nrow = 8,ncol=3,data=rbind(c(1,0.5,0),c(1,3,3.5),c(1,2.5,3),c(1,2,4),c(1,1,0.5),c(1,0.5,1.5),c(1,0,0.5),c(1,2.7,3.6)))
points_plotting(input[,2],input[,3],output)
res1<-perceptron(w,input,output)

w=res1[1:3]
w
err1=res1[4:length(res1)]
min(err1)
sep_plotting(w,lwd=2)
err_plot(err1)

##############################################################################"""
source("pocket.R")

# introduction des données sans bruits
w<-c(0,1,-1)
output<-c(1,1,-1,-1)
input<-matrix(nrow = 4,ncol=3,data=rbind(c(1,2,4),c(1,1,0.5),c(1,0.5,1.5),c(1,0,0.5)))
points_plotting(input[,2],input[,3],output)
res2<-pocket(w,input,output,100,1/8)

w=res2[1:3]
w
err2=res2[4:length(res2)]
min(err2)
sep_plotting(w,lwd=2)
err_plot(err2)

# introduction des données avec bruits
w<-c(0,1,-1)
output<-c(1,1,1,1,-1,-1,-1,-1)
input<-matrix(nrow = 8,ncol=3,data=rbind(c(1,0.5,0),c(1,3,3.5),c(1,2.5,3),c(1,2,4),c(1,1,0.5),c(1,0.5,1.5),c(1,0,0.5),c(1,2.7,3.6)))
points_plotting(input[,2],input[,3],output)
res2<-pocket(w,input,output,100,1/8)

w=res2[1:3]
w
err2=res2[4:length(res2)]
min(err2)
sep_plotting(w,lwd=2)
err_plot(err2)

##############################################################################""
source("adaline.R")

# introduction des données sans bruits
w<-c(0,1,-1)
output<-c(1,1,-1,-1)
input<-matrix(nrow = 4,ncol=3,data=rbind(c(1,2,4),c(1,1,0.5),c(1,0.5,1.5),c(1,0,0.5)))
points_plotting(input[,2],input[,3],output)
res3<-adaline(w,input,output,100,1/8)

w=res3[1:3]
w
err3=res3[4:length(res3)]
min(err3)
sep_plotting(w,lwd=2)
err_plot(err3)

# introduction des données avec bruits
w<-c(0,1,-1)
output<-c(1,1,1,1,-1,-1,-1,-1)
input<-matrix(nrow = 8,ncol=3,data=rbind(c(1,0.5,0),c(1,3,3.5),c(1,2.5,3),c(1,2,4),c(1,1,0.5),c(1,0.5,1.5),c(1,0,0.5),c(1,2.7,3.6)))
points_plotting(input[,2],input[,3],output)
res3<-adaline(w,input,output,100,1/8)

w=res3[1:3]
w
err3=res3[4:length(res3)]
min(err3)
sep_plotting(w,lwd=2)
err_plot(err3)
