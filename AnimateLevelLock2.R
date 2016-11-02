library(animation)
library(zoo)
library(xts)

#ADD LOCKS
#Change to river km


fileHistoric<-"D:\\LTIM\\ModelOutputs\\Lock13-Historic.txt"
filenoEwater<-"D:\\LTIM\\ModelOutputs\\Lock13-NoCEWO.txt"
filenoRaising<-"D:\\LTIM\\ModelOutputs\\Lock13-NoRaising.txt"
fileneither<-"D:\\LTIM\\ModelOutputs\\Lock13-NoCEWO-NoRaising.txt"
  
lowChainage<-88000.000
highChainage<-157250
startDate<-"2015-07-01"
endDate<-"2016-06-30"
# 
PlotVariable<-"V"
Label<-"Lock 2 Velocity (m/s)"

 # PlotVariable<-"WL"
 #  Label<-"Lock 2 Level (m AHD)"

cols<-c("#008CE5","#000000","#008CE5","#000000")

LoadData<-function(file,lowChainage,highChainage)
{
  Chainage<-read.table(file,sep=",",skip=3,nrows=1,stringsAsFactors=FALSE)
  Chainage<-Chainage[-1]
  Variable<-read.table(file,sep=",",skip=4,nrows=1,stringsAsFactors=FALSE,strip.white = TRUE)
  Variable<-Variable[-1]
  Results<-read.table(file,sep=",",skip=8,nrows=length(readLines(file))-10,stringsAsFactors=FALSE)
  Dates<-substr(Results[,1],3,21)
  Dates<-as.Date(Dates)
  
  Results[,1]<-as.numeric(substr(Results[,1],22,42))
  
  Col<-which(Variable==PlotVariable&Chainage<highChainage&Chainage>=lowChainage)
  
  Y<-zoo(Results[,Col],Dates)
  Y<-window(Y,start=startDate,end=endDate)
  
  xChain<-Chainage[Col]
  
  Col<-which(Variable=="Q"&Chainage==highChainage)
  Q<-zoo(Results[,Col],Dates)
  Q<-window(Q,start=startDate,end=endDate)*86.4
  return(list(WL=Y,Q=Q,Chainage=xChain))
}

X<-LoadData(fileHistoric,lowChainage,highChainage)
Y<-X[["WL"]]
Q<-X[["Q"]]
Chainage<-X[["Chainage"]]
Chainage<-rev(Chainage-lowChainage)

X<-LoadData(filenoEwater,lowChainage,highChainage)
Y_noeW<-X[["WL"]]
#Q_noeW<-X[["Q"]]

X<-LoadData(filenoRaising,lowChainage,highChainage)
Y_noWPR<-X[["WL"]]

X<-LoadData(fileneither,lowChainage,highChainage)
Y_neither<-X[["WL"]]
Q_noeW<-X[["Q"]]

# X<-read.csv("BottomElevation/WP5.csv")
# X$x<-X$x/1000

saveGIF({
for(i in 1:nrow(Y))
{
    par(mfrow=c(2,1),mar=c(5.1,4.1,0.1,1))
    plot(as.numeric(Chainage)/1000,Y[i,],type="l",ylim=c(min(Y),max(Y,Y_noWPR)),
         xlab="Distance Downstream (km)",col=cols[1],
         ylab=Label)
    lines(as.numeric(Chainage)/1000,Y_noeW[i,],col=cols[2],lty="longdash")
    lines(as.numeric(Chainage)/1000,Y_noWPR[i,],col=cols[3],lty="longdash")
    lines(as.numeric(Chainage)/1000,Y_neither[i,],col=cols[4])

    if(PlotVariable=="WL")
    {
      text(34.5,6.07,"Pool Level",col="grey")
      abline(h=6.1,lty="dotdash",col="grey")
    }
    abline(v=max(Chainage)/1000,lty="dotdash",col="grey")
    abline(v=0,lty="dotdash",col="grey")
    
    if(PlotVariable=="WL")
    {
      text(-2.5,6.15,"Lock 3",col="grey",srt=90,pos=4)
      text(69.1,6.15,"Lock 2",col="grey",srt=90,pos=4)
    }else
    {
      text(-1,0.05,"Lock 3",col="grey",srt=90,pos=4)
      text(69.5,0.05,"Lock 2",col="grey",srt=90,pos=4)
    }
  
  #  lines(X$x,X$y,col="grey")
    legend("topright",legend=c("with CEW, with WPR","no CEW, with WPR","with CEW, no WPR","no CEW, no WPR"),
           lwd=1,col=cols,lty=c("solid","longdash","longdash","solid"))
    
    plot(Q,ylab="Lock 2 Flow (ML/d)",xlab="Date",col=cols[1],ylim=c(0,max(Q)))
    lines(Q_noeW,col=cols[4])
    abline(v=.index(Q[i]),col="grey",lty="dashed")
    legend("topright",legend=c("with CEW","without CEW"),lwd=1,col=cols[c(1,4)])
}
},movie.name=paste0("Lock2",PlotVariable,".gif"),interval=1/24,ani.height=684,ani.width=1188)


i<-which(index(Y)=="2015-10-31")
# 
# png(paste0("GIFs/",PlotVariable,"_Q.png"),width=15,height=20,units="cm",res=300)
# par(mfrow=c(2,1),mar=c(5.1,4.1,0.1,1))
# plot(as.numeric(Chainage)/1000,Y[i,],type="l",ylim=c(min(Y),max(Y)),
#      xlab="Distance Downstream (km)",col=cols[1],
#      ylab=Label)
# lines(as.numeric(Chainage)/1000,Y_noeW[i,],col=cols[2],lty="longdash")
# lines(as.numeric(Chainage)/1000,Y_noWPR[i,],col=cols[3],lty="longdash")
# lines(as.numeric(Chainage)/1000,Y_neither[i,],col=cols[4])
# 
# if(PlotVariable=="WL")
# {
#   text(30,16.27,"Pool Level",col="grey")
#   abline(h=16.3,lty="dotdash",col="grey")
# }
# abline(v=max(Chainage)/1000,lty="dotdash",col="grey")
# abline(v=0,lty="dotdash",col="grey")
# 
# if(PlotVariable=="WL")
# {
#   text(-1,16.35,"Lock 6",col="grey",srt=90,pos=4)
#   text(57.5,16.35,"Lock 5",col="grey",srt=90,pos=4)
# }else
# {
#   text(-1,0.05,"Lock 6",col="grey",srt=90,pos=4)
#   text(57.5,0.05,"Lock 5",col="grey",srt=90,pos=4)
# }
# 
# #  lines(X$x,X$y,col="grey")
# legend("topright",legend=c("with CEW, with WPR","no CEW, with WPR","with CEW, no WPR","no CEW, no WPR"),
#        lwd=1,col=cols,lty=c("solid","longdash","longdash","solid"))
# 
# plot(Q,ylab="Lock 5 Flow (ML/d)",xlab="Date",col=cols[1],ylim=c(0,max(Q)))
# lines(Q_noeW,col=cols[4])
# abline(v=.index(Q[i]),col="grey",lty="dashed")
# legend("topright",legend=c("with CEW","no CEW"),lwd=1,col=cols[c(1,4)])
# dev.off()
# 
# 


png(paste0("GIFs/",PlotVariable,".png"),width=15,height=10,units="cm",res=300)
par(mar=c(5.1,4.1,5,1))
plot(as.numeric(Chainage)/1000,Y[i,],type="l",ylim=c(min(Y),max(Y,Y_noWPR)),
     xlab="Distance Downstream (km)",col=cols[1],
     ylab=Label)
lines(as.numeric(Chainage)/1000,Y_noeW[i,],col=cols[2],lty="longdash")
lines(as.numeric(Chainage)/1000,Y_noWPR[i,],col=cols[3],lty="longdash")
lines(as.numeric(Chainage)/1000,Y_neither[i,],col=cols[4])
if(PlotVariable=="WL")
{
  text(34.5,6.07,"Pool Level",col="grey")
  abline(h=6.1,lty="dotdash",col="grey")
}
abline(v=max(Chainage)/1000,lty="dotdash",col="grey")
abline(v=0,lty="dotdash",col="grey")

if(PlotVariable=="WL")
{
  text(-2.5,6.15,"Lock 3",col="grey",srt=90,pos=4)
  text(69.1,6.15,"Lock 2",col="grey",srt=90,pos=4)
}else
{
  text(-2.5,0.05,"Lock 3",col="grey",srt=90,pos=4)
  text(69.1,0.05,"Lock 2",col="grey",srt=90,pos=4)
}
#  lines(X$x,X$y,col="grey")
if(PlotVariable=="WL")
{
legend(x=34.5,y=6.95,xpd=TRUE,xjust=0.5,yjust=0,legend=c("with CEW, with WPR","no CEW, with WPR","with CEW, no WPR","no CEW, no WPR"),
       lwd=1,col=cols,lty=c("solid","longdash","longdash","solid"),ncol=2)
}else
{
  legend(x=34.5,y=0.45,xpd=TRUE,xjust=0.5,yjust=0,legend=c("with CEW, with WPR","no CEW, with WPR","with CEW, no WPR","no CEW, no WPR"),
         lwd=1,col=cols,lty=c("solid","longdash","longdash","solid"),ncol=2)
}
dev.off()
