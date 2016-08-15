library(animation)
library(zoo)
library(xts)

fileHistoric<-"D:\\LTIM\\ModelOutputs\\Pike-Historic.txt"
filenoEwater<-"D:\\LTIM\\ModelOutputs\\Pike-NoeWater.txt"
filenoRaising<-"D:\\LTIM\\ModelOutputs\\Pike-NoRaising.txt"
fileneither<-"D:\\LTIM\\ModelOutputs\\Pike-Noewater-NoRaising.txt"
  
lowChainage<-0
highChainage<-57834.150
startDate<-"2015-07-01"
endDate<-"2016-06-30"

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
  
  Col<-which(Variable=="V"&Chainage<highChainage&Chainage>=lowChainage)
  
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
    plot(as.numeric(Chainage)/1000,Y[i,],type="l",ylim=c(min(Y),max(Y)),ylab="Lock 5 Level (m AHD)",xlab="Distance Downstream (km)",col="blue")
    lines(as.numeric(Chainage)/1000,Y_noeW[i,])
    lines(as.numeric(Chainage)/1000,Y_noWPR[i,],col="red")
    lines(as.numeric(Chainage)/1000,Y_neither[i,],col="green")
  #  lines(X$x,X$y,col="grey")
    
    plot(Q,ylab="Lock 5 Flow (ML/d)",xlab="Date",col="blue")
    lines(Q_noeW)
    abline(v=.index(Q[i]),col="grey",lty="dashed")
    
}
},movie.name="Lock5.gif",interval=1/24,ani.height=810,ani.width=1080)