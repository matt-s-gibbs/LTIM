library(zoo)
library(hydroGOF)
library(hydroTSM)


#Mapping
#Water Level 
NameLevel<-c( "DS Lock 5","Lyrup PS","Berri PS","US Lock 4","DS Lock 4","Solora PS","Loxton PS","US Lock 3")
FolderLevel<-    c("Flow",    "Level",  "Level",    "Level",   "Flow",   "Level","Level",       "Level")
HydstraLevel<-c("A4260513","A4260663","A4260537","A4260514","A4260515","A4261065","A4260550",   "A4260516")
ChainageLevel<-c(0,        26073.750, 37325.500,46250.000,46750.000,58454.299,69341.926,135760.152)

#Flow
NameFlow<-  c("DS Lock 4","DS Lock 3")
FolderFlow<-c("Flow","Flow")
HydstraFlow<-c("A4260515","A4260517")
ChainageFlow<-c(46750.000,135760.152)

TheReach<-"MURRAY_L3_L5"

#get results
file<-"Modelling/ModelOutputs/Kat-TSOut.txt"

Reach<-read.table(file,sep="",skip=2,nrows=1,stringsAsFactors=FALSE)
Reach<-Reach[-1] #River name has a space in it, index is out by 1
Chainage<-read.table(file,sep="",skip=3,nrows=1,stringsAsFactors=FALSE)
Variable<-read.table(file,sep="",skip=4,nrows=1,stringsAsFactors=FALSE)
Results<-read.table(file,sep="",skip=8,nrows=length(readLines(file))-10,stringsAsFactors=FALSE)

Index<-1 #1 for level, 2 for flow

for(i in 1:length(HydstraLevel))
{
site<-HydstraLevel[i]
chain<-ChainageLevel[i]
folder<-FolderLevel[i]
name<-NameLevel[i]

A<-read.csv(paste0(folder,"/",site,"_100.00_DAY_0900.csv"),skip=2)
X<-xts(A$Mean,as.POSIXct(strptime(A$Date,"%H:%M:%S %d/%m/%Y")))

C1<-which(Chainage==chain)
C2<-which(Reach==TheReach)
Col<-intersect(C1,C2)[Index] #first variable is level

Date<-as.POSIXct(paste(Results[,1],Results[,2]))
Y<-xts(Results[,Col+1],Date)

#read chainage
#NA bad values
#zoo
png(paste0("Modelling/Plots/Kat/",name,".png"))
plot2(Y,X,plot.type="single",legend=c("Sim","Obs"),main=name)
graphics.off()
}

Index<-2 #1 for level, 2 for flow

Diversions<-list()

for(i in 1:length(HydstraFlow))
{
  site<-HydstraFlow[i]
  chain<-ChainageFlow[i]
  folder<-FolderFlow[i]
  name<-NameFlow[i]
  
  A<-read.csv(paste0(folder,"/",site,"_141.00_DAY_0900.csv"),skip=2)
  X<-xts(A$Mean,as.POSIXct(strptime(A$Date,"%H:%M:%S %d/%m/%Y"),tz = "GMT"))
  
  C1<-which(Chainage==chain)
  C2<-which(Reach==TheReach)
  Col<-intersect(C1,C2)[Index] #second variable is flow
  
  Date<-as.POSIXct(strptime(paste(Results[,1],Results[,2]),"%Y-%m-%d %H:%M:%S"),tz = "GMT")
  Y<-xts(Results[,Col+1]*86.4,Date)
  #Y[1:72]<-NA #remove intial instability
  
  #read chainage
  #NA bad values
  #zoo
  png(paste0("Modelling/Plots/Kat/",name,"Flow.png"))
  plot2(Y,X,plot.type="single",legend=c("Sim","Obs"),main=name)
  graphics.off()
  
  
  Y<-apply.daily(Y,mean)
  Y<-to.daily(Y,drop.time=TRUE)[,1]
  X<-to.daily(X,drop.time=TRUE)[,1]
  
  Diversions[[i]]<-apply.weekly(Y,function (x) mean(x,na.rm=TRUE))-apply.weekly(X,function (x) mean(x,na.rm=TRUE))
  
  write.csv(Diversions[[i]],file=paste0("Modelling/Plots/Kat/",site,"Diversions.csv"))
  
}

#format diversions file


f<-"KatDiversions.txt"
cat("Discharge[Ml/day]:Instantaneous\n",file=paste0("Modelling/Plots/Kat/",f))
cat(paste("Time","Lock 5","Lock 4","\n",sep="\t"),file=paste0("Modelling/Plots/Kat/",f),append=TRUE)

Data<-cbind(paste("09:00:00",index(Diversions[[1]])),round(as.numeric(Diversions[[1]],3),0),round(as.numeric(Diversions[[2]]),0))
write.table(Data,sep="\t",file=paste0("Modelling/Plots/Kat/",f),append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)

