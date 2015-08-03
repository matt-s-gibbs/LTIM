library(zoo)
library(hydroGOF)
library(hydroTSM)


#Mapping
#Water Level 
NameLevel<-c( "DS Lock 6","US Lock 5","DS Lock 5","Lyrup PS","Berri PS","US Lock 4")
FolderLevel<-    c("Flow",    "Level",   "Flow",    "Level",    "Level",   "Level")
HydstraLevel<-c("A4260511","A4260512","A4260513","A4260663","A4260537","A4260514")
ChainageLevel<-c(0,        57693.850,  57834.150,82940.600,91938.400,105174.970)

#Flow
NameFlow<-  c("DS Lock 5","DS Lock4")
FolderFlow<-c("Flow","Flow")
HydstraFlow<-c("A4260513","A4260515")
ChainageFlow<-c(57834.150,105174.970)

TheReach<-"MURRAY"

#get results
file<-"Modelling/ModelOutputs/Pike-TSOut.txt"


Chainage<-read.table(file,sep="",skip=3,nrows=1,stringsAsFactors=FALSE)

Reach<-read.fwf(file,skip=2,n=1,widths=c(21,rep(20,length(Chainage)))) #deal with spaces in names (TODO, use sep in MIKE output)

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
png(paste0("Modelling/Plots/Pike/",name,".png"))
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
  png(paste0("Modelling/Plots/Pike/",name,"Flow.png"))
  plot2(Y,X,plot.type="single",legend=c("Sim","Obs"),main=name)
  graphics.off()
  
  
  Y<-apply.daily(Y,mean)
  Y<-to.daily(Y,drop.time=TRUE)[,1]
  X<-to.daily(X,drop.time=TRUE)[,1]
  
  Diversions[[i]]<-apply.weekly(Y,function (x) mean(x,na.rm=TRUE))-apply.weekly(X,function (x) mean(x,na.rm=TRUE))
  
  write.csv(Diversions[[i]],file=paste0("Modelling/Plots/Pike/",site,"Diversions.csv"))
  
}

#format diversions file


f<-"PikeDiversions.txt"
cat("Discharge[Ml/day]:Instantaneous\n",file=paste0("Modelling/Plots/Pike/",f))
cat(paste("Time","Lock 5","Lock 4","\n",sep="\t"),file=paste0("Modelling/Plots/Pike/",f),append=TRUE)

Data<-cbind(paste("09:00:00",index(Diversions[[1]])),round(as.numeric(Diversions[[1]],3),0),round(as.numeric(Diversions[[2]]),0))
write.table(Data,sep="\t",file=paste0("Modelling/Plots/Pike/",f),append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)

