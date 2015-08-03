library(zoo)
library(hydroGOF)
library(hydroTSM)
library(ggplot2)


#Mapping
#Water Level 
NameLevel<-c(   "DS Lock 3","Overland Corner", "US Lock 2", "DS Lock 2", "Morgan", "US Lock 1")
FolderLevel<-c("Flow"    ,"Level"   ,"Level"   ,"Flow"    ,"Level"   ,"Level")
HydstraLevel<-c("A4260517","A4260528","A4260518","A4260519","A4261110","A4260902")
ChainageLevel<-c(157250,   143250,     88000,      87750,     41750,       0 )

#Flow
NameFlow<-c("DS Lock 2", "DS Lock 1", "DS Lock 3")
HydstraFlow<-c("A4260519","A4260903","A4260517")
ChainageFlow<-c(87750,0,157250)

#get results
#file<-"Modelling/ModelOutputs/Lock13-TSOut.txt"
file<-"E:/L3Trial/Observed/Lock13-TSOut.txt"

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
  
  Col<-which(Chainage==chain)[Index]
  Date<-as.POSIXct(paste(Results[,1],Results[,2]))
  Y<-xts(Results[,Col+1],Date)
  
  #read chainage
  #NA bad values
  #zoo
  png(paste0("Modelling/Plots/L3-L1/",name,".png"))
  plot2(Y,X,plot.type="single",legend=c("Sim","Obs"),main=name)
  graphics.off()
}

Index<-2 #1 for level, 2 for flow

Diversions<-list()

for(i in 1:length(HydstraFlow))
{
  site<-HydstraFlow[i]
  chain<-ChainageFlow[i]
  folder<-"Flow"
  name<-NameFlow[i]
  
  A<-read.csv(paste0(folder,"/",site,"_141.00_DAY_0900.csv"),skip=2)
  X<-xts(A$Mean,as.POSIXct(strptime(A$Date,"%H:%M:%S %d/%m/%Y"),tz = "GMT"))
  
  Col<-which(Chainage==chain)[Index]
  Date<-as.POSIXct(strptime(paste(Results[,1],Results[,2]),"%Y-%m-%d %H:%M:%S"),tz = "GMT")
  Y<-xts(Results[,Col+1]*86.4,Date)
  #Y[1:72]<-NA #remove intial instability
  
  #read chainage
  #NA bad values
  #zoo
  png(paste0("Modelling/Plots/L3-L1/",name,"Flow.png"))
  plot2(Y,X,plot.type="single",legend=c("Sim","Obs"),main=name)
  graphics.off()
  
  
  Y<-apply.daily(Y,mean)
  Y<-to.daily(Y,drop.time=TRUE)[,1]
  X<-to.daily(X,drop.time=TRUE)[,1]
  
  Diversions[[i]]<-apply.weekly(Y,function (x) mean(x,na.rm=TRUE))-apply.weekly(X,function (x) mean(x,na.rm=TRUE))
  
  write.csv(Diversions[[i]],file=paste0("Modelling/Plots/L3-L1/",site,"Diversions.csv"))
  
}

#format diversions file


f<-"L3-L1Diversions.txt"
cat("Discharge[Ml/day]:Instantaneous\n",file=paste0("Modelling/Plots/L3-L1/",f))
cat(paste("Time","Lock 2","Lock 1","Lock 3\n",sep="\t"),file=paste0("Modelling/Plots/L3-L1/",f),append=TRUE)

Data<-cbind(paste("09:00:00",index(Diversions[[1]])),round(as.numeric(Diversions[[1]],3),0),round(as.numeric(Diversions[[2]]),0),round(as.numeric(Diversions[[3]]),0))
write.table(Data,sep="\t",file=paste0("Modelling/Plots/L3-L1/",f),append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)

#velocity validation

VData<-read.csv("SARDIVelocityData/2014VelocityMeasurements.csv",stringsAsFactors=FALSE)
VData$Chainage<-(as.numeric(substr(VData$Transect,1,3))-274.25)*1000

velvar<-which(Variable=="V")

#measurement every 5th row
start<-31 #make 1 when got new data
VMod<-NULL
for(d in seq(start,nrow(VData),5))
{
 
  Date<-as.Date(VData$date[d],"%d/%m/%Y")+1 #moved forward a day to line up with modelling times (i.e. at 9am)
  rows<-which(Results[,1]==Date)
  colsvar<-which(as.numeric(Chainage[velvar])>=(VData$Chainage[d]-500) & as.numeric(Chainage[velvar])<=(VData$Chainage[d+4]+500))
  cols<-colsvar+2+min(velvar)-1 #2 date columns first, and get back to the start of velocity data
  X<-Results[rows,cols]
  VModMean<-apply(X,2,mean)
  
  M<-data.frame(Survey=VData$Survey[d],date=Date,Location=VData$Location[d],Chainage=as.numeric(Chainage[velvar][colsvar]),U=as.numeric(VModMean))
  
  VMod<-rbind(M,VMod)
}

VDataAll<-VData
VData<-VData[c(-2,-5)]
names<-colnames(VData)
names[4]<-"U"
colnames(VData)<-names
VData$date<-as.Date(VData$date,"%d/%m/%Y")+1

VData$Scenario<-"Monitoring"
VMod$Scenario<-"Modelled"

V<-rbind(VData,VMod)

p<-ggplot(V,aes(x=Survey,y=U,colour=Scenario))+ geom_violin(scale="width",aes(fill=Scenario)) +
  facet_wrap(~ Location , ncol=3)+ylab("Velocity (m/s)")+ theme(legend.position="top")
ggsave("Modelling/Plots/L3-L1/Velocity.png",p,width=15.5,height=20,units="cm") 
