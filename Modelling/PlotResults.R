library(zoo)
library(hydroGOF)
library(hydroTSM)
library(ggplot2)

for(Model in 3)
{

if(Model==1)
{
 
#Mapping
#Water Level 
NameLevel<-c(   "DS Lock 3","Overland Corner", "US Lock 2", "DS Lock 2", "Morgan", "US Lock 1")
FolderLevel<-c("Flow"    ,"Level"   ,"Level"   ,"Flow"    ,"Level"   ,"Level")
HydstraLevel<-c("A4260517","A4260528","A4260518","A4260519","A4261110","A4260902")
ChainageLevel<-c(157250,   143250,     88000,      87250,     41750,       0 )
PlotLevel<-      c(TRUE, TRUE      ,FALSE,TRUE, TRUE, FALSE)

#Flow
NameFlow<-c("DS Lock 2", "DS Lock 1", "DS Lock 3")
HydstraFlow<-c("A4260519","A4260903","A4260517")
ChainageFlow<-c(87250,0,157250)

#get results
file<-"D:/LTIM/ModelOutputs/Lock13-TSOut.txt"
#file<-"E:\\LTIM\\ModelOutputs\\Lock13-TSOut-Historic.txt"

OutFolder<-"L3-L1"

}

if(Model==2)
{
  NameLevel<-c( "DS Lock 5","Lyrup PS","Berri PS","US Lock 4","DS Lock 4","Solora PS","Loxton PS","US Lock 3")
  FolderLevel<-    c("Flow",    "Level",  "Level",    "Level",   "Flow",   "Level","Level",       "Level")
  HydstraLevel<-c("A4260513","A4260663","A4260537","A4260514","A4260515","A4261065","A4260550",   "A4260516")
  ChainageLevel<-c(0,        26073.750, 37325.500,46250.000,46750.000,58454.299,69341.926,135760.152)
  PlotLevel<-c(TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE)
  
  #Flow
  NameFlow<-  c("DS Lock 4","DS Lock 3")
  FolderFlow<-c("Flow","Flow")
  HydstraFlow<-c("A4260515","A4260517")
  ChainageFlow<-c(46750.000,135760.152)
  
  #get results
  file<-"D:/LTIM/ModelOutputs/Kat-TSOut.txt"
  #file<-"E:\\LTIM\\ModelOutputs\\Kat-TSOut-Historic.txt"
  
  OutFolder<-"Kat"
  
}

if(Model==3)
{
  #Water Level 
  NameLevel<-c( "DS Lock 6","US Lock 5","DS Lock 5","Lyrup PS","Berri PS","US Lock 4")
  FolderLevel<-    c("Flow",    "Level",   "Flow",    "Level",    "Level",   "Level")
  HydstraLevel<-c("A4260511","A4260512","A4260513","A4260663","A4260537","A4260514")
  #ChainageLevel<-c(0,        57693.850,  57834.150,82940.600,91938.400,105174.970)
  ChainageLevel<-c(0,        57693.850,  57834.150,82940.6,97373.1,105174.970)
  PlotLevel<-c(TRUE,FALSE,TRUE,TRUE,TRUE,FALSE)
  
  #Flow
  NameFlow<-  c("DS Lock 5","DS Lock4")
  FolderFlow<-c("Flow","Flow")
  HydstraFlow<-c("A4260513","A4260515")
  ChainageFlow<-c(57834.150,105174.970)
  
  #get results
  file<-"D:\\LTIM\\ModelOutputs\\Pike-TSOut.txt"
  #file<-"E:\\LTIM\\ModelOutputs\\Pike-TSOut-Historic.txt"
  
  OutFolder<-"Pike"
}

Chainage<-read.table(file,sep=",",skip=3,nrows=1,stringsAsFactors=FALSE)
Chainage<-Chainage[-1]
Variable<-read.table(file,sep=",",skip=4,nrows=1,stringsAsFactors=FALSE)
Variable<-Variable[-1]
Results<-read.table(file,sep=",",skip=8,nrows=length(readLines(file))-10,stringsAsFactors=FALSE)
Dates<-substr(Results[,1],3,21)
Dates<-as.Date(Dates)

Results[,1]<-as.numeric(substr(Results[,1],22,42))

Index<-1 #1 for level, 2 for flow

PLO<-NULL
PLS<-NULL

for(i in 1:length(HydstraLevel))
{
  site<-HydstraLevel[i]
  chain<-ChainageLevel[i]
  folder<-FolderLevel[i]
  name<-NameLevel[i]
  
  A<-read.csv(paste0(folder,"/",site,"_100.00_DAY_0900.csv"),skip=2)
  X<-xts(A$Mean,as.POSIXct(strptime(A$Date,"%H:%M:%S %d/%m/%Y")))
  
  Col<-which(Chainage==chain)[Index]
  Y<-xts(Results[,Col],Dates)
  
  #read chainage
  #NA bad values
  #zoo
  png(paste0("Modelling/Plots/",OutFolder,"/",name,".png"))
  plot2(Y,X,plot.type="single",legend=c("Sim","Obs"),main=name)
  graphics.off()
  
  if(PlotLevel[i])
  {
    PLO<-cbind(PLO,window(X,start="2014-07-01",end="2015-06-30"))
    PLS<-cbind(PLS,window(Y,start="2014-07-01",end="2015-06-30"))
  }
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
  Y<-xts(Results[,Col],Dates)*86.4

  #read chainage
  #NA bad values
  #zoo
  png(paste0("Modelling/Plots/",OutFolder,"/",name,"Flow.png"))
  plot2(Y,X,plot.type="single",legend=c("Sim","Obs"),main=name)
  graphics.off()
  
  Y<-apply.daily(Y,mean)
  Y<-to.daily(Y,drop.time=TRUE)[,1]
  X<-to.daily(X,drop.time=TRUE)[,1]
  
  Diversions[[i]]<-apply.weekly(X,function (x) mean(x,na.rm=TRUE))-apply.weekly(Y,function (x) mean(x,na.rm=TRUE))
  
  write.csv(Diversions[[i]],file=paste0("Modelling/Plots/",OutFolder,"/",site,"Diversions.csv"))
  
}

#format diversions file


f<-"DiversionsFormatted.txt"
cat("Discharge[Ml/day]:Instantaneous\n",file=paste0("Modelling/Plots/",OutFolder,"/",f))

if(Model==1)
{
cat(paste("Time","Lock 2","Lock 1","Lock 3\n",sep="\t"),file=paste0("Modelling/Plots/",OutFolder,"/",f),append=TRUE)
Data<-cbind(Diversions[[1]]-Diversions[[3]],Diversions[[2]]-Diversions[[1]]-Diversions[[3]],Diversions[[3]])
Data[is.na(Data)]<-0
}
if(Model==2)
{
  cat(paste("Time","Lock 4","Lock 3","\n",sep="\t"),file=paste0("Modelling/Plots/Kat/",f),append=TRUE)
  Data<-cbind(Diversions[[1]],Diversions[[2]]-Diversions[[1]])
}
if(Model==3)
{
  cat(paste("Time","Lock 5","Lock 4","\n",sep="\t"),file=paste0("Modelling/Plots/Pike/",f),append=TRUE)
  Data<-cbind(Diversions[[1]],Diversions[[2]]-Diversions[[1]])
}
Data[is.na(Data)]<-0
write.table(Data,sep="\t",file=paste0("Modelling/Plots/",OutFolder,"/",f),append=TRUE,quote=FALSE,row.names=index(Data),col.names=FALSE)

#Plot for report
colnames(PLO)<-NameLevel[PlotLevel]
colnames(PLS)<-NameLevel[PlotLevel]

fPLO<-fortify(PLO,melt=TRUE)
fPLS<-fortify(PLS,melt=TRUE)

fPLO$Scenario<-"Observed"
fPLS$Scenario<-"Modelled"

X<-rbind(fPLO,fPLS)
pWL<-ggplot(X,aes(x=Index,y=Value,colour=Scenario))+geom_line()+
  facet_grid(Series ~ .,scales="free") + ylab("Level (m AHD)")+xlab("Date")+theme_bw()+theme(legend.position="top")
#  scale_colour_manual(values=cols)+ theme(legend.position="top",legend.direction="horizontal",legend.box="horizontal")
ggsave(paste0("Modelling/Plots/",OutFolder,"/",OutFolder,"_WaterLevelCalibration.png"),pWL,width=16,height=22,units="cm",dpi=300)

}