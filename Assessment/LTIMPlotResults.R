library(xts)
library(ggplot2)

VelocityQuantiles<-function(file,Split,LevelLocations,WPNames)
{
  
  Chainage<-read.table(file,sep=",",skip=3,nrows=1,stringsAsFactors=FALSE)
  Chainage<-Chainage[-1]
  Variable<-read.table(file,sep=",",skip=4,nrows=1,stringsAsFactors=FALSE,strip.white = TRUE)
  Variable<-Variable[-1]
  Results<-read.table(file,sep=",",skip=8,nrows=length(readLines(file))-10,stringsAsFactors=FALSE)
  Dates<-substr(Results[,1],3,21)
  Date<-as.POSIXct(Dates,tz="UTC")
  
  Results[,1]<-as.numeric(substr(Results[,1],22,42))
  
  for(i in 1:2)
  {
  
    Col1<-which(Variable=="V")
    
    if(i==1)  Col2<-which(Chainage <Split)
    if(i==2)  Col2<-which(Chainage >=Split)
    
    Col<-intersect(Col1,Col2)
    Y<-xts(Results[,Col],Date)
    Y<-window(Y,start=as.POSIXct("2014-07-01 00:00:00", "%Y-%m-%d %H:%M:%S",tz="UTC"))
    
    Q<-t(apply(Y,1,function(x) quantile(x,c(0.1,0.25,0.5,0.75,0.9))))
    Q<-as.data.frame(Q)
    colnames(Q)<-c("Q10","Q25","Q50","Q75","Q90")
    Q$Date<-index(Y)
    
    if(i==1)
    {
      Q1<-Q
      Q1$WeirPool<-WPNames[1]
    }
    if(i==2)
    {
      Q2<-Q
      Q2$WeirPool<-WPNames[2]
    }
  }
  Q<-rbind(Q1,Q2)
  
  Col1<-intersect(which(Variable=="WL"),which(Chainage ==LevelLocations[1]))
  Col2<-intersect(which(Variable=="WL"),which(Chainage ==LevelLocations[2]))
  Col3<-intersect(which(Variable=="WL"),which(Chainage ==LevelLocations[3]))
  Col4<-intersect(which(Variable=="WL"),which(Chainage ==LevelLocations[4]))
  Col<-c(Col1,Col2,Col3,Col4)
  Y<-xts(Results[,Col],Date)
  Y<-window(Y,start=as.POSIXct(strptime("2014-07-01 00:00:00", "%Y-%m-%d %H:%M:%S")))
  
  WL<-fortify(Y,melt=TRUE)
  WL$WeirPool<-WPNames[1]
  WL$Location<-WL$Series
  WL[WL$Location==paste0("V",Col3),]$WeirPool<-WPNames[2]
  WL[WL$Location==paste0("V",Col4),]$WeirPool<-WPNames[2]
  
  levels(WL$Location)<-c("Mid","Upper","Mid","Upper")
  
  return(list(Q,WL))
}

LoadResults<-function(folder,Model,Runs,RunNames,Lock,LevelLocations,WPNames)
{
  file<-paste0(folder,"/",Model,Runs[1])
  O<-VelocityQuantiles(file,Lock,LevelLocations,WPNames)
  QO<-O[[1]]
  WLO<-O[[2]]
  QO$Scenario<-RunNames[1]
  WLO$Scenario<-RunNames[1]
  
  file<-paste0(folder,"/",Model,Runs[2])
  NEW<-VelocityQuantiles(file,Lock,LevelLocations,WPNames)
  QNEW<-NEW[[1]]
  WLNEW<-NEW[[2]]
  QNEW$Scenario<-RunNames[2]
  WLNEW$Scenario<-RunNames[2]
  
  Q<-rbind(QO,QNEW)
  WL<-rbind(WLO,WLNEW)
  
  return(list(Q,WL))
}

folder<-"E:\\LTIM\\ModelOutputs"
Runs<-c("-TSOut-Historic.txt","-TSOut-NoEwater.txt")
RunNames<-c("With eWater","No eWater")

Model<-"Lock13"
Lock<-88000
WPNames<-c("Weir Pool 1","Weir Pool 2")
#order - mid, upper, mid, upper
LevelLocations<-c(44000,87750,122500,157000)

X<-LoadResults(folder,Model,Runs,RunNames,Lock,LevelLocations,WPNames)
Q<-X[[1]]
WL<-X[[2]]

Model<-"Pike"
Lock<-57693.850
WPNames<-c("Weir Pool 6","Weir Pool 5")
LevelLocations<-c(27849.1,0,81966.6,57834.150)
X<-LoadResults(folder,Model,Runs,RunNames,Lock,LevelLocations,WPNames)

Q<-rbind(Q,X[[1]])
WL<-rbind(WL,X[[2]])

#sort from L6-L1
WL$WeirPool<-factor(WL$WeirPool,levels=sort(unique(WL$WeirPool),decreasing=TRUE))
Q$WeirPool<-factor(Q$WeirPool,levels=sort(unique(Q$WeirPool),decreasing=TRUE))

cols<-c("#009E73","#56B4E9")

 pv<-ggplot(Q,aes(x=Date))+geom_ribbon(aes(ymin=Q10,ymax=Q90,fill=Scenario),alpha=0.5)+geom_line(aes(y=Q50,colour=Scenario))+
     facet_grid(WeirPool ~ .) + ylab("Velocity (m/s)")+xlab("Date")+theme_bw()+theme(legend.position="top")+
   scale_fill_manual(values=cols)+scale_colour_manual(values=cols)

pWL<-ggplot(WL,aes(x=Index,y=Value,colour=Scenario))+geom_line(aes(linetype=Location))+
  facet_grid(WeirPool ~ .,scales="free") + ylab("Level (m AHD)")+xlab("Date")+theme_bw()+
  scale_colour_manual(values=cols)+ theme(legend.position="top",legend.direction="horizontal",legend.box="horizontal")+
  scale_linetype_manual(values=c("dashed","solid"))

ggsave(paste0("Assessment/Output/WaterLevel.png"),pWL,width=16,height=22,units="cm",dpi=300)
ggsave(paste0("Assessment/Output/Velocity.png"),pv,width=16,height=22,units="cm",dpi=300)

#+geom_ribbon(aes(ymin=Q25,ymax=Q75,fill=Scenario),alpha=0.2)
#   geom_ribbon(data=ONEW,fill="red",aes(ymin=Q10,ymax=Q90),alpha=0.2)+geom_ribbon(data=ONEW,fill="red",aes(ymin=Q25,ymax=Q75),alpha=0.2)+geom_line(data=ONEW,colour="red",aes(y=Q50))#+
# #  geom_ribbon(data=ONWP,fill="blue",aes(ymin=Q10,ymax=Q90),alpha=0.2)+geom_ribbon(data=ONWP,fill="blue",aes(ymin=Q25,ymax=Q75),alpha=0.2)+geom_line(data=ONWP,colour="blue",aes(y=Q50))
