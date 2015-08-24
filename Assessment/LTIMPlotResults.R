library(xts)
library(ggplot2)
library(Hmisc) #for weighted 

#NOTE FIXED DATE AROUND LINE 170

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
    
    if(i==1)
    {
      Col2<-which(Chainage <Split)
    }
    if(i==2)
    {
      Col2<-which(Chainage >=Split)
    }
    
    Col<-intersect(Col1,Col2)
    Y<-xts(Results[,Col],Date)
    Y<-window(Y,start=as.POSIXct("2014-07-01 00:00:00", "%Y-%m-%d %H:%M:%S",tz="UTC"))
    
    #weight velocities by distance between chainages to account for  uneven representation.
    WeightsDummy<-as.numeric(c(0,Chainage[Col],Chainage[Col[length(Col)]]))
    WeightsDummy<-diff(WeightsDummy)
    Weights<-rollsum(WeightsDummy/2,2)
    
    Q<-t(apply(Y,1,function(x) wtd.quantile(x,weights=Weights,probs=c(0.1,0.5,0.9))))
    Q<-as.data.frame(Q)
    colnames(Q)<-c("Q10","Q50","Q90")
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
  WL$Date<-index(Y)
  WL[WL$Location==paste0("V",Col3),]$WeirPool<-WPNames[2]
  WL[WL$Location==paste0("V",Col4),]$WeirPool<-WPNames[2]
  
  levels(WL$Location)<-c("Mid","Upper","Mid","Upper")
  
  return(list(Q,WL))
}

LoadResults<-function(folder,Model,Runs,RunNames,Lock,LevelLocations,WPNames)
{
  for(i in 1:length(Runs))
  {
    file<-paste0(folder,"/",Model,Runs[i])
    O<-VelocityQuantiles(file,Lock,LevelLocations,WPNames)
    QO<-O[[1]]
    WLO<-O[[2]]
    QO$Scenario<-RunNames[i]
    WLO$Scenario<-RunNames[i]
    
    if(i ==1)
    {
      Q<-QO
      WL<-WLO
    }else
    {
      Q<-rbind(Q,QO)
      WL<-rbind(WL,WLO)
    }
  }
  
  return(list(Q,WL))
}

getSeason <- function(DATES) {
  #2012 used as it is a leap year
  WS <- as.Date("2012-06-01", format = "%Y-%m-%d") # Winter start
  SE <- as.Date("2012-09-01",  format = "%Y-%m-%d") # Spring start
  SS <- as.Date("2012-12-01",  format = "%Y-%m-%d") # Summer start
  FE <- as.Date("2012-03-01",  format = "%Y-%m-%d") # Autumn Start
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS & d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS | d < FE, "Summer", "Autumn")))
}


DifferencePlot<-function(X1,X2,Name,xlab,binwidth)
{
  X1<-subset(X1,Date>as.POSIXct("2014-09-09 09:00:00", "%Y-%m-%d %H:%M:%S",tz="UTC"))  #FIXED DATE
  X2<-subset(X2,Date>as.POSIXct("2014-09-09 09:00:00", "%Y-%m-%d %H:%M:%S",tz="UTC"))  #FIXED DATE
  Xs<-merge(X1,X2,by=c("Date","WeirPool"))
  
  Xs$Value=Xs$Value.x-Xs$Value.y
  Xs$Value[Xs$Value<0]<-0  #zeros are possible, but this is numerical error, looks weird for plots. #CHECK!!
  
  Xs$Season<-getSeason(Xs$Date)
  Xs$Season<-factor(Xs$Season,levels=c("Winter","Spring","Summer","Autumn"))
  
  p<-ggplot(Xs)+geom_histogram(binwidth=binwidth,aes(x=Value,fill=Season))+
    #scale_x_discrete(expand = c(0,0),limits=c(0,pretty(max(Xs$Value))[2]))+
    facet_grid(WeirPool ~ .,scales="free") + xlab(xlab)+theme_bw()+theme(legend.position="top")+
    ylab("Number of days")+scale_fill_manual(values=c("#5DA5DA","#60BD68","#F15854","#FAA43A"))
    
  ggsave(paste0("Assessment/Output/",Name,".png"),p,width=16,height=22,units="cm",dpi=300)
  
  # 
  # p<-ggplot(Xs,aes(x=Value))+stat_ecdf(color=cols[3])+#geom_histogram(binwidth=0.01,fill=cols[3])+
  #   facet_grid(WeirPool ~ .,scales="free") + xlab("Change in median velocity (m/s)")+theme_bw()+theme(legend.position="top")+
  #   ylab("Proportion of the time the change was less than y")
  # ggsave(paste0("Assessment/Output/ChangeInVelocity.png"),p,width=16,height=22,units="cm",dpi=300)
}


#########################################################################################################

folder<-"E:\\LTIM\\ModelOutputs"
Runs<-c("-TSOut-Historic.txt","-TSOut-NoEwater.txt","-TSOut-withoutCEW.txt")
RunNames<-c("With eWater","No eWater","No CEW")

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
WPNames<-c("Weir Pool 5","Weir Pool 4P")
LevelLocations<-c(27849.1,0,81966.6,57834.150)
X<-LoadResults(folder,Model,Runs,RunNames,Lock,LevelLocations,WPNames)

Q<-rbind(Q,X[[1]][X[[1]]$WeirPool=="Weir Pool 5",])
WL<-rbind(WL,X[[2]][X[[2]]$WeirPool=="Weir Pool 5",])

Model<-"Kat"
Lock<-46500
WPNames<-c("Weir Pool 4","Weir Pool 3")
LevelLocations<-c(23248.033,0,91124.469,46750)
X<-LoadResults(folder,Model,Runs,RunNames,Lock,LevelLocations,WPNames)

Q<-rbind(Q,X[[1]])
WL<-rbind(WL,X[[2]])

#sort from L6-L1
WL$WeirPool<-factor(WL$WeirPool,levels=sort(unique(WL$WeirPool),decreasing=TRUE))
Q$WeirPool<-factor(Q$WeirPool,levels=sort(unique(Q$WeirPool),decreasing=TRUE))

#cols<-c("#009E73","#56B4E9")
cols<-c("#60BD68","#FAA43A","#5DA5DA") #colours from http://www.mulinblog.com/a-color-palette-optimized-for-data-visualization/

#time series

 pv<-ggplot(Q,aes(x=Date))+geom_ribbon(aes(ymin=Q10,ymax=Q90,fill=Scenario),alpha=0.2)+geom_line(aes(y=Q50,colour=Scenario))+
     facet_grid(WeirPool ~ .,scales="free") + ylab("Velocity (m/s)")+xlab("Date")+theme_bw()+theme(legend.position="top")+
   scale_fill_manual(values=cols)+scale_colour_manual(values=cols)

pWL1<-ggplot(subset(WL,Location=="Mid"),aes(x=Index,y=Value,colour=Scenario))+geom_line()+
  facet_grid(WeirPool ~ .,scales="free") + ylab("Level (m AHD)")+xlab("Date")+theme_bw()+
  scale_colour_manual(values=cols)+ theme(legend.position="top",legend.direction="horizontal",legend.box="horizontal")

pWL2<-ggplot(subset(WL,Location=="Upper"))+geom_line(aes(x=Index,y=Value,colour=Scenario))+
  facet_grid(WeirPool ~ .,scales="free") + ylab("Level (m AHD)")+xlab("Date")+theme_bw()+
  scale_colour_manual(values=cols)+ theme(legend.position="top",legend.direction="horizontal",legend.box="horizontal")

ggsave(paste0("Assessment/Output/WaterLevelMid.png"),pWL1,width=16,height=22,units="cm",dpi=300)
ggsave(paste0("Assessment/Output/WaterLevelUpper.png"),pWL2,width=16,height=22,units="cm",dpi=300)
ggsave(paste0("Assessment/Output/Velocity.png"),pv,width=16,height=22,units="cm",dpi=300)


##subsets for summary section
A<-rbind(subset(Q,WeirPool=="Weir Pool 1"), subset(Q, WeirPool=="Weir Pool 4"))
pv<-ggplot(A,aes(x=Date))+geom_ribbon(aes(ymin=Q10,ymax=Q90,fill=Scenario),alpha=0.2)+geom_line(aes(y=Q50,colour=Scenario))+
  facet_grid(WeirPool ~ .,scales="free") + ylab("Velocity (m/s)")+xlab("Date")+theme_bw()+theme(legend.position="top")+
  scale_fill_manual(values=cols)+scale_colour_manual(values=cols)

A<-rbind(subset(WL,Location=="Upper"&WeirPool=="Weir Pool 1"), subset(WL,Location=="Upper"& WeirPool=="Weir Pool 4"))

pWL2<-ggplot(A)+geom_line(aes(x=Index,y=Value,colour=Scenario))+
  facet_grid(WeirPool ~ .,scales="free") + ylab("Level (m AHD)")+xlab("Date")+theme_bw()+
  scale_colour_manual(values=cols)+ theme(legend.position="top",legend.direction="horizontal",legend.box="horizontal")

  ggsave(paste0("Assessment/Output/WaterLevelUpper_Subset.png"),pWL2,width=16,height=10,units="cm",dpi=300)
ggsave(paste0("Assessment/Output/Velocity_Subset.png"),pv,width=16,height=10,units="cm",dpi=300)

#difference historgrams
X1<-subset(Q,Scenario=="With eWater")
colnames(X1)<-gsub("Q50","Value",colnames(X1))
xlab<-"Change in median velocity (m/s)"

X2<-subset(Q,Scenario=="No eWater")
colnames(X2)<-gsub("Q50","Value",colnames(X2))
Name<-"Velocity_eWater"
DifferencePlot(X1,X2,Name,xlab,0.005)

X2<-subset(Q,Scenario=="No CEW")
colnames(X2)<-gsub("Q50","Value",colnames(X2))
Name<-"Velocity_CEW"
DifferencePlot(X1,X2,Name,xlab,0.005)

X1<-subset(WL,Scenario=="With eWater"& Location=="Upper")
xlab<-"Change in upper pool water level (m)"

X2<-subset(WL,Scenario=="No eWater" & Location=="Upper")
Name<-"WaterLevel_eWater"
DifferencePlot(X1,X2,Name,xlab,0.05)

X2<-subset(WL,Scenario=="No CEW"& Location=="Upper")
Name<-"WaterLevel_CEW"
DifferencePlot(X1,X2,Name,xlab,0.05)
#dummy


