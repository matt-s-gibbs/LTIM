#velocity validation
#get results
#file<-"Modelling/ModelOutputs/Lock13-TSOut.txt"
library(ggplot2)

Model=2

if(Model==1)
{
  file<-"E:\\LTIM\\ModelOutputs\\Lock13-TSOut-Historic.txt"
  VData<-read.csv("SARDIVelocityData/2014VelocityMeasurements.csv",stringsAsFactors=FALSE)
}
if(Model==2) 
{
  file<-"E:\\LTIM\\ModelOutputs\\Kat-TSOut-Historic.txt"
  #file<-"D:\\LTIM\\Kat_M11\\Results\\Kat-TSOut.txt"
  VData<-read.csv("SARDIVelocityData/2014VelocityMeasurementsL3-L4.csv",stringsAsFactors=FALSE)
}

Chainage<-read.table(file,sep=",",skip=3,nrows=1,stringsAsFactors=FALSE)
Variable<-read.table(file,sep=",",skip=4,nrows=1,stringsAsFactors=FALSE,strip.white = TRUE)
Results<-read.table(file,sep=",",skip=8,nrows=length(readLines(file))-10,stringsAsFactors=FALSE)

if(Model==1) VData$Chainage<-(as.numeric(substr(VData$Transect,1,3))-274.25)*1000
if(Model==2) VData$Chainage<-(as.numeric(substr(VData$Transect,1,3))-562.4)/131*-135760.152

velvar<-which(Variable=="V")

#measurement every 5th row
start<-1 #make 1 when got new data
VMod<-NULL
for(d in seq(start,nrow(VData),5))
{
  
  Date<-as.Date(VData$date[d],"%d/%m/%Y")+1 #moved forward a day to line up with modelling times (i.e. at 9am)
  rows<-which(Results[,1]==Date)
  if(Model==1) colsvar<-which(as.numeric(Chainage[velvar])>=(VData$Chainage[d]-500) & as.numeric(Chainage[velvar])<=(VData$Chainage[d+4]+500))
  if(Model==2) colsvar<-which(as.numeric(Chainage[velvar])<=(VData$Chainage[d]-500) & as.numeric(Chainage[velvar])>=(VData$Chainage[d+4]+500))
  
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

V$SurveyS<-factor(V$Survey,levels=rev(unique(V$Survey))) #fix order on X axis

p<-ggplot(V,aes(x=SurveyS,y=U,colour=Scenario))+ geom_violin(scale="width",aes(fill=Scenario)) +
  facet_wrap(~ Location , ncol=3)+ylab("Velocity (m/s)")+theme_bw()+ theme(legend.position="top") + xlab("Survey Date")
if(Model==1) ggsave("Modelling/Plots/L3-L1/L1-L3_Velocity.png",p,width=15.5,height=15,units="cm")
if(Model==2) ggsave("Modelling/Plots/Kat/Kat_Velocity.png",p,width=15.5,height=8,units="cm") 