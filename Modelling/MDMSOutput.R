library(zoo)
library(dplyr)

startDate<-"2014-07-01"
Scenarios<-c("Historic","withoutCEW","NoEwater")

for(Scenario in Scenarios)
{
AllData<-NULL
StationNo<-1

for(Model in 1:3)
{

if(Model==1)
{
 
#Mapping
#Water Level 
#NameLevel<-c(   "DS Lock 3","Overland Corner", "US Lock 2", "DS Lock 2", "Morgan", "US Lock 1")
  NameLevel<-c("LK3DS_431km","LK3DS_417km","LK3DS_361km","LK2DS_361km","LK2DS_316km","LK2DS_274km")
ChainageLevel<-c(157250,   143250,     88000,      87250,     41750,       0 )

#get results
#file<-"Modelling/ModelOutputs/Lock13-TSOut.txt"
file<-paste0("E:\\LTIM\\ModelOutputs\\Lock13-TSOut-",Scenario,".txt")


}

if(Model==2)
{
  #NameLevel<-c( "DS Lock 5","Lyrup PS","Berri PS","US Lock 4","DS Lock 4","Solora PS","Loxton PS","US Lock 3")
  NameLevel<-c("LK5DS_563km","LK5DS_537km","LK5DS_526km","LK5DS_516km","LK4DS_516km","LK4DS_505km","LK4DS_494km","LK4DS_431km")
  ChainageLevel<-c(0,        26073.750, 37325.500,46250.000,46750.000,58454.299,69341.926,135760.152)
  
  #get results
  #file<-"Modelling/ModelOutputs/Kat-TSOut.txt"
  file<-paste0("E:\\LTIM\\ModelOutputs\\Kat-TSOut-",Scenario,".txt")
  
}

if(Model==3)
{
  #Water Level 
  #NameLevel<-c( "DS Lock 6","US Lock 5","DS Lock 5","Lyrup PS","Berri PS","US Lock 4")
  NameLevel<-c("LK6DS_620km","LK6DS_563km")
  ChainageLevel<-c(0,        57693.850)
  
  #get results
  file<-paste0("E:\\LTIM\\ModelOutputs\\Pike-TSOut-",Scenario,".txt")
  
}

Chainage<-read.table(file,sep=",",skip=3,nrows=1,stringsAsFactors=FALSE)
Chainage<-Chainage[-1]
Variable<-read.table(file,sep=",",skip=4,nrows=1,stringsAsFactors=FALSE)
Variable<-Variable[-1]
Results<-read.table(file,sep=",",skip=8,nrows=length(readLines(file))-10,stringsAsFactors=FALSE)
Dates<-substr(Results[,1],3,21)
Dates<-as.Date(Dates)

Results[,1]<-as.numeric(substr(Results[,1],22,42))


for(i in 1:length(NameLevel))
{
  chain<-ChainageLevel[i]
  MDMSName<-NameLevel[i]
  
  Col<-which(Chainage==chain)
  Q<-zoo(Results[,Col],Dates)
  Q[,2]<-Q[,2]*86.4 #convert flow to ML/d
  Q<-window(Q,start=startDate)
  
  Data<-data.frame(Q)
  colnames(Data)<-c("WL","Q","V")
  #add extra columns
  Data$sampleDate<-paste0(format(index(Q),format="%d/%m/%Y")," 09:00:",formatC(StationNo,width=2,flag="0")) #date and time needs to be unique.
  Data$samplingTime<-paste0("09:00:",formatC(StationNo,width=2,flag="0"))
  StationNo<-StationNo+1
  Data$Name<-MDMSName
  Data$Comments<-"ModelledData"
  
  AllData<-rbind(AllData,Data)
  
  
}

}

Data<-AllData
#reorder
Data = Data %>% select(Name,sampleDate,samplingTime,Q,WL,V,Comments)

#rename
names<-colnames(Data)
names<-gsub("WL","waterLevel (m)",names)
names<-gsub("Q","discharge (ML/day)",names)
names<-gsub("V","velocity (m/s)",names)
names<-gsub("Name","Sample Point Name",names)

write.table(Data,paste0("Output/LowerMurrayData",Scenario,"_Modelled.csv"),row.names=FALSE,col.names = names,sep = ",",quote = FALSE) 

}