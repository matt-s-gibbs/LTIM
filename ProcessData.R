library(zoo)
library(dplyr)

#map the quality codes
# 1: Best quality unedited data. Meets operational standards and is considered a good representation of the true value.
# 2: Good quality. Minimal editing, may include sensor drift correction this is considered a good representation of the true value.
# 3: Modified or transformed data this is considered a reasonable representation of the true value.
# 4: Unreliable data - considered a poor representation (e.g. debris effecting sensor, flat batteries)
# 5: Estimated or modelled data.

#reserving 5 for modelled data only to distinguish. Using 4 for bad/extrapolated data.

QualCodes<<-read.csv("Stations/QualityCodes.csv",stringsAsFactors = FALSE)

MapQualityCodes<-function(data)
{
  col<-grep("Code",colnames(data))
  
  QC<-as.numeric(data[,col])

  data[QC<30,col]<-1 #Code less than 30 Good
  data[QC>=30 & QC<90,col]<-2 #code between 30 and 85 Fair
  data[QC>=90 & QC<130,col]<-3 #code between 30 and 85 Poor
  data[QC>=130,col]<-4 #Code greater than 130 unreliable (include direct from telemetry)
  return(data)
}


AllData<-NULL


for(Flow in c(FALSE,TRUE))
{


#if FLOW true, read flow station, else read level stations
  if(Flow)
  {
    Stations<-read.csv("Stations/FlowStations.csv",stringsAsFactors = FALSE)
    Folder<-"Flow"
  }else
  {
    Stations<-read.csv("Stations/LevelStationsDummy.csv",stringsAsFactors = FALSE)
    Folder<-"Level"
  }

#loop over stations

for(s in 1:nrow(Stations))
{

Station<-Stations$Station[s]
MDMSName<-Stations$MDMSName[s]

#read in data
A<-read.csv(paste0(Folder,"\\",Station,"_100.00_DAY_0900.csv"),skip=3,header=FALSE,stringsAsFactors=FALSE)
A[,4]<-QualCodes[match(A[,3],QualCodes[,1]),4]
WL<-zoo(A[,-1],as.Date(A[,1],"%H:%M:%S %d/%m/%Y"))
WL<-window(WL,start="2014-07-01",end="2015-06-29")
colnames(WL)<-c("dailyStage (m)","dailyStagequalityCode","Comments")
WL<-MapQualityCodes(WL)

#Deal with missing values
if(sum(is.na(WL$"dailyStage (m)"))>0)
{
  WL[is.na(WL$"dailyStage (m)"),]$dailyStagequalityCode<-5
  WL[is.na(WL$"dailyStage (m)"),]$Comments<-"Daily stage not calculated or not available"
  WL[is.na(WL$"dailyStage (m)"),]$"dailyStage (m)"<-0
}
  
if(Flow)
{
  A<-read.csv(paste0(Folder,"\\",Station,"_141.00_DAY_0900.csv"),skip=3,header=FALSE,stringsAsFactors=FALSE)
  A[,4]<-QualCodes[match(A[,3],QualCodes[,1]),4]
  Q<-zoo(A[,-1],as.Date(A[,1],"%H:%M:%S %d/%m/%Y"))
  Q<-window(Q,start="2014-07-01",end="2015-06-29")
  colnames(Q)<-c("dailyVolume (ML/day)","qualityCode","Comments") 
  Q<-MapQualityCodes(Q)
  
  #Deal with missing values
  if(sum(is.na(Q$"dailyVolume (ML/day)"))>0)
  {
  Q[is.na(Q$"dailyVolume (ML/day)"),]$qualityCode[]<-5
  Q[is.na(Q$"dailyVolume (ML/day)"),]$Comments[]<-"Daily discharge not calculated or not available"
  Q[is.na(Q$"dailyVolume (ML/day)"),]$"dailyVolume (ML/day)"[]<-0
  }
}else
{
  Q<-WL
  colnames(Q)<-c("dailyVolume (ML/day)","qualityCode","Comments") 
  Q$"dailyVolume (ML/day)"[]<-0
  Q$qualityCode[]<-5
  Q$Comments[]<-"No discharge at this sample point"
}
  
  Comments<-zoo(paste0(Q[,3]," (",WL[,2]," ",WL[,3],")"),index(Q))
  
  Data<-data.frame(cbind(WL[,1],Q[,1:2],Comments))

#add extra columns
Data$sampleDate<-paste(format(index(Q),format="%d/%m/%Y"),"09:00:00")
Data$Name<-MDMSName

 AllData<-rbind(AllData,Data)
}
}
Data<-AllData
#reorder
Data = Data %>% select(Name,sampleDate,WL...1.,dailyVolume..ML.day.,qualityCode,Comments)

#rename
names<-colnames(Data)
names<-gsub("WL...1.","dailyStage (m)",names)
names<-gsub("dailyVolume..ML.day.","dailyVolume (ML/day)",names)
names<-gsub("Name","Sample Point Name",names)

write.table(Data,paste0("Output/LowerMurrayData.csv"),row.names=FALSE,col.names = names,sep = ",",quote = FALSE) 
