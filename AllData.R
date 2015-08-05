library(chron)
library(zoo)
#source("ExportStations.R")

startdate<-"09:00_01/06/2014"
path<-"D:\\MIKEHydro\\HydstraInput"

Interpolate<-function(filename)
{
  file.copy(paste0("../Cat1Indicator/",Folder,"/",filename),paste0("Data/Orig/"),overwrite=TRUE)
  A<-read.csv(paste0("../Cat1Indicator/",Folder,"/",filename),skip=3)
  A<-zoo(A[,2],as.Date(A[,1],"%H:%M:%S %d/%m/%Y"))
  A<-na.trim(A)
  print(paste(filename,":",sum(is.na(A)),"days (",round(sum(is.na(A))/length(A),2),"%) interpolated"))
  A<-na.approx(A)
  B<-cbind(paste("09:00:00",format(index(A),"%d/%m/%Y")),as.numeric(A),1)
  
  
  Header<-read.csv(paste0("../Cat1Indicator/",Folder,"/",filename),nrows=2,header=TRUE)
  write.csv(Header,file=paste0("Data\\",filename),quote=TRUE,row.names=FALSE)
  write.table(B,file=paste0("Data\\",filename),row.names=FALSE,append=TRUE,sep=",",col.names=FALSE,quote=FALSE)
}

Folders<-c("Level","Flow")
for(Folder in Folders)
{

files<-list.files(paste0("../Cat1Indicator/",Folder),"*.csv")
for(f in files)
{
  #interpolate missing values condtions
  Interpolate(f)
  
  A<-read.csv(paste0("Data/",f),nrows=2,header=FALSE,stringsAsFactors=FALSE)
  Name<-A[1,2]
  Variable<-A[2,2]
  Variable<-gsub(' \\(','\\[',Variable)
  Variable<-gsub('\\)','\\]',Variable)
  Variable<-gsub('Level','Water Level',Variable)
  Variable<-gsub('\\[m\\]','\\[meter\\]',Variable)
  cat(paste0(Variable,":Instantaneous"),file=paste0("Formatted\\",f))
  cat(paste("\nTime",Name,"\n",sep="\t"),file=paste0("Formatted\\",f),append=TRUE)
  
  A<-read.csv(paste0("Data/",f),skip=3,header=FALSE,stringsAsFactors=FALSE)
  write.table(cbind(A[,1],A[,2]),sep="\t",file=paste0("Formatted\\",f),append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
  
  if(length(grep("A4260518",f))>0 | length(grep("A4260514",f))>0 | length(grep("A4260512",f))>0)
  {
    cat(paste0(Variable,":Instantaneous"),file=paste0("Formatted\\1cmLow",f))
    cat(paste("\nTime",Name,"\n",sep="\t"),file=paste0("Formatted\\1cmLow",f),append=TRUE)
    write.table(cbind(A[,1],A[,2]-0.01),sep="\t",file=paste0("Formatted\\1cmLow",f),append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
    
    cat(paste0(Variable,":Instantaneous"),file=paste0("Formatted\\1cmHigh",f))
    cat(paste("\nTime",Name,"\n",sep="\t"),file=paste0("Formatted\\1cmHigh",f),append=TRUE)
    write.table(cbind(A[,1],A[,2]+0.01),sep="\t",file=paste0("Formatted\\1cmHigh",f),append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

}
