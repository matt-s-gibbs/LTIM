# #TO DO
# 1. check the flow variables for A4261091 and DS locks, i.e. 141.1?
# 2. are there ever comments on the flow? Maybe for Chowilla. paste them together?
# 3. Check comments output works
# 4. Get quality codes
# 5. Map quality codes
# 

source("../HydstraInput/ExportStations.R")

startdate<-"09:00_01/07/2014"
path<-"D:\\Documents\\Dropbox\\LTIM\\Cat1Indicator"

Stations<-read.csv("Stations/FlowStations.csv",stringsAsFactors = FALSE)

for(i in 1:nrow(Stations))
{
  if(Stations$Station[i]=="A4261091")
  {
    ExportStation(Stations$Station[i],startdate,"141.00","141.00","DAY",paste0(path,"\\Flow")) #check this.
  }else
  {
    ExportStation(Stations$Station[i],startdate,"141.10","141.00","DAY",paste0(path,"\\Flow"))
  }
  if(Stations$Level[2]=="Y")
  {
    ExportStation(Stations$Station[i],startdate,"100.00","100.00","DAY",paste0(path,"\\Flow"))
  }
}

Stations<-read.csv("Stations/LevelStations.csv",stringsAsFactors = FALSE)

for(i in 1:nrow(Stations))
{
  ExportStation(Stations$Station[i],startdate,"100.00","100.00","DAY",paste0(path,"\\Level"))
}

