@echo.
@echo ****************************
@echo Processing A4261159
@echo ****************************
@echo.
@echo Creating subdirectory if not already there
@echo (will fail if cannot create)
set HYBATCHMKDIR=D:\LTIM\LTIM\Level
if not exist D:\LTIM\LTIM\Level mkdir D:\LTIM\LTIM\Level
if not exist D:\LTIM\LTIM\Level goto DIRERROR
if exist D:\LTIM\LTIM\Level\A4261159.* del D:\LTIM\LTIM\Level\A4261159.*
@echo.
@echo ****************************
@echo Processing A4261159
@echo ****************************
@echo.
echo A4261159        P04 - CSV File - Lake - All Record
@echo Clearing error and warning files
if exist C:\HYD_SCIENCE\TEMP\HYDSYS.ERR del C:\HYD_SCIENCE\TEMP\HYDSYS.ERR
if exist C:\HYD_SCIENCE\TEMP\HYDSYS.WRN del C:\HYD_SCIENCE\TEMP\HYDSYS.WRN
@echo Clearing temporary output files
if exist C:\HYD_SCIENCE\TEMP\WIPFILE.?? del C:\HYD_SCIENCE\TEMP\WIPFILE.??
if exist C:\HYD_SCIENCE\TEMP\JUNK del C:\HYD_SCIENCE\TEMP\JUNK
echo DATA A4261159        AT 100.00 100.00 MEAN > C:\HYD_SCIENCE\TEMP\PRM\BATCH.PRM
echo TIME DAY 1.0000 0.00 09:00_31/05/2015 N:N:N_N/N/N END D:\LTIM\LTIM\Level\A4261159_100.00_DAY_0900.csv No Yes "HH:II:EE DD/MM/YYYY" >> C:\HYD_SCIENCE\TEMP\PRM\BATCH.PRM
start /w O:\TS_MANAGE\hyd\sys\run\HYCSV @C:\HYD_SCIENCE\TEMP\PRM\BATCH.PRM
if errorlevel 1 echo *** Error in CSV File - Lake - All Record (HYCSV) >> C:\HYD_SCIENCE\TEMP\DataRqst.log
if errorlevel 1 del C:\HYD_SCIENCE\TEMP\JUNK
if exist C:\HYD_SCIENCE\TEMP\HYDSYS.ERR echo *** Error file details for CSV File - Lake - All Record (HYCSV): >> C:\HYD_SCIENCE\TEMP\DataRqst.log
if exist C:\HYD_SCIENCE\TEMP\HYDSYS.ERR type C:\HYD_SCIENCE\TEMP\HYDSYS.ERR >> C:\HYD_SCIENCE\TEMP\DataRqst.log
if exist C:\HYD_SCIENCE\TEMP\HYDSYS.WRN echo *** Warnings generated by CSV File - Lake - All Record (HYCSV): >> C:\HYD_SCIENCE\TEMP\DataRqst.log
if exist C:\HYD_SCIENCE\TEMP\HYDSYS.WRN type C:\HYD_SCIENCE\TEMP\HYDSYS.WRN >> C:\HYD_SCIENCE\TEMP\DataRqst.log
if NOT exist C:\HYD_SCIENCE\TEMP\JUNK echo *** P04 text output file not found (C:\HYD_SCIENCE\TEMP\JUNK) >> C:\HYD_SCIENCE\TEMP\DataRqst.log
@echo Copying output to D:\LTIM\LTIM\Level\A4261159.p04.txt
if exist C:\HYD_SCIENCE\TEMP\JUNK copy C:\HYD_SCIENCE\TEMP\JUNK D:\LTIM\LTIM\Level\A4261159.p04.txt >nul
  @echo Clearing error and warning files
if exist C:\HYD_SCIENCE\TEMP\HYDSYS.ERR del C:\HYD_SCIENCE\TEMP\HYDSYS.ERR
if exist C:\HYD_SCIENCE\TEMP\HYDSYS.WRN del C:\HYD_SCIENCE\TEMP\HYDSYS.WRN
@echo Clearing temporary output files
if exist C:\HYD_SCIENCE\TEMP\PLOT.?? del C:\HYD_SCIENCE\TEMP\PLOT.??
if exist C:\HYD_SCIENCE\TEMP\JUNK del C:\HYD_SCIENCE\TEMP\JUNK
