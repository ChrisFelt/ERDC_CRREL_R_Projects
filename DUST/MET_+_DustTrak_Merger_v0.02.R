#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# FILE: MET + DustTrak Merger
# AUTHORS: Christopher Felt ERDC-CRREL-NH
# DATE CREATED: 11 May 2020
# Version: 0.02
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# PURPOSE
# =======
#
# Combines DustTrak data with MET station data in a 
# new Excel spreadsheet.
# Resolves the resolution disparity between the two data sets and 
# and outputs at a 2 minute resolution.
# 
# 
# ASSUMPTIONS
# =======
#
# DustTrak raw data resolution must be 2 minutes.
# MET station raw data resolution must be 1 minute.
# Raw data formatting must be consistent with JER DustTrak/MET data.
# *Output and Input directories must be separate locations.
#


#----------------------------------------------------------------------
# Load required libraries.
#----------------------------------------------------------------------

library(lubridate)          # date time manipulation library
library(dplyr)              # data manipulation tools 
library(scales)
library(Rmisc)
library(stringr)
library(chron)              # more time manipulation (hms)
library(openxlsx)           # EXCEL!
library(CircStats)        # circ.mean FUN


#----------------------------------------------------------------------
# Set hard-coded parameters.
#----------------------------------------------------------------------

# I/O directories
oDir  <- '~/DUST/Test/Output'
iDir  <- '~/DUST/Test/DustTrak'

# Set time resolution to x minutes (only 2 minute resolution available as of v0.01).
timeRes <- 10

# Set site names for dust dTrakentration data
dTrakName <- c("Site_3", "Site_4", "Site_5")

# Set site names for MET data (NOTE: must correspond 1:1 with site names in dTrakName)
metName <- c("Site3", "Site4", "Site5")

#----------------------------------------------------------------------
# Process data files.
#----------------------------------------------------------------------

# Method to make is.nan() work with dfs
# Source: https://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame/18142212
is.nan.data.frame <- function(isnan)
do.call(cbind, lapply(isnan, is.nan))

# Import MET Station Data 
metList <- list.files(iDir, pattern="*.dat")

# Import .csv files from the input directory
dTrakList <- list.files(iDir, pattern="*.csv")

for(q in 1:length(dTrakName)){

  #============================
  # Process MET data files.
  #============================

    # Generate file lists...
    metFileSite <- grep(metName[q], metList)
    metSiteList <- metList[metFileSite]
    metSiteList <- paste0(iDir,"/",metSiteList)
    
    # Announce progress
    print(paste0(dTrakName[q],":"))
    print("Generating MET dataframe from the following files...")
    
    # Loop through and combine all available files for current site
    z = 1
    while(z <= length(metFileSite)){
      # Read and create a df of current .dat file
      metStationDF <- data.frame(read.table(metSiteList[z], header = FALSE, sep = ",", 
                                 col.names = paste0("V", seq_len(max(count.fields(metSiteList[z], sep = ',')))), fill = TRUE, 
                                 stringsAsFactors = FALSE))
      # Select header/data
      metStationHead <- metStationDF[1:4,]
      metStationDF <- metStationDF[5:nrow(metStationDF),]
      
      # Announce progress
      print(metSiteList[z])

      # Check if masterMet df exists
      if(exists("masterMetDF") == T){
        
        masterMetDF <- rbind(masterMetDF, metStationDF)
        
      }
      
      if(exists("masterMetDF") == F){
        
        masterMetDF <- metStationDF
        
      }
      
      
      z = z + 1
    } # End while(z <= length(metFileSite)) loop
    
    # Remove unnecessary columns from masterMetDF
    masterMetDF[,2] <- NULL # Record no. column
    
  
  #========================================
  # Process dust dTrak data files.
  #========================================
  
  # Import .csv files from the input directory
  processdTrak <- grep(dTrakName[q], dTrakList)
  processList <- dTrakList[processdTrak]
  
  # Find site/date info in dust dTrak. file names
  dTrakDate <- substr(processList, start = 0, stop = 8) # Seperate dust dTrak. date
  dTrakSite <- substr(processList, start = 10, stop = 15) # Separate dust dTrak. sites
  dTrakSite <- paste0(substr(dTrakSite, start = 1, stop = 4),substr(dTrakSite, start = 6, stop = 6))
  
  # Remove serial ID # from file names and generate list for output file names
  # Function to remove everything after the last "_" in a string
  removeTail <- function(x, sep = "_", del = 1){
    sapply(strsplit(x, split = sep, fixed = TRUE),
           function(xray) paste(head(xray, -del), collapse = sep))
  }
  # Function to remove everything after the first "_" in a string
  removeTail2 <- function(x2, sep = "_", del = 1){
    sapply(strsplit(x2, split = sep, fixed = TRUE),
           function(xray2) paste(head(xray2, del), collapse = sep))
  }
  
  tempdTrakID <- removeTail(processList, sep = "_", del = 1)
  tempdTrakID <- gsub("^.*?_DRX","",tempdTrakID) 
  dTrakID <- removeTail2(processList, sep = "_", del = 3)
  
  dTrakID <- paste0(dTrakID, tempdTrakID)
  
  
  
  # Loop through and process each dust dTrak .csv
  i = 1
  while(i <= length(processList)){
    
    # Print progress
    print(paste0("Processing files ", processList[i], " and ", processList[i+1],"..."))
    if(dTrakDate[i] != dTrakDate[i+1]){
      
      print("Warning! Dates do not match. Current files will not be processed.")
      
      i = i + 2
      break
      
    }
    
    # File to read in
    processFile <- paste0(iDir, "/", processList[i])
    processFile2 <- paste0(iDir, "/", processList[i+1])
    
    # Read table. Detect all columns that contain data. 
    # read.table() would not read in some fields even with fill = TRUE.
    process <- read.csv(processFile, col.names = paste0("V", seq_len(max(count.fields(processFile, sep = ',')))))
    process2 <- read.csv(processFile2, col.names = paste0("V", seq_len(max(count.fields(processFile2, sep = ',')))))
    
    # Make two separate dataframes 
    df <- process
    df2 <- process2
    
    # Make an initial dataframe from Date only
    tempDf <- slice(df[2], (which(df[1] == "Test Start Date")))
    tempDf2 <- slice(df2[2], (which(df2[1] == "Test Start Date")))
    
    # Add time to tempDf & tempDf2
    tempDf <- cbind(tempDf, slice(df[2], (which(df[1] == "Test Start Time"))))
    tempDf2 <- cbind(tempDf2, slice(df2[2], (which(df2[1] == "Test Start Time"))))
    
    # Rename columns
    colnames(tempDf) <- c("V1","V2")
    colnames(tempDf2) <- c("V1","V2")
    
    # Create new dataframe with standard date/time stamp using Date/Time from columns 1 and 2.
    dTrakTS <- data.frame(as.POSIXct(paste(tempDf$V1, format(strptime(tempDf$V2, "%I:%M:%S %p"), 
                                     format="%H:%M:%S")), format="%m/%d/%Y %H:%M:%S", tz = "GMT"))
    dTrakTS2 <- data.frame(as.POSIXct(paste(tempDf2$V1, format(strptime(tempDf2$V2, "%I:%M:%S %p"), 
                                     format="%H:%M:%S")), format="%m/%d/%Y %H:%M:%S", tz = "GMT"))

    # Convert from UTC to GMT... *only necessary when using with(dTrakTS, ymd(V1) + hms(V2))
    # dTrakTS <- data.frame(as.POSIXct(as.character(dTrakTS[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT"))
    # dTrakTS2 <- data.frame(as.POSIXct(as.character(dTrakTS2[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT"))
    # Other ways to convert to POSIXct
    # timeStamp <- data.frame(as.POSIXct(as.character(paste$paste), format="%m/%d/%Y %H:%M:%S"))
    
    # Create dTrak dataframe
    dTrakDf <- df[(which(df == "Elapsed Time [s]") + 1):nrow(df),1:ncol(df)]
    dTrakDf <- mutate_all(dTrakDf, function(makeNum) as.numeric(as.character(makeNum))) # Convert to numeric
    # dTrakDf <- cbind(dTrakTS, dTrakDf) # Combine dTrakDf with timeStamp
    
    # Generate TS column for dTrakDf 
    # must be separate frome dTrakDf2 to account for possible different lengths in the files
    ts = 1
    dTrakError <- FALSE # check for system resets
    while(ts <= (nrow(dTrakDf) - 1)){
      
      dTrakTS[ts+1,] <- dTrakTS[1,1] + dTrakDf[ts,1]
      
      ts = ts + 1
      
      # Check for system resets
      if(is.na(dTrakDf[ts,1] > dTrakDf[ts+1,1]) != TRUE){
        
        if(dTrakDf[ts,1] > dTrakDf[ts+1,1]){
          
          dTrakTS[ts+1,] <- dTrakTS[1,1] + dTrakDf[ts,1]
          ts = ts + 1
          dTrakError <- TRUE
          print(paste0("DustTrak System Reset detected for ", processList[i]))
          break
        }
      }
      
    } # End while(ts <= (nrow(dTrakDf) - 1)) condition
    
    # Account for sensor restarts 
    # Seconds from start time reset, and new start time must be used
    if(dTrakError == TRUE){
      
      # Make an initial dataframe from Date only
      tempDf <- slice(df[5], (which(df[4] == "Test Start Date")))
      
      # Add time to tempDf & tempDf2
      tempDf <- cbind(tempDf, slice(df[5], (which(df[4] == "Test Start Time"))))
      # Rename columns
      colnames(tempDf) <- c("V1","V2")
      
      # Create new dataframe with standard date/time stamp using Date/Time from columns 1 and 2.
      dTrakTSError <- data.frame(as.POSIXct(paste(tempDf$V1, format(strptime(tempDf$V2, "%I:%M:%S %p"), 
                                       format="%H:%M:%S")), format="%m/%d/%Y %H:%M:%S", tz = "GMT"))
      # dTrakTSError <- data.frame(with(tempDf, mdy(V1) + hms(V2)))
      
      # Convert from UTC to GMT...
      # dTrakTSError <- data.frame(as.POSIXct(as.character(dTrakTSError[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT"))
      
      # Insert new start time into dTrakTS
      dTrakTS[ts+1,] <- dTrakTSError[1,1]
      ts = ts + 1
      
      while(ts <= (nrow(dTrakDf) - 1)){
        
        dTrakTS[(ts+1),] <- dTrakTSError[1,1] + dTrakDf[(ts-1),1]
        
        ts = ts + 1
        
      } # End while(ts <= (nrow(dTrakDf) - 1)) condition
      
    } # End if(dTrakError == TRUE) condition
    
    dTrakError <- FALSE # reset error check
    dTrakDf[,1] <- as.POSIXct(as.character(dTrakTS[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
    
    dTrakDf2 <- df2[(which(df2 == "Elapsed Time [s]") + 1):nrow(df2),1:ncol(df2)]
    dTrakDf2 <- mutate_all(dTrakDf2, function(makeNum) as.numeric(as.character(makeNum))) # Convert to numeric
    # dTrakDf2 <- cbind(dTrakTS2, dTrakDf2) # Combine dTrakDf with timeStamp
    
    # Generate TS column for dTrakDf2 
    # must be separate frome dTrakDf to account for possible different lengths in the files
    ts = 1
    while(ts <= (nrow(dTrakDf2) - 1)){
      
      dTrakTS2[ts+1,] <- dTrakTS2[1,1] + dTrakDf2[ts,1]
      
      ts = ts + 1
      
      # Check for system resets
      if(is.na(dTrakDf2[ts,1] > dTrakDf2[ts+1,1]) != TRUE){
        
        if(dTrakDf2[ts,1] > dTrakDf2[ts+1,1]){
          
          dTrakTS2[ts+1,] <- dTrakTS2[1,1] + dTrakDf2[ts,1]
          ts = ts + 1
          dTrakError <- TRUE
          print(paste0("DustTrak System Reset detected for ", processList[i]))
          break
        }
      }
      
    } # end while(ts <= (nrow(dTrakDf2) - 1)) condition
    
    # Account for sensor restarts 
    # Seconds from start time reset, and new start time must be used
    if(dTrakError == TRUE){
      
      # Make an initial dataframe from Date only
      tempDf2 <- slice(df2[5], (which(df2[4] == "Test Start Date")))
      
      # Add time to tempDf & tempDf2
      tempDf2 <- cbind(tempDf2, slice(df2[5], (which(df2[4] == "Test Start Time"))))
      # Rename columns
      colnames(tempDf2) <- c("V1","V2")
      
      # Create new dataframe with standard date/time stamp using Date/Time from columns 1 and 2.
      dTrakTSError2 <- data.frame(as.POSIXct(paste(tempDf2$V1, format(strptime(tempDf$V2, "%I:%M:%S %p"), 
                                            format="%H:%M:%S")), format="%m/%d/%Y %H:%M:%S", tz = "GMT"))
      # dTrakTSError2 <- data.frame(with(tempDf2, mdy(V1) + hms(V2)))
      
      # Convert from UTC to GMT...
      # dTrakTSError2 <- data.frame(as.POSIXct(as.character(dTrakTSError2[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT"))
      
      # Insert new start time into dTrakTS
      dTrakTS2[ts+1,] <- dTrakTSError2[1,1]
      ts = ts + 1
      
      while(ts <= (nrow(dTrakDf2) - 1)){
        
        dTrakTS2[(ts+1),] <- dTrakTSError2[1,1] + dTrakDf2[(ts-1),1]
        
        ts = ts + 1
        
      } # End while(ts <= (nrow(dTrakDf) - 1)) condition
      
    } # End if(dTrakError == TRUE) condition
    
    dTrakDf2[,1] <- as.POSIXct(as.character(dTrakTS2[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
    
    # Create temporary metDf
    metDf <- masterMetDF 
    # colnames(masterMetDF) <- paste0("V",1:ncol(masterMetDF))
    
    # Convert MET TIMESTAMP to POSIXct
    metTS <- data.frame(as.POSIXct(as.character(metDf$V1), format="%Y-%m-%d %H:%M:%S", tz = "GMT"))
    metDf <- mutate_all(metDf, function(makeNum) as.numeric(as.character(makeNum)))
    metDf$V1 <- data.frame(as.POSIXct(as.character(metDf$V1), format="%Y-%m-%d %H:%M:%S", tz = "GMT"))
    metDf[,1] <- metTS[,1]
    tempMetDf <- metDf[(which(metDf[,1] == dTrakDf[1,1]) + 1),]
    
    # Create masterTime index
    offsetdTrakTS <- data.frame(dTrakTS)
    offsetdTrakTS[,1] <- offsetdTrakTS[,1] + 60
    masterTime <- data.frame(offsetdTrakTS[1,1]) # Start time
    c2 = 1 # secondary counter to count through all MET times
    for(c in 1:nrow(offsetdTrakTS)){
      
      # Insert timestamp from dustTrack df
      masterTime[c2,1] <- offsetdTrakTS[c,1] 
      c2 = c2 + 1
      # Add 1 minute to previous time
      masterTime[c2,1] <- offsetdTrakTS[c,1] + 60
      c2 = c2 + 1
    }
    
    
    # Select timestamp range from metDF corresponding to dTrakDf
    tempMT <- masterTime
    tempMT <- round(tempMT[,1], units = "mins") # Round off seconds in case of system restart.
    tempMT <- data.frame(tempMT)
    for(c in 1:nrow(tempMT)){
      tempMetDf[c,] <- metDf[which(metDf[,1] == tempMT[c,1]),] # Select 
      c = c + 1
    }
    
    metDf <- tempMetDf
    
    #---------------------------
    # Generate headers.
    #---------------------------
    
    # split dust dTrak. data from header
    # Create header
    dTrakHead <- data.frame(lapply(slice(df[35,1:ncol(df)]), as.character), stringsAsFactors = FALSE)
    
    # Convert serial number to height
    df <- data.frame(lapply(df, function(height15) {gsub("8533171102", "1.5m", height15)}), stringsAsFactors = FALSE)
    df <- data.frame(lapply(df, function(height30) {gsub("8533171012", "3.0m", height30)}), stringsAsFactors = FALSE)
    
    # Insert height into column headers
    dTrakHead[,1:8] <- paste0(dTrakHead[,1:8], " ", df[2,2])
    
    # Create header for second df
    dTrakHead2 <- data.frame(lapply(slice(df2[35,1:ncol(df2)]), as.character), stringsAsFactors = FALSE)
    
    # Convert serial number to height
    df2 <- data.frame(lapply(df2, function(height15) {gsub("8533171102", "1.5m", height15)}), stringsAsFactors = FALSE)
    df2 <- data.frame(lapply(df2, function(height30) {gsub("8533171012", "3.0m", height30)}), stringsAsFactors = FALSE)
    
    # Insert height into column headers
    dTrakHead2[,1:8] <- paste0(dTrakHead2[,1:8], " ", df2[2,2])
    
    # Remove unnecessary columns
    cutHead <- c(1,7,8)
    dTrakHead[,cutHead] <- NULL
    dTrakHead2[,cutHead] <- NULL
    
    #Split MET data from header
    metHead <- metStationHead[2,]
    metHead[,2] <- NULL # Remove record no. column
    
    # Replace placeholder heights with actual heights
    metHead <- data.frame(lapply(metHead, function(height48) {gsub("_4_", "_4.8m_", height48)}), stringsAsFactors = FALSE)
    metHead <- data.frame(lapply(metHead, function(height24) {gsub("_3_", "_2.4m_", height24)}), stringsAsFactors = FALSE)
    metHead <- data.frame(lapply(metHead, function(height14) {gsub("_2_", "_1.4m_", height14)}), stringsAsFactors = FALSE)
    metHead <- data.frame(lapply(metHead, function(height07) {gsub("_1_", "_0.7m_", height07)}), stringsAsFactors = FALSE)
    
    tempMetHead <- data.frame(as.character(metStationHead[2,1])) # Pull metadata in column 1
    # Convert column 1 to POSIXct
    metHead[,1] <- as.POSIXct(as.character(metHead[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
    
    # Bookmark columns with column headers
    metResHead <- c(2:18)
    
    
    
    #==========================================================
    # Generate dTrakStats and metStats with 2 minute resolution
    #==========================================================
    
    # 2 min resolution
    if (timeRes == 2){
      
      # Average metDf to 2 min resolution ****(11:01:00 - 11:02:00 averaged and labelled as 11:00:00)****
      metDf$Time2 <- as.factor(ceiling_date(metDf[,1], "2 minutes"))
      metDf[is.nan(metDf)] <- NA # Replace NaNs with NA
      metDf2min <- aggregate(.~Time2, metDf[,2:ncol(metDf)], FUN=mean, na.rm = TRUE, na.action = na.pass) # aggregate() appears to ignore NaNs...!
      
      # convert wind dir from degrees to radians
      metDf$winddir_rad <- metDf$V14*(3.14/180)
      # calculate 2min average wind dir and replace wind direction column in metDf2min
      tempMetDf2min <- aggregate(.~Time2, metDf[ ,c("Time2", "winddir_rad")], FUN=circ.mean)
      
      # convert back to degrees
      tempMetDf2min$winddir2_deg <- (tempMetDf2min$winddir_rad)*(180/3.14)
      tempMetDf2min$winddir2_deg <- ifelse(tempMetDf2min$winddir2_deg < 0,tempMetDf2min$winddir2_deg + 360, tempMetDf2min$winddir2_deg) 
      tempMetDf2min$winddir2_deg <- format(tempMetDf2min$winddir2_deg, scientific = FALSE) 
      
      # replace degrees column in metDf2min 
      metDf2min$V14 <- tempMetDf2min$winddir2_deg
      
      # sum rainfall total, sensor total, and sensor sec in metDf
      tempMetDf2min <- aggregate(.~Time2, metDf[ ,c("Time2", "V3", "V11", "V12")], FUN=sum, na.rm = TRUE, na.action = na.pass)
      metSums <- c("V3", "V11", "V12") # vector of summed columns
      # replace summed columns with averaged columns in metDf2min
      metDf2min[,metSums] <- tempMetDf2min[,metSums]
      
      # Convert new timestamps to POSIXct
      metDf2min[,1] <- as.POSIXct(as.character(metDf2min[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
      # Subtract 2 minutes from all times to match dTrak data
      metDf2min[,1] <- metDf2min[,1] - 120
      
      # Create masterDf
      finalMetDf <- metDf2min
      finaldTrakDf <- dTrakDf
      finaldTrakDf2 <- dTrakDf2
      
      # Set resolution for excelName
      resName <- "2_min_avg"
      
      
    } # End if(timeRes == 2) condition
    
    
    #==========================================================
    # Generate dTrakStats and metStats with 10 minute resolution
    #==========================================================
    
    # 10 min resolution
    if (timeRes == 10){
      
      # Average metDf to 2 min resolution ****(11:01:00 - 11:10:00 averaged and labelled as 11:00:00)****
      metDf$Time10 <- as.factor(ceiling_date(metDf[,1], "10 minutes"))
      metDf[is.nan(metDf)] <- NA # Replace NaNs with NA
      metDf10min <- aggregate(.~Time10, metDf[,2:ncol(metDf)], FUN=mean, na.rm = TRUE, na.action = na.pass) # aggregate() appears to ignore NaNs...!
      
      # convert wind dir from degrees to radians
      metDf$winddir_rad <- metDf$V14*(3.14/180)
      # calculate 2min average wind dir and replace wind direction column in metDf2min
      tempMetDf10min <- aggregate(.~Time10, metDf[ ,c("Time10", "winddir_rad")], FUN=circ.mean)
      
      # convert back to degrees
      tempMetDf10min$winddir2_deg <- (tempMetDf10min$winddir_rad)*(180/3.14)
      tempMetDf10min$winddir2_deg <- ifelse(tempMetDf10min$winddir2_deg < 0,tempMetDf10min$winddir2_deg + 360, tempMetDf10min$winddir2_deg) 
      tempMetDf10min$winddir2_deg <- format(tempMetDf10min$winddir2_deg, scientific = FALSE) 
      
      # replace degrees column in metDf2min 
      metDf10min$V14 <- tempMetDf10min$winddir2_deg
      
      # sum rainfall total, sensor total, and sensor sec in metDf
      tempMetDf10min <- aggregate(.~Time10, metDf[ ,c("Time10", "V3", "V11", "V12")], FUN=sum, na.rm = TRUE, na.action = na.pass)
      metSums <- c("V3", "V11", "V12") # vector of summed columns
      # replace summed columns with averaged columns in metDf2min
      metDf10min[,metSums] <- tempMetDf10min[,metSums]
      
      # Convert new timestamps to POSIXct
      metDf10min[,1] <- as.POSIXct(as.character(metDf10min[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
      # Subtract 2 minutes from all times to match dTrak data
      metDf10min[,1] <- metDf10min[,1] - 600
      
      # Average dTrakDfs to 10 min resolution
      # process dTrakDf
      dTrakDf$Time10 <- as.factor(ceiling_date(dTrakDf[,1], "10 minutes"))
      dTrakDf[is.nan(dTrakDf)] <- NA # Replace NaNs with NA
      dTrakDf10min <- aggregate(.~Time10, dTrakDf[,2:ncol(dTrakDf)], FUN=mean, na.rm = TRUE, na.action = na.pass) # aggregate() appears to ignore NaNs...!
      
      # process dTrakDf2
      dTrakDf2$Time10 <- as.factor(ceiling_date(dTrakDf2[,1], "10 minutes"))
      dTrakDf2[is.nan(dTrakDf2)] <- NA # Replace NaNs with NA
      dTrakDf10min2 <- aggregate(.~Time10, dTrakDf2[,2:ncol(dTrakDf2)], FUN=mean, na.rm = TRUE, na.action = na.pass) # aggregate() appears to ignore NaNs...!
      
      # Create masterDf
      finalMetDf <- metDf10min
      finaldTrakDf <- dTrakDf10min
      finaldTrakDf2 <- dTrakDf10min2
      
      # Set resolution for excelName
      resName <- "10_min_avg"
      
      
    } # End if(timeRes == 10) condition
    
    
    #----------------------------------------
    # Merge dfs and generate spreadsheet
    #----------------------------------------  
    
    # Excel spreadsheet filename.
    tabName <- paste0(dTrakDate[i], "_", dTrakSite[i], "_", resName)
    excelName <- paste0(oDir,"/",dTrakID[i],"_Merged_DustTrak_and_MET_data_",resName,".xlsx")
    
    # Find shortest Df
    short <- c(nrow(finaldTrakDf), nrow(finaldTrakDf2), nrow(finalMetDf))
    clipRows <- min(short)
    
    # Clip all Dfs to same length
    finaldTrakDf <- finaldTrakDf[1:clipRows,]
    finaldTrakDf2 <- finaldTrakDf2[1:clipRows,]
    finalMetDf <- finalMetDf[1:clipRows,]
    
    # Check for consistency in timestamps between MET and OPS data
    if(all(finalMetDf[,1] != finaldTrakDf[,1]) || all(finalMetDf[,1] != finaldTrakDf2[,1])){
      
      print(paste0("Warning! Timestamps in ",dTrakID[i], " not consistent with MET data timestamps!"))
      
    }
    
    # Check for errors
    finaldTrakDf$V8[finaldTrakDf$V7 == ""] <- NA
    finaldTrakDf$V8[finaldTrakDf$V8 == ""] <- NA
    if(all(is.na(finaldTrakDf[,7:8])) == FALSE){
      print(paste0("Warning! Entry found in Error/Alarm columns in ", processList[i]))
    }
    
    finaldTrakDf2$V8[finaldTrakDf2$V7 == ""] <- NA
    finaldTrakDf2$V8[finaldTrakDf2$V8 == ""] <- NA
    if(all(is.na(finaldTrakDf2[,7:8])) == FALSE){
      print(paste0("Warning! Entry found in Error/Alarm columns in ", processList[i+1]))
    }
    
    # Remove Error/Alarm columns
    finaldTrakDf[,8] <- NULL
    finaldTrakDf[,7] <- NULL 
    finaldTrakDf2[,8] <- NULL
    finaldTrakDf2[,7] <- NULL
    
    # Remove timestamps from finaldTrakDfs
    finaldTrakDf[,1] <- NULL 
    finaldTrakDf2[,1] <- NULL
    
    # Coerce names of head and df to be identical
    names(dTrakHead) <- names(finaldTrakDf) 
    names(dTrakHead2) <- names(finaldTrakDf2) 
    names(metHead) <- names(finalMetDf) 
    
    # Merge Dfs
    gattaidTrak <- rbind(dTrakHead, finaldTrakDf)
    gattaidTrak2 <- rbind(dTrakHead2, finaldTrakDf2)
    gattaidTrak <- cbind(gattaidTrak, gattaidTrak2)
    gattaiMet <- rbind(metHead, finalMetDf)
    
    gattaiDf <- cbind(gattaiMet, gattaidTrak)
    
    # Convert column 1 to character class'
    gattaiDf[,1] <- as.character(gattaiDf[,1])
    
    # Paste metadata into first column
    gattaiDf[1,1] <- as.character(tempMetHead[1,1])
    
    # Write first tab.
    excelWB <- createWorkbook()
    addWorksheet(excelWB, tabName)
    writeData(excelWB, sheet = tabName, gattaiDf, colNames = FALSE)
    
    # Auto-adjust column width and save. Slow...
    # Auto-adjust code inspiration: https://stackoverflow.com/questions/45860085/r-autofit-excel-column-width
    columnWidth2 <- apply(gattaiDf[1:nrow(gattaiDf),], 
                          2, function(yy) max(nchar(as.character(yy)) + 2, na.rm = TRUE)) 
    setColWidths(excelWB, tabName, cols = 1:ncol(gattaiDf), widths = columnWidth2)

    
    # Save workbook.
    saveWorkbook(excelWB, excelName, overwrite = TRUE)
    
    print("Complete!") # Print progress
  
    i = i + 2
    
  } # End for(i in 1:length(processList)) loop
  
  # Remove masterMetDF
  rm(masterMetDF)
  
} # End for(q in 1: length(dTrakName)) loop

# Print progress
print("Processing for all files complete. Goodbye") 