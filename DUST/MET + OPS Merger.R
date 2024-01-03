#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# FILE: MET + OPS Merger
# AUTHORS: Christopher Felt ERDC-CRREL-NH
# DATE CREATED: 02 August 2020
# Version: 0.30
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# PURPOSE
# =======
#
# Combines OPS dust concentration data with MET station data in a 
# new Excel spreadsheet.
# Resolves the resolution disparity between the two data sets and 
# allows the user to define 1, 5, 10, or 15 minute resolutions.
# 
# 
# ASSUMPTIONS
# =======
#
# OPS raw data resolution must be 1 second.
# MET station raw data resolution must be 1 minute.
# Raw data formatting must be consistent with JER OPS/MET data.
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
library(CircStats)          # circ.mean FUN
library(raster)
library(RcppRoll)           # roll_sum()



#----------------------------------------------------------------------
# Set hard-coded parameters.
#----------------------------------------------------------------------

# I/O directories
oDir  <- '~/DUST/Test/Output'
iDir  <- '~/DUST/Test'

# Set time resolution to 1, 5, 10, or 15 minutes.
timeRes <- 1

# Set site names for dust concentration data
concName <- c("Site_3", "Site_4", "Site_5")

# Set site names for MET data (NOTE: must correspond 1:1 with site names in concName)
metName <- c("Site3", "Site4", "Site5")

RUNNING <- timeRes

ma <- function(x, n = RUNNING){stats::filter(x,rep(1/n,n), sides=1)}

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
concList <- list.files(iDir, pattern="*.csv")

for(q in 1:length(concName)){

  #============================
  # Process MET data files.
  #============================

    # Generate file lists...
    metFileSite <- grep(metName[q], metList)
    metSiteList <- metList[metFileSite]
    metSiteList <- paste0(iDir,"/",metSiteList)
    
    # Announce progress
    print(paste0(concName[q],":"))
    print("Generating MET dataframe from the following files...")
    
    # Loop through and combine all available files for current site
    z = 1
    rm(masterMetDf) # Remove masterMetDf from the global environment if it exists
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
  # Process dust concentration data files.
  #========================================
  
  # Import .csv files from the input directory
  processConc <- grep(concName[q], concList)
  processList <- concList[processConc]
  
  # Find site/date info in dust conc. file names
  concDate <- substr(processList, start = 0, stop = 8) # Seperate dust conc. date
  concSite <- substr(processList, start = 10, stop = 15) # Separate dust conc. sites
  concSite <- paste0(substr(concSite, start = 1, stop = 4),substr(concSite, start = 6, stop = 6))
  
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
  
  tempConcID <- removeTail(processList, sep = "_", del = 1)
  tempConcID <- gsub("^.*?_OPS","",tempConcID) 
  concID <- removeTail2(processList, sep = "_", del = 3)
  
  concID <- paste0(concID, tempConcID)
  
  
  
  # Loop through and process each dust concentration .csv
  i = 1
  while(i <= length(processList)){
    
    # Print progress
    print(paste0("Processing files ", processList[i], " and ", processList[i+1],"..."))
    if(concDate[i] != concDate[i+1]){
      
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
    tempDf <- slice(df[2], (which(df[2] == "Date")+1:n()))
    tempDf2 <- slice(df2[2], (which(df2[2] == "Date")+1:n()))
    
    # Add time to tempDf & tempDf2
    tempDf <- cbind(tempDf, slice(df[3], (which(df[3] == "Start Time")+1:n())))
    tempDf2 <- cbind(tempDf2, slice(df2[3], (which(df2[3] == "Start Time")+1:n())))
    
    # Create new dataframe with standard date/time stamp using Date/Time from columns 1 and 2.
    concTS <- data.frame(with(tempDf, mdy(V2) + hms(V3)))
    concTS2 <- data.frame(with(tempDf2, mdy(V2) + hms(V3)))

    # Convert from UTC to GMT...
    concTS <- data.frame(as.POSIXct(as.character(concTS[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT"))
    concTS2 <- data.frame(as.POSIXct(as.character(concTS2[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT"))
    # Other ways to convert to POSIXct
    # timeStamp <- data.frame(as.POSIXct(as.character(paste$paste), format="%m/%d/%Y %H:%M:%S"))
    
    # Create dust conc. dataframe
    concDf <- df[14:nrow(df),4:ncol(df)]
    concDf <- mutate_all(concDf, function(makeNum) as.numeric(as.character(makeNum))) # Convert to numeric
    concDf <- cbind(concTS, concDf) # Combine concDf with timeStamp
    
    concDf2 <- df2[14:nrow(df2),4:ncol(df2)]
    concDf2 <- mutate_all(concDf2, function(makeNum) as.numeric(as.character(makeNum))) # Convert to numeric
    concDf2 <- cbind(concTS2, concDf2) # Combine concDf with timeStamp
    
    # Temporarily replace NAs with 0s (necessary for aggregate() to work)
    concDf[,c("V7","V8","V17","V30","V31","V44","V45")] <- 0
    concDf2[,c("V7","V8","V17","V30","V31","V44","V45")] <- 0
    # selectively target NAs...
    # concDf[[c("V7","V8","V17","V30","V31","V44","V45")]]
    # [is.na(concDf[[c("V7","V8","V17","V30","V31","V44","V45")]])] <- 0
    
    # Create temporary metDf
    metDf <- masterMetDF 
    # colnames(masterMetDF) <- paste0("V",1:ncol(masterMetDF))
    
    # Convert MET TIMESTAMP to POSIXct
    metTS <- data.frame(as.POSIXct(as.character(metDf$V1), format="%Y-%m-%d %H:%M:%S", tz = "GMT"))
    metDf <- mutate_all(metDf, function(makeNum) as.numeric(as.character(makeNum)))
    metDf$V1 <- data.frame(as.POSIXct(as.character(metDf$V1), format="%Y-%m-%d %H:%M:%S", tz = "GMT"))
    metDf[,1] <- metTS[,1]
    
    # Average concDf to 1 min resolution ****(11:00:00 - 11:00:59 averaged and labelled as 11:01:00)****
    concDf$Time1 <- as.factor(ceiling_date(concDf[,1], "1 minutes"))
    concDf[is.nan(concDf)] <- NA # Replace NaNs with NA
    concDf1min <- aggregate(.~Time1, concDf[,2:ncol(concDf)], FUN=mean, na.rm = TRUE, na.action = na.pass) # aggregate() appears to ignore NaNs...!
    concDf1min[is.nan(concDf1min)] <- NA # Replace NaNs with NA
    
    # Apply FUN=sum Dead_Time column in concDf
    concSums <- c("V10") # bookmark column name
    tempConcDf1min <- aggregate(.~Time1, concDf[ ,c("Time1", "V10")], FUN=sum, na.rm = TRUE, na.action = na.pass) # add 'em up!
    concDf1min[,concSums] <- tempConcDf1min[,concSums] # replace old column with summed column
    
    concDf2$Time1 <- as.factor(ceiling_date(concDf2[,1], "1 minutes"))
    concDf2[is.nan(concDf2)] <- NA # Replace NaNs with NA
    concDf1min2 <- aggregate(.~Time1, concDf2[,2:ncol(concDf2)], FUN=mean, na.rm = TRUE, na.action = na.pass)
    concDf1min2[is.nan(concDf1min2)] <- NA # Replace NaNs with NA
    
    # Apply FUN=sum Dead_Time column in concDf
    tempConcDf1min2 <- aggregate(.~Time1, concDf2[ ,c("Time1", "V10")], FUN=sum, na.rm = TRUE, na.action = na.pass) # add 'em up!
    concDf1min2[,concSums] <- tempConcDf1min2[,concSums] # replace old column with summed column
    
    # Convert new timestamps to POSIXct
    concDf1min[,1] <- as.POSIXct(as.character(concDf1min[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
    concDf1min2[,1] <- as.POSIXct(as.character(concDf1min2[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
    
    # Select timestamp range from metDF corresponding to concDf1min
    metDf <- metDf[which(metDf[,1] == concDf1min[1,1]):which(metDf[,1] == concDf1min[nrow(concDf1min),1]),]
    
    
    #---------------------------
    # Generate headers.
    #---------------------------
    
    # split dust conc. data from header
    # Create header
    concHead <- data.frame(lapply(slice(df[,3:ncol(df)], (9:13)), as.character), stringsAsFactors = FALSE)
    # Copy metadata into new head
    concHead[1:2,1:2] <- data.frame(lapply(df[1:2,1:2], as.character), stringsAsFactors = FALSE)
    concHead[1:2,4:5] <- data.frame(lapply(df[3:4,1:2], as.character), stringsAsFactors = FALSE)
    concHead[1:2,7:8] <- data.frame(lapply(df[5:6,1:2], as.character), stringsAsFactors = FALSE)
    concHead[1:2,10:11] <- data.frame(lapply(df[7:8,1:2], as.character), stringsAsFactors = FALSE)
    concHead[5,1] <- NA
    concHead[6,] <- NA # Place holder for resolution column headers
    
    # Convert serial number to height
    concHead <- data.frame(lapply(concHead, function(height15) {gsub("3330171003", "1.5m", height15)}), stringsAsFactors = FALSE)
    concHead <- data.frame(lapply(concHead, function(height30) {gsub("3330171004", "3.0m", height30)}), stringsAsFactors = FALSE)
    concHead <- data.frame(lapply(concHead, function(metaHeight) {
      gsub("Instrument Serial Number", "OPS Height", metaHeight)}), stringsAsFactors = FALSE)
    
    # Narrow down to single row
    tempConcHead <- concHead
    concHead <- concHead[5,]
    concHead[1,15:27] <- tempConcHead[2,15:27]
    concHead[,1:ncol(concHead)] <- paste0(concHead[,1:ncol(concHead)], "_", tempConcHead[2,2])
    
    # Convert "(" and ")" to "" and " " to "_" in concHead
    concHead <- data.frame(lapply(concHead, function(bracket1) {gsub("\\(","", bracket1)}), stringsAsFactors = FALSE)
    concHead <- data.frame(lapply(concHead, function(bracket2) {gsub(")","", bracket2)}), stringsAsFactors = FALSE)
    concHead <- data.frame(lapply(concHead, function(space) {gsub(" ","_", space)}), stringsAsFactors = FALSE)
    
    # Add "UB_" prefix to all UB columns
    concHead[1,16:27] <- paste0("UB_", concHead[1,16:27])
    
    #------------------------------
    # Create header for second df
    concHead2 <- data.frame(lapply(slice(df2[,3:ncol(df2)], (9:13)), as.character), stringsAsFactors = FALSE)
    # Copy metadata into new head
    concHead2[1:2,1:2] <- data.frame(lapply(df2[1:2,1:2], as.character), stringsAsFactors = FALSE)
    concHead2[1:2,4:5] <- data.frame(lapply(df2[3:4,1:2], as.character), stringsAsFactors = FALSE)
    concHead2[1:2,7:8] <- data.frame(lapply(df2[5:6,1:2], as.character), stringsAsFactors = FALSE)
    concHead2[1:2,10:11] <- data.frame(lapply(df2[7:8,1:2], as.character), stringsAsFactors = FALSE)
    concHead2[5,1] <- NA
    concHead2[6,] <- NA # Place holder for resolution column headers
    
    # Convert serial number to height
    concHead2 <- data.frame(lapply(concHead2, function(height15) {gsub("3330171003", "1.5m", height15)}), stringsAsFactors = FALSE)
    concHead2 <- data.frame(lapply(concHead2, function(height30) {gsub("3330171004", "3.0m", height30)}), stringsAsFactors = FALSE)
    concHead2 <- data.frame(lapply(concHead2, function(metaHeight) {
      gsub("Instrument Serial Number", "OPS Height", metaHeight)}), stringsAsFactors = FALSE)
    
    # Narrow down to single row
    tempConcHead2 <- concHead2
    concHead2 <- concHead2[5,]
    concHead2[1,15:27] <- tempConcHead2[2,15:27]
    concHead2[,1:ncol(concHead2)] <- paste0(concHead2[,1:ncol(concHead2)], "_", tempConcHead2[2,2])
    
    # Convert "(" and ")" to "" and " " to "_" in concHead
    concHead2 <- data.frame(lapply(concHead2, function(bracket1) {gsub("\\(","", bracket1)}), stringsAsFactors = FALSE)
    concHead2 <- data.frame(lapply(concHead2, function(bracket2) {gsub(")","", bracket2)}), stringsAsFactors = FALSE)
    concHead2 <- data.frame(lapply(concHead2, function(space) {gsub(" ","_", space)}), stringsAsFactors = FALSE)
    
    # Add "UB_" prefix to all UB columns
    concHead2[1,16:27] <- paste0("UB_", concHead2[1,16:27])
    
    # Bookmark columns with column headers
    concResHead <- c(2,3,4,7,8,9,10,11,12,13,14,16,17,18,19,20,21,22,23,24,25,26,27)
    
    #Split MET data from header
    metHead <- metStationHead[2,]
    metHead[,2] <- NULL # Remove record number column
    
    # Replace placeholder heights with actual heights
    metHead <- data.frame(lapply(metHead, function(height48) {gsub("_4_", "_4.8m_", height48)}), stringsAsFactors = FALSE)
    metHead <- data.frame(lapply(metHead, function(height24) {gsub("_3_", "_2.4m_", height24)}), stringsAsFactors = FALSE)
    metHead <- data.frame(lapply(metHead, function(height14) {gsub("_2_", "_1.4m_", height14)}), stringsAsFactors = FALSE)
    metHead <- data.frame(lapply(metHead, function(height07) {gsub("_1_", "_0.7m_", height07)}), stringsAsFactors = FALSE)
    
    tempMetHead <- data.frame(as.character(metHead[1:3,1])) # Pull metadata in column 1
    # Convert column 1 to POSIXct
    metHead[,1] <- as.POSIXct(as.character(metHead[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
    
    # Bookmark columns with column headers
    metResHead <- c(2:18)
    
    
    
    #==========================================================
    # Generate concStats and metStats with 1 minute resolution
    #==========================================================
    
    # 1 min resolution
    if (timeRes == 1){
      
      # Create masterDf
      finalMetDf <- metDf
      finalConcDf <- concDf1min
      finalConcDf2 <- concDf1min2
      
      # Set resolution for excelName
      resName <- "1_min_avg"
      
      
    } # End if(timeRes == 1) condition
    
    
    #==========================================================
    # Generate concStats and metStats with 5 minute resolution
    #==========================================================
    
    # 5 min resolution
    if (timeRes == 5){
      
      # Average metDf to 5 min resolution ****(11:01:00 - 11:05:00 averaged and labelled as 11:05:00)****
      metDf$Time5 <- as.factor(ceiling_date(metDf[,1], "5 minutes"))
      metDf[is.nan(metDf)] <- NA # Replace NaNs with NA
      metDf5min <- aggregate(.~Time5, metDf[,2:ncol(metDf)], FUN=mean, na.rm = TRUE, na.action = na.pass) # aggregate() appears to ignore NaNs...!
      
      # convert wind dir from degrees to radians
      metDf$winddir_rad <- metDf$V14*(3.14/180)
      # calculate 5min average wind dir and replace wind direction column in metDf5min
      tempMetDf5min <- aggregate(.~Time5, metDf[ ,c("Time5", "winddir_rad")], FUN=circ.mean)
      
      # convert back to degrees
      tempMetDf5min$winddir5_deg <- (tempMetDf5min$winddir_rad)*(180/3.14)
      tempMetDf5min$winddir5_deg <- ifelse(tempMetDf5min$winddir5_deg < 0,tempMetDf5min$winddir5_deg + 360, tempMetDf5min$winddir5_deg) 
      tempMetDf5min$winddir5_deg <- format(tempMetDf5min$winddir5_deg, scientific = FALSE) 
      
      # replace degrees column in metDf5min 
      metDf5min$V14 <- tempMetDf5min$winddir5_deg
      
      # sum rainfall total, sensor total, and sensor sec in metDf
      tempMetDf5min <- aggregate(.~Time5, metDf[ ,c("Time5", "V3", "V11", "V12")], FUN=sum, na.rm = TRUE, na.action = na.pass)
      metSums <- c("V3", "V11", "V12") # vector of summed columns
      # replace summed columns with averaged columns in metDf5min
      metDf5min[,metSums] <- tempMetDf5min[,metSums]
      
      # Convert new timestamps to POSIXct
      metDf5min[,1] <- as.POSIXct(as.character(metDf5min[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
      # Subtract 5 minutes from all times
      # metDf5min[,1] <- metDf5min[,1] - 300

      # Average concDf to 5 min resolution ****(11:01:00 - 11:05:00 averaged and labelled as 11:05:00)****
      concDf1min$Time5 <- as.factor(ceiling_date(concDf1min[,1], "5 minutes"))
      concDf5min <- aggregate(.~Time5, concDf1min[,2:ncol(concDf1min)], FUN=mean, na.rm = TRUE, na.action = na.pass) # aggregate() appears to ignore NaNs...!
      # Repeat for second concDf
      concDf1min2$Time5 <- as.factor(ceiling_date(concDf1min2[,1], "5 minutes"))
      concDf5min2 <- aggregate(.~Time5, concDf1min2[,2:ncol(concDf1min2)], FUN=mean, na.rm = TRUE, na.action = na.pass)
      
      # Apply FUN=sum Dead_Time column in concDf1min
      tempConcDf5min <- aggregate(.~Time5, concDf1min[ ,c("Time5", "V10")], FUN=sum, na.rm = TRUE, na.action = na.pass) # add 'em up!
      concDf5min[,concSums] <- tempConcDf5min[,concSums] # replace old column with summed column
      
      # Apply FUN=sum Dead_Time column in concDf1min2
      tempConcDf5min2 <- aggregate(.~Time5, concDf1min2[ ,c("Time5", "V10")], FUN=sum, na.rm = TRUE, na.action = na.pass) # add 'em up!
      concDf5min2[,concSums] <- tempConcDf5min2[,concSums] # replace old column with summed column
      
      # Convert new timestamps to POSIXct
      concDf5min[,1] <- as.POSIXct(as.character(concDf5min[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
      concDf5min2[,1] <- as.POSIXct(as.character(concDf5min2[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
      # Subtract 5 minutes from all times
      # concDf5min[,1] <- concDf5min[,1] - 300
      # concDf5min2[,1] <- concDf5min2[,1] - 300
      
      # Create masterDf
      finalMetDf <- metDf5min
      finalConcDf <- concDf5min
      finalConcDf2 <- concDf5min2
      
      # Set resolution for excelName
      resName <- "5_min_avg"
      
      
    } # End if(timeRes == 5) condition
    
    
    #==========================================================
    # Generate concStats and metStats with 10 minute resolution
    #==========================================================
    
    # 10 min resolution
    if (timeRes == 10){
      
      # Average metDf to 10 min resolution ****(11:01:00 - 11:10:00 averaged and labelled as 11:00:00)****
      metDf$Time10 <- as.factor(ceiling_date(metDf[,1], "10 minutes"))
      metDf[is.nan(metDf)] <- NA # Replace NaNs with NA
      metDf10min <- aggregate(.~Time10, metDf[,2:ncol(metDf)], FUN=mean, na.rm = TRUE, na.action = na.pass) # aggregate() appears to ignore NaNs...!
      
      # convert wind dir from degrees to radians
      metDf$winddir_rad <- metDf$V14*(3.14/180)
      # calculate 10min average wind dir and replace wind direction column in metDf10min
      tempMetDf10min <- aggregate(.~Time10, metDf[ ,c("Time10", "winddir_rad")], FUN=circ.mean)
      
      # convert back to degrees
      tempMetDf10min$winddir10_deg <- (tempMetDf10min$winddir_rad)*(180/3.14)
      tempMetDf10min$winddir10_deg <- ifelse(tempMetDf10min$winddir10_deg < 0,tempMetDf10min$winddir10_deg + 360, tempMetDf10min$winddir10_deg) 
      tempMetDf10min$winddir10_deg <- format(tempMetDf10min$winddir10_deg, scientific = FALSE) 
      
      # replace degrees column in metDf10min 
      metDf10min$V14 <- tempMetDf10min$winddir10_deg
      
      # sum rainfall total, sensor total, and sensor sec in metDf
      tempMetDf10min <- aggregate(.~Time10, metDf[ ,c("Time10", "V3", "V11", "V12")], FUN=sum, na.rm = TRUE, na.action = na.pass)
      metSums <- c("V3", "V11", "V12") # vector of summed columns
      # replace summed columns with averaged columns in metDf10min
      metDf10min[,metSums] <- tempMetDf10min[,metSums]
      
      # Convert new timestamps to POSIXct
      metDf10min[,1] <- as.POSIXct(as.character(metDf10min[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
      # Subtract 10 minutes from all times
      # metDf10min[,1] <- metDf10min[,1] - 600
      
      # Average concDf to 10 min resolution ****(11:01:00 - 11:10:00 averaged and labelled as 11:10:00)****
      concDf1min$Time10 <- as.factor(ceiling_date(concDf1min[,1], "10 minutes"))
      concDf10min <- aggregate(.~Time10, concDf1min[,2:ncol(concDf1min)], FUN=mean, na.rm = TRUE, na.action = na.pass) # aggregate() appears to ignore NaNs...!
      # Repeat for second concDf
      concDf1min2$Time10 <- as.factor(ceiling_date(concDf1min2[,1], "10 minutes"))
      concDf10min2 <- aggregate(.~Time10, concDf1min2[,2:ncol(concDf1min2)], FUN=mean, na.rm = TRUE, na.action = na.pass)
      
      # Apply FUN=sum Dead_Time column in concDf1min
      tempConcDf10min <- aggregate(.~Time10, concDf1min[ ,c("Time10", "V10")], FUN=sum, na.rm = TRUE, na.action = na.pass) # add 'em up!
      concDf10min[,concSums] <- tempConcDf10min[,concSums] # replace old column with summed column
      
      # Apply FUN=sum Dead_Time column in concDf1min2
      tempConcDf10min2 <- aggregate(.~Time10, concDf1min2[ ,c("Time10", "V10")], FUN=sum, na.rm = TRUE, na.action = na.pass) # add 'em up!
      concDf10min2[,concSums] <- tempConcDf10min2[,concSums] # replace old column with summed column
      
      # Convert new timestamps to POSIXct
      concDf10min[,1] <- as.POSIXct(as.character(concDf10min[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
      concDf10min2[,1] <- as.POSIXct(as.character(concDf10min2[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
      # Subtract 10 minutes from all times
      # concDf10min[,1] <- concDf10min[,1] - 600
      # concDf10min2[,1] <- concDf10min2[,1] - 600
      
      # Create masterDf
      finalMetDf <- metDf10min
      finalConcDf <- concDf10min
      finalConcDf2 <- concDf10min2
      
      # Set resolution for excelName
      resName <- "10_min_avg"
      
      
    } # End if(timeRes == 10) condition
    
    
    #==========================================================
    # Generate concStats and metStats with 15 minute resolution
    #==========================================================
    
    # 15 min resolution
    if (timeRes == 15){
      
      # Remove first entry of metDf and generate new df for current resolution ****(11:01:00 - 11:15:00 averaged and labelled as 11:15:00)****
      metDf <- metDf[-1,]
      metDf15min <- metDf
      
      # Apply ma() function to all columns of new df besides timestamp
      metDf15min[,2:ncol(metDf15min)] <- as.numeric(ma(metDf15min[,2:ncol(metDf15min)]))
      # test accuracy of ma() function sum(metDf[1:15,17])/15
      
      # convert wind dir from degrees to radians
      metDf$winddir_rad <- metDf$V14*(3.14/180)
      # create temporary vectors to contain moving circ.mean
      tempCircDf <- metDf$winddir_rad
      circMeanDf <- metDf$winddir_rad
      
      # Calculate circular mean in a 15 minute moving window ****(11:01:00 - 11:15:00 averaged and labelled as 11:15:00)****
      movCirc = 1
      while(movCirc <= length(tempCircDf)){
        
        if((movCirc - RUNNING) < 0){circMeanDf[movCirc] <- NA} # first 14 mins NA
        if((movCirc - RUNNING) >= 0){circMeanDf[movCirc] <- circ.mean(tempCircDf[((movCirc + 1) - RUNNING):movCirc])}
        
        movCirc <- movCirc + 1
      }
      # convert temporary vector to df
      tempMetDf15min <- as.data.frame(as.numeric(circMeanDf))
      # convert radians back to degrees
      tempMetDf15min$winddir15_deg <- tempMetDf15min[,1]*(180/3.14)
      tempMetDf15min$winddir15_deg <- ifelse(tempMetDf15min$winddir15_deg < 0,tempMetDf15min$winddir15_deg + 360, tempMetDf15min$winddir15_deg) 
      tempMetDf15min$winddir15_deg <- format(tempMetDf15min$winddir15_deg, scientific = FALSE) 
      
      # replace degrees column in metDf15min 
      metDf15min$V14 <- tempMetDf15min$winddir15_deg
      
      # sum rainfall total, sensor total, and sensor sec in metDf
      # metSums <- c("V3", "V11", "V12") # vector of summed columns
      metDf15min$V3 <- roll_sum(metDf$V3, 15, align = "right", fill = NA)
      metDf15min$V11 <- roll_sum(metDf$V11, 15, align = "right", fill = NA)
      metDf15min$V12 <- roll_sum(metDf$V12, 15, align = "right", fill = NA)
      
      # Convert new timestamps to POSIXct
      metDf15min[,1] <- as.POSIXct(as.character(metDf15min[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
      # Subtract 15 minutes from all times
      # metDf15min[,1] <- metDf15min[,1] - 900
      
      # Remove first entry of concDf and generate new concDf for current resolution ****(11:01:00 - 11:15:00 averaged and labelled as 11:15:00)****
      concDf1min <- concDf1min[-1,] # 1st concDf
      concDf15min <- concDf1min
      concDf1min2 <- concDf1min2[-1,] # 2nd concDf
      concDf15min2 <- concDf1min2
      
      # Apply ma() function to all columns of new concDf besides timestamp
      concDf15min[,2:ncol(concDf15min)] <- as.numeric(ma(concDf15min[,2:ncol(concDf15min)]))
      concDf15min2[,2:ncol(concDf15min2)] <- as.numeric(ma(concDf15min2[,2:ncol(concDf15min2)]))
      
      # Rolling sum of Dead_Time column in concDf15min
      concDf15min$V10 <- roll_sum(concDf1min$V10, 15, align = "right", fill = NA)
      
      # Rolling sum of Dead_Time column in concDf15min
      concDf15min2$V10 <- roll_sum(concDf1min2$V10, 15, align = "right", fill = NA)
      
      # Convert new timestamps to POSIXct
      concDf15min[,1] <- as.POSIXct(as.character(concDf15min[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
      concDf15min2[,1] <- as.POSIXct(as.character(concDf15min2[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
      # Subtract 10 minutes from all times
      # concDf15min[,1] <- concDf15min[,1] - 900
      # concDf15min2[,1] <- concDf15min2[,1] - 900
      
      # Remove first 14 rows from all dfs
      metDf15min <- metDf15min[-(1:14),]
      concDf15min <- concDf15min[-(1:14),]
      concDf15min2 <- concDf15min2[-(1:14),]
      
      # Create masterDf
      finalMetDf <- metDf15min
      finalConcDf <- concDf15min
      finalConcDf2 <- concDf15min2
      
      # Set resolution for excelName
      resName <- "15_min_mov_avg"
      
      
    } # End if(timeRes == 15) condition

    
    
    #----------------------------------------
    # Merge dfs and generate spreadsheet
    #----------------------------------------  
    
    # Excel spreadsheet filename.
    tabName <- paste0(concDate[i], "_", concSite[i], "_", resName)
    excelName <- paste0(oDir,"/",concID[i],"_Merged_OPS_and_MET_data_",resName,".xlsx")
    
    # Find shortest Df
    short <- c(nrow(finalConcDf), nrow(finalConcDf2), nrow(finalMetDf))
    clipRows <- min(short)
    
    # Clip all Dfs to same length
    finalConcDf <- finalConcDf[1:clipRows,]
    finalConcDf2 <- finalConcDf2[1:clipRows,]
    finalMetDf <- finalMetDf[1:clipRows,]
    
    # Check for consistency in timestamps between MET and OPS data
    if(all(finalMetDf[,1] != finalConcDf[,1]) || all(finalMetDf[,1] != finalConcDf2[,1])){
      
      print(paste0("Warning! Timestamps in ",concID[i], " not consistent with MET data timestamps!"))
      
    }
    
    # Remove timestamps from finalConcDfs
    finalConcDf[,1] <- NULL 
    concHead[,1] <- NULL
    finalConcDf2[,1] <- NULL 
    concHead2[,1] <- NULL
    
    # Replace placeholder columns
    finalConcDf[,4] <- NULL
    concHead[,4] <- NULL
    finalConcDf[,4] <- NULL
    concHead[,4] <- NULL
    finalConcDf[,12] <- NA
    
    finalConcDf2[,4] <- NULL
    concHead2[,4] <- NULL
    finalConcDf2[,4] <- NULL
    concHead2[,4] <- NULL
    finalConcDf2[,12] <- NA
    
    # Coerce names of head and df to be identical
    names(concHead) <- names(finalConcDf) 
    names(concHead2) <- names(finalConcDf2) 
    names(metHead) <- names(finalMetDf) 
    
    # Merge Dfs
    gattaiConc <- rbind(concHead, finalConcDf)
    gattaiConc[,25:40] <- NULL
    gattaiConc2 <- rbind(concHead2, finalConcDf2)
    gattaiConc2[,25:40] <- NULL
    # Last minute edits... remove UB columns with NA 
    gattaiConc[,12] <- NULL
    gattaiConc2[,12] <- NULL
    # And change encoding
    # enc2utf8(as(gattaiConc[,11], "character"))
    # Encoding(gattaiConc[1,11]) <- "UTF-8"
    # Encoding(gattaiConc2[1,11]) <- "UTF-8"
    # Merge gattaiConc dfs and create gattaiMet
    gattaiConc <- cbind(gattaiConc, gattaiConc2)
    gattaiMet <- rbind(metHead, finalMetDf)

    # Merge gattai conc and met dfs
    gattaiDf <- cbind(gattaiMet, gattaiConc)
    # Final edits... remove first row of data
    # gattaiDf <- gattaiDf[-2,]
    
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
  
} # End for(q in 1: length(concName)) loop

# Print progress
print("Processing for all files complete. Goodbye") 