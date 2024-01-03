#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# FILE: MET + Dust Conc. Merger
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
iDir2 <- '~/DUST/Nancy Processed/'

# Set time resolution to 1, 5, 10, or 15 minutes.
timeRes <- 15

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

# import processed files w/ dust flux
fluxList <- list.files(iDir2, pattern="*.csv")


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
  # masterMetDF[,2] <- NULL # Record no. column
  
  
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
    metHead <- metStationHead
    # metHead[,2] <- NULL # Remove record number column
    
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
  

    
    #----------------------------------------
    # Merge dfs and generate spreadsheet
    #----------------------------------------  
    
    # .csv file name.
    csvName <- paste0(oDir,"/",concID[i],"_1_min_raw_MET_data", ".csv")
    
    # prep header...
    # fluxDFHead$V1 <- as.POSIXct(as.character(fluxDFHead$V1), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
    
    # change V1 in metHeadDF to POSIXct
    tempMetHead <- data.frame(as.character(metStationHead[1:3,1])) # Pull metadata in column 1
    # Convert column 1 to POSIXct
    
    metHead[,1] <- as.POSIXct(as.character(metHead[,1]), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
    
    
    # merge DFs
    gattaiDF <- rbind(metHead, metDf)
    
    # fix header
    gattaiDF$V1 <- as.character(gattaiDF$V1)
    gattaiDF[1:3,1] <- as.character(tempMetHead[1:3,1])
    
    write.table(gattaiDF, csvName, sep = ",", row.names = FALSE, col.names = FALSE, fileEncoding = "UTF-8")
    
    print("Complete!") # Print progress
    
    i = i + 2
    
  }
  
  # Remove masterMetDF
  rm(masterMetDF)
  
}