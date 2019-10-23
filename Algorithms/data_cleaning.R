setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(lubridate)

source("util.R")



########################################################
# Data cleaning
########################################################
# After the round of the dates some of them are equals (eg: 7h50 and 8h10 will be round to 8h)
# This function fix this problem by keeping the one with the original date closest to the rounded one
remove_dates_close <- function(data){
  row_to_delete <- c()
  for(i in 1:(length(data[, 5])-1)){
    rounded_date_1 = data[i, 5]
    rounded_date_2 = data[i+1, 5]
    if(data[i, 1] == data[i+1, 1] && rounded_date_1 == rounded_date_2){
      # Check closest date to the rounded one to keep it
      if(abs(difftime(rounded_date_1, data[i, 4])) < abs(difftime(rounded_date_2, data[i+1, 4]))){
        row_to_delete <- c(row_to_delete, i)
      }else{
        row_to_delete <- c(row_to_delete, (i+1))
      }
    }
  }
  
  data <- data[-row_to_delete, ]
  return(data)
}
# TEST: sum(duplicated(data[,-c(3,4)]))



# After the round of the dates some of them equals 7h30 but we want to start at 8h
# This function fix this problem by replacing 7h30 with 8h if the measure corresponding of 8h is missing
fix_first_measure_of_day <- function(data){
  row_to_delete <- c()
  rownames(data) <- 1:dim(data)[1]
  temp <- as.numeric(rownames(data[(data[,5]$min == 30) & (data[,5]$hour == 7),]))
  temp2 <- as.numeric(rownames(data[(data[,5]$min == 30) & (data[,5]$hour == 7),]))+1
  for(i in 1:length(temp)){
    idx <- temp[i]
    idx2 <- temp2[i]
    if( ! (data[idx2,5]$hour == 8 & data[idx2,5]$min == 0)){
      data[idx,5] <- data[idx,5] + 30*60
    }else{
      row_to_delete <- c(row_to_delete, idx)
    }
  }
  
  data <- data[-row_to_delete, ]
  return(data)
}
# TEST: as.numeric(rownames(data[(data[,5]$min == 30) & (data[,5]$hour == 7),]))



# Sort by name and (rounded) date
sort_dataframe <- function(data){
  return(data[order(data[,1], data[,dim(data)[2]]), ])
}



# Create missing rows on the dataframe using average 
get_missing_values <- function (data) {
  
  # -------------------------------------------------
  # Inner function to add multiple rows based on gap time
  add_multiple_rows <- function(data1, data2, newdf){
    time_diff <- difftime(data2[1,5], data1[1,5], units="mins")
    if(time_diff > 30) {
      row_to_add <- as.integer((time_diff-30)/30)
      # Add multiple rows based on gap time
      for(i in 1:row_to_add){
        # Add row
        new_row <- as.matrix(data1)
        new_row[3] = as.integer( data1[1,3] + i*(data2[1,3] - data1[1,3])/(row_to_add+1) )
        new_row[5] = as.character(ymd_hms(new_row[5]) + (i*30*60))
        new_row[4] <- new_row[5]
        newdf <- rbind(newdf, new_row) 
      }
    } 
    return(newdf)
  }
  # -------------------------------------------------
  
  #print("Starting...")
  newdf <- rbind(data[1, ])
  for (idx in 1:(dim(data)[1]-1)) {
    
    #progress_percent<-as.integer(idx/dim(data)[1]*100)
    #progress(progress_percent, progress.bar = FALSE)
    
    current_day <- as.Date(as.POSIXlt(data[idx, 5], tz = "Europe/Amsterdam"), "%Y-%m-%d")
    next_row_day <- as.Date(as.POSIXlt(data[idx+1, 5], tz = "Europe/Amsterdam"), "%Y-%m-%d")
    
    # If both parkinglot code are the same
    if(data[idx, 1] == data[idx+1, 1]) {
      
      # If same day
      if(current_day == next_row_day){
        newdf <- add_multiple_rows(data[idx,], data[idx+1,], newdf)
        next
      }
      
      # If different days
      if(current_day != next_row_day) {
        # Add rows to current day if the measure it's not the last one (16h30)
        if( ! (data[idx, 5]$hour == 16 & data[idx, 5]$min == 30)){
          # Make fictitious date at 17h to fill the gap
          temp <- data[idx, ]
          diff <- (17 - data[idx, 5]$hour)*60 - data[idx, 5]$min
          temp[5] = as.character(ymd_hms(data[idx, 5]) + (diff*60))
          newdf <- add_multiple_rows(data[idx, ], temp, newdf)
        }
        # Add rows to new day if the measure it's not the first one (8h)
        if( ! (data[idx+1, 5]$hour == 8 & data[idx+1, 5]$min == 0)){
          # Make fictitious date at 7h30 to fill the gap
          temp <- data[idx+1, ]
          diff <- (7 - data[idx+1, 5]$hour)*60 + (30 - data[idx+1, 5]$min)
          temp[5] = as.character(ymd_hms(data[idx+1, 5]) + (diff*60))
          newdf <- add_multiple_rows(temp, data[idx+1, ], newdf)
        }
        next
      }
    }
  }
  
  newdf <- newdf[-1, ]
  return(newdf)
}



# Fill missing days
get_missing_days <- function(data){
  alldays <- as.character(seq(as.Date("2016-10-04"), as.Date("2016-12-19"), by="days"))
  newdf <- rbind(data[1, ])
  
  for(parking in unique(data[,1])){
    # Remove hours from time
    data_parking <- data[data[, 1] == parking, ]
    measures <- sapply(as.character(data_parking[, 4]), FUN = function(x){ unlist(strsplit(x, " "))[1] })
    days <- unique(measures)
    missing_days <- setdiff(alldays, days)
    for(day in missing_days){
      temp <- data_parking[1, ]
      temp[3] <- NA
      temp_date <- as.POSIXlt(paste(day, "07:30:00"))
      wday <- temp_date$wday
      for(i in 1:18){
        new_date <- as.character(ymd_hms(temp_date) + (i*30*60))
        temp[5] <- new_date
        temp[4] <- temp[5]
        temp[3] <- as.integer(mean(as.integer(data_parking[data_parking[, 5]$wday == wday & 
                                       data_parking[, 5]$hour == as.POSIXlt(new_date)$hour & 
                                       data_parking[, 5]$min == as.POSIXlt(new_date)$min, 3])))
        newdf <- rbind(newdf, temp)
      }
    }
  }
  
  newdf <- newdf[-1, ]
  return(newdf)
}



# Full data cleaning pipeline
data_cleaning <- function(data){
  data <- sort_dataframe(data)
  
  # Remove parking with too few samples
  print("Remove parking with too few samples...")
  data <- data[data[,1] != "BHMBRTARC01", ]
  data <- data[data[,1] != "NIA North", ]
  
  # Remove duplicates
  print("Remove duplicates...")
  data <- unique(data)
  
  # Round dates 
  print("Round dates...")
  data$LastUpdated <- as.POSIXlt(data$LastUpdated, tz = "Europe/Amsterdam")
  data$LastUpdated_rounded <- round_date(data$LastUpdated,'30 minute')
  
  # Manage duplicates after round
  print("Manage duplicates after round...")
  data <- remove_dates_close(data)
  
  # Manage 7h30 measures
  print("Manage 7h30 measures...")
  data <- fix_first_measure_of_day(data)
  
  # Manage capacity < occupancy and occupancy < 0
  print("Manage capacity < occupancy and occupancy < 0...")
  data[data[, 2] < data[, 3], 3] <- data[data[, 2] < data[,3], 2]
  data[data[, 3] < 0 ,3] <- rep(0, sum(data[, 3] < 0))
  
  # Add missing values
  print("Add missing values...")
  temp <- get_missing_values(data)
  data <- rbind(data, temp)
  
  # Add missing days
  print("Add missing days...")
  temp <- get_missing_days(data)
  data <- rbind(data, temp)
  
  # Rename rownames
  print("Rename rownames...")
  data <- sort_dataframe(data)
  rownames(data) <- 1:dim(data)[1]
  data[,3] <- as.integer(data[,3])
  
  print("Done.")
  return(data)
}