library('lubridate')
library("svMisc")


setwd(dirname(rstudioapi::getSourceEditorContext()$path))

data <- read.table("./Datasets/dataset.csv", sep=",", header = TRUE)
#data$LastUpdated <- as.POSIXlt(data$LastUpdated, tz = "Europe/Amsterdam")
data$Occupancy<-as.integer(data$Occupancy)

# Remove duplicates
data <- unique(data)

# Round dates 
data$LastUpdated_rounded<-round_date(as.POSIXlt(data$LastUpdated, tz = "Europe/Amsterdam"),'30 minute')


# Add measurements
add_missing_values <- function (data, idx) {
  idx<-1
  print("Starting...")
  newdf <- rbind(data[1,])
  for (idx in 1:(dim(data)[1]-1)) {
    
    progress_percent<-as.integer(idx/dim(data)[1]*100)
    #progress(progress_percent, progress.bar = FALSE)
    
    
    current_day<-as.Date(as.POSIXlt(data[idx,5], tz = "Europe/Amsterdam"),"%Y-%m-%d")
    next_row_day<-as.Date(as.POSIXlt(data[idx+1,5], tz = "Europe/Amsterdam"),"%Y-%m-%d")
    
    #If both parkinglot code are the same
    if(data[idx,1]==data[idx+1,1]) {
      #if same day
      if(current_day == next_row_day & difftime(data[idx+1,5],data[idx,5],units="mins") > 30) {
        # Add row
        new_row<-as.matrix(data[idx,])
        new_row[3]=as.integer(mean(c(data[idx,3],data[idx+1,3])))
        new_row[5]=as.character(ymd_hms(new_row[5]) + (30*60))
        new_row[4]<-new_row[5]
        newdf <- rbind(newdf,new_row)
        next  
      }
      # If different days
      if(current_day != next_row_day & difftime(data[idx+1,5],data[idx,5],units="hours") > 16) {
        new_row=as.matrix(data[idx,])
        #Add row to same day
        if(as.POSIXlt(data[idx,5], tz = "Europe/Amsterdam")$hour < 16) {
          new_row<-as.matrix(data[idx,])
          new_row[5]=as.character(ymd_hms(new_row[5]) + (30*60))
        }
        
        #Add row the following day
        else {
          new_row<-as.matrix(data[idx+1,])
          new_row[5]=as.character(ymd_hms(new_row[5]) - (30*60))
        }
        new_row[3]=as.integer(mean(c(data[idx,3],data[idx+1,3])))
        new_row[4]<-new_row[5]
        newdf <- rbind(newdf,new_row)
        next
      }
      
    }
  }
  return(newdf)
}
new_data<-add_missing_values(data)

