library('dplyr')
library('reshape2')

norm_vec <- function(x) x/sqrt(sum(x^2))

pause <- function(){
  readline(prompt="Press [enter] to continue") 
}

make_week_dataset <- function(data, num_parking=28, num_week=11){
  # # Add week number and weekday to group data
  # data$LastUpdated_week_hours<-strftime(data$LastUpdated_rounded,format="%w.%H:%M")
  # data$LastUpdated_week<-strftime(as.POSIXlt(data$LastUpdated, tz = "Europe/Amsterdam"),format="%W")
  # 
  # grouped_data=group_by(data,SystemCodeNumber,LastUpdated_week, LastUpdated_week_hours)
  # grouped_data <- grouped_data %>% summarise(
  #   Occupancy = sum(Occupancy)
  # )
  # final_matrix <- dcast(grouped_data, SystemCodeNumber + LastUpdated_week ~ LastUpdated_week_hours, value.var = "Occupancy")
  # #final_matrix[1,]
  
  final_matrix <- matrix(data[,3], nrow = num_parking*num_week, byrow = TRUE)
  return(final_matrix)
}


make_day_dataset <- function(data, num_parking=28, num_day=77){
  final_matrix <- matrix(data[,3], nrow = num_parking*num_day, byrow = TRUE)
  return(final_matrix)
}

make_parking_dataset <- function(data, num_parking=28){
  final_matrix <- matrix(data[,3], nrow = num_parking, byrow = TRUE)
  return(final_matrix)
}



make_week_label <- function(data, num_parking=28, num_week=11){
  final_matrix <- rep(unique(data[,1]), rep(num_week, length(unique(data[,1]))))
  return(final_matrix)
}


make_day_label <- function(data, num_parking=28, num_day=77){
  final_matrix <- rep(unique(data[,1]), rep(num_day, length(unique(data[,1]))))
  return(final_matrix)
}

make_parking_label <- function(data, num_parking=28){
  final_matrix <- unique(data[,1])
  return(final_matrix)
}