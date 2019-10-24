library('dplyr')
library('reshape2')


########################################################
# Color
########################################################
degrade.bleu <- function(n){
  return(rgb(0,0.4,1,alpha=seq(0,1,1/n)))
}

coolBlueHotRed = function(n, alpha = 1){
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}



########################################################
# Normalization
########################################################
norm_vec <- function(x) x/sqrt(sum(x^2))

norm_mat <- function(x) t(apply(x, MARGIN = 1, FUN = norm_vec))

norm_dataset <- function(data, capacities=unlist(read.csv("../Datasets/capacities.csv", row.names = 1))){
  return(as.matrix(data / rep(capacities, rep(dim(data)[1]/length(capacities), length(capacities)))))
}

norm_chi_2 <- function(data){
  temp <- data
  row_sum <- apply(data, MARGIN=1, FUN=sum)
  col_sum <- apply(data, MARGIN=2, FUN=sum)
  for(i in 1:dim(data)[1])
    for(j in 1:dim(data)[2])
      temp[i,j] <- data[i,j] / (sqrt(col_sum[j])*row_sum[i])
  return(as.matrix(temp))
}



########################################################
# Dataset creation
########################################################
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

make_week_label <- function(data, num_parking=28, num_week=11){
  final_matrix <- rep(unique(data[,1]), rep(num_week, length(unique(data[,1]))))
  return(final_matrix)
}

make_day_dataset <- function(data, num_parking=28, num_day=77){
  final_matrix <- matrix(data[,3], nrow = num_parking*num_day, byrow = TRUE)
  return(final_matrix)
}

make_day_label <- function(data, num_parking=28, num_day=77){
  final_matrix <- rep(unique(data[,1]), rep(num_day, length(unique(data[,1]))))
  return(final_matrix)
}

make_parking_dataset <- function(data, num_parking=28){
  final_matrix <- matrix(data[,3], nrow = num_parking, byrow = TRUE)
  return(final_matrix)
}

make_parking_label <- function(data, num_parking=28){
  final_matrix <- unique(data[,1])
  return(final_matrix)
}



########################################################
# Plot
########################################################
plot_charge <- function(x, cluster){
  x_min <- min(x)
  x_max <- max(x)
  label <- unique(cluster)
  plot(colMeans(x[cluster == label[1],]), type='l', col=1, lty=1, ylim = c(x_min,x_max))
  for(i in 2:length(label)){
    lines(colMeans(x[cluster == label[i],]), type='l', col=i, lty=i, ylim = c(x_min,x_max)) 
  } 
  legend("topleft",legend=1:length(label), lty=length(label), col=1:length(label))
}



########################################################
# Other
########################################################
pause <- function(){
  readline(prompt="Press [enter] to continue") 
}