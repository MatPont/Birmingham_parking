setwd(dirname(rstudioapi::getSourceEditorContext()$path))

data <- read.table("../Datasets/dataset.csv", sep=",", header = TRUE)

data_cleaning <- function(data){
  # TODO
  return(data)
}

# Check outliers
sum(data[, 2] < data[, 3])
sum(data[, 3] < 0)

# Manage outliers
data[data[, 2] < data[, 3], 3] <- data[data[, 2] < data[,3], 2]
data[data[, 3] < 0 ,3] <- rep(0, sum(data[, 3] < 0))

# Check outliers
sum(data[, 2] < data[, 3])
sum(data[, 3] < 0)

# Count number of lines for each parking
for(i in unique(data[,1])){
  print(i)
  print(sum(data[,1] == i))
}

# Check if all days have 18 measures
for(parking in unique(data[,1])){
  print("=======")
  print(parking)
  # Remove hours from time
  days <- sapply(as.character(data[data[, 1] == parking, 4]), FUN = function(x){ unlist(strsplit(x, " "))[1]})
  print(length(days))
  if(length(days) < 1300){
    for(day in unique(days)){
      print(day)
      print(sum(days == day)) 
    }
  }else{
    for(day in unique(days)){
      if(sum(days == day) != 18){
        print(day)
        print(sum(days == day)) 
      }
    }
  }
  
  #readline(prompt="Press [enter] to continue")
}
