setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("util.R")
source("data_cleaning.R")

data <- read.table("../Datasets/dataset.csv", sep=",", header = TRUE)
data <- data_cleaning(data)

week_data <- make_week_dataset(data)
day_data <- make_day_dataset(data)

# Descriptive analysis
boxplot(data[, 2:4])
summary(data)
pairs(data)