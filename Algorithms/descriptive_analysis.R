setwd(dirname(rstudioapi::getSourceEditorContext()$path))

data <- read.table("../Datasets/dataset.csv", sep=",", header = TRUE)

# Descriptive analysis
boxplot(data[, 2:4])
summary(data)
pairs(data)

# Check outliers
sum(data[, 2] < data[, 3])
sum(data[, 3] < 0)

# Manage outliers
data[data[, 2] < data[, 3], 3] <- data[data[, 2] < data[,3], 2]
data[data[, 3] < 0 ,3] <- rep(0, sum(data[, 3] < 0))

sum(data[, 2] < data[, 3])
sum(data[, 3] < 0)

# Descriptive analysis after outliers management
summary(data)
boxplot(data[, 2:4])
