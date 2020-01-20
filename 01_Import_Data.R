library(data.table)

# Load data into memory
train_values <- fread("train_values.csv")
train_labels <- fread("train_labels.csv")

# Dimensions of each dataset
dim(train_values)
dim(train_labels)

# Columns in each dataset
names(train_labels)
names(train_values)

# Combine the label and values
train <- merge(train_values, train_labels, by = "id")
dim(train)
