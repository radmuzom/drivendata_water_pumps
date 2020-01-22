# Libraries
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

# Drop the original values and labels
rm(train_labels)
rm(train_values)

# Remove columns which cannot be used as features
dropcols <- c("date_recorded", "wpt_name", "num_private", "recorded_by", "id")
train <- train[, -..dropcols]

# Type of each column
sapply(train, class)

# Transformations ---------------------------------------------------------

### Transformations need to be repeated for test set for accurate scoring

# Convert region_code and district_code to character
train[["region_code"]] <- as.character(train[["region_code"]])
train[["district_code"]] <- as.character(train[["district_code"]])

# Convert all character values to lower case
train <- as.data.table(lapply(train, function(v) {
  if (is.character(v)) {
    return(tolower(v))
  } else {
    return(v)
  }
}))
