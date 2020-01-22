# Libraries
library(ggplot2)
library(summarytools)

# Target ------------------------------------------------------------------

# Target variable
table(train$status_group, useNA = "ifany")
prop.table(table(train$status_group, useNA = "ifany"))

# No missing values
# functional - 54.3%, functional needs repair - 7.3%, non functional - 38.4%

# Features ----------------------------------------------------------------

# Get a sense of the distribution of values using the dfSummary function
view(dfSummary(train))

# The only variables with missing values are public_meeting and
# permit, the two boolean variables

### Running the summary by the different classes of the Target to see if any significant distributional differences pop up

target_vals <- names(table(train$status_group))
sapply(target_vals, function(z) {
  view(dfSummary(train[train$status_group == z,]))
  })

### Quick view - nothing stands out.