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