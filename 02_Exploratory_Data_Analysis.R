# Libraries
library(ggplot2)
library(summarytools)

# Target ------------------------------------------------------------------

# Target variable
table(train$status_group, useNA = "ifany")
prop.table(table(train$status_group, useNA = "ifany"))

### No missing values
### functional - 54.3%, functional needs repair - 7.3%, non functional - 38.4%

# Features ----------------------------------------------------------------

# Get a sense of the distribution of values using the dfSummary function
view(dfSummary(train))

### The only variables with missing values are public_meeting and
### permit, the two boolean variables

# # Running the summary by the different classes of the Target to see if
# # any significant distributional differences pop up
# target_vals <- names(table(train$status_group))
# sapply(target_vals, function(z) {
#   view(dfSummary(train[train$status_group == z,]))
# })

### Quick view - nothing stands out
### Commented out the previous block on 1/25

# Plot the functional and non functional pumps with a different color on a
# map using the rworldmap package
library(rworldmap)
newmap <- getMap(resolution = "high")
rnglong <- range(train$longitude)
rnglat <- range(train$latitude)
plot(newmap, xlim = rnglong, ylim = rnglat, asp = 1)
points(train[status_group == "functional"]$longitude,
       train[status_group == "functional"]$latitude,
       col = rgb(0, 1, 0, 0.1), cex = .6, pch = 21)
points(train[status_group == "non functional"]$longitude,
       train[status_group == "non functional"]$latitude,
       col = rgb(1, 0, 0, 0.1), cex = .6, pch = 23)

### Remember to Zoom before viewing plot
### Conclusion - Pumps located in a longtidunal band across the centre are
### functional as compared to the borders?

# Explore what's going on with 0s on Lat and Long
quantile(train$longitude, p = seq(0,1,0.02))
quantile(train$latitude, p = seq(0,1,0.02))
nrow(train[train$longitude == 0 & train$latitude > -1.2,])
prop.table(table(train$status_group[train$longitude == 0 & train$latitude > -1.2], useNA = "ifany"))

### 1812 Rows
### functional - 48.0%, functional needs repair - 21.3%, non functional - 30.7%
### Higher proportion of FNRs in the incorrect longitude

# Rerunning map eliminating the 0 longitudes
train <- subset(train, longitude > 0)
newmap <- getMap(resolution = "high")
rnglong <- range(train$longitude)
rnglat <- range(train$latitude)
plot(newmap, xlim = rnglong, ylim = rnglat, asp = 1)
points(train[status_group == "functional"]$longitude,
       train[status_group == "functional"]$latitude,
       col = rgb(0, 1, 0, 0.1), cex = .6, pch = 21)
points(train[status_group == "non functional"]$longitude,
       train[status_group == "non functional"]$latitude,
       col = rgb(1, 0, 0, 0.1), cex = .6, pch = 23)
