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

# Latitude & Longitude ----------------------------------------------------

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
prop.table(table(
  train$status_group[train$longitude == 0 & train$latitude > -1.2],
  useNA = "ifany"
))

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

# amount_tsh --------------------------------------------------------------

# amount_tsh - Total static head (amount water available to waterpoint)

summary(train$amount_tsh)
aggregate(train$amount_tsh, list(train$status_group),
          function(x) sum(x == 0) / length(x))
aggregate(train$amount_tsh, list(train$status_group), mean)
aggregate(train$amount_tsh, list(train$status_group), var)

t.test(train[status_group == 'functional']$amount_tsh,
       train[status_group == 'functional needs repair']$amount_tsh,
       alternative = "greater", var.equal = FALSE)
t.test(train[status_group == 'functional']$amount_tsh,
       train[status_group == 'non functional']$amount_tsh,
       alternative = "greater", var.equal = FALSE)
t.test(train[status_group == 'functional needs repair']$amount_tsh,
       train[status_group == 'non functional']$amount_tsh,
       alternative = "greater", var.equal = FALSE)

train_temp <- train[amount_tsh > 0, c("amount_tsh", "status_group")]
summary(train_temp$amount_tsh)
aggregate(train_temp$amount_tsh, list(train_temp$status_group), mean)
aggregate(train_temp$amount_tsh, list(train_temp$status_group), var)
aggregate(train_temp$amount_tsh, list(train_temp$status_group), median)

t.test(train_temp[status_group == 'functional']$amount_tsh,
       train_temp[status_group == 'functional needs repair']$amount_tsh,
       alternative = "greater", var.equal = FALSE)
t.test(train_temp[status_group == 'functional']$amount_tsh,
       train_temp[status_group == 'non functional']$amount_tsh,
       alternative = "greater", var.equal = FALSE)
t.test(train_temp[status_group == 'functional needs repair']$amount_tsh,
       train_temp[status_group == 'non functional']$amount_tsh,
       alternative = "greater", var.equal = FALSE)

ggplot(data = train[amount_tsh > 0],
       aes(x = status_group, y = amount_tsh)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point",
               shape = 18, size = 5, show.legend = FALSE) +
  scale_y_log10() +
  labs(x = NULL, y = NULL,
       title = "Total static head by status group (Log scale)",
       subtitle = "Excluding 0 values") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, vjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.5)
  )
ggplot(data = train[amount_tsh > 0],
       aes(x = status_group, y = amount_tsh)) +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point",
               shape = 18, size = 5, show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = "Total static head by status group (Mean)",
       subtitle = "Excluding 0 values") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, vjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.5)
  )

### Source: https://stat.ethz.ch/pipermail/r-help/2010-April/234387.html
median.test <- function(x, y) {
  z <- c(x,y)
  g <- rep(1:2, c(length(x),length(y)))
  m <- median(z)
  fisher.test(z<m, g)$p.value
}
median.test(train_temp[status_group == 'functional']$amount_tsh,
            train_temp[status_group == 'functional needs repair']$amount_tsh)
median.test(train_temp[status_group == 'functional']$amount_tsh,
            train_temp[status_group == 'non functional']$amount_tsh)
median.test(train_temp[status_group == 'functional needs repair']$amount_tsh,
            train_temp[status_group == 'non functional']$amount_tsh)

### Key conclusions
### 1) amount_tsh definititely shows a relationship with status_group
### 2) A majority of the values of this variable is zero, even though the % of
### 0 values is significantly higher for the needs repair and non functional
### group
### 3) T-test shows that the difference in means is significant, which
### continues to hold even after removing all 0 values
### 4) Interestingly, after removing 0's, the median value for needs repair is
### is the highest even though the mean is lower than functional

# funder ------------------------------------------------------------------

### Around ~1900 distinct values

# Convert all values to lower case and trim
funder <- trimws(tolower(train$funder))

# Remove all non alphanumeric characters
funder <- gsub("[^[:alnum:]]", "", funder)

# Find possible duplicates using distance functions
funder_uniq <- unique(funder)
funder_uniq <- sort(funder_uniq)
length(funder_uniq)
dist_mat <- adist(funder_uniq, funder_uniq)
funder_closest_idx <- apply(dist_mat, 1, function(x) {
  m <- min(x[x > 0])
  which(x == m)[1]
})
funder_closest <- data.table(
  funder_uniq,
  funder_closest = funder_uniq[funder_closest_idx]
)
funder_cnts <- unclass(table(funder))
funder_counts <- data.table(
  funder_uniq = names(funder_cnts),
  funder_counts = funder_cnts
)
funder_closest <- merge(funder_closest, funder_counts,
                        by = "funder_uniq", all = TRUE)
funder_closest$funder_counts_pct <-
  funder_closest$funder_counts / sum(funder_closest$funder_counts)
fwrite(funder_closest, "02_Exploratory_Outputs/funder_impute.csv")
