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
train_temp <- subset(train, longitude > 0)
newmap <- getMap(resolution = "high")
rnglong <- range(train_temp$longitude)
rnglat <- range(train_temp$latitude)
plot(newmap, xlim = rnglong, ylim = rnglat, asp = 1)
points(train_temp[status_group == "functional"]$longitude,
       train_temp[status_group == "functional"]$latitude,
       col = rgb(0, 1, 0, 0.1), cex = .6, pch = 21)
points(train_temp[status_group == "non functional"]$longitude,
       train_temp[status_group == "non functional"]$latitude,
       col = rgb(1, 0, 0, 0.1), cex = .6, pch = 23)

### dodoma -6.1630 35.7516
### mwanza -2.5164 32.9175
library(sf)
train_sf <- st_as_sf(train[, c("latitude", "longitude")],
                     coords = c("longitude", "latitude"))
dodoma_dist <- st_distance(
  train_sf,
  st_as_sf(
    data.frame(lat = -6.1630, long = 35.7516),
    coords = c("long", "lat")
  )
)
mwanza_dist <- st_distance(
  train_sf,
  st_as_sf(
    data.frame(lat = -2.5164, long = 32.9175),
    coords = c("long", "lat")
  )
)
train[["distance_dodoma"]] <- as.vector(dodoma_dist)
train[["distance_mwanza"]] <- as.vector(mwanza_dist)

train[longitude == 0, "distance_dodoma"] <- NA
train[longitude == 0, "distance_mwanza"] <- NA
train[["min_distance_dodmwa"]] <- pmin(train$distance_dodoma,
                                       train$distance_mwanza)

summary(train$distance_dodoma)
summary(train$distance_mwanza)
summary(train$min_distance_dodmwa)

aggregate(train$distance_dodoma, list(train$status_group), mean, na.rm = TRUE)
aggregate(train$distance_mwanza, list(train$status_group), mean, na.rm = TRUE)
aggregate(train$min_distance_dodmwa, list(train$status_group), mean,
          na.rm = TRUE)

ggplot(data = train[!is.na(distance_dodoma)],
       aes(x = status_group, y = distance_dodoma)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point",
               shape = 18, size = 5, show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = "Distance from Dodoma") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, vjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.5)
  )
ggplot(data = train[!is.na(distance_mwanza)],
       aes(x = status_group, y = distance_mwanza)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point",
               shape = 18, size = 5, show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = "Distance from Mwanza") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, vjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.5)
  )
ggplot(data = train[!is.na(min_distance_dodmwa)],
       aes(x = status_group, y = min_distance_dodmwa)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point",
               shape = 18, size = 5, show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = "Minimum Distance from either Dodoma or Mwanza") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, vjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.5)
  )

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

# Load the data back after funder imputation and merge back with training data
funder_imputed <- fread("02_Exploratory_Outputs/funder_imputed.csv")
funder_imputed <- funder_imputed[, c("funder_uniq", "funder_imputed")]
table(funder_imputed$funder_imputed, useNA = "ifany")
train[["funder_uniq"]] <- trimws(tolower(train$funder))
train$funder_uniq <- gsub("[^[:alnum:]]", "", train$funder_uniq)
train <- merge(train, funder_imputed, by = "funder_uniq", all = TRUE)

# Chi-squared test
chisq.test(factor(train$funder_imputed), factor(train$status_group)) #warning
chisq.test(factor(train$funder_imputed), factor(train$status_group),
           simulate.p.value = TRUE, B = 100000)

### Key conclusions
### 1) The imputation and chi-squared test suggests that definitely funder is
### related to the status_group

# Arvind additional checks on funder 5th Feb
plyr::count(train$funder_imputed)

ggplot(data = train,
       aes(x = funder_imputed, y = gps_height)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point",
               shape = 18, size = 5, show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = "GPS height by funder") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, vjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.5)
  )

ggplot(data = train,
       aes(x = funder_imputed, y = as.numeric(status_group == "non functional"))) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point",
               shape = 18, size = 5, show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = "Funder imputed by non-functional rate") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10, angle = 45),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, vjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.5)
  )

# installer ---------------------------------------------------------------

length(unique(train$installer))

### 1,936 unique values.

# Convert all values to lower case and trim
installer <- trimws(tolower(train$installer))

# Remove all non alphanumeric characters
installer <- gsub("[^[:alnum:]]", "", installer)

# Find possible duplicates using distance functions
installer_uniq <- unique(installer)
installer_uniq <- sort(installer_uniq)
length(installer_uniq)
dist_mat <- adist(installer_uniq, installer_uniq)
installer_closest_idx <- apply(dist_mat, 1, function(x) {
  m <- min(x[x > 0])
  which(x == m)[1]
})
installer_closest <- data.table(
  installer_uniq,
  installer_closest = installer_uniq[installer_closest_idx]
)
installer_cnts <- unclass(table(installer))
installer_counts <- data.table(
  installer_uniq = names(installer_cnts),
  installer_counts = installer_cnts
)
installer_closest <- merge(installer_closest, installer_counts,
                        by = "installer_uniq", all = TRUE)
installer_closest$installer_counts_pct <-
  installer_closest$installer_counts / sum(installer_closest$installer_counts)
fwrite(installer_closest, "02_Exploratory_Outputs/installer_impute.csv")

# Load the data back after installer imputation and merge back with training data
installer_imputed <- fread("02_Exploratory_Outputs/installer_imputed.csv")
installer_imputed <- installer_imputed[, c("installer_uniq", "installer_imputed")]
table(installer_imputed$installer_imputed, useNA = "ifany")
train[["installer_uniq"]] <- trimws(tolower(train$installer))
train$installer_uniq <- gsub("[^[:alnum:]]", "", train$installer_uniq)
train <- merge(train, installer_imputed, by = "installer_uniq", all = TRUE)

# Chi-squared test
chisq.test(factor(train$installer_imputed), factor(train$status_group)) #warning
chisq.test(factor(train$installer_imputed), factor(train$status_group),
           simulate.p.value = TRUE, B = 100000)

### Key conclusions
### 1) The imputation and chi-squared test suggests that definitely installer is
### related to the status_group

# gps_height --------------------------------------------------------------

summary(train$gps_height)
aggregate(train$gps_height, list(train$status_group),
          function(x) sum(x <= 0) / length(x))
aggregate(train$gps_height, list(train$status_group), mean)
aggregate(train$amount_tsh, list(train$status_group), var)

t.test(train[status_group == 'functional']$gps_height,
       train[status_group == 'functional needs repair']$gps_height,
       alternative = "greater", var.equal = FALSE)
t.test(train[status_group == 'functional']$gps_height,
       train[status_group == 'non functional']$gps_height,
       alternative = "greater", var.equal = FALSE)
t.test(train[status_group == 'functional needs repair']$gps_height,
       train[status_group == 'non functional']$gps_height,
       alternative = "greater", var.equal = FALSE)

train_temp <- train[gps_height > 0, c("gps_height", "status_group")]
summary(train_temp$gps_height)
aggregate(train_temp$gps_height, list(train_temp$status_group), mean)
aggregate(train_temp$gps_height, list(train_temp$status_group), var)
aggregate(train_temp$gps_height, list(train_temp$status_group), median)

t.test(train_temp[status_group == 'functional']$gps_height,
       train_temp[status_group == 'functional needs repair']$gps_height,
       alternative = "greater", var.equal = FALSE)
t.test(train_temp[status_group == 'functional']$gps_height,
       train_temp[status_group == 'non functional']$gps_height,
       alternative = "greater", var.equal = FALSE)
t.test(train_temp[status_group == 'functional needs repair']$gps_height,
       train_temp[status_group == 'non functional']$gps_height,
       alternative = "greater", var.equal = FALSE)

ggplot(data = train,
       aes(x = status_group, y = gps_height)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point",
               shape = 18, size = 5, show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = "GPS height by status group") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, vjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.5)
  )

# Added by Arvind on 5th Feb
quantile(train$gps_height, p = seq(0,1,0.02))
target_vals <- names(table(train$status_group))
gps_height_distr <- as.data.frame(sapply(target_vals, function(z) {
  quantile(train$gps_height[train$status_group == z], p = seq(0,1,0.02))
}))
write.csv(gps_height_distr,file = "gps_height_distr.csv", row.names = TRUE)

### Key conclusions
### 1) gps_height definititely shows a relationship with status_group
### 2) Around 35-40% of the values of this variable is <=0, even though the % of
### such values is higher for the needs repair and non functional
### group
### 3) T-test shows that the difference in means is significant, which
### continues to hold even after removing all 0 values

# basin -------------------------------------------------------------------

# Number of unique basins
length(unique(train$basin))

### There are 9 of them

# Frequency of unique values
table(train$basin, useNA = "ifany")

### Highest frequencies are lake victoria and pangani
### There is a value called 'internal'. What does it mean?

# Distribution of status by basin
prop.table(table(train$basin, train$status_group, useNA = "ifany"), margin = 1)

### lake rukwa and ruvuma / southern coast definitely stand out as less than
### half of the pumps are functional there. lake tanganyika / lake victoria
### are borderline.

# region  -------------------------------------------------------------------

# Number of unique regions
length(unique(train$region))

### There are 21 of them

dplyr::arrange(plyr::count(train$region), desc(freq))
reg <- train[, .N, keyby = c("region", "region_code")]

### Adequate counts in all of them.
### Same regions seem to have different codes in few cases. Assuming region is
### right and the region_code can be ignored.

# Distribution of status by region
prop.table(table(train$region, train$status_group, useNA = "ifany"), margin = 1)

### Clear separation by region. lindi has less than 30% functional
### whereas iringa has almost 80% functional

# subvillage ----------------------------------------------------------------

# Number of unique subvillages
length(unique(train$subvillage))

### There are 19288 of them; need to figure out a way to group down

# Frequency of unique values
all_subvillages <- plyr::count(train$subvillage)

# district_code -----------------------------------------------------------

# Number of unique districts
length(unique(train$district_code))

### 20 unique values

regdist <- train[, .N, keyby = c("region", "district_code")]

### Same district code appears for many different regions

prop.table(table(train$district_code, train$status_group, useNA = "ifany"),
           margin = 1)

# lga ---------------------------------------------------------------------

# Number of unique lga
length(unique(train$lga))

### 125 unique values

# Look at values
table(train$lga)

# Chi-squared test
chisq.test(factor(train$lga), factor(train$status_group))
chisq.test(factor(train$lga), factor(train$status_group),
           simulate.p.value = TRUE, B = 100000)

# Proportion by group
prop.table(table(train$lga, train$status_group, useNA = "ifany"),
           margin = 1)

### From the above, the rates definitely vary by lga. To avoid 1-hot encoding
### such a large number of values, perhaps better to encode it another way

# ward --------------------------------------------------------------------

# Number of unique ward
length(unique(train$ward))

### 2092 unique values

# Look at values
table(train$ward)

# Frequency of unique values
all_wards <- plyr::count(train$ward)

# population --------------------------------------------------------------

summary(train$population)
aggregate(train$population, list(train$status_group),
          function(x) sum(x == 0) / length(x))
aggregate(train$population, list(train$status_group), mean)
aggregate(train$population, list(train$status_group), var)

### Average population is higher for functional cases

t.test(train[status_group == 'functional']$population,
       train[status_group == 'functional needs repair']$population,
       alternative = "greater", var.equal = FALSE)
t.test(train[status_group == 'functional']$population,
       train[status_group == 'non functional']$population,
       alternative = "greater", var.equal = FALSE)
t.test(train[status_group == 'functional needs repair']$population,
       train[status_group == 'non functional']$population,
       alternative = "greater", var.equal = FALSE)

### Interestingly, only the 2nd t-test has a really low p-value

ggplot(data = train[population > 0],
       aes(x = status_group, y = population)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point",
               shape = 18, size = 5, show.legend = FALSE) +
  scale_y_log10() +
  labs(x = NULL, y = NULL,
       title = "Population by status group (Log scale)",
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
ggplot(data = train[population > 0],
       aes(x = status_group, y = population)) +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point",
               shape = 18, size = 5, show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = "Population by status group (Mean)",
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

# public_meeting ----------------------------------------------------------

# Check proportion of functional by 0, 1, NA
train[["public_meeting"]] <- train$public_meeting * 1
prop.table(table(train$public_meeting, train$status_group,
                 useNA = "ifany"), margin = 1)

### Wherever public_meeting is 1 or NA, the proportion of functional pumps
### are much higher

# scheme_management -------------------------------------------------------

# Number of unique values
length(unique(train$scheme_management))

### 13 unique values

prop.table(table(train$scheme_management, train$status_group,
                 useNA = "ifany"), margin = 1)

### Clear some of the scheme managers have higher percentage of functional
### pumps


# scheme_name -------------------------------------------------------------

length(unique(train$scheme_name))

### 2577 unique values

# permit ------------------------------------------------------------------

prop.table(table(train$permit, train$status_group,
                 useNA = "ifany"), margin = 1)

### Does not seem to have much relationship to status group

# construction_year -------------------------------------------------------

length(unique(train$construction_year))

### 55 unique values

table(train$construction_year, useNA = "ifany")

### Around 20K values with value 0

prop.table(table(train$construction_year, train$status_group,
                 useNA = "ifany"), margin = 1)

### More recent constructions have higher percentage of functional pumps

# extraction_type ---------------------------------------------------------

length(unique(train$extraction_type))

### 18 distinct values

prop.table(table(train$extraction_type, train$status_group,
                 useNA = "ifany"), margin = 1)

# extraction_type_group ---------------------------------------------------

length(unique(train$extraction_type_group))

### 13 distinct values

prop.table(table(train$extraction_type_group, train$status_group,
                 useNA = "ifany"), margin = 1)

# extraction_type_class ---------------------------------------------------

length(unique(train$extraction_type_class))

### 7 distinct values

prop.table(table(train$extraction_type_class, train$status_group,
                 useNA = "ifany"), margin = 1)

ext_type_summ <- train[, .N, keyby=c(
  "extraction_type",
  "extraction_type_group",
  "extraction_type_class"
)]

fwrite(ext_type_summ, "02_Exploratory_Outputs/ext_type_summ.csv")

ext_imputed <- fread("02_Exploratory_Outputs/ext_type_imputed.csv")
ext_imputed <- ext_imputed[, -c("N")]

train <- merge(train, ext_imputed, by=c(
  "extraction_type",
  "extraction_type_group",
  "extraction_type_class"
), all.x = TRUE)

prop.table(table(train$extraction_type_final, train$status_group,
                 useNA = "ifany"), margin = 1)

# Chi-squared test
chisq.test(factor(train$extraction_type_final), factor(train$status_group))
chisq.test(factor(train$extraction_type_final), factor(train$status_group),
           simulate.p.value = TRUE, B = 100000)

# management --------------------------------------------------------------

length(unique(train$management))

### 12 distinct values

prop.table(table(train$management, train$status_group,
                 useNA = "ifany"), margin = 1)

# management_group --------------------------------------------------------

length(unique(train$management_group))

### 5 distinct values

prop.table(table(train$management_group, train$status_group,
                 useNA = "ifany"), margin = 1)

mgmt_summ <- train[, .N, keyby=c(
  "management",
  "management_group"
)]