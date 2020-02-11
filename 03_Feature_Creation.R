# Libraries
library(data.table)
library(vtreat)
library(sf)

# Load the train and test data
train_values <- fread("train_values.csv")
test_values <- fread("test_values.csv")

# Remove columns which cannot be used as features
dropcols <- c("date_recorded", "wpt_name", "num_private", "recorded_by", "id",
              "region_code")
train_values <- train_values[, -..dropcols]
test_values <- test_values[, -..dropcols]

# Convert all character values to lower case
train_values <- as.data.table(lapply(train_values, function(v) {
  if (is.character(v)) {
    return(tolower(v))
  } else {
    return(v)
  }
}))
test_values <- as.data.table(lapply(test_values, function(v) {
  if (is.character(v)) {
    return(tolower(v))
  } else {
    return(v)
  }
}))

# amount_tsh
train_values[["amount_tsh_0_flag"]] <- ifelse(train_values$amount_tsh > 0, 0, 1)
test_values[["amount_tsh_0_flag"]] <- ifelse(test_values$amount_tsh > 0, 0, 1)

# funder
funder_imputed <- fread("02_Exploratory_Outputs/funder_imputed.csv")
funder_imputed <- funder_imputed[, c("funder_uniq", "funder_imputed")]
table(funder_imputed$funder_imputed, useNA = "ifany")

train_values[["funder_uniq"]] <- trimws(tolower(train_values$funder))
train_values$funder_uniq <- gsub("[^[:alnum:]]", "", train_values$funder_uniq)
train_values <- merge(train_values, funder_imputed,
                      by = "funder_uniq", all.x = TRUE)
train_values <- train_values[, -c("funder", "funder_uniq")]

test_values[["funder_uniq"]] <- trimws(tolower(test_values$funder))
test_values$funder_uniq <- gsub("[^[:alnum:]]", "", test_values$funder_uniq)
test_values[is.na(funder_uniq), "funder_uniq"] <- ""
test_values <- merge(test_values, funder_imputed,
                      by = "funder_uniq", all.x = TRUE)
test_values <- test_values[, -c("funder", "funder_uniq")]
sum(is.na(test_values$funder_imputed))

### 249 missing values. But should not matter due to encoding.

dtz <- designTreatmentsZ(train_values, "funder_imputed")
train_dtz <- prepare(dtz, train_values)
train_dtz <- train_dtz[, -1]
test_dtz <- prepare(dtz, test_values)
test_dtz <- test_dtz[, -1]

train_values <- cbind(train_values, train_dtz)
train_values <- train_values[, -"funder_imputed"]

test_values <- cbind(test_values, test_dtz)
test_values <- test_values[, -"funder_imputed"]

rm(dtz)

# gps_height
train_values[["gps_height_neg_flag"]] <- ifelse(train_values$gps_height < 0,
                                                0, 1)
test_values[["amount_tsh_neg_flag"]] <- ifelse(test_values$gps_height > 0,
                                               0, 1)
train_values[["gps_height_0_flag"]] <- ifelse(train_values$gps_height == 0,
                                              0, 1)
test_values[["gps_height_0_flag"]] <- ifelse(test_values$gps_height == 0,
                                             0, 1)

# installer
installer_imputed <- fread("02_Exploratory_Outputs/installer_imputed.csv")
installer_imputed <- installer_imputed[, c("installer_uniq", "installer_imputed")]
table(installer_imputed$installer_imputed, useNA = "ifany")

train_values[["installer_uniq"]] <- trimws(tolower(train_values$installer))
train_values$installer_uniq <- gsub("[^[:alnum:]]", "", train_values$installer_uniq)
train_values <- merge(train_values, installer_imputed,
                      by = "installer_uniq", all.x = TRUE)
train_values <- train_values[, -c("installer", "installer_uniq")]

test_values[["installer_uniq"]] <- trimws(tolower(test_values$installer))
test_values$installer_uniq <- gsub("[^[:alnum:]]", "", test_values$installer_uniq)
test_values[is.na(installer_uniq), "installer_uniq"] <- ""
test_values <- merge(test_values, installer_imputed,
                     by = "installer_uniq", all.x = TRUE)
test_values <- test_values[, -c("installer", "installer_uniq")]
sum(is.na(test_values$installer_imputed))

### 225 missing values. But should not matter due to encoding.

dtz <- designTreatmentsZ(train_values, "installer_imputed")
train_dtz <- prepare(dtz, train_values)
train_dtz <- train_dtz[, -1]
test_dtz <- prepare(dtz, test_values)
test_dtz <- test_dtz[, -1]

train_values <- cbind(train_values, train_dtz)
train_values <- train_values[, -"installer_imputed"]

test_values <- cbind(test_values, test_dtz)
test_values <- test_values[, -"installer_imputed"]

rm(dtz)

# latitude / longitude

train_sf <- st_as_sf(train_values[, c("latitude", "longitude")],
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
train_values[["distance_dodoma"]] <- as.vector(dodoma_dist)
train_values[["distance_mwanza"]] <- as.vector(mwanza_dist)

train_values[longitude == 0, "distance_dodoma"] <- NA
train_values[longitude == 0, "distance_mwanza"] <- NA
train_values[["min_distance_dodmwa"]] <- pmin(train_values$distance_dodoma,
                                              train_values$distance_mwanza)
train_values[["max_distance_dodmwa"]] <- pmax(train_values$distance_dodoma,
                                              train_values$distance_mwanza)

test_values[is.na(latitude), "latitude"] <- 10
test_values[is.na(longitude), "longitude"] <- 70

test_sf <- st_as_sf(test_values[, c("latitude", "longitude")],
                     coords = c("longitude", "latitude"))
dodoma_dist <- st_distance(
  test_sf,
  st_as_sf(
    data.frame(lat = -6.1630, long = 35.7516),
    coords = c("long", "lat")
  )
)
mwanza_dist <- st_distance(
  test_sf,
  st_as_sf(
    data.frame(lat = -2.5164, long = 32.9175),
    coords = c("long", "lat")
  )
)
test_values[["distance_dodoma"]] <- as.vector(dodoma_dist)
test_values[["distance_mwanza"]] <- as.vector(mwanza_dist)

test_values[longitude == 0, "distance_dodoma"] <- NA
test_values[longitude == 70, "distance_dodoma"] <- NA
test_values[latitude == 10, "distance_dodoma"] <- NA
test_values[longitude == 0, "distance_mwanza"] <- NA
test_values[longitude == 70, "distance_mwanza"] <- NA
test_values[latitude == 10, "distance_mwanza"] <- NA

test_values[latitude == 10, "latitude"] <- NA
test_values[longitude == 70, "longitude"] <- NA

test_values[["min_distance_dodmwa"]] <- pmin(test_values$distance_dodoma,
                                              test_values$distance_mwanza)
test_values[["max_distance_dodmwa"]] <- pmax(test_values$distance_dodoma,
                                              test_values$distance_mwanza)
