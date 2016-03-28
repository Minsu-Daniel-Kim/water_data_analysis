
library(caret)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(tidyr)
library(stringr)

data.raw <- read.csv("train.csv")
data.test <- read.csv("test.csv")
data.test$status_group <- "NA"
data.test$type = 'test'
data.label <- read.csv("train_label.csv")


# preprocessing
data <- merge(data.raw, data.label)
data$type <- "train"
data <- rbind(data, data.test)

data$date_recorded <- ymd(data$date_recorded)
data$date_recorded_year <- factor(year(data$date_recorded))
data$date_recorded_month <- factor(month(data$date_recorded))
data$district_code <- factor(data$district_code)
data$status_group <- gsub(" ", "_", data$status_group)
data$recorded_by <- NULL

data$construction_year <- factor(year(years(data$construction_year)))
data$region_code <- factor(data$region_code)
data$funder <- as.character(data$funder)
data$funder <- str_to_lower(data$funder)
data$funder[data$funder == ''] <- 'Unknown'
data$funder <- factor(data$funder)

# data$basin <- as.character(data$basin)
# data$basin <- str_to_lower(data$basin)
# data$basin <- factor(data$basin)

# data$wpt_name <- as.character(data$wpt_name)
# data$wpt_name <- str_to_lower(data$wpt_name)
# data$wpt_name <- factor(data$wpt_name)

# data$ward <- as.character(data$ward)
# data$ward <- str_to_lower(data$ward)
# data$ward <- factor(data$ward)

# data$payment_type <- as.character(data$payment_type)
# data$payment_type <- factor(data$payment_type)

data$installer <- as.character(data$installer)
data$installer <- str_to_lower(data$installer)
data$installer[data$installer == ''] <- 'Unknown'
data$installer <- factor(data$installer)

data$subvillage <- as.character(data$subvillage)
data$subvillage <- str_to_lower(data$subvillage)
data$subvillage[data$subvillage == ''] <- 'Unknown'
data$subvillage <- factor(data$subvillage)

data$scheme_management <- as.character(data$scheme_management)
data$scheme_management[data$scheme_management == ''] <- 'Unknown'
data$scheme_management <- factor(data$scheme_management)

data$scheme_name <- as.character(data$scheme_name)
data$scheme_name[data$scheme_name == ''] <- 'Unknown'
data$scheme_name <- factor(data$scheme_name)

data$public_meeting <- as.character(data$public_meeting)
data$public_meeting[data$public_meeting == ''] <- 'Unknown'
data$public_meeting <- factor(data$public_meeting)

data$permit <- as.character(data$permit)
data$permit[data$permit == ''] <- 'Unknown'
data$permit <- factor(data$permit)

for (i in 1:ncol(data)) {
  if(is.factor(data[, i])) {
    data[, i] <- as.character(data[, i])
    data[, i] <- str_to_lower(data[, i])
    data[, i] <- factor(gsub('[ .?\'$()*/&:"-]', '_', data[, i]))
  }
}

group_by.status_group <- group_by(filter(data, type == 'train'), status_group)
# dplyr::summarise(group_by.status_group, mean(amount_tsh), gps_height = mean(gps_height))
# dplyr::summarise(group_by.status_group, mean(population))


data.train <- filter(data, type == 'train')
installer <- as.data.frame(sort(table(data.train$installer), decreasing = T))
names(installer) <- "num"

installer.function <- as.data.frame(table(as.character(data.train$installer) , as.character(data.train$status_group)))

installer.function.spread <- spread(installer.function, Var2, Freq)
# installer.function.spread %>% View
installer.function.ratio <- mutate(installer.function.spread, total = functional + non_functional + functional_needs_repair, func_ratio = functional / total, non_func_ratio = non_functional / total, needs_repair_ratio = functional_needs_repair / total)
installer.function.ratio <- installer.function.ratio[ ,-c(2:4)]

pre <- preProcess(select(installer.function.ratio, total, func_ratio, non_func_ratio, needs_repair_ratio), method = c('center', 'scale'))
installer.scaled <- predict(pre, select(installer.function.ratio, total, func_ratio, non_func_ratio, needs_repair_ratio))


set.seed(10)
model.kmeans <- kmeans(installer.scaled, 20, iter.max = 100)
installer.function.ratio$total <- NULL
installer.function.ratio$cluster <- factor(model.kmeans$cluster)
installer.scaled$cluster <- factor(model.kmeans$cluster)
# ggplot(installer.function.ratio, aes(x = func_ratio, fill = cluster)) + geom_histogram()

# installer.scaled[installer.scaled$cluster == 1, ]
dists <- NULL
for (i in 1:nrow(model.kmeans$centers)) {
  a <- apply(installer.scaled[installer.scaled$cluster == i, ][-5], 1, function(x) {
    return(dist(rbind(model.kmeans$centers[i, ], x)))
  })
  
  dists <- c(dists, a)
}

installer.function.ratio$dist <- dists
# data <- left_join(data, installer.function.ratio, by = c("installer" = "Var1"))
data <- left_join(data, installer.function.ratio, by = c("installer" = "Var1"))
data$installer <- factor(data$installer)
# ggplot(installer.function.ratio, aes(x = cluster, y = dist)) + geom_boxplot()





# longitude / latitude imputation
data$latitude[data$latitude < 1 & data$latitude > -1] <- NA
data$longitude[data$longitude < 1 & data$longitude > -1] <- NA

imputed <- TRUE
if (imputed) {
  imputed <- readRDS("imputation.RDS")
} else {
  imputed <- select(data, longitude, latitude, subvillage, region, region_code, district_code) %>% mutate(subvillage = as.numeric(subvillage), region = as.numeric(region), region_code = as.numeric(region), district_code = as.numeric(district_code))
  pre2 <- preProcess(imputed, method = c("knnImpute"))
  imputed <- predict(pre2, imputed)
  saveRDS(imputed, "imputation.RDS")
  
}

data$latitude <- imputed$latitude
data$longitude <- imputed$longitude

data$needs_repair_ratio[is.na(data$needs_repair_ratio)] <- mean(data$needs_repair_ratio, na.rm = T)
data$non_func_ratio[is.na(data$non_func_ratio)] <- mean(data$non_func_ratio, na.rm = T)
data$func_ratio[is.na(data$func_ratio)] <- mean(data$func_ratio, na.rm = T)
data$dist[is.na(data$dist)] <- mean(data$dist, na.rm = T)
data$cluster <- as.character(data$cluster)
data$cluster[is.na(data$cluster)] <- 21
data$cluster <- factor(data$cluster)

tmp <- select(data, -c(type, dist, func_ratio, non_func_ratio, needs_repair_ratio, population, num_private, latitude, longitude, gps_height, amount_tsh, id, date_recorded, wpt_name, subvillage, funder, installer, ward, scheme_name))
a1 <- dummyVars(status_group ~ ., data = tmp, sep = "_")
pred1 <- predict(a1, tmp)

tmp2 <- select(data, funder, installer, ward, scheme_name, status_group)
a2 <- dummyVars(status_group ~ ., data = tmp2, sep = "_")
pred2 <- predict(a2, tmp2)

dummy <- as.data.frame(cbind(pred1, pred2))
dummy$dist <- data$dist
dummy$id <- data$id
dummy$func_ratio <- data$func_ratio
dummy$non_func_ratio <- data$non_func_ratio
dummy$needs_repair_ratio <- data$needs_repair_ratio
dummy$population <- data$population
dummy$num_private <- data$num_private
dummy$latitude <- data$latitude
dummy$longitude <- data$longitude
dummy$gps_height <- data$gps_height
dummy$amount_tsh <- data$amount_tsh
dummy$subvillage <- data$subvillage
dummy$type <- data$type

saveRDS(dummy, "data_sparse.RDS")

