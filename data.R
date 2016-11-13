library(data.table)
library(tidyr)
library(ggplot2)
library(rgdal)
library(rgeos)
library(fastcluster)

memory.limit()
memory.size(max = FALSE)
# rm(NA)
# 2015.12.27 - 2016.1.6
# 27日361 28一362 29二363 30三364 31四365 1五休 2六休 3日休 4一 5二 6三

location <- fread("D:/data/联通数据/初赛数据/shujudasai_1.csv")
location <- unique(location, by = c("V2", "V1"), fromFirst = TRUE)
setkey(location, V2, V1)
location <- location[1 : 10001, ]
length(unique(location$V2)) # 1128
save.image()
day <- strptime(location$V1, "%Y%m%d")
location[, c("yday", "wday") := .(yday(day), ifelse(wday(day) == 1, 7, wday(day) - 1))]

lng <- data.table(gather(location[, .(day = V1, yday, wday, imei = V2, '0' = V3, 
                                      '1' = V5, '2' = V7, '3' = V9, '4' = V11, '5' = V13, 
                                      '6' = V15, '7' = V17, '8' = V19, '9' = V21, 
                                      '10' = V23, '11' = V25, '12' = V27, '13' = V29, 
                                      '14' = V31, '15' = V33, '16' = V35, '17' = V37, 
                                      '18' = V39, '19' = V41, '20' = V43, '21' = V45, 
                                      '22' = V47, '23' = V49)], "hour", "lng", 5 : 28))
lat <- data.table(gather(location[, .(day = V1, yday, wday, imei = V2, '0' = V4, 
                                      '1' = V6, '2' = V8, '3' = V10, '4' = V12, '5' = V14, 
                                      '6' = V16, '7' = V18, '8' = V20, '9' = V22, 
                                      '10' = V24, '11' = V26, '12' = V28, '13' = V30, 
                                      '14' = V32, '15' = V34, '16' = V36, '17' = V38, 
                                      '18' = V40, '19' = V42, '20' = V44, '21' = V46, 
                                      '22' = V48, '23' = V50)], "hour", "lat", 5 : 28))
location <- merge(lng, lat, by = c("imei", "day", "yday", "wday", "hour"))
location[, hour := as.integer(hour)]
setkey(location, imei, day, hour)
rm(lat, lng, day)

# 样本
ggplot(data = na.omit(location[imei == "00004822f78c4bd256cefccc4b82832f" & day == "20151228", ]), aes(x = lng, y = lat)) + 
    geom_point() + geom_path() + geom_text(aes(label = hour), size = 4)
location[imei == "00004822f78c4bd256cefccc4b82832f" & day == "20151228", ]
unique(location[imei == "00004822f78c4bd256cefccc4b82832f" & day == "20151228", .(lng, lat)])

# coords
coords <- na.omit(unique(location[, .(lng, lat)]))
coords[, c("x", "y") := .(lng, lat)]
coordinates(coords) <- c("x", "y")
proj4string(coords) <- CRS("+init=epsg:4326")
coords <- spTransform(coords, CRS("+init=epsg:2335"))
coords <- data.table(coordinates(coords), coords@data)
ggplot(data = coords, aes(x = lng, y = lat)) + geom_point()
ggplot(data = coords, aes(x = x, y = y)) + geom_point()
location <- merge(location, coords, by = c("lng", "lat"), all.x = TRUE)
setkey(location, imei, day, hour)

# clust
getClust <- function(Coord) {
    Coord <- data.frame(x = Coord[, x], y = Coord[, y])
    if(nrow(Coord) < 2) return(rep(NULL, nrow(Coord)))
    Clust <- hclust(dist(Coord), method = "complete")
    Clust <- cutree(Clust, h = 300)
    return(Clust)
}

locationNA <- na.omit(location) ###
locationNA[, count := .N, by = imei] ###
locationNA <- locationNA[count >= 22, ]
locationNew <- data.table()
for (i in unique(locationNA$imei)) {
    locationOne <- locationNA[imei == i, ]
    locationClust <- data.table(Clust = getClust(locationOne))
    locationOne <- cbind(locationOne, locationClust)
    locationNew <- rbind(locationNew, locationOne)
}

rm(i, Clust, locationOne, locationClust)

locationNew[, c("x", "y") := .(mean(x), mean(y)), by = .(imei, Clust)]
setkey(locationNew, imei, day, hour)

# 去除多余的
locationNew[, ClustLag := shift(Clust, type = "lag"), by = imei]
locationNew[, ClustDiff := .(ifelse(Clust == ClustLag, 0, 1))]
locationNew[is.na(ClustDiff), ClustDiff := 0]
locationNew[, ClustIndex := cumsum(ClustDiff), by = imei]
locationNew <- unique(locationNew, by = c("imei", "ClustIndex"), fromFirst = TRUE)


ggplot(data = na.omit(locationNew[imei == "00004822f78c4bd256cefccc4b82832f" & day == "20151228", ]), aes(x = lng, y = lat)) + 
    geom_point() + geom_path() + geom_text(aes(label = hour), size = 4)

# home
home <- na.omit(location[(yday %in% c(362, 363, 364, 365, 4, 5, 6) & hour %in% c(21, 22, 23, 0, 1, 2, 3, 4, 5, 6)) | yday %in% c(361, 1, 2, 3), ])
home <- home[, .(count = .N), by = .(imei, lng, lat)]
setkey(home, imei, count)
home <- unique(home, by = "imei", fromLast = TRUE)

# work
work <- na.omit(location[(yday %in% c(362, 363, 364, 365, 4, 5, 6) & hour %in% c(8, 9, 10, 14, 15, 16)), ])
work <- work[, .(count = .N), by = .(imei, lng, lat)]
setkey(work, imei, count)
work <- unique(work, by = "imei", fromLast = TRUE)

# OD
od <- rbind(cbind(home, type = "home"), cbind(work, type = "work"))
ggplot(data = od, aes(x = lng, y = lat, group = type)) + geom_point(aes(color = type))












usuage <- fread("D:/data/联通数据/初赛数据/沃+数据大赛数据.csv")
usuage <- unique(usuage)

brand <- data.table()
for (i in c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) {
    temp <- fread(paste0("D:/data/联通数据/初赛数据/数据大赛2015", i, ".csv"))
    brand <- rbind(brand, temp)
}
rm(temp, i)

brand[, c("网别", "性别", "年龄值段", "ARPU值段", "终端品牌", "终端型号", "流量使用量", "语音通话时长", "短信条数") := 
          list(as.factor(网别), as.factor(性别), as.factor(年龄值段), as.factor(ARPU值段), 
               as.factor(终端品牌), as.factor(终端型号), as.factor(流量使用量), 
               as.integer(语音通话时长), as.integer(短信条数))]

str(brand)