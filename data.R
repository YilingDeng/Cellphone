library(data.table)
library(tidyr)
library(ggplot2)
library(rgdal)
library(rgeos)
library(fastcluster)

memory.limit()
memory.size(max = FALSE)
# 2015.12.27 - 2016.1.6
# 27日361 28一362 29二363 30三364 31四365 1五休1 2六休2 3日休3 4一4 5二5 6三6

location <- fread("D:/data/联通数据/初赛数据/shujudasai_1.csv")
location <- unique(location, by = c("V2", "V1"), fromFirst = TRUE)
setkey(location, V2, V1)

# sample
location <- location[1 : 10001, ]
length(unique(location$V2)) # 1128
save.image()

# wide data to long data
day <- strptime(location$V1, "%Y%m%d")
location[, c("yday", "wday") := .(yday(day), ifelse(wday(day) == 1, 7, wday(day) - 1))]

lng <- data.table(na.omit(gather(location[, .(day = V1, yday, wday, imei = V2, '0' = V3, 
                                      '1' = V5, '2' = V7, '3' = V9, '4' = V11, '5' = V13, 
                                      '6' = V15, '7' = V17, '8' = V19, '9' = V21, 
                                      '10' = V23, '11' = V25, '12' = V27, '13' = V29, 
                                      '14' = V31, '15' = V33, '16' = V35, '17' = V37, 
                                      '18' = V39, '19' = V41, '20' = V43, '21' = V45, 
                                      '22' = V47, '23' = V49)], "hour", "lng", 5 : 28)))
lat <- data.table(na.omit(gather(location[, .(day = V1, yday, wday, imei = V2, '0' = V4, 
                                      '1' = V6, '2' = V8, '3' = V10, '4' = V12, '5' = V14, 
                                      '6' = V16, '7' = V18, '8' = V20, '9' = V22, 
                                      '10' = V24, '11' = V26, '12' = V28, '13' = V30, 
                                      '14' = V32, '15' = V34, '16' = V36, '17' = V38, 
                                      '18' = V40, '19' = V42, '20' = V44, '21' = V46, 
                                      '22' = V48, '23' = V50)], "hour", "lat", 5 : 28)))
location <- merge(lng, lat, by = c("imei", "day", "yday", "wday", "hour"))
location[, hour := as.integer(hour)]
setkey(location, imei, day, hour)
rm(lat, lng, day)

# coords
coords <- unique(location[, .(lng, lat)])
coords[, c("x", "y") := .(lng, lat)]
coordinates(coords) <- c("x", "y")
proj4string(coords) <- CRS("+init=epsg:4326")
coords <- spTransform(coords, CRS("+init=epsg:2335"))
coords <- data.table(coordinates(coords), coords@data)
location <- merge(location, coords, by = c("lng", "lat"))
setkey(location, imei, day, hour)

# ggplot(data = coords, aes(x = lng, y = lat)) + geom_point()
# ggplot(data = coords, aes(x = x, y = y)) + geom_point()

# clust
getClust <- function(Coord) {
    Coord <- data.frame(x = Coord[, x], y = Coord[, y])
    if(nrow(Coord) == 1) return(1)
    Clust <- hclust(dist(Coord), method = "complete")
    Clust <- cutree(Clust, h = 400)
    return(Clust)
}

locationNew <- data.table()
for (i in unique(location$imei)) {
    locationOne <- location[imei == i, ]
    locationClust <- data.table(Clust = getClust(locationOne))
    locationOne <- cbind(locationOne, locationClust)
    locationNew <- rbind(locationNew, locationOne)
}
rm(i, locationOne, locationClust, location)

locationNew[, c("x", "y") := .(mean(x), mean(y)), by = .(imei, Clust)]
setkey(locationNew, imei, day, hour)

#  删除1次的地址

# home
home <- locationNew[(yday %in% c(362, 363, 364, 365, 4, 5, 6) & hour %in% c(19, 20, 21, 22, 23, 0, 1, 2, 3, 4, 5, 6)) | yday %in% c(361, 1, 2, 3), ]
home <- home[, .(home = .N), by = .(imei, Clust, x, y)]
setkey(home, imei, home)
home <- unique(home, by = "imei", fromLast = TRUE)
table(home$home)
locationNew <- merge(locationNew, home[, .(imei, Clust, home)], by = c("imei", "Clust"), all.x = TRUE)

# work
work <- locationNew[(yday %in% c(362, 363, 364, 365, 4, 5, 6) & hour %in% c(8, 9, 10, 14, 15, 16)) & is.na(home), ]
work <- work[, .(work = .N), by = .(imei, Clust, x, y)]
setkey(work, imei, work)
work <- unique(work, by = "imei", fromLast = TRUE)
table(work$work)
locationNew <- merge(locationNew, work[, .(imei, Clust, work)], by = c("imei", "Clust"), all.x = TRUE)
setkey(locationNew, imei, day, hour)

# OD 计算通勤距离，分析
od <- locationNew[!is.na(home) | !is.na(work), .(imei, x, y, home, work)]
od <- unique(od)
od[, type := ifelse(is.na(work), "home", "work")]
ggplot(data = od, aes(x = x, y = y, group = type)) + geom_point(aes(color = type)) + geom_path(alpha = 0.2)

# insert start home
homeStart <- data.table(lng = rep(NA, 11), lat = rep(NA, 11), day = c("20151227", "20151228","20151229","20151230","20151231","20160101","20160102","20160103","20160104","20160105","20160106"), 
                      yday = c(361, 362, 363, 364, 365, 1, 2, 3, 4, 5, 6), wday = c(7, 1, 2, 3, 4, 5, 6, 7, 1, 2, 3), hour = rep(3, 11), work = rep(NA, 11))
homeRep <- home[rep(seq(.N), 11), ]
setkey(homeRep, imei)
homeStart <- cbind(homeRep, homeStart)
withHome <- home[, imei] # remove imei which can not detect home
locationNew <- rbind(locationNew[imei %in% withHome & hour != 3, ], homeStart)
rm(homeStart, homeRep, withHome)

# create model day
setkey(locationNew, imei, day, hour)
locationNew[, modelDay := ifelse(hour %in% c(0, 1, 2), yday - 1, yday)]
locationNew[modelDay == 0, modelDay := 365]
locationNew <- locationNew[modelDay != 360, ] # yday 6 特别少

# remove duplicate location 1/2
setkey(locationNew, imei, day, hour)
locationNew[, ClustLag := shift(Clust, type = "lag"), by = .(imei, modelDay)]
locationNew[, ClustDiff := .(ifelse(Clust == ClustLag, 0, 1))]
locationNew[is.na(ClustDiff), ClustDiff := 0]
locationNew[, ClustIndex := cumsum(ClustDiff), by = .(imei, modelDay)]
locationNew <- unique(locationNew, by = c("imei", "modelDay", "ClustIndex"), fromFirst = TRUE)
locationNew[, c("ClustLag", "ClustDiff", "ClustIndex") := NULL]

# generate tour
locationNew[, TourIndex := ifelse(is.na(home), 0, 1), by = .(imei, modelDay)]
locationNew[, tour := cumsum(TourIndex), by = .(imei, modelDay)]

# generate activtiy & H motif
getActivity <- function(Clust) {
  Activity <- vector()
  Activity[1] <- 1
  j <- 1
  if (length(Clust) == 1) {
    return(Activity)
  }
  else {
    for (i in 2 : length(Clust)) {
      if (Clust[i] %in% Clust[1 : i - 1]) {
        Activity[i] <- Activity[match(Clust[i], Clust[1 : i - 1])]
        
      }
      else {
        j <- j + 1
        Activity[i] <- j
      }
    }
    return(Activity)
  }
}

setkey(locationNew, imei, day, hour)
locationNew[, TourDiff := cumsum(TourIndex)]
for (i in 1 : max(locationNew[, TourDiff])) {
  Activity <- getActivity(locationNew[TourDiff == i, Clust])
  locationNew[TourDiff == i, activity := Activity]
  locationNew[TourDiff == i & TourIndex == 1, motif := paste(Activity, collapse = '-')]
}
rm(i, Activity)
locationNew[, c("TourIndex", "TourDiff") := NULL]
locationNew <- locationNew[is.na(motif) | motif != "1", ] # 删除只有1个activty的tour

# generate activity & HW/HO motif
getActivity <- function(Clust) {
  Activity <- vector()
  Activity[1] <- 1
  j <- 2
  if (nrow(Clust) == 1) {
    return(Activity)
  }
  else {
    for (i in 2 : nrow(Clust)) {
      if (!is.na(Clust[i, work])) {
        Activity[i] <- 2
      }
      else {
        if (Clust[i, Clust] %in% Clust[1 : i - 1, Clust]) {
          Activity[i] <- Activity[match(Clust[i, Clust], Clust[1 : i - 1, Clust])]
          
        }
        else {
          j <- j + 1
          Activity[i] <- j
        }
      }
    }
    return(Activity)
  }
}

setkey(locationNew, imei, day, hour)
locationNew[, TourDiff := cumsum(TourIndex)]
for (i in 1 : max(locationNew[, TourDiff])) {
  Activity <- getActivity(locationNew[TourDiff == i, .(Clust, work)])
  locationNew[TourDiff == i, activity := Activity]
  locationNew[TourDiff == i & TourIndex == 1, motif := paste(Activity, collapse = '-')]
}
rm(i, Activity)
locationNew[, c("TourIndex", "TourDiff") := NULL]
locationNew <- locationNew[is.na(motif) | motif != "1", ] # 删除只有1个activty的tour

# view motif
sort(table(locationNew[, motif]))
length(locationNew[!is.na(motif), motif]) # 11485
length(locationNew[!is.na(motif) & grepl("2", motif), motif]) # 4665
length(unique(locationNew[!is.na(motif), motif])) # 506
length(unique(locationNew[!is.na(motif) & grepl("2", motif), motif])) # 374

# generate pattern
locationNew[!is.na(motif), motifType := ifelse(grepl("2", motif), )]

# view pattern

# 样本 "20151228" 不能完全排除中途点
ggplot(data = locationNew[imei == "00004822f78c4bd256cefccc4b82832f" & modelDay == 362, ], aes(x = x, y = y)) + 
  geom_point() + geom_path() + geom_text(aes(label = hour), size = 4)
locationNew[imei == "00004822f78c4bd256cefccc4b82832f" & modelDay == 362, ]
unique(locationNew[imei == "00004822f78c4bd256cefccc4b82832f" & day == 362, .(x, y)])






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