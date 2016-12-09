library(data.table)
library(ggplot2)
library(rgdal)
library(rgeos)
library(fastcluster)

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

lng <- melt(location[, .(day = V1, yday, wday, imei = V2, '0' = V3, '1' = V5, '2' = V7, 
                         '3' = V9, '4' = V11, '5' = V13, '6' = V15, '7' = V17, '8' = V19, 
                         '9' = V21, '10' = V23, '11' = V25, '12' = V27, '13' = V29, 
                         '14' = V31, '15' = V33, '16' = V35, '17' = V37, '18' = V39, 
                         '19' = V41, '20' = V43, '21' = V45, '22' = V47, '23' = V49)], 
            id = c("imei", "day", "yday", "wday"), variable.name = "hour", value.name = "lng", na.rm = TRUE)
lat <- melt(location[, .(day = V1, yday, wday, imei = V2, '0' = V4, '1' = V6, '2' = V8, 
                         '3' = V10, '4' = V12, '5' = V14, '6' = V16, '7' = V18, '8' = V20, 
                         '9' = V22, '10' = V24, '11' = V26, '12' = V28, '13' = V30,
                         '14' = V32, '15' = V34, '16' = V36, '17' = V38, '18' = V40, 
                         '19' = V42, '20' = V44, '21' = V46, '22' = V48, '23' = V50)], 
            id = c("imei", "day", "yday", "wday"), variable.name = "hour", value.name = "lat", na.rm = TRUE)
location <- merge(lng, lat, by = c("imei", "day", "yday", "wday", "hour"))
location[, hour := as.integer(hour)]
setkey(location, imei, day, hour)
location[, modelDay := ifelse(hour %in% c(0, 1, 2), yday - 1, yday)]
location[modelDay == 0, modelDay := 365]
rm(lat, lng, day)

# 20160106 特别少，删除，减少数据量 58877240 -> 55471159
location <- location[modelDay != 360 & modelDay != 6, ]
# save.image()

# 检查小时、天、imei的记录数量
byHour <- location[, .N, by = hour]
ggplot(data = byHour, aes(x = hour, y = N)) + geom_bar(stat = "identity")

bymodelDay <- location[, .N, by = modelDay]
ggplot(data = bymodelDay, aes(x = as.factor(modelDay), y = N)) + geom_bar(stat = "identity")

byImei <- location[, .(count = .N), by = .(imei, modelDay)][, .N, by = count]
ggplot(data = byImei, aes(x = count, y = N)) + geom_bar(stat = "identity")

# 删除8条记录以下的天 55471159 -> 53527282
location[, count := .N, by = .(imei, modelDay)]
location <- location[count >= 8, ]

# 检查不同天数的imei数量
dayCount <- location[, .(homeDay = unique(modelDay)), by = imei]
dayCount[, workDay := ifelse(homeDay %in% c(362 : 365, 4, 5), 1, 0) ]
dayCount <- dayCount[, .(homeDay = .N, workDay = sum(workDay)), by = imei]
ggplot(data = dayCount[, .N, by = homeDay], aes(x = homeDay, y = N)) + geom_bar(stat = "identity")

# coords
coords <- unique(location[, .(lng, lat)])
coords[, c("x", "y") := .(lng, lat)]
coordinates(coords) <- c("x", "y")
proj4string(coords) <- CRS("+init=epsg:4326")
coords <- spTransform(coords, CRS("+init=epsg:2335"))
coords <- data.table(coordinates(coords), coords@data)
# write.csv(coords, "coords.csv")
location <- merge(location, coords, by = c("lng", "lat"))
setkey(location, imei, day, hour)

# ggplot(data = coords, aes(x = lng, y = lat)) + geom_point()
# ggplot(data = coords, aes(x = x, y = y)) + geom_point()

# clust
getClust <- function(Coord) {
    Coord <- data.frame(x = Coord[, x], y = Coord[, y])
    if(nrow(Coord) == 1) return(1L)
    Clust <- hclust(dist(Coord), method = "complete")
    Clust <- cutree(Clust, h = 600)
    return(Clust)
}
location[, Clust := getClust(.SD), by = imei]
location[, c("x", "y", "count") := .(mean(x), mean(y), .N), by = .(imei, Clust)]
setkey(location, imei, day, hour)

# remove 1 time stop 53527282 -> 48490103
location <- location[count > 1, ]
location[, count := NULL]

# home
home <- location[(yday %in% c(362, 363, 364, 365, 4, 5, 6) & hour %in% c(19 : 23, 0 : 6)) | yday %in% c(361, 1, 2, 3), ]
home <- home[, .(home = .N), by = .(imei, Clust, x, y)]
setkey(home, imei, home)
home <- unique(home, by = "imei", fromLast = TRUE) 
home <- merge(home, dayCount, by = "imei")
home <- home[home >= homeDay]
location <- location[imei %in% home$imei, ]
location <- merge(location, home[, .(imei, Clust, home)], by = c("imei", "Clust"), all.x = TRUE)

# work
work <- location[(yday %in% c(362, 363, 364, 365, 4, 5, 6) & hour %in% c(8, 9, 10, 11, 13, 14, 15, 16, 17)) & is.na(home), ]
work <- work[, .(work = .N), by = .(imei, Clust, x, y)]
setkey(work, imei, work)
work <- unique(work, by = "imei", fromLast = TRUE)
work <- merge(work, dayCount, by = "imei")
work <- work[work >= workDay / 2 & work > 1, ]
location <- merge(location, work[, .(imei, Clust, work)], by = c("imei", "Clust"), all.x = TRUE)
setkey(location, imei, day, hour)

# OD 计算通勤距离，分析
od <- location[!is.na(home) | !is.na(work), .(imei, x, y, home, work)]
od <- unique(od)
od[, type := ifelse(is.na(work), "home", "work")]
# ggplot(data = od, aes(x = x, y = y, group = type)) + geom_point(aes(color = type)) + geom_path(alpha = 0.2)

# insert start home
homeStart <- data.table(lng = rep(NA, 10), lat = rep(NA, 10), day = c("20151227", "20151228","20151229","20151230","20151231","20160101","20160102","20160103","20160104","20160105"), 
                      yday = c(361, 362, 363, 364, 365, 1, 2, 3, 4, 5), 
                      wday = c(7, 1, 2, 3, 4, 5, 6, 7, 1, 2), 
                      modelDay = c(361, 362, 363, 364, 365, 1, 2, 3, 4, 5), 
                      hour = rep(3, 10), work = rep(NA, 10))
homeRep <- home[rep(seq(.N), 10), .(imei, Clust, x, y, home)]
setkey(homeRep, imei)
homeStart <- cbind(homeRep, homeStart)
location <- rbind(location[hour != 3, ], homeStart)
location[, count := .N, by = .(imei, modelDay)]
location <- location[count > 1, ]
location[, count := NULL] # 删除在空白天插入的数据
setkey(location, imei, day, hour)
rm(homeStart, homeRep)

# 只研究workday
location <- location[!modelDay %in% c(1, 2, 3, 361), ]

# Choice 2: remove 1 hour stop (Clust-NA-NA删除, 不采用)
location[, ClustLag := shift(Clust, type = "lag"), by = .(imei, modelDay)]
location[, ClustDiff := ifelse(Clust == ClustLag, 0, 1)]
location[is.na(ClustDiff), ClustDiff := 0]
location <- location[ClustDiff == 0 | !is.na(home) | !is.na(work), ]

# remove duplicate stop
location[, ClustLag := shift(Clust, type = "lag"), by = .(imei, modelDay)]
location[, ClustDiff := ifelse(Clust == ClustLag, 0, 1)]
location[is.na(ClustDiff), ClustDiff := 0]
location[, ClustIndex := cumsum(ClustDiff), by = .(imei, modelDay)]
location <- unique(location, by = c("imei", "modelDay", "ClustIndex"), fromFirst = TRUE)
location[, c("ClustLag", "ClustDiff", "ClustIndex") := NULL]

# choice 1: remove 1 hour stop (Clust-NA-NA保留)
location[, hourLag := shift(hour, type = "lag"), by = imei]
location[, duration := ifelse(hour > hourLag, hour - hourLag, hour - hourLag + 24)]
location <- location[duration > 1 | !is.na(home) | !is.na(work), ]
location[, c("hourLag", "duration") := NULL]

# remove duplicate stop
location[, ClustLag := shift(Clust, type = "lag"), by = .(imei, modelDay)]
location[, ClustDiff := ifelse(Clust == ClustLag, 0, 1)]
location[is.na(ClustDiff), ClustDiff := 0]
location[, ClustIndex := cumsum(ClustDiff), by = .(imei, modelDay)]
location <- unique(location, by = c("imei", "modelDay", "ClustIndex"), fromFirst = TRUE)
location[, c("ClustLag", "ClustDiff", "ClustIndex") := NULL]

# choice 3: all remain

# generate tourNum
location[, TourIndex := ifelse(is.na(home), 0, 1), by = .(imei, modelDay)]
location[, tourNum := cumsum(TourIndex), by = .(imei, modelDay)]

# choice B: generate H tour (不采用)
getTour <- function(Clust) {
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

# choice A : generate HW/HO tour
getTour <- function(Clust) {
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

# generate tour
setkey(location, imei, day, hour)
location[, TourDiff := cumsum(TourIndex)]
location[, activityNum := getTour(.SD), by = TourDiff]
location[, tour := paste(activityNum, collapse = '-'), by = TourDiff]
location[TourIndex != 1, tour := NA]
location[, c("TourIndex", "TourDiff") := NULL]
location <- location[is.na(tour) | tour != "1" | hour == 3, ] # 删除只有1个activty的tour，但保留没有出行的

# generate tour type
location[!is.na(tour), tourType1 := ifelse(nchar(tour) > 3, "C", "S")] 
location[!is.na(tour), tourType2 := ifelse(grepl("2", tour), "HW", "HO")]
location[!is.na(tour), tourType := paste0(tourType1, tourType2)]
location[tour == 1, tourType := "H"]
location[, c("tourType1", "tourType2") := .(NULL, NULL)]

# generate pattern
getPattern <- function(Clust) {
  Activity <- vector()
  Activity[1] <- 1
  j <- 2
  if (nrow(Clust) == 1) {
    return(Activity)
  }
  else {
    for (i in 2 : nrow(Clust)) {
      if (!is.na(Clust[i, home])) {
        Activity[i] <- 1
      }
      else {
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
    }
  }
  return(Activity)
}

setkey(location, imei, day, hour)
location[, activity := getPattern(.SD), by = .(imei, modelDay)]
location[, pattern := paste(activity, collapse = '-'), by = .(imei, modelDay)]
location[hour != 3, pattern := NA]

# generate pattern type
location[!is.na(tourType), patternType := paste(tourType, collapse = '-'), by =.(imei, modelDay)]
location[hour != 3, patternType := NA]

# view pattern
sort(table(location[, pattern]))
location[!is.na(pattern), .N] # pattern数量
length(unique(location[!is.na(pattern), pattern])) # pattern种类数
sort(table(location[, patternType]), decreasing = TRUE)

# view tour
sort(table(location[, tour]))
location[!is.na(tour), .N] # tour数量
location[!is.na(tour), .N, by = modelDay] # tour数量 by modelDay
location[!is.na(tour) & grepl("2", tour), .N] # work-tour数量
location[!is.na(tour) & grepl("2", tour), .N, by = modelDay] # work-tour数量 by modelDay
length(unique(location[!is.na(tour), tour])) # tour种类数
length(unique(location[!is.na(tour) & grepl("2", tour), tour])) # work-tour种类数
table(location[, tourType]) # CHO CHW SHO SHW 1545 788 2287 662

# view trips
mean(location[, .N, by = .(imei, modelDay)][N == 1, N := 0][, N])
mean(location[, .N, by = .(imei, modelDay)][N > 1, N])
location[, .N, by = .(imei, modelDay)][N == 1, N := 0][, mean(N), by = modelDay]
table(location[, .N, by = .(imei, modelDay)][N == 1, N := 0][, N])
ggplot(data = location[, .N, by = .(imei, modelDay)][N == 1, N := 0], aes(x = N)) + geom_histogram(binwidth = 1)

# H VS W VS O
location[is.na(home), .N]

# 样本 "20151228" 不能完全排除中途点
ggplot(data = location[imei == "00004822f78c4bd256cefccc4b82832f" & modelDay == 362, ], aes(x = x, y = y)) + 
  geom_point() + geom_path() + geom_text(aes(label = hour), size = 4)
location[imei == "00004822f78c4bd256cefccc4b82832f" & modelDay == 362, ]
unique(location[imei == "00004822f78c4bd256cefccc4b82832f" & modelDay == 362, .(x, y)])

# read usuage
usuage <- fread("D:/data/联通数据/初赛数据/沃+数据大赛数据.csv")
usuage <- unique(usuage)
# IMEI                 月银行短信通知次数   使用汽车类APP的PV数  使用理财类APP的PV数  使用股票类APP的PV数 
# 交往圈规模           是否有跨省行为       是否有出国行为       访问购物类网站的次数 访问IT类网站的次数  
# 访问餐饮类网站的次数 访问房产类网站的次数 访问健康类网站的次数 访问金融类网站的次数 访问旅游类网站的次数
# 访问体育类网站的次数 访问汽车类网站的次数 访问时事类网站的次数 访问社会类网站的次数 访问文娱类网站的次数
# 访问招聘类网站的次数 访问教育类网站的次数 访问其他类网站的次数 访问网游类网站的次数

# read brand 无法对应
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