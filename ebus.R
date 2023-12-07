
library(tidyr)
library(dplyr)
library(purrr)
library(stringr)


ebus <- read.csv("/Users/brenna/Downloads/BUS04_2023_06.csv")[-1, ]

head(test)

ebus <- tx

head(ebus)

ebus$Latitude <- as.numeric(ebus$Latitude)
ebus$Longitude <- as.numeric(ebus$Longitude)

ebus$lat <- ebus$Latitude
ebus$lon <- ebus$Longitude

ebus <- ebus[which(!ebus$lon == -9999), ]
ebus <- ebus[which(!ebus$lat == -9999), ]

head(ebus)

names(ebus)


library(sf)
library(tmap)
library(viridis)

ebus_sf <- st_as_sf(ebus, coords = c("lon", "lat"), crs = 4326)

ebus$Bus_Top_Temperature <- as.numeric(ifelse(ebus$Bus_Top_Temperature == -9999, NA, 
                                              ebus$Bus_Top_Temperature))


tm_shape(ebus_sf) +
  tm_dots(col = "Bus_Top_Temperature", palette = "viridis", legend.show = FALSE)


files <- list.files("/Users/brenna/Downloads/ebuses only", full.names = T, pattern = "\\csv$")

dat_bus <- read.csv(files[1])
dat_bus$name <- files[1]# substr(files[1], (n - 13), (n - 4))
dat_bus[c(0), ]

for(i in 1:length(files)) {
  
  dat <- read.csv(files[i])
  print(i)
  dat$name <- files[i] #paste0("bus_", substr(files[i], (n - 13), (n - 4)))
  dat_bus <- rbind(dat_bus, dat)
  
}

##### other pm, get to mesh lower
table(dat_bus$PM2.5_Data_Flagged)

files <- list.files("/Users/brenna/Downloads/trax only", full.names = T, pattern = "\\csv$")

dat_trax <- read.csv(files[1])
dat_trax$name <- paste0("trax_", substr(files[1], (n - 12), (n - 11)))
dat_trax <- dat_trax[c(0), ]

for(i in 1:length(files)) {
  
  dat <- read.csv(files[i])
  dat$name <- files[i] #paste0("trax_", substr(files[i], (n - 12), (n - 11)))
  dat_trax <- rbind(dat_trax, dat)
  
}


names(files) <- c("file")
n = max(nchar(files))
files$name <-  substr(files$file, (n - 13), (n - 4))
substr(list.files("/Users/brenna/Downloads/EBUS_TEST_DATA-2", full.names = T, pattern = "\\csv$"), (n - 13), (n - 4))

# for all files, trax and bus
dat <- map(list.files("/Users/brenna/Downloads/EBUS_TEST_DATA-2", full.names = T, pattern = "\\csv$"), read.csv)

# for bus files 1-2; bus files 3-6
dat <- map(list.files("/Users/brenna/Downloads/ebuses only", full.names = T, pattern = "\\csv$"), read.csv)

dat <- dat[-7]

test <- dat[[1]]
test <- test[-1, ]


lapply(dat, names)
new_dat <- data.frame(matrix(NA, ncol = 17, nrow=0))
names(new_dat) <- c("monitor",
                    "Timestamp", "Latitude", "Longitude", 
                    # pollutants, temperature
                    "Top_Temperature",
                    "PM2.5_Concentration",
                    "Ozone_Concentration",
                    "Top_Relative_Humidity",
                    # specs
                    #"GPS_Speed", "GPS_Direction", "GPS_RMC_Valid",
                    # "Battery_Voltage", "Air_Flow_Rate", #"Error_Code",
                    # # validation
                     "Internal_Air_Temperature", 
                     "Internal_Relative_Humidity", 
                    # "Internal_Air_Pressure", 
                     "Box_Temperature", 
                    # # flags
                     "Ozone_Data_Flagged",
                     "PM2.5_Data_Flagged", 
                    # only buses 1-2
                    "NO_Concentration",
                    "NO2_Concentration",
                    "NOX_Concentration",
                    "GPS_Data_Flagged"
                    )

# for all:
1:length(dat)
# for bus files 1-2
1:2
# for bus files 3-6
3:6

for(i in 1:2){#length(dat)) {
  
  f <- dat[[i]]
  
  f$monitor <- paste0("file_", i)
  
  names(f) <- ifelse(names(f) == names(f[which(grepl("Box_Temperature", names(f)) == TRUE)]),
                     "Box_Temperature", names(f))
  
  # bus only
  names(f) <- ifelse(names(f) == names(f[which(grepl("NO_Concentration", names(f)) == TRUE)]),
                     "NO_Concentration", names(f))
  names(f) <- ifelse(names(f) == names(f[which(grepl("NO2_Concentration", names(f)) == TRUE)]),
                     "NO2_Concentration", names(f))
  names(f) <- ifelse(names(f) == names(f[which(grepl("NOX_Concentration", names(f)) == TRUE)]),
                     "NOX_Concentration", names(f))
  names(f) <- ifelse(names(f) == names(f[which(grepl("GPS_Data_Flagged", names(f)) == TRUE)]),
                     "GPS_Data_Flagged", names(f))
  
  # all (trax and bus):
  names(f) <- ifelse(names(f) == names(f[which(grepl("Internal_Air_Temperature", names(f)) == TRUE)]),
                     "Internal_Air_Temperature", names(f))
  names(f) <- ifelse(names(f) == names(f[which(grepl("Internal_Relative_Humidity", names(f)) == TRUE)]),
                     "Internal_Relative_Humidity", names(f))
  #names(f) <- ifelse(names(f) == names(f[which(grepl("Internal_Air_Pressure", names(f)) == TRUE)]),
  #                   "Internal_Air_Pressure", names(f))
  names(f) <- ifelse(names(f) == names(f[which(grepl("Error_Code", names(f)) == TRUE)]),
                     "Error_Code", names(f))
  
  # PM1,4,10 not available for trax?
  #names(f) <- ifelse(names(f) == names(f[which(grepl("PM1_Concentration", names(f)) == TRUE)]),
  #                   "PM1_Concentration", names(f))
  #names(f) <- ifelse(names(f) == names(f[which(grepl("PM4_Concentration", names(f)) == TRUE)]),
  #                   "PM4_Concentration", names(f))
  #names(f) <- ifelse(names(f) == names(f[which(grepl("PM10_Concentration", names(f)) == TRUE)]),
  #                   "PM10_Concentration", names(f))
  names(f) <- ifelse(names(f) == names(f[which(grepl("PM2.5_Concentration", names(f)) == TRUE)]),
                     "PM2.5_Concentration", names(f))
  names(f) <- ifelse(names(f) == names(f[which(grepl("Ozone_Concentration", names(f)) == TRUE)]),
                     "Ozone_Concentration", names(f))
  names(f) <- ifelse(names(f) == names(f[which(grepl("Top_Temperature", names(f)) == TRUE)]),
                     "Top_Temperature", names(f))
  names(f) <- ifelse(names(f) == names(f[which(grepl("Top_Relative_Humidity", names(f)) == TRUE)]),
                     "Top_Relative_Humidity", names(f))
  
  print(names(f))
  
  f <- f[-1, c("monitor",
               "Timestamp", "Latitude", "Longitude", # space, time
               # pollutants, temperature
               "Top_Temperature",
               "PM2.5_Concentration",
               "Ozone_Concentration",
               "Top_Relative_Humidity",
               # specs
               # #"GPS_Speed", "GPS_Direction", "GPS_RMC_Valid",
               # "Battery_Voltage", "Air_Flow_Rate", # "Error_Code"
               # # validation
                "Internal_Air_Temperature", 
                "Internal_Relative_Humidity", 
               # "Internal_Air_Pressure", 
                "Box_Temperature", 
               # # flags
                "Ozone_Data_Flagged",
                "PM2.5_Data_Flagged", 
               # only bus
               "NO_Concentration",
               "NO2_Concentration",
               "NOX_Concentration",
               "GPS_Data_Flagged"
               )]
  
  new_dat <- rbind(f, new_dat)
}

head(new_dat)

for(i in names(new_dat)[3:length(new_dat)]) {
  new_dat[, i] <- as.numeric(new_dat[, i])
  new_dat[, i] <- ifelse(new_dat[, i] == -9999,
                         NA, new_dat[, i])
}

summary(new_dat)


all_data# <- new_dat

s <- list.files("/Users/brenna/Downloads/EBUS_TEST_DATA-2", full.names = T, pattern = "\\csv$")
table(new_dat$monitor)

table(new_dat$monitor %in% c("file_7", "file_8",
                             "file_9", "file_10"))

new_dat$bus_rail <- ifelse(new_dat$monitor %in% c("file_7", "file_8",
                                              "file_9", "file_10"),
                       "rail", "bus")

prop.table(table(new_dat$bus_rail,
      new_dat$pm2.5_data_flagged))

names(new_dat) <- tolower(names(new_dat))

new_dat_sf <- new_dat |>
  filter(!is.na(longitude))
new_dat_sf <- st_as_sf(new_dat_sf, coords = c("longitude", "latitude"),
                       crs = 4326)

tm_shape(new_dat_sf) +
  tm_dots(col = "top_temperature", palette = "viridis")#, legend.show = FALSE)

hist(new_dat_sf$top_temperature)
table(new_dat_sf$top_temperature < 0)
table(new_dat_sf$internal_air_temperature < 0)

small <- new_dat_sf |>
  filter(ozone_data_flagged == 0) |>
  filter(pm2.5_data_flagged == 0) |>
  filter(top_temperature > 0) |>
  filter(internal_air_temperature > 0)

summary(small)



tm_shape(small) +
  tm_dots(col = "top_temperature", palette = "viridis", style = "cont", alpha = 0.25)#, legend.show = FALSE)

st_crs(grid) <- 4326
grid <- st_transform(grid, st_crs(small))

system.time(
  grid_dat <- st_join(grid, small, st_contains) |>
    filter(!is.na(timestamp))
)

#grid_dat <- st_join(grid, small, st_contains) |>
#  filter(!is.na(timestamp))

head(grid_dat)

test <- as.data.frame(table(grid_dat$raster.tif))
names(test) <- c("raster.tif", "count")

min(small$timestamp)

summary(grid)

test <- merge(grid, test)
test$count_log <- log(test$count)

test$count_10 <- ifelse(test$count >= 10, "10", "less")

prop.table(table(test$count_20))

tm_shape(test) +
  tm_polygons(col = "count_10", lwd = 0, style = "cont", palette = "viridis")

table(test$count == 10)
max(test$count)

hist(test[which(test$count < 5000), ]$count)

library(lubridate)

#test_time <- aggregate(small$, by = list(small$timestamp))
small$timestamp_test <- ymd_hms(small$timestamp)
small$hour <- round_date(small$timestamp_test, unit = "hour")
small$day <- round_date(small$timestamp_test, unit = "day")
small$week <- round_date(small$timestamp_test, unit = "week")
small$month <- round_date(small$timestamp_test, unit = "month")

length(unique(small$hour)) / 2101552

tm <- as.data.frame(table(small$week))
names(tm) <- c("time", "count")

library(ggpubr)
library(ggplot2)

table(tm$time > 2023-06-30)

tm$time[5:9]

tm <- tm |>
  filter(time %in% tm$time[5:9])
# hour: 270:1002
# day: 23:53

summary(tm$count)

tm$time_md <- str_sub(tm$time, start = 7, end = 10)

ggplot(tm, aes(x = time, y = count)) +
  #geom_line() +
  geom_point(size = 2, alpha = 1) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank()) +
  ylim(0, 600000) + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme_minimal()

table(small$month)
table(small$week)



prop.table(table(test$count > 20))
prop.table(table(test$count > 10))
prop.table(table(test$count > 1))
hist(test$count)


prop.table(table(new_dat_sf$ozone_data_flagged))
prop.table(table(new_dat_sf$pm2.5_data_flagged))


dat <- map(list.files("/Users/brenna/Downloads/ebuses only", full.names = T, pattern = "\\csv$"), read.csv) |>
  bind_rows()


tx <- map(list.files("/Users/brenna/Downloads/trax only", full.names = T, pattern = "\\csv$"), read.csv) |>
  bind_rows()


acs <- map(list.files("/Users/brenna/Documents/School/PHS 7050/final project/data", full.names = T, pattern = "\\.shp$"), st_read) %>%
  bind_rows()

head(dat)

lapply(dat, )

  #bind_rows()
  print(length(names()))
  
head(dat)


c(10**2,19**2,37**2)




