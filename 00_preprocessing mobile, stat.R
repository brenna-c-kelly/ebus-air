
#### preprocessing stationary data

library(dplyr)
library(lubridate)

o3_files <- paste0("data/aqs/o3/", list.files("data/aqs/o3/"))

for(i in 1:length(unique(o3_files))) {
  
  o3 <- read.csv(o3_files[i]) |>
    filter(State.Name == "Utah" & 
             County.Name == "Salt Lake")
  table(o3$State.Code)
  write.csv(o3, o3_files[i], 
            row.names = FALSE)
  
}

o3_files <- paste0("data/aqs/o3/", list.files("data/aqs/o3/"))

o3_table <- lapply(o3_files, read.csv, header = TRUE)

o3.df <- do.call(rbind , o3_table) |>
  select(c("Site.Num", "Latitude", "Longitude", "Parameter.Name", 
           "Date.Local", "Time.Local", # use local time, not GMT (Greenwich Mean Time)
           "Sample.Measurement", "Units.of.Measure"))

names(o3.df) <- tolower(names(o3.df))

o3.df$day_time <- ymd_hm(paste(o3.df$date.local, o3.df$time.local), tz = "UTC")
# o3.df$day_time <- paste0(gsub(" ", "T", o3.df$day_time), "Z")
o3.df$date <- ymd(o3.df$date.local)
o3.df$hour <- hour(o3.df$day_time)

o3.df <- o3.df[, c("site.num", "latitude", "longitude", "parameter.name",
                   "day_time", "date", "hour", "sample.measurement")]

write.csv(o3.df, "data/aqs/aqs_o3.csv", row.names = FALSE)

#### preprocessing mobile data

### ebus
ebus_files <- paste0("data/mobile/ebus/", list.files("data/mobile/ebus/"))

ebus_table <- lapply(ebus_files, read.csv, header = TRUE)

ebus.df <- do.call(rbind , ebus_table) |>
  select(c("times", "LAT", "LON", "ELV", "PM2.5", "PMF"))

### trax
trax_files <- paste0("data/mobile/trax/", list.files("data/mobile/trax"))

trax_table <- lapply(trax_files, read.csv, header = TRUE)

# find column number
for(i in 1:68) {
  print(
    do.call(length, trax_table[i])
  )
}

trax.early <- do.call(rbind, trax_table[1:4]) |>
  select(c("times", "LAT", "LON", "ELV", "PM2.5", "PMF"))
trax.mid <- do.call(rbind, trax_table[5]) |> # this guy doesn't have O3
  select(c("times", "LAT", "LON", "ELV", "PM2.5", "PMF"))
trax_table <- do.call(rbind, trax_table[6:68]) |>
  select(c("times", "LAT", "LON", "ELV", "PM2.5", "PMF"))

trax.df <- rbind(trax.early, trax.mid, trax_table)


### combine
ebus.df$ebus_trax <- "ebus"
trax.df$ebus_trax <- "trax"

mobile <- rbind(ebus.df, trax.df)

write.csv(mobile, "data/mobile/mobile_pm.csv", row.names = FALSE)

library(stringr)

mobile$year <- str_sub(mobile$times, start = 0, end = 4)

prop.table(table(is.na(mobile$O3), mobile$year))

aggregate(is.na(mobile$O3), by = list(mobile$year), FUN = sum)$x / 
  t(table(mobile$year))

ggplot(mobile, aes(x = times, y = O3)) +
  geom_dotplot()


mobile <- read.csv("data/mobile/mobile_o3.csv")

mobile_clean <- mobile |>
  filter(!is.na(O3)) |>
  filter(O3F == 0)

write.csv(mobile_clean, "data/mobile/mobile_o3_clean2.csv", row.names = FALSE)
head(mobile_clean)
