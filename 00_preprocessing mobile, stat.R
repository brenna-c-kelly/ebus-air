
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
  dplyr::select(c("Site.Num", "Latitude", "Longitude", "Parameter.Name", 
           "Date.Local", "Time.Local", # use local time, not GMT (Greenwich Mean Time)
           "Sample.Measurement", "Units.of.Measure"))

names(o3.df) <- tolower(names(o3.df))

# note: when DST comes into effect, won't parse
# force_tz(
#   ymd_hm("2019-03-10 02:00"),
#   tzone = "America/Denver")

o3.df$day_time <- ymd_hm(paste(o3.df$date.local, o3.df$time.local), tz = "America/Denver")
# o3.df$day_time <- paste0(gsub(" ", "T", o3.df$day_time), "Z")
o3.df$date <- ymd(o3.df$date.local)
o3.df$hour <- hour(o3.df$day_time)

o3.df <- o3.df[, c("site.num", "latitude", "longitude", "parameter.name",
                   "day_time", "date", "hour", "sample.measurement")]

##### make sure the hour is adjusted for by 30m
# o3.df$day_time <- 
o3.df$day_time <- o3.df$day_time - minutes(30)

o3.df <- o3.df |>
  filter(day_time >= "2018-12-14")

write.csv(o3.df, "data/aqs/aqs_o3_v5.csv", row.names = FALSE)
# v1: 2019-2024 (ragged)
# v2: 2018-2023 (need last two weeks of 2018)
# v3: 2019-2023 (last two weeks of 2019; mobile doesn't have 2018)
# v4: 2018-2024 (only last two weeks of 2018)
# v_2024: 2024 + last two weeks of 2023
# v5: 2018-2024, including last quarter, adjusted time by 30m

test <- o3.df |>
  mutate(day_time = as.character(format(day_time)))
head(test)
#### preprocessing mobile data

### ebus
ebus_files <- paste0("data/mobile/ebus/", list.files("data/mobile/ebus/"))

ebus_table <- lapply(ebus_files, read.csv, header = TRUE)

ebus.df <- do.call(rbind, ebus_table) |>
  select(c("times", "LAT", "LON", "ELV", "O3", "O3F")) # substitute with O3 O3F

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
  select(c("times", "LAT", "LON", "ELV", "O3", "O3F"))
trax.mid <- do.call(rbind, trax_table[5]) |> # this guy doesn't have O3
  select(c("times", "LAT", "LON", "ELV", "O3", "O3F"))
trax_table <- do.call(rbind, trax_table[6:68]) |>
  select(c("times", "LAT", "LON", "ELV", "O3", "O3F"))

trax.df <- rbind(trax.early, 
                 # trax.mid, # drop when getting gO3
                 trax_table)


### combine
ebus.df$ebus_trax <- "ebus"
trax.df$ebus_trax <- "trax"

mobile <- rbind(ebus.df, trax.df)
summary(ymd_hms(mobile$times))
head(mobile)

# write.csv(mobile, "data/mobile/mobile_pm.csv", row.names = FALSE)

library(stringr)

mobile$year <- str_sub(mobile$times, start = 0, end = 4)

prop.table(table(is.na(mobile$O3), mobile$year))

aggregate(is.na(mobile$O3), by = list(mobile$year), FUN = sum)$x / 
  t(table(mobile$year))

# don't. why
# ggplot(mobile, aes(x = times, y = O3)) +
#   geom_dotplot()


# mobile <- read.csv("data/mobile/mobile_o3.csv")

mobile_clean <- mobile |>
  filter(!is.na(O3)) |>
  filter(O3F == 0)

summary(ymd_hms(mobile_clean$times))
# fix time
mobile_clean$times <- ymd_hms(mobile_clean$times) - 7*60*60

# fixing time 5/21/25
mobile_clean$times <- as.character(format(mobile_clean$times))

# check that time is correct
plot(aggregate(mobile_clean$O3, by = list(hour(mobile_clean$times)), FUN = mean))

# write.csv(mobile_clean, "data/mobile/mobile_o3_v3.csv", row.names = FALSE)
write.csv(mobile_clean, "data/mobile/mobile_o3_v4.csv", row.names = FALSE)
# v3: with only last two weeks of 2019; no 2024 (apr 12 2025)
# v4: 2018-2024, complete


