
#### preprocessing stationary data

library(dplyr)

o3_files <- paste0("data/aqs/pm/", list.files("data/aqs/pm/"))

for(i in 1:length(unique(o3_files))) {
  
  o3 <- read.csv(o3_files[i]) |>
    filter(State.Name == "Utah" & 
             County.Name == "Salt Lake")
  table(o3$State.Code)
  write.csv(o3, o3_files[i], 
            row.names = FALSE)
  
}

o3_files <- paste0("data/aqs/pm/", list.files("data/aqs/pm/"))

o3_table <- lapply(o3_files, read.csv, header = TRUE)

head(o3_table[1])

o3.df <- do.call(rbind , o3_table) |>
  select(c("Time.Local", "Date.Local", "Date.GMT", "Time.GMT", 
           "Latitude", "Longitude", "Sample.Measurement", "Units.of.Measure"))

write.csv(o3.df, "data/aqs/aqs_pm.csv", row.names = FALSE)

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




