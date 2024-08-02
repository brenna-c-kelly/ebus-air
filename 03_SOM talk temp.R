
library(dplyr)
library(leaflet)
library(RColorBrewer)

#out <- read.csv("/Users/brenna/Documents/School/Research/ebus_air/ebus-air/output/pred_temp_example.csv")
out <- read.csv("/Users/brenna/Documents/School/Research/ebus_air/ebus-air/output/pred_temp_example.csv")

head(out)
hist(out$diff)

out_sf <- st_as_sf(out, coords = c("lon", "lat"),
                   crs = 4326, agr = "constant")
out$date <- ymd_hms(out$date)

out_date <- out[which(out$date >= "2023-07-13 04:00:00" &
                        out$date < "2023-07-14"), ]
#out_date <- out_date[which(!is.na(out_date$obs)), ]

summary(out_date$obs)
summary(out_date$pred)
summary(out_date$diff)

library(viridis)
#palette <- turbo(8)
palette <- brewer.pal(8, "BuPu")
palette <- magma(9)
out_date$z_colors_p <- cut(out_date$pred, breaks = c(19, 21, 23, 25, 27, 29, 31, 33, 35, 37), labels = rev(palette))  # 
out_date$z_colors_o <- cut(out_date$obs, breaks = c(19, 21, 23, 25, 27, 29, 31, 33, 35, 37), labels = rev(palette))
palette <- brewer.pal(10, "Spectral")
out_date$z_colors_diff <- cut(out_date$diff, breaks = c(-6, -5, -4, -3, -1.5, 0, 1.5, 3, 4, 5, 6), labels = palette)

out_date <- st_drop_geometry(out_date)
out_date_sf <- st_as_sf(out_date, coords = c("lon", "lat"),
                        crs = 4326, agr = "constant")

# size: 600
leaflet(out_date) |>
  setView(-111.868, 40.75, zoom = 11) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(radius = 5,
                   color = ~z_colors_diff,
                   lng = ~lon, lat = ~lat, 
                   popup = ~diff,
                   stroke = FALSE, fillOpacity = 0.7) |>
  addLegend(data = out_date, "bottomleft", pal = pal, values = ~diff,
            title = "Adjustment",
            #labFormat = labelFormat(prefix = "$"),
            opacity = 1,
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))


# hourly_pm <- read.csv("/Users/brenna/Documents/School/Research/ebus_air/ebus-air/data/pm25_data.csv")
hourly_pm <- read.csv("/Users/brenna/Documents/School/Research/ebus_air/ebus-air/data/temp_data.csv")

hourly_pm <- hourly_pm |>
  filter(date == "2023-07-13" &
           hour > "4H 0M 0S") |>
  filter(sample.measurement > 0)

hourly_pm <- st_as_sf(hourly_pm, coords = c("longitude",
                                            "latitude"),
                      crs = 4326, agr = "constant")  |>
  st_jitter(factor = 0.015)

# palette <- brewer.pal(8, "PiYG")
palette <- magma(7)
hourly_pm$z_colors <- cut(hourly_pm$sample.measurement,
                          breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07), labels = rev(palette))

pal <- colorNumeric(
  palette = mag,
  domain = out_date$obs
)

pal <- colorNumeric(
  palette = rev("Spectral"),
  domain = hourly_pm$diff
)

leaflet(hourly_pm) |>
  setView(-111.95, 40.675, zoom = 11) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(radius = 5,
                   color = ~z_colors,
                   #lng = ~lon, lat = ~lat, 
                   popup = ~sample.measurement,
                   stroke = FALSE, fillOpacity = 0.8)


leaflet(hourly_pm) |>
  setView(-111.935, 40.65, zoom = 11) |>
  #setView(-111.95, 40.675, zoom = 11) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(radius = 5,
                   color = "gray",
                   #lng = ~lon, lat = ~lat, 
                   popup = ~sample.measurement,
                   stroke = FALSE, fillOpacity = 0.8) |>
  addCircleMarkers(data = out_date,
                   radius = 5,
                   color = ~z_colors_diff,
                   lng = ~lon, lat = ~lat, 
                   popup = ~z_colors_diff,
                   stroke = FALSE, fillOpacity = 0.7) |>
  addLegend(data = out_date, "bottomright", pal = pal, values = ~diff,
            title = "Temperature (F)",
            #labFormat = labelFormat(prefix = "$"),
            opacity = 1,
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))

ggplot(out, aes(x = normalized_time)) +
  geom_point(aes(y = obs, colour = "obs", alpha = 1)) +
  geom_point(aes(y = pred, colour = "pred", alpha = 1)) +
  scale_color_manual(values = c("#C5524D", "#5963A5"))  +
  xlab("Time") +
  ylab("Predicted") +
  theme_bw()


