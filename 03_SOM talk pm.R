
library(sf)
library(dplyr)
library(leaflet)
library(lubridate)
library(RColorBrewer)

#out <- read.csv("/Users/brenna/Documents/School/Research/ebus_air/ebus-air/output/pred_temp_example.csv")
out <- read.csv("/Users/brenna/Documents/School/Research/ebus_air/ebus-air/output/pred_pm25_example.csv")

head(out)
hist(out$diff)



out_sf <- st_as_sf(out, coords = c("lon", "lat"),
                   crs = 4326, agr = "constant")
out$date <- ymd_hms(out$date)

table(out$date < "2023-07-10")

aq_df = aq_df[(aq_df.day_time >= "2023-07-07") & (aq_df.day_time <= "2023-07-21")]

# out_date <- out[which(out$date >= "2023-07-13" &
#                         out$date < "2023-07-14"), ]

# out_date <- out_date[which(out_date$date > "2023-07-13 12:00:00.0000" &
#                              out_date$date < "2023-07-13 19:00:00.0000"), ]
out_date <- out[which(out$date >= "2023-07-13 04:00:00" &
                        out$date < "2023-07-14"), ]

out_date <- out[which(out$date > "2023-07-13 1:30:00" &
                        out$date < "2023-07-13 16:36:00"), ]
table(out_date$date)

# summary(out_date$diff)
# summary(out_date$pred)
# summary(out_date$pred)

library(viridis)
#palette <- turbo(8)
palette <- brewer.pal(8, "BuPu")
palette <- magma(8)
out_date$z_colors_p <- cut(out_date$pred, breaks = c(0, 1, 2, 3, 5, 10, 18, 25, 45), labels = rev(palette))  # 
out_date$z_colors_o <- cut(out_date$obs, breaks = c(0, 1, 2, 3, 5, 10, 18, 25, 45), labels = rev(palette))
palette <- brewer.pal(10, "Spectral")
out_date$z_colors_diff <- cut(out_date$diff, breaks = c(-40, -10, -5, -3, -1, 0, 1, 3, 5, 10, 40), labels = palette)

out_date <- st_drop_geometry(out_date)
out_date_sf <- st_as_sf(out_date, coords = c("lon", "lat"),
                        crs = 4326, agr = "constant")

# size: 600
leaflet(out_date) |>
  setView(-111.868, 40.675, zoom = 12) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(radius = 5,
                   color = ~z_colors_p,
                   lng = ~lon, lat = ~lat, 
                   popup = ~diff,
                   stroke = FALSE, fillOpacity = 0.7)


hourly_pm <- fread("/Users/brenna/Documents/School/Research/ebus_air/ebus-air/data/pm25_data.csv")
library(data.table)
summary(hourly_pm)
hourly_pm <- hourly_pm |>
  filter(date == "2023-07-13" &
           hour > "4H 0M 0S") |>
  filter(sample.measurement > 0)

hourly_pm <- st_as_sf(hourly_pm, coords = c("longitude",
                                            "latitude"),
                      crs = 4326, agr = "constant")  |>
  st_jitter(factor = 0.015)

palette <- brewer.pal(8, "PiYG")
palette <- magma(8)
hourly_pm$z_colors <- cut(hourly_pm$sample.measurement,
                            breaks = c(0, 1, 2, 3, 5, 10, 18, 25, 130), labels = rev(palette))
table(hourly_pm$hour)
hourly_pm_onetime <- hourly_pm |>
  filter(hour == "9H 0M 0S")

leaflet(hourly_pm_onetime) |>
  setView(-111.95, 40.675, zoom = 11) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(radius = 10,
                   color = ~z_colors,
                   #lng = ~lon, lat = ~lat, 
                   popup = ~sample.measurement,
                   stroke = FALSE, fillOpacity = 0.8)

leaflet(hourly_pm) |>
  setView(-111.935, 40.67, zoom = 11) |>
  #setView(-111.95, 40.675, zoom = 11) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(radius = 5,
                   color = ~z_colors,
                   #lng = ~lon, lat = ~lat, 
                   popup = ~sample.measurement,
                   stroke = FALSE, fillOpacity = 0.8) |>
  addCircleMarkers(data = out_date,
                   radius = 5,
                   color = ~z_colors_p,
                   lng = ~lon, lat = ~lat, 
                   popup = ~diff,
                   stroke = FALSE, fillOpacity = 0.7) |>
  addLegend("bottomright", pal = pal, values = ~sample.measurement,
            title = "Ozone Concentration",
            #labFormat = labelFormat(prefix = "$"),
            opacity = 1,
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))

daily_pm <- read.csv("/Users/brenna/Downloads/ad_viz_plotval_data-11.csv")
names(daily_pm) <- tolower(names(daily_pm))
daily_pm <- daily_pm |>
  filter(date == "07/13/2023")

daily_pm_sf <- st_as_sf(daily_pm, coords = c("site_longitude",
                                             "site_latitude"),
                        crs = 4326, agr = "constant")
daily_pm_sf$z_colors <- cut(daily_pm_sf$daily.mean.pm2.5.concentration,
                            breaks = c(0, 1, 2, 3, 4, 5, 8, 12, 18, 25, 40), labels = rev(palette))

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
            title = "Adjustment",
            #labFormat = labelFormat(prefix = "$"),
            opacity = 1,
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))

ggplot(out_date, aes(x = normalized_time)) +
  geom_point(aes(y = obs, colour = "obs", alpha = 1)) +
  geom_point(aes(y = pred, colour = "pred", alpha = 1)) +
  scale_color_manual(values = c("#C5524D", "#5963A5"))  +
  xlab("Time") +
  ylab("Predicted") +
  theme_bw()



#ggplot(out_date_sf, mapping = aes(geometry = geometry, col = diff)) +
#  geom_sf(colour = out_date_sf$z_colors)


#summary(out_date)




