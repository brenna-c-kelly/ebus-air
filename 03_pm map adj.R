
library(sf)
library(dplyr)
library(ggpubr)
library(leaflet)
library(ggplot2)
library(lubridate)
library(RColorBrewer)

#out <- read.csv("/Users/brenna/Documents/School/Research/ebus_air/ebus-air/output/pred_temp_example.csv")
out <- read.csv("output/pred_pm25_example.csv")
head(out)
summary(out)
out$relative_diff <- out$obs / out$pred
summary(out$relative_diff)
summary(out$obs)
summary(out$pred)

out[which(out$relative_diff < 0.1), ]

rel <- ggplot(out, aes(x = date, y = relative_diff)) +
  geom_line(col = "seagreen", alpha = 0.7) +
  theme_bw()
abs <- ggplot(out, aes(x = date, y = diff)) +
  geom_line(col = "orangered", alpha = 0.7) +
  theme_bw()
ggarrange(abs, rel, nrow = 2)
obs <- ggplot(out, aes(x = date, y = obs)) +
  geom_line(col = "lightseagreen", alpha = 0.7) +
  theme_bw()
pred <- ggplot(out, aes(x = date, y = pred)) +
  geom_line(col = "mediumvioletred", alpha = 0.7) +
  theme_bw()
ggarrange(obs, pred, abs, rel, nrow = 4)
names(out)
length(unique(out$date))
out[1:10, ]
summary(glm(relative_diff ~ obs, data = out))



out_sf <- st_as_sf(out, coords = c("lon", "lat"),
                   crs = 4326, agr = "constant")
out$date <- ymd_hms(out$date)
# summary(out_date$diff)
table(out$date < "2023-07-10")

# out_date <- out[which(out$date >= "2023-07-13" &
#                         out$date < "2023-07-14"), ]

# out_date <- out_date[which(out_date$date > "2023-07-13 12:00:00.0000" &
#                              out_date$date < "2023-07-13 19:00:00.0000"), ]
out_date <- out[which(out$date >= "2023-07-13 04:00:00" &
                        out$date < "2023-07-14"), ]
out_date <- out_date[which(!is.na(out_date$obs)), ]


# summary(out_date$obs)
# summary(out_date$pred)
hist(out_date$diff)
summary(out$diff)

library(viridis)
#palette <- turbo(8)
palette <- brewer.pal(8, "BuPu")
palette <- magma(7)
# out_date$z_colors_p <- cut(out_date$pred, breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.071), labels = palette)  # 
# out_date$z_colors_o <- cut(out_date$obs, breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.071), labels = palette)
palette <- rev(brewer.pal(6, "Spectral"))
out$z_colors_diff <- cut(out$diff, breaks = c(-133, -5, -2, 0, 2, 5, 133), labels = palette)
# out$z_colors_diff_r <- cut(out$, breaks = c(-0.05, -0.02, -0.01, 0, 0.01, 0.02, 0.053), labels = palette)

out_date <- st_drop_geometry(out)
out_date_sf <- st_as_sf(out_date, coords = c("lon", "lat"),
                        crs = 4326, agr = "constant")

library(tidycensus)
slc_cnty <- get_acs(geography = "county",
                    variables = c('B01001_001'),
                    state = "UT",
                    geometry = TRUE,
                    year = 2021) |>
  filter(NAME == "Salt Lake County, Utah")

slc_cnty <- st_transform(slc_cnty, st_crs(out_sf))

fishnet = slc_cnty %>%
  st_make_grid(cellsize = 0.01, what = "polygons") %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(fishnet_id = 1:n()) %>%
  dplyr::select(fishnet_id, geometry) 

summary(ymd_hms(out_sf$date))

out_sf$adj_pos_neg <- ifelse(out_sf$diff > 0, 1, 0)

tmp <- out_sf %>%
  st_join(fishnet) %>%
  as.data.frame() %>%
  group_by(fishnet_id) %>%
  summarize(obs = mean(obs, na.rm = TRUE),
            pred = mean(pred, na.rm = TRUE),
            adj = mean(diff, na.rm = TRUE),
            adj_pos = mean(adj_pos_neg, na.rm = TRUE)) %>%
  ungroup()

library(tidyr)
fishnet = fishnet %>%
  # Join in pvout fishnet grid averages
  left_join(by = "fishnet_id", y = tmp) %>%
  drop_na()

pal <- colorNumeric(
  palette = "Spectral",
  domain = c(-5.1, 5.1), reverse = TRUE)

leaflet() |>
  setView(-111.86, 40.675, zoom = 11) |>
  #setView(-111.868, 40.675, zoom = 11) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(data = fishnet, color = ~pal(adj), fillOpacity = 0.75, stroke = FALSE) |>
  addLegend('bottomright', pal = pal, values = fishnet$adj,
            title = "Average adjustment") #|>
  # addLabelOnlyMarkers(data = fishnet,
  #                     lng = ~lon, lat = ~lat, label = ~n,
  #                     labelOptions = labelOptions(noHide = TRUE, direction = "right",
  #                                                 textsize = "8px", textOnly = TRUE,
  #                                                 style = list(color = "white")))

















# size: 600
leaflet(out_date) |>
  setView(-111.90, 40.675, zoom = 11) |>
  #setView(-111.868, 40.675, zoom = 11) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(radius = 5,
                   color = ~z_colors_diff,
                   lng = ~lon, lat = ~lat, 
                   popup = ~diff,
                   stroke = FALSE, fillOpacity = 0.7)


# hourly_pm <- read.csv("/Users/brenna/Documents/School/Research/ebus_air/ebus-air/data/pm25_data.csv")
hourly_pm <- read.csv("/Users/brenna/Documents/School/Research/ebus_air/ebus-air/data/ozone_data.csv")

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
                          breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07), labels = palette)

hist(out_date$diff)
mag <- rev(magma(7))
pal <- colorNumeric(
  palette = rev("Spectral"),
  domain = hourly_pm$diff
)
pal
mag <- rev(magma(7))
pal <- colorNumeric(
  palette = mag,
  domain = hourly_pm$sample.measurement
)


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



summary(hourly_pm)
summary(hourly_pm$sample.measurement)

ggplot(out_date, aes(x = normalized_time)) +
  geom_point(aes(y = obs, colour = "obs", alpha = 1)) +
  geom_point(aes(y = pred, colour = "pred", alpha = 1)) +
  scale_color_manual(values = c("#C5524D", "#5963A5"))  +
  xlab("Time") +
  ylab("Predicted") +
  theme_bw()

gg_p <- ggplot(out, aes(x = normalized_time)) +
  geom_point(aes(y = pred, colour = "pred", alpha = 0.1)) +
  scale_color_manual(values = c("#5963A5")) +
  xlab("Time") +
  ylab("Predicted")

gg_o <- ggplot(out, aes(x = normalized_time)) +
  geom_point(aes(y = obs, colour = "obs", alpha = 0.1)) +
  scale_color_manual(values = c("#DA6951"))

ggarrange(gg_p, gg_o)

ggsave("foo.pdf", arrangeGrob(plot1, plot2))

