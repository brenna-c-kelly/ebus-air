
library(sf)
library(tmap)
library(tidyr)
library(dplyr)
library(ggpubr)
library(stringr)
library(leaflet)
library(ggplot2)
library(lubridate)
library(tidycensus)
library(RColorBrewer)


dat <- read.csv("/Users/brenna/Downloads/adj_o3_2023.csv")

## cleaning up
dat$day_time <- ymd_hms(dat$day_time)
dat$date_time <- as_datetime(dat$day_time)
dat$date <- as_date(dat$day_time)

dat$hour_of_day <- hour(dat$day_time)

ggplot(dat, aes(x = day_time, y = adj)) +
  geom_line(col = "seagreen", alpha = 0.7) +
  scale_x_datetime(date_labels = "%m-%d-%Y") +
  theme_bw()

# make sure the time zone is right
# dat$day_time <- with_tz(dat$day_time, "America/Denver") # do not
## minute of day
dat$hour_minute <- paste0(str_pad(hour(dat$day_time), width = 2, pad = "0"),
                          str_pad(minute(dat$day_time), width = 2, pad = "0"))

# adjustment
adj_by_mod <- aggregate(dat$adj, by = list(hour(dat$day_time),
                                           minute(dat$day_time)), FUN = mean)
ggplot(adj_by_mod, aes(x = Group.1, y = x)) +
  geom_point(alpha = 0.3) +
  labs(x = "Time of Day", y = "Average Adjustment") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw()

# value
val_by_mod <- aggregate(dat$val, by = list(hour(dat$day_time),
                                           minute(dat$day_time)), FUN = mean)
ggplot(val_by_mod, aes(x = Group.1, y = x)) +
  geom_point(alpha = 0.3) +
  labs(x = "Time of Day", y = "Average Value") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw()

# prediction
pred_by_mod <- aggregate(dat$yhat, by = list(hour(dat$day_time),
                                             minute(dat$day_time)), FUN = mean)
ggplot(pred_by_mod, aes(x = Group.1, y = x)) +
  geom_point(alpha = 0.3) +
  labs(x = "Time of Day", y = "Average Prediction") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw()

# all together
adj_by_mod$measure <- "adjustment"
val_by_mod$measure <- "value"
pred_by_mod$measure <- "prediction"

adj_by_mod$unit <- "adjustment"
val_by_mod$unit <- "ppb"
pred_by_mod$unit <- "ppb"

val_pred_adj <- rbind(adj_by_mod, val_by_mod, pred_by_mod)

ggplot(val_pred_adj, aes(x = Group.1, y = x, colour = measure)) +
  geom_point(alpha = 0.3) +
  labs(x = "Time of Day", y = "ppb / adj") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ unit, ncol = 1, scales = "free") +
  theme_classic()



## hour of day
adj_by_hod <- aggregate(dat$adj, by = list(dat$hour_of_day), FUN = mean)

ggplot(adj_by_tod, aes(x = Group.1, y = x)) +
  geom_line() +
  geom_point() +
  labs(x = "Time of Day", y = "Average Adjustment") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw()

# hour, not aggregated
ggplot(dat, aes(x = hour_of_day, y = adj)) +
  geom_point(alpha = 0.1) +
  labs(x = "Time of Day", y = "Average Adjustment") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw()


## make the grid cells
slc_cnty <- get_acs(geography = "county",
                    variables = c('B01001_001'),
                    state = "UT",
                    geometry = TRUE,
                    year = 2021) |>
  filter(NAME == "Salt Lake County, Utah")

slc_cnty <- st_transform(slc_cnty, st_crs(out_sf))

fishnet = slc_cnty %>%
  st_make_grid(cellsize = 0.005, what = "polygons") %>% # make cell size 0.005 degrees, or ~500 m
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(fishnet_id = 1:n()) %>%
  dplyr::select(fishnet_id, geometry)

## assign points to grid cells
dat_sf <- st_as_sf(dat, coords = c("longitude", "latitude"), crs = 4326)

fishnet <- st_transform(fishnet, crs = st_crs(dat_sf))

grid_sf <- st_join(dat_sf, fishnet, join = st_intersects)


# aggregating data to the grid level
grid_no_geom <- grid_sf |>
  st_drop_geometry() |>
  merge(fishnet, by = "fishnet_id") |>
  group_by(fishnet_id) |>
  summarize(obs_mean = mean(val, na.rm = TRUE),
            pred_mean = mean(yhat, na.rm = TRUE),
            adj_mean = mean(adj, na.rm = TRUE),
            obs_median = median(val, na.rm = TRUE),
            pred_median = median(yhat, na.rm = TRUE),
            adj_median = median(adj, na.rm = TRUE),
            obs_high = quantile(val, 0.95, na.rm = TRUE),
            pred_high = quantile(yhat, 0.95, na.rm = TRUE),
            adj_high = quantile(adj, 0.95, na.rm = TRUE),
            count = n()) %>%
  ungroup()

grid_dat <- merge(fishnet, grid_no_geom, by = "fishnet_id")


## period adjustments
pal <- colorNumeric(
  palette = "PiYG",
  domain = c(-0.037, 0.037), reverse = TRUE)

leaflet() |>
  setView(-111.86, 40.675, zoom = 10) |>
  addProviderTiles("Stadia.AlidadeSmoothDark") |> #providers$CartoDB.Positron
  addPolygons(data = grid_dat, color = ~pal(adj_high), fillOpacity = 0.75, stroke = FALSE) |>
  addLegend('bottomright', pal = pal, values = grid_dat$adj_high,
            title = "Peak adjustment")

leaflet() |>
  setView(-111.86, 40.675, zoom = 10) |>
  addProviderTiles("Stadia.AlidadeSmoothDark") |>
  addPolygons(data = grid_dat, color = ~pal(adj_mean), fillOpacity = 0.75, stroke = FALSE) |>
  addLegend('bottomright', pal = pal, values = grid_dat$adj_mean,
            title = "Average adjustment")

leaflet() |>
  setView(-111.86, 40.675, zoom = 10) |>
  addProviderTiles("Stadia.AlidadeSmoothDark") |>
  addPolygons(data = grid_dat, color = ~pal(adj_median), fillOpacity = 0.75, stroke = FALSE) |>
  addLegend('bottomright', pal = pal, values = grid_dat$adj_median,
            title = "Median adjustment")

mean(dat$val)
mean(dat$yhat)
mean(dat$adj)

# aggregating data to the grid level, with hour
grid_hour_no_geom <- grid_sf |>
  st_drop_geometry() |>
  merge(fishnet, by = "fishnet_id") |>
  group_by(fishnet_id, hour_of_day) |>
  summarize(obs_mean = mean(val, na.rm = TRUE),
            pred_mean = mean(yhat, na.rm = TRUE),
            adj_mean = mean(adj, na.rm = TRUE),
            obs_median = median(val, na.rm = TRUE),
            pred_median = median(yhat, na.rm = TRUE),
            adj_median = median(adj, na.rm = TRUE),
            obs_high = quantile(val, 0.95, na.rm = TRUE),
            pred_high = quantile(yhat, 0.95, na.rm = TRUE),
            adj_high = quantile(adj, 0.95, na.rm = TRUE),
            count = n()) %>%
  ungroup()

grid_hour_dat <- merge(fishnet, grid_hour_no_geom, by = "fishnet_id")

hour_no_geom <- grid_hour_dat |>
  filter(hour_of_day == 19)

summary(hour_no_geom$adj_high)
summary(hour_no_geom$adj_mean)
summary(hour_no_geom$adj_median)

## period by hour adjustments
pal <- colorNumeric(
  palette = "Spectral",
  domain = c(-0.03, 0.03), reverse = TRUE)

leaflet() |>
  setView(-111.86, 40.675, zoom = 10) |>
  addProviderTiles(providers$CartoDB.Positron) |> #providers$CartoDB.Positron
  addPolygons(data = hour_no_geom, color = ~pal(adj_high), fillOpacity = 0.75, stroke = FALSE) |>
  addLegend('bottomright', pal = pal, values = hour_no_geom$adj_high,
            title = "Peak adjustment @ 1900")

leaflet() |>
  setView(-111.86, 40.675, zoom = 10) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(data = hour_no_geom, color = ~pal(adj_mean), fillOpacity = 0.75, stroke = FALSE) |>
  addLegend('bottomright', pal = pal, values = hour_no_geom$adj_mean,
            title = "Average adjustment @ 1900")

leaflet() |>
  setView(-111.86, 40.675, zoom = 10) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(data = hour_no_geom, color = ~pal(adj_median), fillOpacity = 0.75, stroke = FALSE) |>
  addLegend('bottomright', pal = pal, values = hour_no_geom$adj_median,
            title = "Median adjustment @ 1900")



## descriptives
agg_time <- cbind(
  aggregate(grid_sf$val, by = list(grid_sf$time_of_day), 
            FUN = mean) |>
    dplyr::rename(time = Group.1,
                  mean_O3 = x),
  aggregate(grid_sf$val, by = list(grid_sf$time_of_day), 
            FUN = quantile, probs = c(0.5)) |>
    select(x) |>
    dplyr::rename(median_O3 = x),
  aggregate(grid_sf$val, by = list(grid_sf$time_of_day), 
            FUN = quantile, probs = c(0.95)) |>
    select(x) |>
    dplyr::rename(high_O3 = x),
  aggregate(grid_sf$yhat, by = list(grid_sf$time_of_day), 
            FUN = mean) |>
    select(x) |>
    dplyr::rename(mean_O3_pred = x),
  aggregate(grid_sf$yhat, by = list(grid_sf$time_of_day), 
            FUN = quantile, probs = c(0.5)) |>
    select(x) |>
    dplyr::rename(median_O3_pred = x),
  aggregate(grid_sf$yhat, by = list(grid_sf$time_of_day), 
            FUN = quantile, probs = c(0.95)) |>
    select(x) |>
    dplyr::rename(high_O3_pred = x))

agg_time_long <- agg_time |>
  pivot_longer(cols = c("mean_O3", "median_O3", "high_O3",
                        "mean_O3_pred", "median_O3_pred", "high_O3_pred"),
               names_to = "variable")

ggplot(agg_time_long, aes(x = time, y = value, group = variable)) +
  geom_line(aes(colour = variable)) +
  scale_color_manual(values = c("#E7298A", "#e3909c",
                                "#1B9E77", "#a0be95", 
                                "#D95F02", "#ecb56a"))



## maps



obs_mean <- tm_shape(grid_dat) +
  tm_polygons(col = "obs_mean", lwd = 0, 
              style = "cont", palette = "viridis",
              title = "Average mobile value")
pred_mean <- tm_shape(grid_dat) +
  tm_polygons(col = "pred_mean", lwd = 0, 
              style = "cont", palette = "viridis",
              title = "Average predicted value")
avg_adj <- tm_shape(grid_dat) +
  tm_polygons(col = "adj_mean", lwd = 0, 
              style = "cont", palette = "plasma",
              title = "Average adjustment")




obs_per_grid <- grid_sf |>
  group_by(time_of_day) |>
  summarize(count = n())

tmap_mode("view")
tm_shape(obs_per_grid) +
  tm_dots(col = "count", style = "cont", palette = "viridis")

## period adjustments
pal <- colorNumeric(
  palette = "Spectral",
  domain = c(-0.025, 0.025), reverse = FALSE)

leaflet() |>
  setView(-111.86, 40.675, zoom = 10) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(data = grid_dat, color = ~pal(adj_high), fillOpacity = 0.75, stroke = FALSE) |>
  addLegend('bottomright', pal = pal, values = grid_dat$adj_high,
            title = "Peak adjustment")

leaflet() |>
  setView(-111.86, 40.675, zoom = 10) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(data = grid_dat, color = ~pal(adj_median), fillOpacity = 0.75, stroke = FALSE) |>
  addLegend('bottomright', pal = pal, values = grid_dat$adj_median,
            title = "Median adjustment")

leaflet() |>
  setView(-111.86, 40.675, zoom = 10) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(data = grid_dat, color = ~pal(adj_mean), fillOpacity = 0.75, stroke = FALSE) |>
  addLegend('bottomright', pal = pal, values = grid_dat$adj_mean,
            title = "Mean adjustment")

