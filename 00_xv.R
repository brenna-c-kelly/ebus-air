
library(sf)
library(tdr)
library(tmap)
library(dplyr)
library(purrr)
library(ggplot2)
library(lubridate)
library(data.table)
library(tidycensus)


### cross-validation

# read / write
xv_files <- paste0("../output/o3_rf_xv/", list.files("../output/o3_rf_xv")) # includes all sites
xv_tables <- lapply(xv_files, read.csv, header = TRUE)
full_cv <- do.call(rbind, xv_tables)

# ppm to ppb
full_cv <- full_cv |>
  mutate(yhat = yhat * 1000,
         sample.measurement = sample.measurement * 1000,
         adj = adj * 1000)

# write.csv(full_cv, "output/full_cv.csv", row.names = FALSE)
full_cv <- fread("output/full_cv.csv")

# get summary stats
cv_list = list()

sites = unique(full_cv$site.num)

for(i in 1:length(unique(full_cv$site.num))) {
  
  cv_site <- full_cv |>
    filter(site.num == sites[i])
  
  print(i)
  
  cv_site <- data.frame(site = sites[i],
                        rmse = sqrt(mean((cv_site$sample.measurement - cv_site$yhat)^2)),
                        sd = sd(cv_site$sample.measurement),
                        bias = tdStats(cv_site$sample.measurement, cv_site$yhat, functions = "mbe"),
                        cor = cor.test(cv_site$sample.measurement, cv_site$yhat)$estimate[[1]],
                        r_sq = (cor.test(cv_site$sample.measurement, cv_site$yhat)$estimate[[1]])^2,
                        lat = unique(full_cv[which(full_cv$site == sites[i]), ] |>
                                       select(latitude, longitude))[[1]],
                        lon = unique(full_cv[which(full_cv$site == sites[i]), ] |>
                                       select(latitude, longitude))[[2]])
  # print(cv_site)
  cv_list[[i]] <- cv_site
  
  # print(paste0("RMSE: ", round(sqrt(mean((cv_site$sample.measurement - cv_site$yhat)^2)), 3)))
  # print(paste0("SD: ", round(sd(cv_site$sample.measurement), 3)))
  # print(paste0("Bias: ", round(tdStats(cv_site$sample.measurement, cv_site$yhat, functions = "mbe"), 3)))
  # print(paste0("R-squared: ", round(cor.test(cv_site$sample.measurement, cv_site$yhat)$estimate[[1]], 3)))
  
}

cv_df <- map_df(cv_list, ~as.data.frame(.))
row.names(cv_df) <- NULL

# adding overall results
cv_df_overall <- rbind(cv_df,
                       c(site = "overall",
                         rmse = sqrt(mean((full_cv$sample.measurement - full_cv$yhat)^2)),
                         sd = sd(full_cv$sample.measurement),
                         bias = tdStats(full_cv$sample.measurement, full_cv$yhat, functions = "mbe"),
                         cor = cor.test(full_cv$sample.measurement, full_cv$yhat)$estimate[[1]],
                         r_sq = (cor.test(full_cv$sample.measurement, full_cv$yhat)$estimate[[1]])^2,
                         lat = NA,
                         lon = NA))

# save table
cv_df_overall |>
  select(!c("lon", "lat")) |>
  write.csv("methods paper/tables, figures/xv_summary.csv", row.names = FALSE)

# maps for rmse, bias, corr
cv_sf <- st_as_sf(cv_df, coords = c("lon", "lat"), crs = 4326)

# get rid of areas outside SLC, for basemap
ut_counties <- get_acs(geography = "county",
                       variables = c('B01001_001'),
                       state = "UT", geometry = TRUE, year = 2021) |>
  filter(NAME != "Salt Lake County, Utah")
slc <- get_acs(geography = "county",
                       variables = c('B01001_001'),
                       state = "UT", geometry = TRUE, year = 2021) |>
  filter(NAME == "Salt Lake County, Utah")

tmap_mode("view")
# save png of leaflet map
tm_shape(cv_sf) + # set extent to SLC
  tm_shape(ut_counties) + # remove areas outside SLC
  tm_polygons(fill = "white", col = "white")

tmap_mode("plot")
### rmse
tm_shape(slc) +
  tm_polygons(fill_alpha = 0)+
  tm_shape(cv_sf) +
  tm_dots(size = 1, shape = 18, fill = "rmse",
          fill.scale = tm_scale_continuous(values = "viridis"),
          fill.legend = tm_legend(reverse = TRUE)) +
  tm_text(text = "site", size = 0.8, col = "black", xmod = 1, ymod = 0.1,
          options = opt_tm_text(point.label.gap = 0,
                                just = "left")) +
  tm_layout(frame = FALSE, legend.frame = FALSE)

### mean bias error
tm_shape(slc) +
  tm_polygons(fill_alpha = 0)+
  tm_shape(cv_sf) +
  tm_dots(fill = "bias", col = "black", size = 1, shape = 23, 
          fill.scale = tm_scale_continuous(values = c("royalblue1", 
                                                      "white", 
                                                      "orangered"),
                                           limits = c(-11.8, 11.8),
                                           midpoint = 0),
          fill.legend = tm_legend(reverse = TRUE)) +
  tm_text(text = "site", size = 0.8, col = "black", xmod = 1, ymod = 0.1,
          options = opt_tm_text(point.label.gap = 0,
                                just = "left")) +
  tm_layout(frame = FALSE, legend.frame = FALSE)

### correlation
tm_shape(slc) +
  tm_polygons(fill_alpha = 0)+
  tm_shape(cv_sf) +
  tm_dots(fill = "cor", col = "black", size = 1, shape = 23, 
          fill.scale = tm_scale_continuous(values = "viridis"),
                                           # limits = c(-1, 1)),
          fill.legend = tm_legend(reverse = TRUE)) +
  tm_text(text = "site", size = 0.8, col = "black", xmod = 1, ymod = 0.1,
          options = opt_tm_text(point.label.gap = 0,
                                just = "left")) +
  tm_layout(frame = FALSE, legend.frame = FALSE)

tm_shape(slc) +
  tm_polygons(fill_alpha = 0)+
  tm_shape(cv_sf) +
  tm_dots(fill = "r_sq", col = "black", size = 1, shape = 23, 
          fill.scale = tm_scale_continuous(values = "viridis",
                                           limits = c(0, 1)),
          fill.legend = tm_legend(reverse = TRUE)) +
  tm_text(text = "site", size = 0.8, col = "black", xmod = 1, ymod = 0.1,
          options = opt_tm_text(point.label.gap = 0,
                                just = "left")) +
  tm_layout(frame = FALSE, legend.frame = FALSE)



### time series for temporal coverage
# most-to-least coverage
cov_sort <- full_cv |>
  group_by(site.num) |>
  tally() |>
  arrange(n)

cov_sort$site.num

# plot of coverage
full_cv |>
  mutate(coverage = 1) |>
  filter(year(day_time) != 2018) |>
  mutate(site.num = factor(site.num, levels = cov_sort$site.num)) |>
  ggplot(aes(x = day_time, y = coverage, group = site.num)) +
  geom_bar(stat = "identity", colour = "black") +
  ylim(c(0, 1)) +
  scale_x_datetime(date_breaks = "1 year",
                   date_labels = "%Y") +
  facet_wrap(~ site.num, scales = "fixed", ncol = 1) +
  theme_classic() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  # xlab("Study Period") +
  ggtitle("Stationary Monitor Data Temporal Coverage")

#### mobile coverage

# source: https://horel.chpc.utah.edu/data/meop/data/
# dropped rail 1; that was for a short-term study
mobile_coverage <- data.frame(
  monitor = c("Bus 01", "Bus 02", "Bus 03", "Bus 04", 
              "Bus 05", "Bus 06", "Bus 07", "Bus 08", 
              "Bus 09", "Bus 10", "Bus 11",
              "Bus 12", "Bus 13", "Bus 14",
              "Trax 1", "Trax 2", "Trax 3",
              "Bus 11"), 
  step = c("1", "1", "1", "1",
           "1", "1", "1", "1", 
           "1", "1", "1",
           "1", "1", "1", 
           "1", "1", "1",
           "2"), 
  start = my(c("09-2021", "02-2022", "02-2022", "05-2023", 
            "07-2023", "09-2023", "09-2023", "01-2024", 
            "01-2024", "03-2024", "03-2024", 
            "03-2024", "04-2024", "05-2024", 
            "01-2019", "01-2019", "01-2019", # 11-2018 changed to 1-19, for study period
            "12-2024")),
  stop = my(c("09-2024", "09-2024 ", "12-2024", "12-2024", 
           "12-2024", "12-2024", "12-2024", "12-2024", 
           "12-2024", "12-2024", "09-2024", 
           "12-2024", "12-2024", "12-2024", 
           "12-2024", "12-2024", "12-2024",
           "12-2024")))

# for gantt only
mobile_coverage$stop <- mobile_coverage$stop + months(1)

# sorting by start, for plot
mobile_coverage |>
  arrange(start) |>
  mutate(id = as.numeric(factor(monitor, levels = unique(monitor))))

mobile_coverage <- mobile_coverage |>
  arrange(start) |>
  mutate(id = as.factor(
    as.numeric(factor(monitor, levels = unique(monitor)))))

# gantt
mobile_coverage |>
  ggplot(aes(x = start, xend = stop, 
             y = id, yend = id)) +
  geom_segment(size = 12, color = "black") + 
  scale_y_discrete(labels = unique(mobile_coverage$monitor))+
  labs(title = "Mobile monitor temporal coverage (monthly)",
       x = "Study Period",
       y = "Monitor Name") +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme_minimal()




