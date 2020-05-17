if(!require(viridis)) {
  install.packages("viridis", repos="http://cloud.r-project.org")
  library(viridis)
}

library(sf)
library(ggplot2)
# library(broom)
library(dplyr)
library(ggsn)

# theme_set(theme_bw())

# https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

nowt <- function(x = NULL) x

# catchments_02 <- st_read("1.1-dev/data_stream_spatial/spatial_02/Catchments02.shp", stringsAsFactors = FALSE)

# flowlines
flow_01 <- st_read("1.1-dev/data_stream_spatial/spatial_01/truncatedFlowlines01.shp", stringsAsFactors = FALSE)
flow_02 <- st_read("1.1-dev/data_stream_spatial/spatial_02/truncatedFlowlines02.shp", stringsAsFactors = FALSE)
flow_03 <- st_read("1.1-dev/data_stream_spatial/spatial_03/truncatedFlowlines03.shp", stringsAsFactors = FALSE)
flow_04 <- st_read("1.1-dev/data_stream_spatial/spatial_04/truncatedFlowlines04.shp", stringsAsFactors = FALSE)
flow_05 <- st_read("1.1-dev/data_stream_spatial/spatial_05/truncatedFlowlines05.shp", stringsAsFactors = FALSE)
flow_06 <- st_read("1.1-dev/data_stream_spatial/spatial_06/truncatedFlowlines06.shp", stringsAsFactors = FALSE)

# derived metrics
df_derived <- readRDS("1.1-dev/model-predict-derived.rds")

# need to get projection and crs right
# st_crs(flow_02) <- 4326
# st_crs(flow_02) <- 4269
flow_01 <- st_transform(flow_01, "+proj=longlat +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
flow_02 <- st_transform(flow_02, "+proj=longlat +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
flow_03 <- st_transform(flow_03, "+proj=longlat +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
flow_04 <- st_transform(flow_04, "+proj=longlat +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
flow_05 <- st_transform(flow_05, "+proj=longlat +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
flow_06 <- st_transform(flow_06, "+proj=longlat +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

# st_crs(flow_02)
# catchments_02 <- st_transform(catchments_02, "+proj=longlat +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
# st_crs(flow_02)

# flow_02 <- st_transform(flow_02, crs = 4269)

# str(flow_02)
# flow_02$geometry
# summary(flow_02)

# combine flowlines from HUC2s
# flow <- st_union(flow_01, flow_02) # too slow, not worth it unless there is a problem with the scale when mapping t

# left join predictions
flow_01 <- flow_01 %>%
  dplyr::rename(featureid = FEATUREID) %>%
  left_join(df_derived) %>%
  nowt()
flow_02 <- flow_02 %>%
  rename(featureid = FEATUREID) %>%
  left_join(df_derived) %>%
  nowt()
flow_03 <- flow_03 %>%
  rename(featureid = FEATUREID) %>%
  left_join(df_derived) %>%
  nowt()
flow_04 <- flow_04 %>%
  rename(featureid = FEATUREID) %>%
  left_join(df_derived) %>%
  nowt()
flow_05 <- flow_05 %>%
  rename(featureid = FEATUREID) %>%
  left_join(df_derived) %>%
  nowt()
flow_06 <- flow_06 %>%
  rename(featureid = FEATUREID) %>%
  left_join(df_derived) %>%
  nowt()

# 
# catchments_02 <- catchments_02 %>%
#   rename(featureid = FEATUREID) %>%
#   left_join(df_derived) %>%
#   nowt()

# summary(flow_02)
# left join hucs
# filter to one huc for example
# dplyr::filter(flow_02, grepl(FEATUREID, ))
# combine all shapefiles or fasterize then combine
# fasterize to create smooth image
              
# map
# map_02 <- ggplot() + 
#   geom_sf(data = catchments_02, aes(fill = max_temp_30d), color = NA) + 
  # geom_polygon(catchments_02, aes(lon, lat, group = FEATUREID)) + 
    # nowt()
# map_02 # hangs

# map_catchments_02 <- ggplot(data = catchments_02) + 
#   geom_sf(aes(fill = max_temp_30d), color = NA) +
#   coord_sf(xlim = c(-79, -77), ylim = c(40, 41), expand = FALSE) + 
#   # geom_polygon(catchments_02, aes(lon, lat, group = FEATUREID)) + 
#   nowt()
# map_catchments_02 


# map flowlines (very slow - but catchments even more problematic)
map_flow <- ggplot(data = flow_01) + 
  geom_sf(aes(color = max_temp_30d)) +
  geom_sf(data = flow_02, aes(color = max_temp_30d)) + # will color ramp be the same?
  geom_sf(data = flow_03, aes(color = max_temp_30d)) +
  geom_sf(data = flow_04, aes(color = max_temp_30d)) +
  geom_sf(data = flow_05, aes(color = max_temp_30d)) +
  geom_sf(data = flow_06, aes(color = max_temp_30d)) +
  # theme_bw() +
  # north(flow_06, location = "topleft", symbol = 10) +
  # scalebar(flow_05, dist = 4, dist_unit = "km", transform = TRUE, model = "GRS80") +
  xlab("Longitude") +
  ylab("Latitude") + 
  labs(color = "30-day max") +
  # coord_equal() +
  # coord_fixed(1.3) +
  theme_bw() +
  nowt() 
# map_flow # add scalebar and compass rose?
ggsave(plot = map_flow, filename = "1.1-dev/max_temp_30d_blue.png", dpi = 72, height = 4, width = 5, units = "in")


# mean summer temp b/c easy to understand and present uncertainty
map_flow <- ggplot(data = flow_01) + 
  geom_sf(aes(color = mean_summer_temp)) +
  geom_sf(data = flow_02, aes(color = mean_summer_temp)) + # will color ramp be the same?
  geom_sf(data = flow_03, aes(color = mean_summer_temp)) +
  geom_sf(data = flow_04, aes(color = mean_summer_temp)) +
  geom_sf(data = flow_05, aes(color = mean_summer_temp)) +
  geom_sf(data = flow_06, aes(color = mean_summer_temp)) +
  nowt() 

map_flow <- map_flow + 
  # theme_bw() +
  # north(flow_06, location = "topleft", symbol = 10) +
  # scalebar(flow_05, dist = 4, dist_unit = "km", transform = TRUE, model = "GRS80") +
  xlab("Longitude") +
  ylab("Latitude") + 
  labs(color = "Temp. (C)") +
  nowt() 

# map_flow # add scalebar and compass rose?
ggsave(plot = map_flow, filename = "1.1-dev/mean_summer_temp.png", dpi=72, height=4, width=5, units="in")

map_flow_summer_bw <- map_flow +
  scale_color_viridis(name = "Temp. (C)") + 
  theme_bw() + 
  nowt()
ggsave(plot = map_flow_summer_bw, filename = "1.1-dev/mean_summer_temp_bw.png", dpi = 72, height = 4, width = 5, units = "in")
ggsave(plot = map_flow_summer_bw, filename = "1.1-dev/mean_summer_temp_bw.tiff", dpi = 1000, height = 4, width = 5, units = "in")

# smaller legend within plot area
map_flow_summer_bw <- map_flow_summer_bw +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(color = 'black'),
    legend.position = c(0.87, 0.40),
    legend.justification = c("center", "top"),
    legend.box.just = "center",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 8),
    legend.key.width = unit(0.17, "in") #,
    # legend.key.height = 0.7
  ) +
  guides(
    color = guide_colorbar(
      # barwidth = 0.5 * legend.key.width,
      barheight = 5
    )
  )+ 
  nowt()
# map_flow_summer_bw
ggsave(plot = map_flow_summer_bw, filename = "1.1-dev/mean_summer_temp_bw.png", dpi = 72, height = 5, width = 5, units = "in")
ggsave(plot = map_flow_summer_bw, filename = "1.1-dev/mean_summer_temp_bw.tiff", dpi = 1000, height = 5, width = 5, units = "in")




## Colors and map options from: https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
# map_flow_magma <- map_flow + 
#   theme_map() +
#   theme(legend.position = "bottom") + 
#   scale_color_viridis(option = "magma",
#                       name = "Mean Summer Temp.",
#                       discrete = F,
#                       # direction = -1,
#                       guide = guide_colourbar(
#                         direction = "horizontal",
#                         label.position = "bottom",
#                         barheight = unit(2, units = "mm"),
#                         barwidth = unit(50, units = "mm"),
#                         draw.ulim = FALSE,
#                         title.position = 'top',
#                         title.hjust = 0.5,
#                         label.hjust = 0.5)
#                       ) +
#   nowt()
# 
# ggsave(plot = map_flow_magma, filename = "1.1-dev/mean_summer_temp_magma.png", dpi = 72, height = 4, width = 5, units = "in")
# ggsave(plot = map_flow_magma, filename = "1.1-dev/mean_summer_temp_magma.tiff", dpi = 1000, height = 4, width = 5, units = "in")

# 
# map_flow_viridis <- map_flow + 
#   theme_map() +
#   theme(legend.position = "bottom") + 
#   scale_color_viridis(name = "Mean Summer Temp.",
#                       discrete = F,
#                       # direction = -1,
#                       guide = guide_colourbar(
#                         direction = "horizontal",
#                         label.position = "bottom",
#                         barheight = unit(2, units = "mm"),
#                         barwidth = unit(50, units = "mm"),
#                         draw.ulim = F,
#                         title.position = 'top',
#                         title.hjust = 0.5,
#                         label.hjust = 0.5)
#   )
# 
# ggsave(plot = map_flow_viridis, filename = "1.1-dev/mean_summer_temp_viridis.png", dpi = 72, height = 4, width = 5, units = "in")
# ggsave(plot = map_flow_viridis, filename = "1.1-dev/mean_summer_temp_viridis.tiff", dpi = 1000, height = 4, width = 5, units = "in")

# add scalebar
# my_scalebar(x.min = 144.5, x.max = 147.5,
#             y.min = 13.5,  y.max = 16.5,
#             dist = 50, dd2km = TRUE, model = 'WGS84',
#             box.fill = c("yellow", "white), st.color = "white")



# Variation (among years)

# Uncertainty (posterior SD of mean summer temp.)

# RMSE spatially

# Validation of the derived metrics (would have to be by year, not across years and only for those locations with sufficient data)




# variogram?

# torgegram?

