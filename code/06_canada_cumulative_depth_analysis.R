#analysis of detph in the Canadian Marine Conservation network as a cumulative frequency 

#load libraries
library(tidyverse)
library(sf)
library(terra)
library(exactextractr)

#projections
can_ea <- 3347 # Canadian Equal-Area projection (EPSG:3347)

#load the shapefiles for the Canadian MCN
can_mcn_proj <- read_sf("data/shapefiles/cpcad_complete.shp")%>%
  st_transform(can_ea)%>%
  mutate(area_km2=as.numeric(st_area(geometry) / 1e6))

#area summary 
can_mcn_proj%>%st_drop_geometry()%>%filter(type!="OECM")%>%summarise(mean=mean(area_km2),
                                                                     median=median(area_km2),
                                                                     sd=sd(area_km2),se=sd/sqrt(n()))

can_eez <- read_sf("data/shapefiles/can_eez.shp")%>%
  st_transform(can_ea)

#load depth raster
gebco_proj <- rast("c:/Users/stanleyr/Documents/GitHub/gebco_projected_3347.tif")

# 1.Pixel areas
pixel_area_m2 <- xres(gebco_proj) * yres(gebco_proj)

# 2. Extract raw pixel values and coverage fractions for every polygon (fun = NULL returns a list of data frames)
pixel_list <- exact_extract(gebco_proj, can_mcn_proj, include_cols = c("NAME_E", "ocean", "bioregion"))

# 3. pixel extract into a (very large) data.frame
pixel_df <- bind_rows(lapply(seq_along(pixel_list), function(i) {
  df <- pixel_list[[i]]
  if (nrow(df) == 0) return(NULL)
  df$ocean <- can_mcn_proj$ocean[i]
  df$bioregion <- can_mcn_proj$bioregion[i]
  df$site_name <- can_mcn_proj$NAME_E[i]
  return(df)
}))

# 4. Clean pixels: Keep only ocean (negative values) and calculate exact area in km2 per pixel piece
ocean_pixels <- pixel_df %>%
  filter(!is.na(value), value < 0) %>%
  mutate(
    depth_m = abs(value), # positive depth below surface
    area_km2 = (coverage_fraction * pixel_area_m2) / 1e6
  )


# Create a fine-grained depth sequence to evaluate cumulative area across
depth_seq <- seq(0, 5100, by = 10) # 0m to 5100m in 10m intervals ~ max depth is around 5070 but make sure this is scaled to any new sites/new depths

# Function to compute cumulative area greater than a given depth threshold for a subset of pixels
calc_cumulative_area <- function(df, depths) {
  total_ocean_area <- sum(df$area_km2, na.rm = TRUE)
  
  sapply(depths, function(d) {
    # Area where depth is greater than or equal to threshold 'd'
    sum(df$area_km2[df$depth_m >= d], na.rm = TRUE)
  })
}

# Calculate cumulative area and percentages by Ocean
cumulative_by_ocean <- ocean_pixels %>%
  group_by(ocean) %>%
  group_modify(~ {
    total_area <- sum(.x$area_km2, na.rm = TRUE)
    tibble(
      depth_m = depth_seq,
      cumulative_area_km2 = sapply(depth_seq, function(d) sum(.x$area_km2[.x$depth_m >= d], na.rm = TRUE))
    ) %>%
      mutate(cumulative_pct = (cumulative_area_km2 / total_area) * 100)
  }) %>%
  ungroup()

# What percentage of the Atlantic is deeper than 500m?
cumulative_by_ocean %>%
  filter(ocean == "Atlantic", depth_m == 500)

cumulative_by_ocean %>%
  filter(ocean == "Pacific", depth_m == 500)

cumulative_by_ocean %>%
  filter(ocean == "Arctic", depth_m == 500)


#plot it

p2 <- ggplot(cumulative_by_ocean, aes(x = cumulative_pct, y = depth_m, color = ocean)) +
  geom_hline(yintercept = c(300, 500), linetype = 2, color = "grey60", alpha = 0.7) +
  geom_line(linewidth = 1.2) +
  # Invert y-axis so surface (0m) is at the top
  scale_y_reverse(
    limits = c(3500, 0),
    labels = scales::label_comma(suffix = " m"),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_x_continuous(
    labels = scales::label_percent(scale = 1),
    limits = c(0, 100)
  ) +
  scale_color_manual(
    values = c("Pacific" = "#2b5c8f", "Arctic" = "#41b6c4", "Atlantic" = "#225ea8"),
    name = "Ocean region"
  ) +
  labs(
    x = "Cumulative percentage of area (%)",
    y = "Depth (m)"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.9,0.12),
    legend.background = element_blank(),
    legend.key = element_blank()
  )

ggsave("output/cdf_depth_plot.png",p2,height=4,width=6,units="in",dpi=300)

#National scale


# B. Calculate cumulative data for the National Total (all oceans combined)
total_national_area <- sum(ocean_pixels$area_km2, na.rm = TRUE)

cumulative_national <- tibble(
  ocean = "National",
  depth_m = depth_seq,
  cumulative_area_km2 = sapply(depth_seq, function(d) sum(ocean_pixels$area_km2[ocean_pixels$depth_m >= d], na.rm = TRUE))
) %>%
  mutate(cumulative_pct = (cumulative_area_km2 / total_national_area) * 100)

# C. Combine both together for plotting
cumulative_all <- bind_rows(cumulative_by_ocean, cumulative_national)

# Quick check: What percentage of the entire National network is deeper than 500m?
cumulative_all %>%
  filter(ocean == "National", depth_m == 500)

### grouped national and ocean based map

cumulative_all <- bind_rows(cumulative_by_ocean, cumulative_national) %>%
  mutate(
    # Set factor levels so Pacific, Arctic, Atlantic have colors and National is treated separately
    line_group = factor(ocean, levels = c("Pacific", "Arctic", "Atlantic", "National")),
    ocean =  factor(ocean, levels = c("Pacific", "Arctic", "Atlantic", "National"))
  )

# 5. Generate the Cumulative Frequency Plot
p3 <-  ggplot(cumulative_all, aes(x = cumulative_pct, y = depth_m, group = line_group)) +
  geom_hline(yintercept = c(300, 500), linetype = 2, color = "grey60", alpha = 0.7) +
  # Draw ocean lines with their respective colors
  geom_line(
    data = filter(cumulative_all),
    aes(color = ocean),
    linewidth = 1.1
  ) +
  # Draw the National line as a prominent solid or dashed black line
  geom_line(
    data = filter(cumulative_all, ocean == "National"),
    aes(color = ocean),
    linewidth = 1.5
  ) +
  # Invert y-axis so surface (0m) is at the top
  scale_y_reverse(
    limits = c(3500, 0),
    labels = scales::label_comma(suffix = " m"),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_x_continuous(
    labels = scales::label_percent(scale = 1),
    limits = c(0, 100)
  ) +
  scale_color_manual(
    values = c("Pacific" = "#2b5c8f", "Arctic" = "#41b6c4", "Atlantic" = "#225ea8","National" = "black"),
    name = ""
  ) +
  scale_linetype_manual(
    values = c("National" = "solid"), 
    labels = c("National" = "National Network"),
    name = NULL
  ) +
  labs(
    x = "Cumulative percentage of total area (%)",
    y = "Depth (m)"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.9,0.15),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.box = "horizontal",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10)
  )

ggsave("output/cdf_depth_plot_national.png",p3,height=4,width=6.5,units="in",dpi=300)
