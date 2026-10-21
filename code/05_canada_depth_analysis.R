#Code to assess coverage with depth within the Canadain MCN

#load libraries
library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(rnaturalearth)
library(exactextractr)
library(forcats)
library(tidytext)

#projections
can_ea <- 3347 # Canadian Equal-Area projection (EPSG:3347)

#load the shapefiles for the Canadian MCN
can_mcn_proj <- read_sf("data/shapefiles/cpcad_complete.shp")%>%
                st_transform(can_ea)

can_eez <- read_sf("data/shapefiles/can_eez.shp")%>%
           st_transform(can_ea)

#load the massive GEBCO for the EEZ
gebco_proj <- rast("c:/Users/stanleyr/Documents/GitHub/gebco_projected_3347.tif") #note that I already reprojected this to 3347

##################
#Area weighted depth per site
##################


# Get the area of a single full pixel in square meters from the projected raster
pixel_area <- xres(gebco_proj) * yres(gebco_proj)

# 2. Extract exact water area and depth-volume for every site
site_metrics <- exact_extract(gebco_proj, can_mcn_proj, fun = function(value, coverage_fraction) {
  # Keep only valid bathymetry (below sea level / negative values)
  valid <- !is.na(value) & value < 0
  
  if (!any(valid)) {
    return(data.frame(water_area = 0, depth_volume = 0))
  }
  
  # Calculate exact area of each pixel piece inside the polygon
  cell_areas <- coverage_fraction[valid] * pixel_area
  depths <- value[valid]
  
  data.frame(
    water_area = sum(cell_areas),                          # True water area
    depth_volume = sum(depths * cell_areas)                # Depth-weighted area sum
  )
})

#bring outputs back into the sf dataframe

can_mcn_proj <- can_mcn_proj%>%
                mutate(water_area = site_metrics$water_area,
                       depth_volume = site_metrics$depth_volume,
                       site_mean_depth = depth_volume/water_area)

#aggreate calculations at larger spatial scales 

mcn_df <- can_mcn_proj %>%
  st_drop_geometry()%>%
  filter(water_area > 0) # Remove sites with no ocean area

# --- A. BIOREGION SCALE ---
bioregion_summary <- mcn_df %>%
  group_by(ocean, bioregion) %>%
  summarise(
    total_water_area = sum(water_area, na.rm = TRUE),
    area_weighted_depth = sum(depth_volume, na.rm = TRUE) / sum(water_area, na.rm = TRUE),
    .groups = "drop"
  )

# --- B. OCEAN SCALE ---
ocean_summary <- mcn_df %>%
  group_by(ocean) %>%
  summarise(
    total_water_area = sum(water_area, na.rm = TRUE),
    area_weighted_depth = sum(depth_volume, na.rm = TRUE) / sum(water_area, na.rm = TRUE),
    .groups = "drop"
  )

# --- C. NATIONAL TOTAL ---
national_summary <- mcn_df %>%
  summarise(
    ocean = "All Oceans",
    bioregion = "All Bioregions",
    total_water_area = sum(water_area, na.rm = TRUE),
    area_weighted_depth = sum(depth_volume, na.rm = TRUE) / sum(water_area, na.rm = TRUE)
  )

#make the plot 


# 1. Prepare Bioregion Data with Ocean Groupings
bio_df <- bioregion_summary %>%
  transmute(
    ocean = ocean,
    category = bioregion,
    depth_m = abs(area_weighted_depth),
    group_type = "Bioregion"
  )

# 2. Prepare Ocean Average Data
ocean_df <- ocean_summary %>%
  transmute(
    ocean = ocean,
    category = paste(ocean, "Average"),
    depth_m = abs(area_weighted_depth),
    group_type = "Ocean Average"
  )

# 3. Prepare National Average Data
national_df <- national_summary %>%
  transmute(
    ocean = "National",
    category = "National Average",
    depth_m = abs(area_weighted_depth),
    group_type = "National Average"
  )

# 4. Combine and build the ordered sequence with blank spacers
# We define the exact display order: Pacific regions + its average -> Spacer -> Arctic regions + its average -> Spacer -> Atlantic regions + its average -> Spacer -> National
# (Adjust the ordering of bioregions within each ocean to match your preferred sort order)

pacific_cats <- bio_df %>% filter(ocean == "Pacific") %>% arrange(desc(depth_m)) %>% pull(category)
arctic_cats  <- bio_df %>% filter(ocean == "Arctic") %>% arrange(desc(depth_m)) %>% pull(category)
atlantic_cats <- bio_df %>% filter(ocean == "Atlantic") %>% arrange(desc(depth_m)) %>% pull(category)

# Combine datasets into a master plot frame
all_plot_data <- bind_rows(bio_df, ocean_df, national_df)

get_sorted_categories <- function(ocean_name, avg_name) { # nest ocean averages within the bioregions for plot order
   bios <- bio_df %>% filter(ocean == ocean_name)
   avg <- ocean_df %>% filter(ocean == ocean_name)
  
   bind_rows(bios, avg) %>%
    arrange(desc(depth_m)) %>%
    pull(category)
}

# Generate sorted sequences for each ocean with the average nested inline
pacific_seq <- get_sorted_categories("Pacific", "Pacific Average")
arctic_seq  <- get_sorted_categories("Arctic", "Arctic Average")
atlantic_seq <- get_sorted_categories("Atlantic", "Atlantic Average")

full_sequence <- c(
  pacific_seq, "",
  arctic_seq, " ",
  atlantic_seq, "  ",
  "National Average"
)

# Convert category to an ordered factor matching our sequence, merging back the data values
plot_data_spaced <- tibble(category = full_sequence) %>%
  left_join(all_plot_data, by = "category") %>%
  mutate(
    # Re-apply the category factor levels explicitly so they plot in this exact order
    category = factor(category, levels = full_sequence),
    
    # Set fill group based on ocean, falling back for National and Spacers
    fill_group = case_when(
      group_type == "National Average" ~ "National",
      ocean == "Pacific"               ~ "Pacific",
      ocean == "Arctic"                ~ "Arctic",
      ocean == "Atlantic"              ~ "Atlantic",
      TRUE                             ~ "Spacer"
    ),
    
    # Make the border thick (1.2) for averages, thin (0.2) for normal bioregions/spacers
    border_width = case_when(
      group_type %in% c("Ocean Average", "National Average") ~ 1.2,
      TRUE ~ 0.2
    )
  )
# 2. Generate the Plot
p1 <- ggplot(plot_data_spaced, aes(x = category, y = depth_m, fill = fill_group)) +
  
  # Bold line at sea level (0 m)
  geom_hline(yintercept = 0, linewidth = 1.1, col = "black") +
  geom_hline(yintercept = c(300, 500), linetype = 2, col = "grey50") +
  
  # Single geom_col pass using aes(linewidth = I(border_width)) for dynamic outlines
  geom_col(
    aes(linewidth = I(border_width)), 
    width = 0.7, 
    color = "black"
  ) +
  
  # Add exact depth values below the tip of valid bars
  geom_text(
    data = filter(plot_data_spaced, !is.na(depth_m)),
    aes(label = paste0(round(depth_m), " m")),
    vjust = 1.3,
    size = 2.6,
    color = "grey15"
  ) +
  
  scale_y_reverse(
    limits = c(3200, 0),
    labels = scales::label_comma(suffix = " m"),
    expand = expansion(mult = c(0, 0))
  ) +
  
  # Add padding on the left and right sides so the end labels aren't cut off
  scale_x_discrete(
    expand = expansion(add = c(0.8, 0.8))
  ) +
  
  # Map fills so Ocean Averages match their ocean color, and National is green
  scale_fill_manual(
    values = c(
      "Pacific"  = "#2b5c8f",
      "Arctic"   = "#41b6c4",
      "Atlantic" = "#225ea8",
      "National" = "#2ca02c", # Green for National Average on the far right
      "Spacer"   = "transparent"
    ),
    name = NULL,
    breaks = c("Pacific", "Arctic", "Atlantic", "National"),
    labels = c("Pacific", "Arctic", "Atlantic", "National")
  ) +
  
  labs(
    x = NULL,
    y = "Area-weighted mean depth (m)",
  ) +
  
  theme_bw() +
  coord_cartesian(clip = "off") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold", size = 6, color = "black"),
    axis.text.y = element_text(size = 7, color = "black"),
    axis.title.y = element_text(size = 8.5, color = "black"), # note: changed axis.title.x to y based on context, adjust if needed
    legend.position = "none",
    plot.margin = margin(t = 5, r = 10, b = 25, l = 10)
  )

ggsave("output/can_mcn_depths.png",p1,width=6,height=4,units="in",dpi=300)

