#Analysis of global MPA coverage based on the World Protected Areas Database inclusive of MPAs and OECMs

#load libraries
library(sf)
library(tidyverse)
library(rnaturalearth)
library(mregions2)

#### note you can skip this code to the end and load the saved file 

#load shapefiles
can_eez <- read_sf("data/shapefiles/can_eez.shp")

#calculate  coverage based on the reported CPCAD (noting that protected planet may not be up to date)
can_coverage <- as.numeric(st_area(can_eez) / 1e6) * 0.1765

#Assess coverage from the wdpa data

gdb_path <- "data/wdpa/WDPA_WDOECM_Oct2026_Public_marine.gdb"

Check what layers are inside the geodatabase
layers <- st_layers(gdb_path)

#load the marine PAs as a sf shapefile (this is very large, takes time)
marine_pas <- st_read(gdb_path, layer = layers$name[1])%>%
              st_transform(crs = 6933)%>%# Transform to an equal-area coordinate reference system (EPSG: 6933 - WGS 84 / World Behrmann)

#there are some transboundary MPAs so lets get those and split them then reallocate
marine_pas%>%st_drop_geometry()%>%filter(grepl(";",ISO3))%>%pull(ISO3)%>%unique()

target_iso_codes <- marine_pas %>%
  filter(grepl(";", ISO3)) %>%
  pull(ISO3)%>%
  strsplit(";") %>%
  unlist() %>%
  trimws() %>%
  unique()

cql_query <- paste0("iso_ter1 IN ('", paste(target_iso_codes, collapse = "', '"), "')")

#get the eezs for those 
filtered_eez_sf <- mrp_get("eez", cql_filter = cql_query) %>% 
  st_transform(crs = 6933)

transboundary_mpas <- marine_pas %>%
  filter(grepl(";", ISO3))

intersected_tb <- st_intersection(transboundary_mpas, filtered_eez_sf)

# ---------------------------------------------------------
# MPAs that are only within an EEZ based on the protected planet data
# ---------------------------------------------------------
single_country <- marine_pas %>%
  filter(!grepl(";", ISO3)) %>%
  rename(geometry = SHAPE) %>%
  group_by(ISO3) %>%
  summarize(geometry = st_union(geometry), .groups = "drop") %>% 
  mutate(net_marine_prot_km2 = as.numeric(st_area(geometry) / 1e6)) %>%
  st_drop_geometry() %>%
  select(ISO3, net_marine_prot_km2)

# ---------------------------------------------------------
# Transboundary MPAs from protected planet (flagged with a ";" in the ISO code)
# ---------------------------------------------------------
tb_summary <- intersected_tb %>%
  rename(geometry = SHAPE) %>%
  group_by(ISO3 = iso_ter1) %>%
  summarize(geometry = st_union(geometry), .groups = "drop") %>% 
  mutate(net_marine_prot_km2 = as.numeric(st_area(geometry) / 1e6)) %>%
  st_drop_geometry() %>%
  select(ISO3, net_marine_prot_km2)


all_country_marine <- bind_rows(single_country, tb_summary) %>%
  group_by(ISO3) %>%
  summarize(total_marine_prot_km2 = sum(net_marine_prot_km2, na.rm = TRUE),
            .groups = "drop")

#correct for Canada
mpa_coverage_df <- all_country_marine%>%
                    mutate(total_marine_prot_km2 = ifelse(ISO3 == "CAN",can_coverage,total_marine_prot_km2)) #sub in the Canadian reported coverage

#how different is the WDPA vs Canada (%)
   1-(all_country_marine%>%filter(ISO3=="CAN")%>%pull(total_marine_prot_km2)/
    mpa_coverage_df%>%filter(ISO3=="CAN")%>%pull(total_marine_prot_km2))%>%round(.,2)


#Now get the population estimates 
   
   world_data <- ne_countries(scale = "medium", returnclass = "sf") %>%
     st_drop_geometry() %>%
     select(iso_a3, name, pop_est) %>%
     rename(ISO3 = iso_a3, population = pop_est)
   
   #now get the global EEZs
   global_eez <- mrp_get("eez") %>% 
     st_transform(crs = 6933)
   
   eez_areas_df <- global_eez %>%
     mutate(polygon_area_km2 = as.numeric(st_area(geometry) / 1e6)) %>%
     st_drop_geometry() %>%
     group_by(iso_ter1) %>% # Use 'iso_ter1' to capture specific territories as well as sovereign states
     summarize(
       eez_area_km2 = sum(polygon_area_km2, na.rm = TRUE),
       .groups = "drop"
     ) %>%
     rename(ISO3 = iso_ter1)
   
   # Merge protected area totals with population data
   analysis_df <- mpa_coverage_df %>%
     left_join(world_data, by = "ISO3")%>%
     mutate(name = case_when( #fix the sub categories. 
       ISO3 == "BES" ~ "Bonaire, Sint Eustatius and Saba", #issue spots - population fixed below
       ISO3 == "BVT" ~ "Bouvet Island",
       ISO3 == "CCK" ~ "Cocos (Keeling) Islands",
       ISO3 == "CXR" ~ "Christmas Island",
       ISO3 == "GIB" ~ "Gibraltar",
       ISO3 == "GLP" ~ "Guadeloupe",
       ISO3 == "GUF" ~ "French Guiana",
       ISO3 == "MTQ" ~ "Martinique",
       ISO3 == "MYT" ~ "Mayotte",
       ISO3 == "REU" ~ "Réunion",
       ISO3 == "SJM" ~ "Svalbard and Jan Mayen",
       ISO3 == "UMI" ~ "U.S. Minor Outlying Islands",
       ISO3 == "ABNJ" ~ "Areas Beyond National Jurisdiction (High Seas)",
       TRUE ~ name # keeps whatever name was already there
     ))%>%
     left_join(eez_areas_df, by = "ISO3")%>%
     mutate(eez_area_km2 = ifelse(ISO3 == "ABNJ",215000000,eez_area_km2), # Approximate total High Seas area in km2
            population = case_when( #manually assign values since they are not on the Rnaturalearth
              ISO3 == "ABNJ" ~ 0,
              ISO3 == "BES"  ~ 27000,
              ISO3 == "CCK"  ~ 600,
              ISO3 == "CXR"  ~ 1700,
              ISO3 == "GIB"  ~ 34000,
              ISO3 == "GLP"  ~ 380000,
              ISO3 == "GUF"  ~ 300000,
              ISO3 == "MTQ"  ~ 350000,
              ISO3 == "MYT"  ~ 310000,
              ISO3 == "REU"  ~ 860000,
              ISO3 == "SJM"  ~ 2900,
              ISO3 == "UMI"  ~ 0,
              ISO3 == "BVT"  ~ 0, # Uninhabited
              TRUE ~ population)) %>%
     filter(!is.na(eez_area_km2))%>%
     mutate(prop_coverage = round(total_marine_prot_km2/eez_area_km2,4)*100,
            per_capita_cons = round(total_marine_prot_km2/population,4)*100,
            per_capita_target= round((eez_area_km2*0.3)/population,4)*100,
            prop_coverage_capped = pmin(prop_coverage, 100) #there are some small areas and overseas territories that cause issues. For the sake of the analysis just cap these at 100 "BES", "GLP", "MAF", "MCO", "MTQ", "MYT", "NCL" 
            )
   
   write.csv(analysis_df,file="output/global_mpa_coverage_stats.csv",row.names = FALSE)
       
