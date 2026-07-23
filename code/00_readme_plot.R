## make a map of Canadian closures for the eDNA summary

#load libraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(patchwork)
library(ggspatial)
library(MarConsNetData) #go here to install https://github.com/dfo-mar-mpas/MarConsNetData
library(marmap)
library(terra)

#map projections
latlong <- "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#Load in Canadian Bioregions
bioregion_ord <- c("Southern Shelf","Strait of Georgia","Northern Shelf","Offshore Pacific",
                   "Western Arctic","Arctic Archipelago","Arctic Basin","Eastern Arctic","Hudson Bay Complex",
                   "Newfoundland-Labrador Shelves","Gulf of Saint Lawrence","Scotian Shelf")   

#load bioregions                  
bioregion_df <- read_sf("data/shapefiles/canadian_planning_regions.shp")%>% 
              st_transform(CanProj)%>%
              mutate(region=factor(region,levels=bioregion_ord))

ocean_df <- bioregion_df%>%
            st_make_valid()%>%
            group_by(ocean)%>%
            summarise(geometry = st_union(geometry))%>%
            st_make_valid()%>%
            st_transform(CanProj)

#load the cpcad_marine datafile
cpcad_marine <- read_sf("data/shapefiles/cpcad_complete.shp")%>% #can also update this with MarConsNetData but it takes a looong time
                st_transform(CanProj)%>%
                st_make_valid()

#colour palatte for plotting
colour_pal_types <- c("MPA" = "#252A6B", 
                      "OECM" = "#2AA2BD",
                      "Marine Refuge" = "#84E7DA",
                      "Gap" = "white",
                      "AOI" = "#005E6B",
                      "Draft" = "grey85")

colour_pal_bioregion <- c("Newfoundland-Labrador Shelves" = "#BCE4DF",
                          "Gulf of Saint Lawrence" = "#51AFA5",
                          "Scotian Shelf" = "#005E6B",
                          "Western Arctic"  = "#E3F6FC",
                          "Arctic Archipelago" = "#49E8F2",
                          "Arctic Basin" = "#0087D1",
                          "Eastern Arctic" = "#27ADF5",
                          "Hudson Bay Complex" = "#2B3686",
                          "Southern Shelf" = "#F3DE02",
                          "Strait of Georgia" = "#F78E12",
                          "Northern Shelf"= "#F7680C",
                          "Offshore Pacific"= "#C70007")

#Canadian eez
canada_eez <- read_sf("data/shapefiles/can_EEZ.shp")%>%
  st_transform(CanProj)

#load basemap of Canada
basemap <- ne_states(country = "Canada",returnclass = "sf")%>%
  dplyr::select(name_en,geometry)%>%
  st_as_sf()%>%
  st_union()%>%
  st_transform(latlong)%>%
  st_as_sf()%>%
  mutate(country="Canada")%>%
  rbind(.,ne_states(country = "United States of America",returnclass = "sf")%>%
          dplyr::select(name_en,geometry)%>%
          st_as_sf()%>%
          st_union()%>%
          st_transform(latlong)%>%
          st_as_sf()%>%
          mutate(country="US"),
        ne_states(country = "Greenland",returnclass = "sf")%>%
          dplyr::select(name_en,geometry)%>%
          st_as_sf()%>%
          st_union()%>%
          st_transform(latlong)%>%
          st_as_sf()%>%
          mutate(country="Greenland"),
        ne_states(country = "Iceland",returnclass = "sf")%>%
          dplyr::select(name_en,geometry)%>%
          st_as_sf()%>%
          st_union()%>%
          st_transform(latlong)%>%
          st_as_sf()%>%
          mutate(country="Iceland"))%>%
  st_transform(CanProj)

plot_region <- canada_eez%>%st_bbox()

##map of Canadian MPAs  --------------

#bounding boxes for ocean plots
atlantic_box <- ocean_df%>%
  filter(ocean=="Atlantic")%>%
  st_transform(CanProj)%>%
  st_buffer(50*1000)%>%
  st_bbox()

pacific_box <- ocean_df%>%
  filter(ocean=="Pacific")%>%
  st_transform(CanProj)%>%
  st_buffer(50*1000)%>%
  st_bbox()

arctic_box <- ocean_df%>%
  filter(ocean=="Arctic")%>%
  st_transform(CanProj)%>%
  st_buffer(20*1000)%>%
  st_bbox()

#construct plots ----

p_atlantic <- ggplot()+
  geom_sf(data=bioregion_df,fill=NA)+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country == "Canada"),fill="grey60")+
  geom_sf(data=canada_eez,fill=NA)+
  geom_sf(data=cpcad_marine%>%filter(ocean=="Atlantic"),aes(fill=type))+
  theme_bw()+
  scale_fill_manual(values = colour_pal_types)+
  coord_sf(xlim=atlantic_box[c(1,3)],ylim=atlantic_box[c(2,4)],expand=0)+
  theme(legend.position = "none")+
  annotation_scale()

p_arctic <- ggplot()+
  geom_sf(data=bioregion_df,fill=NA)+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country == "Canada"),fill="grey60")+
  geom_sf(data=canada_eez,fill=NA)+
  geom_sf(data=cpcad_marine%>%filter(ocean=="Arctic"),aes(fill=type))+
  theme_bw()+
  scale_fill_manual(values = colour_pal_types)+
  coord_sf(xlim=arctic_box[c(1,3)],ylim=arctic_box[c(2,4)],expand=0)+
  theme(legend.position = "none")+
  annotation_scale()

p_pacific <- ggplot()+
  geom_sf(data=bioregion_df,fill=NA)+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country == "Canada"),fill="grey60")+
  geom_sf(data=canada_eez,fill=NA)+
  geom_sf(data=cpcad_marine%>%filter(ocean=="Pacific"),aes(fill=type))+
  theme_bw()+
  scale_fill_manual(values = colour_pal_types)+
  coord_sf(xlim=pacific_box[c(1,3)],ylim=pacific_box[c(2,4)],expand=0)+
  annotation_scale()

p_canada <- ggplot()+
  geom_sf(data=bioregion_df,aes(fill=region))+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country == "Canada"),fill="grey60")+
  geom_sf(data=canada_eez,fill=NA)+
  geom_sf(data=cpcad_marine,fill=NA,col="black")+
  theme_bw()+
  scale_fill_manual(values=colour_pal_bioregion)+
  coord_sf(xlim=plot_region[c(1,3)],ylim=plot_region[c(2,4)],expand=0)+
  labs(fill="")+
  theme(legend.title = element_blank())

p_pacific <- p_pacific + theme(legend.position = "none")

combo_regions <- p_pacific + p_arctic +  p_atlantic
#ggsave("output/combo_regions.png",combo_regions,width=10,height=5,units="in",dpi=300)
#ggsave("output/canada_mcn.png",p_canada,width=10,height=5,units="in",dpi=300)

#stack the plots
p_canada_no_legend <- p_canada + 
  theme(
    legend.position = "none", #remove the legend which causes issues when combined
    plot.margin = margin(b = 0, t = 0)
  )

combo_regions <- p_pacific + p_arctic + p_atlantic + 
  plot_layout(ncol = 3) & 
  theme(plot.margin = margin(t = 0, b = 0))

full_combo_plot <- p_canada_no_legend / wrap_elements(full = combo_regions) + 
  plot_layout(heights = c(1, 0.8))

ggsave("output/canada_and_regions_combined.png",full_combo_plot,width = 10,height = 7.5,units = "in",dpi = 300)

#maritimes region quick plot ----

mar_network <- data_draft_areas()%>% #download from open data
  filter(SiteName_E != "Northeast Channel Coral Marine Refuge")%>% #this is just overlapped by the Fundian
  st_transform(CanProj)%>%
  mutate(type=case_when(grepl("refuge",tolower(SiteName_E)) ~ "Marine Refuge",
                        grepl("marine protected",tolower(SiteName_E)) ~ "MPA",
                        TRUE ~ "Draft"),
         type=ifelse(type == "Draft" & Classification_E == "Existing site", "OECM",type),
         type=ifelse(SiteName_E %in% c("Fundian Channel-Browns Bank","Eastern Shore Islands"),"AOI",type),
         type=factor(type,levels=c("MPA","Marine Refuge","OECM","AOI","Draft")))

got_edna_df <- read.csv("data/edna_samples/goteDNA_July_26.csv")%>% #these are all unique coordinates ignore the taxonomy
               st_as_sf(coords=c("decimalLongitude","decimalLatitude"),crs=latlong)%>%
               st_transform(CanProj)

mar_edna_df <- got_edna_df%>%st_intersection(mar_network) #note these are not all the coordinates for samples, we have much more from the RV survey yet processed

#bounding box for the maritimes plot
ss_bound <- bioregion_df%>%
  filter(region == "Scotian Shelf")%>%
  st_transform(CanProj)%>%
  st_buffer(100*1000)%>%
  st_bbox()

#get the 250m contour

atlantic_bbox_latlong <- ocean_df %>% 
  filter(ocean == "Atlantic") %>% 
  st_transform(4326) %>% 
  st_bbox()

atlantic_bathy <- getNOAA.bathy(
  lon1 = atlantic_bbox_latlong["xmin"],
  lon2 = atlantic_bbox_latlong["xmax"],
  lat1 = atlantic_bbox_latlong["ymin"],
  lat2 = atlantic_bbox_latlong["ymax"],
  resolution = 1, #1 min resolution can make it smaller if needed
  keep=TRUE,
  path="data/bathymetry"
)

#force into an sf object
contour_250m_sf <- atlantic_bathy%>%
  marmap::as.raster()%>%
  rast()%>%
  as.contour(., levels = -250) %>% 
  st_as_sf() %>% 
  st_set_crs(4326) %>% 
  st_transform(CanProj)

mar_cpcad <- cpcad_marine%>%
              filter(ocean=="Atlantic",
                     bioregion!="Scotian Shelf")


p1 <- ggplot()+
  geom_sf(data=contour_250m_sf,linewidth=0.25,col="grey80")+
  geom_sf(data=bioregion_df%>%filter(region=="Scotian Shelf"),fill=NA)+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country == "Canada"),fill="grey60")+
  geom_sf(data=mar_cpcad,aes(fill=type),alpha=0.65)+
  geom_sf(data=mar_network,aes(fill=type),alpha=0.85,col="black")+
  geom_sf(data=mar_edna_df,size=0.80)+
  geom_sf(data=ss_bound%>%st_as_sfc(),fill=NA,linewidth=0.5,linetype=2)+
  theme_bw()+
  coord_sf(xlim=ss_bound[c(1,3)],ylim=ss_bound[c(2,4)],expand=0)+
  labs(fill="")+
  scale_fill_manual(values=colour_pal_types)+
  annotation_scale(location="bl")+
  theme(legend.position="inside",
        legend.position.inside = c(0.87,0.15),
        legend.title = element_blank(),
        legend.background = element_blank())

ggsave("output/mar_edna_readme.png",p1,height=6,width=6,units="in",dpi=300)
