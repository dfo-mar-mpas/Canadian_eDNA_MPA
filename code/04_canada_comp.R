#plotting out the outputs of 03_global_coverage for analysis

library(tidyverse)
library(sf)

#load global analysis outputs
analysis_df <- read.csv("output/global_mpa_coverage_stats.csv")


#Plot it out ------------
# 1. Filter for larger countries/nations (population >= 1,000,000) 
# and remove infinities/NAs
large_countries_data <- analysis_df %>%
  filter(!is.infinite(per_capita_cons) & !is.na(per_capita_cons) & population >= 1000000) %>%
  mutate(is_canada = ISO3 == "CAN")

# 2. Select Top 25 large countries plus Canada (ensuring Canada is always present)
top_large_countries <- large_countries_data %>%
  arrange(desc(per_capita_cons)) %>%
  slice_head(n = 25) %>%
  bind_rows(large_countries_data %>% filter(ISO3 == "CAN")) %>%
  distinct(ISO3, .keep_all = TRUE)

# 3. Create the ggplot for large countries
  p1 <- ggplot(top_large_countries, aes(x = reorder(name, per_capita_cons), y = per_capita_cons, fill = is_canada)) +
  # Draw bars
  geom_col(width = 0.75, alpha = 0.9,col="black") +
  
  # Flip coordinates for horizontal ranking bars
  coord_flip() +
  
  # Custom colors: Highlight Canada in bright coral, others in professional blue
  scale_fill_manual(
    values = c("FALSE" = "#2b5c8f", "TRUE" = "#e74c3c"),
    labels = c("Large/Mid-sized Countries", "Canada"),
    name = ""
  ) +
  
  # Format y-axis numbers nicely
  scale_y_continuous(labels = comma,expand = 0) +
  
  # Labels and themes
  labs(
    #title = "Marine Protection Per Capita (Populations > 1 Million)",
    x = "",
    y = expression(Square~Kilometers~(km^2)~Protected~Per~Person),
    caption = "Data Source: Protected Planet (WDPCA), Marine Regions EEZ, & RNaturalEarth"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.77,0.1),
    legend.title = element_blank(),
    legend.background = element_blank(),
    axis.text.y = element_text(size = 8, face = "plain")
  )

# Print the plot
ggsave("output/canada_global_coverage.png",p1,height=4,width=6,units="in",dpi=300)
