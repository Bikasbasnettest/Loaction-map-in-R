library(ggplot2)
library(sf)
library(ggspatial)
studyarea<-st_read("E:/Location map in Rshape file/local_unit.shp")
# Plot the study area
Nepal<-ggplot() +
  geom_sf(data = studyarea, fill= "darkgreen") +
  theme_minimal()
Nepal

colnames(studyarea)

# Define custom colors for each province
custom_colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF", "#FFA500")

# Plot shapefile with provinces highlighted and custom colors
Biku1<-ggplot() +
  geom_sf(data = studyarea, aes(fill = Province)) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()
Biku1


# # Load Nepal district map data
# studyarea <- st_read("E:/Location map in Rshape file/local_unit.shp")

# Filter Nepal district map data for specific study area districts
study_area_districts <- c("SUNSARI", "BARA", "CHITAWAN", "BANKE")
filtered_study_area_districts <- studyarea[studyarea$DISTRICT %in% study_area_districts, ]

# Create a base map
base_map <- ggplot() +
  geom_sf(data = studyarea, aes(fill = DISTRICT), color = "darkgreen") +  # Standard map of Nepal with filled districts
  geom_sf(data = filtered_study_area_districts, aes(fill = DISTRICT), color = "black", alpha = 0.5) +  # Overlay study area districts with specific fill color
  scale_fill_manual(values = c("SUNSARI" = "red", "BARA" = "green", "CHITAWAN" = "blue", "BANKE" = "orange"), 
                    name = "District", guide = "legend") +  # Customize colors and add legend
  theme_void() +  # Standard theme without any background elements
  labs(title = "Study Area in Nepal")  # Add a title

# Add directional arrow legend at top right
Biku<-base_map + 
  annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl")
Biku
# to combine the plot
print(Biku1)
print(Biku)
library(gridExtra)
combined_plot <- grid.arrange(Biku1, Biku, Nepal, ncol = 2)
combined_plot
library(patchwork)
spacer_plot <- plot_spacer()
combined_plot <- Biku1 + spacer_plot + Biku
combined_plot
ggsave("map_with_annotations.jpeg", combined_plot, width = 10, height = 8, units = "in", dpi = 300)
getwd()

##33to highlishts the selected municpalities only
colnames(studyarea)
municipalities <- c("Bharatpur", "Itahari", "Nepalgunj", "Parwanipur")
filtered_municipalities <- studyarea[studyarea$GaPa_NaPa %in% municipalities, ]

# Create a plot to visualize the municipalities
municipalities_plot <- ggplot() +
  geom_sf(data = studyarea, color = "darkgreen") +  # Base map of Nepal
  geom_sf(data = filtered_municipalities, aes(fill = GaPa_NaPa)) +  # Highlighted municipalities
  scale_fill_manual(values = c("red", "green", "blue", "orange")) +  # Customize fill colors
  theme_void() +  # Remove background elements
  labs(title = "Highlighted Municipalities")  # Add a title

# Display the plot
municipalities_plot
library(gridExtra)
combined_plot <- grid.arrange(Biku1, Biku, Nepal,municipalities_plot, ncol = 2)
combined_plot