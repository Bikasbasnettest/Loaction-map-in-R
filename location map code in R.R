library(ggplot2)
library(sf)
library(ggspatial)
studyarea<-st_read("E:/Location map in Rshape file/local_unit.shp")
# Plot the study area
Nepal<-ggplot() +
  geom_sf(data = studyarea, fill= "darkgreen") +
  theme_minimal()
Nepal
#for the additions of the  location data 
library(ggplot2)
library(sf)
library(ggspatial)
studyarea<-st_read("E:/Location map in Rshape file/Kasi/India_State_Boundary.shp")
library(readxl)
Res_Site1CD<- read_excel("E:/Location map in Rshape file/SMD CURRENT DATA TABLE.xlsx")
Res_Site1CD
########another code 
library(dplyr)
Res_Site <- Res_Site1CD %>%
  mutate(EI_Level = case_when(
    EI == 0 ~ "0",
    EI >= 1 & EI < 26 ~ "25",
    EI >= 26 & EI < 51 ~ "50",
    EI >= 51 & EI < 76 ~ "75",
    EI >= 76 & EI <= 100 ~ "100"
  )) %>%
  mutate(EI_Level = factor(EI_Level, levels = c("0", "25", "50", "75", "100")))  # Set the correct order

# Convert to spatial data frame
research_sites_sf <- st_as_sf(Res_Site, coords = c("Longitude", "Latitude"), crs = 4326)

# Create the map with specified EI levels
Mapcd <- ggplot() +
  geom_sf(data = studyarea, fill = "white", color = "black") +  # Shapefile layer
  geom_sf(data = research_sites_sf, aes(color = EI_Level), size = 3) +  # Research sites layer with color
  geom_sf_text(data = research_sites_sf, aes(label = Location), 
               nudge_y = 0.2, size = 3, color = "black") +  # Adding labels
  scale_color_manual(values = c(
    "0" = "#D3D3D3",           # Light grey for 0
    "25" = "#FF9999",          # Light red for 25
    "50" = "#FF6666",          # Medium red for 50
    "75" = "#FF3333",          # Darker red for 75
    "100" = "#FF0000"          # Bright red for 100
  )) +  # Customize colors
  theme_minimal() +
  labs(title = "Research Sites(Current data) in India",
       x = "Longitude",
       y = "Latitude",
       color = "EI Level") +  # Legend title
  guides(color = guide_legend(override.aes = list(size = 4)))
Mapcd
#####################
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
