# Set directory
setwd("Serotype_19A/Manuscript/Figures_Tables_n153_FINAL/")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(maps)
library(ggrepel)
library(scatterpie)
library(tidyr)  # For pivot_wider
library(svglite)

# Load the dataset
data <- read.csv("Supplementary_Data_03.csv")

# Summarize the data to count the number of each Subtype per Country
data_summary <- data %>%
  group_by(Country, Subtype) %>%
  summarise(Count = n(), .groups = 'drop')

# Calculate total counts per Country to set the size of each pie
country_totals <- data_summary %>%
  group_by(Country) %>%
  summarise(Total = sum(Count), .groups = 'drop')

# Merge with total counts per Country
data_summary <- merge(data_summary, country_totals, by = "Country")

# Load world map data with country coordinates (centroids)
country_coords <- map_data("world") %>%
  group_by(region) %>%
  summarise(Longitude = mean(long), Latitude = mean(lat)) %>%
  rename(Country = region)

# Merge latitude and longitude into the summarized data by Country
data_summary <- merge(data_summary, country_coords, by = "Country", all.x = TRUE)

# Prepare the data for scatterpie plotting using pivot_wider
data_pie <- data_summary %>%
  pivot_wider(names_from = Subtype, values_from = Count, values_fill = list(Count = 0))  # Transform to wide format for pie charts

# Ensure column names are syntactically valid
names(data_pie) <- make.names(names(data_pie), unique = TRUE)

# Create a base plot
p <- ggplot() +
  geom_polygon(data = map_data("world"),
               aes(x = long, y = lat, group = group),
               fill = "grey95", color = "gray40",) +
  geom_scatterpie(data = data_pie,
                  aes(x = Longitude, y = Latitude, r = sqrt(Total) / 2),  # Adjust size with sqrt and larger scaling
                  cols = c("X19A", "X19A.I", "X19A.II", "X19AF"),  # Reference the Subtype columns
                  color = "black", alpha = 0.8) +  # Black borders for visibility
  geom_text_repel(data = data_pie,
                  aes(x = Longitude, y = Latitude, label = Country),
                  size = 6, color = "black",
                  max.overlaps = 50) +  # Allow more overlapping labels
  theme_minimal() +
  labs(title = "Subtype Distribution by Country",
       x = "Longitude", y = "Latitude")

# Manually add a pie chart legend for size
# Create a dataset with points for the legend
legend_data <- data.frame(
  Longitude = c(-160, -160, -160),  # Place legend pies on the left side of the map
  Latitude = c(-40, -45, -50),      # Stack the legend pies vertically
  Total = c(10, 50, 100),             # Representative sizes for the legend
  X19A = c(1, 1, 1),                # Dummy data to fill the pies
  X19A.I = c(0, 0, 0),
  X19A.II = c(0, 0, 0),
  X19AF = c(0, 0, 0)
)

# Add the legend manually to the plot
S <- p + geom_scatterpie(data = legend_data,
                    aes(x = Longitude, y = Latitude, r = sqrt(Total) / 2),
                    cols = c("X19A", "X19A.I", "X19A.II", "X19AF"),
                    color = "black", alpha = 0.8) +
  annotate("text", x = -155, y = -40, label = "Size: 10", hjust = 0, size = 5) +
  annotate("text", x = -155, y = -45, label = "Size: 50", hjust = 0, size = 5) +
  annotate("text", x = -155, y = -50, label = "Size: 100", hjust = 0, size = 5)

# Save the plot as an SVG file
ggsave(filename = "Global_Subtype_by_Country.svg", plot = S, width = 30, height = 15, units = "in", device = "svg")



##########################
###  GPSC by Country #####
##########################

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(maps)
library(ggrepel)
library(scatterpie)
library(tidyr)  # For pivot_wider
library(svglite)
library(stringr)  # For string manipulation

# Load the dataset from CSV file
data <- read.csv("Supplementary_Data_03.csv")

# Convert relevant columns to characters for consistency
data$GPSC <- as.character(data$GPSC)
data$Country <- as.character(data$Country)

# Step 1: Identify and reassign GPSC values under the threshold as "Others"
gpsc_counts <- data %>%
  group_by(GPSC) %>%
  tally() %>%
  mutate(GPSC = if_else(n >= 10, GPSC, "Others"))  # Assign GPSC as "Others" if count is below threshold

# Replace any NA values in GPSC explicitly with "Others"
gpsc_counts$GPSC[is.na(gpsc_counts$GPSC)] <- "Others"

# Step 2: Replace original GPSC values in the main dataset with the reassigned values
data <- data %>%
  left_join(gpsc_counts %>% select(GPSC, n), by = "GPSC") %>%
  mutate(GPSC = ifelse(is.na(GPSC) | n < 10, "Others", GPSC)) %>%
  select(-n)  # Drop the count column

# Check explicitly for any remaining NA values in the GPSC column and replace them
data$GPSC[is.na(data$GPSC)] <- "Others"

# Step 3: Capitalize first letter of each country name and trim whitespace
data$Country <- str_to_title(trimws(data$Country))

# Step 4: Summarize the data by GPSC and Country
data_summary <- data %>%
  group_by(Country, GPSC) %>%
  summarise(Count = n(), .groups = 'drop')

# Step 5: Calculate total counts per Country to set the size of each pie
country_totals <- data_summary %>%
  group_by(Country) %>%
  summarise(Total = sum(Count), .groups = 'drop')

# Merge with total counts per Country
data_summary <- merge(data_summary, country_totals, by = "Country")

# Step 6: Load world map data with country coordinates (centroids)
country_coords <- map_data("world") %>%
  group_by(region) %>%
  summarise(Longitude = mean(long), Latitude = mean(lat)) %>%
  rename(Country = region)

# Clean up and capitalize country names in country_coords for consistency
country_coords$Country <- str_to_title(trimws(country_coords$Country))

# Merge latitude and longitude into the summarized data by Country
data_summary <- merge(data_summary, country_coords, by = "Country", all.x = TRUE)

# Step 7: Prepare the data for scatterpie plotting using pivot_wider
data_pie <- data_summary %>%
  pivot_wider(names_from = GPSC, values_from = Count, values_fill = list(Count = 0))  # Transform to wide format for pie charts

# Step 8: Clean up column names by removing "X" prefix
# This step ensures the legend labels do not show with an "X" in front of them
names(data_pie) <- gsub("^X", "", names(data_pie))

# Identify the columns to be included in the pie chart (exclude non-GPSC columns)
gpsc_cols <- setdiff(names(data_pie), c("Country", "Total", "Longitude", "Latitude"))

# Step 9: Create a base plot with scatter pies for GPSC
S <- ggplot() +
  geom_polygon(data = map_data("world"),
               aes(x = long, y = lat, group = group),
               fill = "grey95", color = "gray40") +
  geom_scatterpie(data = data_pie,
                  aes(x = Longitude, y = Latitude, r = sqrt(Total) / 2),  # Adjust size with sqrt and larger scaling
                  cols = gpsc_cols,  # Reference the GPSC columns
                  color = "black", alpha = 0.8) +  # Black borders for visibility
  geom_text_repel(data = data_pie,
                  aes(x = Longitude, y = Latitude, label = Country),
                  size = 5, color = "black", family = "Arial",  # Set font family to Arial
                  max.overlaps = 50) +  # Allow more overlapping labels
  theme_minimal() +
  labs(title = "GPSC Distribution by Country",
       x = "Longitude", y = "Latitude") +
  theme(
    plot.title = element_text(size = 17, face = "bold", family = "Arial"),
    axis.title.x = element_text(size = 17, face = "bold", family = "Arial"),
    axis.title.y = element_text(size = 17, face = "bold", family = "Arial"),
    axis.text.x = element_text(size = 17, family = "Arial"),
    axis.text.y = element_text(size = 17, family = "Arial"),
    legend.title = element_text(size = 17, family = "Arial", face = "bold"),
    legend.text = element_text(size = 17, family = "Arial")
  )

# Save the plot as an SVG file
ggsave(filename = "Global_GPSC_by_Country.svg", plot = S, width = 30, height = 15, units = "in", device = "svg")


##########################
###  ST by Country #####
##########################

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(maps)
library(ggrepel)
library(scatterpie)
library(tidyr)  # For pivot_wider
library(svglite)
library(stringr)  # For string manipulation

# Load the dataset
data <- read.csv("Supplementary_Data_03.csv")

# Convert ST column in data to character for consistency
data$ST <- as.character(data$ST)

# Step 1: Identify and reassign ST values under the threshold as "Others"
st_counts <- data %>%
  group_by(ST) %>%
  tally() %>%
  mutate(ST = if_else(n >= 10, ST, "Others"))  # Assign "Others" for ST values below the threshold

# Ensure no ST values are left as NA by converting all NAs to "Others"
st_counts$ST[is.na(st_counts$ST)] <- "Others"

# Replace original ST values in the main dataset with the reassigned values
data <- data %>%
  left_join(st_counts %>% select(ST, n), by = "ST") %>%
  mutate(ST = ifelse(is.na(ST) | n < 10, "Others", ST)) %>%
  select(-n)  # Drop the count column

# Explicitly replace any remaining NA values in the ST column
data$ST[is.na(data$ST)] <- "Others"

# Step 2: Capitalize first letter of each country name and trim whitespace
data$Country <- str_to_title(trimws(data$Country))

# Step 3: Summarize the data by ST and Country
data_summary <- data %>%
  group_by(Country, ST) %>%
  summarise(Count = n(), .groups = 'drop')

# Step 4: Calculate total counts per Country to set the size of each pie
country_totals <- data_summary %>%
  group_by(Country) %>%
  summarise(Total = sum(Count), .groups = 'drop')

# Merge with total counts per Country
data_summary <- merge(data_summary, country_totals, by = "Country")

# Step 5: Load world map data with country coordinates (centroids)
country_coords <- map_data("world") %>%
  group_by(region) %>%
  summarise(Longitude = mean(long), Latitude = mean(lat)) %>%
  rename(Country = region)

# Clean up and capitalize country names in country_coords for consistency
country_coords$Country <- str_to_title(trimws(country_coords$Country))

# Merge latitude and longitude into the summarized data by Country
data_summary <- merge(data_summary, country_coords, by = "Country", all.x = TRUE)

# Step 6: Prepare the data for scatterpie plotting using pivot_wider
data_pie <- data_summary %>%
  pivot_wider(names_from = ST, values_from = Count, values_fill = list(Count = 0))  # Transform to wide format for pie charts

# Clean up column names by removing "X" prefix added by R
names(data_pie) <- gsub("^X", "", names(data_pie))

# Identify the columns to be included in the pie chart (exclude non-ST columns)
st_cols <- setdiff(names(data_pie), c("Country", "Total", "Longitude", "Latitude"))

# Create a base plot with scatter pies for ST
S <- ggplot() +
  geom_polygon(data = map_data("world"),
               aes(x = long, y = lat, group = group),
               fill = "grey95", color = "gray40") +
  geom_scatterpie(data = data_pie,
                  aes(x = Longitude, y = Latitude, r = sqrt(Total) / 2),  # Adjust size with sqrt and larger scaling
                  cols = st_cols,  # Reference the ST columns
                  color = "black", alpha = 0.8) +  # Black borders for visibility
  geom_text_repel(data = data_pie,
                  aes(x = Longitude, y = Latitude, label = Country),
                  size = 5, color = "black", family = "Arial",  # Set font family to Arial
                  max.overlaps = 50) +  # Allow more overlapping labels
  theme_minimal() +
  labs(title = "ST Distribution by Country",
       x = "Longitude", y = "Latitude") +
  theme(
    plot.title = element_text(size = 17, face = "bold", family = "Arial"),
    axis.title.x = element_text(size = 17, face = "bold", family = "Arial"),
    axis.title.y = element_text(size = 17, face = "bold", family = "Arial"),
    axis.text.x = element_text(size = 17, family = "Arial"),
    axis.text.y = element_text(size = 17, family = "Arial"),
    legend.title = element_text(size = 17, family = "Arial", face = "bold"),
    legend.text = element_text(size = 17, family = "Arial")
  )

# Save the plot as an SVG file
ggsave(filename = "Global_ST_by_Country.svg", plot = S, width = 30, height = 15, units = "in", device = "svg")
