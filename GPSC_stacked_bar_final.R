# Set directory
setwd("Serotype_19A/Manuscript/Figures_Tables_n153_FINAL/")

# Load libraries
library(ggplot2)
library(dplyr)
library(svglite)

# Loading data
data <- read.csv("Supplementary_Data_01.csv")

###################################################
# Stacked bar plot - GPSC vs Year (Invasive only) #
###################################################

# Filter the data to include only SourceFinal="Invasive"
data_inv <- data %>%
  filter(SourceFinal == "Invasive")

# Group GPSC <2 genomes to 'Other'
gpsc_counts <- data_inv %>%
  group_by(GPSC) %>%
  tally()

# Identify GPSC <2 genomes
small_gpscs <- gpsc_counts %>%
  filter(n < 2) %>%
  pull(GPSC)

# Replace small GPSC values with 'Other'
data_inv$GPSC <- ifelse(data_inv$GPSC %in% small_gpscs, 'Other', data_inv$GPSC)

# Count GPSC numbers per year
year_counts <- data_inv %>%
  group_by(Year) %>%
  summarize(total = n())

# Merge total count with original data
data_inv <- merge(data_inv, year_counts, by = "Year")

# Create labels for the X-axis with Year and GPSC numbers
data_inv$Year_label <- paste0(data_inv$Year, " (n=", data_inv$total, ")")

# Define color palette for GPSCs
custom_colors <- c(
  "84" = "#fc8d62",  # Color for GPSC 84
  "307" = "#4393c3",  # Color for GPSC 307
  "10" = "#FFD319",   # Color for GPSC 10
  "53" = "#66c2a5",   # Color for GPSC 53
  "5" = "#a6d854",    # Color for GPSC 5
  "Other" = "#cbd5e8"    # Color for "Other"
)

# Factor of GPSC column
data_inv$GPSC <- factor(data_inv$GPSC, levels = c("84", "307", "10", "53", "5", "Other"))

# Stacked percentage bar chart with color palette and borders around each box
P <- ggplot(data_inv, aes(x = factor(Year_label), fill = factor(GPSC))) +
  geom_bar(position = "fill", width = 0.6, color = "black", size = 0.1) +  # Add black borders for each stacked bars
  labs(x = "Year", y = "Percentage", fill = "GPSC") +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +  # Remove Y-axis gap
  scale_x_discrete(expand = c(0, 0)) +  # Remove X-axis gap
  scale_fill_manual(values = custom_colors) +  # Apply color palette for GPSC values
  theme_minimal() +
  theme(
    # X and Y axis labels font - Arial
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, family = "Arial", color = "black"),  # X-axis text size 12
    axis.text.y = element_text(size = 12, family = "Arial", color = "black"),  # Y-axis text size 12
    
    # X and Y axis titles - bold and size 14
    axis.title.x = element_text(size = 14, family = "Arial", face = "bold", color = "black"),  # X-axis title
    axis.title.y = element_text(size = 14, family = "Arial", face = "bold", color = "black"),  # Y-axis title
    
    # Legend title - bold and size 14
    legend.title = element_text(size = 14, family = "Arial", face = "bold", color = "black"),  # Legend title
    
    # Legend text font size 12
    legend.text = element_text(size = 12, family = "Arial", color = "black"),
    
    # X and Y axis lines - black, 1.0 width
    axis.line = element_line(color = "black", size = 1.0),
    
    # Remove default grid lines. Looks clean
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  # Add X and Y axis lines manually
  theme(axis.line.x = element_line(color = "black", size = 1.0),
        axis.line.y = element_line(color = "black", size = 1.0))

# Save the plot as an SVG file
ggsave(filename = "GPSC_by_Year_inv_stacked.svg", plot = P, width = 7, height = 6, units = "in", device = "svg")



###########################################################
# Stacked bar plot - GPSC vs Year (Otitis media, OM only) #
###########################################################
data <- read.csv("Supplementary_Data_01.csv")

# Filter the data to include only SourceFinal="OM"
data_om <- data %>%
  filter(SourceFinal == "OM")

# Group GPSC <2 genomes to 'Other'
gpsc_counts <- data_om %>%
  group_by(GPSC) %>%
  tally()

# Identify GPSC <2 genomes
small_gpscs <- gpsc_counts %>%
  filter(n < 2) %>%
  pull(GPSC)

# Replace small GPSC values with 'Other'
data_om$GPSC <- ifelse(data_om$GPSC %in% small_gpscs, 'Other', data_om$GPSC)

# Count GPSC numbers per year
year_counts <- data_om %>%
  group_by(Year) %>%
  summarize(total = n())

# Merge total count with original data
data_om <- merge(data_om, year_counts, by = "Year")

# Create labels for the X-axis with Year and GPSC numbers
data_om$Year_label <- paste0(data_om$Year, " (n=", data_om$total, ")")

# Define color palette for GPSCs
custom_colors <- c(
  "84" = "#fc8d62",  # Color for GPSC 84
  "307" = "#4393c3",  # Color for GPSC 307
  "10" = "#FFD319",   # Color for GPSC 10
  "53" = "#66c2a5",   # Color for GPSC 53
  "5" = "#a6d854",    # Color for GPSC 5
  "Other" = "#cbd5e8"    # Color for "Other"
)

# Factor of GPSC column
data_om$GPSC <- factor(data_om$GPSC, levels = c("84", "307", "10", "53", "5", "Other"))

# Stacked percentage bar chart with color palette and borders around each box
P <- ggplot(data_om, aes(x = factor(Year_label), fill = factor(GPSC))) +
  geom_bar(position = "fill", width = 0.6, color = "black", size = 0.1) +  # Add black borders for each stacked bars
  labs(x = "Year", y = "Percentage", fill = "GPSC") +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +  # Remove Y-axis gap
  scale_x_discrete(expand = c(0, 0)) +  # Remove X-axis gap
  scale_fill_manual(values = custom_colors) +  # Apply color palette for GPSC values
  theme_minimal() +
  theme(
    # X and Y axis labels font - Arial
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, family = "Arial", color = "black"),  # X-axis text size 12
    axis.text.y = element_text(size = 12, family = "Arial", color = "black"),  # Y-axis text size 12
    
    # X and Y axis titles - bold and size 14
    axis.title.x = element_text(size = 14, family = "Arial", face = "bold", color = "black"),  # X-axis title
    axis.title.y = element_text(size = 14, family = "Arial", face = "bold", color = "black"),  # Y-axis title
    
    # Legend title - bold and size 14
    legend.title = element_text(size = 14, family = "Arial", face = "bold", color = "black"),  # Legend title
    
    # Legend text font size 12
    legend.text = element_text(size = 12, family = "Arial", color = "black"),
    
    # X and Y axis lines - black, 1.0 width
    axis.line = element_line(color = "black", size = 1.0),
    
    # Remove default grid lines. Looks clean
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  # Add X and Y axis lines manually
  theme(axis.line.x = element_line(color = "black", size = 1.0),
        axis.line.y = element_line(color = "black", size = 1.0))

# Save the plot as an SVG file
ggsave(filename = "GPSC_by_Year_OM_stacked.svg", plot = P, width = 4.89, height = 6, units = "in", device = "svg")


#############################################
# Stacked bar plot - GPSC vs Year (NP only) #
#############################################
data <- read.csv("Supplementary_Data_01.csv")

# Filter the data to include only SourceFinal="NP"
data_np <- data %>%
  filter(SourceFinal == "NP")

# Group GPSC <2 genomes to 'Other'
gpsc_counts <- data_np %>%
  group_by(GPSC) %>%
  tally()

# Identify GPSC <2 genomes
small_gpscs <- gpsc_counts %>%
  filter(n < 2) %>%
  pull(GPSC)

# Replace small GPSC values with 'Other'
data_np$GPSC <- ifelse(data_np$GPSC %in% small_gpscs, 'Other', data_np$GPSC)

# Count GPSC numbers per year
year_counts <- data_np %>%
  group_by(Year) %>%
  summarize(total = n())

# Merge total count with original data
data_np <- merge(data_np, year_counts, by = "Year")

# Create labels for the X-axis with Year and GPSC numbers
data_np$Year_label <- paste0(data_np$Year, " (n=", data_np$total, ")")

# Define color palette for GPSCs
custom_colors <- c(
  "84" = "#fc8d62",  # Color for GPSC 84
  "307" = "#4393c3",  # Color for GPSC 307
  "10" = "#FFD319",   # Color for GPSC 10
  "53" = "#66c2a5",   # Color for GPSC 53
  "5" = "#a6d854",    # Color for GPSC 5
  "44" = "#807dba",    # Color for GPSC 44
  "Other" = "#cbd5e8"    # Color for "Other"
)

# Factor of GPSC column
data_np$GPSC <- factor(data_np$GPSC, levels = c("84", "307", "10", "53", "5", "44", "Other"))

# Stacked percentage bar chart with color palette and borders around each box
P <- ggplot(data_np, aes(x = factor(Year_label), fill = factor(GPSC))) +
  geom_bar(position = "fill", width = 0.6, color = "black", size = 0.1) +  # Add black borders for each stacked bars
  labs(x = "Year", y = "Percentage", fill = "GPSC") +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +  # Remove Y-axis gap
  scale_x_discrete(expand = c(0, 0)) +  # Remove X-axis gap
  scale_fill_manual(values = custom_colors) +  # Apply color palette for GPSC values
  theme_minimal() +
  theme(
    # X and Y axis labels font - Arial
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, family = "Arial", color = "black"),  # X-axis text size 12
    axis.text.y = element_text(size = 12, family = "Arial", color = "black"),  # Y-axis text size 12
    
    # X and Y axis titles - bold and size 14
    axis.title.x = element_text(size = 14, family = "Arial", face = "bold", color = "black"),  # X-axis title
    axis.title.y = element_text(size = 14, family = "Arial", face = "bold", color = "black"),  # Y-axis title
    
    # Legend title - bold and size 14
    legend.title = element_text(size = 14, family = "Arial", face = "bold", color = "black"),  # Legend title
    
    # Legend text font size 12
    legend.text = element_text(size = 12, family = "Arial", color = "black"),
    
    # X and Y axis lines - black, 1.0 width
    axis.line = element_line(color = "black", size = 1.0),
    
    # Remove default grid lines. Looks clean
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  # Add X and Y axis lines manually
  theme(axis.line.x = element_line(color = "black", size = 1.0),
        axis.line.y = element_line(color = "black", size = 1.0))

# Save the plot as an SVG file
ggsave(filename = "GPSC_by_Year_NP_stacked.svg", plot = P, width = 4.89, height = 6, units = "in", device = "svg")

