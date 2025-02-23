# Set directory
setwd("Serotype_19A/Manuscript/Figures_Tables_n153_FINAL/")

# Load libraries
library(ggplot2)
library(dplyr)
library(svglite)


###############################################
# Stacked bar plot - ST vs Year (all data)    #
###############################################
# Loading data
data <- read.csv("Supplementary_Data_01_v2.csv")

# Group ST <4 genomes to 'Other'
ST_counts <- data %>%
  group_by(ST) %>%
  tally()

# Identify ST <4 genomes
small_STs <- ST_counts %>%
  filter(n < 4) %>%
  pull(ST)

# Replace small ST values with 'Other'
data$ST <- ifelse(data$ST %in% small_STs, 'Other', data$ST)

# Count ST numbers per year
year_counts <- data %>%
  group_by(Year) %>%
  summarize(total = n())

# Merge total count with original data
data <- merge(data, year_counts, by = "Year")

# Create labels for the X-axis with Year and ST numbers
data$Year_label <- paste0(data$Year, " (n=", data$total, ")")

# Define color palette for STs
custom_colors <- c(
  "12888" = "#ff9966",  # Bright Peach
  "2464" = "#66B2FF",  # Bright Blue
  "12473" = "#66FF66",  # Bright Green
  "12891" = "#99FFCC",
  "2988" = "#FFFF66",  # Bright Yellow
  "5282" = "#FFCC66",  # Bright Orange
  "12894" = "#ffccff",  # Bright Purple
  "18596" = "#4393c3",  # Bright Pink
  "Other" = "#cbd9e0"    # Color for "Other"
)

# Factor of ST column
data$ST <- factor(data$ST, levels = c("2464", "2988", "5282", "12473", "12888", "12891", "12894", 
                                      "18596", "Other"))

# Stacked percentage bar chart with color palette and borders around each box
P <- ggplot(data, aes(x = factor(Year_label), fill = factor(ST))) +
  geom_bar(position = "fill", width = 0.6, color = "black", size = 0.1) +  # Add black borders for each stacked bars
  labs(x = "Year", y = "Percentage", fill = "ST") +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +  # Remove Y-axis gap
  scale_x_discrete(expand = c(0, 0)) +  # Remove X-axis gap
  scale_fill_manual(values = custom_colors) +  # Apply color palette for ST values
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
ggsave(filename = "ST_by_Year_stacked.svg", plot = P, width = 8, height = 6, units = "in", device = "svg")


###################################################
# Stacked bar plot - ST vs Year (Invasive only) #
###################################################
# Loading data
data <- read.csv("Supplementary_Data_01_v2.csv")

# Filter the data to include only SourceFinal="Invasive"
data_inv <- data %>%
  filter(SourceFinal == "Invasive")

# Group ST <2 genomes to 'Other'
ST_counts <- data_inv %>%
  group_by(ST) %>%
  tally()

# Identify ST <2 genomes
small_STs <- ST_counts %>%
  filter(n < 2) %>%
  pull(ST)

# Replace small ST values with 'Other'
data_inv$ST <- ifelse(data_inv$ST %in% small_STs, 'Other', data_inv$ST)

# Count ST numbers per year
year_counts <- data_inv %>%
  group_by(Year) %>%
  summarize(total = n())

# Merge total count with original data
data_inv <- merge(data_inv, year_counts, by = "Year")

# Create labels for the X-axis with Year and ST numbers
data_inv$Year_label <- paste0(data_inv$Year, " (n=", data_inv$total, ")")

# Define color palette for STs
custom_colors <- c(
  "12888" = "#ff9966",  # Bright Peach
  "2464" = "#66B2FF",  # Bright Blue
  "12473" = "#66FF66",  # Bright Green
  "12891" = "#99FFCC",
  "2988" = "#FFFF66",  # Bright Yellow
  "5282" = "#FFCC66",  # Bright Orange
  "12894" = "#ffccff",  # Bright Purple
  "18596" = "#4393c3",  # Bright Pink
  "Other" = "#cbd9e0"    # Color for "Other"
)

# Factor of ST column
data$ST <- factor(data$ST, levels = c("2464", "2988", "5282", "12473", "12888", "12891", "12894", 
                                      "18596", "Other"))

# Stacked percentage bar chart with color palette and borders around each box
P<-ggplot(data_inv, aes(x = factor(Year_label), fill = factor(ST))) +
  geom_bar(position = "fill", width = 0.6, color = "black", size = 0.1) +  # Add black borders for each stacked bars
  labs(x = "Year", y = "Percentage", fill = "ST") +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +  # Remove Y-axis gap
  scale_x_discrete(expand = c(0, 0)) +  # Remove X-axis gap
  scale_fill_manual(values = custom_colors) +  # Apply color palette for ST values
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
ggsave(filename = "ST_by_Year_Inv_stacked.svg", plot = P, width = 7, height = 6, units = "in", device = "svg")


###########################################################
# Stacked bar plot - ST vs Year (Otitis media, OM only) #
###########################################################
# Loading data
data <- read.csv("Supplementary_Data_01_v2.csv")

# Filter the data to include only SourceFinal="OM"
data_om <- data %>%
  filter(SourceFinal == "OM")

# Group ST <2 genomes to 'Other'
ST_counts <- data_om %>%
  group_by(ST) %>%
  tally()

# Identify ST <2 genomes
small_STs <- ST_counts %>%
  filter(n < 2) %>%
  pull(ST)

# Replace small ST values with 'Other'
data_om$ST <- ifelse(data_om$ST %in% small_STs, 'Other', data_om$ST)

# Count ST numbers per year
year_counts <- data_om %>%
  group_by(Year) %>%
  summarize(total = n())

# Merge total count with original data
data_om <- merge(data_om, year_counts, by = "Year")

# Create labels for the X-axis with Year and ST numbers
data_om$Year_label <- paste0(data_om$Year, " (n=", data_om$total, ")")

# Define color palette for STs
custom_colors <- c(
  "12888" = "#ff9966",  # Bright Peach
  "2464" = "#66B2FF",  # Bright Blue
  "12473" = "#66FF66",  # Bright Green
  "12891" = "#99FFCC",
  "2988" = "#FFFF66",  # Bright Yellow
  "5282" = "#FFCC66",  # Bright Orange
  "12894" = "#ffccff",  # Bright Purple
  "18596" = "#4393c3",  # Bright Pink
  "Other" = "#cbd9e0"    # Color for "Other"
)

# Factor of ST column
data$ST <- factor(data$ST, levels = c("2464", "2988", "5282", "12473", "12888", "12891", "12894", 
                                      "18596", "Other"))

# Stacked percentage bar chart with color palette and borders around each box
P <- ggplot(data_om, aes(x = factor(Year_label), fill = factor(ST))) +
  geom_bar(position = "fill", width = 0.6, color = "black", size = 0.1) +  # Add black borders for each stacked bars
  labs(x = "Year", y = "Percentage", fill = "ST") +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +  # Remove Y-axis gap
  scale_x_discrete(expand = c(0, 0)) +  # Remove X-axis gap
  scale_fill_manual(values = custom_colors) +  # Apply color palette for ST values
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
ggsave(filename = "ST_by_Year_OM_stacked.svg", plot = P, width = 4.89, height = 6, units = "in", device = "svg")


#############################################
# Stacked bar plot - ST vs Year (NP only) #
#############################################
# Loading data
data <- read.csv("Supplementary_Data_01_v2.csv")

# Filter the data to include only SourceFinal="NP"
data_np <- data %>%
  filter(SourceFinal == "NP")

# Group ST <2 genomes to 'Other'
ST_counts <- data_np %>%
  group_by(ST) %>%
  tally()

# Identify ST <2 genomes
small_STs <- ST_counts %>%
  filter(n < 2) %>%
  pull(ST)

# Replace small ST values with 'Other'
data_np$ST <- ifelse(data_np$ST %in% small_STs, 'Other', data_np$ST)

# Count ST numbers per year
year_counts <- data_np %>%
  group_by(Year) %>%
  summarize(total = n())

# Merge total count with original data
data_np <- merge(data_np, year_counts, by = "Year")

# Create labels for the X-axis with Year and ST numbers
data_np$Year_label <- paste0(data_np$Year, " (n=", data_np$total, ")")

# Define color palette for STs
custom_colors <- c(
  "12888" = "#ff9966",  # Bright Peach
  "2464" = "#66B2FF",  # Bright Blue
  "12473" = "#66FF66",  # Bright Green
  "12891" = "#99FFCC",
  "2988" = "#FFFF66",  # Bright Yellow
  "5282" = "#FFCC66",  # Bright Orange
  "12894" = "#ffccff",  # Bright Purple
  "18596" = "#4393c3",  # Bright Pink
  "Other" = "#cbd9e0"    # Color for "Other"
)

# Factor of ST column
data$ST <- factor(data$ST, levels = c("2464", "2988", "5282", "12473", "12888", "12891", "12894", 
                                      "18596", "Other"))

# Stacked percentage bar chart with color palette and borders around each box
P <- ggplot(data_np, aes(x = factor(Year_label), fill = factor(ST))) +
  geom_bar(position = "fill", width = 0.6, color = "black", size = 0.1) +  # Add black borders for each stacked bars
  labs(x = "Year", y = "Percentage", fill = "ST") +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +  # Remove Y-axis gap
  scale_x_discrete(expand = c(0, 0)) +  # Remove X-axis gap
  scale_fill_manual(values = custom_colors) +  # Apply color palette for ST values
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
ggsave(filename = "ST_by_Year_NP_stacked.svg", plot = P, width = 4.89, height = 6, units = "in", device = "svg")

