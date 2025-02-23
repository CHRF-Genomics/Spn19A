# Set directory
setwd("Serotype_19A/Manuscript/Figures_Tables_n153_FINAL/")

# Load libraries
library(ggplot2)
library(dplyr)
library(svglite)

##############################################
# Stacked bar plot - Res vs Year (all data) #
##############################################

# Load the data
data <- read.csv("Supplementary_Data_01_v2.csv")

# Count the number of observations per year and ResProfile
data_summary <- data %>%
  group_by(Year, ResProfile) %>%
  tally()

# Count total observations per year
year_counts <- data %>%
  group_by(Year) %>%
  summarize(total = n())

# Merge the observation counts with the summarized data
data_summary <- merge(data_summary, year_counts, by = "Year")

# Create a new X-axis label that includes Year and the total count
data_summary$Year_label <- paste0(data_summary$Year, " (n=", data_summary$total, ")")

# Define the custom color palette for ResProfile values in the specified order
custom_colors <- c(
  "None" = "#2ca02c",                # Green
  "ChlR,CotR,TetR" = "#7f7f7f",      # Grey
  "CotR" = "#1f77b4",                # Blue
  "CotR,EryR" = "#17becf",           # Cyan
  "CotR,EryR,PenR,TetR" = "#ef3b2c", # Dark Red
  "CotR,EryR,TetR" = "#fb6a4a",      # Light Red
  "CotR,PenR" = "#9467bd",           # Purple
  "CotR,TetR" = "#bcbd22",           # Yellow-green
  "EryR,TetR" = "#e377c2",           # Pink
  "TetR" = "#8c564b"                 # Brown
)

# Reorder the ResProfile factor levels to match the legend order
data_summary$ResProfile <- factor(data_summary$ResProfile, 
                                  levels = c("None", "ChlR,CotR,TetR", "CotR", "CotR,EryR", "CotR,EryR,PenR,TetR", 
                                             "CotR,EryR,TetR", "CotR,PenR", "CotR,TetR", "EryR,TetR", "TetR"))

# Create the stacked percentage bar chart for ResProfile by Year with observation counts
P <- ggplot(data_summary, aes(x = factor(Year_label), y = n, fill = factor(ResProfile))) +
  geom_bar(stat = "identity", position = "fill", color = "black", width = 0.7) +  # Black borders and control bar width
  labs(x = "Year", y = "Percentage", fill = "Resistance Profile") +  # Change legend box title here
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +  # Percentage format
  scale_x_discrete(expand = c(0, 0)) +  # Remove X-axis gap
  scale_fill_manual(values = custom_colors) +  # Apply custom colors for ResProfile
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
ggsave(filename = "Res_by_Year_stacked.svg", plot = P, width = 10, height = 6, units = "in", device = "svg")


##############################################
# Stacked bar plot - Res vs GPSC (all data) #
##############################################
# Load the data
data <- read.csv("Supplementary_Data_01_v2.csv")

# Count the occurrences of GPSC
gpsc_counts <- data %>%
  group_by(GPSC) %>%
  tally()

# Identify GPSC values with less than 5 occurrences
small_gpsc <- gpsc_counts %>%
  filter(n < 5) %>%
  pull(GPSC)

# Replace small GPSC values with 'Others'
data$GPSC <- ifelse(data$GPSC %in% small_gpsc, 'Others', data$GPSC)

# Count the number of observations per GPSC and ResProfile
data_summary <- data %>%
  group_by(GPSC, ResProfile) %>%
  tally()

# Count total observations per GPSC
gpsc_counts <- data %>%
  group_by(GPSC) %>%
  summarize(total = n())

# Merge the observation counts with the summarized data
data_summary <- merge(data_summary, gpsc_counts, by = "GPSC")

# Create a new X-axis label that includes GPSC and the total count
data_summary$GPSC_label <- paste0(data_summary$GPSC, " (n=", data_summary$total, ")")

# Define the custom color palette for ResProfile values in the specified order
custom_colors <- c(
  "None" = "#2ca02c",                # Green
  "ChlR,CotR,TetR" = "#7f7f7f",      # Grey
  "CotR" = "#1f77b4",                # Blue
  "CotR,EryR" = "#17becf",           # Cyan
  "CotR,EryR,PenR,TetR" = "#ef3b2c", # Dark Red
  "CotR,EryR,TetR" = "#fb6a4a",      # Light Red
  "CotR,PenR" = "#9467bd",           # Purple
  "CotR,TetR" = "#bcbd22",           # Yellow-green
  "EryR,TetR" = "#e377c2",           # Pink
  "TetR" = "#8c564b"                 # Brown
)

# Reorder the ResProfile factor levels to match the legend order
data_summary$ResProfile <- factor(data_summary$ResProfile, 
                                  levels = c("None", "ChlR,CotR,TetR", "CotR", "CotR,EryR", "CotR,EryR,PenR,TetR", 
                                             "CotR,EryR,TetR", "CotR,PenR", "CotR,TetR", "EryR,TetR", "TetR"))

# Create the stacked percentage bar chart for ResProfile by GPSC with observation counts
P <- ggplot(data_summary, aes(x = factor(GPSC_label), y = n, fill = factor(ResProfile))) +
  geom_bar(stat = "identity", position = "fill", color = "black", width = 0.7) +  # Black borders and control bar width
  labs(x = "GPSC", y = "Percentage", fill = "Resistance Profile") +  # Change legend box title here
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +  # Percentage format
  scale_x_discrete(expand = c(0, 0)) +  # Remove X-axis gap
  scale_fill_manual(values = custom_colors) +  # Apply custom colors for ResProfile
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
ggsave(filename = "Res_by_GPSC_stacked.svg", plot = P, width = 8, height = 6, units = "in", device = "svg")


##############################################
# Stacked bar plot - Res vs ST (all data) #
##############################################
# Load the data
data <- read.csv("Supplementary_Data_01_v2.csv")

# Count the occurrences of ST
st_counts <- data %>%
  group_by(ST) %>%
  tally()

# Identify ST values with less than 5 occurrences
small_st <- st_counts %>%
  filter(n < 4) %>%
  pull(ST)

# Replace small ST values with 'Others'
data$ST <- ifelse(data$ST %in% small_st, 'Others', data$ST)

# Count the number of observations per ST and ResProfile
data_summary <- data %>%
  group_by(ST, ResProfile) %>%
  tally()

# Count total observations per ST
st_counts <- data %>%
  group_by(ST) %>%
  summarize(total = n())

# Merge the observation counts with the summarized data
data_summary <- merge(data_summary, st_counts, by = "ST")

# Create a new X-axis label that includes ST and the total count
data_summary$ST_label <- paste0(data_summary$ST, " (n=", data_summary$total, ")")

# Define the custom color palette for ResProfile values in the specified order
custom_colors <- c(
  "None" = "#2ca02c",                # Green
  "ChlR,CotR,TetR" = "#7f7f7f",      # Grey
  "CotR" = "#1f77b4",                # Blue
  "CotR,EryR" = "#17becf",           # Cyan
  "CotR,EryR,PenR,TetR" = "#ef3b2c", # Dark Red
  "CotR,EryR,TetR" = "#fb6a4a",      # Light Red
  "CotR,PenR" = "#9467bd",           # Purple
  "CotR,TetR" = "#bcbd22",           # Yellow-green
  "EryR,TetR" = "#e377c2",           # Pink
  "TetR" = "#8c564b"                 # Brown
)

# Reorder the ResProfile factor levels to match the legend order
data_summary$ResProfile <- factor(data_summary$ResProfile, 
                                  levels = c("None", "ChlR,CotR,TetR", "CotR", "CotR,EryR", "CotR,EryR,PenR,TetR", 
                                             "CotR,EryR,TetR", "CotR,PenR", "CotR,TetR", "EryR,TetR", "TetR"))

# Create the stacked percentage bar chart for ResProfile by ST with observation counts
P <- ggplot(data_summary, aes(x = factor(ST_label), y = n, fill = factor(ResProfile))) +
  geom_bar(stat = "identity", position = "fill", color = "black", width = 0.7) +  # Black borders and control bar width
  labs(x = "ST", y = "Percentage", fill = "Resistance Profile") +  # Change legend box title here
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +  # Percentage format
  scale_x_discrete(expand = c(0, 0)) +  # Remove X-axis gap
  scale_fill_manual(values = custom_colors) +  # Apply custom colors for ResProfile
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
ggsave(filename = "Res_by_ST_stacked.svg", plot = P, width = 8, height = 6, units = "in", device = "svg")



###############################################
#   Stacked bar plot - Res vs SourceFinal     #
###############################################
# Load libraries
library(ggplot2)
library(dplyr)
library(svglite)

# Load the data
data <- read.csv("Supplementary_Data_01_v2.csv")

create_stacked_bar_plot <- function(data, source_type, filename) {
  
  # Filter the data by SourceFinal
  data_filtered <- data %>% filter(SourceFinal == source_type)
  
  # Count the number of observations per year and ResProfile
  data_summary <- data_filtered %>%
    group_by(Year, ResProfile) %>%
    tally()
  
  # Count total observations per year
  year_counts <- data_filtered %>%
    group_by(Year) %>%
    summarize(total = n())
  
  # Merge the observation counts with the summarized data
  data_summary <- merge(data_summary, year_counts, by = "Year")
  
  # Create a new X-axis label that includes Year and the total count
  data_summary$Year_label <- paste0(data_summary$Year, " (n=", data_summary$total, ")")
  
  # Define the custom color palette for ResProfile values in the specified order
  custom_colors <- c(
    "None" = "#2ca02c",                # Green
    "ChlR,CotR,TetR" = "#7f7f7f",      # Grey
    "CotR" = "#1f77b4",                # Blue
    "CotR,EryR" = "#17becf",           # Cyan
    "CotR,EryR,PenR,TetR" = "#ef3b2c", # Dark Red
    "CotR,EryR,TetR" = "#fb6a4a",      # Light Red
    "CotR,PenR" = "#9467bd",           # Purple
    "CotR,TetR" = "#bcbd22",           # Yellow-green
    "EryR,TetR" = "#e377c2",           # Pink
    "TetR" = "#8c564b"                 # Brown
  )
  
  # Reorder the ResProfile factor levels to match the legend order
  data_summary$ResProfile <- factor(data_summary$ResProfile, 
                                    levels = c("None", "ChlR,CotR,TetR", "CotR", "CotR,EryR", "CotR,EryR,PenR,TetR", 
                                               "CotR,EryR,TetR", "CotR,PenR", "CotR,TetR", "EryR,TetR", "TetR"))
  
  # Create the stacked percentage bar chart for ResProfile by Year with observation counts
  P <- ggplot(data_summary, aes(x = factor(Year_label), y = n, fill = factor(ResProfile))) +
    geom_bar(stat = "identity", position = "fill", color = "black", width = 0.7) +  # Black borders and control bar width
    labs(x = "Year", y = "Percentage", fill = "Resistance Profile") +  # Change legend box title here
    scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +  # Percentage format
    scale_x_discrete(expand = c(0, 0)) +  # Remove X-axis gap
    scale_fill_manual(values = custom_colors) +  # Apply custom colors for ResProfile
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
  ggsave(filename = filename, plot = P, width = 10, height = 6, units = "in", device = "svg")
}

#######################################
# Generate plots for each SourceFinal #
#######################################

# Plot for SourceFinal = "Invasive"
create_stacked_bar_plot(data, "Invasive", "Res_by_Year_Invasive.svg")

# Plot for SourceFinal = "NP"
create_stacked_bar_plot(data, "NP", "Res_by_Year_NP.svg")

# Plot for SourceFinal = "OM"
create_stacked_bar_plot(data, "OM", "Res_by_Year_OM.svg")

