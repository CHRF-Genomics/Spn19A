setwd("Serotype_19A/Manuscript/Figures_Tables_n153_FINAL/")

library(alluvial)
library(magrittr)
library(tidyverse)
library(dplyr)

###########################
# Source, GPSC, and Year  #
###########################

# Load the data
data <- read.csv("Alluvial_Source_GPSC_Year.csv")
fdata <- as.data.frame(data, stringsAsFactors = FALSE)

# Group the data
fdata %>% group_by(Source, GPSC, Year) %>% summarise(n = sum(Freq)) -> data3d

# Assign colors based on Source
colors <- case_when(
  data3d$Source == "Invasive" ~ "lightpink", ##FFB6C1
  data3d$Source == "NP" ~ "skyblue", ##87CEEB
  data3d$Source == "OM" ~ "mediumpurple1", ##AB82FF
  data3d$Source == "Urine" ~ "khaki3", ##CDBE70
  TRUE ~ "white"  # Default color
)

# Create the alluvial plot
alluvial(data3d[,1:3], 
         freq = data3d$n, 
         col = colors,
         border = "white", 
         alpha = 0.7, 
         blocks = TRUE)
# All figures were saved in SVG, 1486x1404 (pixel) resolution.

###################################
# Source, Subtype, and PCV Period #
###################################

# Load the data
data <- read.csv("Alluvial_Source_Subtype_PCV.csv")
fdata <- as.data.frame(data, stringsAsFactors = FALSE)

# Group the data
fdata %>% group_by(Source, Subtype, PCV) %>% summarise(n = sum(Freq)) -> data3d

# Assign colors based on Source
colors <- case_when(
  data3d$Source == "Invasive" ~ "lightpink",
  data3d$Source == "NP" ~ "skyblue",
  data3d$Source == "OM" ~ "mediumpurple1",
  data3d$Source == "Urine" ~ "khaki3",
  TRUE ~ "white"  # Default color
)

# Create the alluvial plot
alluvial(data3d[,1:3], 
         freq = data3d$n, 
         col = colors,
         border = "white", 
         alpha = 0.7, 
         blocks = TRUE)
# All figures were saved in SVG, 1486x1404 (pixel) resolution.


###########################
## Source, ST, and Year  ##
###########################

# Load the data
data <- read.csv("Alluvial_Source_ST_Year.csv")
fdata <- as.data.frame(data, stringsAsFactors = FALSE)

# Group the data
fdata %>% group_by(Source, ST, Year) %>% summarise(n = sum(Freq)) -> data3d

# Assign colors based on Source
colors <- case_when(
  data3d$Source == "Invasive" ~ "lightpink",
  data3d$Source == "NP" ~ "skyblue",
  data3d$Source == "OM" ~ "mediumpurple1",
  data3d$Source == "Urine" ~ "khaki3",
  TRUE ~ "white"  # Default color
)

# Create the alluvial plot
alluvial(data3d[,1:3], 
         freq = data3d$n, 
         col = colors,
         border = "white", 
         alpha = 0.7, 
         blocks = TRUE)
# All figures were saved in SVG, 1486x1404 (pixel) resolution.

