### Subtype, SOURCE, and PCV Period
# Setting working directory where the CSV files are. 
setwd("CSV_FOLDER/")

library(alluvial)
library(magrittr)
library(tidyverse)
library(dplyr)

# Load the data
data <- read.csv("Alluvial_Subtype_Source_PCV.csv")
fdata <- as.data.frame(data, stringsAsFactors = FALSE)

# Group the data
fdata %>% group_by(Source, Subtype, Period) %>% summarise(n = sum(Freq)) -> data3d

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

##############################################################
##############################################################

### Year, GPSC, Source
# Setting working directory where the CSV files are. 
setwd("CSV_FOLDER/")

library(alluvial)
library(magrittr)
library(tidyverse)
library(dplyr)

# Load the data
data <- read.csv("Alluvial_Source_GPSC_Year.csv")
fdata <- as.data.frame(data, stringsAsFactors = FALSE)

# Group the data
fdata %>% group_by(Source, GPSC, Year) %>% summarise(n = sum(Freq)) -> data3d

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

##############################################################
##############################################################

# Setting working directory where the CSV files are. 
setwd("CSV_FOLDER/")

library(alluvial)
library(magrittr)
library(tidyverse)
library(dplyr)

### Year, ST, Source

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
