###Project: B.1.1.7 Burden
###Purpose: Figure 1
###Author: Josh Petrie
###Date: 12/23/2021
###Input: "MI County Details.xlsx"
###Input: "counties_and_fips_codes.csv"
###Output: "region_map.pdf"

# =========== Load Libraries ==================
library(tigris)
library(data.table)
library(openxlsx)
library(dplyr)
library(ggplot2)

# =========== Get data ==================
ofp <- "original/data/filepath"
#get Michigan county geometries from tigris
#mich has a variable called "GEOID" which corresponds to "County.FIPS"
mich <- counties("Michigan", cb = TRUE)

# load in data to map 
ph_regions <- read.xlsx(paste0(ofp,"/MI County Details.xlsx"),
                        sheet="Cumulative")
cf <- fread(paste0(ofp,"/counties_and_fips_codes.csv"))

# =========== Figure 1: Map of Michigan Public Health Preparedness Regions ==================
# join data to mich
ph_regions$County <- gsub(" County, Michigan", "", ph_regions$County)

cf$County <- gsub(" County", "", cf$County.Name)

region_with_fips <- left_join(ph_regions, cf, by="County")

mich <- merge(mich, region_with_fips, by.x = c("GEOID"), 
              by.y = c("County.FIPS"))

region_labels <- data.frame(
  "Region" = sort(unique(mich$PH_Region)),
  "Latitude" = c(42.6, 42.7, 42.25, 43.5, 42.3, 43.6, 45.1, 46.3),
  "Longitude" = c(-84.36, -82.8, -83.2, -83.5, -85.5, -85.5, -84.5, -86.5)
  )

#plot map
p1 <- ggplot(data = mich) + 
  geom_sf(color="white", size=0.25) + 
  geom_sf(aes(fill = PH_Region)) + 
  geom_text(
    data=region_labels,
    aes(label = Region, fontface = "bold", x = Longitude, y = Latitude),
    size = 4
    ) +
  labs(fill = "Public Health \nPreparedness Region") + 
  theme_void() +
  theme(legend.position = "none")

#save map
pdf("region_map.pdf",
    width = 6, height = 6)
  p1
dev.off() 
jpeg("region_map.jpg",
    width = 6, height = 6, units="in",res=800)
p1
dev.off() 
