[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)
## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) ** SPL_FECAmaps** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml
Name of Quantlet:  SPL_FECAmaps

Published in:      Statistical Programming Languages

Description:       'Uses a shapefile of California to draw polygons of California zip codes are 
                   and fill them with a color representing the candidate who has collected the 
                   largest amount of donations in total in the corresponding zip code. The filled 
                   polygons are then plotted by longitute and latitute on a Google map image of 
                   California.'

Keywords:          ggplot2, ggmap, map, visualization, gis, shapefile, rgdal, rgeos, fec, election

Author:            Lukas Moedl

Submitted:         Mon, March 13 2017

Datafile:          'dat4.csv, tl_2010_06_zcta510 (folder)'
```


![Picture1](FECAmap_ca1.png)
![Picture2](FECAmap_la1.png)
![Picture3](FECAmap_sf1.png)

## R Code
```r
# Clean up the workspace
rm(list = ls())
if(length(sessionInfo()$otherPkgs) > 0)
    lapply(paste("package:", names(sessionInfo()$otherPkgs), sep = ""), detach, 
        character.only = TRUE, unload = TRUE, force = TRUE)



# Load libraries
library("magrittr")
library("dplyr")
library("maptools")
library("rgdal")
library("rgeos")
library("ggplot2")
library("ggmap")
# Note that if you dont want to install maptools, rgdal and rgeos you can just
# download the file "cazipshp.csv" from our github repository, comment out these 
# packages and everything from line 29 until line 47. Replace then
#
#   shpdat = shpdat %>%
#       mutate(grp = group, zip = id) %>%
#       select(long, lat, grp, zip)
#
# in line 48 with 
#
#   shpdat = "cazipshp.csv" %>% read.csv()
#


# Load the full prepared data set
dat = "dat4.csv" %>% read.csv()

# We need the variables zip and area1_cand. Drop all nonunique values to 
# increase CPU efficiency
dat = dat %>%
    select(zip, area1_cand) %>%
    unique()

# Load the shapefile and convert it to long lat format with the datum used by
# google maps (WGS84)
shp = readOGR("tl_2010_06_zcta510","tl_2010_06_zcta510") %>%
    spTransform(CRS("+proj=longlat +datum=WGS84"))

# But shp is a special data frame (SpatialPolygonsDataFrame) which is not very
# handy for plotting. Luckily there is a command from ggplot2 which allows 
# transforming SpatialPolygonsDataFrame into normal data.frame
shpdat = shp %>% fortify(region = "ZCTA5CE10")

# Drop unecessary columns and rename the remaing
shpdat = shpdat %>%
    mutate(grp = group, zip = id) %>%
    select(long, lat, grp, zip)

# Convert zip to integer
shpdat$zip = as.integer(shpdat$zip)

# Merge shpdat and dat to assign the values of dat to the corresponding long and
# lat values for the map
mapdat = left_join(shpdat, dat)

# # Order the candidates
mapdat$area1_cand = factor(mapdat$area1_cand, levels = c("Clinton", "Trump", 
    "Sanders", "Cruz", "Other"), ordered = TRUE)

# Download a map of CA and LA+SF metropolitean area from Google Maps
mapca = "california" %>% get_map(zoom = 6)
mapla = "los angeles" %>% get_map(zoom = 9)
mapsf = "san francisco" %>% get_map(zoom = 10)

# Plot the data on the maps
plotca1 = ggmap(mapca) +
    geom_polygon(aes(long, lat, group = grp, fill = area1_cand), mapdat, 
        alpha = 0.4, color = "black", linetype = "dotted", size = 0.05) +
    scale_fill_manual(values = c("red", "blue", "green", "orange", "black")) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 6), axis.title = element_blank(),
        axis.ticks = element_blank()) +
    ggtitle("California")

plotla1 = ggmap(mapla) +
    geom_polygon(aes(long, lat, group = grp, fill = area1_cand), mapdat, 
        alpha = 0.4, color = "black", linetype = "dotted", size = 0.05) +
    scale_fill_manual(values = c("red", "blue", "green", "orange", "black")) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 6), axis.title = element_blank(),
        axis.ticks = element_blank()) +
    ggtitle("Los Angeles")
   
plotsf1 = ggmap(mapsf) +
    geom_polygon(aes(long, lat, group = grp, fill = area1_cand), mapdat, 
        alpha = 0.4, color = "black", linetype = "dotted", size = 0.05) +
    scale_fill_manual(values = c("red", "blue", "green", "orange", "black")) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 6), axis.title = element_blank(),
        axis.ticks = element_blank()) +
    ggtitle("San Francisco")

# Save each plot to a file
# As .png and not as .pdf because the plots in pdf format would be too large
ggsave("FECAmap_ca1.png", plotca1)
ggsave("FECAmap_la1.png", plotla1)
ggsave("FECAmap_sf1.png", plotsf1)
```
