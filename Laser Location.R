# Laser Locations.R
# Location of laser clinics around Australia
# Created by Aja Manu on 25/01/17

# Clear working environment
rm(list = ls())

# set working directory
setwd("U:/Data/Laser Clinics")

# Load libraries
library(ggmap) # for plotting and mapping
library(ggplot2) # for plotting
library(rgeos) # for plotting and mapping
library(dismo) # for plotting and mapping
library(scales) # for `alpha()` function
library(gridExtra) # for combining plots
library(grid) # for combining plots

#### Load data------------------------------------------------------------------
data <- read.csv("Shop_Locations.csv")

#### Get Co-ords----------------------------------------------------------------
# Get unique set of Locations
places <- paste(data$Address, data$State, sep = ', ')

# Get the lat and long for locations
locs <- geocode(paste(places, "Australia", sep = ", "))
plot(locs) # check locations and correct

# Merge locations with original data
data1 <- cbind(data, locs)

# Go through the locations manually to fill in any blanks and be sure to 
#  remove the "," cell as it is blank and just points to the middle of Australia
# Write file to folder
write.csv(data1, "Locations.csv", row.names = FALSE)

# Load Locations data
locations <- read.csv("Locations.csv")

#### Draw Locations-------------------------------------------------------------
# Google Map of Australia
aust_map <- get_map(location='Australia', zoom=4)

# Draw Locations in Australia
ggmap(aust_map) + 
      geom_point(data = locations, aes(lon, lat, col = Code), size = 3,
                 alpha = 0.5) +
      ggtitle("Store Locations Australia") +
      theme(plot.title = element_text(size=20, face="bold", vjust=2, hjust = 0.5)) +
      ylim(-43,-10) +
      xlim(113,155)      

# Draw locations in major cities
### Sydney
# Google Map of Sydney
syd_map <- get_map(location='Sydney', zoom=10)

ggmap(syd_map) + 
      geom_point(data = locations, aes(lon, lat, col = Code), size = 10,
                 alpha = 0.5) +
      ggtitle("Store Locations Sydney") +
      theme(plot.title = element_text(size=20, face="bold", vjust=2, hjust = 0.5))

### Melbourne
mel_map <- get_map(location='Melbourne', zoom=10)

ggmap(mel_map) + 
      geom_point(data = locations, aes(lon, lat, col = Code), size = 10,
                 alpha = 0.5) +
      ggtitle("Store Locations Melbourne") +
      theme(plot.title = element_text(size=20, face="bold", vjust=2, hjust = 0.5))

### Brisbane
bri_map <- get_map(location='Brisbane', zoom=10)

ggmap(bri_map) + 
      geom_point(data = locations, aes(lon, lat, col = Code), size = 10,
                 alpha = 0.5) +
      ggtitle("Store Locations Brisbane") +
      theme(plot.title = element_text(size=20, face="bold", vjust=2, hjust = 0.5))

### Adelaide
adl_map <- get_map(location='Adelaide', zoom=10)

ggmap(adl_map) + 
      geom_point(data = locations, aes(lon, lat, col = Code), size = 10,
                 alpha = 0.5) +
      ggtitle("Store Locations Adelaide") +
      theme(plot.title = element_text(size=20, face="bold", vjust=2, hjust = 0.5))

### Perth
pth_map <- get_map(location='Perth', zoom=10)

ggmap(pth_map) + 
      geom_point(data = locations, aes(lon, lat, col = Code), size = 10,
                 alpha = 0.5) +
      ggtitle("Store Locations Perth") +
      theme(plot.title = element_text(size=20, face="bold", vjust=2, hjust = 0.5))

#### Combine Plots--------------------------------------------------------------

# Load plots into variable
s_plot <- ggmap(syd_map) + 
      geom_point(data = locations, aes(lon, lat, col = Code), size = 5,
                 alpha = 0.5) +
      ggtitle("Sydney") +
      theme(plot.title = element_text(size=10, face="bold", vjust=2, hjust = 0.5)) +
      theme(axis.title = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position="none")

m_plot <- ggmap(mel_map) + 
      geom_point(data = locations, aes(lon, lat, col = Code), size = 5,
                 alpha = 0.5) +
      ggtitle("Melbourne") +
      theme(plot.title = element_text(size=10, face="bold", vjust=2, hjust = 0.5)) +
      theme(axis.title = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position="none")

b_plot <- ggmap(bri_map) + 
      geom_point(data = locations, aes(lon, lat, col = Code), size = 5,
                 alpha = 0.5) +
      ggtitle("Brisbane") +
      theme(plot.title = element_text(size=10, face="bold", vjust=2, hjust = 0.5)) +
      theme(axis.title = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position="none")

a_plot <- ggmap(adl_map) + 
      geom_point(data = locations, aes(lon, lat, col = Code), size = 5,
                 alpha = 0.5) +
      ggtitle("Adelaide") +
      theme(plot.title = element_text(size=10, face="bold", vjust=2, hjust = 0.5)) +
      theme(axis.title = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position="none")

p_plot <- ggmap(pth_map) + 
      geom_point(data = locations, aes(lon, lat, col = Code), size = 5,
                 alpha = 0.5) +
      ggtitle("Perth") +
      theme(plot.title = element_text(size=10, face="bold", vjust=2, hjust = 0.5)) +
      theme(axis.title = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())

# Function to share legend
# https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, 
                                       position = c("bottom", "right")) {
      
      plots <- list(...)
      position <- match.arg(position)
      g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
      legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
      lheight <- sum(legend$height)
      lwidth <- sum(legend$width)
      gl <- lapply(plots, function(x) x + theme(legend.position="none"))
      gl <- c(gl, ncol = ncol, nrow = nrow)
      
      combined <- switch(position,
                         "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                                legend,
                                                ncol = 1,
                                                heights = unit.c(unit(1, "npc") - lheight, lheight)),
                         "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                               legend,
                                               ncol = 2,
                                               widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
      grid.newpage()
      grid.draw(combined)
      
}

grid_arrange_shared_legend(s_plot, m_plot, b_plot, a_plot, p_plot, ncol = 3, 
                           nrow = 2)

#grid.arrange(s_plot, m_plot, b_plot, a_plot, p_plot,ncol=3, 
#             nrow=2, top=textGrob("Title", gp=gpar(fontsize=15)))


#### Deawing Cricles------------------------------------------------------------
# http://gis.stackexchange.com/questions/119736/ggmap-create-circle-symbol-where-radius-represents-distance-miles-or-km

# Get map of sydney
syd <- gmap("Sydney,Australia", zoom = 10, scale = 2)
plot(syd)

# Get data fram of locations
d <- data.frame(lat = locations$lat, lon = locations$lon)

# convert to flat coordinants
coordinates(d) <- ~ lon + lat
projection(d) <- "+init=epsg:4326"

# transform map ot falt coordinants
d_mrc <- spTransform(d, CRS = CRS(projection(syd)))

# Buffer creation
d_mrc_bff <- gBuffer(d_mrc, width = 5000)

# Plot chart
plot(syd)
plot(d_mrc_bff, col =  alpha("blue", .35), add = TRUE)
points(d_mrc, cex = 2, pch = 20)