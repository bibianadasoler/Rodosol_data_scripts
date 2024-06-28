library(dplyr)
library(magick)
library(imager)
library(ggplot2)
library(ggimage)
library(ggrepel)
library(here)

# loading and organizing data
data <- read.csv(here::here("records.csv"), sep = ";") %>%
  dplyr::mutate(roadkills = roadkill_km45 + roadkill_km50 + roadkill_km59,
                use = crossings_km45 + crossings_km50 + crossings_km59,
                image = c('da.png', 'boi.png', 'ct.png', 'ci.png', 'tt.png', #white.png for species that are overlaped in the graph
                          'pc.png', 'dasy.png', 'hh.png', 'sb.png', 'sm.png', 
                          'eb.png', 'cp.png', 'll.png')) %>%
  dplyr::select(species, roadkills,  use,  image)

# function to collapse y-axis
library(scales)
squish_trans <- function(from, to, factor) {
  
  trans <- function(x) {
    
    if (any(is.na(x))) return(x)
    
    # get indices for the relevant regions
    isq <- x > from & x < to
    ito <- x >= to
    
    # apply transformation
    x[isq] <- from + (x[isq] - from)/factor
    x[ito] <- from + (to - from)/factor + (x[ito] - to)
    
    return(x)
  }
  
  inv <- function(x) {
    
    if (any(is.na(x))) return(x)
    
    # get indices for the relevant regions
    isq <- x > from & x < from + (to - from)/factor
    ito <- x >= from + (to - from)/factor
    
    # apply transformation
    x[isq] <- from + (x[isq] - from) * factor
    x[ito] <- to + (x[ito] - (from + (to - from)/factor))
    
    return(x)
  }
  
  # return the transformation
  return(trans_new("squished", trans, inv))
}

(quadrant_graph <- ggplot(data, aes(x = roadkills, y = use, fill = species)) +
    # collapse the y-axis
    scale_y_continuous(trans = squish_trans(1140,1160, 0.1),
                       breaks = c(0, 500, 1000, 1140, 1160, 1500, 2200)) +
    # # add boxes of priority quadrants
    annotate("rect", xmin = (median(data$roadkills) + 0.15), xmax = max(data$roadkills), 
             ymin = 0, ymax = (median(data$use) - 6), alpha = 0, colour  = "red") +
    annotate("rect", xmin = median(data$roadkills), xmax = max(data$roadkills), 
             ymin = (median(data$use) + 6), ymax = max(data$use), alpha = 0, colour = "blue") +
    annotate("rect", xmin = 0, xmax = (median(data$roadkills) - 0.15), 
             ymin = (median(data$use) + 6), ymax = max(data$use), alpha = 0, colour = "green3") +
    # median
    geom_hline(yintercept = median(data$use), color = "black", linewidth = 0.75) +
    geom_vline(xintercept = median(data$roadkills), color = "black", linewidth = 0.75) +
    # add images
    geom_image(aes(image = here::here("silhouette", image)), size = 0.05, asp = 1) +
    
    # add (x,y) point of each specie
    geom_point(color = "red") +
    # theme 
    theme(panel.background = element_rect(fill = "white"),
          axis.text = element_text(size = 10, colour = "gray10"),
          axis.line = element_line(linewidth = 0.75, colour = "gray10"),
          axis.title = element_text(size = 15),
          legend.position = "none") +
    # labs
    labs(x = "Roadkill records",  y = "Crossing records")
)

ggsave(quadrant_graph, filename = here::here("quadrant_graph.png"), 
       dpi = 300, width = 3000, height = 2200, unit = "px")
