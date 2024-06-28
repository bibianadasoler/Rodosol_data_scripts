library(dplyr)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(gridExtra)


# loading files needed to create graphs
txts <- list.files(path = here::here(), 
                   pattern = "^\\w.+txt$")
names <- txts %>%
  str_split(., "/|\\.",
            simplify = TRUE) %>%
  as_tibble() %>%
  pull(V1) 

files <- txts %>%
  set_names(., names)

load <- map(files, function(x){ 
  read_file <- read.table(here::here(x), skip = 7) %>%
    rename(km = V1, HS = V5, UCL = V6, LCL =  V7) %>%
    select(km, HS, UCL, LCL)
  
})

# a graph for each species
(A <- ggplot(load$Boidae, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = HS), color = "red", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "Road distance (km)", y = "Roadkill intensity", title = "A) Boidae") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(),
          axis.title = element_text(size = 12),
          axis.title.x = element_text(colour = "white"),
          axis.text = element_text(size = 10)))

(B <- ggplot(load$Cinsidiosus, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = HS), color = "red", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "Road distance (km)", y = "Roadkill intensity", title = "B) *Coendou insidiosus*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(),
          axis.title = element_text(size = 12),
          axis.title.y = element_text(colour = "white"),
          axis.title.x = element_text(colour = "white"),
          axis.text = element_text(size = 10)))

(C <- ggplot(load$Cthous, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = HS), color = "red", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "Road distance (km)", y = "Roadkill intensity", title = "C) *Cerdocyon thous*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(),
          axis.title = element_text(size = 12),
          axis.title.y = element_text(colour = "white"),
          axis.title.x = element_text(colour = "white"),
          axis.text = element_text(size = 10)))

(D <- ggplot(load$Dasypodidae, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = HS), color = "red", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "Road distance (km)", y = "Roadkill intensity", title = "D) Dasypodidae") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(),
          axis.title = element_text(size = 12),
          axis.title.x = element_text(colour = "white"),
          axis.text = element_text(size = 10)))

(E <- ggplot(load$Daurita, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = HS), color = "red", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "Road distance (km)", y = "Roadkill intensity", title = "E) *Didelphis aurita*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(),
          axis.title = element_text(size = 12),
          axis.title.y = element_text(colour = "white"),
          axis.title.x = element_text(colour = "white"),
          axis.text = element_text(size = 10)))

(F <- ggplot(load$Hhydrochaeris, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = HS), color = "red", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "Road distance (km)", y = "Roadkill intensity", title = "F) *Hydrochoerus hydrochaeris*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(),
          axis.title = element_text(size = 12),
          axis.title.y = element_text(colour = "white"),
          axis.title.x = element_text(colour = "white"),
          axis.text = element_text(size = 10)))

(G <- ggplot(load$Pcancrivorus, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = HS), color = "red", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "Road distance (km)", y = "Roadkill intensity", title = "G) *Procyon cancrivorus*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)))

(H <- ggplot(load$Ttetradactyla, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = HS), color = "red", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "Road distance (km)", y = "Roadkill intensity", title = "H) *Tamandua tetradactyla*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)))

(I <- ggplot(load$PotentialUsers, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = HS), color = "red", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "Road distance (km)", y = "Roadkill intensity", title = "I) Potential Users") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)))

# legend
(plot_legend <- ggplot(load$Ttetradactyla, aes(x = km)) +
    geom_line(aes(y = UCL, color = "CI"), linewidth = 1) +  
    geom_line(aes(y = LCL, color = "CI"), linewidth = 1) +  
    geom_line(aes(y = HS, color = "HS"), linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    scale_color_manual(name = "Legend",
                       limits = c("HS", "CI"),
                       values = c("CI" = "grey50", "HS" = "red"),
                       labels = c("Roadkill intensity", "Confidence interval 95%")) +
    labs(x = "Road distance (km)", y = "Roadkill intensity", title = "H) *Tamandua tetradactyla*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.title = element_text(size = 14),
          legend.position = "bottom",
          legend.text = element_text(size = 14),
          legend.key = element_rect(fill = "white")))
legend <- get_legend(plot_legend)

# combining graphs in a panel
panel <- grid.arrange(arrangeGrob(A, B, C,
                                  D, E, F,
                                  G, H, I,
                                  nrow = 3),
                      legend, heights = c(9.5, 0.5))

ggsave(panel, filename = here::here("hotspot_panel.png"), 
       dpi = 300, width = 3509, height = 2245, unit = "px") 
