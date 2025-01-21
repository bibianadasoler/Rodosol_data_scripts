library(dplyr)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(magick)


# loading files needed to create graphs
txts <- list.files(path = here::here("Hotspot"), 
                   pattern = "^\\w.+txt$")
names <- txts %>%
  str_split(., "/|\\.",
            simplify = TRUE) %>%
  as_tibble() %>%
  pull(V1) 

files <- txts %>%
  set_names(., names)

load <- map(files, function(x){ 
  read_file <- read.table(here::here("Hotspot", x), skip = 19) %>%
    rename(km = V1, HS = V5, UCL = V6, LCL =  V7) %>%
    dplyr::select(km, HS, UCL, LCL)
  
})

Boi <- image_read(here::here("Hotspots_location", "silhouette", "boi.png"))
Ci <- image_read(here::here("Hotspots_location","silhouette", "ci.png")) 
Ct <- image_read(here::here("Hotspots_location","silhouette", "ct.png")) %>% image_flop()
Da <- image_read(here::here("Hotspots_location","silhouette", "da.png"))
Dasy <- image_read(here::here("Hotspots_location","silhouette", "dasy.png")) %>% image_flop()
Hh <- image_read(here::here("Hotspots_location","silhouette", "hh.png"))
Pc <- image_read(here::here("Hotspots_location","silhouette", "pc.png"))
Tt <- image_read(here::here("Hotspots_location","silhouette", "tt.png")) %>% image_flop()

# a graph for each species
(A <- ggplot(load$AllSpecies, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = HS), color = "red", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "", y = "Roadkill intensity", title = "A) All species (2004 - 2017)") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 10)))

(B <- ggplot(load$AllSpecies_T1, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = HS), color = "red", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "", y = "", title = "B) All species (2004 - 2007)") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)))

(C <- ggplot(load$AllSpecies_T2, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = HS), color = "red", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "", y = "", title = "C) All species (2008 - 2012)") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)))

(D <- ggplot(load$AllSpecies_T3, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = HS), color = "red", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "", y = "", title = "D) All species (2013 - 2017)") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)))

(E <- ggplot(load$Boidae, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = HS), color = "red", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "", y = "Roadkill intensity", title = "E) Boidae") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 10)) +
    annotation_raster(Boi, xmin = 1.2, xmax = 10, ymin = 15, ymax = 17.5))

(F <- ggplot(load$Cthous, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = HS), color = "red", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "", y = "", title = "F) *Cerdocyon thous*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)) +
    annotation_raster(Ct, xmin = 1, xmax = 10, ymin = 6.5, ymax = 8))


(G <- ggplot(load$Cinsidiosus, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = HS), color = "red", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "", y = "", title = "G) *Coendou insidiosus*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)) +
    annotation_raster(Ci, xmin = 1, xmax = 11, ymin = 7.3, ymax = 9))

(H <- ggplot(load$Dasypodidae, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = HS), color = "red", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "", y = "", title = "H) Dasypodidae") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)) +
    annotation_raster(Dasy, xmin = 1, xmax = 11, ymin = 2, ymax = 2.3))

(I <- ggplot(load$Daurita, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = HS), color = "red", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "Road distance (km)", y = "Roadkill intensity", title = "I) *Didelphis aurita*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 10)) +
    annotation_raster(Da, xmin = 1, xmax = 9, ymin = 36.7, ymax = 44.7))

(J <- ggplot(load$Hhydrochaeris, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = HS), color = "red", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "Road distance (km)", y = "", title = "J) *Hydrochoerus hydrochaeris*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 10)) +
    annotation_raster(Hh, xmin = 1, xmax = 9, ymin = 3.7, ymax = 4.5))

(K <- ggplot(load$Pcancrivorus, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = HS), color = "red", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "Road distance (km)", y = "", title = "K) *Procyon cancrivorus*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 10)) +
    annotation_raster(Pc, xmin = 1, xmax = 9, ymin = 3, ymax = 3.4))

(L <- ggplot(load$Ttetradactyla, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = HS), color = "red", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "Road distance (km)", y = "", title = "L) *Tamandua tetradactyla*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 10)) +
    annotation_raster(Tt, xmin = 1, xmax = 10, ymin = 2.9, ymax = 3.3))



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
panel <- grid.arrange(arrangeGrob(A, B, C, D, 
                                  E, F, G, H,
                                  I, J, K, L,
                                  nrow = 3),
                      legend, heights = c(9.5, 0.5))

ggsave(panel, filename = here::here("Hotspot", "hotspot_panel.png"), 
       dpi = 300, width = 4000, height = 2245, unit = "px") 
