library(dplyr)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(gridExtra)


# loading files needed to create graphs
txts <- list.files(path = here::here("K_ripley"), 
           pattern = "^\\w.+txt$")
names <- txts %>%
  str_split(., "/|\\.",
            simplify = TRUE) %>%
  as_tibble() %>%
  pull(V1) 

files <- txts %>%
  set_names(., names)

load <- map(files, function(x){ 
  read_file <- read.table(here::here("K_ripley", x), skip = 22) %>%
    rename(km = V1, L = V2, UCL = V3, LCL =  V4)
    
  })

Boi <- image_read(here::here("Hotspots_location", "silhouette", "boi.png"))
Ci <- image_read(here::here("Hotspots_location","silhouette", "ci.png")) 
Ct <- image_read(here::here("Hotspots_location","silhouette", "ct.png")) %>% image_flop()
Da <- image_read(here::here("Hotspots_location","silhouette", "da.png"))
Dasy <- image_read(here::here("Hotspots_location","silhouette", "dasy.png")) %>% image_flop()
Hh <- image_read(here::here("Hotspots_location","silhouette", "hh.png"))
Pc <- image_read(here::here("Hotspots_location","silhouette", "pc.png"))
Tt <- image_read(here::here("Hotspots_location","silhouette", "tt.png")) %>% image_flop()
Sm <- image_read(here::here("Hotspots_location","silhouette", "sm.jpg"))
Sb <- image_read(here::here("Hotspots_location","silhouette", "sb.png")) %>% image_flop()

# a graph for each species
(A <- ggplot(load$AllSpecies, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = L), color = "black", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "", y = "L-statistics", title = "A) All species (2004 - 2017)") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 10)) 
  )

(B <- ggplot(load$AllSpecies_T1, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = L), color = "black", linewidth = 0.5) + 
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
    geom_line(aes(y = L), color = "black", linewidth = 0.5) + 
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
    geom_line(aes(y = L), color = "black", linewidth = 0.5) + 
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
  geom_line(aes(y = L), color = "black", linewidth = 0.5) + 
  scale_x_continuous(expand = c(0,0.5)) +
  labs(x = "", y = "L-statistics", title = "E) Boidae") +  
  theme(panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(fill = NA, colour = "grey20"),
        plot.title = ggtext::element_markdown(size = 10),
        axis.title = element_text(size = 11),
        axis.title.x = element_text(colour = "white"),
        axis.text = element_text(size = 10)) +
    annotation_raster(Boi, xmin = 57, xmax = 66, ymin = 7600, ymax = 9200))

(F <- ggplot(load$Cthous, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = L), color = "black", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "", y = "", title = "F) *Cerdocyon thous*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 10)) +
    annotation_raster(Ct, xmin = 56, xmax = 66, ymin = 5700, ymax = 7400))

(G <- ggplot(load$Cinsidiosus, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = L), color = "black", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "", y = "", title = "G) *Coendou insidiosus*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)) +
    annotation_raster(Ci, xmin = 56, xmax = 67, ymin = 7100, ymax = 8900))

(H <- ggplot(load$Dasypodidae, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = L), color = "black", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "", y = "", title = "H) Dasypodidae") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)) +
    annotation_raster(Dasy, xmin = 54, xmax = 67, ymin = 4800, ymax = 6000))

(I <- ggplot(load$Daurita, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = L), color = "black", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "", y = "L-statistics", title = "I) *Didelphis aurita*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 10)) +
    annotation_raster(Da, xmin = 56, xmax = 66, ymin = 6500, ymax = 8500))

(J <- ggplot(load$Hhydrochaeris, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = L), color = "black", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "", y = "", title = "J) *Hydrochoerus hydrochaeris*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)) +
    annotation_raster(Hh, xmin = 57, xmax = 66, ymin = 7000, ymax = 9600))

(K <- ggplot(load$Pcancrivorus, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = L), color = "black", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "Radius (km)", y = "", title = "K) *Procyon cancrivorus*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 10)) +
    annotation_raster(Pc, xmin = 57, xmax = 66, ymin = 6200, ymax = 7800))

(L <- ggplot(load$Ttetradactyla, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = L), color = "black", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "Radius (km)", y = "", title = "L) *Tamandua tetradactyla*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 10)) +
    annotation_raster(Tt, xmin = 56, xmax = 66, ymin = 5900, ymax = 7600))

(M <- ggplot(load$Smerianae, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = L), color = "black", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "Radius (km)", y = "L-statistics", title = "M) *Salvator merianae*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
            axis.title = element_text(size = 11),
          axis.text = element_text(size = 10)) +
    annotation_raster(Sm, xmin = 57, xmax = 66, ymin = 15600, ymax = 19000))

(N <- ggplot(load$Sbrasiliensis, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 0.5) +  
    geom_line(aes(y = L), color = "black", linewidth = 0.5) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "Radius (km)", y = "", title = "N) *Sylvilagus brasiliensis*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(size = 10),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 10)) +
    annotation_raster(Sb, xmin = 57, xmax = 66, ymin = 11500, ymax = 14600))


# legend
(plot_legend <- ggplot(load$AllSpecies, aes(x = km)) +
    geom_line(aes(y = UCL, color = "CI"), linewidth = 1) +  
    geom_line(aes(y = LCL, color = "CI"), linewidth = 1) +  
    geom_line(aes(y = L, color = "L"),  linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    scale_color_manual(name = "Legend",
                       limits = c("L", "CI"),
                       values = c("CI" = "grey50", "L" = "black"),
                       labels = c("L-statistics", "Confidence interval 95%")) +
    labs(x = "radius (km)", y = "L-statistics", title = "K) Potential Users") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
         plot.title = ggtext::element_markdown(),
         axis.title = element_text(size = 12),
         axis.title.y = element_text(colour = "white"),
         axis.text = element_text(size = 10),
         legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
         legend.key = element_rect(fill = "white")))


legend <- get_legend(plot_legend)

# combining graphs in a panel
(panel <- grid.arrange(A, B, C, D,
                      E, F, G, H,
                      I, J, K, L,
                      M, N, legend,
             ncol = 4) )

ggsave(panel, filename = here::here("K_ripley", "K_panel.png"), 
       dpi = 300, width = 4000, height = 2245, unit = "px") 
