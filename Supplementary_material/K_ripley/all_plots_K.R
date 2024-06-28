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
  read_file <- read.table(here::here(x), skip = 8) %>%
    rename(km = V1, L = V2, UCL = V3, LCL =  V4)
    
  })

# a graph for each species
(A <- ggplot(load$Boidae, aes(x = km)) +
  geom_line(aes(y = UCL), color = "grey50", linewidth = 1) +  
  geom_line(aes(y = LCL), color = "grey50", linewidth = 1) +  
  geom_line(aes(y = L), color = "black", linewidth = 1) + 
  scale_x_continuous(expand = c(0,0.5)) +
  labs(x = "radius (km)", y = "K-statistics", title = "A) Boidae") +  
  theme(panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(fill = NA, colour = "grey20"),
        plot.title = ggtext::element_markdown(),
        axis.title = element_text(size = 12),
        axis.title.x = element_text(colour = "white"),
        axis.text = element_text(size = 10)))

(B <- ggplot(load$Cinsidiosus, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = L), color = "black", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "radius (km)", y = "K-statistics", title = "B) *Coendou insidiosus*") +  
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
    geom_line(aes(y = L), color = "black", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "radius (km)", y = "K-statistics", title = "C) *Cerdocyon thous*") +  
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
    geom_line(aes(y = L), color = "black", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "radius (km)", y = "K-statistics", title = "D) Dasypodidae") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(),
          axis.title = element_text(size = 12),
          axis.title.x = element_text(colour = "white"),
          axis.text = element_text(size = 10)))

(E <- ggplot(load$Daurita, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = L), color = "black", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "radius (km)", y = "K-statistics", title = "E) *Didelphis aurita*") +  
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
    geom_line(aes(y = L), color = "black", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "radius (km)", y = "K-statistics", title = "F) *Hydrochoerus hydrochaeris*") +  
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
    geom_line(aes(y = L), color = "black", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "radius (km)", y = "K-statistics", title = "G) *Procyon cancrivorus*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(),
          axis.title = element_text(size = 12),
          axis.title.x = element_text(colour = "white"),
          axis.text = element_text(size = 10)))

(H <- ggplot(load$Sbrasiliensis, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = L), color = "black", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "radius (km)", y = "K-statistics", title = "H) *Sylvilagus brasiliensis*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(),
          axis.title = element_text(size = 12),
          axis.title.y = element_text(colour = "white"),
          axis.title.x = element_text(colour = "white"),
          axis.text = element_text(size = 10)))

(I <- ggplot(load$Smerianae, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = L), color = "black", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "radius (km)", y = "K-statistics", title = "I) *Salvator merianae*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(),
          axis.title = element_text(size = 12),
          axis.title.y = element_text(colour = "white"),
          axis.text = element_text(size = 10)))

(J <- ggplot(load$Ttetradactyla, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = L), color = "black", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "radius (km)", y = "K-statistics", title = "J) *Tamandua tetradactyla*") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)))

(K <- ggplot(load$Ttetradactyla, aes(x = km)) +
    geom_line(aes(y = UCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = LCL), color = "grey50", linewidth = 1) +  
    geom_line(aes(y = L), color = "black", linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    labs(x = "radius (km)", y = "K-statistics", title = "K) Potential Users") +  
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          plot.title = ggtext::element_markdown(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)))
# legend
(plot_legend <- ggplot(load$PotentialUsers, aes(x = km)) +
    geom_line(aes(y = UCL, color = "CI"), linewidth = 1) +  
    geom_line(aes(y = LCL, color = "CI"), linewidth = 1) +  
    geom_line(aes(y = L, color = "L"),  linewidth = 1) + 
    scale_x_continuous(expand = c(0,0.5)) +
    scale_color_manual(name = "Legend",
                       limits = c("L", "CI"),
                       values = c("CI" = "grey50", "L" = "black"),
                       labels = c("K-statistics", "Confidence interval 95%")) +
    labs(x = "radius (km)", y = "K-statistics", title = "K) Potential Users") +  
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
panel <- grid.arrange(A, B, C,
             D, E, F,
             G, H, I,
             J, K, legend,
             ncol = 3)

ggsave(panel, filename = here::here("K_panel.png"), 
       dpi = 300, width = 3509, height = 2245, unit = "px") 
