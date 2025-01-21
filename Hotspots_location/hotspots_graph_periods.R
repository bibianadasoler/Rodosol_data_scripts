library(dplyr)
library(magick)
library(ggplot2)
library(ggtext)

hotspots_files <- list.files(#path = "C:/Users/bibia/OneDrive/Doutorado/Rodosol/Rodosol_data_scripts/Supplementary_material/Hotspot",
                             path = "/Users/bibianaterradasolerdeoliveira/Library/CloudStorage/OneDrive-Personal/Doutorado/Rodosol/Rodosol_data_scripts/Supplementary_material/Hotspot",
                             pattern = "\\.csv$",
                             full.names = TRUE, 
                             all.files = FALSE) 

# Function to read and process each file
hotspots_filter <- function(file_path) {  
  read.csv(file_path) %>%
    mutate(specie = gsub(".*/Hotspot/(.*)\\.csv", "\\1", file_path)) %>%
    rename(hotspot = `HS.UCL`,
           km_centroid = km_round) %>%
    filter(hotspot > 0) %>%
    mutate(km_min = km_centroid - 0.339,
           km_max = km_centroid + 0.338) %>%
    dplyr::select(specie, km_centroid, km_min, km_max)
}

# Combining information of hotspots km for all species
hotspots_km <- lapply(hotspots_files, hotspots_filter) %>%
  do.call(rbind, .)

hotspots_km <- hotspots_km %>%
  filter(specie %in% c("PotentialUsers", "PotentialUsers_T1", "PotentialUsers_T2", "PotentialUsers_T3")) %>%
  arrange(factor(specie, levels = c("PotentialUsers", "PotentialUsers_T1", "PotentialUsers_T2", "PotentialUsers_T3")))
table(hotspots_km$specie)

# criando o eixo y para as especies, ordem de cima para baixo (considerando numero de kms que sao hotspots)
ally <- rep(0.35, 24) # potential
allT1y <- rep(0.3, 13) # potential
allT2y <- rep(0.25, 19) # potential
allT3y <- rep(0.2, 18) # potential

y <- c(ally, allT1y, allT2y, allT3y) #juntando as infos do y das tres espécies num vetor (NOTAR: na mesma ordem que ja aparecem na tabela dos dados)

#definindo a altura do retangulo to hostspot atraves do ymin e ymax
hotspots_km$y_min <- y - 0.01 #calculando o inicio da altura
hotspots_km$y_max <- y + 0.01 #calculano o fim da altura

y_mapping <- c('PotentialUsers_T3' = 0.2, 
               'PotentialUsers_T2' = 0.25, 
               'PotentialUsers_T1' = 0.3, 
               'PotentialUsers' = 0.35)

# Adicionando uma coluna numérica ao data frame para o eixo y
hotspots_km <- hotspots_km %>%
  mutate(y_numeric = y_mapping[specie])

# Criando o gráfico base com os hotspots no x e as espécies no y
(hotspots_graph <- ggplot(hotspots_km, aes(x = km_centroid, y = y_numeric)) +
    labs(x = "Road km", y = "") +
    annotate("rect", xmin = 43.8, xmax = 46.7, ymin = 0.15, ymax = 0.39,
             fill = "grey80") + # retângulo do cluster km 45
    annotate("rect", xmin = 49.8, xmax = 50.2, ymin = 0.15, ymax = 0.39,
             fill = "grey80") + # retângulo do cluster km 50
    annotate("rect", xmin = 59.3, xmax = 60.2, ymin = 0.15, ymax = 0.39,
             fill = "grey80") + # retângulo do cluster km 59
    geom_richtext(label = "<span style='font-size:9pt; color:black'>Cluster<br>45", x = 44.90, y = 0.38) +
    geom_richtext(label = "<span style='font-size:9pt; color:black'>Cluster<br>50", x = 50.15, y = 0.38) +
    geom_richtext(label = "<span style='font-size:9pt; color:black'>Cluster<br>59", x = 59.85, y = 0.38) +
    # city tags
    geom_richtext(label = "<span style='font-size:11pt; color:black'>Vitória", x = 0.5, y = 0.17) +
    geom_richtext(label = "<span style='font-size:11pt; color:black'>Guarapari", x = 67.4, y = 0.17) + 
    # species road line
    annotate("segment", x = 0, xend = 68, y = 0.2, yend = 0.2, colour = "grey85") +
    annotate("segment", x = 0, xend = 68, y = 0.25, yend = 0.25, colour = "grey85") +
    annotate("segment", x = 0, xend = 68, y = 0.3, yend = 0.3, colour = "grey85") +
    annotate("segment", x = 0, xend = 68, y = 0.35, yend = 0.35, colour = "grey85") +
    
    # retângulos dos hotspots usando os x e y min e max criados no início
    annotate("rect", xmin = hotspots_km$km_min, xmax = hotspots_km$km_max,
             ymin = hotspots_km$y_min, ymax = hotspots_km$y_max, fill = "black") +
    
    # Definir a escala do x
    scale_x_continuous(limits = c(0,69), 
                       breaks = seq(0, 68, by = 4)) + 
    
    # Definir a escala do y como contínua com labels personalizados
    scale_y_continuous(
      breaks = c(0.2, 0.25, 0.3, 0.35), 
      labels = c("Timeframe 3 (2013 - 2017)", "Timeframe 2 (2008 - 2012)", "Timeframe 1 (2004 - 2007)", "Full survey time (2004 - 2017)"),
      limits = c(0.15, 0.4),
      expand = expansion(add = c(0, 0)) ) +
    
    theme(panel.background = element_rect(fill = "white"),
           axis.line.x = element_line(linewidth = 3, colour = "black"),
           axis.text = element_text(size = 11, colour = "black"),
          axis.text.x = element_text(color = "black"),
          axis.ticks.x = element_line(color = "black"),
          axis.ticks.y = element_blank(),
          axis.ticks.length.x = unit(.25, "cm"),
          legend.position = "none",
          axis.title.x = element_text(size = 12, color = "black", margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(size = 12, color = "black"))
          )

# save
ggsave(hotspots_graph, filename = here::here("Hotspots_location", "hotspots_graph_periods.png"), 
       dpi = 300, width = 3000, height = 1400, unit = "px")

