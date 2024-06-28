library(dplyr)
library(magick)
library(ggplot2)
library(ggtext)

hotspots_files <- list.files(path = "C:/Users/bibia/OneDrive/Doutorado/Rodosol/Supplementary_material/Hotspot",
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
    select(specie, km_centroid, km_min, km_max)
}

# Combining information of hotspots km for all species
hotspots_km <- lapply(hotspots_files, hotspots_filter) %>%
  do.call(rbind, .)

hotspots_km <- hotspots_km %>%
  tibble::add_row(specie = c('Llongicaudis', 'Cpaca', 'Ebarbara', 'Smerianae', 'Sbrasiliensis'))


# criando o eixo y para as especies, ordem de cima para baixo (considerando numero de kms que sao hotspots)
ally <- rep(14, 24) # potential
ciy <- rep(13, 13) # coendou  
boiy <- rep(12, 14) # boidae
day <- rep(11, 16)  # didelphis
cty <- rep(10, 13) # cerdocyon
pcy <- rep(9, 7) # procyon
dasyy <- rep(8, 4) # dasypodidae
tty <- rep(7, 7) # tamandua
hhy <- rep(6,7) # hydrochoerus
lly <- NA
cpy <- NA
eby <- NA
smy <- NA
sby <- NA


y <- c(boiy, ciy, cty, dasyy, day, hhy, pcy, ally, tty, lly, cpy, eby, smy, sby) #juntando as infos do y das tres espécies num vetor (NOTAR: na mesma ordem que ja aparecem na tabela dos dados)

#definindo a altura do retangulo to hostspot atraves do ymin e ymax
hotspots_km$y_min <- y - 0.25 #calculando o inicio da altura
hotspots_km$y_max <- y + 0.25 #calculano o fim da altura


#definindo a ordem que as espécies vão aparecer no grafico
# o primeiro fica mais embaixo no grafico e o ultimo mais em cima
level_order <- c('Llongicaudis',
                 'Cpaca',
                 'Ebarbara',
                 'Smerianae',
                 'Sbrasiliensis',
                 'Hhydrochaeris',
                 'Ttetradactyla', 
                 'Dasypodidae', 
                 'Pcancrivorus', 
                 'Cthous',
                 'Daurita',
                 'Boidae', 
                 'Cinsidiosus',
                 'PotentialUsers')

#### função hpline ----
geom_hpline <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHpline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}


GeomHpline <- ggproto("GeomHpline", GeomSegment,
                      required_aes = c("x", "y"),
                      non_missing_aes = c("size", "colour", "linetype", "width"),
                      default_aes = aes(
                        width = 0.5, colour = "black", linewidth = 2, linetype = 1,
                        alpha = NA
                      ),
                      
                      draw_panel = function(self, data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                                            lineend = "butt", linejoin = "round", na.rm = FALSE) {
                        data <- mutate(data, x = x - width/2, xend = x + width, yend = y)
                        ggproto_parent(GeomSegment, self)$draw_panel(
                          data, panel_params, coord, arrow = arrow, arrow.fill = arrow.fill,
                          lineend = lineend, linejoin = linejoin, na.rm = na.rm
                        )
                      }
)

#### fim função hpline ----

# loading roadkill km records
roadkills_km <- read.table(here::here("roadkill_km.txt"), h = T)

# loading images
Boi <- image_read(here::here("silhouette", "boi.png"))
Ci <- image_read(here::here("silhouette", "ci.png")) 
Ct <- image_read(here::here("silhouette", "ct.png")) %>% image_flop()
Da <- image_read(here::here("silhouette", "da.png"))
Dasy <- image_read(here::here("silhouette", "dasy.png")) %>% image_flop()
Hh <- image_read(here::here("silhouette", "hh.png"))
Pc <- image_read(here::here("silhouette", "pc.png"))
Tt <- image_read(here::here("silhouette", "tt.png")) %>% image_flop()
Pu <- image_read(here::here("silhouette", "pu.png"))
Ll <- image_read(here::here("silhouette", "ll.png")) %>% image_flop()
Sm <- image_read(here::here("silhouette", "sm.jpg"))
Sb <- image_read(here::here("silhouette", "sb.png")) %>% image_flop()
Eb <- image_read(here::here("silhouette", "eb.png")) %>% image_flop()
Cp <- image_read(here::here("silhouette", "cp.png")) %>% image_flop()

#criando o grafico base com os hots no x e as especies no y 
(rodosol <- ggplot(hotspots_km, aes(x = km_centroid, y = factor(specie, level = level_order))) +
   labs(x = "Road km", y = "")
)

(hotspots_graph <- rodosol + 
    geom_hpline(aes(colour = "red")) + 
    
    # CLUSTERS
    annotate("rect", xmin = 43.8, xmax = 46.7, ymin = -1.5, ymax = 15.1,
            fill = "grey80") + #retangulo do cluster km 45
    annotate("rect", xmin = 49.8, xmax = 50.2, ymin = -1.5, ymax = 15.1,
             fill = "grey80") + #retangulo do cluster km 50
    annotate("rect", xmin = 59.3, xmax = 60.2, ymin = -1.5, ymax = 15.1,
             fill = "grey80") + #retangulo do cluster km 59
    geom_richtext(label = "<span style='font-size:8pt; color:black'>Cluster<br>45", x = 45.1, y = 15.1) +
    geom_richtext(label = "<span style='font-size:8pt; color:black'>Cluster<br>50", x = 50.1, y = 15.1) +
    geom_richtext(label = "<span style='font-size:8pt; color:black'>Cluster<br>59", x = 59.9, y = 15.1) +
    
    #retangulos dos hotspots usando os x e y min e max criados no inicio
    annotate("rect", xmin = hotspots_km$km_min, xmax = hotspots_km$km_max, 
             ymin = hotspots_km$y_min, ymax = hotspots_km$y_max, fill = "black") + 
    
    # species road line
    annotate("segment", x = 0, xend = 68, y = 1, yend = 1, colour = "grey85") +
    annotate("segment", x = 0, xend = 68, y = 2, yend = 2, colour = "grey85") +
    annotate("segment", x = 0, xend = 68, y = 3, yend = 3, colour = "grey85") +
    annotate("segment", x = 0, xend = 68, y = 4, yend = 4, colour = "grey85") +
    annotate("segment", x = 0, xend = 68, y = 5, yend = 5, colour = "grey85") +
    annotate("segment", x = 0, xend = 68, y = 6, yend = 6, colour = "grey85") +
    annotate("segment", x = 0, xend = 68, y = 7, yend = 7, colour = "grey85") +
    annotate("segment", x = 0, xend = 68, y = 8, yend = 8, colour = "grey85") +
    annotate("segment", x = 0, xend = 68, y = 9, yend = 9, colour = "grey85") +
    annotate("segment", x = 0, xend = 68, y = 10, yend = 10, colour = "grey85") +
    annotate("segment", x = 0, xend = 68, y = 11, yend = 11, colour = "grey85") +
    annotate("segment", x = 0, xend = 68, y = 12, yend = 12, colour = "grey85") +
    annotate("segment", x = 0, xend = 68, y = 13, yend = 13, colour = "grey85") +
    annotate("segment", x = 0, xend = 68, y = 14, yend = 14, colour = "grey85") +
    
    # roadkill records
    geom_point(data = roadkills_km, aes(x = km, y = specie), color = "brown3", size = 1.2, alpha = 0.5) +
    
    # city tags
    geom_richtext(label = "<span style='font-size:10pt; color:black'>Vitória", x = 0, y = - 0.5) +
    geom_richtext(label = "<span style='font-size:10pt; color:black'>Guarapari", x = 67.4, y = -0.5) + 
    
    #definir a escala do x
    scale_x_continuous(limits = c(0,70), breaks = seq(0, 68, by = 4)) + 
    
    #definir a escala do y e colocar os nomes em italico
    scale_y_discrete(labels = c(expression(italic("Lontra longicaudis")),
                                expression(italic("Cuniculus paca")),
                                expression(italic("Eira barbara")),
                                expression(italic("Salvator merianae")),
                                expression(italic("Sylvilagus brasiliensis")),
                                expression(italic("Hydrochoerus hydrochaeris")),
                                expression(italic("Tamandua tetradactyla")), 
                                "Dasypodidae",
                                expression(italic("Procyon cancrivorus")),
                                expression(italic("Cerdocyon thous")),
                                expression(italic("Didelphis aurita")),
                                'Boidae', 
                                expression(italic("Coendou insidiosus")),
                                "All potential users"),
                     position = "right", expand = c(0,2.5)) + 
    
    theme(panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(linewidth = 3, colour = "black"),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(face = "bold"), 
          axis.text.y = element_text(face = "italic"), 
          axis.ticks.y = element_blank(),
          axis.ticks.length.x = unit(.25, "cm"),
          legend.position = "none",
          axis.title.x = element_text(size = 10, margin = margin(t = 20, r = 0, b = 0, l = 0))) + 
    
    
    # species silhoutte
    annotation_raster(Ll, xmin = 68.6, xmax = 73.3, ymin = 0.6, ymax = 1.2) +
    annotation_raster(Cp, xmin = 69, xmax = 73.4, ymin = 1.6, ymax = 2.3) +
    annotation_raster(Eb, xmin = 69, xmax = 73.4, ymin = 2.6, ymax = 3.3) +
    annotation_raster(Sm, xmin = 68.5, xmax = 73, ymin = 3.6, ymax = 4.5) +
    annotation_raster(Sb, xmin = 69.2, xmax = 73, ymin = 4.6, ymax = 5.4) +
    annotation_raster(Hh, xmin = 69, xmax = 73.4, ymin = 5.6, ymax = 6.4) +
    annotation_raster(Tt, xmin = 68.7, xmax = 73.3, ymin = 6.8, ymax = 7.4) +
    annotation_raster(Dasy, xmin = 68.8, xmax = 73.5, ymin = 7.8, ymax = 8.2) +
    annotation_raster(Pc, xmin = 68.7, xmax = 73.2, ymin = 8.7, ymax = 9.2) +
    annotation_raster(Ct, xmin = 68.5, xmax = 73.1, ymin = 9.5, ymax = 10.3) +
    annotation_raster(Da, xmin = 69, xmax = 73.5, ymin = 10.5, ymax = 11.4) +
    annotation_raster(Boi, xmin = 69.3, xmax = 73.3, ymin = 11.8, ymax = 12.3) +
    annotation_raster(Ci, xmin = 68.7, xmax = 73.3, ymin = 12.7, ymax = 13.3) +
    annotation_raster(Pu, xmin = 69, xmax = 73.5, ymin = 13.5, ymax = 14.5) )

# save
ggsave(hotspots_graph, filename = here::here("hotspots_graph.png"), 
       dpi = 300, width = 3000, height = 2000, unit = "px")

