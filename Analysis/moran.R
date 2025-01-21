####

# Carregar os pacotes
library(spdep)
library(sf)
library(dplyr)

# Converter os dados para um objeto 'sf'
atrop <- read.table(here::here("Analysis", "atrops.txt"), h = T)
head(atrop)
coord_km <- read.table(here::here("Analysis", "coords_km.txt"), h = T)
coord_km  <- coord_km %>%
  dplyr::filter(km >= 10)
head(coord_km)

coord_km_sf <- st_as_sf(coord_km, coords = c("x", "y"), crs = 32724)  
coord_latlong_sf <- st_transform(coord_km_sf, crs = 4326)

atrop_km <- atrop %>% 
  filter(km >= 10) %>%
  group_by(km) %>%
  summarise(atrop_n = n()) %>%
  dplyr::select(km, atrop_n) %>%
  left_join(., coord_km, by = "km") %>%
  as.data.frame()

km_seq <- seq(10, 67.5, by = 0.5)
km_df <- data.frame(km = km_seq)
atrop_km_complete <- merge(km_df, atrop_km, by = "km", all.x = TRUE)
atrop_km_complete$atrop_n[is.na(atrop_km_complete$atrop_n)] <- 0
atrop_km_complete <- atrop_km_complete %>%
  left_join(., coord_km, by = "km") %>%
  dplyr::select(km, atrop_n, x.y, y.y) %>%
  rename(x = x.y,
         y = y.y)
head(atrop_km_complete)

atrop_km_sf <- st_as_sf(atrop_km_complete, coords = c("x", "y"), crs = 32724)

# Calcule a matriz de distância entre os pontos
dist_matrix <- st_distance(coord_latlong_sf)

# Remover unidades da matriz de distância
dist_matrix_numeric <- units::drop_units(dist_matrix)

# Criar o Minimum Spanning Tree (MST)
library(igraph)
mst <- mst(graph_from_adjacency_matrix(dist_matrix_numeric, mode = "undirected", weighted = TRUE))

# Extraia as distâncias das arestas do MST
mst_edges <- E(mst)$weight

# Definir a distância de truncagem como a maior aresta do MST
dist_cutoff_mst <- max(mst_edges)
print(paste("Distância de truncagem do MST:", dist_cutoff_mst))



# Criar a matriz de adjacência com base na distância de corte
adjacency_matrix <- ifelse(dist_matrix_numeric <= dist_cutoff_mst, 1, 0)
diag(adjacency_matrix) <- 0  # Remover auto-conexões

# Exibir a matriz de adjacência
print(adjacency_matrix)

head(adjacency_matrix)
# Criar o objeto de vizinhança a partir da matriz de adjacência
nb <- mat2listw(adjacency_matrix, style = "W")


# Calcular o Índice de Moran
moran_test <- moran.test(atrop_km_sf$atrop_n, nb)
print(moran_test)

