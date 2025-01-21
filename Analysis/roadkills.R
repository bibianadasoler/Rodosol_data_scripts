library(dplyr)
library(MASS)
library(performance)
library(car)
library(emmeans)
library(ggeffects)
library(lubridate)
library(ggplot2)
library(ggtext)
library(patchwork)
library(sf)
library(ggspatial)

# DATA -----
# loading roadkill data
data <- read.csv(here::here("Analysis", "roadkill_records.csv"), sep = ";", header = T) 
head(data)

# summarizing roadkills by period and segment
roadkills <- data %>%
  group_by(year, segment) %>%
  summarize(roadkills = n(), .groups = 'drop')

# clusters segments
clusters <- c(44, 44.5, 45, 45.5, 46, 46.5,
              50,
              59.5, 60)

# clusters segments side to exclude from controls
clusters_side <- c(43.5, 47,
                   49.5, 50.5,
                   59, 60.5)

# protected areas
ucs <- c(seq(11, 14, by = 0.5), # jacarenema
         seq(20, 27.5, by = 0.5), # lagoa grande 
         seq(28, 42, by = 0.5)) # setiba and paulo cesar

# filter segments with at least 1 roadkill in the first period as control
# all segments as controls
controls <- seq(11, 67.5, by = 0.5) # excluding all initial urban segments 
control_ALLsegments <- data %>%
  filter(segment %in% controls,
         !segment %in% clusters,
         !segment %in% clusters_side) %>%
  pull(segment) %>%
  unique() %>%
  sort()
control_ALLsegments <- sample(control_ALLsegments, size = 10, replace = F)

# visualizing segments
coords_km <- read.table(here::here("Analysis", "coords_km.txt"), h = T) %>% st_as_sf(., coords = c("x", "y"), crs = 32724)

coords_control_ALL <- coords_km %>%
  filter(km %in% control_ALLsegments) 
coords_clusters <- coords_km %>%
  filter(km %in% clusters) 

coords_km <- st_transform(coords_km, crs = 4326)
coords_clusters <- st_transform(coords_clusters, crs = 4326)
coords_control_ALL <- st_transform(coords_control_ALL, crs = 4326)
(segments <- ggplot() +
    geom_path(data = as.data.frame(st_coordinates(coords_km)), 
              aes(X, Y), color = "black", linewidth = 0.3) + 
    geom_sf(data = coords_clusters, aes(geometry = geometry, color = "Mitigated"), size = 1.3) + 
    geom_sf(data = coords_control_ALL, aes(geometry = geometry, color = "Non-mitigated"), size = 1.3) + 
    scale_color_manual(values = c("Mitigated" = "firebrick", "Non-mitigated" = "black")) +
    scale_x_continuous(breaks = seq(-40.55, -40.3, by = 0.08)) +
    labs(color = "Segment type") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          axis.title = element_blank()) +
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", pad_x = unit(0, "in"), pad_y = unit(0.25, "in"), which_north = "true", style = north_arrow_nautical) +
    annotate("text", x = -40.34, y = -20.315, label = "Vitória", size = 4, fontface = "bold") +
    annotate("text", x = -40.51, y = -20.755, label = "Guarapari", size = 4, fontface = "bold") )
segments
#ggsave(here::here("Analysis", "segments.jpg"), segments)

## organizing data
years <- 2004:2017

#### ALL control roadkills ----
roadkills_ALLcontrol <- expand.grid(year = years, segment = control_ALLsegments) %>% 
  left_join(roadkills, by = c("segment", "year")) %>%
  mutate(roadkills = tidyr::replace_na(roadkills, 0),
         type = "control")
roadkills_ALLcontrol %>%
  group_by(year) %>%
  summarise(total_roadkills = sum(roadkills),
            mean_roadkills = round(mean(roadkills), 2),
            sd_roadkills = round(sd(roadkills), 2))
sum(roadkills_ALLcontrol$roadkills)
roadkills_ALLcontrol %>%
  summarise(
    mean_roadkills = round(mean(roadkills), 2),
    sd_roadkills = round(sd(roadkills), 2)
  )

#### IMPACT roadkills ----
roadkills_impact <- expand.grid(year = years, segment = clusters) %>% 
  left_join(roadkills, by = c("segment", "year")) %>%
  mutate(roadkills = tidyr::replace_na(roadkills, 0),
         type = "impact")
roadkills_impact %>%
  group_by(year) %>%
  summarise(total_roadkills = sum(roadkills),
            mean_roadkills = round(mean(roadkills), 2),
            sd_roadkills = sd(roadkills))
sum(roadkills_impact$roadkills)
roadkills_impact %>%
  summarise(
    mean_roadkills = round(mean(roadkills), 2),
    sd_roadkills = round(sd(roadkills), 2)
  )

# ANALYSES -----
# complete data - ALL control and impact
complete_ALL_Impact <- bind_rows(roadkills_ALLcontrol, roadkills_impact)

complete_glm_ALL <- glm.nb(roadkills ~ type*year, data = complete_ALL_Impact)
summary(complete_glm_ALL)
check_residuals(complete_glm_ALL)
#Anova(complete_glm_ALL, type = "III")
predictions_complete_ALL <- ggemmeans(complete_glm_ALL, terms = c("year", "type"))
plot(predictions_complete_ALL)
df_predictions_complete_ALL <- as.data.frame(predictions_complete_ALL) %>% rename(year = x, type = group)

df_predictions_complete_ALL$type <- factor(
  df_predictions_complete_ALL$type,
  levels = c("impact", "control"), # Ordem desejada: Mitigated (impact) à esquerda
  labels = c("Mitigated", "Non-mitigated") # Renomear para os rótulos finais
)

# figure
(predicted_values_ALL <- ggplot(df_predictions_complete_ALL, aes(x = year, y = predicted)) +
    geom_line(linewidth = 1, aes(color = type)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
    facet_grid(~ type) + 
    scale_color_manual(values = c("Mitigated" = "firebrick", "Non-mitigated" = "black")) +
    labs(x = "Years", y = "Number of roadkills") +
    scale_y_continuous(limits = c(0, 3), breaks = c(0, 1, 2, 3)) +
    scale_x_continuous(limits = c(2004, 2017), breaks = c(2004, 2017), expand = c(0, 0)) +
    theme_minimal() + 
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14, margin = margin(r = 10)),
          axis.text.x = element_text(size = 12, color = "grey10"),
          axis.text.y = element_text(size = 12, color = "grey10"),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(size = 14),
          legend.position = "none",
          panel.spacing = unit(2.5, "lines"),
          plot.margin = margin(1.5, 15, 5, 5))
)

#ggsave(predicted_values_ALL, filename = here::here("Analysis", "CI_ALL.png"), 
#       dpi = 300, width = 2300, height = 1800, unit = "px")

ggpubr::ggarrange(observed_values_ALL,
                  predicted_values_ALL, 
                  ncol = 2)
ggsave(here::here("Analysis", "ALL_combined.png"), dpi = 300, width = 3000, height = 1780, unit = "px")


ggpubr::ggarrange(observed_values_UCS,
                  predicted_values_UCS,
                  observed_values_ALL,
                  predicted_values_ALL, 
                  ncol = 2, nrow = 2)
ggsave(here::here("Analysis", "control_impact.png"), dpi = 300, width = 3000, height = 2500, unit = "px")


# # complete data - UCS control and impact
# complete_UCS_Impact <- bind_rows(roadkills_UCScontrol, roadkills_impact) %>% mutate(year = as.numeric(year))
# 
# complete_glm_UCS <- glm.nb(roadkills ~ year * type, data = complete_UCS_Impact)
# summary(complete_glm_UCS)
# check_residuals(complete_glm_UCS)
# Anova(complete_glm_UCS)
# predictions_complete_UCS <- ggemmeans(complete_glm_UCS, terms = c("year", "type"))
# plot(predictions_complete_UCS) 
# df_predictions_complete_UCS <- as.data.frame(predictions_complete_UCS) %>% rename(year = x, type = group)
# 
# # figure
# (observed_values_UCS <- ggplot(data = complete_UCS_Impact, aes(y = roadkills, x = year, color = type, shape = type)) +
#     geom_point(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.2, dodge.width = 0.6), 
#                alpha = 0.5, size = 2) +
#     scale_color_manual(values = c("control" = "firebrick", "impact" = "darkgreen"),
#                        labels = c("Control", "Impact")) +
#     scale_shape_manual(values = c("control" = 16, "impact" = 17),  # Formas personalizadas
#                        labels = c("Control", "Impact")) +
#     labs(x = "Years", y = "Number of roadkills", color = "Segment Type", shape = "Segment Type", title = "a) Observed values UCs") +
#     theme_minimal() +
#     theme(axis.title.x = element_text(size = 14),
#           axis.title.y = element_text(size = 14),
#           axis.text.x = element_text(size = 12),
#           axis.text.y = element_text(size = 12),
#           panel.grid.major.x = element_blank(),
#           legend.position = "bottom") +
#     scale_y_continuous(limits = c(-0.2, 10.1), breaks = c(0, 2, 4, 6, 8, 10)) +
#     scale_x_continuous(limits = c(2003.7, 2017.4), breaks = c(2004, 2007, 2011, 2014, 2017)) 
# )
# 
# (predicted_values_UCS <- ggplot(df_predictions_complete_UCS, aes(x = year, y = predicted, color = type, linetype = type)) +
#     geom_line(aes(y = predicted), size = 1) +
#     geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, linetype = 0) +
#     scale_color_manual(values = c("control" = "firebrick", "impact" = "darkgreen"),
#                        labels = c("Control", "Impact")) +
#     scale_linetype_manual(values = c("control" = "solid", "impact" = "solid"),
#                           labels = c("Control", "Impact")) +
#     labs(x = "Years", y = "Number of roadkills", color = "Segment type", linetype = "Segment type", title = "b) Predicted values UCs") +
#     scale_y_continuous(limits = c(0, 3), breaks = c(0, 1, 2, 3)) +
#     scale_x_continuous(limits = c(2004, 2017.2), breaks = c(2004, 2007, 2011, 2014, 2017), expand = c(0, 0)) +
#     theme_minimal() + 
#     theme(axis.title.x = element_text(size = 14),
#           axis.title.y = element_text(size = 14, margin = margin(r = 10)),
#           axis.text.x = element_text(size = 12, color = "grey10"),
#           axis.text.y = element_text(size = 12, color = "grey10"),
#           panel.grid.major.x = element_blank(),
#           legend.position = "bottom")
# )
# 
# #ggsave(predicted_values_UCS, filename = here::here("Analysis", "CI_UCS.png"), 
# #       dpi = 300, width = 2100, height = 1780, unit = "px")
# 
# ggpubr::ggarrange(observed_values_UCS,
#                   predicted_values_UCS, 
#                   ncol = 2)
#ggsave(here::here("Analysis", "UCS_combined.png"), dpi = 300, width = 3000, height = 1780, unit = "px")
