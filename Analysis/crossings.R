library(dplyr)
library(MASS)
library(performance)
library(car)
library(ggeffects)
library(lubridate)
library(ggplot2)
library(ggtext)
library(patchwork)

# loading raw data
crossing_records <- read.csv(here::here("Analysis", "crossing_records.csv"), sep = ";") %>%
  mutate(date = dmy(paste(Dia, Mês, Ano, sep = "-")),
         cluster = case_when(
           km == 45 ~ "km45",
           km == 50 ~ "km50",
           km == 59 ~ "km59")) %>%
  dplyr::select(date, spp, cluster)
head(crossing_records)

# organizing data
clusters_crossings <- crossing_records %>%
  mutate(month = factor(month(date), levels = 1:12, labels = month.name),
         year = year(date)) %>%
  group_by(cluster, year, month) %>%
  summarise(n_records = n(), .groups = 'drop') 

# period data
clusters = c("km45", "km50", "km59")
years <- 2004:2017

data <- expand.grid(month = month.name, year = years, cluster = clusters) %>% 
  left_join(clusters_crossings, by = c("month", "year", "cluster")) %>%
  mutate(n_records = tidyr::replace_na(n_records, 0))


# km 45
month_crossings_km45 <- data %>%
  filter(cluster == "km45") %>%
  group_by(year) %>%
  summarise(total_records = sum(n_records))

# km 50
month_crossings_km50 <- data %>%
  filter(cluster == "km50") %>%
  group_by(year) %>%
  summarise(total_records = sum(n_records))

# km 59
month_crossings_km59 <- data %>%
  filter(cluster == "km59") %>%
  group_by(year) %>%
  summarise(total_records = sum(n_records))


# analyses
# cluster km 45
cluster_km45 <- glm.nb(total_records ~ year, data = month_crossings_km45)
summary(cluster_km45)
check_residuals(cluster_km45)
predictions_km45 <- ggemmeans(cluster_km45, terms = "year")
plot(predictions_km45)
df_km45 <- as.data.frame(predictions_km45) %>% rename(year = x)

# cluster km 50
cluster_km50 <- glm.nb(total_records ~ year, data = month_crossings_km50)
summary(cluster_km50)
check_residuals(cluster_km50)
predictions_km50 <- ggemmeans(cluster_km50, terms = "year")
plot(predictions_km50)
df_km50 <- as.data.frame(predictions_km50) %>% rename(year = x)

# cluster km 59
cluster_km59 <- glm.nb(total_records ~ year, data = month_crossings_km59)
summary(cluster_km59)
check_residuals(cluster_km59)
predictions_km59 <- ggemmeans(cluster_km59, terms = "year")
plot(predictions_km59)
df_km59 <- as.data.frame(predictions_km59) %>% rename(year = x)

# Create the plot
(cluster_45 <- ggplot(df_km45, aes(x = year, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5) +
    geom_line(aes(y = predicted)) +
    geom_point(data = month_crossings_km45, aes(x = year, y = total_records),  size = 1.8, shape = 17,
               color = "black") + 
    labs(x = "", y = "Number of crossings", title = "a) Km 45") +
    theme_minimal() + 
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14, margin = margin(r = 10)),
          axis.text.x = element_text(size = 12, color = "grey10"),
          axis.text.y = element_text(size = 12, color = "grey10"),
          plot.title = element_text(size = 16, hjust = 0.5),
          panel.grid.major.x = element_blank()) +
    scale_y_continuous(limits = c(0, 1400), breaks = c(0, 700, 1400)) +
    scale_x_continuous(limits = c(2004, 2017), breaks = c(2004, 2017))
)

(cluster_50 <- ggplot(df_km50, aes(x = year, y = predicted)) +
    geom_ribbon(aes( ymin = conf.low, ymax = conf.high), alpha = 0.5) +
    geom_line(aes(y = predicted)) +
    geom_point(data = month_crossings_km50, aes(x = year, y = total_records), size = 1.8, shape = 17,
               color = "black") + 
    labs(x = "Years", y = "", title = "b) Km 50") +
    theme_minimal() + 
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14, margin = margin(r = 10)),
          axis.text.x = element_text(size = 12, color = "grey10"),
          axis.text.y = element_text(size = 12, color = "grey10"),
          plot.title = element_text(size = 16, hjust = 0.5),
          panel.grid.major.x = element_blank()) +
    scale_y_continuous(limits = c(0, 340), breaks = c(0, 170, 340)) +
    scale_x_continuous(limits = c(2004, 2017), breaks = c(2004, 2017))
)

(cluster_59 <- ggplot(df_km59, aes(x = year, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5) +
    geom_line(aes(y = predicted)) +
    geom_point(data = month_crossings_km59, aes(x = year, y = total_records), size = 1.8, shape = 17,
               color = "black") + 
    labs(x = "", y = "", title = "c) Km 59") +
    theme_minimal() + 
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14, margin = margin(r = 10)),
          axis.text.x = element_text(size = 12, color = "grey10"),
          axis.text.y = element_text(size = 12, color = "grey10"),
          plot.title = element_text(size = 16, hjust = 0.5),
          panel.grid.major.x = element_blank()) +
    scale_y_continuous(limits = c(0, 600), breaks = c(0, 300, 600)) +
    scale_x_continuous(limits = c(2004, 2017), breaks = c(2004, 2017))
)


# all together
(clusters_plot <- cluster_45 + cluster_50 + cluster_59)

ggsave(clusters_plot, filename = here::here("Analysis", "clusters_plots.png"), 
       dpi = 300, width = 3600, height = 1500, unit = "px")


