# Librerías
library(tidyverse)
library(sf)
library(scales)
library(biscale)
library(cowplot)
library(RColorBrewer)
library(viridis)
library(lubridate)
library(showtext)
library(ggrepel)
library(hrbrthemes)

# Simular datos
set.seed(42)
n <- 50

sim_mun <- tibble(
  cve_municipio = paste("MUN", sprintf("%02d", 1:n)),
  promedio_diario_2023_2024 = runif(n, 0, 15),
  promedio_diario_2024_2025 = promedio_diario_2023_2024 + rnorm(n, 0, 3)
)

sim_mun$cve_municipio[10] <- "CULIACAN"

# Clasificación solo para Culiacán
sim_mun <- sim_mun %>%
  mutate(
    is_culiacan = cve_municipio == "CULIACAN",
    color_cat = ifelse(is_culiacan, "Culiacán", "Normal"),
    alpha_cat = ifelse(is_culiacan, 1, 0.7),
    size_cat = ifelse(is_culiacan, 3.5, 2)
  )

# Límites
lim_min <- min(sim_mun$promedio_diario_2023_2024, sim_mun$promedio_diario_2024_2025)
lim_max <- max(sim_mun$promedio_diario_2023_2024, sim_mun$promedio_diario_2024_2025)

# Colores
colores <- c(
  "Culiacán" = "#671435",
  "Normal"   = "#006157"
)

# Gráfica
graf5 <- ggplot(sim_mun, aes(x = promedio_diario_2023_2024, y = promedio_diario_2024_2025)) +
  geom_abline(slope = 1, intercept = 0, color = "#9D792A", linewidth = 1.2) +
  geom_point(aes(color = color_cat, alpha = alpha_cat, size = size_cat), shape = 16) +
  geom_text_repel(
    data = subset(sim_mun, is_culiacan),
    aes(label = cve_municipio),
    family = "Poppins", fontface = "bold", size = 3.5
  ) +
  scale_color_manual(values = colores) +
  scale_alpha_identity() +
  scale_size_identity() +
  scale_x_continuous(labels = comma_format(), limits = c(lim_min, lim_max)) +
  scale_y_continuous(labels = comma_format(), limits = c(lim_min, lim_max)) +
  coord_equal() +
  labs(x = NULL, y = NULL) +
  theme_ipsum(base_family = "Poppins") +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F9F9F9", color = NA)
  )

svglite("rstudio/scatter/scatter.svg", width = 12, height = 6)
print(graf5)
dev.off()

