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
library(treemapify)
library(svglite)

# Fuente 
font_add_google("Poppins", "Poppins")
showtext_auto()

# Datos simulados
data <- tibble::tibble(
  entidad = c("CDMX", "Jalisco", "Edomex", "Nuevo León", "Chiapas", "Yucatán", "Sonora"),
  total = c(20000, 18000, 17000, 16000, 9000, 6000, 4000)
)

# Porcentaje
data <- data %>%
  arrange(desc(total)) %>%
  mutate(
    porcentaje = round(total / sum(total) * 100, 1),
    etiqueta = paste0(entidad, "\n", format(total, big.mark = ","), "\n", porcentaje, "%")
  )

# Colores
verde_oscuro <- "#10302C"
verde_claro <- "#4C6A67"
data <- data %>%
  mutate(
    color = ifelse(total == max(total), verde_oscuro, verde_claro)
  )

# Treemap
grafica <- ggplot(data, aes(area = total, fill = color, label = etiqueta)) +
  geom_treemap(color = "white", linewidth = 3) +
  geom_treemap_text(
    family = "Poppins", fontface = "bold", colour = "white",
    place = "topleft", grow = FALSE, reflow = TRUE, 
    lineheight = 1.1, size = 7
  ) +
  scale_fill_identity() +
  theme_void()

svglite("rstudio/treemap/treemap.svg", width = 12, height = 6)
print(grafica)
dev.off()
