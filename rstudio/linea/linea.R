# Librerías 
library(tidyverse)
library(sf)
library(scales)
library(biscale)
library(cowplot)
library(RColorBrewer)
library(viridis)
library(lubridate)
library(ggtext)
library(readxl)
library(showtext)
library(svglite)

# Colores estilo Python
color_area <- "#d4dce9"        # área azul claro
color_linea <- "#2f5597"       # línea principal
color_tendencia <- "#c00000"   # línea punteada roja

# Fuente 
font_add_google("Poppins", "Poppins")
showtext_auto()

# Tema adaptado estilo matplotlib institucional
tema_python_atdt <- function() {
  theme_minimal(base_family = "Poppins") +
    theme(
      plot.title = element_text(size = 14, face = "bold", color = "black"),
      axis.text = element_text(size = 10, color = "#767676"),
      axis.title = element_text(size = 10, face = "bold", color = "#000000"),
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "#000000", linewidth = 0.4, linetype = "solid"),
      axis.ticks = element_blank(),
      legend.position = "none",
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )
}

# Simular datos
set.seed(123)

# Crear fechas mensuales desde enero 2015 hasta diciembre 2024
fechas <- seq(as.Date("2015-01-01"), as.Date("2024-12-01"), by = "month")

# Generar salarios mínimos simulados con una ligera tendencia creciente
total_def <- round(70 + cumsum(rnorm(length(fechas), mean = 0.5, sd = 1.2)), 2)

# Crear dataframe
datos <- tibble(
  fecha = fechas,
  total_def = total_def
)

# Línea de tendencia
modelo <- lm(total_def ~ fecha, data = datos)
datos$tendencia <- predict(modelo)

# Crear gráfica
linea <- ggplot(datos, aes(x = fecha, y = total_def)) +
  geom_area(fill = color_area, alpha = 1) +
  geom_line(color = color_linea, linewidth = 1.2) +
  geom_line(aes(y = tendencia), color = color_tendencia, linewidth = 1.2, linetype = "dashed") +
  geom_text(
    data = tail(datos, 1),
    aes(label = scales::dollar(total_def)),
    hjust = 0.5, vjust = -1,
    family = "Poppins", size = 4, color = "#10302C"
  ) +
  scale_x_date(date_labels = "%b %Y",
               date_breaks = "1 year")+
  scale_y_continuous(labels = dollar_format(), expand = expansion(mult = c(0.1, 0.15))) +
  labs(
    x = NULL,
    y = NULL
  ) +
  tema_python_atdt()

svglite("rstudio/linea/linea.svg", width = 12, height = 6)
