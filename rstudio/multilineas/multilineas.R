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
library(svglite)

# Colores y tema
colores <- c("#006157", "#767676", "#671435", "#9B2247")
blanco <- "white"

set.seed(123)
fechas <- seq(as.Date("2000-12-31"), as.Date("2025-12-31"), by = "year")
n <- length(fechas)

df <- tibble(
  Fecha = fechas,
  Trigo = c(sort(runif(n - 5, 100, 4000)), runif(5, 500, 1000)),
  Maiz = c(sort(runif(n - 5, 100, 3500)), runif(5, 400, 800)),
  Frijol = c(sort(runif(n - 5, 100, 3000)), runif(5, 300, 700)),
  Soja = c(sort(runif(n - 5, 100, 2500)), runif(5, 100, 500))
)

# Transformar a formato largo
df_largo <- df %>%
  pivot_longer(cols = -Fecha, names_to = "variable", values_to = "valor")

# Últimos puntos para badge
etiquetas <- df_largo %>%
  group_by(variable) %>%
  filter(Fecha == max(Fecha)) %>%
  mutate(etiqueta = scales::dollar(valor))

# Crear badges bien distribuidos verticalmente
etiquetas <- df_largo %>%
  group_by(variable) %>%
  filter(Fecha == max(Fecha)) %>%
  ungroup() %>%
  arrange(desc(valor)) %>%  # ordenar globalmente por valor
  mutate(
    etiqueta = scales::dollar(valor),
    x_label = Fecha + lubridate::days(30),         # desplazar a la derecha
    y_label = valor + row_number() * -250           # separar verticalmente
  )

# Gráfica
grafica <- ggplot(df_largo, aes(x = Fecha, y = valor, color = variable)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "gray30", linewidth = 0.4) +
  scale_color_manual(values = colores) +
  geom_label(data = etiquetas,
             aes(x = x_label, y = y_label, label = etiqueta, fill = variable),
             color = blanco,
             family = "Poppins",
             size = 3.5,
             label.size = 0,
             label.r = unit(6, "pt"),
             hjust = 0.6,
             vjust = -4,
             show.legend = FALSE) +
  scale_fill_manual(values = colores) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 year") +
  tema_estilo_multilinea() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

svglite("rstudio/multilineas/multilinea.svg", width = 12, height = 6)
print(grafica)
dev.off()