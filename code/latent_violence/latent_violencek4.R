# LIBRERÍAS -------------------------------------------------------------------
library(LMest)
library(dplyr)
library(lavaan)
library(psych)   
library(semPlot)
library(car)
library(stringr)
library(sjlabelled)
library(sjmisc)
library(here)
library(lcmm)
library(haven)
library(panelr)
library(tidyverse)
library(viridis)
library(xtable)

# FUNCIONES AUXILIARES -------------------------------------------------------
save_plot <- function(plot, filename, width, height) {
  ggsave(
    filename = file.path("code/latent_violence", filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white",
    device = "png"
  )
}

# LIMPIEZA DE ENTORNO --------------------------------------------------------
cat("\014")
rm(list = ls())
gc()

# CARGA DE DATOS -------------------------------------------------------------
load(here("data/BBDD_ELRI_LONG.RData"))

# SELECCIÓN Y TRANSFORMACIÓN DE VARIABLES ------------------------------------
subset_data <- BBDD_ELRI_LONG %>% 
  mutate(
    indi = case_when(a1 %in% 1:11 ~ "indi",
                     a1 == 12 ~ "no_indi"),
    cat_indi = case_when(
      a1 == 1 ~ "mapuche", 
      a1 %in% c(2, 4, 5, 6, 7, 10) ~ "andino",
      a1 == 12 ~ "chileno_noindig",
      TRUE ~ "otro"),
    mujer = case_when(g2 == 1 ~ "0",
                      g2 == 2 ~ "1"),
    indi = case_when(indi == "indi" ~ "0",
                     indi == "no_indi" ~ "1"), 
    edad = case_when(
      g18 %in% 18:24 ~ "18_24",
      g18 %in% 25:34 ~ "25_34",
      g18 %in% 35:44 ~ "35_44",
      g18 %in% 45:54 ~ "45_54",
      g18 %in% 55:64 ~ "55_64", 
      g18 %in% 65:89 ~ "65+")
  ) %>% 
  select("folio", "ola", 
         "a4",
         "a6",
         "d3_1", 
         "d3_2", 
         "d4_2",
         "d4_3", 
         "c1",
         "c2", 
         "d5_1", 
         "d5_2",
         "d6_1", 
         "c7_3",
         "c14",
         "c27_1",
         "c27_3",
         "urbano_rural", "mujer", "edad", "indi")




# LIMPIEZA Y RECODIFICACIÓN DE VARIABLES -------------------------------------
subset_data$folio <- as.numeric(as.character(as.factor(subset_data$folio)))
subset_data$ola <- as.numeric(subset_data$ola)

subset_data <- subset_data %>%
  mutate(across(c(d3_1, d3_2, d4_2, d4_3, c14), ~as.numeric(zap_labels(.)))) %>%
  mutate(
    across(where(is.numeric), 
           ~if_else(. %in% c(8888, 9999, 88, 99), NA_real_, .)),
    across(where(is.character), 
           ~if_else(. %in% c("8888", "9999", "88", "99"), NA_character_, .))
  )


# Asegurarse de que 'a4' es numérico puro
subset_data$a4 <- as.numeric(subset_data$a4)

# Reemplazar NaN con NA
subset_data$a4[is.nan(subset_data$a4)] <- NA

subset_data |> select(a4)

subset_data <- subset_data %>%
  group_by(folio, ola) %>%
  mutate(
    media_a4 = mean(a4, na.rm = TRUE),
    media_a4 = ifelse(is.nan(media_a4), NA_real_, media_a4),  # prevenir NaN
    a4 = if_else(is.na(a4), media_a4, a4)
  ) %>%
  select(-media_a4) %>%
  ungroup()


subset_data |> select(a4)

# Paso 1: Reemplazar NaN por NA en a4 y c14
subset_data$a4 <- as.numeric(subset_data$a4)
subset_data$a4[is.nan(subset_data$a4)] <- NA

subset_data$c14 <- as.numeric(subset_data$c14)
subset_data$c14[is.nan(subset_data$c14)] <- NA

# Paso 2: Imputar a4 y c14 por grupo (folio, ola), luego por promedio global
subset_data <- subset_data %>%
  group_by(folio, ola) %>%
  mutate(
    prom_a4_grupo = mean(a4, na.rm = TRUE),
    prom_c14_grupo = mean(c14, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    a4 = if_else(is.na(a4), prom_a4_grupo, a4),
    c14 = if_else(is.na(c14), prom_c14_grupo, c14),
    prom_a4_global = mean(a4, na.rm = TRUE),
    prom_c14_global = mean(c14, na.rm = TRUE),
    a4 = if_else(is.na(a4), prom_a4_global, a4),
    c14 = if_else(is.na(c14), prom_c14_global, c14)
  ) %>%
  select(-starts_with("prom_"))


# RECLASIFICACIÓN ------------------------------------------------------------
reclasificar <- function(x) {
  case_when(
    x <= 2 ~ "1",
    x == 3 ~ "2",
    x >= 4 ~ "3",
    TRUE ~ NA_character_
  )
}

subset_data <- subset_data %>%
  mutate(across(c(d3_1, d3_2, d4_2, d4_3), 
                ~reclasificar(.), 
                .names = "{.col}_red"))

subset_data %>%
  select(ends_with("_red")) %>%
  summarise(across(everything(), ~table(.)))

# PREPARACIÓN PARA LCM / LCA CON LMest ---------------------------------------
subset_data <- subset_data %>% 
  panel_data(id = folio, wave = ola) %>% 
  complete_data(min.waves = 4) %>%  
  group_by(folio) %>%
  filter(n_distinct(ola) == 4 & all(ola %in% 1:4)) %>%
  ungroup() %>% 
  as.data.frame()


print(colSums(is.na(subset_data)))
table(subset_data$folio, subset_data$ola)

subset_data$d5_1[is.na(subset_data$d5_1)] <- median(subset_data$d5_1, na.rm = TRUE)

subset_data$d3_1_red <- as.numeric(subset_data$d3_1_red) - 1
subset_data$d3_2_red <- as.numeric(subset_data$d3_2_red) - 1
subset_data$d4_2_red <- as.numeric(subset_data$d4_2_red) - 1
subset_data$d4_3_red <- as.numeric(subset_data$d4_3_red) - 1

# MODELAMIENTO LATENTE CON LMest ---------------------------------------------
data_lmest <- lmestData(data = subset_data, 
                        id = "folio", 
                        time = "ola")

responsesFormula <- d3_1_red + d3_2_red + d4_2_red + d4_3_red ~ NULL
latentFormula <- ~ urbano_rural + mujer + edad +  d5_1 + d5_2 + a4 + a6 + c7_3 + c14


# Reestimamos el modelo con 3 clases
modelo_lm3 <- lmest(responsesFormula = responsesFormula,
                    latentFormula = latentFormula,
                    index = c("folio", "ola"),
                    data = data_lmest,
                    k = 4,  # Fijamos en 3 clases
                    modSel = "BIC",
                    start = 1,
                    out_se = TRUE)  # Para obtener errores estándar
seed = (1234)


# Visualización de probabilidades condicionales
plot(modelo_lm3, what = "CondProb")
# Visualizar matriz de transición
plot(modelo_lm3, what = "transitions")

## plot ------------------------------------------------------------------------ 

# plots 
LMmodelo <- reshape2::melt(modelo_lm3$Psi, level=1)
glimpse(LMmodelo)
LMmodelo <- LMmodelo %>% mutate(value = round(value * 100))

library(gridExtra)


# Creamos un vector con las descripciones de los ítems
item_descriptions <- c(
  "1" = "El uso de la fuerza por\nparte de Carabineros para\ndisolver protestas de\ngrupos indígenas",
  "2" = "Que agricultores usen\narmas para enfrentar a\ngrupos de personas\nindígenas",
  "3" = "Que grupos de personas\nindígenas se tomen\nterrenos que se\nconsideran propios",
  "4" = "El bloqueo o corte de\ncarreteras por parte de\ngrupos de personas\nindígenas"
)

p1 <- ggplot(LMmodelo, aes(x = factor(item), y = value, fill = factor(category))) +
  geom_col(position = "stack") +
  facet_wrap(~ state, ncol = 1, labeller = labeller(state = function(x) paste("Clase", x))) +
  scale_fill_viridis(discrete = TRUE, 
                     alpha = 1,
                     option = "D",
                     direction = -1,
                     name = "¿Se justifica?",
                     labels = c("Nunca o casi nunca", "A veces o a menudo", "A menudo o siempre")) +
  scale_x_discrete(labels = item_descriptions) +
  labs(x = NULL, y = "P(y)", 
       title = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, lineheight = 0.8),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.x = element_blank(),
    panel.spacing = unit(1, "lines")
  )

print(p1)
#save_plot(p1, "p1.png", width = 10, height = 8)



# Calcular proporciones usando Piv
proporciones <- colMeans(modelo_lm3$Piv)
proporciones <- round(proporciones * 100, 1)

# Verificar las proporciones
print("Proporción de la muestra en cada clase:")
print(proporciones)

# Crear etiquetas para las clases con sus proporciones
class_labels <- paste("Clase", 1:4, "\n(", proporciones, "%)", sep = "")

# Preparar los datos
LMmodelo <- reshape2::melt(modelo_lm3$Psi, level=1)
LMmodelo <- LMmodelo %>% mutate(value = round(value * 100))

# Vector con las descripciones de los ítems
item_descriptions <- c(
  "1" = "El uso de la fuerza por\nparte de Carabineros para\ndisolver protestas de\ngrupos indígenas",
  "2" = "Que agricultores usen\narmas para enfrentar a\ngrupos de personas\nindígenas",
  "3" = "Que grupos de personas\nindígenas se tomen\nterrenos que se\nconsideran propios",
  "4" = "El bloqueo o corte de\ncarreteras por parte de\ngrupos de personas\nindígenas"
)

# Crear el gráfico mejorado
p2 <- ggplot(LMmodelo, aes(x = factor(item), y = value, fill = factor(category))) +
  geom_col(position = "stack") +
  facet_wrap(~ state, ncol = 1, 
             labeller = labeller(state = function(x) class_labels[as.numeric(x)])) +
  scale_fill_manual(values = c("#1f2041", "#4b3f72", "#ffc857"),  # Colores personalizados
                    name = "¿Se justifica?",
                    labels = c("Nunca o casi nunca", "A veces o a menudo", "A menudo o siempre")) +
  scale_x_discrete(labels = item_descriptions) +
  labs(x = NULL, y = "P(y)") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(angle = 0, color = "black", hjust = 0.5, vjust = 1, lineheight = 0.8, size = 11),
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 12),
    axis.title.x = element_blank(),
    panel.spacing = unit(1, "lines")
  )

print(p2)
save_plot(p2, "p2.png", width = 11.5, height = 9)


# También podemos ver cómo cambian las proporciones en el tiempo usando PI
print("Matriz de transición promedio:")
trans_matrix <- apply(modelo_lm3$PI, c(1,2), mean, na.rm = TRUE)
print(round(trans_matrix, 3))



# Resultados de regresión multinomial ------------------------------------------  
# Calculamos errores estándar
errores_estandar <- se(modelo_lm3)

# Preparamos los datos para visualización
be_df <- as.data.frame(modelo_lm3$Be)
names(be_df) <- paste("Estado", 2:4)  # Estado 1 es referencia
be_df$Variable <- rownames(modelo_lm3$Be)

se_df <- as.data.frame(errores_estandar$seBe)
names(se_df) <- paste("Estado", 2:4)
se_df$Variable <- rownames(modelo_lm3$Be)

colnames(se_df)


# Convertimos a formato largo
be_long <- tidyr::pivot_longer(be_df, 
                               cols = starts_with("Estado"),
                               names_to = "Estado",
                               values_to = "Coeficiente")

se_long <- tidyr::pivot_longer(se_df,
                               cols = starts_with("Estado"),
                               names_to = "Estado",
                               values_to = "SE")

# Combinamos coeficientes y errores estándar
be_long <- dplyr::left_join(be_long, se_long)

# Calculamos intervalos de confianza
be_long <- be_long %>%
  mutate(
    IC_lower = Coeficiente - 1.96 * SE,
    IC_upper = Coeficiente + 1.96 * SE,
    OR = exp(Coeficiente),
    OR_IC_lower = exp(IC_lower),
    OR_IC_upper = exp(IC_upper),
    significativo = (IC_lower * IC_upper > 0)  # Para identificar efectos significativos
  )



# Definimos el orden deseado (invertido para que aparezca correctamente de arriba a abajo)
#orden_deseado <- rev(c("intercept", "urbano_rural", "mujer1", "edad25_34", "edad35_44", "edad45_54", "edad55_64", "edad65+",  "c1", "c2", "d5_1", "d5_2", "d6_1", "c27_1", "c27_3"))

# Convertimos la variable 'Variable' en un factor con el orden especificado
be_long$Variable <- factor(be_long$Variable)

# Definimos los colores manualmente
colores_manuales <- c("Estado 2" = "#4b3f72", "Estado 3" = "#ffc857",
                      "Estado 4" = "#21f041")

# Visualización con intervalos de confianza y orden personalizado
p3 <- ggplot(be_long, aes(x = Variable, 
                          y = Coeficiente, 
                          color = Estado)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = IC_lower, ymax = IC_upper),
                position = position_dodge(width = 0.5),
                width = 0.5) +
  coord_flip() +
  theme_minimal() +
  scale_color_manual(values = colores_manuales) +  # Usamos colores manuales
  labs(title = "",
       subtitle = "",
       x = "",
       y = "Coeficiente (log-odds)") +
  theme(
    axis.text.x = element_text(angle = 0, color = "black", hjust = 0.5, vjust = 1, lineheight = 0.8, size = 13),
    axis.text.y = element_text(angle = 0, color = "black", hjust = 0.5, vjust = 1, lineheight = 0.8, size = 13),
    legend.position = "bottom", 
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    plot.title = element_text(size=16),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.x = element_blank(),
    panel.spacing = unit(1, "lines")
  )

print(p3)
save_plot(p3, "p3.png", width = 10, height = 10)


# Tabla de resultados completa
tabla_resultados <- be_long %>%
  select(Variable, Estado, Coeficiente, SE, IC_lower, IC_upper, OR, OR_IC_lower, OR_IC_upper, significativo) %>%
  arrange(Variable, Estado)

print("Resultados completos (coeficientes, OR e intervalos de confianza):")
print(knitr::kable(tabla_resultados, digits = 3))

# Análisis de patrones de respuesta por estado
print("Probabilidades de respuesta condicional por estado:")
print(round(modelo_lm3$Psi, 3))

# Matriz de transición promedio
trans_matrix <- apply(modelo_lm3$PI, c(1,2), mean, na.rm = TRUE)
print("Matriz de transición promedio:")
print(round(trans_matrix, 3))


library(tidyverse)
library(ggraph)
library(tidygraph)
library(igraph)

trans_matrix <- matrix(c(
  0.120, 0.257, 0.211, 0.161,
  0.078, 0.404, 0.154, 0.113,
  0.069, 0.286, 0.276, 0.119,
  0.072, 0.250, 0.131, 0.296
), nrow = 4, byrow = TRUE)

library(tibble)
library(dplyr)
library(tidyr)


transitions <- as_tibble(trans_matrix) %>%
  mutate(from = row_number()) %>%
  pivot_longer(
    cols = -from,
    names_to = "to",
    values_to = "probability"
  ) %>%
  mutate(to = as.integer(gsub("V", "", to))) 

# Crear datos para los nodos con tamaños basados en probabilidad de permanencia
nodes <- tibble(
  name = 1:4,
  label = paste("Clase", 1:4),
  size = c(0.15, 0.22, 0.8, 0.55),  # Probabilidades de permanencia
  prop = c(26, 24, 20, 30)      # Proporción de la muestra
)

# Crear el objeto graph
graph <- tbl_graph(nodes = nodes, edges = transitions, directed = TRUE)



# Crear el gráfico
p4 <- ggraph(graph, layout = 'circle') +
  # Agregar conexiones
  geom_edge_arc(
    aes(label = sprintf("%.2f", probability)),
    arrow = arrow(length = unit(3, 'mm'), type = "closed"),
    angle_calc = 'along',
    label_dodge = unit(4, 'mm'),
    start_cap = circle(15, 'mm'),  # Aumentado de 8 a 10 mm
    end_cap = circle(18, 'mm'),    # Aumentado de 8 a 10 mm
    edge_width = 0.5,
    strength = 0.15,
    show.legend = FALSE
  ) +
  # Agregar nodos con tamaño variable
  geom_node_point(
    aes(size = size * 100),
    color = "white",
    fill = "#ffc857",
    shape = 21,
    stroke = 1  # Aumentado de 0.8 a 1 para un borde más visible
  ) +
  # Agregar etiquetas de clase y probabilidad de permanencia
  geom_node_text(
    aes(label = sprintf("%s\n(%.1f%%)\np=%.2f", 
                        label, 
                        prop,
                        size)),
    size = 3.8, # Reducido de 4 a 3.5 para evitar superposición
    color =  "#4b3f72"
  ) +
  # Ajustar la escala de tamaño
  scale_size_continuous(range = c(30, 45)) +  
  # Tema y título
  theme_void() +
  labs(title = "") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "none"
  ) +
  # Ajustar el tamaño del gráfico
  coord_fixed(ratio = 1, xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))  # Aumentado de 1.5 a 1.6


print(p4)
save_plot(p4, "p4.png", width = 10, height = 10)






