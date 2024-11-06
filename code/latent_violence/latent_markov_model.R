

# librerías. 
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
library(dplyr)
library(haven)
library(LMest)
library(panelr)
library(tidyverse)
library(viridis)
library(xtable)

save_plot <- function(plot, filename, width, height) {
  ggsave(
    filename = file.path("/home/rober/Documents/elri_code_analysis/code/latent_violence", filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white",
    device = "png"
  )
}

# limpiar espacio
cat("\014")
rm(list = ls())
gc()

# Load data
load(here("data/BBDD_ELRI_LONG.RData"))

BBDD_ELRI_LONG$d3_1

subset_data <- BBDD_ELRI_LONG %>% 
  dplyr::mutate(indi = case_when(a1 %in% 1:11 ~ "indi",
                                 a1 == 12 ~ "no_indi")) %>% 
  dplyr::mutate(cat_indi = case_when(a1 == 1 ~ "mapuche", 
                                     a1 %in% c(2, 4, 5, 6, 7, 10) ~ "andino",
                                     a1 == 12 ~ "chileno_noindig",
                                     TRUE ~ "otro")) %>% 
  dplyr::mutate (mujer = case_when(g2 == 1 ~ "0",
                                   g2 == 2 ~ "1")) %>% 
  dplyr::mutate (edad = case_when(g18 %in% 18:24 ~ "18_24",
                                  g18 %in% 25:34 ~ "25_34",
                                  g18 %in% 35:44 ~ "35_44",
                                  g18 %in% 45:54 ~ "45_54",
                                  g18 %in% 55:64 ~ "55_64", 
                                  g18 %in% 65:89 ~ "65+")) %>% 
  select("folio", "ola", "d3_1", "d3_2", "d4_2", "d4_3", "c1", "c2", "d5_1", "d5_2", "d6_1", "c27_1", "c27_3",
         "urbano_rural", "mujer", "edad")

# Asumiendo que subset_data ya está cargado
# Convertir 'folio' a factor para usarlo como identificador de sujeto
subset_data$folio <- as.factor(subset_data$folio)

# Convertir 'ola' a numérico si no lo es ya
subset_data$ola <- as.numeric(subset_data$ola)
subset_data$folio <- as.numeric(as.character(subset_data$folio))

subset_data <- subset_data %>%
  # Primero, convertimos las columnas haven_labelled a numérico
  mutate(across(c(d3_1, d3_2, d4_2, d4_3), ~as.numeric(zap_labels(.)))) %>%
  # Luego, aplicamos las transformaciones para manejar los valores especiales
  mutate(
    across(where(is.numeric), 
           ~if_else(. %in% c(8888, 9999, 88, 99), NA_real_, .)),
    across(where(is.character), 
           ~if_else(. %in% c("8888", "9999", "88", "99"), NA_character_, .))
  )

glimpse(subset_data)

# recod. 
reclasificar <- function(x) {
  case_when(
    x <= 2 ~ "1",
    x == 3 ~ "2",
    x >= 4 ~ "3",
    TRUE ~ NA_character_
  )
}

# Aplicar la reclasificación a las variables de interés
subset_data <- subset_data %>%
  mutate(across(c(d3_1, d3_2, d4_2, d4_3), 
                ~reclasificar(.), 
                .names = "{.col}_red"))

# Verificar la nueva clasificación
subset_data %>%
  select(ends_with("_red")) %>%
  summarise(across(everything(), ~table(.)))



# modelos lmest ----------------------------------------------------------------
subset_data <- subset_data %>% 
  panel_data(subset_data, id = folio, wave = ola) %>% 
  complete_data(min.waves = 4) %>%  
  group_by(folio) %>%
  filter(n_distinct(ola) == 4 & all(ola %in% 1:4)) %>%
  ungroup() %>% 
  as.data.frame()
subset_data %>% frq(ola)


print(colSums(is.na(subset_data)))
table(subset_data$folio, subset_data$ola)
subset_data$d5_1[is.na(subset_data$d5_1)] <- median(subset_data$d5_1, na.rm = TRUE) # imputar un valor NA
subset_data_clean <- subset_data[complete.cases(subset_data), ]
subset_data_clean <- na.omit(subset_data)







str(subset_data)
table(subset_data$d3_1_red)
table(subset_data$d3_2_red)
table(subset_data$d4_2_red)
table(subset_data$d4_3_red)

subset_data$d3_1_red <- as.numeric(subset_data$d3_1_red) - 1
subset_data$d3_2_red <- as.numeric(subset_data$d3_2_red) - 1
subset_data$d4_2_red <- as.numeric(subset_data$d4_2_red) - 1
subset_data$d4_3_red <- as.numeric(subset_data$d4_3_red) - 1

# Verificamos la codificación
table(subset_data$d3_1_red)
table(subset_data$d3_2_red)
table(subset_data$d4_2_red)
table(subset_data$d4_3_red)

# Definimos datos tipo LMest
data_lmest <- lmestData(data = subset_data, 
                        id = "folio", 
                        time = "ola")

# Ecuación 
responsesFormula <- d3_1_red + d3_2_red + d4_2_red + d4_3_red ~ NULL
latentFormula <- ~ urbano_rural + mujer + edad + c1 + c2 + d5_1 + d5_2 + d6_1 + c27_1 + c27_3

# Ejacutamos modelo
modelo_lm <- lmest(responsesFormula = responsesFormula,
                   latentFormula = latentFormula,
                   index = c("folio", "ola"),
                   data = data_lmest,
                   k = 1:6,  # Prueba de 1 a 6 clases latentes
                   modSel = "BIC",
                   start = 1,
                   seed = 1234,
                   ntry = 3)  # Número de intentos con inicializaciones aleatorias



# Examinar criterios de información para selección del número de estados
summary(modelo_lm)
plot(modelo_lm, what = "modSel")

# Ver los valores BIC específicos
modelo_lm$Bic

# Visualizar matriz de transición
plot(modelo_lm, what = "transitions")

# Examinar probabilidades de transición específicas
print(round(apply(modelo_lm$Pi, c(1,2), mean), 3))

# Probabilidades de respuesta por estado
print(round(modelo_lm$Psi, 3))

# Visualización de probabilidades condicionales
plot(modelo_lm, what = "CondProb")

# Distribución marginal de estados
plot(modelo_lm, what = "marginal")

# Calcular prevalencia de estados por tiempo
estado_tiempo <- data.frame(
  Tiempo = rep(1:4, each = ncol(modelo_lm$Pmarg)),
  Estado = rep(1:ncol(modelo_lm$Pmarg), 4),
  Prevalencia = as.vector(modelo_lm$Pmarg)
)

# Visualizar con ggplot2
ggplot(estado_tiempo, aes(x = Tiempo, y = Prevalencia, color = factor(Estado))) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(color = "Estado", 
       title = "Prevalencia de estados latentes a lo largo del tiempo")


# Examinar coeficientes para covariables
summary(modelo_lm)$Be  # Efectos en probabilidades iniciales
summary(modelo_lm)$Ga  # Efectos en probabilidades de transición

# Calcular odds ratios para interpretación
exp(modelo_lm$Be)  # Para probabilidades iniciales
exp(modelo_lm$Ga)  # Para probabilidades de transición

# Decodificación local y global
predicciones <- lmestDecoding(modelo_lm)

# Examinar secuencias más comunes
tabla_secuencias <- table(apply(predicciones$Ug, 1, paste, collapse = ""))
sort(tabla_secuencias, decreasing = TRUE)[1:10]

# Calcular errores estándar
errores_estandar <- se(modelo_lm)


n_por_ola <- datos_longitudinales %>%
  group_by(ola) %>%
  summarise(N = n_distinct(id_participante))





# Criterios de información para diferentes números de estados
print("Criterios de información por número de estados:")
data.frame(
  k = 1:6,
  BIC = modelo_lm$Bic,
  AIC = modelo_lm$Aic,
  t = 4:4
)

# Crear gráfico de criterios de información
bic_aic_df <- data.frame(
  k = 1:6,
  BIC = modelo_lm$Bic,
  AIC = modelo_lm$Aic
)

ggplot(bic_aic_df, aes(x = k)) +
  geom_line(aes(y = BIC, color = "BIC")) +
  geom_line(aes(y = AIC, color = "AIC")) +
  geom_point(aes(y = BIC, color = "BIC")) +
  geom_point(aes(y = AIC, color = "AIC")) +
  scale_color_manual(values = c("BIC" = "blue", "AIC" = "red")) +
  labs(title = "Criterios de información por número de estados",
       x = "Número de estados",
       y = "Valor",
       color = "Criterio") +
  theme_minimal()


# Crear un dataframe con las probabilidades de respuesta
prob_resp <- as.data.frame.table(modelo_lm$Psi)
names(prob_resp) <- c("Categoria", "Estado", "Item", "Probabilidad")

# Visualizar las probabilidades de respuesta por estado
ggplot(prob_resp, aes(x = Item, y = Probabilidad, fill = Categoria)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Estado) +
  theme_minimal() +
  labs(title = "Probabilidades de respuesta condicional por estado",
       x = "Item",
       y = "Probabilidad",
       fill = "Categoría")


# Calcular matriz de transición promedio
trans_matrix <- apply(modelo_lm$PI, c(1,2), mean, na.rm = TRUE)
rownames(trans_matrix) <- paste("Estado", 1:4)
colnames(trans_matrix) <- paste("Estado", 1:4)

# Imprimir matriz de transición
print("Matriz de transición promedio:")
print(round(trans_matrix, 3))

# Visualizar transiciones como heatmap
trans_df <- as.data.frame(as.table(trans_matrix))
names(trans_df) <- c("Estado_origen", "Estado_destino", "Probabilidad")

ggplot(trans_df, aes(x = Estado_origen, y = Estado_destino, fill = Probabilidad)) +
  geom_tile() +
  scale_fill_viridis() +
  theme_minimal() +
  labs(title = "Probabilidades de transición entre estados",
       x = "Estado de origen",
       y = "Estado de destino")


# Efectos en probabilidades iniciales
be_df <- as.data.frame(modelo_lm$Be)
names(be_df) <- paste("logit", 1:ncol(be_df))
be_df$Variable <- rownames(modelo_lm$Be)

# Convertir a formato largo para visualización
be_long <- tidyr::pivot_longer(be_df, 
                               cols = starts_with("logit"),
                               names_to = "Logit",
                               values_to = "Coeficiente")

# Visualizar efectos de covariables en probabilidades iniciales
ggplot(be_long, aes(x = Variable, y = Coeficiente, fill = Logit)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Efectos de covariables en probabilidades iniciales",
       x = "Variable",
       y = "Coeficiente")




# profundizando en efectos de covariables. 

# Crear una tabla más legible de probabilidades de respuesta por estado
prob_estado <- modelo_lm$Psi
dimnames(prob_estado)[[1]] <- c("Bajo", "Medio", "Alto")  # Categorías de respuesta
dimnames(prob_estado)[[2]] <- paste("Estado", 1:4)        # Estados
dimnames(prob_estado)[[3]] <- c("Item1", "Item2", "Item3", "Item4")  # Items

# Función para imprimir las probabilidades de manera formateada
print_probs <- function(prob_array) {
  for(i in 1:dim(prob_array)[3]) {
    cat("\nItem", i, ":\n")
    print(round(prob_array[,,i], 3))
  }
}

print_probs(prob_estado)



# Modelo de 3 clases -----------------------------------------------------------

# Reestimamos el modelo con 3 clases
modelo_lm3 <- lmest(responsesFormula = responsesFormula,
                    latentFormula = latentFormula,
                    index = c("folio", "ola"),
                    data = data_lmest,
                    k = 3,  # Fijamos en 3 clases
                    modSel = "BIC",
                    start = 1,
                    out_se = TRUE)  # Para obtener errores estándar
                    #seed = 1234)


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
grid.arrange()

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
save_plot(p1, "p1.png", width = 10, height = 8)



# Calcular proporciones usando Piv
proporciones <- colMeans(modelo_lm3$Piv)
proporciones <- round(proporciones * 100, 1)

# Verificar las proporciones
print("Proporción de la muestra en cada clase:")
print(proporciones)

# Crear etiquetas para las clases con sus proporciones
class_labels <- paste("Clase", 1:3, "\n(", proporciones, "%)", sep = "")

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
names(be_df) <- paste("Estado", 2:3)  # Estado 1 es referencia
be_df$Variable <- rownames(modelo_lm3$Be)

se_df <- as.data.frame(errores_estandar$seBe)
names(se_df) <- paste("Estado", 2:3)
se_df$Variable <- rownames(modelo_lm3$Be)

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
orden_deseado <- rev(c("intercept", "urbano_rural", "mujer1", "edad25_34", "edad35_44", "edad45_54", "edad55_64", "edad65+",  "c1", "c2", "d5_1", "d5_2", "d6_1", "c27_1", "c27_3"))

# Convertimos la variable 'Variable' en un factor con el orden especificado
be_long$Variable <- factor(be_long$Variable, levels = orden_deseado)

# Definimos los colores manualmente
colores_manuales <- c("Estado 2" = "#4b3f72", "Estado 3" = "#ffc857")

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

# Crear datos para las transiciones
transitions <- tibble(
  from = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  to = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  probability = c(0.58, 0.23, 0.19,  # Desde clase 1
                  0.39, 0.43, 0.18,   # Desde clase 2
                  0.37, 0.25, 0.39)   # Desde clase 3
)

# Crear datos para los nodos con tamaños basados en probabilidad de permanencia
nodes <- tibble(
  name = 1:3,
  label = paste("Clase", 1:3),
  size = c(0.58, 0.43, 0.39),  # Probabilidades de permanencia
  prop = c(56, 24, 20)      # Proporción de la muestra
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















