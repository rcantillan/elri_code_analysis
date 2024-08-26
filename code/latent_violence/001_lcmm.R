## Análisis Invarianza ELRI --------------------------------
library(semTools)
#install.packages("XQuartz")
#library(influence.SEM)
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
         "urbano_rural", "indi", "mujer", "edad")

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
subset_data_clean <- subset_data[complete.cases(subset_data), ]

# Modelo -----------------------------------------------------------------------
## Modelo 1
modelos <-  lmest(responsesFormula = d3_1_red + d3_2_red + d4_2_red + d4_3_red ~ NULL,
                  latentFormula =~ urbano_rural + mujer + edad + c1 + c2 + d5_2 + d6_1 + c27_1 + c27_3,
                  index = c("folio","ola"),
                  output = TRUE,
                  out_se = TRUE,
                  paramLatent = "multilogit",
                  data = subset_data,
                  k = 1:6,
                  start = 0,
                  modBasic = 1,
                  modManifest="LM",
                  seed = 1234)



#`modBasic` model on the transition probabilities: default 0 for time-heterogeneous transition
#matrices, 1 for time-homogeneous transition matrices, 2 for partial time homogeneity based 
#on two transition matrices one from 2 to (TT-1) and the other for TT.

#modManifest model for manifest distribution 
#("LM" = Latent Markov with stationary transition, "FM" = finite mixture model) 
#where a mixture of AR(1) processes is estimated with common variance and specific correlation coefficients.


plot(modelos,what="modSel")
plot(modelos, what = "CondProb")
plot(modelos, what="marginal")


## Modelo 3c
## Variables que puedan ser interesantes. 


modelo_3c <-  lmest(responsesFormula = d3_1_red + d3_2_red + d4_2_red + d4_3_red ~ NULL,
                    latentFormula =~ urbano_rural + mujer + edad + c1 + c2 + d5_2 + d6_1 + c27_1 + c27_3,
                    index = c("folio","ola"),
                    output = TRUE,
                    out_se = TRUE,
                    paramLatent = "multilogit",
                    data = subset_data,
                    k = 3,
                    start = 0,
                    modBasic = 1,
                    modManifest="LM",
                    seed = 1234)


plot(modelo_3c, what = "CondProb") #transitions
plot(modelo_3c, what="marginal")
plot(modelo_3c, what = "transitions")

## Resultados de regresión multinomial para describir grupos.
Be<-as.data.frame(modelo_3c$Be)
seBe<-as.data.frame(modelo_3c$seBe)
z <- modelo_3c$Be/modelo_3c$seBe
p <- (1 - pnorm(abs(z), 0, 1))*2 # two-tailed z test

## Tablas
options(scipen=999)
print(xtable(Be, type = "latex"), file = "long_latent_class/multinom_coeff.tex")
print(xtable(p, type = "latex"), file = "long_latent_class/multinom_pvaluestex")

# Probabilidades iniciales 
plot(modelo3, what="CondProb")


# bootstrap
mboot <- bootstrap(modelo_3c, B = 1000, seed = 172)


# Probabilidades de transición
# Probabilidades individuales de transición. 
trans_ind <- modelo_3c$V   

tras_ind1 <- trans_ind[, , 1]
colnames(tras_ind1) <- c("t1_t2_clase1", "t1_t2_clase2", "t1_t2_clase3")
tras_ind1 <- as_tibble(tras_ind1)

tras_ind2 <- trans_ind[, , 2]
colnames(tras_ind2) <- c("t1_t3_clase1", "t1_t3_clase2", "t1_t3_clase3")
tras_ind2 <- as_tibble(tras_ind2)

tras_ind3 <- trans_ind[, , 3]
colnames(tras_ind3) <- c("t2_t3_clase1", "t2_t3_clase2", "t2_t3_clase3")
tras_ind3 <- as_tibble(tras_ind3)




## Modelo 4c
modelo_4c <-  lmest(responsesFormula = d3_1_red + d3_2_red + d4_2_red + d4_3_red ~ NULL,
                  latentFormula =~ urbano_rural + mujer + edad + c1 + c2 + d5_2 + d6_1 + c27_1 + c27_3,
                  index = c("folio","ola"),
                  output = TRUE,
                  out_se = TRUE,
                  paramLatent = "multilogit",
                  data = subset_data,
                  k = 4,
                  start = 0,
                  modBasic = 1,
                  modManifest="LM",
                  seed = 1234)


#Acá

plot(modelo_3c, what = "CondProb")
plot(modelo_3c, what="marginal")
plot(modelo_3c, what = "transitions")


# Abrir un dispositivo gráfico para guardar el primer gráfico
png("code/latent_violence/marginal_modelo_3c.png", width = 800, height = 600)
plot(modelo_3c, what="marginal")
dev.off()  # Cerrar el dispositivo gráfico


# Abrir un dispositivo gráfico para guardar el segundo gráfico
png("code/latent_violence/transitions_modelo_3c.png", width = 800, height = 600)
plot(modelo_3c, what = "transitions")
dev.off()  # Cerrar el dispositivo gráfico



# plots 
LMmodelo <- reshape2::melt(modelo_3c$Psi, level=1)
glimpse(LMmodelo)
LMmodelo <- LMmodelo %>% mutate(value = round(value * 100))


ggplot(LMmodelo, aes(x = factor(item), y = value, fill = factor(category))) +
  geom_col(position = "stack") +
  facet_wrap(~ state, ncol = 1, labeller = labeller(state = function(x) paste("Estado", x))) +
  scale_fill_brewer(palette = "Set2", name = "Categoría") +
  labs(x = "Ítem", y = "Probabilidad", 
       title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "right")



library(gridExtra)
grid.arrange()

# Creamos un vector con las descripciones de los ítems
item_descriptions <- c(
  "1" = "El uso de la fuerza por\nparte de Carabineros para\ndisolver protestas de\ngrupos indígenas",
  "2" = "Que agricultores usen\narmas para enfrentar a\ngrupos de personas\nindígenas",
  "3" = "Que grupos de personas\nindígenas se tomen\nterrenos que se\nconsideran propios",
  "4" = "El bloqueo o corte de\ncarreteras por parte de\ngrupos de personas\nindígenas"
)

ggplot(LMmodelo, aes(x = factor(item), y = value, fill = factor(category))) +
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

# Guardamos 
ggsave("code/latent_violence/probabilidad_respuesta1.jpg", width = 12, height = 10, dpi = 300)



# Modelos usando lcmm-----------------------------------------------------------

modelo_1clase <- multlcmm(d3_1 + d3_2 + d4_2 + d4_3 ~ ola,
                          random = ~ 1,
                          subject = 'folio',
                          ng = 1,
                          data = subset_data,
                          link = c("thresholds", "thresholds", "thresholds", "thresholds"),
                          maxiter = 100,
                          convB = 1e-3,
                          convL = 1e-3,
                          convG = 1e-3,
                          nproc = 8)



modelo_2clase <- gridsearch(
  multlcmm(d3_1 + d3_2 + d4_2 + d4_3 ~ ola,
           random = ~ ola,
           subject = 'folio',
           mixture = ~ ola,
           classmb = ~ c1 + c2 + d5_1 + d5_2 + d6_1,  # Reducido el número de covariables
           link = c("thresholds", "thresholds", "thresholds", "thresholds"),
           ng = 2,
           data = subset_data,
           maxiter = 100,
           convB = 1e-3,
           convL = 1e-3,
           convG = 1e-3,
           nproc = 4),
  rep = 10,
  maxiter = 20,
  minit = modelo_1clase
)



modelo_2clase <- multlcmm(d3_1 + d3_2 + d4_2 + d4_3 ~ ola,
                          random = ~ ola,
                          subject = 'folio',
                          mixture = ~ ola,
                          classmb = ~ c1 + c2 + d5_1 + d5_2 + d6_1 + c27_1 + c27_3,
                          link = c("thresholds", "thresholds", "thresholds", "thresholds"),
                          ng = 2,
                          data = subset_data, 
                          B = modelo_1clase)


modelo_3clase <- multlcmm(d3_1 + d3_2 + d4_2 + d4_3 ~ ola,
                          random = ~ ola,
                          subject = 'folio',
                          mixture = ~ ola,
                          classmb = ~ c1 + c2 + d5_1 + d5_2 + d6_1 + c27_1 + c27_3,
                          link = c("thresholds", "thresholds", "thresholds", "thresholds"),
                          ng = 3,
                          data = subset_data, 
                          B = modelo_1clase)


modelo_4clase <- multlcmm(d3_1 + d3_2 + d4_2 + d4_3 ~ ola,
                          random = ~ ola,
                          subject = 'folio',
                          mixture = ~ ola,
                          classmb = ~ c1 + c2 + d5_1 + d5_2 + d6_1 + c27_1 + c27_3,
                          link = c("thresholds", "thresholds", "thresholds", "thresholds"),
                          ng = 4,
                          data = subset_data, 
                          B = modelo_1clase)

modelo_5clase <- multlcmm(d3_1 + d3_2 + d4_2 + d4_3 ~ ola,
                          random = ~ ola,
                          subject = 'folio',
                          mixture = ~ ola,
                          classmb = ~ c1 + c2 + d5_1 + d5_2 + d6_1 + c27_1 + c27_3,
                          link = c("thresholds", "thresholds", "thresholds", "thresholds"),
                          ng = 5,
                          data = subset_data, 
                          B = modelo_1clase)




crear_modelos_multlcmm_eficiente <- function(data, max_clases = 5, n_cores = 1) {
  require(lcmm)
  modelos <- list()
  
  # Modelo de 1 clase
  modelos[[1]] <- multlcmm(d3_1 + d3_2 + d4_2 + d4_3 ~ ola,
                           random = ~ ola,
                           subject = 'folio',
                           ng = 1,
                           data = data,
                           link = c("thresholds", "thresholds", "thresholds", "thresholds"),
                           maxiter = 200,
                           convB = 1e-3,
                           convL = 1e-3,
                           convG = 1e-3)
  
  # Función para intentar ajustar el modelo con y sin partialH
  ajustar_modelo <- function(i) {
    tryCatch({
      multlcmm(d3_1 + d3_2 + d4_2 + d4_3 ~ ola,
               random = ~ ola,
               subject = 'folio',
               mixture = ~ ola,
               classmb = ~ c1 + c2 + d5_1 + d5_2 + d6_1 + c27_1 + c27_3,
               link = c("thresholds", "thresholds", "thresholds", "thresholds"),
               ng = i,
               data = data,
               B = modelos[[1]],
               nproc = n_cores,
               maxiter = 200,
               convB = 1e-3,
               convL = 1e-3,
               convG = 1e-3,
               partialH = TRUE)
    }, error = function(e) {
      message("Intentando sin partialH para ng = ", i)
      multlcmm(d3_1 + d3_2 + d4_2 + d4_3 ~ ola,
               random = ~ ola,
               subject = 'folio',
               mixture = ~ ola,
               classmb = ~ c1 + c2 + d5_1 + d5_2 + d6_1 + c27_1 + c27_3,
               link = c("thresholds", "thresholds", "thresholds", "thresholds"),
               ng = i,
               data = data,
               B = modelos[[1]],
               nproc = n_cores,
               maxiter = 200,
               convB = 1e-3,
               convL = 1e-3,
               convG = 1e-3,
               partialH = FALSE)
    })
  }
  
  # Modelos de 2 a max_clases
  for (i in 2:max_clases) {
    modelos[[i]] <- ajustar_modelo(i)
  }
  
  return(modelos)
}

# Uso de la función
modelos <- crear_modelos_multlcmm_eficiente(subset_data, max_clases = 5, n_cores = 4)

# Acceder a los modelos individuales
modelo_1clase <- modelos[[1]]
modelo_2clase <- modelos[[2]]
modelo_3clase <- modelos[[3]]
modelo_4clase <- modelos[[4]]
modelo_5clase <- modelos[[5]]

# Comparar modelos
summarytable(modelo_1clase, modelo_2clase, modelo_3clase, modelo_4clase, modelo_5clase)

