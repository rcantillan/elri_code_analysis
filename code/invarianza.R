

## Análisis Invarianza ELRI --------------------------------
library(semTools)
library(influence.SEM)
library(dplyr)
library(lavaan)
library(psych)   
library(semPlot)
library(car)
library(stringr)
library(sjlabelled)
library(sjmisc)
library(here)

# limpiar espacio
cat("\014")
rm(list = ls())
gc()

# Load data
load(here("data/BBDD_ELRI_LONG.RData"))

# Análisis 1
# seleccionar datos ---------------------------------------------------------
elri_inv <- BBDD_ELRI_LONG %>% 
  remove_all_labels() %>% 
  mutate(
    g5_1 = coalesce(g5_1, g5_1_v1, g5_1_v2),
    g5_2 = coalesce(g5_2, g5_2_v1, g5_2_v2),
    g5_3 = coalesce(g5_3, g5_3_v1, g5_3_v2),
    g5_4 = coalesce(g5_4, g5_4_v1, g5_4_v2),
    g5_5 = coalesce(g5_5, g5_5_v1, g5_5_v2),
    g5_6 = coalesce(g5_6, g5_6_v1, g5_6_v2),
    g5_7 = coalesce(g5_7, g5_7_v1, g5_7_v2),
    g5_8 = coalesce(g5_8, g5_8_v1, g5_8_v2),
    g5_9 = coalesce(g5_9, g5_9_v1, g5_9_v2), 
    across(c(a1, g5_1:g5_9), ~na_if(., 88)),
    across(c(a1, g5_1:g5_9), ~na_if(., 99)),
    across(c(a1, g5_1:g5_9), ~na_if(., 8888)),
    across(c(a1, g5_1:g5_9), ~na_if(., 9999))) %>% 
  mutate(indi = case_when(a1 %in% 1:11 ~ "indi",
                          a1 == 12 ~ "no_indi")) %>% 
  filter(!is.na(indi)) %>% 
  select(folio, ano, indi, g5_1, g5_2, g5_3, g5_4, g5_5, g5_6, g5_7, g5_8, g5_9) 


rm(BBDD_ELRI_LONG)
glimpse(elri_inv)
elri_inv %>% frq(ano)
elri_inv %>% frq(indi)


# Separar datos por año
data_2016 <- elri_inv %>% filter(ano == 2016)
data_2018 <- elri_inv %>% filter(ano == 2018)
data_2021 <- elri_inv %>% filter(ano == 2021)
data_2023 <- elri_inv %>% filter(ano == 2023)

# Separar datos por grupo indi/no_indi
data_indi <- elri_inv %>% filter(indi == "indi")
data_no_indi <- elri_inv %>% filter(indi == "no_indi")

# Definir modelos de medición
model_1f <- '
  depression =~ g5_1 + g5_2 + g5_3 + g5_4 + g5_5 + g5_6 + g5_7 + g5_8 + g5_9
'

model_2f <- '
  somatic =~ g5_3 + g5_4 + g5_5 + g5_7 + g5_8
  nonsomatic =~ g5_1 + g5_2 + g5_6 + g5_9
'

# Invarianza longitudinal ------------------------------------------------

# Modelo de un factor
fit_1f_configural_year <- cfa(model_1f,
                              data = elri_inv,
                              group = "ano",
                              estimator = "WLSMV",
                              ordered = paste0("g5_", 1:9))

fit_1f_metric_year <- cfa(model_1f,
                          data = elri_inv,
                          group = "ano",
                          group.equal = c("loadings"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

fit_1f_scalar_year <- cfa(model_1f,
                          data = elri_inv,
                          group = "ano",
                          group.equal = c("loadings", "thresholds"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

# Comparar modelos anidados
anova(fit_1f_configural_year, fit_1f_metric_year)
anova(fit_1f_metric_year, fit_1f_scalar_year)

# Modelo de dos factores
fit_2f_configural_year <- cfa(model_2f,
                              data = elri_inv,
                              group = "ano",
                              estimator = "WLSMV",
                              ordered = paste0("g5_", 1:9))

fit_2f_metric_year <- cfa(model_2f,
                          data = elri_inv,
                          group = "ano",
                          group.equal = c("loadings"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

fit_2f_scalar_year <- cfa(model_2f,
                          data = elri_inv,
                          group = "ano",
                          group.equal = c("loadings", "thresholds"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

# Comparar modelos anidados
anova(fit_2f_configural_year, fit_2f_metric_year)
anova(fit_2f_metric_year, fit_2f_scalar_year)

# Invarianza intergrupal (indi vs no_indi) para cada año -------------------

# 2016 -------------------------------------------------------------------

# Modelo de un factor
fit_2016_1f_configural <- cfa(model_1f,
                              data = data_2016,
                              group = "indi",
                              estimator = "WLSMV",
                              ordered = paste0("g5_", 1:9))

fit_2016_1f_metric <- cfa(model_1f,
                          data = data_2016,
                          group = "indi",
                          group.equal = c("loadings"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

fit_2016_1f_scalar <- cfa(model_1f,
                          data = data_2016,
                          group = "indi",
                          group.equal = c("loadings", "thresholds"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

# Comparar modelos anidados
anova(fit_2016_1f_configural, fit_2016_1f_metric)
anova(fit_2016_1f_metric, fit_2016_1f_scalar)

# Modelo de dos factores
fit_2016_2f_configural <- cfa(model_2f,
                              data = data_2016,
                              group = "indi",
                              estimator = "WLSMV",
                              ordered = paste0("g5_", 1:9))

fit_2016_2f_metric <- cfa(model_2f,
                          data = data_2016,
                          group = "indi",
                          group.equal = c("loadings"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

fit_2016_2f_scalar <- cfa(model_2f,
                          data = data_2016,
                          group = "indi",
                          group.equal = c("loadings", "thresholds"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

# Comparar modelos anidados
anova(fit_2016_2f_configural, fit_2016_2f_metric)
anova(fit_2016_2f_metric, fit_2016_2f_scalar)

# 2018 -------------------------------------------------------------------

# Modelo de un factor
fit_2018_1f_configural <- cfa(model_1f,
                              data = data_2018,
                              group = "indi",
                              estimator = "WLSMV",
                              ordered = paste0("g5_", 1:9))

fit_2018_1f_metric <- cfa(model_1f,
                          data = data_2018,
                          group = "indi",
                          group.equal = c("loadings"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

fit_2018_1f_scalar <- cfa(model_1f,
                          data = data_2018,
                          group = "indi",
                          group.equal = c("loadings", "thresholds"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

# Comparar modelos anidados
anova(fit_2018_1f_configural, fit_2018_1f_metric)
anova(fit_2018_1f_metric, fit_2018_1f_scalar)

# Modelo de dos factores
fit_2018_2f_configural <- cfa(model_2f,
                              data = data_2018,
                              group = "indi",
                              estimator = "WLSMV",
                              ordered = paste0("g5_", 1:9))

fit_2018_2f_metric <- cfa(model_2f,
                          data = data_2018,
                          group = "indi",
                          group.equal = c("loadings"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

fit_2018_2f_scalar <- cfa(model_2f,
                          data = data_2018,
                          group = "indi",
                          group.equal = c("loadings", "thresholds"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

# Comparar modelos anidados
anova(fit_2018_2f_configural, fit_2018_2f_metric)
anova(fit_2018_2f_metric, fit_2018_2f_scalar)

# 2021 --------------------------------------------------------------------

# Modelo de un factor  
fit_2021_1f_configural <- cfa(model_1f,
                              data = data_2021,
                              group = "indi",
                              estimator = "WLSMV",
                              ordered = paste0("g5_", 1:9))

fit_2021_1f_metric <- cfa(model_1f,
                          data = data_2021,
                          group = "indi",
                          group.equal = c("loadings"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

fit_2021_1f_scalar <- cfa(model_1f,
                          data = data_2021,
                          group = "indi",
                          group.equal = c("loadings", "thresholds"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

# Comparar modelos anidados
anova(fit_2021_1f_configural, fit_2021_1f_metric)
anova(fit_2021_1f_metric, fit_2021_1f_scalar)

# Modelo de dos factores
fit_2021_2f_configural <- cfa(model_2f,
                              data = data_2021,
                              group = "indi",
                              estimator = "WLSMV",
                              ordered = paste0("g5_", 1:9))

fit_2021_2f_metric <- cfa(model_2f,
                          data = data_2021,
                          group = "indi",
                          group.equal = c("loadings"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

fit_2021_2f_scalar <- cfa(model_2f,
                          data = data_2021,
                          group = "indi",
                          group.equal = c("loadings", "thresholds"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

# Comparar modelos anidados
anova(fit_2021_2f_configural, fit_2021_2f_metric)
anova(fit_2021_2f_metric, fit_2021_2f_scalar)

# 2023 -------------------------------------------------------------------

# Modelo de un factor
fit_2023_1f_configural <- cfa(model_1f,
                              data = data_2023,
                              group = "indi",
                              estimator = "WLSMV",
                              ordered = paste0("g5_", 1:9))

fit_2023_1f_metric <- cfa(model_1f,
                          data = data_2023,
                          group = "indi",
                          group.equal = c("loadings"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

fit_2023_1f_scalar <- cfa(model_1f,
                          data = data_2023,
                          group = "indi",
                          group.equal = c("loadings", "thresholds"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

# Comparar modelos anidados
anova(fit_2023_1f_configural, fit_2023_1f_metric)
anova(fit_2023_1f_metric, fit_2023_1f_scalar)

# Modelo de dos factores
fit_2023_2f_configural <- cfa(model_2f,
                              data = data_2023,
                              group = "indi",
                              estimator = "WLSMV",
                              ordered = paste0("g5_", 1:9))

fit_2023_2f_metric <- cfa(model_2f,
                          data = data_2023,
                          group = "indi",
                          group.equal = c("loadings"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

fit_2023_2f_scalar <- cfa(model_2f,
                          data = data_2023,
                          group = "indi",
                          group.equal = c("loadings", "thresholds"),
                          estimator = "WLSMV",
                          ordered = paste0("g5_", 1:9))

# Comparar modelos anidados
anova(fit_2023_2f_configural, fit_2023_2f_metric)
anova(fit_2023_2f_metric, fit_2023_2f_scalar)

