# =========================
# Paquetes
# =========================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  ggplot2, ggraph, igraph, tidygraph, reshape2,
  gridExtra, grid, ggpubr, cowplot, knitr, kableExtra,
  dplyr, tidyr
)




#* trato con las policías (justicia procidimental).
#* teoría del contacto.
#* teoría de la identidad social (---).
#* confianza intergrupal.
#* identificación con la causa.
#* ¿Voto constituyente..? para la ola 4.

# --- Confianza --- #
#c2 En general, ¿cuánto confía en los chilenos no indígenas?
#c5 Confianza en los pueblos originarios
# --- Respeto---- #
#c27_1  En general, las personas respetan a los (% PUEBLO ORIGINARIO)
#c27_3  En general, las personas respetan a los chilenos no indígenas
# --- Percepción de conflicto ---- #
#d1_1   Conflicto diría usted que existe actualmente entre...El Estado chileno y los pueblos originarios?
#d1_2   Conflicto diría usted que existe actualmente entre...Indígenas y no-indígenas?
#---- Justicia procidimental ---- #
#d5_1   En general, los Carabineros tratan a las personas indígenas con respeto.
#d5_2   En general, los Carabineros tratan a las personas NO indígenas con respeto.
#---- Teoría identidad social  -----#
#d6_1   Me siento identificado con la causa de los pueblos indígenas
#---- Teoría del contacto -----#
#c7_2   Con qué frecuencia conversa o interactúa con personas chilenas no-indígenas?
#c13    ¿Con qué frecuencia conversa o interactúa con personas (% PUEBLO ORIGINARIO)?



# =========================
# Cargar modelo
# =========================
modelo_lm4 <- readRDS("code/latent_violence/modelo/modelo_4c.rds")


# Visualizar matriz de transición
plot(modelo_lm4, what = "transitions")

# Examinar probabilidades de transición específicas
print(round(apply(modelo_lm4$Pi, c(1,2), mean), 3))

# Probabilidades de respuesta por estado
print(round(modelo_lm4$Psi, 3))

# Visualización de probabilidades condicionales
plot(modelo_lm4, what = "CondProb")

# Distribución marginal de estados
plot(modelo_lm4, what = "marginal")

# =========================
# 1) Etiquetas de clases con la mayoritaria como referencia
# =========================
props <- colMeans(modelo_lm4$Piv)
props_perc <- round(100 * props, 1)
ref_idx <- which.max(props)  # esperamos 1
if (ref_idx != 1) {
  message(sprintf("ATENCIÓN: la clase mayoritaria es la %d. 
Se mantiene la referencia en 1 (modelo), pero las etiquetas tratarán a la mayoritaria como 'Universal Rejecters'.",
                  ref_idx))
}
class_names <- c(
  "Universal Rejecters of Violence",
  "Pro Control Social",
  "Pro-Indigenous Force Sympathizers",
  "Violentista"
)
names(class_names) <- as.character(1:length(class_names))
class_labels <- paste0(class_names, " (", props_perc, "%)")
names(class_labels) <- as.character(1:length(class_labels))

# =========================
# 2) Probabilidades condicionales (gráfico apilado)
# =========================
LMmodelo <- reshape2::melt(modelo_lm4$Psi, level = 1) |>
  mutate(value = round(value * 100))

item_descriptions <- c(
  "1" = "The use of force by\nCarabineros to break up\nprotests by Indigenous\ngroups",
  "2" = "Farmers using weapons\nto confront groups of\nIndigenous people",
  "3" = "Indigenous groups\noccupying land they\nconsider their own",
  "4" = "Road blockades or\nclosures by Indigenous\ngroups"
)

p_cond <- ggplot(
  LMmodelo,
  aes(x = factor(item), y = value, fill = factor(category))
) +
  geom_col(position = "stack") +
  facet_wrap(
    ~ state, ncol = 1,
    labeller = labeller(state = function(x) class_labels[as.character(x)])
  ) +
  scale_fill_manual(
    values = c("#1f2041", "#ffc857"),
    name   = "Justification of Violence",
    labels = c("Never", "Sometimes")
  ) +
  scale_x_discrete(labels = item_descriptions) +
  labs(x = NULL, y = "P(y)") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(
      angle = 0, color = "black", hjust = 0.5, vjust = 1,
      lineheight = 0.8, size = 11
    ),
    legend.position = "right",
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 14),
    strip.text   = element_text(size = 12),
    panel.spacing = unit(1, "lines")
  )

print(p_cond)

# =========================
# 3) Resultados multinomiales (baseline = clase 1)
# =========================
errores_estandar <- se(modelo_lm4)
be_df <- as.data.frame(modelo_lm4$Be)              # coef (k-1) vs clase 1
colnames(be_df) <- paste0("Class ", 1:ncol(be_df)) # "Class 1..3" (≡ estados 2..4 vs 1)
be_df$Variable <- rownames(be_df)

se_df <- as.data.frame(errores_estandar$seBe)
colnames(se_df) <- paste0("Class ", 1:ncol(se_df))
se_df$Variable <- rownames(se_df)

be_long <- be_df |>
  pivot_longer(
    cols = starts_with("Class"),
    names_to = "Estado", values_to = "Coeficiente"
  ) |>
  left_join(
    se_df |>
      pivot_longer(
        cols = starts_with("Class"),
        names_to = "Estado", values_to = "SE"
      ),
    by = c("Variable", "Estado")
  ) |>
  mutate(
    IC_lower = Coeficiente - 1.96 * SE,
    IC_upper = Coeficiente + 1.96 * SE,
    OR = exp(Coeficiente),
    OR_IC_lower = exp(IC_lower),
    OR_IC_upper = exp(IC_upper),
    significativo = (IC_lower * IC_upper > 0)
  )

orden_vars <- c(
  "intercept", "urbano_rural", "mujer1",
  "edad25_34", "edad35_44", "edad45_54", "edad55_64", "edad65+",
  "c1", "c2", "c27_1", "c27_3", "d5_2", "d6_1"
)
be_long$Variable <- factor(be_long$Variable)

tabla_resultados <- be_long |>
  arrange(Variable, Estado) |>
  select(
    Variable, Estado, Coeficiente, SE, IC_lower, IC_upper,
    OR, OR_IC_lower, OR_IC_upper, significativo
  )
print(knitr::kable(tabla_resultados, digits = 3))

colores_manuales <- c("Class 1" = "#4b3f72", "Class 2" = "#ffc857", "Class 3" = "#2a9d8f")
p_coef <- ggplot(be_long, aes(x = Variable, y = Coeficiente, color = Estado)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(
    aes(ymin = IC_lower, ymax = IC_upper),
    position = position_dodge(width = 0.5), width = 0.5
  ) +
  coord_flip() +
  theme_minimal() +
  scale_color_manual(
    values = colores_manuales,
    labels = c("vs Class 2", "vs Class 3", "vs Class 4")
  ) +
  labs(
    x = "", y = "Coeficiente (log-odds)",
    color = "Comparación vs referencia\n(Universal Rejecters)"
  ) +
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    panel.spacing = unit(1, "lines")
  )

print(p_coef)

# =========================
# 4) Transiciones
# =========================

# 4a) Model-based (promedio PI) -> MATRIZ (no tibble)
trans_matrix_model <- apply(modelo_lm4$PI, c(1,2), mean, na.rm = TRUE)
rn <- paste0(1:nrow(trans_matrix_model), ": ", class_labels)
cn <- paste0(1:ncol(trans_matrix_model), ": ", class_labels)
rownames(trans_matrix_model) <- rn
colnames(trans_matrix_model) <- cn

cat("\nMatriz de transición (model-based, promedio PI) – proporciones:\n")
print(round(trans_matrix_model, 3))
cat("\nMatriz de transición (model-based) – % por fila:\n")
print(round(100 * prop.table(trans_matrix_model, 1), 1))

# 4b) Transiciones EMPÍRICAS (decodificación robusta desde V)
k  <- modelo_lm4$k
TT <- modelo_lm4$TT
V  <- modelo_lm4$V

stopifnot(length(dim(V)) == 3)
dims <- dim(V)

# Helper: devuelve matriz S (n x TT) con el estado más probable por sujeto y ola
get_S_from_V <- function(V, k, TT) {
  d <- dim(V)
  # Casos plausibles de disposición de V
  # 1) (n, k, TT)
  if (d[2] == k && d[3] == TT) {
    return(apply(V, c(1, 3), which.max))
  }
  # 2) (n, TT, k)
  if (d[2] == TT && d[3] == k) {
    V_ap <- aperm(V, c(1, 3, 2)) # -> (n, k, TT)
    return(apply(V_ap, c(1, 3), which.max))
  }
  # 3) (k, n, TT)
  if (d[1] == k && d[3] == TT) {
    V_ap <- aperm(V, c(2, 1, 3)) # -> (n, k, TT)
    return(apply(V_ap, c(1, 3), which.max))
  }
  # 4) (TT, n, k)
  if (d[1] == TT && d[3] == k) {
    V_ap <- aperm(V, c(2, 3, 1)) # -> (n, k, TT)
    return(apply(V_ap, c(1, 3), which.max))
  }
  # 5) (k, TT, n)
  if (d[1] == k && d[2] == TT) {
    V_ap <- aperm(V, c(3, 1, 2)) # -> (n, k, TT)
    return(apply(V_ap, c(1, 3), which.max))
  }
  # 6) (TT, k, n)
  if (d[1] == TT && d[2] == k) {
    V_ap <- aperm(V, c(3, 2, 1)) # -> (n, k, TT)
    return(apply(V_ap, c(1, 3), which.max))
  }
  stop("No se pudo inferir el orden de las dimensiones de V.")
}

S_mat <- get_S_from_V(V, k, TT)  # n x TT

# Conteos t -> t+1
counts_emp <- matrix(0L, nrow = k, ncol = k)
for (t in 1:(TT - 1)) {
  tab <- table(
    factor(S_mat[, t],   levels = 1:k),
    factor(S_mat[, t+1], levels = 1:k)
  )
  counts_emp <- counts_emp + as.matrix(tab)
}
rownames(counts_emp) <- rn
colnames(counts_emp) <- cn

cat("\nTransiciones empíricas (conteos 'número real'):\n")
print(counts_emp)

props_emp <- counts_emp / pmax(rowSums(counts_emp), 1)
cat("\nTransiciones empíricas – proporción por fila:\n")
print(round(props_emp, 3))

cat("\nTransiciones empíricas – % por fila:\n")
print(round(100 * props_emp, 1))

# =========================
# 5) (Opcional) Exportar tablas/matrices a CSV
# =========================
# dir.create("code/latent_violence/output", showWarnings = FALSE, recursive = TRUE)
# write.csv(tabla_resultados, "code/latent_violence/output/tabla_multilogit_baseline_mayoritaria.csv", row.names = FALSE)
# write.csv(round(trans_matrix_model, 4), "code/latent_violence/output/transiciones_model_based_PI.csv")
# write.csv(counts_emp, "code/latent_violence/output/transiciones_empiricas_conteos.csv")
# write.csv(round(props_emp, 4), "code/latent_violence/output/transiciones_empiricas_prop.csv")



# =========================
# Gráficos de transiciones (heatmaps)
# =========================

# ========= HEATMAPS DE TRANSICIONES (ROBUSTO) =========
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, dplyr, tidyr, cowplot, scales, tibble)

out_dir <- "code/latent_violence/output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Asegurar que tengan nombres de fila/columna
if (is.null(rownames(trans_matrix_model))) rownames(trans_matrix_model) <- rn
if (is.null(colnames(trans_matrix_model))) colnames(trans_matrix_model) <- cn
if (is.null(rownames(props_emp))) rownames(props_emp) <- rn
if (is.null(colnames(props_emp))) colnames(props_emp) <- cn

# Forzar a matrix si vienen como 'table' (o dejar tabla y tratarla)
force_matrix_if_table <- function(x){
  if (inherits(x, "table")) {
    # reconstruir matriz a partir de tabla
    df <- as.data.frame(x, stringsAsFactors = FALSE)
    # Asumimos primer col = from, segunda = to, tercera = Freq
    froms <- unique(df[[1]])
    tos   <- unique(df[[2]])
    M <- matrix(0, nrow = length(froms), ncol = length(tos),
                dimnames = list(as.character(froms), as.character(tos)))
    for (i in seq_len(nrow(df))) {
      M[as.character(df[i,1]), as.character(df[i,2])] <- df[i,3]
    }
    return(M)
  }
  as.matrix(x)
}

trans_matrix_model <- force_matrix_if_table(trans_matrix_model)
props_emp          <- force_matrix_if_table(props_emp)

# Función robusta a matrix o table (ya convertidos a matrix arriba)
mat_to_long <- function(mat, title){
  mat <- as.matrix(mat)
  df <- mat |>
    as.data.frame(check.names = FALSE) |>
    rownames_to_column("from") |>
    pivot_longer(-from, names_to = "to", values_to = "val") |>
    group_by(from) |>
    mutate(
      rs = sum(val, na.rm = TRUE),
      pct_row = ifelse(is.finite(rs) & rs > 0, val / rs, 0)
    ) |>
    ungroup() |>
    mutate(
      pct_lab = sprintf("%.1f%%", 100 * pct_row),
      panel   = title
    ) |>
    select(from, to, pct_row, pct_lab, panel)
  return(df)
}

df_model <- mat_to_long(trans_matrix_model, "Model-based (avg. PI)")
df_emp   <- mat_to_long(props_emp,           "Empirical (decoded)")

# Orden de ejes
lvl_from <- unique(df_model$from)
lvl_to   <- unique(df_model$to)
df_model$from <- factor(df_model$from, levels = rev(lvl_from))
df_model$to   <- factor(df_model$to,   levels = lvl_to)
df_emp$from   <- factor(df_emp$from,   levels = rev(lvl_from))
df_emp$to     <- factor(df_emp$to,     levels = lvl_to)

# Estética
fill_low  <- "#f7fbff"
fill_high <- "#08519c"
text_col <- function(p) ifelse(p >= 0.5, "white", "black")

plot_heat <- function(dflong){
  ggplot(dflong, aes(x = to, y = from, fill = pct_row)) +
    geom_tile() +
    geom_text(aes(label = pct_lab, color = I(text_col(pct_row))), size = 3.6) +
    scale_fill_gradient(limits = c(0, 1), low = fill_low, high = fill_high,
                        labels = percent_format(accuracy = 1)) +
    labs(x = "To (t+1)", y = "From (t)", fill = "% row",
         title = unique(dflong$panel)) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1),
      panel.grid = element_blank(),
      legend.position = "right",
      plot.title = element_text(face = "bold")
    )
}

p_model <- plot_heat(df_model)
p_emp   <- plot_heat(df_emp)

p_transiciones <- cowplot::plot_grid(p_model, p_emp, ncol = 2,
                                     rel_widths = c(1,1), labels = c("A", "B"))
print(p_transiciones)

fn <- file.path(out_dir, "transiciones_heatmaps.png")
ggsave(fn, p_transiciones, width = 13, height = 6.8, dpi = 300)
message("Gráfico guardado en: ", fn)

library(ggplot2)

add_diag <- function(p, n=4){
  p + geom_rect(data = data.frame(i = 1:n),
                aes(xmin = i-0.5, xmax = i+0.5, ymin = n-i+0.5, ymax = n-i+1.5),
                inherit.aes = FALSE, fill = NA, color = "grey30", linewidth = 0.6)
}
p_model_diag <- add_diag(p_model); p_emp_diag <- add_diag(p_emp)
p_transiciones <- cowplot::plot_grid(p_model_diag, p_emp_diag, ncol=2,
                                     rel_widths=c(1,1), labels=c("A","B"))
ggsave(file.path(out_dir, "transiciones_heatmaps_diag.png"),
       p_transiciones, width=13, height=6.8, dpi=300)



df_diff <- df_emp |>
  rename(p_emp = pct_row) |>
  inner_join(df_model |> rename(p_mod = pct_row), by = c("from","to")) |>
  mutate(diff = p_emp - p_mod,
         lab  = sprintf("%+.1f pp", 100*diff))

ggplot(df_diff, aes(x = to, y = from, fill = diff)) +
  geom_tile() +
  geom_text(aes(label = lab), size = 3.6) +
  scale_fill_gradient2(limits = c(-0.25, 0.25), midpoint = 0,
                       labels = scales::percent_format(accuracy = 1),
                       low = "#b2182b", mid = "white", high = "#2166ac") +
  labs(x="To (t+1)", y="From (t)", fill="Emp - Mod",
       title="Transition differences: Empirical minus Model-based") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1),
        panel.grid = element_blank())
ggsave(file.path(out_dir, "transiciones_diff_emp_vs_model.png"),
       width=9, height=7, dpi=300)


# =========================
# Year-by-year ALLUVIALS (Sankey) for transitions
# =========================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggalluvial, cowplot, scales)

out_dir <- "code/latent_violence/output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

k  <- modelo_lm4$k
TT <- modelo_lm4$TT
V  <- modelo_lm4$V
PI <- modelo_lm4$PI  # dims = (k, k, n, TT)

# -------------------------
# Class labels (reuse if already defined)
# -------------------------
if (!exists("class_labels")) {
  props       <- colMeans(modelo_lm4$Piv)
  props_perc  <- round(100 * props, 1)
  class_names <- c("Universal Rejecters of Violence",
                   "Pro Control Social",
                   "Pro-Indigenous Force Sympathizers",
                   "Violentista")
  names(class_names) <- as.character(1:length(class_names))
  class_labels <- paste0(class_names, " (", props_perc, "%)")
  names(class_labels) <- as.character(1:length(class_labels))
}
rn <- paste0(1:k, ": ", class_labels)

# -------------------------
# (A) Decode S (n x TT) robustly from V
# -------------------------
get_S_from_V <- function(V, k, TT){
  d <- dim(V); stopifnot(length(d)==3)
  if (d[2]==k && d[3]==TT)   return(apply(V, c(1,3), which.max))
  if (d[2]==TT && d[3]==k)   return(apply(aperm(V, c(1,3,2)), c(1,3), which.max))
  if (d[1]==k && d[3]==TT)   return(apply(aperm(V, c(2,1,3)), c(1,3), which.max))
  if (d[1]==TT && d[3]==k)   return(apply(aperm(V, c(2,3,1)), c(1,3), which.max))
  if (d[1]==k && d[2]==TT)   return(apply(aperm(V, c(3,1,2)), c(1,3), which.max))
  if (d[1]==TT && d[2]==k)   return(apply(aperm(V, c(3,2,1)), c(1,3), which.max))
  stop("No se pudo inferir el orden de V.")
}
S_mat <- get_S_from_V(V, k, TT)  # n x TT

# -------------------------
# (B) Model-based transitions per year (average across subjects)
# PI dims here are (k, k, n, TT). We build one kxk matrix for each t -> t+1
# -------------------------
get_PI_by_time <- function(PI, k, TT) {
  stopifnot(length(dim(PI)) == 4)
  # dims: (k, k, n, TT) with transitions at times 1..(TT-1)
  TTm1 <- dim(PI)[4] - 1
  out  <- vector("list", TTm1)
  for (tt in 1:TTm1) {
    M <- apply(PI[,,,tt], c(1,2), mean, na.rm = TRUE)    # average over subjects
    M <- sweep(M, 1, pmax(rowSums(M), 1e-12), "/")       # row-normalize
    out[[tt]] <- M
  }
  out
}
PI_by_t <- get_PI_by_time(PI, k, TT)  # list of kxk matrices, one per period

# -------------------------
# (C) Labels for panels (replace with actual years if you have them)
# -------------------------
years_vec <- paste0("t", 1:TT)  # e.g., c("2016","2017","2018","2019")
panel_lab <- paste0(years_vec[-length(years_vec)], " \u2192 ", years_vec[-1])

# -------------------------
# (D) Build tidy data for ALLUVIALS
# -------------------------

# Empirical (decoded) per year: proportions by row
emp_alluv <- map_dfr(1:(TT-1), function(t){
  tab <- table(factor(S_mat[,t],   levels = 1:k),
               factor(S_mat[,t+1], levels = 1:k))
  prop <- prop.table(tab, 1)
  as.data.frame(prop) |>
    transmute(
      panel = paste0("Empirical ", panel_lab[t]),
      from  = paste0(Var1, ": ", class_labels[as.character(Var1)]),
      to    = paste0(Var2, ": ", class_labels[as.character(Var2)]),
      freq  = as.numeric(Freq)
    )
})

# Model-based per year (averaged over subjects)
mod_alluv <- map_dfr(seq_along(PI_by_t), function(t){
  M <- PI_by_t[[t]]
  rownames(M) <- rn; colnames(M) <- rn
  as.data.frame(M) |>
    rownames_to_column("from") |>
    pivot_longer(-from, names_to = "to", values_to = "freq") |>
    mutate(panel = paste0("Model-based ", panel_lab[t])) |>
    relocate(panel, from, to, freq)
})

# -------------------------
# (E) Plot helpers (ggalluvial)
# -------------------------
plot_alluvial <- function(df, title){
  # order the strata so they appear in class order
  lvl_from <- rn
  lvl_to   <- rn
  df %>%
    mutate(
      from = factor(from, levels = lvl_from),
      to   = factor(to,   levels = lvl_to)
    ) %>%
    ggplot(aes(y = freq, axis1 = from, axis2 = to)) +
    geom_alluvium(aes(fill = from), width = 0.25, knot.pos = 0.4, alpha = 0.85) +
    geom_stratum(width = 0.25, color = "grey30") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.2) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = NULL, y = "Row %", title = title, fill = "From (t)") +
    facet_wrap(~ panel, ncol = 1, scales = "free_y") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      strip.text = element_text(face = "bold")
    )
}

p_emp_alluv <- plot_alluvial(emp_alluv, "Empirical transitions (decoded) by year")
#p_mod_alluv <- plot_alluvial(mod_alluv, "Model-based transitions (avg. over subjects) by year")

# -------------------------
# (F) Save
# -------------------------
ggsave(file.path(out_dir, "alluvial_empirico_por_anio.png"),
       p_emp_alluv, width = 12, height = 12, dpi = 300)
ggsave(file.path(out_dir, "alluvial_model_por_anio.png"),
       p_mod_alluv, width = 12, height = 12, dpi = 300)


## paquetes
#if (!require("pacman")) install.packages("pacman")
#pacman::p_load(tidyverse, ggalluvial, scales)
#
#out_dir <- "code/latent_violence/output"
#dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
#
## ---------- 1) Sankey EMPÍRICO (conteos) ----------
## counts_emp: matriz k x k con RN=rn, CN=cn ya seteados
#df_emp_edges <- counts_emp %>%
#  as.data.frame() %>%
#  rownames_to_column("from") %>%
#  pivot_longer(-from, names_to="to", values_to="n") %>%
#  mutate(p = n / pmax(rowSums(counts_emp)[from], 1)) %>%  # % por fila
#  arrange(from, to)
#
## Opcional: filtra flujos pequeños para limpieza visual (p.ej. <5%)
#thr <- 0.05
#df_emp_edges_plot <- df_emp_edges %>% mutate(show = p >= thr)
#
## Orden consistente
#lvl_from <- rownames(counts_emp)
#lvl_to   <- colnames(counts_emp)
#
#p_sankey_emp <- ggplot(
#  df_emp_edges_plot,
#  aes(y = n, axis1 = from, axis2 = to)
#) +
#  geom_alluvium(aes(fill = from, alpha = show), width = 1/12, knot.pos = 0.4) +
#  geom_stratum(width = 1/12, fill = "grey90", color = "grey60") +
#  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.2, hjust = 0) +
#  scale_x_discrete(limits = c("From (t)", "To (t+1)"), expand = c(.01, .01)) +
#  scale_alpha_manual(values = c(`FALSE` = 0.15, `TRUE` = 0.85), guide = "none") +
#  guides(fill = "none") +
#  labs(
#    title = "Sankey – Empirical transitions (decoded)",
#    subtitle = sprintf("Se muestran flujos ≥ %.0f%% de la fila", 100*thr),
#    y = "Casos (n)",
#    x = NULL
#  ) +
#  theme_minimal(base_size = 12) +
#  theme(
#    panel.grid = element_blank(),
#    axis.text.y = element_blank()
#  )
#
#ggsave(file.path(out_dir, "sankey_empirical_decoded.png"),
#       p_sankey_emp, width = 12, height = 6.5, dpi = 300)
#
## ---------- 2) Sankey MODEL-BASED (porcentajes fila) ----------
## trans_matrix_model: matriz k x k de promedios (proporciones)
#df_mod_edges <- trans_matrix_model %>%
#  as.data.frame() %>%
#  rownames_to_column("from") %>%
#  pivot_longer(-from, names_to="to", values_to="p") %>%
#  # Escala a un total ficticio por fila para dibujar volúmenes comparables:
#  group_by(from) %>% mutate(n = 1000 * p / sum(p)) %>% ungroup()
#
#df_mod_edges_plot <- df_mod_edges %>%
#  mutate(show = p >= thr)
#
#p_sankey_mod <- ggplot(
#  df_mod_edges_plot,
#  aes(y = n, axis1 = from, axis2 = to)
#) +
#  geom_alluvium(aes(fill = from, alpha = show), width = 1/12, knot.pos = 0.4) +
#  geom_stratum(width = 1/12, fill = "grey90", color = "grey60") +
#  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.2, hjust = 0) +
#  scale_x_discrete(limits = c("From (t)", "To (t+1)"), expand = c(.01, .01)) +
#  scale_alpha_manual(values = c(`FALSE` = 0.15, `TRUE` = 0.85), guide = "none") +
#  guides(fill = "none") +
#  labs(
#    title = "Sankey – Model-based transitions (avg. PI)",
#    subtitle = sprintf("Se muestran flujos ≥ %.0f%% de la fila", 100*thr),
#    y = "Volumen relativo (ficticio)",
#    x = NULL
#  ) +
#  theme_minimal(base_size = 12) +
#  theme(
#    panel.grid = element_blank(),
#    axis.text.y = element_blank()
#  )
#
#ggsave(file.path(out_dir, "sankey_model_based.png"),
#       p_sankey_mod, width = 12, height = 6.5, dpi = 300)
#
#
#
#pacman::p_load(tidyverse, networkD3, htmlwidgets)
#
## Construir nodos únicos (from y to ya incluyen etiquetas con %)
#nodes <- tibble(name = c(rownames(counts_emp), colnames(counts_emp))) %>% distinct()
#
## Enlaces desde matriz empírica (solo flujos ≥ 5% de la fila)
#thr <- 0.05
#links_emp <- counts_emp %>%
#  as.data.frame() %>%
#  rownames_to_column("from") %>%
#  pivot_longer(-from, names_to = "to", values_to = "n") %>%
#  mutate(p = n / pmax(rowSums(counts_emp)[from], 1)) %>%
#  filter(p >= thr) %>%
#  transmute(
#    source = match(from, nodes$name) - 1,
#    target = match(to,   nodes$name) - 1,
#    value  = n,
#    label  = paste0(sprintf("%.1f", 100*p), "%")
#  )
#
#sank <- sankeyNetwork(
#  Links = links_emp, Nodes = nodes,
#  Source = "source", Target = "target",
#  Value  = "value",  NodeID = "name",
#  fontSize = 12, nodeWidth = 28, sinksRight = TRUE
#)
#
#saveWidget(sank, file.path(out_dir, "sankey_empirical_decoded.html"),
#           selfcontained = TRUE)
#
#
#
#
#
## ===============================================
## Transiciones por año/ola (Empírico y Model-based)
## Requiere: modelo_lm4 con slots: k, TT, V, PI  (de LMest)
## ===============================================
#
#if (!require("pacman")) install.packages("pacman")
#pacman::p_load(tidyverse, cowplot, scales)
#
#out_dir <- "code/latent_violence/output"
#dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
#
#k  <- modelo_lm4$k
#TT <- modelo_lm4$TT
#V  <- modelo_lm4$V
#PI <- modelo_lm4$PI  # ¡ojo mayúsculas!
#
## --- (A) Helper: decodificar S (n x TT) desde V con orden robusto
#get_S_from_V <- function(V, k, TT){
#  d <- dim(V)
#  stopifnot(length(d) == 3)
#  # Casos posibles para (n,k,TT)
#  if (d[2]==k && d[3]==TT)   return(apply(V, c(1,3), which.max))
#  if (d[2]==TT && d[3]==k)   return(apply(aperm(V, c(1,3,2)), c(1,3), which.max))
#  if (d[1]==k && d[3]==TT)   return(apply(aperm(V, c(2,1,3)), c(1,3), which.max))
#  if (d[1]==TT && d[3]==k)   return(apply(aperm(V, c(2,3,1)), c(1,3), which.max))
#  if (d[1]==k && d[2]==TT)   return(apply(aperm(V, c(3,1,2)), c(1,3), which.max))
#  if (d[1]==TT && d[2]==k)   return(apply(aperm(V, c(3,2,1)), c(1,3), which.max))
#  stop("No se pudo inferir el orden de V.")
#}
#S_mat <- get_S_from_V(V, k, TT)  # n x TT
#
## --- (B) Helper: promediar PI por sujetos conservando el eje temporal
## Devuelve una lista de matrices k x k por cada t (t=1..TT-1), ya normalizadas por fila.
#get_PI_by_time <- function(PI, k, TT){
#  d <- dim(PI)
#  if (length(d) == 3){
#    # asumimos (k, k, TT-1) o cualquier permutación que tenga dos dims==k
#    idx_k <- which(d == k)
#    if (length(idx_k) != 2) stop("No se identificaron 2 ejes k en PI.")
#    idx_t <- setdiff(1:3, idx_k)
#    # reordenar a (k,k,TT-1)
#    ord <- c(idx_k, idx_t)
#    PI3 <- aperm(PI, ord)
#    TTm1 <- dim(PI3)[3]
#    stopifnot(TTm1 %in% c(TT-1, TT))   # según paquete, puede ser TT-1
#    out <- vector("list", TTm1)
#    for (tt in 1:TTm1){
#      M <- PI3[,,tt, drop=FALSE][,,1]
#      # normaliza por fila por seguridad
#      M <- sweep(M, 1, pmax(rowSums(M), 1e-12), "/")
#      out[[tt]] <- M
#    }
#    return(out)
#  } else if (length(d) == 4){
#    # (k, k, TT-1, n) en algún orden → promediamos sobre el eje sujetos
#    idx_k <- which(d == k);            stopifnot(length(idx_k)==2)
#    idx_t <- which(d %in% c(TT-1, TT)) # eje temporal
#    idx_n <- setdiff(1:4, c(idx_k, idx_t))
#    PI4   <- aperm(PI, c(idx_k, idx_t, idx_n))  # -> (k,k,TT-1,n)
#    TTm1  <- dim(PI4)[3]
#    n     <- dim(PI4)[4]
#    out <- vector("list", TTm1)
#    for (tt in 1:TTm1){
#      M <- apply(PI4[,,tt, 1:n, drop=FALSE], c(1,2), mean, na.rm=TRUE)
#      M <- sweep(M, 1, pmax(rowSums(M), 1e-12), "/")
#      out[[tt]] <- M
#    }
#    return(out)
#  } else {
#    stop("PI debe ser 3D o 4D.")
#  }
#}
#PI_by_t <- get_PI_by_time(PI, k, TT)  # lista de matrices por t
#
## --- (C) Etiquetas de clase (ya las tenías; reuso si existen)
#if (!exists("class_labels")) {
#  props <- colMeans(modelo_lm4$Piv)
#  props_perc <- round(100*props, 1)
#  class_names <- c("Universal Rejecters of Violence","Pro Control Social",
#                   "Pro-Indigenous Force Sympathizers","Violentista")
#  names(class_names) <- as.character(1:length(class_names))
#  class_labels <- paste0(class_names," (",props_perc,"%)")
#  names(class_labels) <- as.character(1:length(class_labels))
#}
#rn <- paste0(1:k, ": ", class_labels)
#cn <- rn
#
## --- (D) Etiquetas temporales (edítalas si tienes años reales)
## p.ej.: years_vec <- c(2016,2017,2018,2023,2024)  # si TT=5
#years_vec <- paste0("t", 1:TT)
#panel_lab <- paste0(years_vec[-length(years_vec)], " \u2192 ", years_vec[-1])  # "t1 → t2", etc.
#
## --- (E) EMPÍRICO por año (proporciones por fila)
#emp_long <- map_dfr(1:(TT-1), function(t){
#  tab <- table(factor(S_mat[,t],   levels=1:k),
#               factor(S_mat[,t+1], levels=1:k))
#  prop <- prop.table(tab, 1)
#  df <- as.data.frame(prop) |>
#    mutate(from = paste0(Var1, ": ", class_labels[as.character(Var1)]),
#           to   = paste0(Var2, ": ", class_labels[as.character(Var2)]),
#           pct  = Freq,
#           panel = paste0("Empirical ", panel_lab[t])) |>
#    select(from, to, pct, panel)
#  df
#})
#
## --- (F) MODEL-BASED por año (proporciones por fila)
#mod_long <- map_dfr(seq_along(PI_by_t), function(t){
#  M <- PI_by_t[[t]]
#  rownames(M) <- rn; colnames(M) <- cn
#  as.data.frame(M) |>
#    rownames_to_column("from") |>
#    pivot_longer(-from, names_to="to", values_to="pct") |>
#    mutate(panel = paste0("Model-based ", panel_lab[t]))
#})
#
## --- (G) Plot helper
#plot_heat_facets <- function(dflong, title){
#  dflong %>%
#    mutate(pct_lab = sprintf("%.1f%%", 100*pct),
#           from = factor(from, levels = rev(rn)),
#           to   = factor(to,   levels = cn)) %>%
#    ggplot(aes(x = to, y = from, fill = pct)) +
#    geom_tile() +
#    geom_text(aes(label = pct_lab,
#                  color = I(ifelse(pct >= 0.5, "white", "black"))),
#              size = 3.1) +
#    scale_fill_gradient(limits = c(0,1), low = "#f7fbff", high = "#08519c",
#                        labels = percent_format(accuracy = 1)) +
#    facet_wrap(~ panel) +
#    labs(x = "To (t+1)", y = "From (t)", fill = "% row", title = title) +
#    theme_minimal(base_size = 12) +
#    theme(
#      axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1),
#      panel.grid = element_blank(),
#      legend.position = "right",
#      plot.title = element_text(face = "bold")
#    )
#}
#
#p_emp_year <- plot_heat_facets(emp_long, "Empirical transitions by year")
#p_mod_year <- plot_heat_facets(mod_long, "Model-based transitions by year")
#
## Guardar
#ggsave(file.path(out_dir, "transiciones_empiricas_por_anio.png"),
#       p_emp_year, width = 14, height = 8.5, dpi = 300)
#ggsave(file.path(out_dir, "transiciones_model_por_anio.png"),
#       p_mod_year, width = 14, height = 8.5, dpi = 300)
#
## --- (H) Exportar CSV (útil para apéndice)
#write.csv(emp_long, file.path(out_dir, "transiciones_empiricas_por_anio.csv"), row.names = FALSE)
#write.csv(mod_long, file.path(out_dir, "transiciones_model_por_anio.csv"),    row.names = FALSE)
#
#str(modelo_lm4$PI)
#dim(modelo_lm4$PI)
#
#