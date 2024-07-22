
# Lmest

# librerías
pacman::p_load(tidyverse,jtools,polycor,ggplot2,ggstatsplot,ggcorrplot,broom,survey,
               kableExtra,scales,panelr,sjPlot,sjlabelled,sjmisc,stargazer,skimr,texreg,
               igraph, signnet, ggraph, extrafont, forcats, xtable, Hmisc, psych, psy,
               nFactors, GPArotation, psychTools, here, LMest, tidyr, tigerstats, sjmisc,
               labelled, sjPlot, sjtable2df)

#options(knitr.kable.NA = '')

# load data
load("/home/rober/Desktop/ELRI/BBDD_ELRI_LONG.RData")
a_full <- BBDD_ELRI_LONG

#BBDD_ELRI_LONG$urbano_rural
# seleccionar variables 
#a_full<- panel_data(a_full, id = folio, wave = ano) %>%  
#  complete_data(min.waves = "all") %>%
#  as.data.frame()

# Process data -----------------------------------------------------------------
a_full <- a_full%>%
  #filter(a1 %in% 1) %>%
  dplyr::mutate(indi = case_when(a1 %in% 1:11 ~ "indi",
                                 a1 == 12 ~ "no_indi")) %>% 
  dplyr::mutate(cat_indi = case_when(a1 == 1 ~ "mapuche", 
                                     a1 %in% c(2, 4, 5, 6, 7, 10) ~ "andino",
                                     a1 == 12 ~ "chileno_noindig",
                                     TRUE ~ "otro")) %>% 
  dplyr::mutate(ola = case_when(ano == "2016" ~ 0, 
                                ano == "2018" ~ 1,
                                ano == "2021" ~ 2,
                                ano == "2023" ~ 3)) %>% 
  dplyr::mutate (mujer = case_when(g2 == 1 ~ "0",
                                   g2 == 2 ~ "1")) %>% 
  dplyr::mutate (edad = case_when(g18 %in% 18:24 ~ "18_24",
                                  g18 %in% 25:34 ~ "25_34",
                                  g18 %in% 35:44 ~ "35_44",
                                  g18 %in% 45:54 ~ "45_54",
                                  g18 %in% 55:64 ~ "55_64", 
                                  g18 %in% 65:89 ~ "65+")) %>% 
  dplyr::select(folio, ola, b11_1, b11_2, b11_3, b11_4, b11_5, mujer, edad, indi, cat_indi, urbano_rural,pond) %>% 
  mutate(across(starts_with("b11"), as.numeric)) %>% drop_na() %>% ungroup() %>% as_tibble()


a_full %>% panel_data(a_full, id = folio, wave = ola) %>% complete_data(min.waves = 3) 

#BBDD_ELRI_LONG$b7_5
#BBDD_ELRI_LONG$b7_6
#BBDD_ELRI_LONG$b7_7
#BBDD_ELRI_LONG$b7_8
#BBDD_ELRI_LONG$b11_1
#BBDD_ELRI_LONG$b11_2
#BBDD_ELRI_LONG$b11_3
#BBDD_ELRI_LONG$b11_4
#BBDD_ELRI_LONG$b11_5
#BBDD_ELRI_LONG$tipo_participante

# set NA
a_full[a_full==8888] <- NA
a_full[a_full==9999] <- NA
a_full[a_full==88] <- NA
a_full[a_full==99] <- NA

datos <- a_full %>% 
  group_by(folio) %>% 
  summarise(num_anios = n_distinct(ola))
  
folio_3_anios <- datos %>% 
  filter(num_anios >= 3) %>% 
  select(folio)


# join
a_full <- a_full %>% left_join(datos, by = "folio")
a_full <- a_full %>% filter(num_anios == 3)
a_full %>% frq (ola)



# set panel 
a_full <- a_full %>% panel_data(a_full, id = folio, wave = ola) %>% 
  complete_data(min.waves = 3) %>% 
  as.data.frame()

  table(a_full$ola)
#a_full %>% frq(g18)


a_plot <- a_full %>% select(b11_1, 
                            b11_2,  
                            b11_3,
                            b11_4,
                            b11_5) %>% mutate_all(as.numeric)

plot_likert(a_plot,
            geom.colors = c("#3F00FF","#28282B"),
            reverse.colors=F,
            #cat.neutral=NULL,
            values="sum.outside",
            show.prc.sign=F)


# delete NA in covariable
a_full<-a_full %>% drop_na(c("mujer", "edad", "b11_1", "b11_2", "b11_3", "b11_4", "b11_5"))
glimpse(a_full)
a_full$mujer <- as.factor(a_full$mujer)
a_full$edad <- as.factor(a_full$edad)
a_full$indi <- as.factor(a_full$indi)
a_full$urbano_rural <- as.factor(a_full$urbano_rural)

# Verificar filas con valores faltantes en las covariables
complete_rows <- complete.cases(a_full[, c("mujer", "edad", "b11_1", "b11_2", "b11_3", "b11_4", "b11_5", "indi", "urbano_rural")])

# Filtrar el dataframe para incluir solo las filas completas
a_full_complete <- a_full[complete_rows, ]
colSums(is.na(a_full))

a_full_complete2 <- a_full_complete %>%
  group_by(folio) %>%
  filter(n() == 3) %>%
  ungroup()

resultado <- datos %>%
  group_by(folio) %>%
  filter(n() == 3) %>%
  ungroup()

a_full_complete3 <- a_full_complete2 %>% 
  panel_data(a_full_complete2, id = folio, wave = ola) %>% 
  complete_data(min.waves = 3) %>%  
  group_by(folio) %>%
  filter(n_distinct(ola) == 3 & all(ola %in% 1:3)) %>%
  ungroup() %>% 
  as.data.frame()

a_full_complete3 %>% frq(ola)

# Modelo -----------------------------------------------------------------------
## Modelo 1
modelo1 <-  lmest(responsesFormula = b11_1 + b11_2 + b11_3 + b11_4 + b11_5 ~ NULL,
                  latentFormula =~ mujer + edad + urbano_rural,
                  index = c("folio","ola"),
                  output = TRUE,
                  out_se = TRUE,
                  paramLatent = "multilogit",
                  data = a_full_complete3,
                  k = 1:5,
                  start = 0,
                  #modBasic = 1,
                  #modManifest="LM",
                  seed = 1234)


a_full_complete3 %>% frq(b11_1)


plot(modelo1,what="modSel")
plot(modelo1, what = "CondProb")
plot(modelo1, what="marginal")


# multinomial regression -------------------------------------------------------
## Resultados de regresión multinomial para describir grupos.
Be<-as.data.frame(modelo1$Be)
seBe<-as.data.frame(modelo1$seBe)
z <- modelo1$Be/modelo1$seBe
p <- (1 - pnorm(abs(z), 0, 1))*2 # two-tailed z test


# reorder classes 
# Intercambiar coeficientes y probabilidades de las categorías 1 y 3
#tmp <- modelo1$Be[1, ]
#modelo1$Be[1, ] <- modelo1$Be[3, ]
#modelo1$Be[3, ] <- tmp
#
#tmp_ga <- modelo1$Ga[, , 1]
#modelo1$Ga[, , 1] <- modelo1$Ga[, , 3]
#modelo1$Ga[, , 3] <- tmp_ga
#
#tmp_psi <- modelo1$Psi[, , 1]
#modelo1$Psi[, , 1] <- modelo1$Psi[, , 3]
#modelo1$Psi[, , 3] <- tmp_psi
#
#tmp_piv <- modelo1$Piv[1]
#modelo1$Piv[1] <- modelo1$Piv[3]
#modelo1$Piv[3] <- tmp_piv
#
## Imprimir coeficientes para las probabilidades iniciales (Be)
#print(modelo1$Be)
#
## Imprimir coeficientes para las probabilidades de transición (Ga)
#print(modelo1$Ga)

# Decoding ---------------------------------------------------------------------
dec <- lmestDecoding(modelo1)
head(dec$Ul)
head(dec$Ug)

# join
ul <- as.data.frame(dec$Ul)
#glimpse(a_full_complete3)
#glimpse(ul)

folio <- as.data.frame(unique(a_full_complete3$folio))
folio <- folio %>% rename(folio = `unique(a_full_complete3$folio)`) 
ul <- cbind(folio, ul)
a_full_complete3 <- a_full_complete3 %>% left_join(ul, by = "folio")
tabs <- a_full_complete3 %>% select(folio, ola, mujer, edad, indi, cat_indi, urbano_rural, V1, pond)
tabs <- tabs %>% filter(ola == 1)


# crosstabs --------------------------------------------------------------------
tabs$V1 <- as.factor(tabs$V1)

xtab1 <- sjPlot::tab_xtab(var.row = tabs$V1, 
                          var.col = tabs$cat_indi, 
                          show.na = TRUE,
                          show.row.prc = TRUE,
                          #show.col.prc = TRUE,
                          #show.summary = TRUE,
                          use.viewer = TRUE,
                          #show.exp = TRUE,
                          remove.spaces=TRUE, 
                          weight.by= tabs$pond)

xtab1 <- sjtable2df::xtab2df(xtab = xtab1, output = "data.frame")
save(xtab1, file = "~/Documents/ELRI/bbdd/xtab1.RData")


xtab2 <- sjPlot::tab_xtab(var.row = tabs$V1, 
                          var.col = tabs$mujer, 
                          show.na = TRUE,
                          show.row.prc = TRUE,
                          #show.col.prc = TRUE,
                          #show.summary = TRUE,
                          use.viewer = TRUE,
                          #show.exp = TRUE,
                          remove.spaces=TRUE, 
                          weight.by= tabs$pond)

xtab2 <- sjtable2df::xtab2df(xtab = xtab2, output = "data.frame")
save(xtab2, file = "~/Documents/ELRI/bbdd/xtab2.RData")

xtab3 <- sjPlot::tab_xtab(var.row = tabs$V1, 
                          var.col = tabs$edad, 
                          show.na = TRUE,
                          show.row.prc = TRUE,
                          #show.col.prc = TRUE,
                          #show.summary = TRUE,
                          use.viewer = TRUE,
                          #show.exp = TRUE,
                          remove.spaces=TRUE, 
                          weight.by= tabs$pond)

xtab3 <- sjtable2df::xtab2df(xtab = xtab3, output = "data.frame")
save(xtab3, file = "~/Documents/ELRI/bbdd/xtab3.RData")

xtab4 <- sjPlot::tab_xtab(var.row = tabs$V1, 
                          var.col = tabs$indi, 
                          show.na = TRUE,
                          show.row.prc = TRUE,
                          #show.col.prc = TRUE,
                          #show.summary = TRUE,
                          use.viewer = TRUE,
                          #show.exp = TRUE,
                          remove.spaces=TRUE, 
                          weight.by= tabs$pond)

xtab4 <- sjtable2df::xtab2df(xtab = xtab4, output = "data.frame")
save(xtab4, file = "~/Documents/ELRI/bbdd/xtab4.RData")


xtab5 <- sjPlot::tab_xtab(var.row = tabs$V1, 
                          var.col = tabs$urbano_rural, 
                          show.na = TRUE,
                          show.row.prc = TRUE,
                          #show.col.prc = TRUE,
                          #show.summary = TRUE,
                          use.viewer = TRUE,
                          #show.exp = TRUE,
                          remove.spaces=TRUE, 
                          weight.by= tabs$pond)

xtab5 <- sjtable2df::xtab2df(xtab = xtab5, output = "data.frame")
save(xtab5, file = "~/Documents/ELRI/bbdd/xtab5.RData")




# plot 
var_label(BBDD_ELRI_LONG$b11_1)
var_label(BBDD_ELRI_LONG$b11_2)
var_label(BBDD_ELRI_LONG$b11_3)
var_label(BBDD_ELRI_LONG$b11_4)
var_label(BBDD_ELRI_LONG$b11_5)

val_labels(BBDD_ELRI_LONG$b11_1)
val_labels(BBDD_ELRI_LONG$b11_2)
val_labels(BBDD_ELRI_LONG$b11_3)
val_labels(BBDD_ELRI_LONG$b11_4)
val_labels(BBDD_ELRI_LONG$b11_5)


LMmodelo1 <- reshape2::melt(modelo1$Psi, level=1)
LMmodelo1 <- LMmodelo1 %>% mutate(value = round(value * 100))
LMmodelo1 <- LMmodelo1 %>% dplyr::filter(item == "1")

# plot -------------------------------------------------------------------------
## general (Averaged probabilities)

## Graficar modelo 3
LMmodelo3 <- reshape2::melt(modelo3$Psi, level=1)
LMmodelo3 = LMmodelo3 %>%
  dplyr::mutate (clase = case_when(state == 1 ~ "Class 2\n Closed (36%)",
                                   state == 2 ~ "Class 1\n Broker (10%)",
                                   state == 3 ~ "Class 3\n Apathetic (54%)")) %>%
  dplyr::mutate (category = case_when(category == 0 ~ "no (not member)",
                                      category == 1 ~ "yes (member)")) 

LMmodelo3$item <- plyr::mapvalues(LMmodelo3$item, 
                                  c('1','2','3','4','5','6','7','8'),
                                  c("neighborhood","religious","political","union","professional","charity","sports","student"))
level_order <- c("neighborhood","religious","charity","political","union","professional","sports","student") 


## plot 
LMmodelo1 <- reshape2::melt(modelo1$Psi, level=1)
#LMmodelo1 <- LMmodelo1 %>% mutate(value = round(value * 100))
#LMmodelo1 <- LMmodelo1 %>% dplyr::filter(item == "1")

LMmodelo1$state <- as.factor(LMmodelo1$state)
LMmodelo1$category <- as.factor(LMmodelo1$category)

LMmodelo1 = LMmodelo1 %>%
  dplyr::mutate (clase = case_when(state == 1 ~ "Class 1\n Expansiva\n empoderadora (31%)",
                                   state == 2 ~ "Class 1\n Normativa\n estructurada (54%)",
                                   state == 3 ~ "Class 3\n Compasiva\n tradicional (15%)")) 

ggplot(LMmodelo1, aes(x = factor(item), y = value, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(clase ~ .) +
  scale_fill_manual(values = c("#28282B", "#3F00FF")) + 
  labs(x = "",y="", fill ="") + theme(text = element_text(size=15)) +
  theme(axis.ticks.y=element_blank(),
        strip.text = element_text(size = 15),
        #legend.position = "top",
        panel.grid.major.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 8),
        axis.title = element_text(size=10),
        axis.text.x = element_text(size = 15, hjust = 0.9),
        axis.text.y = element_text(size = 11)) +
  guides(fill = guide_legend(reverse=F)) 


## By item. 
### Item 1
LMmodelo1 <- reshape2::melt(modelo1$Psi, level=1)
LMmodelo1 <- LMmodelo1 %>% mutate(value = round(value * 100))
LMmodelo1 <- LMmodelo1 %>% dplyr::filter(item == "1")


zp1 <- ggplot(LMmodelo1,aes(x = factor(category), y = value, fill = factor(state), color))
zp1 <- zp1 + geom_bar(stat = "identity", position = "dodge") 
zp1 <- zp1 + labs(x = "",y="") + theme(text = element_text(size=10))
zp1 <- zp1 + theme_bw() + 
  theme(
    plot.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.border = element_rect(size = 1, colour = "black"),
    panel.spacing = unit(0.3, "lines"),
    axis.line = element_line(size = 0.1, colour = "black"),
    axis.ticks.y = element_line(size = 0.5, colour = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(size = 12, color = "black", margin = margin(t = 3)),
    axis.text.y = element_text(size = 12, color = "black", margin = margin(r = 3)),
    text = element_text(size = 18),
    legend.position = "bottom",
    plot.title = element_text(size = 15, hjust = 0.5)
  )
zp1 <- zp1 + scale_y_continuous(limits = c(0,100))
zp1 <- zp1 + geom_text(aes(label = paste0(round(value), "%")), size = 3.5,
                       position = position_dodge(0.9), vjust = -0.5)
zp1 <- zp1 + scale_x_discrete(name = "", 
                              labels = c("0" = "Desarrollar confianza\nen sí mismo",
                                         "1" = "Aprender a obedecer a\nlas personas mayores")) 
  
zp1 <- zp1 + scale_fill_manual(name = "Clases",
                               values = c("#440154", "#3b528b", "#5ec962")) +
  labs(title = "¿Qué característica cree usted que es más importante que los niños y niñas?")

zp1  



### Item 2
LMmodelo1 <- reshape2::melt(modelo1$Psi, level=1)
LMmodelo1 <- LMmodelo1 %>% mutate(value = round(value * 100))
LMmodelo1 <- LMmodelo1 %>% dplyr::filter(item == "2")


zp1 <- ggplot(LMmodelo1,aes(x = factor(category), y = value, fill = factor(state), color))
zp1 <- zp1 + geom_bar(stat = "identity", position = "dodge") 
zp1 <- zp1 + labs(x = "",y="") + theme(text = element_text(size=10))
zp1 <- zp1 + theme_bw() + 
  theme(
    plot.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.border = element_rect(size = 1, colour = "black"),
    panel.spacing = unit(0.3, "lines"),
    axis.line = element_line(size = 0.1, colour = "black"),
    axis.ticks.y = element_line(size = 0.5, colour = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(size = 12, color = "black", margin = margin(t = 3)),
    axis.text.y = element_text(size = 12, color = "black", margin = margin(r = 3)),
    text = element_text(size = 18),
    legend.position = "bottom",
    plot.title = element_text(size = 15, hjust = 0.5)
  )
zp1 <- zp1 + scale_y_continuous(limits = c(0,100))
zp1 <- zp1 + geom_text(aes(label = paste0(round(value), "%")), size = 3.5,
                       position = position_dodge(0.9), vjust = -0.5)
zp1 <- zp1 + scale_x_discrete(name = "", 
                              labels = c("0" = "Desarrollar competitividad",
                                         "1" = "Aprender a obedecer a los padres")) 

zp1 <- zp1 + scale_fill_manual(name = "Clases",
                               values = c("#440154", "#3b528b", "#5ec962")) +
  labs(title = "¿Qué característica cree usted que es más importante que los niños y niñas?")

zp1  


### Item 3
LMmodelo1 <- reshape2::melt(modelo1$Psi, level=1)
LMmodelo1 <- LMmodelo1 %>% mutate(value = round(value * 100))
LMmodelo1 <- LMmodelo1 %>% dplyr::filter(item == "3")


zp1 <- ggplot(LMmodelo1,aes(x = factor(category), y = value, fill = factor(state), color))
zp1 <- zp1 + geom_bar(stat = "identity", position = "dodge") 
zp1 <- zp1 + labs(x = "",y="") + theme(text = element_text(size=10))
zp1 <- zp1 + theme_bw() + 
  theme(
    plot.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.border = element_rect(size = 1, colour = "black"),
    panel.spacing = unit(0.3, "lines"),
    axis.line = element_line(size = 0.1, colour = "black"),
    axis.ticks.y = element_line(size = 0.5, colour = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(size = 12, color = "black", margin = margin(t = 3)),
    axis.text.y = element_text(size = 12, color = "black", margin = margin(r = 3)),
    text = element_text(size = 18),
    legend.position = "bottom",
    plot.title = element_text(size = 15, hjust = 0.5)
  )
zp1 <- zp1 + scale_y_continuous(limits = c(0,100))
zp1 <- zp1 + geom_text(aes(label = paste0(round(value), "%")), size = 3.5,
                       position = position_dodge(0.9), vjust = -0.5)
zp1 <- zp1 + scale_x_discrete(name = "", 
                              labels = c("0" = "Desarrollar autoestima",
                                         "1" = "Aprender a cuidar el bienestar de los demás")) 

zp1 <- zp1 + scale_fill_manual(name = "Clases",
                               values = c("#440154", "#3b528b", "#5ec962")) +
  labs(title = "¿Qué característica cree usted que es más importante que los niños y niñas?")

zp1  


### Item 4
LMmodelo1 <- reshape2::melt(modelo1$Psi, level=1)
LMmodelo1 <- LMmodelo1 %>% mutate(value = round(value * 100))
LMmodelo1 <- LMmodelo1 %>% dplyr::filter(item == "4")


zp1 <- ggplot(LMmodelo1,aes(x = factor(category), y = value, fill = factor(state), color))
zp1 <- zp1 + geom_bar(stat = "identity", position = "dodge") 
zp1 <- zp1 + labs(x = "",y="") + theme(text = element_text(size=10))
zp1 <- zp1 + theme_bw() + 
  theme(
    plot.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.border = element_rect(size = 1, colour = "black"),
    panel.spacing = unit(0.3, "lines"),
    axis.line = element_line(size = 0.1, colour = "black"),
    axis.ticks.y = element_line(size = 0.5, colour = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(size = 12, color = "black", margin = margin(t = 3)),
    axis.text.y = element_text(size = 12, color = "black", margin = margin(r = 3)),
    text = element_text(size = 18),
    legend.position = "bottom",
    plot.title = element_text(size = 15, hjust = 0.5)
  )
zp1 <- zp1 + scale_y_continuous(limits = c(0,100))
zp1 <- zp1 + geom_text(aes(label = paste0(round(value), "%")), size = 3.5,
                       position = position_dodge(0.9), vjust = -0.5)
zp1 <- zp1 + scale_x_discrete(name = "", 
                              labels = c("0" = "Desarrollar independencia",
                                         "1" = "Aprender a animar a los demás")) 

zp1 <- zp1 + scale_fill_manual(name = "Clases",
                               values = c("#440154", "#3b528b", "#5ec962")) +
  labs(title = "¿Qué característica cree usted que es más importante que los niños y niñas?")

zp1  


### Item 5
LMmodelo1 <- reshape2::melt(modelo1$Psi, level=1)
LMmodelo1 <- LMmodelo1 %>% mutate(value = round(value * 100))
LMmodelo1 <- LMmodelo1 %>% dplyr::filter(item == "5")


zp1 <- ggplot(LMmodelo1,aes(x = factor(category), y = value, fill = factor(state), color))
zp1 <- zp1 + geom_bar(stat = "identity", position = "dodge") 
zp1 <- zp1 + labs(x = "",y="") + theme(text = element_text(size=10))
zp1 <- zp1 + theme_bw() + 
  theme(
    plot.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.border = element_rect(size = 1, colour = "black"),
    panel.spacing = unit(0.3, "lines"),
    axis.line = element_line(size = 0.1, colour = "black"),
    axis.ticks.y = element_line(size = 0.5, colour = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(size = 12, color = "black", margin = margin(t = 3)),
    axis.text.y = element_text(size = 12, color = "black", margin = margin(r = 3)),
    text = element_text(size = 18),
    legend.position = "bottom",
    plot.title = element_text(size = 15, hjust = 0.5)
  )
zp1 <- zp1 + scale_y_continuous(limits = c(0,100))
zp1 <- zp1 + geom_text(aes(label = paste0(round(value), "%")), size = 3.5,
                       position = position_dodge(0.9), vjust = -0.5)
zp1 <- zp1 + scale_x_discrete(name = "", 
                              labels = c("0" = "Desarrollar un sentido de si mismo",
                                         "1" = "Aprender a controlar las emociones")) 

zp1 <- zp1 + scale_fill_manual(name = "Clases",
                               values = c("#440154", "#3b528b", "#5ec962")) +
  labs(title = "¿Qué característica cree usted que es más importante que los niños y niñas?")

zp1  
































