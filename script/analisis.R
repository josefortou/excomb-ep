# Cargar librerias ####

library(tidyverse) # trabajar con datos y visualizacion
library(texreg) # tablas de regresion
library(ggeffects) # graficas de interaccion
library(cowplot) # organizar graficas

# Cargar datos ####

datos <- read_rds("data/datos.rds") # cambiar a carpeta donde estan los datos

# opciones de figuras ####

# diseño simple blanco/negro

theme_set(
  theme_classic(base_size = 14) +
    theme(legend.position = "bottom")
)

# Figura 1 ####

# distribucion de variables NEP y margen de victoria

datos %>%
  pivot_longer(
    cols = c("nep_lt", "margen_prop"),
    names_to = "indicador",
    values_to = "valor"
  ) %>% 
  mutate(
    indicador = factor(indicador, levels = c("nep_lt", "margen_prop"), 
                       labels = c("NEP", "Margen")),
    tipo_eleccion = factor(tipo_eleccion, levels = c("ALCALDE", "CONCEJO"),
                           labels = c("Alcalde", "Concejo"))
  ) %>%
  ggplot(aes(valor)) +
  geom_histogram() +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  facet_grid(vars(tipo_eleccion), vars(indicador), scales = "free_x") +
  labs(x = "Indicador", y = "Número de municipios") +
  ggsave("output/dv_histograms.png", width = 10, height = 5)

# Figura 2 ####

# valores de NEP y margen en municipios con/sin participacion FARC

datos %>%
  group_by(dummy_farc) %>%
  summarize(nep_lt = mean(nep_lt, na.rm = TRUE),
            margen_prop = mean(margen_prop, na.rm = TRUE))

datos %>%
  group_by(dummy_farc) %>%
  summarize(nep_lt = mean(nep_lt, na.rm = TRUE),
            margen_prop = mean(margen_prop, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_longer(
    cols = c("nep_lt", "margen_prop"),
    names_to = "indicador",
    values_to = "valor"
  ) %>%
  mutate(indicador = factor(indicador, levels = c("nep_lt", "margen_prop"), 
                            labels = c("NEP", "Margen"))) %>%
  ggplot(aes(dummy_farc, valor)) +
  geom_col() +
  scale_x_discrete(labels = c("No", "Sí")) +
  facet_wrap(~ indicador, scales = "free_y") +
  labs(x = "Participación partido FARC", y = "Indicador") +
  ggsave("output/dummyfarc_indicadores.png", width = 10, height = 5)

# Modelos de regresion ####

# NEP: todos los datos

nep_mod <- datos %>%
  lm(
  nep_lt ~ tipo_eleccion + dummy_farc*o_homic_last + dummy_pdet + dummy_etcr + dummy_cep   + o_acto_terror_last + o_amenazas_last + o_desap_for_last + o_desplaza_last + o_secuest_last 
  + mdm_2017 + log_pob_2019 +  log_areaoficialkm2 + log_discapital + log_disbogota,
  data = .
)

# NEP: solo alcaldia

nep_mod_alc <- datos %>%
  filter(tipo_eleccion == "ALCALDE") %>%
  lm(
  nep_lt ~ dummy_farc*o_homic_last + dummy_pdet + dummy_etcr + dummy_cep   + o_acto_terror_last + o_amenazas_last + o_desap_for_last + o_desplaza_last + o_secuest_last 
  + mdm_2017 + log_pob_2019 +  log_areaoficialkm2 + log_discapital + log_disbogota,
  data = .
)

# NEP: solo concejo

nep_mod_con <- datos %>%
  filter(tipo_eleccion == "CONCEJO") %>%
  lm(
  nep_lt ~ dummy_farc*o_homic_last + dummy_pdet + dummy_etcr + dummy_cep   + o_acto_terror_last + o_amenazas_last + o_desap_for_last + o_desplaza_last + o_secuest_last 
  + mdm_2017 + log_pob_2019 +  log_areaoficialkm2 + log_discapital + log_disbogota,
  data = .
)

# margen de victoria: todos los datos

marg_mod <- datos %>%
  lm(
  margen_prop ~ tipo_eleccion + dummy_farc*o_homic_last + dummy_pdet + dummy_etcr + dummy_cep   + o_acto_terror_last + o_amenazas_last + o_desap_for_last + o_desplaza_last + o_secuest_last 
  + mdm_2017 + log_pob_2019 +  log_areaoficialkm2 + log_discapital + log_disbogota,
  data = .
)

# margen de victoria: solo alcaldia

marg_mod_alc <- datos %>%
  filter(tipo_eleccion == "ALCALDE") %>%
  lm(
  margen_prop ~ dummy_farc*o_homic_last + dummy_pdet + dummy_etcr + dummy_cep   + o_acto_terror_last + o_amenazas_last + o_desap_for_last + o_desplaza_last + o_secuest_last 
  + mdm_2017 + log_pob_2019 +  log_areaoficialkm2 + log_discapital + log_disbogota,
  data = .
)

# margen de victoria: solo concejo

marg_mod_con <- datos %>%
  filter(tipo_eleccion == "CONCEJO") %>%
  lm(
  margen_prop ~ dummy_farc*o_homic_last + dummy_pdet + dummy_etcr + dummy_cep   + o_acto_terror_last + o_amenazas_last + o_desap_for_last + o_desplaza_last + o_secuest_last 
  + mdm_2017 + log_pob_2019 +  log_areaoficialkm2 + log_discapital + log_disbogota,
  data = .
)

# Tabla de regresion ####

lista_mods <- list(
  nep_mod, nep_mod_alc, nep_mod_con, marg_mod, marg_mod_alc, marg_mod_con
)

htmlreg(
  lista_mods, file = "output/tabla_modelos.doc", doctype = TRUE, html.tag = TRUE, 
  head.tag = TRUE, body.tag = TRUE, caption = "", center = TRUE, digits = 3,
  custom.model.names = c("NEP: todas", "NEP: alcalde", "NEP: concejo",
                         "Margen: todas", "Margen: alcalde", "Margen: concejo"),
  custom.gof.names = c("R<sup>2</sup>", "R<sup>2</sup> ajust.", "Núm. obs.", "RECM"),
  custom.coef.map = list(
    "dummy_pdet1" = "Municipio PDET",
    "dummy_etcr1" = "Municipio ETCR",
    "dummy_cep1" = "Municipio CEP",
    "dummy_farc1" = "Participación FARC",
    "o_homic_last" = "Víctimas por homicidio, 2015-2018",
    "dummy_farc1:o_homic_last" = "FARC &times; homicidio",
    "tipo_eleccionCONCEJO" = "Tipo de elección: Concejo",
    "mdm_2017" = "Desempeño municipal, 2017",
    "log_pob_2019" = "Población, log., 2019",
    "log_areaoficialkm2" = "Área municipal, km.<sup>2</sup>, log.",
    "log_discapital" = "Distancia a capital, log.",
    "log_disbogota" = "Distancia a Bogotá, log."
  )
)

# Graficas de efectos ####

plota <- nep_mod_con %>%
  ggpredict(terms = c("o_homic_last [0:50]", "dummy_farc")) %>%
  ggplot(aes(x, predicted, group = group)) +
  geom_line(aes(linetype = group), size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
  scale_linetype_discrete(labels = c("No", "Sí")) +
  labs(x = "Número de víctimas por homicidio, acumuladas (2015-2018)",
       y = "NEP (predecido)",
       linetype = "Presencia partido FARC")
plota + ggsave("output/interact_nep_mod.png", width = 10, height = 5)

plotb <- marg_mod_con %>%
  ggpredict(terms = c("o_homic_last", "dummy_farc")) %>%
  ggplot(aes(x, predicted, group = group)) +
  geom_line(aes(linetype = group), size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
  scale_linetype_discrete(labels = c("No", "Sí")) +
  labs(x = "Número de víctimas por homicidio, acumuladas (2015-2018)",
       y = "Margen de victoria (predecido)",
       linetype = "Presencia partido FARC") 
plotb + ggsave("output/interact_marg_mod.png", width = 10, height = 5)

plot_grid(plota, plotb, nrow = 2, labels = list("A", "B")) +
  ggsave("output/interact_plots.png", width = 10, height = 10)
