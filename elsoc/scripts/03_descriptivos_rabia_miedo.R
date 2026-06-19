######################################
# Descriptivos tendenciales — ELSOC
# Rabia (c41) y miedo (c42) ante conflictos sociales
######################################

# c41_01–c41_04 / c42_01–c42_04: olas 2019, 2021, 2022 (4, 5, 6)
# c41_05–c41_08 / c42_05–c42_08: solo ola 2023 (7), muestra 2
# Escala: 1 = Nada ... 5 = Mucha/o

rm(list = ls())
if (file.exists("/tmp/elsoc_long.RData")) {
  load("/tmp/elsoc_long.RData")
} else {
  load(url("https://dataverse.harvard.edu/api/access/datafile/10735184"))
}
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

miss_codes <- c(-666, -777, -888, -999)
ola_years <- c(`1` = 2016, `2` = 2017, `3` = 2018, `4` = 2019,
               `5` = 2021, `6` = 2022, `7` = 2023)

var_labels <- c(
  c41_01 = "Rabia: Desigualdad en Chile",
  c41_02 = "Rabia: Costo de la vida",
  c41_03 = "Rabia: Manifestantes violentos",
  c41_04 = "Rabia: Fuerzas de Seguridad en manifestaciones",
  c41_05 = "Rabia 2: Desigualdad en Chile",
  c41_06 = "Rabia 2: Costo de la vida",
  c41_07 = "Rabia 2: Manifestantes violentos",
  c41_08 = "Rabia 2: Fuerzas de Seguridad en manifestaciones",
  c42_01 = "Miedo: Desigualdad en Chile",
  c42_02 = "Miedo: Costo de la vida",
  c42_03 = "Miedo: Manifestantes violentos",
  c42_04 = "Miedo: Fuerzas de Seguridad en manifestaciones",
  c42_05 = "Miedo 2: Desigualdad en Chile",
  c42_06 = "Miedo 2: Costo de la vida",
  c42_07 = "Miedo 2: Manifestantes violentos",
  c42_08 = "Miedo 2: Fuerzas de Seguridad en manifestaciones"
)

vars_trend <- paste0("c4", rep(c(1, 2), each = 4), "_", sprintf("%02d", 1:4))
vars_trend <- intersect(vars_trend, names(elsoc_long_2016_2023))
vars_cross <- paste0("c4", rep(c(1, 2), each = 4), "_", sprintf("%02d", 5:8))
vars_cross <- intersect(vars_cross, names(elsoc_long_2016_2023))

clean_var <- function(x) {
  x <- as.numeric(x)
  x[x %in% miss_codes] <- NA
  x
}

descriptivos_ola <- function(data, variables, olas = NULL) {
  long <- data %>%
    select(idencuesta, ola, all_of(variables)) %>%
    pivot_longer(all_of(variables), names_to = "variable", values_to = "valor") %>%
    mutate(
      valor = clean_var(valor),
      year = unname(ola_years[as.character(ola)])
    )

  if (!is.null(olas)) long <- long %>% filter(ola %in% olas)

  long %>%
    filter(!is.na(valor)) %>%
    group_by(variable, ola, year) %>%
    summarise(
      n = n(),
      mean = mean(valor),
      sd = sd(valor),
      median = median(valor),
      q25 = quantile(valor, 0.25),
      q75 = quantile(valor, 0.75),
      min = min(valor),
      max = max(valor),
      .groups = "drop"
    ) %>%
    mutate(etiqueta = unname(var_labels[variable]))
}

desc_trend <- descriptivos_ola(elsoc_long_2016_2023, vars_trend, olas = c(4, 5, 6))
desc_cross <- descriptivos_ola(elsoc_long_2016_2023, vars_cross, olas = 7)

desc_panel_balanceado <- local({
  d <- elsoc_long_2016_2023
  panel_olas <- c(4, 5, 6)
  ok_ids <- lapply(vars_trend, function(v) {
    x <- clean_var(d[[v]])
    d %>% mutate(ok = !is.na(x)) %>%
      filter(ola %in% panel_olas, ok) %>%
      group_by(idencuesta) %>%
      filter(n() == length(panel_olas)) %>%
      pull(idencuesta) %>%
      unique()
  })
  ids_completos <- Reduce(intersect, ok_ids)

  d %>%
    filter(idencuesta %in% ids_completos) %>%
    descriptivos_ola(vars_trend, olas = panel_olas) %>%
    mutate(muestra = "Panel balanceado 2019-2021-2022")
})

dir.create("elsoc/data/data_proc", recursive = TRUE, showWarnings = FALSE)
dir.create("elsoc/output", recursive = TRUE, showWarnings = FALSE)

write_csv(desc_trend, "elsoc/data/data_proc/descriptivos_rabia_miedo_tendencia.csv")
write_csv(desc_cross, "elsoc/data/data_proc/descriptivos_rabia_miedo_2023.csv")
write_csv(desc_panel_balanceado, "elsoc/data/data_proc/descriptivos_rabia_miedo_panel_balanceado.csv")

cat("=== Descriptivos tendenciales (todas las respuestas válidas) ===\n")
print(as.data.frame(desc_trend), row.names = FALSE)

cat("\n=== Panel balanceado 2019-2021-2022 (N personas = ",
    length(Reduce(intersect, lapply(vars_trend, function(v) {
      x <- clean_var(elsoc_long_2016_2023[[v]])
      elsoc_long_2016_2023 %>%
        mutate(ok = !is.na(x)) %>%
        filter(ola %in% c(4, 5, 6), ok) %>%
        group_by(idencuesta) %>%
        filter(n() == 3) %>%
        pull(idencuesta) %>%
        unique()
    }))), ")\n", sep = "")
print(as.data.frame(desc_panel_balanceado), row.names = FALSE)

cat("\n=== Descriptivos transversales 2023 (c41_05–c42_08) ===\n")
print(as.data.frame(desc_cross), row.names = FALSE)

plot_trend <- desc_trend %>%
  ggplot(aes(x = factor(year), y = mean, colour = variable, group = variable)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ etiqueta, scales = "free_y", ncol = 2) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    title = "Tendencia de rabia y miedo — ELSOC 2019, 2021, 2022",
    subtitle = "Medias muestrales (todas las respuestas válidas por ola)",
    x = "Año",
    y = "Media (1 = nada, 5 = mucha/o)",
    colour = "Variable"
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")

ggsave("elsoc/output/tendencia_rabia_miedo_2019_2022.png", plot_trend,
       width = 12, height = 14, dpi = 300)

plot_rabia <- desc_trend %>%
  filter(grepl("^c41", variable)) %>%
  ggplot(aes(x = factor(year), y = mean, colour = etiqueta, group = etiqueta)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    title = "Grado de rabia — tendencia 2019-2022",
    x = "Año", y = "Media", colour = NULL
  ) +
  theme_minimal(base_size = 11)

plot_miedo <- desc_trend %>%
  filter(grepl("^c42", variable)) %>%
  ggplot(aes(x = factor(year), y = mean, colour = etiqueta, group = etiqueta)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    title = "Grado de miedo — tendencia 2019-2022",
    x = "Año", y = "Media", colour = NULL
  ) +
  theme_minimal(base_size = 11)

ggsave("elsoc/output/tendencia_rabia_2019_2022.png", plot_rabia, width = 10, height = 6, dpi = 300)
ggsave("elsoc/output/tendencia_miedo_2019_2022.png", plot_miedo, width = 10, height = 6, dpi = 300)

save(desc_trend, desc_cross, desc_panel_balanceado, var_labels,
     file = "elsoc/data/data_proc/descriptivos_rabia_miedo.Rdata")
