rm(list = ls())
load("/tmp/elsoc_long.RData")
library(lcmm)
library(dplyr)
library(misty)

df_lcga <- select(elsoc_long_2016_2023, idencuesta, ola, t10)
df_lcga$c32_2 <- as.numeric(df_lcga$t10)
df_lcga$t10 <- NULL
df_lcga <- as.na(df_lcga, na = c(-666, -777, -888, -999))
df_lcga <- subset(df_lcga, ola > 2 & ola != 5)
df_lcga <- df_lcga %>% na.omit()
panel_olas <- c(3, 4, 6, 7)
ids_completos <- df_lcga %>%
  group_by(idencuesta) %>%
  filter(n() == length(panel_olas)) %>%
  pull(idencuesta) %>%
  unique()
df_lcga <- df_lcga %>% filter(idencuesta %in% ids_completos)
df_lcga$ola <- match(df_lcga$ola, panel_olas) - 1L

cat("N personas:", length(unique(df_lcga$idencuesta)), "\n")
cat("N obs:", nrow(df_lcga), "\n\n")

cat("=== Modelos lineales ===\n")
lcga1 <- hlme(c32_2 ~ ola, subject = "idencuesta", ng = 1, data = df_lcga)
lcga2 <- gridsearch(rep = 100, maxiter = 10, minit = lcga1,
                    m = hlme(c32_2 ~ ola, subject = "idencuesta",
                             ng = 2, data = df_lcga, mixture = ~ ola))
lcga3 <- gridsearch(rep = 100, maxiter = 10, minit = lcga1,
                    m = hlme(c32_2 ~ ola, subject = "idencuesta",
                             ng = 3, data = df_lcga, mixture = ~ ola))
lcga4 <- gridsearch(rep = 100, maxiter = 10, minit = lcga1,
                    m = hlme(c32_2 ~ ola, subject = "idencuesta",
                             ng = 4, data = df_lcga, mixture = ~ ola))
print(summarytable(lcga1, lcga2, lcga3, lcga4,
                   which = c("AIC", "BIC", "entropy", "conv", "loglik", "npm", "%class")))

cat("\n=== Modelos cuadraticos ===\n")
lcga1_sq <- hlme(c32_2 ~ ola + I(ola^2), subject = "idencuesta", ng = 1, data = df_lcga)
lcga2_sq <- gridsearch(rep = 100, maxiter = 10, minit = lcga1_sq,
                       m = hlme(c32_2 ~ ola + I(ola^2), subject = "idencuesta",
                                ng = 2, data = df_lcga, mixture = ~ ola + I(ola^2)))
lcga3_sq <- gridsearch(rep = 100, maxiter = 10, minit = lcga1_sq,
                       m = hlme(c32_2 ~ ola + I(ola^2), subject = "idencuesta",
                                ng = 3, data = df_lcga, mixture = ~ ola + I(ola^2)))
lcga4_sq <- gridsearch(rep = 100, maxiter = 10, minit = lcga1_sq,
                       m = hlme(c32_2 ~ ola + I(ola^2), subject = "idencuesta",
                                ng = 4, data = df_lcga, mixture = ~ ola + I(ola^2)))
print(summarytable(lcga1, lcga2, lcga3, lcga4, lcga1_sq, lcga2_sq, lcga3_sq, lcga4_sq,
                   which = c("AIC", "BIC", "entropy", "conv", "loglik", "npm", "%class")))

print_class_means <- function(model, df, label) {
  df_clases <- model$pprob[, c("idencuesta", "class")]
  df_tmp <- merge(df, df_clases, by = "idencuesta")
  years <- c("2018", "2019", "2022", "2023")
  cat(sprintf("\n=== Medias observadas por clase (%s) ===\n", label))
  for (cl in sort(unique(df_tmp$class))) {
    sub <- df_tmp[df_tmp$class == cl, ]
    pct <- round(100 * length(unique(sub$idencuesta)) / length(unique(df_tmp$idencuesta)), 1)
    cat(sprintf("\nClase %d (%.1f%%):\n", cl, pct))
    for (t in 0:3) {
      m <- mean(sub$c32_2[sub$ola == t])
      cat(sprintf("  %s: %.2f\n", years[t + 1], m))
    }
  }
  invisible(df_tmp)
}

df_tmp4 <- print_class_means(lcga4, df_lcga, "lcga4 lineal")
df_tmp3 <- print_class_means(lcga3, df_lcga, "lcga3 lineal")

dir.create("elsoc/data/data_proc", recursive = TRUE, showWarnings = FALSE)
save(lcga1, lcga2, lcga3, lcga4, lcga1_sq, lcga2_sq, lcga3_sq, lcga4_sq,
     df_lcga, df_tmp4, df_tmp3,
     file = "elsoc/data/data_proc/lcga_results_temp.Rdata")
