######################################
# LCGA estimation
# Outcome: 
# Scale: 1 = Never ... 5 = Always (ELRI waves 2021 and 2023)
######################################


#t10	Percepcion de seguridad del barrio <-- Esta vamos a modelar.
#c05_03	Grado de confianza: Carabineros
# c41_08	Grado de rabia 2: El accionar de las Fuerzas de Seguridad en las manifestaciones

# Percepcion de seguridad del barrio

rm(list=ls())
load(url("https://dataverse.harvard.edu/api/access/datafile/10735184"))
library(lcmm)
library(dplyr)
library(misty)
df_lcga<-select(elsoc_long_2016_2023, idencuesta, ola, c37_05)
df_lcga <- as.na(df_lcga, na = c(-666, -888, -999))
df_lcga<-subset(df_lcga, ola>2)
df_lcga<-df_lcga %>% na.omit()
df_lcga$ola<-df_lcga$ola - 3


df_lcga <- select(BBDD_ELRI_LONG, folio, ola, c32_2)
df_lcga$c32_2 <- as.numeric(zap_labels(df_lcga$c32_2))
df_lcga <- as.na(df_lcga, na = c(-666, -888, -999, 88, 99, 8888, 9999))
df_lcga <- subset(df_lcga, ola > 2)
df_lcga <- df_lcga %>% na.omit()
df_lcga$ola <- df_lcga$ola - 3
######################################################
# LCGA - Intercepto y coeficiente fijo por cada clase
######################################################

# run models with 1-4 classes, each with 100 random starts,
# using the 1-class model to set initial start values:
# (Modelos se demoran en estimar)
lcga1 <- hlme(c32_2 ~ ola, subject = "folio", ng = 1, data = df_lcga)
lcga2 <- gridsearch(rep = 100, maxiter = 10, minit = lcga1,
                    m=hlme(c32_2 ~ ola, subject = "folio",
                           ng = 2, data = df_lcga, mixture = ~ ola))
lcga3 <- gridsearch(rep = 100, maxiter = 10, minit = lcga1,
                    m=hlme(c32_2 ~ ola, subject = "folio",
                           ng = 3, data = df_lcga, mixture = ~ ola))
lcga4 <- gridsearch(rep = 100, maxiter = 10, minit = lcga1,
                    m=hlme(c32_2 ~ ola, subject = "folio",
                           ng = 4, data = df_lcga, mixture = ~ ola))

# make table with results for the 3 models:
summarytable(lcga1, lcga2, lcga3, which = c("AIC","BIC", "entropy", "conv", "loglik", "npm", "%class"))

# Estimamos modelos con término cuadrático
lcga1_sq <- hlme(c32_2 ~ ola + I(ola^2), subject = "folio", ng = 1, data = df_lcga)
lcga2_sq <- gridsearch(rep = 100, maxiter = 10, minit = lcga1_sq,
                       m=hlme(c32_2 ~ ola + I(ola^2), subject = "folio",
                              ng = 2, data = df_lcga, mixture = ~ ola + I(ola^2)))
lcga3_sq <- gridsearch(rep = 100, maxiter = 10, minit = lcga1_sq,
                       m=hlme(c32_2 ~ ola + I(ola^2), subject = "folio",
                              ng = 3, data = df_lcga, mixture = ~ ola + I(ola^2)))
lcga4_sq <- gridsearch(rep = 100, maxiter = 10, minit = lcga1_sq,
                       m=hlme(c32_2 ~ ola + I(ola^2), subject = "folio",
                              ng = 4, data = df_lcga, mixture = ~ ola + I(ola^2)))


# make table with results for the 3 models:
summarytable(lcga1, lcga2, lcga3, lcga4, lcga1_sq, lcga2_sq, lcga3_sq, lcga4_sq, which = c("AIC","BIC", "entropy", "conv", "loglik", "npm", "%class"))

# Assign latent class membership from the final 4-class model (lcga4)
# Selected based on lowest BIC/AIC, highest entropy, and all classes >10%
df_clases <- lcga4$pprob[, c("folio", "class")]
df_lcga <- merge(df_lcga, df_clases, by = "folio", all.x = TRUE)

df_lcga$class <- factor(
  df_lcga$class,
  labels = c(
    "Consistently willing (31.5%)",
    "Increasing willingness (12.8%)",
    "Consistently unwilling (37.2%)"
  )
)

###############################
# Observed means by class (95% CI)
###############################

library(ggplot2)

df_plot <- df_lcga %>%
  group_by(ola, class) %>%
  summarise(
    mean_c32_2 = mean(c32_2, na.rm = TRUE),
    sd_c32_2 = sd(c32_2, na.rm = TRUE),
    n = n(),
    se = sd_c32_2 / sqrt(n),
    ci_lower = mean_c32_2 - 1.96 * se,
    ci_upper = mean_c32_2 + 1.96 * se,
    .groups = "drop"
  )

dir.create("data/data_proc", recursive = TRUE, showWarnings = FALSE)
save.image(file = "data/data_proc/data_trayectorias.Rdata")