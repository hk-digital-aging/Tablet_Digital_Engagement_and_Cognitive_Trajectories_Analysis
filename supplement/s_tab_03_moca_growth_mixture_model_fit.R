library(tidyr)
library(lcmm)
library(stringr)
library(plyr)
library(readr)
library(ggplot2)

df <- read.csv(
  "~/Downloads/Suppl_data_3_Metadata_9MCumulativeUsage.csv",
  stringsAsFactors = FALSE
)

df_1 <- subset(
  df,
  select = c("Case_ID", "MoCA_T0", "MoCA_T1", "MoCA_T2", "MoCA_T3")
)

df_long <- gather(df_1, Time, Scores, -Case_ID)
df_long$Time <- gsub("MoCA_T", "", df_long$Time)

dat <- df_long
dat$Time <- as.numeric(dat$Time)
dat$Time1 <- poly(dat$Time, 2)[, 1]
dat$Time2 <- poly(dat$Time, 2)[, 2]

rep <- 100
set.seed(2002)

dat$Case_ID <- substr(dat$Case_ID, start = 3, stop = nchar(dat$Case_ID))
dat$Case_ID <- as.numeric(dat$Case_ID)

# Fixed-effects models
e_fe <- list()
e_fe[[1]] <- hlme(Scores ~ Time1 + Time2, subject = "Case_ID", ng = 1, data = dat)
e_fe[[2]] <- gridsearch(rep = rep, maxiter = 10, minit = e_fe[[1]], cl = 4,
                        hlme(Scores ~ Time1 + Time2, subject = "Case_ID", ng = 2, data = dat, mixture = ~ Time1 + Time2))
e_fe[[3]] <- gridsearch(rep = rep, maxiter = 10, minit = e_fe[[1]], cl = 4,
                        hlme(Scores ~ Time1 + Time2, subject = "Case_ID", ng = 3, data = dat, mixture = ~ Time1 + Time2))
e_fe[[4]] <- gridsearch(rep = rep, maxiter = 10, minit = e_fe[[1]], cl = 4,
                        hlme(Scores ~ Time1 + Time2, subject = "Case_ID", ng = 4, data = dat, mixture = ~ Time1 + Time2))
e_fe[[5]] <- gridsearch(rep = rep, maxiter = 10, minit = e_fe[[1]], cl = 4,
                        hlme(Scores ~ Time1 + Time2, subject = "Case_ID", ng = 5, data = dat, mixture = ~ Time1 + Time2))
e_fe[[6]] <- gridsearch(rep = rep, maxiter = 10, minit = e_fe[[1]], cl = 4,
                        hlme(Scores ~ Time1 + Time2, subject = "Case_ID", ng = 6, data = dat, mixture = ~ Time1 + Time2))
e_fe[[7]] <- gridsearch(rep = rep, maxiter = 10, minit = e_fe[[1]], cl = 4,
                        hlme(Scores ~ Time1 + Time2, subject = "Case_ID", ng = 7, data = dat, mixture = ~ Time1 + Time2))

# Random-intercept models
e_randI <- list()
e_randI[[1]] <- hlme(Scores ~ Time1 + Time2, random = ~1, subject = "Case_ID", ng = 1, data = dat)
e_randI[[2]] <- gridsearch(rep = rep, maxiter = 10, minit = e_randI[[1]], cl = 4,
                           hlme(Scores ~ Time1 + Time2, random = ~1, subject = "Case_ID", ng = 2, data = dat, mixture = ~ Time1 + Time2))
e_randI[[3]] <- gridsearch(rep = rep, maxiter = 10, minit = e_randI[[1]], cl = 4,
                           hlme(Scores ~ Time1 + Time2, random = ~1, subject = "Case_ID", ng = 3, data = dat, mixture = ~ Time1 + Time2))
e_randI[[4]] <- gridsearch(rep = rep, maxiter = 10, minit = e_randI[[1]], cl = 4,
                           hlme(Scores ~ Time1 + Time2, random = ~1, subject = "Case_ID", ng = 4, data = dat, mixture = ~ Time1 + Time2))
e_randI[[5]] <- gridsearch(rep = rep, maxiter = 10, minit = e_randI[[1]], cl = 4,
                           hlme(Scores ~ Time1 + Time2, random = ~1, subject = "Case_ID", ng = 5, data = dat, mixture = ~ Time1 + Time2))
e_randI[[6]] <- gridsearch(rep = rep, maxiter = 10, minit = e_randI[[1]], cl = 4,
                           hlme(Scores ~ Time1 + Time2, random = ~1, subject = "Case_ID", ng = 6, data = dat, mixture = ~ Time1 + Time2))
e_randI[[7]] <- gridsearch(rep = rep, maxiter = 10, minit = e_randI[[1]], cl = 4,
                           hlme(Scores ~ Time1 + Time2, random = ~1, subject = "Case_ID", ng = 7, data = dat, mixture = ~ Time1 + Time2))

# Random-intercept and slope models
e_randIS <- list()
e_randIS[[1]] <- hlme(Scores ~ Time1 + Time2, random = ~Time1, subject = "Case_ID", ng = 1, data = dat)
e_randIS[[2]] <- gridsearch(rep = rep, maxiter = 10, minit = e_randIS[[1]], cl = 4,
                            hlme(Scores ~ Time1 + Time2, random = ~Time1, subject = "Case_ID", ng = 2, data = dat, mixture = ~ Time1 + Time2))
e_randIS[[3]] <- gridsearch(rep = rep, maxiter = 10, minit = e_randIS[[1]], cl = 4,
                            hlme(Scores ~ Time1 + Time2, random = ~Time1, subject = "Case_ID", ng = 3, data = dat, mixture = ~ Time1 + Time2))
e_randIS[[4]] <- gridsearch(rep = rep, maxiter = 10, minit = e_randIS[[1]], cl = 4,
                            hlme(Scores ~ Time1 + Time2, random = ~Time1, subject = "Case_ID", ng = 4, data = dat, mixture = ~ Time1 + Time2))
e_randIS[[5]] <- gridsearch(rep = rep, maxiter = 10, minit = e_randIS[[1]], cl = 4,
                            hlme(Scores ~ Time1 + Time2, random = ~Time1, subject = "Case_ID", ng = 5, data = dat, mixture = ~ Time1 + Time2))
e_randIS[[6]] <- gridsearch(rep = rep, maxiter = 10, minit = e_randIS[[1]], cl = 4,
                            hlme(Scores ~ Time1 + Time2, random = ~Time1, subject = "Case_ID", ng = 6, data = dat, mixture = ~ Time1 + Time2))
e_randIS[[7]] <- gridsearch(rep = rep, maxiter = 10, minit = e_randIS[[1]], cl = 4,
                            hlme(Scores ~ Time1 + Time2, random = ~Time1, subject = "Case_ID", ng = 7, data = dat, mixture = ~ Time1 + Time2))

# Random-intercept and quadratic-slope models
e_randISQ <- list()
e_randISQ[[1]] <- hlme(Scores ~ Time1 + Time2, random = ~Time1 + Time2, subject = "Case_ID", ng = 1, data = dat)
e_randISQ[[2]] <- gridsearch(rep = rep, maxiter = 10, minit = e_randISQ[[1]], cl = 4,
                             hlme(Scores ~ Time1 + Time2, random = ~Time1 + Time2, subject = "Case_ID", ng = 2, data = dat, mixture = ~ Time1 + Time2))
e_randISQ[[3]] <- gridsearch(rep = rep, maxiter = 10, minit = e_randISQ[[1]], cl = 4,
                             hlme(Scores ~ Time1 + Time2, random = ~Time1 + Time2, subject = "Case_ID", ng = 3, data = dat, mixture = ~ Time1 + Time2))
e_randISQ[[4]] <- gridsearch(rep = rep, maxiter = 10, minit = e_randISQ[[1]], cl = 4,
                             hlme(Scores ~ Time1 + Time2, random = ~Time1 + Time2, subject = "Case_ID", ng = 4, data = dat, mixture = ~ Time1 + Time2))
e_randISQ[[5]] <- gridsearch(rep = rep, maxiter = 10, minit = e_randISQ[[1]], cl = 4,
                             hlme(Scores ~ Time1 + Time2, random = ~Time1 + Time2, subject = "Case_ID", ng = 5, data = dat, mixture = ~ Time1 + Time2))
e_randISQ[[6]] <- gridsearch(rep = rep, maxiter = 10, minit = e_randISQ[[1]], cl = 4,
                             hlme(Scores ~ Time1 + Time2, random = ~Time1 + Time2, subject = "Case_ID", ng = 6, data = dat, mixture = ~ Time1 + Time2))
e_randISQ[[7]] <- gridsearch(rep = rep, maxiter = 10, minit = e_randISQ[[1]], cl = 4,
                             hlme(Scores ~ Time1 + Time2, random = ~Time1 + Time2, subject = "Case_ID", ng = 7, data = dat, mixture = ~ Time1 + Time2))

# Collect BIC values
e_tableBIC <- list()
for (i in c("e_fe", "e_randI", "e_randIS", "e_randISQ")) {
  e_tableBIC[[i]] <- as.vector(
    summarytable(
      get(i)[[1]], get(i)[[2]], get(i)[[3]], get(i)[[4]],
      get(i)[[5]], get(i)[[6]], get(i)[[7]],
      which = c("BIC")
    )
  )
  names(e_tableBIC[[i]]) <- 1:7
}

e_bestmodels <- sort(unlist(e_tableBIC))
e_tableBIC <- as.data.frame(e_tableBIC)

save(
  e_tableBIC, e_bestmodels, e_fe, e_randI, e_randIS, e_randISQ,
  file = paste0("~/Downloads/MoCA_", rep, "rep_growth_models.RData")
)

# Extract six best models
e_bmt <- list()
for (i in names(e_bestmodels)[1:6]) {
  model <- get(sub("..$", "", i))[[as.numeric(str_sub(i, start = -1))]]
  invisible(capture.output(
    e_bmt[[i]] <- as.data.frame(
      summarytable(model, which = c("G", "AIC", "BIC", "SABIC", "entropy", "%class"))
    )
  ))
}

e_best_models_table <- plyr::rbind.fill(
  as.data.frame(e_bmt[[1]]),
  as.data.frame(e_bmt[[2]]),
  as.data.frame(e_bmt[[3]]),
  as.data.frame(e_bmt[[4]]),
  as.data.frame(e_bmt[[5]]),
  as.data.frame(e_bmt[[6]])
)

write_csv(
  e_best_models_table,
  "~/Downloads/supplementary_table_3_best_growth_models.csv"
)

# Chosen model
e_chosen_model_name <- "e_randI.4"
e_chosen_model <- get(sub("..$", "", e_chosen_model_name))[[as.numeric(str_sub(e_chosen_model_name, start = -1))]]

dat$Class <- factor(e_chosen_model$pprob$class[match(dat$Case_ID, e_chosen_model$pprob$Case_ID)])

p1 <- ggplot(dat, aes(x = Time, y = Scores, group = Class, colour = Class)) +
  geom_line(alpha = 0) +
  geom_smooth(aes(group = Class), method = "lm", formula = y ~ poly(x, degree = 2)) +
  scale_y_continuous(limits = c(0, 25)) +
  labs(
    x = "Time",
    y = "MoCA score",
    colour = "Latent class"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

print(p1)

write.csv(
  dat,
  "~/Downloads/supplementary_table_3_chosen_model_data.csv",
  row.names = FALSE
)
