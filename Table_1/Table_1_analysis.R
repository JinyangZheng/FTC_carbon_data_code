data <- readRDS('D:/FTC_carbon_data_code/version3_20261125/Table_1/Table_1_data.rds')

library(nlme)
library(lmerTest)
library(car)

options(contrasts = c('contr.sum', 'contr.poly'))
control <- lmeControl(maxIter = 250, msMaxIter = 400, niterEM = 60, msMaxEval = 2000,
  opt = 'nlminb', returnObject = FALSE)
models <- list(
  Cumulative_Rs = lm(Cumulative_Rs ~ Site + Depth * FTC, data = data$Cumulative_Rs),
  Rs = lme(Response ~ Site + Depth * FTC * Phase + FTC * Day,
    random = ~ 1 | JarID, correlation = corCAR1(value = 0.01, form = ~ Day | JarID),
    data = subset(data$Rs, Day <= 96), method = 'REML', na.action = na.fail, control = control),
  DOC = lme(Response ~ FTC * Depth + Site + Phase + FTC:Phase + FTC:Day,
    random = ~ 1 | JarID, correlation = corCAR1(value = 0.95, form = ~ Day | JarID),
    data = data$DOC, method = 'REML', na.action = na.fail, control = control),
  EEA = lme(Response ~ FTC * Depth + Site + FTC:Day,
    random = ~ 1 | JarID, correlation = corCAR1(value = 0.5, form = ~ Day | JarID),
    data = data$EEA, method = 'REML', na.action = na.fail, control = control),
  MBC = lmer(Response ~ FTC + Site + Depth + (1 | JarID), data = data$MBC,
    REML = TRUE, na.action = na.fail,
    control = lmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 200000))),
  CUE = lmer(Response ~ FTC + Site + Depth + (1 | JarID), data = data$CUE,
    REML = TRUE, na.action = na.fail,
    control = lmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 200000)))
)

cumulative_anova <- as.data.frame(car::Anova(models$Cumulative_Rs, type = 3))
tests <- c(
  list(Cumulative_Rs = data.frame(Effect = rownames(cumulative_anova), Num_df = cumulative_anova$Df,
    Den_df = df.residual(models$Cumulative_Rs), F_value = cumulative_anova$`F value`)),
  lapply(models[c('Rs', 'DOC', 'EEA')], function(fit) {
    a <- as.data.frame(anova(fit, type = 'marginal'))
    data.frame(Effect = rownames(a), Num_df = a$numDF, Den_df = a$denDF, F_value = a$`F-value`)
  }),
  lapply(models[c('MBC', 'CUE')], function(fit) {
    a <- as.data.frame(anova(fit, type = 3, ddf = 'Satterthwaite'))
    data.frame(Effect = rownames(a), Num_df = a$NumDF, Den_df = a$DenDF, F_value = a$`F value`)
  })
)
Table_1 <- do.call(rbind, lapply(names(tests), function(variable) {
  result <- tests[[variable]]
  result <- result[!result$Effect %in% c('(Intercept)', 'Residuals'), ]
  result$P_value <- pf(result$F_value, result$Num_df, result$Den_df, lower.tail = FALSE)
  result$Significance <- as.character(cut(result$P_value, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
    labels = c('***', '**', '*', 'ns'), right = FALSE))
  result$Variable <- variable
  result[c('Variable', 'Effect', 'Num_df', 'Den_df', 'F_value', 'P_value', 'Significance')]
}))
order_by_variable <- list(Cumulative_Rs = c('FTC', 'Site', 'Depth', 'Depth:FTC'),
  Rs = c('FTC', 'Phase', 'Day', 'Site', 'Depth', 'FTC:Phase', 'FTC:Day',
    'Depth:FTC', 'Depth:Phase', 'Depth:FTC:Phase'),
  DOC = c('FTC', 'Phase', 'Depth', 'Site', 'FTC:Day', 'FTC:Phase', 'FTC:Depth'),
  EEA = c('FTC', 'Depth', 'Site', 'FTC:Day', 'FTC:Depth'),
  MBC = c('FTC', 'Site', 'Depth'), CUE = c('FTC', 'Site', 'Depth'))
Table_1 <- do.call(rbind, lapply(names(order_by_variable), function(variable) {
  values <- Table_1[Table_1$Variable == variable, ]
  values[match(order_by_variable[[variable]], values$Effect), ]
}))
rownames(Table_1) <- NULL
output_dir <- 'Your_pathway_outputs/Table_1'
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(Table_1, file.path(output_dir, 'Table_1.csv'), row.names = FALSE)
print(Table_1, row.names = FALSE)
