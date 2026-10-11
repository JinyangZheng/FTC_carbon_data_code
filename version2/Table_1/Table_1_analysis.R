library(nlme)
library(lmerTest)
library(car)

source_files <- Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))
script_argument <- grep('^--file=', commandArgs(FALSE), value = TRUE)
script_path <- if (length(source_files)) tail(source_files, 1L)[[1L]] else
  if (length(script_argument)) sub('^--file=', '', script_argument[1L]) else 'Table_1_analysis.R'
data_dir <- dirname(normalizePath(script_path, winslash = '/', mustWork = FALSE))
data <- readRDS(file.path(data_dir, 'Table_1_data.rds'))

options(contrasts = c('contr.sum', 'contr.poly'))
control <- lmeControl(maxIter = 250, msMaxIter = 400, niterEM = 60, msMaxEval = 2000,
  opt = 'nlminb', returnObject = FALSE)
models <- list(
  Cumulative_Rs = lm(Cumulative_Rs ~ Site + Depth * FTC, data = data$Cumulative_Rs),
  Rs = lme(Response ~ Site + Depth * FTC * Phase + FTC * Day,
    random = ~ 1 | JarID, correlation = corCAR1(value = 0.995, form = ~ Day | JarID),
    data = data$Rs, method = 'REML', na.action = na.fail, control = control),
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

tests <- lapply(names(models), function(variable) {
  fit <- models[[variable]]
  if (inherits(fit, 'lme')) {
    a <- as.data.frame(anova(fit, type = 'marginal'))
    result <- data.frame(Effect = rownames(a), Num_df = a$numDF,
      Den_df = a$denDF, F_value = a$`F-value`)
  } else if (inherits(fit, 'merMod')) {
    a <- as.data.frame(anova(fit, type = 3, ddf = 'Satterthwaite'))
    result <- data.frame(Effect = rownames(a), Num_df = a$NumDF,
      Den_df = a$DenDF, F_value = a$`F value`)
  } else {
    a <- as.data.frame(car::Anova(fit, type = 3))
    result <- data.frame(Effect = rownames(a), Num_df = a$Df,
      Den_df = df.residual(fit), F_value = a$`F value`)
  }
  result <- result[!result$Effect %in% c('(Intercept)', 'Residuals'), ]
  result$P_value <- pf(result$F_value, result$Num_df, result$Den_df, lower.tail = FALSE)
  result$Significance <- ifelse(result$P_value < 0.001, '***',
    ifelse(result$P_value < 0.01, '**', ifelse(result$P_value < 0.05, '*', 'ns')))
  result$Variable <- variable
  result[c('Variable', 'Effect', 'Num_df', 'Den_df', 'F_value', 'P_value', 'Significance')]
})
Table_1 <- do.call(rbind, tests)
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
arguments <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(arguments)) arguments[1L] else Sys.getenv('FTC_OUTPUT_DIR',
  unset = file.path(dirname(dirname(data_dir)), 'version2_outputs', 'Table_1'))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(Table_1, file.path(output_dir, 'Table_1.csv'), row.names = FALSE)
print(Table_1, row.names = FALSE)
