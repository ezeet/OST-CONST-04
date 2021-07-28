##############################################
####### OST-CONST-04_Analyses2_strict ########
##############################################

# Load packages
library(ggplot2)
library(magrittr)
library("ggpubr")
library(Matrix)
library(languageR)
library(dplyr)
library(lsr)
library(tidyr)
library(reshape)
library(lme4)
library(emmeans)
library(lmerTest)
library(lsmeans)
library(rcompanion)
library(car)
library(rstatix)

# Sets working directory to folder where R-script is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


#### Mixed Models Analysis
# Conditions need to be centered to -0.5 and 0.5 (instead of 1 and 2)
long_data_clean_strict %<>%
  dplyr::mutate(ostracism_condition_c = dplyr::if_else(condition = ostracism_condition == "included",
                                                       true = -0.5,
                                                       false = 0.5)) %>%
  dplyr::mutate(self_other_condition_c = dplyr::if_else(condition = self_other_condition == "yellow",
                                                        true = -0.5,
                                                        false = 0.5))

# We only look at negative outcomes (as stated in our preregistration)
long_neg <-
  long_data_clean_strict %>%
  dplyr::filter(outcome %in% c(1:5))

# Check if the centering is done correclty
table(long_neg$ostracism_condition, long_neg$ostracism_condition_c)
table(long_neg$self_other_condition, long_neg$self_other_condition_c)

# How many people are in the study?
length(unique(long_neg$id))

# Stability analyses
# First overall overview
dplyr::group_by(long_neg, ostracism_condition, self_other_condition) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(stability_value, na.rm = TRUE),
                   sd = stats::sd(stability_value, na.rm = TRUE))

# Plot stability
Sum_stability <- rcompanion::groupwiseMean(stability_value ~ ostracism_condition + self_other_condition,
                                           data   = long_neg,
                                           conf   = 0.95,
                                           digits = 3,
                                           traditional = FALSE,
                                           percentile  = TRUE)
pd = ggplot2::position_dodge(.2)
ggplot2::ggplot(Sum_stability, ggplot2::aes(x = ostracism_condition,
                                            y = Mean,
                                            color = self_other_condition)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=Percentile.lower,
                                      ymax=Percentile.upper),
                         width=.2, size=0.7, position=pd) +
  ggplot2::geom_point(shape=15, size=4, position=pd) +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))  +
  ggplot2::geom_point(shape=15, size=4, position=pd) + ggplot2::ylim(1, 5) +
  ggplot2::ylab("Mean stability value (1 = stable; 5 = variable)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))

# Is self_other_condition a good predictor for Stability?
fit_stab_1 <- lme4::lmer (stability_value ~ 1 + (1 | id) + (1 | outcome), data = long_neg)
fit_stab_2 <- lme4::lmer (stability_value ~ self_other_condition_c + (1 | id) + (1 | outcome), data = long_neg)
stats::anova(fit_stab_1, fit_stab_2) # Fit1 better

# Haupteffekt self_other_condition auf Stability
summary(fit_stab_2)

dplyr::group_by(long_neg, self_other_condition) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(stability_value, na.rm = TRUE),
                   sd = stats::sd(stability_value, na.rm = TRUE))
# Mean difference Group Yellow - Group Green

# Is Ostracism a good predictor for Stability?
fit_stab_1 <- lme4::lmer (stability_value ~ 1 + (1 | id) + (1 | outcome), data = long_neg)
fit_stab_3 <- lme4::lmer (stability_value ~ ostracism_condition_c + (1 | id) + (1 | outcome), data = long_neg)
stats::anova (fit_stab_1, fit_stab_3) # Fit1 better

# Haupteffekt ostracism_condition auf Stability
summary(fit_stab_3)

dplyr::group_by(long_neg, ostracism_condition) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(stability_value, na.rm = TRUE),
                   sd = stats::sd(stability_value, na.rm = TRUE))
# Mean difference Group Excluded - Group Included

# Interaction model
fit_stability <- lme4::lmer (stability_value ~ self_other_condition_c * ostracism_condition_c + (1 |id) + (1 | outcome), data = long_neg)
summary(fit_stability)

dplyr::group_by(long_neg, ostracism_condition, self_other_condition) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(stability_value, na.rm = TRUE),
                   sd = stats::sd(stability_value, na.rm = TRUE))

# Mean difference Interaction Stability
# (Excluded-Yellow - Excluded-Green) - ((Included-Yellow - Included-Green))
# (3.70 - 3.26) - (3.68 - 3.51) = 0.27

### Try to also calculate the effect sizes fore mixed models
## For effect sizes, use https://jakewestfall.shinyapps.io/crossedpower/
# Participants within condition
# Unstandardized
# 0 for all residuals we did not include in our model
# Power = X

# effect size self_other_condition: 0.554
# effect size ostracism_condition: 0.153
# effect size interaktion: 0.488

# Hypothese 1 verworfen!
# H1. While in both conditions (exclusion and inclusion) the participants attribute the behavior of an out-group-member more to stable causes
# than the behavior of an ingroup member, this difference is greater in the exclusion condition.

### Locus Analyses
# First overall overview
dplyr::group_by(long_neg, ostracism_condition, self_other_condition) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(locus_value, na.rm = TRUE),
                   sd = stats::sd(locus_value, na.rm = TRUE))

# Plot locus
Sum_locus <- rcompanion::groupwiseMean(locus_value ~ ostracism_condition + self_other_condition,
                                       data   = long_neg,
                                       conf   = 0.95,
                                       digits = 3,
                                       traditional = FALSE,
                                       percentile  = TRUE)
pd = ggplot2::position_dodge(.2)
ggplot2::ggplot(Sum_locus, ggplot2::aes(x = ostracism_condition,
                                       y = Mean,
                                       color = self_other_condition)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=Percentile.lower,
                                      ymax=Percentile.upper),
                         width=.2, size=0.7, position=pd) +
  ggplot2::geom_point(shape=15, size=4, position=pd) +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))  +
  ggplot2::geom_point(shape=15, size=4, position=pd) + ggplot2::ylim(1, 5) +
  ggplot2::ylab("Mean locus value (1 = stable; 5 = variable)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))

# Is self_other_condition a good predictor for Locus?
fit_locus_1 <- lme4::lmer (locus_value ~ 1 + (1 | id) + (1 | outcome), data = long_neg)
fit_locus_2 <- lme4::lmer (locus_value ~ self_other_condition_c + (1 | id) + (1 | outcome), data = long_neg)
stats::anova (fit_locus_1, fit_locus_2) # Fit1 better

# Haupteffekt self_other_condition auf Locus
summary(fit_locus_2)

dplyr::group_by(long_neg, self_other_condition) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(locus_value, na.rm = TRUE),
                   sd = stats::sd(locus_value, na.rm = TRUE))
# Mean difference Group Yellow - Group Green

# Is Ostracism a good predictor for Locus?
fit_locus_1 <- lme4::lmer (locus_value ~ 1 + (1 | id) + (1 | outcome), data = long_neg)
fit_locus_3 <- lme4::lmer (locus_value ~ ostracism_condition_c + (1 | id) + (1 | outcome), data = long_neg)
stats::anova (fit_locus_1, fit_locus_3)

# Haupteffekt ostracism_condition auf Locus
summary(fit_locus_3)

dplyr::group_by(long_neg, ostracism_condition) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(locus_value, na.rm = TRUE),
                   sd = stats::sd(locus_value, na.rm = TRUE))
# Mean difference Group Excluded - Group Included

# Interaction model
fit_locus <- lme4::lmer (locus_value ~ self_other_condition_c * ostracism_condition_c + (1 |id) + (1 | outcome), data = long_neg)
summary(fit_locus)

dplyr::group_by(long_neg, ostracism_condition, self_other_condition) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(locus_value, na.rm = TRUE),
                   sd = stats::sd(locus_value, na.rm = TRUE))

# Mean difference Interaction Locus
# (Excluded-Yellow - Excluded-Green) - ((Included-Yellow - Included-Green))
# (2.94 − 2.54) − (2.96 − 2.89) = 0.33

### Try to also calculate the effect sizes fore mixed models
## For effect sizes, use https://jakewestfall.shinyapps.io/crossedpower/
# Participants within condition
# Unstandardized
# 0 for all residuals we did not include in our model
# Power = X

# effect size self_other_condition: 0.31
# effect size ostracism_condition: 0.256
# effect size interaktion: 0.576

# Hypothese 2 verworfen!
# H2. While in both conditions (exclusion and inclusion) the participants attribute the behavior of an out-group-member more to internal causes
# than the behavior of an in-group-member, this difference is greater in the exclusion condition.
