#######################################
####### OST-CONST-04_Analyses1 ########
#######################################

# Load packages
library(ggplot2)
library(magrittr)
library(ggpubr)
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


# How many participants are in each condition? (get overview using e.g. table())


################## Plots Master_ost:
################## mood, ios, excluded, ignored, NT1-4, perception of other player/group
### mood
# Test
# t-test if only comparing two groups (e.g. exclusion vs. inclusion)
stats::t.test(master_ost$mood[master_ost$cyberball == 1],
              master_ost$mood[master_ost$cyberball == 2])
# Anova to compare all four groups (e.g. main effect cyberball, main effect focus, interaction Cyberbal*focus)
summary(stats::aov(mood ~ self_other_condition * ostracism_condition, data = master_ost))

# Effect size (Cohen's d): z.B.  cohensD(mood ~ ostracism_condition, data = master_ost)
lsr::cohensD(mood ~ ostracism_condition, data = master_ost)

# Display mean & sd of groups (this function is very helpful for documenting results afterwards)
dplyr::group_by(master_ost, cyberball, focus) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(mood, na.rm = TRUE),
                   sd = stats::sd(mood, na.rm = TRUE))

# Plot mood
# Important info:
# This plot cannot deal with NAs (need to be removed before, here not necessary)

# Grouping variables (e.g. cyberball & focus) should not be numeric but factor
# Is focus a factor variable?
utils::str(master_ost$focus)
# Is self_other_condition a factor variable?
utils::str(master_ost$self_other_condition)
# Try to exchange focus with self_other_condition and see how the plot changes
# You can also change self_other_condition to a factor variable and see how the plot changes

Sum_mood <- rcompanion::groupwiseMean(mood ~ ostracism_condition + self_other_condition,
                                      data   = master_ost,
                                      conf   = 0.95,
                                      digits = 3,
                                      traditional = FALSE,
                                      percentile  = TRUE)
pd = ggplot2::position_dodge(.2)
ggplot2::ggplot(Sum_mood, ggplot2::aes(x = ostracism_condition,
                                       y = Mean,
                                       color = self_other_condition)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=Percentile.lower,
                                      ymax=Percentile.upper),
                         width=.2, size=0.7, position=pd) +
  ggplot2::geom_point(shape=15, size=4, position=pd) +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))  +
  ggplot2::geom_point(shape=15, size=4, position=pd) + ggplot2::ylim(1, 9) +
  ggplot2::ylab("Mood value (1 = bad; 9 = good)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))

### ios1 (Relationship towards person group Green OR person group Yellow)
# Test
stats::t.test(master_ost$ios_outcome[master_ost$ios_condition == "group_yellow_own"],
              master_ost$ios_outcome[master_ost$ios_condition == "group_green_other"])

# ANOVA
summary(stats::aov(ios_outcome ~ self_other_condition * ostracism_condition, data = master_ost))

# Display mean & sd of groups
dplyr::group_by(master_ost, ios_condition) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(ios_outcome, na.rm = TRUE),
                   sd = stats::sd(ios_outcome, na.rm = TRUE))

# Plot ios1
Sum_ios_outcome <- rcompanion::groupwiseMean(ios_outcome ~ ostracism_condition + self_other_condition,
                                             data   = master_ost,
                                             conf   = 0.95,
                                             digits = 3,
                                             traditional = FALSE,
                                             percentile  = TRUE)
pd = ggplot2::position_dodge(.2)
ggplot2::ggplot(Sum_ios_outcome, ggplot2::aes(x = ostracism_condition,
                                              y = Mean,
                                              color = self_other_condition)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=Percentile.lower,
                                      ymax=Percentile.upper),
                         width=.2, size=0.7, position=pd) +
  ggplot2::geom_point(shape=15, size=4, position=pd) +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))  +
  ggplot2::geom_point(shape=15, size=4, position=pd) + ggplot2::ylim(1, 9) +
  ggplot2::ylab("IOS_outcome value (1 = close; 7 = distant)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))


### ios2 (Relationship self and own group)
# Test
stats::t.test(master_ost$ios_group_ident[master_ost$ios_condition == "group_yellow_own"],
              master_ost$ios_group_ident[master_ost$ios_condition == "group_green_other"])

# ANOVA
summary(stats::aov(ios_group_ident ~ self_other_condition * ostracism_condition, data = master_ost))

# Display mean & sd of groups
dplyr::group_by(master_ost, ios_condition) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(ios_group_ident, na.rm = TRUE),
                   sd = stats::sd(ios_group_ident, na.rm = TRUE))

# Plot ios2
Sum_ios_group_ident <- rcompanion::groupwiseMean(ios_group_ident ~ ostracism_condition + self_other_condition,
                                                 data   = master_ost,
                                                 conf   = 0.95,
                                                 digits = 3,
                                                 traditional = FALSE,
                                                 percentile  = TRUE)
pd = ggplot2::position_dodge(.2)
ggplot2::ggplot(Sum_ios_group_ident, ggplot2::aes(x = ostracism_condition,
                                                  y = Mean,
                                                  color = self_other_condition)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=Percentile.lower,
                                      ymax=Percentile.upper),
                         width=.2, size=0.7, position=pd) +
  ggplot2::geom_point(shape=15, size=4, position=pd) +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))  +
  ggplot2::geom_point(shape=15, size=4, position=pd) + ggplot2::ylim(1, 7) +
  ggplot2::ylab("IOS_group_ident value (1 = close; 7 = distant)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))


### How much do participants identify with their own group (positive experience and like being part)
## Positive experience
# Test
stats::t.test(master_ost$group_ident_experience[master_ost$ios_condition == "group_yellow_own"],
              master_ost$group_ident_experience[master_ost$ios_condition == "group_green_other"])

# ANOVA
summary(stats::aov(group_ident_experience ~ self_other_condition * ostracism_condition, data = master_ost))

# Display mean & sd of groups
dplyr::group_by(master_ost, ios_condition) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(group_ident_experience, na.rm = TRUE),
                   sd = stats::sd(group_ident_experience, na.rm = TRUE))

# Plot positive experience
Sum_group_ident_experience <- rcompanion::groupwiseMean(group_ident_experience ~ ostracism_condition + self_other_condition,
                                                        data   = master_ost,
                                                        conf   = 0.95,
                                                        digits = 3,
                                                        traditional = FALSE,
                                                        percentile  = TRUE)
pd = ggplot2::position_dodge(.2)
ggplot2::ggplot(Sum_group_ident_experience, ggplot2::aes(x = ostracism_condition,
                                                         y = Mean,
                                                         color = self_other_condition)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=Percentile.lower,
                                      ymax=Percentile.upper),
                         width=.2, size=0.7, position=pd) +
  ggplot2::geom_point(shape=15, size=4, position=pd) +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))  +
  ggplot2::geom_point(shape=15, size=4, position=pd) + ggplot2::ylim(1, 9) +
  ggplot2::ylab("Positive_experience value (1 = disagree; 9 = agree)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))


## Like being a member of Group Yellow
# Test
stats::t.test(master_ost$group_ident_like[master_ost$ios_condition == "group_yellow_own"],
              master_ost$group_ident_like[master_ost$ios_condition == "group_green_other"])

# ANOVA
summary(stats::aov(group_ident_like ~ self_other_condition * ostracism_condition, data = master_ost))

# Display mean & sd of groups
dplyr::group_by(master_ost, ios_condition) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(group_ident_like, na.rm = TRUE),
                   sd = stats::sd(group_ident_like, na.rm = TRUE))

# Plot liking
Sum_group_ident_like <- rcompanion::groupwiseMean(group_ident_like ~ ostracism_condition + self_other_condition,
                                                  data   = master_ost,
                                                  conf   = 0.95,
                                                  digits = 3,
                                                  traditional = FALSE,
                                                  percentile  = TRUE)
pd = ggplot2::position_dodge(.2)
ggplot2::ggplot(Sum_group_ident_like, ggplot2::aes(x = ostracism_condition,
                                                   y = Mean,
                                                   color = self_other_condition)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=Percentile.lower,
                                      ymax=Percentile.upper),
                         width=.2, size=0.7, position=pd) +
  ggplot2::geom_point(shape=15, size=4, position=pd) +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))  +
  ggplot2::geom_point(shape=15, size=4, position=pd) + ggplot2::ylim(1, 9) +
  ggplot2::ylab("Like_beeing_member value (1 = disagree; 9 = agree)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))


### Feeling excluded
# Test
stats::t.test(master_ost$excluded[master_ost$cyberball == 1],
              master_ost$excluded[master_ost$cyberball == 2])

# Display mean & sd of groups
dplyr::group_by(master_ost, cyberball) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(excluded, na.rm = TRUE),
                   sd = stats::sd(excluded, na.rm = TRUE))

# Plot excluded
Sum_excluded <- rcompanion::groupwiseMean(excluded ~ ostracism_condition,
                                          data   = master_ost,
                                          conf   = 0.95,
                                          digits = 3,
                                          traditional = FALSE,
                                          percentile  = TRUE)
pd = ggplot2::position_dodge(.2)
ggplot2::ggplot(Sum_excluded, ggplot2::aes(x = ostracism_condition,
                                           y = Mean)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=Percentile.lower,
                                      ymax=Percentile.upper),
                         width=.2, size=0.7, position=pd) +
  ggplot2::geom_point(shape=15, size=4, position=pd) +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))  +
  ggplot2::geom_point(shape=15, size=4, position=pd) + ggplot2::ylim(1, 9) +
  ggplot2::ylab("Exclusion value (1 = disagree; 9 = agree)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))


### Feeling ignored
# Test
stats::t.test(master_ost$ignored[master_ost$cyberball == 1],
              master_ost$ignored[master_ost$cyberball == 2])

# Display mean & sd of groups
dplyr::group_by(master_ost, cyberball) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(ignored, na.rm = TRUE),
                   sd = stats::sd(ignored, na.rm = TRUE))

# Plot excluded
Sum_ignored <- rcompanion::groupwiseMean(ignored ~ ostracism_condition,
                                         data   = master_ost,
                                         conf   = 0.95,
                                         digits = 3,
                                         traditional = FALSE,
                                         percentile  = TRUE)
pd = ggplot2::position_dodge(.2)
ggplot2::ggplot(Sum_ignored, ggplot2::aes(x = ostracism_condition,
                                          y = Mean)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=Percentile.lower,
                                      ymax=Percentile.upper),
                         width=.2, size=0.7, position=pd) +
  ggplot2::geom_point(shape=15, size=4, position=pd) +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))  +
  ggplot2::geom_point(shape=15, size=4, position=pd) + ggplot2::ylim(1, 9) +
  ggplot2::ylab("Ignoration value (1 = disagree; 9 = agree)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))

### Received tosses
# Test
stats::t.test(master_ost$received_tosses[master_ost$cyberball == 1],
              master_ost$received_tosses[master_ost$cyberball == 2])

# ANOVA
summary(stats::aov(received_tosses ~ self_other_condition * ostracism_condition, data = master_ost))

# Display mean & sd of groups
dplyr::group_by(master_ost, cyberball) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(received_tosses, na.rm = TRUE),
                   sd = stats::sd(received_tosses, na.rm = TRUE))

# Plot received tosses
Sum_received_tosses <- rcompanion::groupwiseMean(received_tosses ~ ostracism_condition,
                                                 data   = master_ost,
                                                 conf   = 0.95,
                                                 digits = 3,
                                                 traditional = FALSE,
                                                 percentile  = TRUE)
pd = ggplot2::position_dodge(.2)
ggplot2::ggplot(Sum_received_tosses, ggplot2::aes(x = ostracism_condition,
                                                  y = Mean)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=Percentile.lower,
                                      ymax=Percentile.upper),
                         width=.2, size=0.7, position=pd) +
  ggplot2::geom_point(shape=15, size=4, position=pd) +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))  +
  ggplot2::geom_point(shape=15, size=4, position=pd) + ggplot2::ylim(1, 60) +
  ggplot2::ylab("Received_tosses value (1 = few; 60 = many)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))

### Need threat 1-4
## NT1
# Test
stats::t.test(master_ost$nt1_accepted[master_ost$cyberball == 1],
              master_ost$nt1_accepted[master_ost$cyberball == 2])

# Display mean & sd of groups
dplyr::group_by(master_ost, cyberball) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(nt1_accepted, na.rm = TRUE),
                   sd = stats::sd(nt1_accepted, na.rm = TRUE))

# Plot NT1
Sum_nt1_accepted <- rcompanion::groupwiseMean(nt1_accepted ~ ostracism_condition,
                                              data   = master_ost,
                                              conf   = 0.95,
                                              digits = 3,
                                              traditional = FALSE,
                                              percentile  = TRUE)
pd = ggplot2::position_dodge(.2)
ggplot2::ggplot(Sum_nt1_accepted, ggplot2::aes(x = ostracism_condition,
                                               y = Mean)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=Percentile.lower,
                                      ymax=Percentile.upper),
                         width=.2, size=0.7, position=pd) +
  ggplot2::geom_point(shape=15, size=4, position=pd) +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))  +
  ggplot2::geom_point(shape=15, size=4, position=pd) + ggplot2::ylim(1, 9) +
  ggplot2::ylab("NT1 value (1 = low; 9 = high)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))


## NT2
# Test
stats::t.test(master_ost$nt2_valued[master_ost$cyberball == 1],
              master_ost$nt2_valued[master_ost$cyberball == 2])

# Display mean & sd of groups
dplyr::group_by(master_ost, cyberball) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(nt2_valued, na.rm = TRUE),
                   sd = stats::sd(nt2_valued, na.rm = TRUE))

# Plot NT2
Sum_nt2_valued <- rcompanion::groupwiseMean(nt2_valued ~ ostracism_condition,
                                            data   = master_ost,
                                            conf   = 0.95,
                                            digits = 3,
                                            traditional = FALSE,
                                            percentile  = TRUE)
pd = ggplot2::position_dodge(.2)
ggplot2::ggplot(Sum_nt2_valued, ggplot2::aes(x = ostracism_condition,
                                             y = Mean)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=Percentile.lower,
                                      ymax=Percentile.upper),
                         width=.2, size=0.7, position=pd) +
  ggplot2::geom_point(shape=15, size=4, position=pd) +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))  +
  ggplot2::geom_point(shape=15, size=4, position=pd) + ggplot2::ylim(1, 9) +
  ggplot2::ylab("NT2 value (1 = low; 9 = high)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))


## NT3
# Test
stats::t.test(master_ost$nt3_powerfull[master_ost$cyberball == 1],
              master_ost$nt3_powerfull[master_ost$cyberball == 2])

# Display mean & sd of groups
dplyr::group_by(master_ost, cyberball) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(nt3_powerfull, na.rm = TRUE),
                   sd = stats::sd(nt3_powerfull, na.rm = TRUE))

# Plot NT3
Sum_nt3_powerfull <- rcompanion::groupwiseMean(nt3_powerfull ~ ostracism_condition,
                                               data   = master_ost,
                                               conf   = 0.95,
                                               digits = 3,
                                               traditional = FALSE,
                                               percentile  = TRUE)
pd = ggplot2::position_dodge(.2)
ggplot2::ggplot(Sum_nt3_powerfull, ggplot2::aes(x = ostracism_condition,
                                                y = Mean)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=Percentile.lower,
                                      ymax=Percentile.upper),
                         width=.2, size=0.7, position=pd) +
  ggplot2::geom_point(shape=15, size=4, position=pd) +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))  +
  ggplot2::geom_point(shape=15, size=4, position=pd) + ggplot2::ylim(1, 9) +
  ggplot2::ylab("NT3 value (1 = low; 9 = high)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))

## N4
# Test
stats::t.test(master_ost$nt4_recognized[master_ost$cyberball == 1],
              master_ost$nt4_recognized[master_ost$cyberball == 2])

# Display mean & sd of groups
dplyr::group_by(master_ost, cyberball) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(nt4_recognized, na.rm = TRUE),
                   sd = stats::sd(nt4_recognized, na.rm = TRUE))

# Plot NT4
Sum_nt4_recognized <- rcompanion::groupwiseMean(nt4_recognized ~ ostracism_condition,
                                                data   = master_ost,
                                                conf   = 0.95,
                                                digits = 3,
                                                traditional = FALSE,
                                                percentile  = TRUE)
pd = ggplot2::position_dodge(.2)
ggplot2::ggplot(Sum_nt4_recognized, ggplot2::aes(x = ostracism_condition,
                                                 y = Mean)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=Percentile.lower,
                                      ymax=Percentile.upper),
                         width=.2, size=0.7, position=pd) +
  ggplot2::geom_point(shape=15, size=4, position=pd) +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))  +
  ggplot2::geom_point(shape=15, size=4, position=pd) + ggplot2::ylim(1, 9) +
  ggplot2::ylab("NT4 value (1 = low; 9 = high)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))



### How is Player 3 from Group Green perceived?
## Gender P3
# Test
stats::t.test(master_ost$gender_green[master_ost$cyberball == 1],
              master_ost$gender_green[master_ost$cyberball == 2])

# Display mean & sd of groups
dplyr::group_by(master_ost, cyberball) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(gender_green, na.rm = TRUE),
                   sd = stats::sd(gender_green, na.rm = TRUE))

# Plot gender P3
Sum_gender_green <- rcompanion::groupwiseMean(gender_green ~ ostracism_condition,
                                              data   = master_ost,
                                              conf   = 0.95,
                                              digits = 3,
                                              traditional = FALSE,
                                              percentile  = TRUE)
pd = ggplot2::position_dodge(.2)
ggplot2::ggplot(Sum_gender_green, ggplot2::aes(x = ostracism_condition,
                                               y = Mean)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=Percentile.lower,
                                      ymax=Percentile.upper),
                         width=.2, size=0.7, position=pd) +
  ggplot2::geom_point(shape=15, size=4, position=pd) +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))  +
  ggplot2::geom_point(shape=15, size=4, position=pd) + ggplot2::ylim(1, 3) +
  ggplot2::ylab("Gender_green value (1 = male; 2 = female)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))

## Age P3
# Test
stats::t.test(master_ost$age_green[master_ost$cyberball == 1],
              master_ost$age_green[master_ost$cyberball == 2])

# Display mean & sd of groups
dplyr::group_by(master_ost, cyberball) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(age_green, na.rm = TRUE),
                   sd = stats::sd(age_green, na.rm = TRUE))

# Plot age P3
Sum_age_green <- rcompanion::groupwiseMean(age_green ~ ostracism_condition,
                                           data   = master_ost,
                                           conf   = 0.95,
                                           digits = 3,
                                           traditional = FALSE,
                                           percentile  = TRUE)
pd = ggplot2::position_dodge(.2)
ggplot2::ggplot(Sum_age_green, ggplot2::aes(x = ostracism_condition,
                                            y = Mean)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=Percentile.lower,
                                      ymax=Percentile.upper),
                         width=.2, size=0.7, position=pd) +
  ggplot2::geom_point(shape=15, size=4, position=pd) +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))  +
  ggplot2::geom_point(shape=15, size=4, position=pd) + ggplot2::ylim(1, 7) +
  ggplot2::ylab("Age_green value (1 = <18; 7 = >65)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))


## P3 Similarity to others from Group Green
# Test
stats::t.test(master_ost$similar_green[master_ost$cyberball == 1],
              master_ost$similar_green[master_ost$cyberball == 2])

# Display mean & sd of groups
dplyr::group_by(master_ost, cyberball) %>%
  dplyr::summarise(count = dplyr::n(),
                   mean = mean(similar_green, na.rm = TRUE),
                   sd = stats::sd(similar_green, na.rm = TRUE))

# Plot similarity
Sum_similar_green <- rcompanion::groupwiseMean(similar_green ~ ostracism_condition,
                                               data   = master_ost,
                                               conf   = 0.95,
                                               digits = 3,
                                               traditional = FALSE,
                                               percentile  = TRUE)
pd = ggplot2::position_dodge(.2)
ggplot2::ggplot(Sum_similar_green, ggplot2::aes(x = ostracism_condition,
                                                y = Mean)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=Percentile.lower,
                                      ymax=Percentile.upper),
                         width=.2, size=0.7, position=pd) +
  ggplot2::geom_point(shape=15, size=4, position=pd) +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))  +
  ggplot2::geom_point(shape=15, size=4, position=pd) + ggplot2::ylim(1, 9) +
  ggplot2::ylab("Similarity_green value (1 = low; 9 = high)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))
