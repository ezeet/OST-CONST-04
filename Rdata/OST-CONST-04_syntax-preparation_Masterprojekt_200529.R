#########################################
####### OST-CONST-04_Syntax prep ########
#########################################


# Load packages
library(ggplot2)
library(ggpubr)
library(Matrix)
library(languageR)
library(lsr)
library(reshape2)
library(rcompanion)
library(magrittr)

# Empty working environment
rm(list = ls(all = TRUE))

# Set working directory
setwd("C:/Users/Robin/Documents/Git/ost-const-04/Rdata")

# Load raw data 1 (exchange name of data file to what you saved on your laptop)
raw_ost <- utils::read.csv("OST-CONST-04_raw-data_200526.csv", header = TRUE, sep = ";")

master_ost <-
  raw_ost %>%
  # Rename variables c_0001 and c_0002
  dplyr::rename(cyberball = c_0001,
                focus = c_0002) %>%
  # Create new variable ostracism_condition by copying c_0001 and changing 1 == included, 2 == excluded
  dplyr::mutate(ostracism_condition = dplyr::case_when(raw_ost$c_0001 == 1 ~ "included",
                                                       raw_ost$c_0001 == 2 ~ "excluded")) %>%
  # Create new variable ostracism_condition by copying c_0002 and changing 1 == yellow, 2 == green
  dplyr::mutate(self_other_condition = dplyr::case_when(raw_ost$c_0002 == 1 ~ "yellow",
                                                        raw_ost$c_0002 == 2 ~ "green")) %>%
  # choose only at participants who are ok to use data (Variable use_data == 2)
  dplyr::filter(use_data == 2) %>%
  # Drop columns that are not needed
  dplyr::select(-c(1:6, 12, 32:33, 138:193)) %>%
  # Exclusion criteria (seriousness); remove two participants who chose 4 or less
  dplyr::filter(seriousness > 4) %>%
  # Master_ost: move new variables (cyberball, focus, ostracism_condition, self_other_condition) to column nr. 1, 2, 3, 4
  dplyr::relocate(cyberball, focus, ostracism_condition, self_other_condition) %>%
  # Remove two participants who indicated 15% in the inclusion condition
  # and remove two participant who indicated 20% in the exclusion condition
  dplyr::filter( !((cyberball == 1 & received_tosses <= 15) | (cyberball == 2 & received_tosses >= 20)) ) %>%
  # Add `id` variable
  tibble::rowid_to_column(var = "id") %>%
  # I usually assign all "missing" values (in this case -77 because participants were only presented with one of two options) with NA
  dplyr::mutate(dplyr::across(c(ios_yellow, ios_green), ~dplyr::na_if(.x, -77))) %>%
  # Create master_ost file in wide format (all IOS scales that ask the same question (but either Group Yellow or Group Green) in one variable)
  # Then the variables can be combined using the function "gather()"
  # Bsp. gather: data_2 <- gather (data1, variable_name, variable_value, v1:v5, factor_key = T, na.rm = T)
  # --> This will summarize v1-v5 into one variable (column) with the name variable_value, variable_name will have the names of the previous variables (v1-v5)
  # also written in one column
  tidyr::gather(key = ios_condition,
                value = ios_outcome,
                ios_yellow, ios_green,
                factor_key = TRUE,
                na.rm = TRUE) %>%
  # ios_condition (1 == "Group_Yellow(Own)"; 2 == "Group_Green(Other")
  dplyr::mutate(ios_condition = dplyr::if_else(condition = ios_condition == "ios_yellow",
                                               true = "group_yellow_own",
                                               false = "group_green_other")) %>%
  # Optional: Again, rearrange the columns so that the new IOS variables are back where they were in the beginning
  dplyr::relocate(ios_condition, ios_outcome,
                  .before = ios_group_ident) %>%
  # Order the dataframe by id (lowest to highest) using "dplyr::arrange()"
  dplyr::arrange(id)

# convert dataframe to tibble
master_ost %<>% tibble::as_tibble()


####################################### Long Format #######################################


# Separate Datafile for yellow and green condition (no NA)
self_cond <-
  master_ost %>%
  dplyr::filter(self_other_condition == "yellow")

# select only a subset of all vars
self_cond_stability <-
  self_cond %>%
  dplyr::select("id",
                "cyberball",
                "focus",
                "ostracism_condition",
                "self_other_condition",
                "ios_condition",
                "ios_outcome",
                matches("^av_stability\\d_own"))

# Create one stability mean per outcome
self_cond_stability$stability_self1 <- (self_cond_stability$av_stability1_own_stabil_1 + self_cond_stability$av_stability2_own_time_1)/2
self_cond_stability$stability_self2 <- (self_cond_stability$av_stability1_own_stabil_2 + self_cond_stability$av_stability2_own_time_2)/2
self_cond_stability$stability_self3 <- (self_cond_stability$av_stability1_own_stabil_3 + self_cond_stability$av_stability2_own_time_3)/2
self_cond_stability$stability_self4 <- (self_cond_stability$av_stability1_own_stabil_4 + self_cond_stability$av_stability2_own_time_4)/2
self_cond_stability$stability_self5 <- (self_cond_stability$av_stability1_own_stabil_5 + self_cond_stability$av_stability2_own_time_5)/2
self_cond_stability$stability_self6 <- (self_cond_stability$av_stability1_own_stabil_6 + self_cond_stability$av_stability2_own_time_6)/2
self_cond_stability$stability_self7 <- (self_cond_stability$av_stability1_own_stabil_7 + self_cond_stability$av_stability2_own_time_7)/2
self_cond_stability$stability_self8 <- (self_cond_stability$av_stability1_own_stabil_8 + self_cond_stability$av_stability2_own_time_8)/2
self_cond_stability$stability_self9 <- (self_cond_stability$av_stability1_own_stabil_9 + self_cond_stability$av_stability2_own_time_9)/2
self_cond_stability$stability_self10 <- (self_cond_stability$av_stability1_own_stabil_10 + self_cond_stability$av_stability2_own_time_10)/2

#Drop columns that are not needed
self_cond_stability %<>% dplyr::select(-matches("^av_stability\\d_own"))

# Same for locus
self_cond_locus <-
  self_cond %>%
  dplyr::select("id",
                "cyberball",
                "focus",
                "ostracism_condition",
                "self_other_condition",
                "ios_condition",
                "ios_outcome",
                matches("^av_locus\\d_own"))

#Create one stability mean per outcome
self_cond_locus$locus_self1 <- (self_cond_locus$av_locus1_own_situation_1 + self_cond_locus$av_locus2_own_internal_1)/2
self_cond_locus$locus_self2 <- (self_cond_locus$av_locus1_own_situation_2 + self_cond_locus$av_locus2_own_internal_2)/2
self_cond_locus$locus_self3 <- (self_cond_locus$av_locus1_own_situation_3 + self_cond_locus$av_locus2_own_internal_3)/2
self_cond_locus$locus_self4 <- (self_cond_locus$av_locus1_own_situation_4 + self_cond_locus$av_locus2_own_internal_4)/2
self_cond_locus$locus_self5 <- (self_cond_locus$av_locus1_own_situation_5 + self_cond_locus$av_locus2_own_internal_5)/2
self_cond_locus$locus_self6 <- (self_cond_locus$av_locus1_own_situation_6 + self_cond_locus$av_locus2_own_internal_6)/2
self_cond_locus$locus_self7 <- (self_cond_locus$av_locus1_own_situation_7 + self_cond_locus$av_locus2_own_internal_7)/2
self_cond_locus$locus_self8 <- (self_cond_locus$av_locus1_own_situation_8 + self_cond_locus$av_locus2_own_internal_8)/2
self_cond_locus$locus_self9 <- (self_cond_locus$av_locus1_own_situation_9 + self_cond_locus$av_locus2_own_internal_9)/2
self_cond_locus$locus_self10 <- (self_cond_locus$av_locus1_own_situation_10 + self_cond_locus$av_locus2_own_internal_10)/2

#Drop columns that are not needed
self_cond_locus %<>% dplyr::select(-matches("^av_locus\\d_own"))

# Create long format (for both stability & locus) and order by id
long_self_cond_stability <- tidyr::gather(self_cond_stability,
                                          key = stability,
                                          value = stability_value,
                                          stability_self1:stability_self10,
                                          factor_key = TRUE,
                                          na.rm = TRUE) %>%
  dplyr::arrange(id)

long_self_cond_locus <- tidyr::gather(self_cond_locus,
                                      key = locus,
                                      value = locus_value,
                                      locus_self1:locus_self10,
                                      factor_key = TRUE,
                                      na.rm = TRUE) %>%
  dplyr::arrange(id)

# Create combined dataframe
long_self <- tibble::tibble(id = long_self_cond_stability$id,
                            cyberball = long_self_cond_stability$cyberball,
                            focus = long_self_cond_stability$focus,
                            ostracism_condition = long_self_cond_stability$ostracism_condition,
                            self_other_condition = long_self_cond_stability$self_other_condition,
                            ios_condition = long_self_cond_stability$ios_condition,
                            ios_outcome = long_self_cond_stability$ios_outcome,
                            stability = long_self_cond_stability$stability,
                            stability_value = long_self_cond_stability$stability_value,
                            locus = long_self_cond_locus$locus,
                            locus_value = long_self_cond_locus$locus_value)
long_self %<>% dplyr::mutate(outcome = rep(1:10, times = nrow(long_self)/10))

####### Same for other condition #####

# Create one dataframe for only id, conditions and other_stability values,
other_cond <-
  master_ost %>%
  dplyr::filter(self_other_condition == "green")

other_cond_stability <-
  other_cond %>%
  dplyr::select("id",
                "cyberball",
                "focus",
                "ostracism_condition",
                "self_other_condition",
                "ios_condition",
                "ios_outcome",
                matches("^av_stability\\d_other"))

#Create one stability mean per outcome (stability_other_1 - stability_other_10)
other_cond_stability$stability_other1 <- (other_cond_stability$av_stability1_other_stabil_1 + other_cond_stability$av_stability2_other_time_1)/2
other_cond_stability$stability_other2 <- (other_cond_stability$av_stability1_other_stabil_2 + other_cond_stability$av_stability2_other_time_2)/2
other_cond_stability$stability_other3 <- (other_cond_stability$av_stability1_other_stabil_3 + other_cond_stability$av_stability2_other_time_3)/2
other_cond_stability$stability_other4 <- (other_cond_stability$av_stability1_other_stabil_4 + other_cond_stability$av_stability2_other_time_4)/2
other_cond_stability$stability_other5 <- (other_cond_stability$av_stability1_other_stabil_5 + other_cond_stability$av_stability2_other_time_5)/2
other_cond_stability$stability_other6 <- (other_cond_stability$av_stability1_other_stabil_6 + other_cond_stability$av_stability2_other_time_6)/2
other_cond_stability$stability_other7 <- (other_cond_stability$av_stability1_other_stabil_7 + other_cond_stability$av_stability2_other_time_7)/2
other_cond_stability$stability_other8 <- (other_cond_stability$av_stability1_other_stabil_8 + other_cond_stability$av_stability2_other_time_8)/2
other_cond_stability$stability_other9 <- (other_cond_stability$av_stability1_other_stabil_9 + other_cond_stability$av_stability2_other_time_9)/2
other_cond_stability$stability_other10 <- (other_cond_stability$av_stability1_other_stabil_10 + other_cond_stability$av_stability2_other_time_10)/2

#Drop columns that are not needed
other_cond_stability %<>% dplyr::select(-matches("^av_stability\\d_other"))

### Repeat for locus

# Create one dataframe for only id, conditions and other_locus values,
other_cond_locus <-
  other_cond %>%
  dplyr::select("id",
                "cyberball",
                "focus",
                "ostracism_condition",
                "self_other_condition",
                "ios_condition",
                "ios_outcome",
                matches("^av_locus\\d_other"))

#Create one stability mean per outcome (locus_other1 - locus_other_10)
other_cond_locus$locus_other1 <- (other_cond_locus$av_locus1_other_situation_1 + other_cond_locus$av_locus2_other_internal_1)/2
other_cond_locus$locus_other2 <- (other_cond_locus$av_locus1_other_situation_2 + other_cond_locus$av_locus2_other_internal_2)/2
other_cond_locus$locus_other3 <- (other_cond_locus$av_locus1_other_situation_3 + other_cond_locus$av_locus2_other_internal_3)/2
other_cond_locus$locus_other4 <- (other_cond_locus$av_locus1_other_situation_4 + other_cond_locus$av_locus2_other_internal_4)/2
other_cond_locus$locus_other5 <- (other_cond_locus$av_locus1_other_situation_5 + other_cond_locus$av_locus2_other_internal_5)/2
other_cond_locus$locus_other6 <- (other_cond_locus$av_locus1_other_situation_6 + other_cond_locus$av_locus2_other_internal_6)/2
other_cond_locus$locus_other7 <- (other_cond_locus$av_locus1_other_situation_7 + other_cond_locus$av_locus2_other_internal_7)/2
other_cond_locus$locus_other8 <- (other_cond_locus$av_locus1_other_situation_8 + other_cond_locus$av_locus2_other_internal_8)/2
other_cond_locus$locus_other9 <- (other_cond_locus$av_locus1_other_situation_9 + other_cond_locus$av_locus2_other_internal_9)/2
other_cond_locus$locus_other10 <- (other_cond_locus$av_locus1_other_situation_10 + other_cond_locus$av_locus2_other_internal_10)/2

#Drop columns that are not needed
other_cond_locus %<>% dplyr::select(-matches("^av_locus\\d_other"))

## Create long format (first for stability and locus individually) and order by id
long_other_cond_stability <- tidyr::gather(other_cond_stability,
                                           key = stability,
                                           value = stability_value,
                                           stability_other1:stability_other10,
                                           factor_key = TRUE,
                                           na.rm = TRUE) %>%
  dplyr::arrange(id)

long_other_cond_locus <- tidyr::gather(other_cond_locus,
                                       key = locus,
                                       value = locus_value,
                                       locus_other1:locus_other10,
                                       factor_key = TRUE,
                                       na.rm = TRUE) %>%
  dplyr::arrange(id)


# Create combined dataframe
long_other <- tibble::tibble(id = long_other_cond_stability$id,
                             cyberball = long_other_cond_stability$cyberball,
                             focus = long_other_cond_stability$focus,
                             ostracism_condition = long_other_cond_stability$ostracism_condition,
                             self_other_condition = long_other_cond_stability$self_other_condition,
                             ios_condition = long_other_cond_stability$ios_condition,
                             ios_outcome = long_other_cond_stability$ios_outcome,
                             stability = long_other_cond_stability$stability,
                             stability_value = long_other_cond_stability$stability_value,
                             locus = long_other_cond_locus$locus,
                             locus_value = long_other_cond_locus$locus_value)

# Add outcome Variable 1-10 (use "rep()")
long_other %<>% dplyr::mutate(outcome = rep(1:10, times = nrow(long_other)/10))


##### Create one combined long format dataframe called long_data ####
# use "merge() and merge long_self and long_other dataframes
long_data <- dplyr::full_join(long_self,
                              long_other,
                              by = c("id",
                                     "cyberball",
                                     "focus",
                                     "ostracism_condition",
                                     "self_other_condition",
                                     "ios_condition",
                                     "ios_outcome",
                                     "stability",
                                     "stability_value",
                                     "locus",
                                     "locus_value",
                                     "outcome"))


######### Add variables for plot visibility ######

### Include causes for different outcomes to long format
cause_self <-
  master_ost %>%
  dplyr::select(c("id",
                  "cyberball",
                  "focus",
                  "ostracism_condition",
                  "self_other_condition",
                  "ios_condition",
                  "ios_outcome",
                  matches("^cause_own_\\d")))

# Repeat for causes other
cause_other <-
  master_ost %>%
  dplyr::select(c("id",
                  "cyberball",
                  "focus",
                  "ostracism_condition",
                  "self_other_condition",
                  "ios_condition",
                  "ios_outcome",
                  matches("^cause_other_\\d")))

# Filter condition yellow for cause_self
cause_self %<>% dplyr::filter(self_other_condition == "yellow")

# Filter condition green for cause_other
cause_other %<>% dplyr::filter(self_other_condition == "green")

# Switch causes into long format using "gather" and order by id
long_cause_self <- tidyr::gather(cause_self,
                                 key = outcome,
                                 value = cause_self,
                                 cause_own_1:cause_own_10,
                                 factor_key = TRUE) %>%
  dplyr::arrange(id)

# Repeat for "long_cause_other"
long_cause_other <- tidyr::gather(cause_other,
                                  key = outcome,
                                  value = cause_other,
                                  cause_other_1:cause_other_10,
                                  factor_key = TRUE) %>%
  dplyr::arrange(id)

# Create combined long_causes dataframe
long_causes <- dplyr::full_join(long_cause_self,
                                long_cause_other,
                                by = c("id",
                                       "cyberball",
                                       "focus",
                                       "ostracism_condition",
                                       "self_other_condition",
                                       "ios_condition",
                                       "ios_outcome",
                                       "outcome"))

# Create variable for outcomes 1-10 using "rep()"
long_causes %<>% dplyr::mutate(outcome = rep(1:10, times = nrow(long_causes)/10))

# Now combine long_causes with long_data into long_data_all
long_data_all <- dplyr::full_join(long_data,
                                  long_causes,
                                  by = c("id",
                                         "cyberball",
                                         "focus",
                                         "ostracism_condition",
                                         "self_other_condition",
                                         "ios_condition",
                                         "ios_outcome",
                                         "outcome"))


# Long_data_all: Rearange: id, cyberball, focus, cyberball, Focus, outcome, cause_self, cause_other, stability_value,
# locus_value, remaining variables
long_data_all %<>%
  dplyr::relocate(c(id, cyberball, focus, ostracism_condition, self_other_condition, ios_condition, ios_outcome, outcome, cause_self, cause_other, stability_value, locus_value)) %>%
  # sort by id and outcome
  dplyr::arrange(id, outcome)


############ Last step: Look at all negative outcomes and remove the ones that are not reasonable. ###########
############ e.g. description of feelings, "don't know" answers, other answers that are not causes for the outcomes

# Filter for negative outcomes (outcomes 1-5)
long_neg <-
  long_data_all %>%
  dplyr::filter(outcome %in% c(1:5))

# Outcome 1
# First filter for outcome 1
o1 <-
  long_neg %>%
  dplyr::filter(outcome == 1) %>%
  # Assign "NA" to all stability_values of all IDs we want to exclude
  # Assign "NA" to all locus_values of all IDs we want to exclude
  dplyr::mutate(dplyr::across(c(stability_value, locus_value), ~replace(x = .x,
                                                                        list = id %in% c(2, 35, 37, 39, 45, 77, 78, 81, 83, 87, 88, 91, 94, 97, 105, 120, 122, 128),
                                                                        values = NA)))

# Repeat for outcome 2:5
# Outcome 2
o2 <-
  long_neg %>%
  dplyr::filter(outcome == 2) %>%
  dplyr::mutate(dplyr::across(c(stability_value, locus_value), ~replace(x = .x,
                                                                        list = id %in% c(2, 35, 40, 45, 54, 70, 77, 115, 128),
                                                                        values = NA)))

# Outcome 3
o3 <-
  long_neg %>%
  dplyr::filter(outcome == 3) %>%
  dplyr::mutate(dplyr::across(c(stability_value, locus_value), ~replace(x = .x,
                                                                        list = id %in% c(30, 35, 62, 70, 81, 96, 115, 127, 128),
                                                                        values = NA)))

# Outcome 4
o4 <-
  long_neg %>%
  dplyr::filter(outcome == 4) %>%
  dplyr::mutate(dplyr::across(c(stability_value, locus_value), ~replace(x = .x,
                                                                        list = id %in% c(35, 48, 54, 81, 83, 88, 92, 94, 104, 108, 117, 128),
                                                                        values = NA)))

# Outcome 5
o5 <-
  long_neg %>%
  dplyr::filter(outcome == 5) %>%
  dplyr::mutate(dplyr::across(c(stability_value, locus_value), ~replace(x = .x,
                                                                        list = id %in% c(35, 67, 70, 77, 88, 102, 105),
                                                                        values = NA)))


# Only positive Outcomes
long_pos <-
  long_data_all %>%
  dplyr::filter(outcome %in% c(6:10))

# Merge together again (long_pos, o1, o2, o3, o4, o5) and save into object long_all
long_all <- dplyr::bind_rows(list(o1, o2, o3, o4, o5, long_pos)) %>%
  dplyr::arrange(id)


# Remove all NAs which we selected before (we could use either stability_value or locus_value, the result would be the same)
long_data_clean <-
  long_all %>%
  dplyr::filter(!is.na(stability_value))

# Stricter rules for exclusion. If NA in stability_value then exclusion of participant.
long_data_clean_strict <-
  long_all %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(is_unstable = any(is.na(stability_value))) %>%
  dplyr::right_join(y = long_all,
                    by = "id") %>%
  # dplyr::mutate(is_unstable = any(is.na(stability_value))) %>%
  dplyr::filter(!is_unstable) %>%
  dplyr::select(-is_unstable)

# Save master file and long file (change name to what you like to call it, I recommend adding a date in the end)
utils::write.csv(long_data_clean, "OST-CONST-04_long_data_clean_200706.csv", row.names = FALSE)
utils::write.csv(long_data_clean_strict, "OST-CONST-04_long_data_clean_strict_200706")
utils::write.csv(master_ost, "OST-CONST-04_master_200706.csv", row.names = FALSE)
