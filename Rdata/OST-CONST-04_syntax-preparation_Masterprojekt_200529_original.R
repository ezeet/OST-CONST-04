#########################################
####### OST-CONST-04_Syntax prep ########
#########################################

# Load packages
library(ggplot2)
library(magrittr)
library("ggpubr")
library(Matrix)
library(languageR)
library(dplyr)
library(lsr)
library(tidyr)
library(reshape2)
library(rcompanion)

# Empty working environment
rm(list=ls())

# Sets working directory to folder where R-script is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Load raw data 1 (exchange name of data file to what you saved on your laptop)
raw_ost <- read.csv("OST-CONST-04_raw-data_200526.csv", header = T, sep=";")

# Load raw data 2 (alternative)
raw_ost <- read.csv(file.choose(), sep = ",", header = TRUE)

#Look only at participants who are ok to use data (Variable use_data == 2)
table(raw_ost$use_data)
ost_complete <- raw_ost %>%
  filter (use_data %in% c(2))
View(ost_complete)

#Drop columns that are not needed (Columns 1, 8:11, 13:137) and assign to a new dataframe (z.B. master_ost)


# Exclusion criteria (seriousness); remove two participants who chose 4 or less



# Check if participants gave answers according to their condition in the received_tosses variable.
# Remove two participants who indicated 15% in the inclusion condition
# Remove two participant who indicated 20% in the exclusion condition


# Add ID variable


# Rearrange columns (Typically I put the ID variable first and let go of the lfdn Variable. Meaning -> Variable Order: 231, 2:130)


# Create master_ost file in wide format (all IOS scales that ask the same question (but either Group Yellow or Group Green) in one variable)
# I usually assign all "missing" values (in this case -77 because participants were only presented with one of two options) with NA
# Then the variables can be combined using the function "gather()"


# Bsp. gather: data_2 <- gather (data1, variable_name, variable_value, v1:v5, factor_key = T, na.rm = T)
# --> This will summarize v1-v5 into one variable (column) with the name variable_value, variable_name will have the names of the previous variables (v1-v5) also written in one column


#Optional: Again, rearrange the collumns so that the new IOS variables are back where they were in the beginning (Variable order: 1:13, 129, 130, 14:128)

# Order the dataframe by ID (lowest to highest) using "order()"


###########################################################################################
####################################### Long Format #######################################
########################################## Self ###########################################
# Separate Datafile for self and other condition (no NA)
self_cond <- master_ost %>%
  filter (c_0002 %in% 1)
names(self_cond)
self_cond_stability <-  self_cond [ , c (1:3,61:80)]

#Create one stability mean per outcome
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
self_cond_stability <- self_cond_stability[ , -c (4:23)]

# Same for locus
self_cond_locus <-  self_cond [ , c (1:3, 41:60)]

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
self_cond_locus <- self_cond_locus[ , - c (4:23)]

# Create long format (for both stability & locus) and order by ID
long_self_cond_stability <- gather (self_cond_stability, stability, stability_value, stability_self1:stability_self10, factor_key = T, na.rm = T)
long_self_cond_stability <- long_self_cond_stability [order (long_self_cond_stability$ID), ]

long_self_cond_locus <- gather (self_cond_locus, locus, locus_value, locus_self1:locus_self10, factor_key = T, na.rm = T)
long_self_cond_locus <- long_self_cond_locus [order (long_self_cond_locus$ID), ]

# Create combined dataframe
long_self <- data.frame (long_self_cond_stability$ID, long_self_cond_stability$c_0001, long_self_cond_stability$c_0002, long_self_cond_stability$stability, long_self_cond_stability$stability_value, long_self_cond_locus$locus, long_self_cond_locus$locus_value)
names (long_self) <- c ("ID", "c_0001", "c_0002", "stability", "stability_value", "locus", "locus_value")
long_self$outcome <- as.factor(rep(1:10, times = 64))

####### Same for other condition #####
other_cond <- master_ost %>%
  filter (c_0002 %in% 2)
names(other_cond)

# Create one dataframe for only ID, conditions and other_stability values,


#Create one stability mean per outcome (stability_other_1 - stability_other_10)

#Drop columns that are not needed


### Repeat for locus
# Create one dataframe for only ID, conditions and other_locus values,


#Create one stability mean per outcome (locus_other1 - locus_other_10)

#Drop columns that are not needed


## Create long format (first for stability and locus individually) and order by ID


# Create combined dataframe
long_other <- data.frame (long_other_cond_stability$ID, long_other_cond_stability$c_0001, long_other_cond_stability$c_0002, long_other_cond_stability$variable, long_other_cond_stability$value, long_other_cond_locus$variable, long_other_cond_locus$value)
names (long_other) <- c ("ID", "c_0001", "c_0002", "stability", "stability_value", "locus", "locus_value")
# Add outcome Variable 1-10 (use "rep()")


##### Create one combined long format dataframe called long_data ####
# use "merge() and merge long_self and long_other dataframes


######### Add variables for plot visibility ######
# Cyberball condition
master_ost$Cyberball[master_ost$c_0001 == 2] <- c("Excluded")
master_ost$Cyberball[master_ost$c_0001 == 1] <- c("Included")

# Focus condition (1 == "Group_Yellow(Own)"; 2 == "Group_Green(Other")


### Include causes for different outcomes to long format
cause_self <- master_ost[ , c(1:30, 131:132, 31:40)]
# Repeat for causes other


# Filter condition self for cause_self
cause_self<- cause_self %>%
  filter (c_0002 %in% 1)
# Filter condition other for cause_other


# Switch causes into long format using "gather" and order by ID
long_cause_self <- gather (cause_self, outcome, cause_self, cause_own_1:cause_own_10, factor_key = T)
long_cause_self <- long_cause_self [order (long_cause_self$ID), ]

# Repeat for "long_cause_other"


# Create combined long_causes dataframe


# Create variable for outcomes 1-10 using "rep()"


# Now combine long_causes with long_data into long_data_all


# Master_ost: Create new variable with values of "c_0001" called "ostracism_condition" and
# values of "c_0002" called "self_other_condition" (this could have also been don in the codebook already)


# Master_ost: Rearrange: Remove Variables c_0001, c_0002, move new variables (ostracism_condition and self_other_condition) to column nr. 2 & 3
# then move new variables Cyberball & Focus on spot 4 &5, then all other variables as before


# Long_data_all: Create new variable with values of "c_0001" called "ostracism_condition" and
# values of "c_0002" called "self_other_condition" (this could have also been don in the codebook already)


# Long_data_all: Reaarange: ID, ostracism_condition, self_other_condition, Cyberball, Focus, outcome, cause_self, cause_other, stability_value, locus_value, remaining variables




##############################################################################################################
############ Last step: Look at all negative outcomes and remove the ones that are not reasonable. ###########
#### e.g. description of feelings, "don't now answers, other answers that are not causes for the outcomes ####
##############################################################################################################

# Filter for negative outcomes (outcomes 1-5)


# Outcome 1
# First filter for outcome 1

# Assign "NA" to all stability_values of all IDs we want to exclude
o1$stability_value [o1$ID == XX | o1$ID == XX | o1$ID == XX] <- NA

# Assign "NA" to all locus_values of all IDs we want to exclude
o1$locus_value [o1$ID == XX | o1$ID == XX | o1$ID == XX ] <- NA


# Repeat for outcome 2.5
# Outcome 2

# Outcome 3


# Outcome 4


# Outcome 5



# Only positive Outcomes
long_pos <- long_data_all %>%
  filter (outcome %in% c(6:10))

# Merge together again (long_pos, o1, o2, o3, o4, o5) and save into object long_all



# Remove all NAs which we selected before (we could use either stability_value or locus_value, the result would be the same)
long_data_clean <- long_all[!is.na(long_all$stability_value),]

# Save master file and long file (change name to what you like to call it, I reccommend adding a date in the end)
write.csv(long_data_clean,'OST-CONST-04_long_data_200527.csv', row.names=FALSE)
write.csv(master_ost, "OST-CONST-04_master_200527.csv", row.names=FALSE)


