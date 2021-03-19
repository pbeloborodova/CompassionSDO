# INFO --------------------------------------------------------------------------------


# Project: SDO, Self-compassion & Mindfulness / with Michael Juberg
# Script: Prepare pre- and post-COVID data: clean, add mean scores


# CONFIGURE R -------------------------------------------------------------------------


options(scipen=99)  # Change scientific notation to decimals
options(stringsAsFactors = FALSE)  # Stop conversion of strings to factors

library(readr)
library(psych)
library(dplyr)
library(lubridate)
library(naniar)
library(writexl)


# LOAD DATA ---------------------------------------------------------------------------


self_comp_raw <- read_csv("data/raw/SCS  raw survey_data 12-7.csv",
                          col_names = TRUE,
                          skip = 1)


# VARAIBLE NAMES ----------------------------------------------------------------------


# Print variable names
colnames(self_comp_raw)


# Rename variables
vars <- colnames(self_comp_raw)
vars[-c(9:24)] <- tolower(gsub("[-: ]", "", vars[-c(9:24)]))
vars[c(9:24)] <- tolower(gsub("-6:", "", vars[c(9:24)]))
vars[c(1,8)] <- c("id", "ethnicity_other")
colnames(self_comp_raw) <- vars


# Create a copy of data
self_comp <- self_comp_raw


# MISSING DATA ------------------------------------------------------------------------


colSums(is.na(self_comp))


# SURVEY DATA -------------------------------------------------------------------------


# Start time

self_comp$start_time <- as_datetime(self_comp$start_time,
                                    format = "%m/%d/%y %H:%M",
                                    tz = "HST")

# End time

self_comp$end_time <- as_datetime(self_comp$end_time,
                                    format = "%m/%d/%y %H:%M",
                                    tz = "HST")


# Add codes for spring and fall data collection periods

self_comp$semester <- as.character(ceiling_date(self_comp$end_time, "halfyear"))

self_comp$semester <- recode(self_comp$semester,
                             "2019-07-01" = "Spring2019",
                             "2020-01-01" = "Fall2019",
                             "2021-01-01" = "Fall2020")

table(self_comp$semester)


# DEMOGRAPHICS ------------------------------------------------------------------------


#### Age ####


# Get values
unique(self_comp$age)

# Replace values
self_comp$age <- recode(self_comp$age,
                        "19 years old" = "19", "18 years" = "18", "25 years" = "25",
                        "nineteen" = "19", "Born on Oahu (September 1st, 2001)" = "19",
                        "21 years old" = "21",
                        "I was born and raised on the island of Oahu, Hawaii. I was born on September 2001." = "19")

# Convert to numeric
self_comp$age <- as.numeric(self_comp$age)


#### Gender ####


# Get values
unique(self_comp$gender)
table(self_comp$gender)


#### Ethnicity ####


# Get values
unique(self_comp$ethnicity)
table(self_comp$ethnicity)

# Inspect "Other"
unique(self_comp$ethnicity_other)

# Replace missing values with NA
self_comp <- replace_with_na(self_comp,
                replace = list(ethnicity_other = c("[Decline to Answer]",
                                                   "n/a","N/A","NA","--","na")))


# CALCULATE VARIABLES -----------------------------------------------------------------


#### Social Dominance Orientation (SDO) ####


# (1) Create a vector of SDO items column numbers

sdo_items <- vars[grep("^sdo", vars)]

# (2) Transform values from character to numeric

# Get values
self_comp[,sdo_items] %>% t %>% c %>% unique

# Replace values
self_comp[,sdo_items] <-
  self_comp[,sdo_items] %>%
  mutate_all(~case_when(. %in% c("Very Negative", "7 Very Negative") ~ "7", 
                        . %in% c("Negative", "6 Negative") ~ "6", 
                        . %in% c("Slightly Negative", "5 Slightly Negative") ~ "5",
                        . %in% c("Neither positive nor negative", "4 Neither positive nor negative") ~ "4",
                        . %in% c("Slightly Positive", "3 Slightly Positive") ~ "3",
                        . %in% c("Positive", "2 Positive") ~ "2",
                        . %in% c("Very Positive", "1 Very Positive") ~ "1",
                        TRUE ~ .))

# Convert to numeric
self_comp[,sdo_items] <- apply(self_comp[,sdo_items], 2, as.numeric)

# (3) Reverse-code items 1-8

# Note: Items were accidentally reversed in the survey; hence, I reverse-coded items 1-8 rather
# than items 9-16, so that 1 = very negative (low SDO), 7 = very positive (high SDO)

sdo_keys <- c(-1,-1,-1,-1,-1,     # Items 1-5 (all R)
              -1,-1,-1, 1, 1,     # Items 6-10 (6, 7, 8 R)
               1, 1, 1, 1, 1, 1)  # Items 11-16 (none R)

self_comp[,sdo_items] <- psych::reverse.code(sdo_keys, self_comp[,sdo_items],
                                             mini = 1, maxi = 7)

# (4) Calculate SDO mean score

self_comp$sdo_mean <- rowMeans(self_comp[,sdo_items])


#### Five Facet Mindfulness Questionnaire (FFMQ) ####


# (1) Create a vector of FFMQ items column numbers

ffmq_items <- vars[grep("^ffmq", vars)]

# (2) Transform values from character to numeric

# Get values
self_comp[,ffmq_items] %>% t %>% c %>% unique

# Replace values
self_comp[,ffmq_items] <-
  self_comp[,ffmq_items] %>%
  mutate_all(~case_when(. == "never or very rarely true" ~ "1", 
                        . == "rarely true" ~ "2", 
                        . == "sometimes true" ~ "3",
                        . == "often true" ~ "4",
                        . == "very often or always true" ~ "5",
                        TRUE ~ .))

# Convert to numeric
self_comp[,ffmq_items] <- apply(self_comp[,ffmq_items], 2, as.numeric)

# (3) Reverse-code items 3, 5, 8, 10, 12, 13, 14, 16, 17, 18, 22, 23, 25, 28, 30, 34, 35, 38, 39

ffmq_keys <- c(1, 1,-1, 1,-1,  # Items 1-5 (3, 5 R)
               1, 1,-1, 1,-1,  # Items 6-10 (8, 10 R)
               1,-1,-1,-1, 1,  # Items 11-15 (12, 13, 14 R)
              -1,-1,-1, 1, 1,  # Items 16-20 (16, 17, 18 R)
               1,-1,-1, 1,-1,  # Items 21-25 (22, 23, 25 R)
               1, 1,-1, 1,-1,  # Items 26-30 (28, 30 R)
               1, 1, 1,-1,-1,  # Items 31-35 (34, 35 R)
               1, 1,-1,-1)     # Items 36-39 (38, 39 R)

self_comp[,ffmq_items] <- psych::reverse.code(ffmq_keys, self_comp[,ffmq_items],
                                             mini = 1, maxi = 5)

# (4) Calculate FFMQ mean scores

# Total scale
self_comp$ffmq_mean <- rowMeans(self_comp[,ffmq_items])

#	Observe subscale: items 1, 6, 11, 15, 20, 26, 31, 36
self_comp$ffmq_observe_mean <- rowMeans(self_comp[,ffmq_items[c(1,6,11,15,20,26,31,36)]])

#	Describe subscale: items 2, 7, 12R, 16R, 22R, 27, 32, 37
self_comp$ffmq_describe_mean <- rowMeans(self_comp[,ffmq_items[c(2,7,12,16,22,27,32,37)]])

#	Act with Awareness subscale: items 5R, 8R, 13R, 18R, 23R, 28R, 34R, 38R
self_comp$ffmq_actaware_mean <- rowMeans(self_comp[,ffmq_items[c(5,8,13,18,23,28,34,38)]])

#	Non-judge subscale: items 3R, 10R, 14R, 17R, 25R, 30R, 35R, 39R
self_comp$ffmq_nonjudge_mean <- rowMeans(self_comp[,ffmq_items[c(3,10,14,17,25,30,35,39)]])

#	Non-react subscale: items: 4, 9, 19, 21, 24, 29, 33
self_comp$ffmq_nonreact_mean <- rowMeans(self_comp[,ffmq_items[c(4,9,19,21,24,29,33)]])


#### Self-Compassion Scale (SCS) ####


# (1) Create a vector of SCS items column numbers

scs_items <- vars[grep("^scs", vars)]

# (2) Transform values from character to numeric

# Get values
self_comp[,scs_items] %>% t %>% c %>% unique

# Replace values
self_comp[,scs_items] <-
  self_comp[,scs_items] %>%
  mutate_all(~case_when(. == "1 Almost never" ~ "1",
                        . == "5 Almost always" ~ "5",
                        TRUE ~ .))

# Convert to numeric
self_comp[,scs_items] <- apply(self_comp[,scs_items], 2, as.numeric)

# (3) Reverse-code items 1, 2, 4, 6, 8, 11, 13, 16, 18, 20, 21, 24, 25 

scs_keys <- c(-1,-1, 1,-1, 1,     # Items 1-5 (1, 2, 4 R)
              -1, 1,-1, 1, 1,     # Items 6-10 (6, 8 R)
              -1, 1,-1, 1, 1,     # Items 11-15 (11, 13 R)
              -1, 1,-1, 1,-1,     # Items 16-20 (16, 18, 20 R)
              -1, 1, 1,-1,-1, 1)  # Items 21-26 (21, 24, 25 R)

self_comp[,scs_items] <- psych::reverse.code(scs_keys, self_comp[,scs_items],
                                              mini = 1, maxi = 5)

# (4) Calculate SCS mean scores

# Total scale
self_comp$scs_mean <- rowMeans(self_comp[,scs_items])

#	Self-Kindness subscale: items 5, 12, 19, 23, 26
self_comp$scs_selfkindness_mean <- rowMeans(self_comp[,scs_items[c(5,12,19,23,26)]])

#	Self-Judgment subscale: items 1, 8, 11, 16, 21 (reverse all)
# Note: subscale is reverse-scored, so that 1 = lower self-judgment, 5 = higher self-judgment
self_comp$scs_selfjudgment_mean <- 6 - rowMeans(self_comp[,scs_items[c(1,8,11,16,21)]])

#	Common Humanity subscale: items 3, 7, 10, 15
self_comp$scs_commonhuman_mean <- rowMeans(self_comp[,scs_items[c(3,7,10,15)]])

#	Isolation subscale: items 4, 13, 18, 25 (reverse all)
# Note: subscale is reverse-scored, so that 1 = lower isolation, 5 = higher isolation
self_comp$scs_isolation_mean <- 6 - rowMeans(self_comp[,scs_items[c(4,13,18,25)]])

#	Mindfulness subscale: items 9, 14, 17, 22
self_comp$scs_mindfulness_mean <- rowMeans(self_comp[,scs_items[c(9,14,17,22)]])

#	Over-identified subscale: items 2, 6, 20, 24 (reverse all)
# Note: subscale is reverse-scored, so that 1 = lower over-identification, 5 = higher over-identification
self_comp$scs_overidentified_mean <- 6 - rowMeans(self_comp[,scs_items[c(2,6,20,24)]])


#### Interpersonal Reactivity Index (IRI) ####


# (1) Create a vector of IRI items column numbers

iri_items <- vars[grep("^iri", vars)]

# (2) Transform values from character to numeric

# Get values
self_comp[,iri_items] %>% t %>% c %>% unique

# Replace values
self_comp[,iri_items] <-
  self_comp[,iri_items] %>%
  mutate_all(~case_when(. == "1 Does not describe me well" ~ "1",
                        . == "5 Describes me very well" ~ "5",
                        TRUE ~ .))

# Convert to numeric
self_comp[,iri_items] <- apply(self_comp[,iri_items], 2, as.numeric)

# (3) Reverse-code items 3, 4, 7, 12, 13, 14, 15, 18, 19

iri_keys <- c( 1, 1,-1,-1, 1,     # Items 1-5 (3, 4 R)
               1,-1, 1, 1, 1,     # Items 6-10 (7 R)
               1,-1,-1,-1,-1,     # Items 11-15 (12, 13, 14, 15 R)
               1, 1,-1,-1, 1,     # Items 16-20 (18, 19 R)
               1, 1, 1,-1, 1,     # Items 21-25 (none R)
               1, 1, 1)           # Items 26-28 (none R)

self_comp[,iri_items] <- psych::reverse.code(iri_keys, self_comp[,iri_items],
                                             mini = 1, maxi = 5)

# (4) Calculate IRI mean scores

# Total scale
self_comp$iri_mean <- rowMeans(self_comp[,iri_items])

# Emp concern + perspective taking composite score
self_comp$iri_ec_pt_mean <- rowMeans(self_comp[,iri_items[c(2,4,9,14,18,20,22,      # EC part
                                                            3,8,11,15,21,25,28)]])  # PT part

#	Fantasy subscale: items 1, 5, 7R, 12R, 16, 23, 26
self_comp$iri_fantasy_mean <- rowMeans(self_comp[,iri_items[c(1,5,7,12,16,23,26)]])

#	Empathetic Concern subscale: items 2, 4R, 9, 14, 18R, 20, 22
self_comp$iri_empconcern_mean <- rowMeans(self_comp[,iri_items[c(2,4,9,14,18,20,22)]])

#	Perspective Taking subscale: items 3R, 8, 11, 15R, 21, 25, 28
self_comp$iri_perstaking_mean <- rowMeans(self_comp[,iri_items[c(3,8,11,15,21,25,28)]])

#	Personal Distress  subscale: items 6, 10, 13R, 17, 19R, 24, 27
self_comp$iri_persdistress_mean <- rowMeans(self_comp[,iri_items[c(6,10,13,17,19,24,27)]])


# SAVE DEMOGRAPHIC DATA ---------------------------------------------------------------


self_comp_demographic <- self_comp[,c(1,5:7,118)]
save(self_comp_demographic, file = "data/clean/selfcomp_demographic.Rda")


# ALPHAS ------------------------------------------------------------------------------


# SDO
psych::alpha(self_comp[,sdo_items])

# SCS Total
psych::alpha(self_comp[,scs_items])

# SCS Self-Kindness
psych::alpha(self_comp[,scs_items[c(5,12,19,23,26)]])

# SCS Self-judgment
psych::alpha(self_comp[,scs_items[c(1,8,11,16,21)]])
  
# SCS Common Humanity
psych::alpha(self_comp[,scs_items[c(3,7,10,15)]])

# SCS Isolation
psych::alpha(self_comp[,scs_items[c(4,13,18,25)]])

# SCS Mindfulness
psych::alpha(self_comp[,scs_items[c(9,14,17,22)]])

# SCS Over-identification
psych::alpha(self_comp[,scs_items[c(2,6,20,24)]])

# IRI EC/PT composite
psych::alpha(self_comp[,iri_items[c(2,4,9,14,18,20,22,3,8,11,15,21,25,28)]])

# IRI EC
psych::alpha(self_comp[,iri_items[c(2,4,9,14,18,20,22)]])

# IRI PT
psych::alpha(self_comp[,iri_items[c(3,8,11,15,21,25,28)]])


# ADD GROUPS ---------------------------------------------------------------------------


# NOT USING RANDOM SELECTION, BUT WILL KEEP FOR REFERENCE

# Randomly select 40% of the pre-COVID sample
set.seed(23)
random40 <-
  self_comp_clean %>%
  filter(semester != "Fall2020") %>%
  select(id) %>%
  sample_n(215)

random40 <- random40$id

# Assign groups
self_comp_clean <- self_comp_clean %>%
  mutate(group = case_when(semester == "Fall2020" ~ "sem_post",
                           id %in% random40 ~ "net",
                           TRUE ~ "sem_pre"))

# Check if groups were assigned correctly
table(self_comp_clean$group)

# GO FROM HERE

# Assign pre/post-COVID groups

self_comp <- self_comp %>%
  mutate(group = ifelse(semester %in% c("Spring2019","Fall2019"), "PreCOVID", "PostCOVID"))


# SAVE ALL DATA -----------------------------------------------------------------------


# All data
save(self_comp, file = "data/clean/selfcomp_all.Rda")

# Means only
self_comp_all_means <- self_comp[,-c(2:117)]
save(self_comp_all_means, file = "data/clean/selfcomp_all_means.Rda")


# CLEAN DATA --------------------------------------------------------------------------


#### Survey completion time ####


# Establish speeding criteria (2x faster than median)
dur_cutoff <- median(self_comp$duration)*0.5

# Number and % of speeding participants
sum(self_comp$duration < dur_cutoff)
100*round(sum(self_comp$duration < dur_cutoff)/nrow(self_comp), 4)

# Remove speeding participants
self_comp_clean_means <- self_comp_all_means[self_comp$duration >= dur_cutoff,]


#### Univariate outliers ####


# Make a df with z-scores

self_comp_clean_z <-
  self_comp_clean_means %>%
  mutate_at(vars(ends_with("mean")), list(z = ~as.vector(scale(.)))) %>%
  select(ends_with("_z"))


# Calculate the number of univariate outliers

colSums(self_comp_clean_z > 3.29)     # High
colSums(self_comp_clean_z < -3.29)    # Low


# Have a look at univariate outliers

self_comp_clean_means[self_comp_clean_z$sdo_mean_z > 3.29, "sdo_mean"]
self_comp_clean_means[self_comp_clean_z$iri_ec_pt_mean_z < -3.29, "iri_ec_pt_mean"]
self_comp_clean_means[self_comp_clean_z$iri_empconcern_mean_z < -3.29, "iri_empconcern_mean"]
self_comp_clean_means[self_comp_clean_z$iri_perstaking_mean_z < -3.29, "iri_perstaking_mean"]


# Winsorize outlier: SDO
self_comp_clean_means$sdo_mean_win <- self_comp_clean_means$sdo_mean
self_comp_clean_means$sdo_mean_win[which(self_comp_clean_z$sdo_mean_z > 3.29)] <-
  3.29*sd(self_comp_clean_means$sdo_mean) + mean(self_comp_clean_means$sdo_mean)


# Winsorize outlier: IRI EC/PT
self_comp_clean_means$iri_ec_pt_mean_win <- self_comp_clean_means$iri_ec_pt_mean
self_comp_clean_means$iri_ec_pt_mean_win[which(self_comp_clean_z$iri_ec_pt_mean_z < -3.29)] <-
  -3.29*sd(self_comp_clean_means$iri_ec_pt_mean) + mean(self_comp_clean_means$iri_ec_pt_mean)


# Winsorize outlier: IRI EC
self_comp_clean_means$iri_empconcern_mean_win <- self_comp_clean_means$iri_empconcern_mean
self_comp_clean_means$iri_empconcern_mean_win[which(self_comp_clean_z$iri_empconcern_mean_z < -3.29)] <-
  -3.29*sd(self_comp_clean_means$iri_empconcern_mean) + mean(self_comp_clean_means$iri_empconcern_mean)


# Winsorize outlier: IRI PT
self_comp_clean_means$iri_perstaking_mean_win <- self_comp_clean_means$iri_perstaking_mean
self_comp_clean_means$iri_perstaking_mean_win[which(self_comp_clean_z$iri_perstaking_mean_z < -3.29)] <-
  -3.29*sd(self_comp_clean_means$iri_perstaking_mean) + mean(self_comp_clean_means$iri_perstaking_mean)


#### Multivariate outliers ####


# Mahalanobis distance
mah_dist <- mahalanobis(self_comp_clean_means[,c(24,11:16,25)],
                        center = colMeans(self_comp_clean[,c(24,11:16,25)], na.rm = TRUE),
                        cov = cov(self_comp_clean[,c(24,11:16,25)], use = "complete.obs"))

# Critical value for 8 df is 26.13
sum(mah_dist > 26.13)

# Remove multivariate outliers
self_comp_clean_means <- self_comp_clean_means[mah_dist < 26.13,]


# How many observations overall were removed

# Pre-COVID sample
sum(self_comp$semester %in% c("Spring2019","Fall2019")) - sum(self_comp_clean_means$semester %in% c("Spring2019","Fall2019"))

# Post-COVID sample
sum(self_comp$semester == "Fall2020") - sum(self_comp_clean_means$semester == "Fall2020")


# SAVE CLEAN DATA ---------------------------------------------------------------------


# Means only
save(self_comp_clean_means, file = "data/clean/selfcomp_clean_means.Rda")


# DON'T NEED TO SAVE THIS, BUT WILL KEEP FOR REFERENCE
# ID groups
groups <- self_comp_clean[,c("id","group")]
write_xlsx(groups, "data/clean/groups.xlsx")

