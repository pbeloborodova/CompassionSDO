# INFO --------------------------------------------------------------------------------

# Project: SDO, Self-compassion & Mindfulness / with Michael Juberg
# Script: Pre-post comparison: demographics & psych variables

# CONFIGURE R -------------------------------------------------------------------------


options(scipen=99)  # Change scientific notation to decimals
options(stringsAsFactors = FALSE)  # Stop conversion of strings to factors

library(tidyverse)
library(rstatix)

# LOAD DATA ---------------------------------------------------------------------------


load("data/clean/selfcomp_clean_means.Rda")
load("data/clean/selfcomp_demographic.Rda")


# Add group to demographics

self_comp_demographic <-
  self_comp_demographic %>%
  mutate(group = ifelse(semester %in% c("Spring2019","Fall2019"), "PreCOVID", "PostCOVID"))


# DEMOGRAPHICS ------------------------------------------------------------------------


#### Age ####


wilcox_age <- wilcox.test(age ~ group, data = sc)
zstat_age <- qnorm(wilcox_age$p.value/2)
d_age <- zstat_age/sqrt(nrow(sc))


#### Gender ####


chisq_gender <- 
  self_comp_demographic %>%
  filter(gender != "Other") %>%
  select(group, gender) %>%
  count(group, gender) %>%
  pivot_wider(names_from = gender, values_from = n) %>%
  select(-1) %>%
  chisq_test()


#### Ethnicity ####


fisher_ethnic <- 
  self_comp_demographic %>%
  select(group, ethnicity) %>%
  count(group, ethnicity) %>%
  pivot_wider(names_from = ethnicity, values_from = n) %>%
  select(-1) %>%
  fisher_test(workspace = 2000000)


# PSYCH VARIABLES ---------------------------------------------------------------------


self_comp_clean_means %>%
  select(group, sdo_mean_win, iri_ec_pt_mean_win, starts_with("scs"), -scs_mean) %>%
  pivot_longer(-group, names_to = "variables", values_to = "value") %>%
  group_by(variables) %>%
  rstatix::t_test(value ~ group) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()

