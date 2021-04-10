# INFO --------------------------------------------------------------------------------

# Project: SDO, Self-compassion & Mindfulness / with Michael Juberg
# Script: Network Analysis

# CONFIGURE R -------------------------------------------------------------------------

options(scipen=99)  # Change scientific notation to decimals
options(stringsAsFactors = FALSE)  # Stop conversion of strings to factors

library(qgraph)
library(bootnet)
library(NetworkComparisonTest)
library(networktools)
library(stringr)


# LOAD DATA ---------------------------------------------------------------------------


load("data/clean/selfcomp_clean_means.Rda")


# NOT USING THIS CHUNK

# Create a variable for privileged / non-privileged
self_comp_means$privileged <-
  ifelse(self_comp_means$ethnicity %in% c("Asian", "Multicultural"), 1, 0)


# SUBSET ------------------------------------------------------------------------------

# Create a column names vector for subsetting subscales

net_vars <- c("sdo_mean_win",
          "scs_selfkindness_mean",
          "scs_selfjudgment_mean",
          "scs_commonhuman_mean",
          "scs_isolation_mean",
          "scs_mindfulness_mean",
          "scs_overidentified_mean",
          "iri_ec_pt_mean_win",
          "group")

# Subset subscales

self_comp_net <- self_comp_clean_means[, net_vars]

colnames(self_comp_net) <- c("SDO", "SCS_SK", "SCS_SJ", "SCS_CH", "SCS_I",
                             "SCS_M", "SCS_OI", "IRI_ECPT", "group")


# REDUNDANCY CHECK --------------------------------------------------------------------


gb_pre <- goldbricker(self_comp_net[self_comp_net$group == "PreCOVID",-9],
                   p = 0.05,
                   method = "hittner2003",
                   threshold = 0.25,
                   corMin = 0.5,
                   progressbar = TRUE)

gb_pre

# NOT USING THE FOLLOWING 2 CHUNKS

gb2_asian <- goldbricker(self_comp_net[self_comp_means$privileged == 1],
                   p = 0.05,
                   method = "hittner2003",
                   threshold = 0.25,
                   corMin = 0.5,
                   progressbar = TRUE)

gb3_nonasian <- goldbricker(self_comp_net[self_comp_means$privileged == 1],
                   p = 0.05,
                   method = "hittner2003",
                   threshold = 0.25,
                   corMin = 0.5,
                   progressbar = TRUE)


# ESTIMATE NETWORK --------------------------------------------------------------------


#### All participants ####

net_total <- estimateNetwork(self_comp_net,
                        default = "EBICglasso", 
                        weighted = TRUE,
                        threshold = TRUE, 
                        corMethod = "cor_auto")

centrality_auto(net_total)$node.centrality


#### Pre-COVID sample ####

net_pre <- estimateNetwork(self_comp_net[self_comp_clean_means$group == "PreCOVID",-9],
                             default = "EBICglasso", 
                             weighted = TRUE,
                             threshold = TRUE, 
                             corMethod = "cor_auto")

centrality_auto(net_pre)$node.centrality


#### Post-COVID sample ####

net_post <- estimateNetwork(self_comp_net[self_comp_clean_means$group == "PostCOVID",-9],
                           default = "EBICglasso", 
                           weighted = TRUE,
                           threshold = TRUE, 
                           corMethod = "cor_auto")

centrality_auto(net_post)$node.centrality


# NOT USING THE FOLLOWING 4 CHUNKS

#### Privileged participants ####

net_privileged <- estimateNetwork(self_comp_net[self_comp_means$privileged == 1,],
                        default = "EBICglasso", 
                        weighted = TRUE,
                        threshold = TRUE, 
                        corMethod = "cor_auto")


#### Non-privileged participants ####

net_nonprivileged <- estimateNetwork(self_comp_net[self_comp_means$privileged == 0,],
                        default = "EBICglasso", 
                        weighted = TRUE,
                        threshold = TRUE, 
                        corMethod = "cor_auto")


#### Male participants ####

net_male <- estimateNetwork(self_comp_net[self_comp_means$gender == "Male",],
                        default = "EBICglasso", 
                        weighted = TRUE,
                        threshold = TRUE, 
                        corMethod = "cor_auto")


#### Female participants ####

net_female <- estimateNetwork(self_comp_net[self_comp_means$gender == "Female",],
                        default = "EBICglasso", 
                        weighted = TRUE,
                        threshold = TRUE, 
                        corMethod = "cor_auto")


# PLOT NETWORK ------------------------------------------------------------------------


#### All participants ####

plot(net_total, details = TRUE)


#### Subsets ####

par(mfrow = c(1, 2))

plot(net_pre, details = TRUE)
title(main = "Pre-COVID", line = 3)

plot(net_post, details = TRUE)
title(main = "Post-COVID", line = 3)


# NOT USING THIS CHUNK

par(mfrow = c(1, 2))

plot(net_privileged, details = TRUE)
title(main = "Asian + Multicultural", line = 3)

plot(net_nonprivileged, details = TRUE)
title(main = "Everyone Else", line = 3)

plot(net_male, details = TRUE)
title(main = "Male Participants", line = 3)

plot(net_female, details = TRUE)
title(main = "Female Participants", line = 3)


# NETWORK DIAGNOSTICS -----------------------------------------------------------------


#### Case-dropping bootstrap stability ####


# Pre-COVID


# Edges

boot_edge_pre <- bootnet(net_pre, nCores = 8, nBoots = 5000)

save(boot_edge_pre, file = "data/clean/boot_edge_pre.Rda")

summary(boot_edge_pre)

plot(boot_edge_pre, order = "sample")


# Centrality indexes

boot_centrality_pre <- bootnet(net_pre, nCores = 8, nBoots = 5000,
                    type = "case",
                    statistics = c("edge", "strength", "expectedInfluence",
                                   "closeness", "betweenness"))

save(boot_centrality_pre, file = "data/clean/boot_centrality_pre.Rda")

corStability(boot_centrality_pre)

plot(boot_centrality_pre, statistics = c("strength","closeness",
                                         "betweenness","expectedInfluence"))


# Post-COVID

# Edges

boot_edge_post <- bootnet(net_post, nCores = 8, nBoots = 5000)

save(boot_edge_post, file = "data/clean/boot_edge_post.Rda")


# Centrality indexes

boot_centrality_post <- bootnet(net_post, nCores = 8, nBoots = 5000,
                    type = "case",
                    statistics = c("edge", "strength", "expectedInfluence",
                                   "closeness", "betweenness"))

save(boot_centrality_post, file = "data/clean/boot_centrality_post.Rda")

corStability(boot_centrality_post)


# NETWORKS COMPARISON -----------------------------------------------------------------


net_comparison <- NCT(self_comp_net[self_comp_clean_means$group == "PreCOVID",-9],
                      self_comp_net[self_comp_clean_means$group == "PostCOVID",-9],
                      it = 100, test.edges = TRUE, edges = "all",
                      # p.adjust.methods = "bonferroni",
                      test.centrality = TRUE,
                      centrality = c("strength", "expectedInfluence", "closeness", "betweenness"))

max(net_comparison$nwinv.perm)


# ITEM-LEVEL NETWORK ------------------------------------------------------------------


load("data/clean/selfcomp_all.Rda")


net_vars <- c(
  sprintf("sdo%d", seq(1:16)),
  sprintf("scs%d", seq(1:26)),
  sprintf("iri%d", c(2,4,9,14,18,20,22,3,8,11,15,21,25,28)))

self_comp_net <- self_comp[, net_vars]

# Back-reverse-code

# SDO

sdo_items <- net_vars[grep("^sdo", net_vars)]

sdo_keys <- c(-1,-1,-1,-1,-1,     # Items 1-5 (all R)
              -1,-1,-1, 1, 1,     # Items 6-10 (6, 7, 8 R)
               1, 1, 1, 1, 1, 1)  # Items 11-16 (none R)

self_comp_net[,sdo_items] <- psych::reverse.code(sdo_keys,
                                                 self_comp_net[,sdo_items],
                                                 mini = 1, maxi = 7)

# SCS

scs_items <- net_vars[grep("^scs", net_vars)]

scs_keys <- c(-1,-1, 1,-1, 1,     # Items 1-5 (1, 2, 4 R)
              -1, 1,-1, 1, 1,     # Items 6-10 (6, 8 R)
              -1, 1,-1, 1, 1,     # Items 11-15 (11, 13 R)
              -1, 1,-1, 1,-1,     # Items 16-20 (16, 18, 20 R)
              -1, 1, 1,-1,-1, 1)  # Items 21-26 (21, 24, 25 R)

self_comp_net[,scs_items] <- psych::reverse.code(scs_keys,
                                                 self_comp_net[,scs_items],
                                                 mini = 1, maxi = 5)

# IRI

iri_items <- net_vars[grep("^iri", net_vars)]

iri_keys <- c(1,-1, 1,-1,-1,      # Items 2,4,9,14,18 (4,14,18 R)
              1, 1,-1, 1, 1,      # Items 20,22,3,8,11 (3 R)
             -1, 1, 1, 1)         # Items 15,21,25,28 (15 R)

keys <- c(sdo_keys, scs_keys, iri_keys)

self_comp_net[,iri_items] <- psych::reverse.code(iri_keys,
                                                 self_comp_net[,iri_items],
                                                 mini = 1, maxi = 5)


colnames(self_comp_net) <- toupper(colnames(self_comp_net))

net_total <- estimateNetwork(self_comp_net,
                             default = "EBICglasso", 
                             weighted = TRUE,
                             threshold = TRUE, 
                             corMethod = "cor_auto")

plot(net_total, details = TRUE)

self_comp_net_groups <- stringr::str_remove(colnames(self_comp_net), "\\d{1,2}$")

plot(net_total, details = TRUE,
     groups = self_comp_net_groups,
     palette = "colorblind")
