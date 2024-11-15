library(revealEquations)
library(did)
library(BMisc)
library(twfeweights)
library(fixest)
library(modelsummary)
library(ggplot2)
library(pte)
library(qte)
library(ife)
library(dplyr)
library(tidyr)
load("data2.RData")


# lagged outcome unconfoundedness
data2$G2 <- data2$G
# lagged outcomes identification strategy
lo_res <- pte::pte_default(
  yname = "lemp",
  tname = "year",
  idname = "id",
  gname = "G2",
  data = data2,
  d_outcome = FALSE,
  lagged_outcome_cov = TRUE
)

did::ggdid(lo_res$att_gt, ylim = c(-.2, 0.05))


summary(lo_res)


ggpte(lo_res) + geom_hline(yintercept = 0, size = 1.2)


# change-in-changes
data2$G2 <- data2$G
cic_res <- qte::cic2(
  yname = "lemp",
  gname = "G2",
  tname = "year",
  idname = "id",
  data = data2,
  boot_type = "empirical",
  cl = 4,
  biters = 100
)
ggpte(cic_res) + geom_hline(yintercept = 0, size = 1.2)


summary(cic_res)


plot_df <- cic_res$attgt_results
plot_df$post <- as.factor(1 * (plot_df$time.period >= plot_df$group))
ggplot(plot_df, aes(x = time.period, y = att, color = post)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.1) +
  facet_wrap(~group, ncol = 1)
labs(title = "Change-in-Changes", x = "Year", y = "ATT(g,t)") +
  geom_hline(yintercept = 0, size = 1.2) +
  theme(legend.position = "none")


# cic qtt's
data2$emp_rate <- exp(data2$lemp) / data2$pop
cic_qte10 <- qte::cic2(
  yname = "emp_rate",
  gname = "G2",
  tname = "year",
  idname = "id",
  data = data2,
  boot_type = "empirical",
  cl = 4,
  biters = 100,
  gt_type = "qtt",
  ret_quantile = 0.1
)
ggpte(cic_qte10) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, nsmall = 5)) +
  geom_hline(yintercept = 0, size = 1.2)

cic_qte50 <- qte::cic2(
  yname = "emp_rate",
  gname = "G2",
  tname = "year",
  idname = "id",
  data = data2,
  boot_type = "empirical",
  cl = 4,
  biters = 100,
  gt_type = "qtt",
  ret_quantile = 0.5
)
ggpte(cic_qte50) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, nsmall = 5)) +
  geom_hline(yintercept = 0, size = 1.2)


cic_qte90 <- qte::cic2(
  yname = "emp_rate",
  gname = "G2",
  tname = "year",
  idname = "id",
  data = data2,
  boot_type = "empirical",
  cl = 4,
  biters = 100,
  gt_type = "qtt",
  ret_quantile = 0.9
)
ggpte(cic_qte90) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, nsmall = 5)) +
  geom_hline(yintercept = 0, size = 1.2)


# interactive fixed effects

load("mw_data_ch_more_years2.RData")
data3 <- data

data3 <- subset(data3, G != 1999) # drop early and already treated groups
data3 <- subset(data3, G != 1998)
data3 <- subset(data3, G != 2000)
data3$cohort <- data3$G


cs_res <- att_gt(
  yname = "lemp",
  gname = "G",
  tname = "year",
  idname = "countyreal",
  data = data3,
  anticipation = 0,
  control_group = "nevertreated"
)
ggdid(cs_res, ylim = c(-.2, .25))


dyn_res <- aggte(cs_res, type = "dynamic")
ggdid(dyn_res)


library(ife)
set.seed(09192024)
ife1 <- staggered_ife2(
  yname = "lemp",
  gname = "cohort",
  tname = "year",
  idname = "id",
  data = data3,
  nife = 1,
  weighting_matrix = "2sls",
  cband = FALSE,
  boot_type = "empirical",
  biters = 250,
  cl = 10,
  anticipation = 0
)

plot_df <- ife1$attgt_results
plot_df <- subset(plot_df, time.period < 2006)
plot_df$post <- as.factor(1 * (plot_df$time.period >= plot_df$group))
ggplot(plot_df, aes(x = time.period, y = att, color = post)) +
  geom_point(size = 2) +
  geom_errorbar(aes(
    ymax = att + 1.96 * se,
    ymin = att - 1.96 * se
  ), width = 0.1) +
  facet_wrap(~group, ncol = 1) +
  theme_classic() +
  ylim(c(-.2, .25)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)


summary(ife1)
