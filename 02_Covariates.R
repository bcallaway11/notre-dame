# load packages
library(did)
library(BMisc)
library(twfeweights)
library(fixest)
library(modelsummary)
library(ggplot2)

# load date
load("data2.RData")
data2$region <- droplevels(data2$region)


## -----------------------------------------------------------------------------
# two period TWFE with lpop and lavg_pay
## -----------------------------------------------------------------------------
data2_subset <- subset(data2, year %in% c(2003, 2004))
data2_subset <- subset(data2_subset, G %in% c(0, 2004))
twfe_x <- fixest::feols(lemp ~ post + lpop + lavg_pay | id + year,
  data = data2_subset,
  cluster = "id"
)
modelsummary(twfe_x, gof_omit = ".*")


# regression covariate balance diagnostics
library(twfeweights)
tp_wts <- two_period_reg_weights(
  yname = "lemp",
  tname = "year",
  idname = "id",
  gname = "G",
  xformula = ~ lpop + lavg_pay,
  extra_balance_vars_formula = ~region,
  data = data2_subset
)

ggtwfeweights(tp_wts,
  absolute_value = FALSE,
  standardize = TRUE,
  plot_relative_to_target = FALSE
) +
  xlim(c(-2, 2))


## ------------------------------------------------------------------------------
# TWFE regression with more periods
## ------------------------------------------------------------------------------
twfe_x <- fixest::feols(lemp ~ post + lpop + lavg_pay | id + year,
  data = data2,
  cluster = "id"
)
modelsummary(twfe_x, gof_omit = ".*")

# twfe weights
twfe_wts <- implicit_twfe_weights(
  yname = "lemp",
  tname = "year",
  idname = "id",
  gname = "G",
  xformula = ~ lpop + lavg_pay,
  data = data2,
  base_period = "gmin1"
)
covariate_balance <- twfe_cov_bal(twfe_wts, ~ region + lpop + lavg_pay + -1)

ggtwfeweights(covariate_balance,
  absolute_value = FALSE,
  standardize = TRUE,
  plot_relative_to_target = FALSE
) +
  xlim(c(-1, 1))


## ------------------------------------------------------------------------------
# TWFE add region
## ------------------------------------------------------------------------------
twfe_x <- fixest::feols(lemp ~ post + lpop + lavg_pay | id + region^year,
  data = data2,
  cluster = "id"
)
modelsummary(twfe_x, gof_omit = ".*")


twfe_wts2 <- implicit_twfe_weights(
  yname = "lemp",
  tname = "year",
  idname = "id",
  gname = "G",
  xformula = ~ lpop + lavg_pay + as.factor(region) * as.factor(year),
  data = data2,
  base_period = "gmin1"
)
covariate_balance2 <- twfe_cov_bal(twfe_wts2, ~ region + lpop + lavg_pay + -1)

ggtwfeweights(covariate_balance2,
  absolute_value = FALSE,
  standardize = TRUE,
  plot_relative_to_target = FALSE
) +
  xlim(c(-1, 1))


## ------------------------------------------------------------------------------
# regression adjustment with \Delta X
# it's reg. adj. even though the function says aipw...
## ------------------------------------------------------------------------------
ra_wts <- implicit_aipw_weights(
  yname = "lemp",
  tname = "year",
  idname = "id",
  gname = "G",
  xformula = ~1,
  d_covs_formula = ~ lpop + lavg_pay,
  pscore_formula = ~1,
  data = data2
)
ra_wts$est

# regression adjustment covariate balance
ra_cov_bal <- aipw_cov_bal(ra_wts, ~ region + lpop + lavg_pay + -1)
ggtwfeweights(ra_cov_bal,
  absolute_value = FALSE,
  standardize = TRUE,
  plot_relative_to_target = FALSE
) +
  xlim(c(-1, 1))


## ------------------------------------------------------------------------------
# Callaway and Sant'Anna, regression adjustment, X_{g-1}, Z
## ------------------------------------------------------------------------------
cs_x <- att_gt(
  yname = "lemp",
  tname = "year",
  idname = "id",
  gname = "G",
  xformla = ~ region + lpop + lavg_pay,
  control_group = "nevertreated",
  base_period = "universal",
  est_method = "reg",
  data = data2
)
cs_x_res <- aggte(cs_x, type = "group")
summary(cs_x_res)

# event study
cs_x_dyn <- aggte(cs_x, type = "dynamic")
ggdid(cs_x_dyn)

# covariate balance weights
ra_wts <- implicit_aipw_weights(
  yname = "lemp",
  tname = "year",
  idname = "id",
  gname = "G",
  xformula = ~ region + lpop + lavg_pay,
  d_covs_formula = ~1,
  pscore_formula = ~1,
  data = data2
)

ra_cov_bal <- aipw_cov_bal(ra_wts, ~ region + lpop + lavg_pay + -1)
ggtwfeweights(ra_cov_bal,
  absolute_value = FALSE,
  standardize = TRUE,
  plot_relative_to_target = FALSE
) +
  xlim(c(-1, 1))

## ------------------------------------------------------------------------------
# callaway and sant'anna ipw, X_{g-1}, Z
## ------------------------------------------------------------------------------
cs_x <- att_gt(
  yname = "lemp",
  tname = "year",
  idname = "id",
  gname = "G",
  xformla = ~ region + lpop + lavg_pay,
  control_group = "nevertreated",
  base_period = "universal",
  est_method = "ipw",
  data = data2
)
cs_x_res <- aggte(cs_x, type = "group")
summary(cs_x_res)

cs_x_dyn <- aggte(cs_x, type = "dynamic")
ggdid(cs_x_dyn)

## ------------------------------------------------------------------------------
# callaway and sant'anna aipw, X_{g-1}, Z
## ------------------------------------------------------------------------------
cs_x <- att_gt(
  yname = "lemp",
  tname = "year",
  idname = "id",
  gname = "G",
  xformla = ~ region + lpop + lavg_pay,
  control_group = "nevertreated",
  base_period = "universal",
  est_method = "dr",
  data = data2
)
cs_x_res <- aggte(cs_x, type = "group")
summary(cs_x_res)

cs_x_dyn <- aggte(cs_x, type = "dynamic")
ggdid(cs_x_dyn)

aipw_wts <- implicit_aipw_weights(
  yname = "lemp",
  tname = "year",
  idname = "id",
  gname = "G",
  xformula = ~ region + lpop + lavg_pay,
  d_covs_formula = ~1,
  data = data2
)

aipw_cov_bal <- aipw_cov_bal(aipw_wts, ~ region + lpop + lavg_pay + -1)
ggtwfeweights(aipw_cov_bal,
  absolute_value = FALSE,
  standardize = TRUE,
  plot_relative_to_target = FALSE
) +
  xlim(c(-1, 1))
