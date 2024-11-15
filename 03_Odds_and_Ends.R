## ------------------------------------------------------------------------------
# load packages and data
## ------------------------------------------------------------------------------
library(did)
library(BMisc)
library(twfeweights)
library(fixest)
library(modelsummary)
library(ggplot2)
load("data2.RData")
data2$region <- droplevels(data2$region)

## ------------------------------------------------------------------------------
# repeated cross sections
## ------------------------------------------------------------------------------
attgt_rc <- did::att_gt(
    yname = "lemp",
    gname = "G",
    tname = "year",
    data = data2,
    control_group = "nevertreated",
    base_period = "universal",
    panel = FALSE
)
ggdid(aggte(attgt_rc, type = "dynamic"))

## ------------------------------------------------------------------------------
# unbalanced panel
## ------------------------------------------------------------------------------
set.seed(123)
# randomly drop 100 observations
this_data <- data2[sample(1:nrow(data2), nrow(data2) - 100), ]
attgt_up <- did::att_gt(
    yname = "lemp",
    idname = "id",
    gname = "G",
    tname = "year",
    data = this_data,
    control_group = "nevertreated",
    base_period = "universal",
    panel = TRUE,
    allow_unbalanced_panel = TRUE
)
ggdid(aggte(attgt_up, type = "dynamic"))

## ------------------------------------------------------------------------------
# not-yet-treated comparison group
## ------------------------------------------------------------------------------
attgt_nyt <- did::att_gt(
    yname = "lemp",
    idname = "id",
    gname = "G",
    tname = "year",
    data = data2,
    control_group = "notyettreated",
    base_period = "universal"
)
ggdid(aggte(attgt_nyt, type = "dynamic"))

## ------------------------------------------------------------------------------
# not-yet-but-eventually comparison group
## ------------------------------------------------------------------------------
# have to do a little hack to get this to work
# drop never-treated group
this_data <- subset(data2, G != 0)
# note: this causes us to lose the 2006 group
# as it no longer has a valid comparison group
# and we lose some periods for the 2004 group
# because it only has a valide comparison group up to 2005
attgt_nye <- did::att_gt(
    yname = "lemp",
    idname = "id",
    gname = "G",
    tname = "year",
    data = this_data,
    control_group = "notyettreated",
    base_period = "universal"
)
ggdid(aggte(attgt_nye, type = "dynamic"))

## ------------------------------------------------------------------------------
# anticipation
## ------------------------------------------------------------------------------
# note: this causes us to lose the 2004 group due
# to not enough pre-treatment periods
attgt_ant <- did::att_gt(
    yname = "lemp",
    idname = "id",
    gname = "G",
    tname = "year",
    data = data2,
    control_group = "nevertreated",
    base_period = "universal",
    anticipation = 1
)
ggdid(aggte(attgt_ant, type = "dynamic"))

## ------------------------------------------------------------------------------
# universal base period (this is a repeat of previous results)
## ------------------------------------------------------------------------------
attgt_uni <- did::att_gt(
    yname = "lemp",
    idname = "id",
    gname = "G",
    tname = "year",
    data = data2,
    control_group = "nevertreated",
    base_period = "universal"
)
ggdid(aggte(attgt_uni, type = "dynamic"))

## ------------------------------------------------------------------------------
# varying base period
## ------------------------------------------------------------------------------
attgt_var <- did::att_gt(
    yname = "lemp",
    idname = "id",
    gname = "G",
    tname = "year",
    data = data2,
    control_group = "nevertreated",
    base_period = "varying"
)
ggdid(aggte(attgt_var, type = "dynamic"))

## ------------------------------------------------------------------------------
# show that joint pre-test is equivalent
## ------------------------------------------------------------------------------
summary(attgt_uni)

summary(attgt_var)

## ------------------------------------------------------------------------------
# code for honest_did
## ------------------------------------------------------------------------------
attgt <- did::att_gt(
    yname = "lemp",
    idname = "id",
    gname = "G",
    tname = "year",
    data = data2,
    control_group = "nevertreated",
    base_period = "universal"
)

# devtools::install_github("asheshrambachan/HonestDiD")
library(HonestDiD)
source("honest_did.R")
cs_es <- aggte(attgt, type = "dynamic")
hd_cs <- honest_did(
    es = cs_es,
    e = 0,
    type = "relative_magnitude"
)
createSensitivityPlot_relativeMagnitudes(
    hd_cs$robust_ci,
    hd_cs$orig_ci
)


hd_cs <- honest_did(
    es = cs_es,
    e = 0,
    type = "smoothness"
)
createSensitivityPlot(
    hd_cs$robust_ci,
    hd_cs$orig_ci
)


## ------------------------------------------------------------------------------
# sampling weights
## ------------------------------------------------------------------------------
# create weights based on population
data2$pop <- exp(data2$lpop)
data2$avg_pop <- BMisc::get_Yibar(data2, "id", "pop")
attgt_sw <- did::att_gt(
    yname = "lemp",
    idname = "id",
    gname = "G",
    tname = "year",
    data = data2,
    control_group = "nevertreated",
    base_period = "universal",
    weightsname = "avg_pop"
)
ggdid(aggte(attgt_sw, type = "dynamic"))

# employment rate
data2$emp_rate <- exp(data2$lemp) / data2$pop
attgt_er <- did::att_gt(
    yname = "emp_rate",
    idname = "id",
    gname = "G",
    tname = "year",
    data = data2,
    control_group = "nevertreated",
    base_period = "universal",
    weightsname = NULL
)
ggdid(aggte(attgt_er, type = "dynamic"))


# employment rate with varying base period
data2$emp_rate <- exp(data2$lemp) / data2$pop
attgt_erv <- did::att_gt(
    yname = "emp_rate",
    idname = "id",
    gname = "G",
    tname = "year",
    data = data2,
    control_group = "nevertreated",
    base_period = "varying",
    weightsname = NULL
)
ggdid(aggte(attgt_erv, type = "dynamic"))

## ------------------------------------------------------------------------------
# results with pointwise confidence interval
## ------------------------------------------------------------------------------
# note: these are the varying base period results
# that were "just barely" not statistically significant
# in pre-treatment periods
attgt_poi <- did::att_gt(
    yname = "lemp",
    idname = "id",
    gname = "G",
    tname = "year",
    data = data2,
    control_group = "nevertreated",
    base_period = "varying",
    cband = FALSE
)
ggdid(aggte(attgt_poi, type = "dynamic", cband = FALSE))
