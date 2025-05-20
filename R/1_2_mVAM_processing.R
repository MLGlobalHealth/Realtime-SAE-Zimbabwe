## In this script, we compute
### - survey weights for mVAM
### - cell scores (i.e., probability that each household in mVAM survey are likely to own mobile-phone)
##### we use [1] MICS survey for mobile-phone only model as well as [2] ZimVAC survey for joint model
# ----------------------
rq_packages <- c(
  "here",
  "tidyverse",
  "readxl"
  #"glmmTMB","memisc",
)
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list = c("rq_packages", "installed_packages"))
# ----------------------
### 1. survey weights
# ----------------------
# read mVAM data and population frequency counts
df_raw_mVAM <- read_csv(here("data", "raw", "mVAM", "mVAM.csv"))
df_popweight <- read_excel(here(
  "data",
  "raw",
  "mVAM",
  "WFP_mvam_weights_pop.xlsx"
))

df_raw_mVAM <- df_raw_mVAM |>
  mutate(
    hhh_edu = factor(
      hhh_edu,
      levels = c("preprimary/other", "primary", "secondary", "higher")
    )
  )
df_popweight <- df_popweight |>
  select(
    ADM1_NAME_Survey,
    ADM2_NAME_Survey,
    Pop,
    HHHEduGrp_noSchool,
    HHHEduGrp_primary,
    HHHEduGrp_secondary,
    HHHEduGrp_higher
  ) |>
  mutate(
    adm1_name = str_to_title(ADM1_NAME_Survey),
    adm2_name = str_to_title(ADM2_NAME_Survey),
  ) |>
  select(-ADM1_NAME_Survey, -ADM2_NAME_Survey)
colnames(df_popweight)[2:5] <- c(
  "preprimary/other",
  "primary",
  "secondary",
  "higher"
)

# Compute relative frequency (proportion) of each category in population
df_popweight_dmg <- df_popweight |>
  select(-Pop, -adm2_name) |>
  unique() |>
  pivot_longer(
    cols = `preprimary/other`:`higher`,
    names_to = "hhh_edu",
    values_to = "prop_pop"
  )
# Compute relative frequency (proportion) of each category in the sample
df_smpweight_dmg <- df_raw_mVAM |>
  group_by(obs_month, adm1_name, hhh_edu) |>
  summarise(n = n()) |>
  ungroup() |>
  left_join(
    (df_raw_mVAM |>
      group_by(obs_month, adm1_name) |>
      summarise(n_pm_padm1 = n())),
    by = c('obs_month', 'adm1_name')
  ) |>
  mutate(
    prop_sample = n / n_pm_padm1
  ) |>
  ungroup() |>
  select(-n, -n_pm_padm1)
# compute demographic weights
df_dmg_weight <- df_smpweight_dmg |>
  left_join(df_popweight_dmg, by = c('adm1_name', 'hhh_edu')) |>
  mutate(dmg_weight_ust = prop_pop / prop_sample) |>
  select(-prop_sample, -prop_pop)

write_rds(df_dmg_weight, here("data", "processed", "mVAM_sample_weights.rds"))
write_csv(df_dmg_weight, here("data", "processed", "mVAM_sample_weights.csv"))
rm(df_smpweight_dmg, df_popweight_dmg)

# Geography weight - if adm1 level estimates are needed.
# This is not needed for our purpose
# df_smpweight_geo <- df_raw_rtm |>
#   group_by(obs_month,adm1_name,adm2_name) |>
#   summarise(n = n()) |> ungroup() |> left_join(
#     (df_raw_rtm |> group_by(obs_month) |> summarise(n_per_month = n())), by = 'obs_month'
#   ) |>
#   mutate(
#     prop_geoadm2_pm = n/n_per_month
#   )
# ----------------------
### 1. cell-score
# ----------------------
library("glmmTMB")
library("memisc")
# ----------------------
# 1.1 using MICS data
data_mics_raw <- read_csv(here("data", "processed", "mics.csv"))
data_mics <- data_mics_raw %>%
  mutate(cell = as.numeric(h_cell == "yes"))
df_raw_mVAM <- read_csv(here("data", "raw", "mVAM", "mVAM.csv"))

vars_mod <- intersect(names(data_mics), names(df_raw_mVAM))
vars_mod <- vars_mod[
  !vars_mod %in%
    c(
      "hh_size_bucket",
      "h_toilet_type_3way",
      "h_toilet_type",
      "h_water_source_2way"
    )
]
df_cell <- data_mics[, c("cell", vars_mod, "weight_hh")] %>%
  mutate(cell = as.integer(cell))

# simple mod - no interactions
model_simple <- glm(
  as.formula(paste0("cell ~ ", paste0(vars_mod, collapse = "+"))),
  data = df_cell,
  family = binomial(link = "logit"),
  weights = weight_hh
)
summary(model_simple)
data_mics$p_cell <- model_simple %>% predict(data_mics, type = "response")
# AUC
pROC::roc(cell ~ p_cell, data_mics)
# check for NAs
data_mics %>%
  filter(is.na(p_cell)) %>%
  count()
# plot deciles
data_mics %>%
  mutate(
    deciles = cut(
      p_cell,
      breaks = quantile(p_cell, probs = seq(0, 1, 0.1)),
      include.lowest = T
    )
  ) %>%
  group_by(deciles) %>%
  dplyr::summarize(model = mean(p_cell), actual = mean(cell)) %>%
  pivot_longer(cols = c("model", "actual")) %>%
  ggplot(aes(x = deciles, y = value, color = name)) +
  geom_point() +
  theme_minimal()

# check dist of preductions
hist(data_mics$p_cell)
# check prediction error by region
data_mics %>%
  group_by(adm1_name) %>%
  dplyr::summarise(
    cell_act_raw = mean(cell),
    cell_act_w = sum(cell * weight_hh) / sum(weight_hh),
    cell_est_raw = mean(p_cell),
    cell_est_w = sum(p_cell * weight_hh) / sum(weight_hh)
  ) %>%
  mutate(
    pred_error_raw = cell_est_raw - cell_act_raw,
    pred_error_w = cell_est_w - cell_act_w
  )

# score on survey data
df_raw_mVAM %>%
  group_by(respondent_id) %>%
  count() %>%
  arrange(-n)

# WFP9064585237481792098 appears more than once in the survey (3 times)
# hence they are given multiple p_cell values

df_raw_mVAM$p_cell <- model_simple %>% predict(df_raw_mVAM, type = "response")
write_csv(
  df_raw_mVAM %>% dplyr::select(respondent_id, p_cell),
  here("data", "processed", "cell_scores.csv")
)
# ----------------------
# 1.2 using ZimVAC data
data_zimvac <- read_csv(here("data", "processed", "zimvac2023_survey.csv"))
zimvac_adm2 <- data_zimvac$adm2_name |> unique()
df_raw_mVAM_zimvacadm2only <- df_raw_mVAM |> filter(adm2_name %in% zimvac_adm2)

## Further processing of zimvac individual data
data_zimvac <- data_zimvac |>
  mutate(
    h_water_source = case_when(
      h_water_source_raw %in%
        c(
          "Piped into dwelling",
          "Piped into neighbour",
          "Piped into public tap or standpipe",
          " Piped into yard or plot"
        ) ~
        "piped",
      h_water_source_raw %in%
        c(
          "Other",
          "Unprotected spring",
          "Unprotected well",
          "Sand abstraction",
          "Surface water (river, dam, lake, pond, stream, canal, irrigation channel)"
        ) ~
        "unprotected/other",
      T ~ "protected"
    ),
  )
vars_mod <- intersect(names(data_zimvac), names(df_raw_mVAM))
vars_mod <- vars_mod[
  !vars_mod %in%
    c(
      "fcs",
      "obs_date",
      "obs_week",
      "obs_month",
      "obs_quarter",
      "obs_year",
      #"adm2_name",
      "hh_size_bucket", #hh_size
      "hhh_edu_raw",
      "h_toilet_type_3way",
      "h_toilet_type",
      "h_toilet_type_raw", #  h_toilet_type_2way ?
      "h_water_source_2way",
      "h_water_source_raw" # use h_water_source
    )
]

df_cell <- data_zimvac[, c("h_cell", vars_mod)] |>
  mutate(
    cell = (h_cell |> as.factor() |> as.numeric() - 1),
  )
df_cell[rowSums(is.na(df_cell)) > 0, ] |> view() # note that there are some NA values
df_cell <- df_cell[rowSums(is.na(df_cell)) == 0, ]
## Formula
formula <- cell ~
  1 +
    hh_size +
    hhh_edu +
    h_water_source +
    h_toilet_type_2way +
    hhh_sex +
    (1 | adm2_name)

# simple mod - no interactions
model_simple <- glmmTMB(
  formula = formula,
  data = df_cell,
  family = binomial(link = "logit") #,
  # Disable the dispersion term
  #dispformula = ~ 0 # only needed when using exponential family with dispersion parameter, otherwise ignored.
)
summary(model_simple)
df_cell$p_cell <- model_simple %>% predict(df_cell, type = "response")

# AUC
pROC::roc(cell ~ p_cell, df_cell)
# check for NAs
df_cell %>%
  filter(is.na(p_cell)) %>%
  count()

# plot deciles
df_cell |>
  mutate(
    deciles = cut(
      p_cell,
      breaks = quantile(p_cell, probs = seq(0, 1, 0.1)),
      include.lowest = T
    )
  ) %>%
  group_by(deciles) %>%
  dplyr::summarize(
    model = mean(p_cell, na.rm = T),
    actual = mean(cell, na.rm = T)
  ) %>%
  pivot_longer(cols = c("model", "actual")) %>%
  ggplot(aes(x = deciles, y = value, color = name)) +
  geom_point() +
  theme_minimal()

# check dist of preductions
hist(df_cell$p_cell)

# check prediction error by region
df_cell %>%
  group_by(adm1_name) %>%
  dplyr::summarise(
    cell_act_raw = mean(cell),
    cell_est_raw = mean(p_cell),
  ) %>%
  mutate(
    pred_error_raw = cell_est_raw - cell_act_raw,
  )

df_raw_mVAM_zimvacadm2only$p_cell <- model_simple %>%
  predict(
    df_raw_mVAM_zimvacadm2only,
    type = "response",
    allow.new.levels = TRUE
  )

write_csv(
  df_raw_mVAM_zimvacadm2only %>%
    dplyr::select(respondent_id, p_cell),
  here("data", "processed", "cell_scores_ZIMVAC.csv")
)
