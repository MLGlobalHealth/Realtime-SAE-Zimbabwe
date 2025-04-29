### final pre-processing of mVAM & ZimVAC data for analysis
## Install and load required packages
rq_packages <- c(
  "here",  "haven", "sf","purrr","recipes",
  "readr","readxl","tidyverse","glmmTMB"
)
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))

## ---- Some setting(s) ----
THRESHOLD <- 28 # FCS threshold (classified to "poor" if below this threshold)

## ---- Read mVAM and ZimVAC data ----
# read WFP(rtm) data and merge p_cell (probability of each household having phone)
# read shape file
sf <- readRDS(here("data", "processed", "shapefile.rds"))
df_raw_mVAM <- read_csv(here("data", "raw","mVAM","mVAM.csv")) %>% distinct()
p_cell <- read_csv(here("data", "processed", "cell_scores.csv")) |> distinct()
p_cell_zimvac <- read_csv(here("data", "processed", "cell_scores_ZIMVAC.csv")) |> distinct()

# read zimvac(f2f) data
df_raw_zimvac23 <- read_csv(here("data", "raw","ZimVAC", "zimvac2023_survey.csv"))
df_raw_zimvac24 <- read_csv(here("data", "raw","ZimVAC", "zimvac2024_survey.csv"))

# ---- Only keep the districts included in ZIMVAC survey ----
adm2_zimvac = df_raw_zimvac23$adm2_name |> unique()
df_raw_mVAM <- df_raw_mVAM |> filter(adm2_name %in%adm2_zimvac)
sf <- sf |> filter(adm2_name %in%adm2_zimvac)

## ---- Process RTM and f2f data ----
df_raw_mVAM_mp <- df_raw_mVAM |> left_join(p_cell, by = "respondent_id") |> mutate(model='mobile-phone')
df_raw_mVAM_joint <- df_raw_mVAM |> left_join(p_cell_zimvac, by = "respondent_id") |> mutate(model='joint')
df_raw_mVAM <- bind_rows(df_raw_mVAM_mp,df_raw_mVAM_joint)
df_mVAM <- df_raw_mVAM |>
  dplyr::select(-"operator_id", -"adm0_code", -"adm1_code", -"adm2_code", 
                -starts_with("fcs_"), -starts_with("rcsi_")) |> 
  mutate(
    modality = 'rtm',
    group = rep(1, nrow(df_raw_mVAM)),
    h_cell = "yes") |>
  filter(!is.na(fcs))

df_zimvac23 <- df_raw_zimvac23[,c(subset(colnames(df_raw_zimvac23),colnames(df_raw_zimvac23)%in%colnames(df_mVAM)),"fcs_raw")]|> 
  mutate(modality = "f2f",
         #group = rep(1, nrow(df_raw_zimvac23)),
         p_cell = as.numeric(as.factor(h_cell))-1,
         #poor = fcs < THRESHOLD,
         #borderline = fcs < THRESHOLD2
  )|>
  filter(!is.na(fcs))

df_zimvac24 <- df_raw_zimvac24[,c(subset(colnames(df_raw_zimvac24),colnames(df_raw_zimvac24)%in%colnames(df_mVAM)),"fcs_raw")]|> 
  mutate(modality = "f2f",
         #group = rep(1, nrow(df_raw_zimvac24)),
         p_cell = as.numeric(as.factor(h_cell))-1,
         #poor = fcs < THRESHOLD,
         #borderline = fcs < THRESHOLD2
  )|>
  filter(!is.na(fcs))

st_lon_lat <- sf |>
  st_centroid() |>
  dplyr::rowwise() |>
  mutate(
    lon = unlist(purrr::map(geometry, 1)),
    lat = unlist(purrr::map(geometry, 2))
  ) |>
  as_tibble() |>
  select(adm2_name, lon, lat)

## ---- Read poststratification-frame ----
popframe_dhs <- read_csv(here("data", "processed", "popframe_dhs_census22.csv"))
popframe_zimvac23ind <- read_csv(here("data", "processed", "popframe_zimvac23ind_freq.csv"))
popframe_zimvac23ind <- popframe_zimvac23ind |> 
  mutate(pop_weight=n) |>
  select(-Freq,-n_adm2,-prop_adm2,-prop_cell_byadm2,-pop_weight_byadm2)

popframe <- bind_rows(
  popframe_dhs |> mutate(popframe_source = "dhs"),
  popframe_zimvac23ind |> mutate(popframe_source = "zimvac23ind")
) #|> filter(adm2_name %in% adm2_zimvac)

# Group by only the variables we have in the model below to avoid repeat prediction calculations
popframe_agg <- popframe |>
  mutate(p_cell = as.factor(h_cell) |> as.numeric()-1) |>
  group_by(
    popframe_source, adm1_name, adm2_name,
    hhh_edu, hhh_sex, h_toilet_type_2way, h_water_source_2way, hh_size_bucket, p_cell, h_cell
  ) |>
  summarise(pop_weight = sum(pop_weight)) |>
  mutate(modality = 'f2f')

## ---- processing ----
processing_for_mlr <- function(df, type = "observation"){
  if (type=="observation"){
    df <- df |> arrange(adm2_name, obs_month) |>
      mutate(
        fcs_scaled = fcs / 112,
        #fcs_scaled_raw = fcs_raw / 112,
        poor = fcs < THRESHOLD,
        #poor_raw = fcs_raw < THRESHOLD2,
        #borderline = fcs < THRESHOLD2,
        #borderline_raw = fcs_raw < THRESHOLD,
        week_idx = as.integer(as.factor(obs_week)),
        month_idx = as.integer(as.factor(obs_month)),
      ) |> select(
        fcs_scaled, poor,sample_source, week_idx, month_idx, adm1_name,adm2_name, modality,
        p_cell, h_cell,hhh_sex, hh_size_bucket, hhh_edu, h_water_source_2way, h_toilet_type_2way, 
        obs_date, obs_month, model
      )
  }
   df <- df |> 
     mutate(    
       # For binary
       hhh_sex = factor(hhh_sex),
       modality = factor(modality),
       h_water_source_2way = factor(h_water_source_2way, levels=c("other","improved")),
       h_toilet_type_2way = factor(h_toilet_type_2way,levels=c("unimproved","improved")),
       # For multiclass categorical
       hh_size_bucket = factor(hh_size_bucket,levels=c("1-2", "3-4", "5-6", "7+")),
       hhh_edu = factor(hhh_edu,levels=c("preprimary/other", "primary", "secondary","higher"))
     )
   df <- drop_na(df, any_of(c("hhh_sex","modality","h_water_source_2way","h_toilet_type_2way","hh_size_bucket","hhh_edu","p_cell")))
   df_region_index <-  as.data.frame((sf |>select(adm1_name,adm2_name))) |> 
     select(-geometry) |>
     mutate(adm2_index = 1:nrow(sf),
            adm1_index = as.factor(adm1_name) |> as.integer()) |> select(-adm1_name)
   
   df <- df |> left_join(df_region_index, by='adm2_name')
   
   df <- fastDummies::dummy_cols(df, 
                                 select_columns = c("modality", 
                                                    "hhh_sex","hh_size_bucket","hhh_edu",
                                                    "h_water_source_2way","h_toilet_type_2way"),
                                 remove_first_dummy = F)
   return(df)
 }

df <- processing_for_mlr(bind_rows(df_mVAM,df_zimvac23|>mutate(model='joint')), type="observation")
df_pf <-  processing_for_mlr(popframe_agg, type="post-strat")

# save 
# [1] dataset for mobile phone only model
# [2] dataset for joint model
# [3] post-stratification cells & weights
df_mp <- df |> filter(model =='mobile-phone') |> select(-model)
df_mp$obs_month |> unique()
df_joint <- df |> filter(model == 'joint') |> select(-model)
write_rds(df_mp, here("data", "processed", "df_model_mp_only.rds"))
write_rds(df_joint, here("data", "processed", "df_model_joint.rds"))
write_rds(df_pf, here("data", "processed", "df_pf.rds"))
