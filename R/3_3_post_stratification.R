
## Install and load required packages
rq_packages <- c(
  "here", "sf",  "spdep", 
  "readr","readxl","tidyverse", "purrr",
  "INLA",  "cmdstanr", "DescTools",
  "ggplot2", "ggrepel","ggpubr"
)
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))

# read helper functions
source(here("R","helper_functions.R"))

# Choose joint/or mobile-phone only
mode = "joint" / "mp_only"
# Choose stan model 
selected_model <- "model1"
# 
sf <- read_rds(here("data", "processed","shapefile.rds"))
df_predict <- read_rds(here("data", "processed", "df_pf.rds"))
##---   Choose covariates
get_var_names <- function(mode){
  variables.interaction <- c("hhh_sex_F",
           "hh_size_bucket_3-4", "hh_size_bucket_5-6","hh_size_bucket_7+",
           "hhh_edu_primary","hhh_edu_secondary","hhh_edu_higher",
           "h_water_source_2way_improved", "h_toilet_type_2way_improved")
  if (mode=="mp_only"){
    variables.toinclude = variables.toinclude <- c('p_cell',variables.interaction)
  } else if (mode=="joint"){
    variables.toinclude <- c('p_cell','modality_f2f',variables.interaction)
    for (var in variables.interaction){
      int_var_name <- paste0('modality_f2f:',var)
      variables.toinclude <- c(variables.toinclude,int_var_name)
    }
  } else {stop}
  return(variables.toinclude)
}
# ---------------------------
# - mobile-phone only model -
#----------------------------
mode = "mp_only"
df_model <- readRDS(here("data", "processed", paste0("df_model_",mode,".rds")))
variables.toinclude <- get_var_names(mode)
# read stanfit file
csv_files = read_rds(here('output','mcmc',paste0('cmdstanr_csv_file_names_',mode,"_",selected_model,'.rds')))
fit_mp_only <- as_cmdstan_fit(csv_files)

# MR estimate - without post-stratification
df_mr_newdata = df_model |> filter(sample_source%in%c('RDD','GeoPoll'))
df_mr_ps <- MR_post_samples(fit_rtm, df_mr_newdata, variables.toinclude,
                            response = 'binary', weights = F
)
df_mr_ps_phat  <- df_mr_ps[[1]] # not to be used, just computed for comparison
write.csv(df_mr_ps_phat, here('output',paste0('post_sample_mr_phat_',mode,'_',selected_model,'.csv')))
df_mr_ps_yhat  <-df_mr_ps[[2]]
write.csv(df_mr_ps_yhat, here('output',paste0('post_sample_mr_yhat_',mode,'_',selected_model,'.csv')))
rm(df_mr_ps_phat,df_mr_ps_yhat,df_mr_ps)

# -- MRP 
month_idx_all =   df_model$month_idx |> unique() |> as.numeric()
month_idx_list = sort(month_idx_all)#[c(33,45)]

df_mrp_dhs_newdata <- df_predict |> 
  filter(popframe_source=='dhs')

df_mrp_dhs_ps <- MRP_post_samples(fit_rtm,df_mrp_dhs_newdata,month_idx_list,
                                  variables.toinclude,
                                  response = 'binary' # "pr" / "binary" / "binomial"
)
df_mrp_dhs_ps_phat  <- df_mrp_dhs_ps[[1]]
write.csv(df_mrp_dhs_ps_phat, here('output',paste0('post_sample_mrp_dhs_phat_',mode,'_',selected_model,'.csv')),row.names = F)
df_mrp_dhs_ps_yhat  <-df_mrp_dhs_ps[[2]]
write.csv(df_mrp_dhs_ps_yhat, here('output',paste0('post_sample_mrp_dhs_yhat_',mode,'_',selected_model,'.csv')),row.names = F)
rm(df_mrp_dhs_ps_phat,df_mrp_dhs_ps_yhat,df_mrp_dhs_ps)
rm(fit_mp_only)

# ---------------------------
# - joint model -------------
#----------------------------
mode = "joint"
df_model <- readRDS(here("data", "processed", paste0("df_model_",mode,".rds")))
variables.toinclude <- get_var_names(mode)
# read stanfit file
csv_files = read_rds(here('output','mcmc',paste0('cmdstanr_csv_file_names_',mode,"_",selected_model,'.rds')))
fit_joint <- as_cmdstan_fit(csv_files)

# --- MR ---
# without post-stratification
df_mr_newdata = df_model |> filter(sample_source%in%c('RDD','GeoPoll'))
df_mr_ps <- MR_post_samples(fit_joint, df_mr_newdata, variables.toinclude,
                            response = 'binary', weights = F
)
df_mr_ps_phat  <- df_mr_ps[[1]]
write.csv(df_mr_ps_phat, here('output',paste0('post_sample_mr_modalityMP_phat_joint_',selected_model,'.csv')),row.names = F)
df_mr_ps_yhat  <-df_mr_ps[[2]]
write.csv(df_mr_ps_yhat, here('output',paste0('post_sample_mr_modalityMP_yhat_joint_',selected_model,'.csv')),row.names = F)
# Change modality
df_mr_newdata = df_model |> filter(sample_source%in%c('RDD','GeoPoll'))
df_mr_newdata$modality_f2f = 1
for (var in variables.interaction){
  int_var_name = paste0('modality_f2f:',var)
  df_mr_newdata[int_var_name] = df_mr_newdata['modality_f2f'] * df_mr_newdata[var]
}

df_mr_ps <- MR_post_samples(fit_joint, df_mr_newdata, variables.toinclude,
                            response = 'binary', weights = F
)
df_mr_ps_phat  <- df_mr_ps[[1]]
write.csv(df_mr_ps_phat, here('output',paste0('post_sample_mr_modalityf2f_phat_joint_',selected_model,'.csv')),row.names = F)
df_mr_ps_yhat  <-df_mr_ps[[2]]
write.csv(df_mr_ps_yhat, here('output',paste0('post_sample_mr_modalityf2f_yhat_joint_',selected_model,'.csv')),row.names = F)
rm(df_mr_ps_phat,df_mr_ps_yhat,df_mr_ps)
# ---------
# -- MRP --
# ---------
month_idx_all =   df_model$month_idx |> unique() |> as.numeric()
month_idx_list = sort(month_idx_all)#[c(33,45)]

df_mrp_dhs_newdata <- df_predict |> 
  filter(popframe_source=='dhs')

df_mrp_dhs_ps <- MRP_post_samples(fit_joint,df_mrp_dhs_newdata,month_idx_list,
                                  variables.toinclude,
                                  response = 'binary'
)
df_mrp_dhs_ps_phat  <- df_mrp_dhs_ps[[1]]
write.csv(df_mrp_dhs_ps_phat, here('output',paste0('post_sample_mrp_dhs_phat_joint_',selected_model,'.csv')),row.names = F)
df_mrp_dhs_ps_yhat  <-df_mrp_dhs_ps[[2]]
write.csv(df_mrp_dhs_ps_yhat, here('output',paste0('post_sample_mrp_dhs_yhat_joint_',selected_model,'.csv')),row.names = F)
df.test <- read.csv(here('output',paste0('post_sample_mrp_dhs_phat_joint_',selected_model,'.csv')))
rm(df_mrp_dhs_ps_phat,df_mrp_dhs_ps_yhat,df_mrp_dhs_ps,df.test)
## ------------------
## ---- Zimvac-------
df_mrp_zimvac_newdata <- df_predict |> 
  filter(popframe_source=='zimvac23ind')

df_mrp_zimvac_ps <- MRP_post_samples(fit_joint,df_mrp_zimvac_newdata,month_idx_list,
                                     variables.toinclude,
                                     response = 'binary'
)
df_mrp_zimvac_ps_phat  <- df_mrp_zimvac_ps[[1]]
write.csv(df_mrp_zimvac_ps_phat, here('output',paste0('post_sample_mrp_zimvac_phat_joint_',selected_model,'.csv')),row.names = F)
df_mrp_zimvac_ps_yhat  <-df_mrp_zimvac_ps[[2]]
write.csv(df_mrp_zimvac_ps_yhat, here('output',paste0('post_sample_mrp_zimvac_yhat_joint_',selected_model,'.csv')),row.names = F)
rm(df_mrp_zimvac_ps_phat,df_mrp_zimvac_ps_yhat,df_mrp_zimvac_ps)
