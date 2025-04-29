## Install and load required packages
rq_packages <- c(
  "here", "sf",  "spdep", "lubridate",
  "readr","readxl","tidyverse", "purrr",
  "INLA",  "cmdstanr", "DescTools",
  "ggplot2", "ggrepel","ggpubr",
  "scoringutils", "epiR"
)
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))

# read helper functions
source(here("R","helper_functions.R"))
# -------------------
# Compute direct estimates
# -------------------
sf <- readRDS(here("data", "processed", "shapefile.rds"))
# - ZimVAC
zimvac23 <- read.csv(here("data", "processed", "zimvac2023_survey.csv")) |> select(adm1_name, adm2_name, obs_month,fcs)
zimvac24 <- read.csv(here("data", "processed", "zimvac2024_survey.csv")) |> select(adm1_name, adm2_name, obs_month,fcs)
zimvac <- bind_rows(zimvac23,zimvac24) |> 
  mutate(poor = as.numeric(fcs < 28 ))
df_zimvac_est <- zimvac |> group_by(adm1_name,adm2_name, obs_month) |>
  summarise(
    zimvac_est = mean(poor, na.rm = T),
    n = n(),
    zimvac_est_se = sqrt(zimvac_est*(1-zimvac_est)*(1/n)),
  )
rural_adm2 = df_zimvac_est$adm2_name[df_zimvac_est$obs_month==as.POSIXct(c("2023-05-01"), format = "%Y-%m-%d", tz="UTC")] |> unique() 
write_rds(df_zimvac_est, here("data", "processed", "df_zimvac_est.rds"))

# - mVAM
# read processed mVAM data, weights and compute weighted mean
df_model <- readRDS(here("data", "processed", "df_model_joint.rds")) 
df_region_month_unique <- df_model |> select(adm1_name,adm1_index, adm2_index,adm2_name, obs_month, month_idx) |> unique()
df_weights <- readRDS(here("data", "processed", "mVAM_sample_weights.rds"))
df_weights <- df_weights |> filter(adm1_name%in%unique(df_model$adm1_name)) |> 
  mutate(
    adm1_name = factor(adm1_name, levels = levels(df_model$adm1_name))
  )

df_direct_est_prep <- df_model |>
  filter(sample_source%in% c("RDD","GeoPoll"))

df_direct_est <- df_direct_est_prep |> left_join(
  (df_direct_est_prep |>
     left_join(df_weights, by = c('obs_month','adm1_name','hhh_edu')) |>
     group_by(adm2_name, obs_month) |>
     reframe(hhh_edu = hhh_edu,
             dmg_weight_norm = dmg_weight_ust/sum(dmg_weight_ust)
     )|> unique()),
  by = c("obs_month","adm2_name","hhh_edu"))|>
  group_by(adm1_name, adm2_name, obs_month) |>
  summarise(
    y_est = weighted.mean(poor,dmg_weight_norm, na.rm = T),
    w_sq_sum = sum(dmg_weight_norm^2),
    y_est_se = sqrt(y_est * (1-y_est) * w_sq_sum),
    n_w = 1/w_sq_sum,
  ) |> select(-w_sq_sum)
write_rds(df_direct_est, here("data", "processed", "df_direct_est.rds"))

# -----------------------------------------------------------------------
#
coverage_level = c(0.8,0.9,0.95)
df_zimvac_est <- process_zimvac(df_zimvac_est, coverage_level)
df_zimvac_est$obs_month = as.POSIXct(df_zimvac_est$obs_month,format = "%Y-%m-%d", tz = "UTC")
df_mVAM_est <- process_direct(df_direct_est, coverage_level)
df_mVAM_est <- df_mVAM_est |> mutate(type = 'Direct (mobile-phone)', model = '')

# -----
# choose only wilson interval
df_mVAM_est_wald <- df_rtm_est |> ungroup() |> select(-starts_with("wald_"), -n_w, - y_est_se)
colnames(df_mVAM_est_wald) <-sub("wilson_","",colnames(df_mVAM_est_wald))

df_zimvac_est_wald <- df_zimvac_est |> ungroup() |> select(-starts_with("zv_wald_"), -n, - zimvac_est_se)
colnames(df_zimvac_est_wald) <-sub("wilson_","",colnames(df_zimvac_est_wald))

# -----------------------------------------------------------------------
# Modelled estimates and further processing
selected_model = 'model1'
df_region_unique <- df_model|>  select(adm1_name,adm1_index, adm2_index,adm2_name) |> unique()
df_month_unique <- df_model|> select(obs_month, month_idx) |> unique()
# Read MR / MRP (dhs ) rtm only
type = 'y' # 'y' or 'p'
df_mr <- read.csv(here('output',paste0('post_sample_mr_',type,'hat_rtmonly_',selected_model,'.csv'))) |> select(-X)
df_mr_est <- process_ps(df_mr,coverage_level)
df_mr_est <- df_mr_est |> mutate(type = 'MR', model = 'mobile-phone-only')
rm(df_mr)
df_mrp_dhs <- read.csv(here('output',paste0('post_sample_mrp_dhs_',type,'hat_rtmonly_',selected_model,'.csv')))
df_mrp_dhs_est <- process_ps(df_mrp_dhs,coverage_level)
df_mrp_dhs_est <- df_mrp_dhs_est |>  mutate(type = 'MRP (dhs)', model = 'mobile-phone-only')
rm(df_mrp_dhs)

# -----------------------------------
# read MR / MRP (dhs / zimvac) joint 
df_mr <- read.csv(here('output',paste0('post_sample_mr_modalityMP_',type,'hat_joint_',selected_model,'.csv'))) 
df_mr_joint_est <- process_ps(df_mr,coverage_level)
df_mr_joint_est <- df_mr_joint_est |> mutate(type = 'MR-modalityPhone', model = 'joint')
rm(df_mr)
df_mr <- read.csv(here('output',paste0('post_sample_mr_modalityf2f_',type,'hat_joint_',selected_model,'.csv'))) 
df_mr_joint_f2f_est <- process_ps(df_mr,coverage_level)
df_mr_joint_f2f_est <- df_mr_joint_f2f_est |> mutate(type = 'MR-modalityF2F', model = 'joint')
rm(df_mr)

df_mrp_dhs <- read.csv(here('output',paste0('post_sample_mrp_dhs_',type,'hat_joint_',selected_model,'.csv')))
df_mrp_dhs_joint_est <- process_ps(df_mrp_dhs,coverage_level)
df_mrp_dhs_joint_est <- df_mrp_dhs_joint_est |> mutate(type = 'MRP (dhs)', model = 'joint')
rm(df_mrp_dhs)
df_mrp_zimvac <- read.csv(here('output',paste0('post_sample_mrp_zimvac_',type,'hat_joint_',selected_model,'.csv')))
df_mrp_zimvac_joint_est <- process_ps(df_mrp_zimvac,coverage_level)
df_mrp_zimvac_joint_est <- df_mrp_zimvac_joint_est |> mutate(type = 'MRP (zimvac)', model = 'joint')
rm(df_mrp_zimvac)


#### 
months_of_interest <- as.POSIXct(c("2023-05-01","2024-05-01"), format = "%Y-%m-%d", tz = "UTC")
#### 
df_combined <- bind_rows(df_mVAM_est_wald,
                         df_mr_est,df_mrp_dhs_est,
                         df_mr_joint_est,df_mr_joint_f2f_est,
                         df_mrp_dhs_joint_est,df_mrp_zimvac_joint_est
                         )

df_combined <- df_combined |> mutate(type_model = paste(type,model),obs_month = as.POSIXct(obs_month,format = "%Y-%m-%d", tz = "UTC")) |>
  left_join(df_zimvac_est_wald |> mutate(obs_month = as.POSIXct(obs_month,format = "%Y-%m-%d", tz = "UTC")), 
            by = c('adm1_name','adm2_name','obs_month'))
df_validation <- df_combined |> filter(obs_month%in%months_of_interest) 

for (q in coverage_level){
  df_validation[paste0("ci_length",as.character(q*100))] = df_validation[paste0("high",as.character(q*100))] - df_validation[paste0("low",as.character(q*100))]
  df_validation[paste0("coverage",as.character(q*100))] = (pmax(df_validation[[paste0("zv_high",as.character(q*100))]], df_validation[[paste0("high",as.character(q*100))]]) - pmin(df_validation[[paste0("zv_low",as.character(q*100))]], df_validation[[paste0("low",as.character(q*100))]])) < ((df_validation[[paste0("zv_high",as.character(q*100))]] - df_validation[[paste0("zv_low",as.character(q*100))]]) + (df_validation[[paste0("high",as.character(q*100))]] - df_validation[[paste0("low",as.character(q*100))]]))
}

df_validation <- df_validation |> 
  mutate(
    bias = y_est - zimvac_est,
    abserror = abs(bias),
    sqerror = (bias^2)
  ) 
df.cis <- df_validation |> select(starts_with("ci_length"),starts_with("coverage"),starts_with("int_score"),type_model, type, model, obs_month)|>
  group_by(type_model, type, model, obs_month) |>
  summarise_each(~ mean(.,na.rm = T)) |> ungroup() |> select(starts_with("ci_length"),starts_with("coverage"),starts_with("int_score"))
for (q in coverage_level){
  df.cis[paste0('coverage_deviation', as.character(q*100))] = df.cis[[paste0('coverage', as.character(q*100))]] - q
}
df_validation_res <- df_validation |>
  group_by(type_model, type, model, obs_month) |>
  summarise(
    pearson = cor(y_est, zimvac_est, method = "pearson"),
    spearman = cor(y_est, zimvac_est, method = "spearman"),
    ccc = epi.ccc(zimvac_est, y_est, ci="z-transform", conf.level=0.9)$rho.c,
    bias = mean(bias),
    MAE = mean(abserror),
    rMSE = sqrt(mean(sqerror))
  ) |> ungroup() |> bind_cols(df.cis)
df_validation_res$type = factor(df_validation_res$type, levels = c("Direct (mobile-phone)","MR","MR-modalityPhone","MR-modalityF2F","MRP (dhs)","MRP (zimvac)"))
df_validation_res$model = factor(df_validation_res$model, levels = c("","mobile-phone-only","joint"))
df_validation_res <- df_validation_res |> arrange(obs_month,model,type) 

d <- df_validation_res |> 
  mutate(
    month = factor(obs_month,  levels=c("2023-05-01","2024-05-01"), labels=c("2023 May","2024 May")),
    method = factor(type_model,levels = c("Direct (mobile-phone) ","MR mobile-phone-only","MRP (dhs) mobile-phone-only","MR-modalityPhone joint","MR-modalityF2F joint","MRP (dhs) joint","MRP (zimvac) joint"),
                    labels = c("mVAM direct", "MR mVAM only", "MRP mVAM only", "MR(phone) joint","MR(F2F) joint", "MRP(dhs) joint","MRP(zimvac) joint")),
    type = factor(type,levels = c("Direct (mobile-phone)", "MR", "MR-modalityPhone", "MR-modalityF2F", "MRP (dhs)", "MRP (zimvac)"),
                  labels = c('mVAM direct',"MR", "MR-modality=mobile phone", "MR-modality=F2F", "MRP (dhs)", "MRP (zimvac)")),
    type_model = factor(type_model,levels = c("Direct (mobile-phone) ","MR mobile-phone-only","MRP (dhs) mobile-phone-only","MR-modalityPhone joint","MR-modalityF2F joint","MRP (dhs) joint","MRP (zimvac) joint"),
                        labels = c("mVAM direct", "MR", "MRP", "jMR-MP","jMR-F2F", "jMRP (DHS-census)","jMRP")),
    ccc = ccc$est) |>
  select(month, type_model,  model, type, pearson, spearman,ccc, rMSE, MAE, bias, coverage80, ci_length80,coverage90, ci_length90) 
d[,4:ncol(d)] <- round(d[,4:ncol(d)],3)
d <- d[,c(2,1,3:ncol(d))] |> arrange(month, model, type) 

d |>select(month,  model, type, pearson, spearman,ccc, rMSE,MAE, bias) |>knitr::kable()
d |>select(month,  model, type, coverage80, ci_length80, coverage90, ci_length90) |>knitr::kable()
d_long = d |> filter(type_model%in%c("mVAM direct", "MRP", "jMR-MP","jMR-F2F","jMRP")) |>
  select(month, type_model, spearman,ccc, MAE, bias, coverage90, ci_length90) |>
  pivot_longer(cols=spearman:ci_length90 ,names_to = "metric", values_to = "value")|>
  arrange(metric, month,type_model) 

# satter plots
df_plot_scatter <- df_combined |>
  mutate(month = case_when(obs_month==as.POSIXct('2023-05-01', tz='GMT')~"May 2023", T~"May 2024"))
library(cowplot)
library(grafify)
p1 <- df_plot_scatter |> 
  filter(obs_month==as.POSIXct('2023-05-01', tz='GMT')) |>
  ggplot(aes(x = y_est, y = zimvac_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point() +
  facet_wrap(~ factor(type_model,levels = c("Direct (mobile-phone) ","MR mobile-phone-only","MRP (dhs) mobile-phone-only","MR-modalityPhone joint","MR-modalityF2F joint","MRP (dhs) joint","MRP (zimvac) joint"),
                      labels = c("mVAM direct", "MR", "MRP", "jMR-MP","jMR-F2F", "jMRP (DHS-census)","jMRP (ZimVAC)")),
             ncol = 4)+
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(
    x = "",
    y = paste0("ZimVAC estimates")
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30)
  ) 
p1

p2 <- df_plot_scatter |> 
  filter(obs_month==as.POSIXct('2024-05-01', tz='GMT')) |>
  ggplot(aes(x = y_est, y = zimvac_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point() +
  facet_wrap(~ factor(type_model,levels = c("Direct (mobile-phone) ","MR mobile-phone-only","MRP (dhs) mobile-phone-only","MR-modalityPhone joint","MR-modalityF2F joint","MRP (dhs) joint","MRP (zimvac) joint"),
                      labels = c("mVAM direct", "MR", "MRP", "jMR-MP","jMR-F2F", "jMRP (DHS-census)","jMRP (ZimVAC)")),
             ncol = 4)+
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(
    x = "",
    y = paste0("ZimVAC estimates")
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30)
  ) 
p2
p = plot_grid(p1, p2, labels = c('May 2023', 'May 2024'),  ncol = 1, label_size = 12)
p
ggsave(here(paste0("output/corr-plot-validation.png")), p, width = 8, height = 10,bg = "white")


# - spatial-plot ---
df_plot <- bind_rows(df_rtm_est,df_mr_est,df_mrp_dhs_est,
                     df_mr_joint_est,df_mr_joint_f2f_est,df_mrp_dhs_joint_est,df_mrp_zimvac_joint_est
) |> mutate(type_model = paste(type,model)) |> 
  filter(obs_month%in%as.POSIXct(c('2023-05-01',"2024-05-01"),tz = "UTC")) 
df_plot_zimvac <- df_zimvac_est_wald
colnames(df_plot_zimvac)= colnames(df_plot)[1:ncol(df_plot_zimvac)] 
df_plot_zimvac <- df_plot_zimvac |> mutate(
  type = "Direct (Zimvac)",
  model = "",
  type_model = paste(type, model)
)
df_plot <- bind_rows(df_plot, df_plot_zimvac |> filter(obs_month%in%as.POSIXct(c('2023-05-01',"2024-05-01"),tz = "UTC")))
df_plot <- df_plot |> left_join(sf|> select(-adm1_name), by ='adm2_name') |> st_as_sf()  |>
  mutate(month = case_when(obs_month==as.POSIXct('2023-05-01', tz='GMT')~"May 2023", T~"May 2024"))

pp1 = df_plot |> 
  filter(obs_month==as.POSIXct('2023-05-01', tz='GMT')) |>
  ggplot( aes(fill = y_est)) +
  geom_sf() +
  
  facet_wrap(~ factor(type_model,levels = c("Direct (mobile-phone) ","MR mobile-phone-only","MRP (dhs) mobile-phone-only","MR-modalityPhone joint","MR-modalityF2F joint","MRP (dhs) joint","MRP (zimvac) joint","Direct (Zimvac) "),
                      labels = c("mVAM direct", "MR", "MRP", "jMR-MP","jMR-F2F", "jMRP (DHS-census)","jMRP (ZimVAC)", "ZimVAC direct")), ncol =4)+
  scale_fill_gradientn(values = c("1","0.8","0.6","0.4","0.3","0.2","0.1","0.05","0"), 
                       colors = c("#9E3D21","#BE4E21","#DA6524","#EF8530","#F0AC72","#D8D4C9","#5789B6","#4071A0","#2B5B8A"),
                       limits=c(0,1), labels=scales::percent, breaks=seq(0,1, by =0.2))+
  labs(fill = "Prevalence of households with \n poor food consumption patterns")+
  theme_void(base_size = 14) +
  theme(
    legend.position = "right",
    legend.text = element_text(color = "white",angle = 45),
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.key.size = unit(0.5, 'cm'),
    legend.key.height= unit(1,, 'cm'),
    legend.ticks.length = unit(c(-.15, 0), 'cm'), # different lengths for ticks on both sides
    legend.ticks = element_line(colour = "black"),
  ) + 
  guides(fill = guide_legend(override.aes= list(alpha = 0, color = "white"))) +
  theme(legend.key=element_rect(colour="white"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
  )

pp2 = df_plot |> 
  filter(obs_month==as.POSIXct('2024-05-01', tz='GMT')) |>
  ggplot( aes(fill = y_est)) +
  geom_sf() +
  facet_wrap(~ factor(type_model,levels = c("Direct (mobile-phone) ","MR mobile-phone-only","MRP (dhs) mobile-phone-only","MR-modalityPhone joint","MR-modalityF2F joint","MRP (dhs) joint","MRP (zimvac) joint","Direct (Zimvac) "),
                      labels = c("mVAM direct", "MR", "MRP", "jMR-MP","jMR-F2F", "jMRP (DHS-census)","jMRP (ZimVAC)", "ZimVAC direct")), ncol =4)+
  scale_fill_gradientn(values = c("1","0.8","0.6","0.4","0.3","0.2","0.1","0.05","0"), 
                       colors = c("#9E3D21","#BE4E21","#DA6524","#EF8530","#F0AC72","#D8D4C9","#5789B6","#4071A0","#2B5B8A"),
                       limits=c(0,1), labels=scales::percent,breaks=seq(0,1, by =0.2))+
  labs(fill = "Prevalence of households with \n poor food consumption patterns")+
  theme_void(base_size = 14) +
  theme(legend.position = "right",
        legend.title.position = "left",
        legend.title=element_blank(),
        legend.text = element_text(angle=45),
        legend.key.size = unit(0.5, 'cm'),
        legend.key.height= unit(1.0, 'cm'),
        legend.ticks.length = unit(c(-.15, 0), 'cm'), # different lengths for ticks on both sides
        legend.ticks = element_line(colour = "black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
  )
pp= plot_grid(pp1, pp2, labels = c('May 2023', 'May 2024'),  ncol = 1, label_size = 12)
ggsave(here(paste0("output/spatial-plots-validation.png")), pp, width = 8, height = 8.5,bg = "white")

df_plot_ci <- df_validation |>  select(adm1_name, adm2_name,type_model,obs_month,starts_with("ci_length")) |>
  filter(obs_month%in%as.POSIXct(c('2023-05-01',"2024-05-01"),tz = "UTC")) |> 
  bind_rows(df_zimvac_est_wald |>
              filter(obs_month%in%as.POSIXct(c('2023-05-01',"2024-05-01"),tz = "UTC")) |>
              mutate(
                ci_length80 = zv_high80-zv_low80,
                ci_length90 = zv_high80-zv_low90,
                ci_length95 = zv_high80-zv_low90,
                type_model = "Direct (Zimvac) ") |>
              select(adm1_name, adm2_name,type_model,obs_month,starts_with("ci_length")) 
  )
df_plot_ci <- df_plot_ci |> left_join(sf|> select(-adm1_name), by ='adm2_name') |> st_as_sf()  |>
  mutate(month = case_when(obs_month==as.POSIXct('2023-05-01', tz='GMT')~"May 2023", T~"May 2024"))

pp1 = df_plot_ci |> mutate(ci_length90=ci_length90*100)|>
  filter(obs_month==as.POSIXct('2023-05-01', tz='GMT')) |>
  ggplot( aes(fill = ci_length90), alpha=0.1) +
  geom_sf() +
  facet_wrap(~ factor(type_model,levels = c("Direct (mobile-phone) ","MR mobile-phone-only","MRP (dhs) mobile-phone-only","MR-modalityPhone joint","MR-modalityF2F joint","MRP (dhs) joint","MRP (zimvac) joint","Direct (Zimvac) "),
                      labels = c("mVAM direct", "MR", "MRP", "jMR-MP","jMR-F2F", "jMRP (DHS-census)","jMRP (ZimVAC)", "ZimVAC direct")), ncol =4)+
  scale_fill_gradientn(values = c("1","0.8","0.6","0.4","0.2","0"),
                       colors = c("red4","red3","#FC4E07","#E7B800","lightgoldenrod"),
                       limits=c(0,100),breaks=seq(0,100, by =20))+  # labs(fill = "Prevalence of households with \n poor food consumption patterns")+
  theme_void(base_size=14) +
  theme(
    legend.position = "right",
    legend.text = element_text(color = "white",angle = 45),
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.key.size = unit(0.5, 'cm'),
    legend.key.height = unit(1,'cm'),
    legend.ticks.length = unit(c(-.15, 0), 'cm'), # different lengths for ticks on both sides
    legend.ticks = element_line(colour = "black"),
  ) + 
  guides(fill = guide_legend(override.aes= list(alpha = 0, color = "white"))) +
  theme(legend.key=element_rect(colour="white"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
  )

pp2 = df_plot_ci |> mutate(ci_length90=ci_length90*100)|>
  filter(obs_month==as.POSIXct('2024-05-01', tz='GMT')) |>
  ggplot( aes(fill = ci_length90), alpha=0.1) +
  geom_sf() +
  facet_wrap(~ factor(type_model,levels = c("Direct (mobile-phone) ","MR mobile-phone-only","MRP (dhs) mobile-phone-only","MR-modalityPhone joint","MR-modalityF2F joint","MRP (dhs) joint","MRP (zimvac) joint","Direct (Zimvac) "),
                      labels = c("mVAM direct", "MR", "MRP", "jMR-MP","jMR-F2F", "jMRP (DHS-census)","jMRP (ZimVAC)", "ZimVAC direct")), ncol =4)+
  scale_fill_gradientn(values = c("1","0.8","0.6","0.4","0.2","0"),
                       colors = c("red4","red3","#FC4E07","#E7B800","lightgoldenrod"),
                       limits=c(0,100), breaks=seq(0,100,by=20))+  # labs(fill = "Prevalence of households with \n poor food consumption patterns")+

  theme_void(base_size=14) +
  theme(legend.position = "right",
        legend.title.position = "left",
        legend.title=element_blank(),
        legend.text = element_text(angle=45),
        legend.key.size = unit(0.5, 'cm'),
        legend.key.height= unit(1, 'cm'),
        legend.ticks.length = unit(c(-.15, 0), 'cm'), # different lengths for ticks on both sides
        legend.ticks = element_line(colour = "black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
  )
pp= plot_grid(pp1, pp2, labels = c('May 2023', 'May 2024'),  ncol = 1, label_size = 12)
ggsave(here(paste0("output/spatial-plots-ci90length.png")), pp, width = 8, height = 8.5,bg = "white")
