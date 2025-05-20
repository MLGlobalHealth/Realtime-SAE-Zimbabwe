## Install and load required packages
rq_packages <- c(
  "here",
  "sf",
  "spdep",
  "readr",
  "readxl",
  "tidyverse",
  "purrr",
  "INLA",
  "cmdstanr",
  "DescTools",
  "ggplot2",
  "ggrepel",
  "ggpubr"
)
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list = c("rq_packages", "installed_packages"))

# read helper functions
source(here("R", "helper_functions.R"))

# Choose joint/or mobile-phone only
mode = "joint"
# Choose stan model
selected_model <- "model1"

# Read data
sf <- read_rds(here("data", "shapefile.rds"))
df_model <- read_rds(here("data", "processed", "df_model_", mode, ".rds"))

# process data
adj_mat <- nb2mat(poly2nb(sf, row.names = sf$adm2_name), style = "B")
prep <- prepare_bym2(adj_mat)

# df_region_index <-  as.data.frame((sf |>select(adm1_name,adm2_name))) |>
#   select(-geometry) |>
#   mutate(adm2_index = 1:nrow(sf),
#          adm1_index = as.factor(adm1_name) |> as.integer()) |> select(-adm1_name)
#
# df_region_month_unique <- df_model |> select(adm1_name,adm1_index, adm2_index,adm2_name, obs_month, month_idx) |> unique()

##---   Choose covariates
variables.interaction <- c(
  "hhh_sex_F",
  "hh_size_bucket_3-4",
  "hh_size_bucket_5-6",
  "hh_size_bucket_7+",
  "hhh_edu_primary",
  "hhh_edu_secondary",
  "hhh_edu_higher",
  "h_water_source_2way_improved",
  "h_toilet_type_2way_improved"
)
if (mode == "joint") {
  variables.toinclude <- c('p_cell', 'modality_f2f', variables.interaction)
  for (var in variables.interaction) {
    int_var_name <- paste0('modality_f2f:', var)
    variables.toinclude <- c(variables.toinclude, int_var_name)
  }
} else {
  variables.toinclude <- c('p_cell', variables.interaction)
}

##---  Choose model
params <- c(
  'b0',
  'betas',
  'sigma_phi',
  'sigma_theta',
  'sigma_nu',
  'sigma_xi',
  'sigma_psi'
)
params_all <- c(
  'b0',
  paste0('betas', as.character(1:length(variables.toinclude))),
  'sigma_phi',
  'sigma_theta',
  'sigma_nu',
  'sigma_xi',
  'sigma_psi'
)

# Run stan
stanfile <- here("Stan", paste0(selected_model, ".stan"))
mod <- cmdstan_model(stanfile)

data_list = list(
  NR = (df$adm1_name |> unique() |> length()),
  NS = (df$adm2_name |> unique() |> length()),
  NT = (df$month_idx |> unique() |> length()),
  N = nrow(df),
  B = length(variables.toinclude),
  y = df$poor,
  W = adj_mat,
  X = df_model[, variables.toinclude],
  admin2 = df$adm2_index,
  admin1 = df$adm1_index,
  time = df$month_idx,
  N_edges = length(prep$n1),
  node1 = prep$n1,
  node2 = prep$n2,
  scaling_factor = prep$scaling_factor
)

fit <- mod$sample(
  data = data_list,
  seed = 1234,
  iter_warmup = 500,
  iter_sampling = 1000,
  save_warmup = TRUE,
  chains = 4,
  parallel_chains = 4,
  refresh = 50
)

fit$save_output_files(
  dir = here('outputs', 'mcmc'),
  basename = paste0("stan_mcmc_", mode, "_", selected_model),
  timestamp = FALSE
)
csv_files <- fit$output_files()
write_rds(
  csv_files,
  here(
    'output',
    'mcmc',
    paste0('cmdstanr_csv_file_names_', mode, "_", selected_model, '.rds')
  )
)

# Check traceplots etc
post_samples = fit$draws(format = "df", inc_warmup = F)
post_samples$chain = as.character(post_samples$.chain)
colnames(post_samples) <- colnames(post_samples) %>%
  gsub("\\[", "", .) %>%
  gsub("\\]", "", .) %>%
  gsub(",", "_", .)
# hyper-parameters
for (param in c(
  'sigma_phi',
  'sigma_theta',
  'sigma_nu',
  'sigma_xi',
  'sigma_psi'
)) {
  dat = post_samples
  gg = ggplot(
    data = dat,
    aes_string(x = ".iteration", y = param, color = "chain")
  ) +
    geom_line() +
    theme_minimal()
  print(gg)
}
# betas
for (param in colnames(post_samples)[grepl('betas', colnames(post_samples))]) {
  gg = ggplot(
    data = post_samples,
    aes_string(x = ".iteration", y = param, color = "chain")
  ) +
    geom_line() +
    theme_minimal()
  print(gg)
}
## summary
df_result = fit$draws(format = "df", variables = params, inc_warmup = F) |>
  select(-.chain, -.iteration, -.draw)
res_table = cbind(
  colMeans(df_result),
  t(apply(df_result, 2, quantile, c(0.025, 0.975)))
)
colnames(res_table)[1] = 'mean'
rownames(res_table) = c('Intercept', variables.toinclude, params[3:7])
knitr::kable(res_table |> round(3), format = "markdown")
