## Script to
## -- census margins
## -- process data extracted from census report
## -- process DHS data
## -- down load and process shape file

rq_packages <- c(
  "here",
  "sf",
  "readr",
  "haven",
  "labelled",
  "readxl",
  "tidyverse",
  "lubridate",
  "rdhs"
)
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list = c("rq_packages", "installed_packages"))


##################################
## Read and process DHS data #####
##################################
# read in WFP survey data to make sure ADM names match
survey <- read_csv(here("data/raw/mVAM/mVAM.csv")) %>% distinct()

survey %>% group_by(h_water_source_raw) %>% count()
survey <- survey %>%
  mutate(
    hhh_edu_4way = case_when(
      hhh_edu %in% c('preprimary/other', 'primary') ~ 'primary or less',
      T ~ hhh_edu
    )
  )
adm2_svy_raw <- survey %>% group_by(adm1_name, adm2_name) %>% count()

# Check if grouping h_toilet_type_raw into h_toilet_type_2way makes sense
table(survey$h_toilet_type_raw, survey$h_toilet_type_2way)

census22_ad2_raw <- read_excel(path = 'data/raw/n_hh_by_district_and_ward.xlsx')
census22_ad2 <- census22_ad2_raw %>%
  rename_with(.fn = str_to_lower) %>%
  rename_with(.fn = function(n) gsub(' ', '_', n)) %>%
  filter(ward != 'Total') %>%
  mutate(ward = gsub("Ward ", "", ward), adm2_name = districts) %>%
  group_by(adm2_name) %>%
  dplyr::summarize(
    average_household_size = sum(total_households * average_household_size) /
      sum(total_households),
    total_households = sum(total_households),
    total_population = sum(total_population)
  ) %>%
  ungroup()

# write to files to do manual join / cleaning of ADM2 names
write_csv(census22_ad2 %>% arrange(adm2_name), file = 'data/raw/adm2_cen.csv')
write_csv(adm2_svy_raw %>% arrange(adm2_name), file = 'data/raw/adm2_mVAM.csv')

# read in cleaned matches
census22_ad2_matched <- read_csv(file = 'data/raw/adm2_matched.csv')

# these are pulled from PDF in git repo (can't find in other format online from zimstat)
census_margins <- list(
  h_cooking_fuel_raw = data.frame(
    h_cooking_fuel_raw = c('firewood', 'none', 'solid', 'clean'),
    Freq = c(2318724, 4630, 19496, 1477601)
  ),
  h_toilet_type_raw = data.frame(
    h_toilet_type_raw = c(
      'flush',
      'ventilatedLatrine',
      'pitLatrineSlab',
      'pitLatrineNoSlab',
      'composting',
      'bucket',
      'none',
      'other'
    ),
    Freq = c(1458267, 596998, 689586, 261179, 25566, 21561, 759992, 10337)
  ),
  h_water_source = data.frame(
    h_water_source = c('piped', 'protected', 'unprotected/other'),
    Freq = c(0.296 + 0.045 + 0.004, 0.242 + 0.19, 0.222)
  ),
  hhh_sex = data.frame(hhh_sex = c('M', 'F'), Freq = c(0.606, 1 - 0.606)),
  h_internet = data.frame(h_internet = c('yes', 'no'), Freq = c(0.343, 0.657)),
  h_cell = data.frame(h_cell = c('yes', 'no'), Freq = c(0.874, 1 - 0.874)),
  h_computer_laptop = data.frame(
    h_computer_laptop = c('yes', 'no'),
    Freq = c(0.113, 1 - 0.113)
  ),
  h_landline = data.frame(
    h_landline = c('yes', 'no'),
    Freq = c(0.029, 1 - 0.029)
  ),
  h_radio = data.frame(h_radio = c('yes', 'no'), Freq = c(0.362, 1 - 0.362)),
  h_tv = data.frame(h_tv = c('yes', 'no'), Freq = c(0.326, 1 - 0.326)),
  adm2_name = census22_ad2_matched %>%
    dplyr::select(
      adm1_name,
      adm2_name = adm2_name_svy,
      Freq = total_households
    ),
  adm1_name = census22_ad2_matched %>%
    group_by(adm1_name) %>%
    dplyr::summarize(Freq = sum(total_households))
)

census_margins$h_cooking_fuel <- census_margins$h_cooking_fuel_raw %>%
  mutate(
    h_cooking_fuel = case_when(
      h_cooking_fuel_raw == 'clean' ~ 'clean',
      T ~ 'other'
    )
  ) %>%
  group_by(h_cooking_fuel) %>%
  dplyr::summarize(Freq = sum(Freq))


census_margins$h_toilet_type <- census_margins$h_toilet_type_raw %>%
  mutate(
    h_toilet_type = case_when(
      h_toilet_type_raw %in% c('flush') ~ 'modernWaterCloset',
      h_toilet_type_raw %in% c('ventilatedLatrine') ~
        'ventilatedBuildInLatrine',
      T ~ h_toilet_type_raw
    )
  ) %>%
  group_by(h_toilet_type) %>%
  dplyr::summarize(Freq = sum(Freq))

census_margins$h_toilet_type_3way <- census_margins$h_toilet_type_raw %>%
  mutate(
    h_toilet_type_3way = case_when(
      h_toilet_type_raw %in%
        c('flush', 'ventilatedLatrine', 'pitLatrineSlab', 'composting') ~
        'improved',
      h_toilet_type_raw %in% c('pitLatrineNoSlab', 'bucket', 'none') ~
        'unimproved',
      h_toilet_type_raw == 'other' ~ 'other'
    )
  )

census_margins$h_toilet_type_2way <- census_margins$h_toilet_type_raw %>%
  mutate(
    h_toilet_type_2way = case_when(
      h_toilet_type_raw %in%
        c('flush', 'ventilatedLatrine', 'pitLatrineSlab', 'composting') ~
        'improved',
      T ~ 'unimproved'
    )
  )


census_margins$h_water_source_2way <- census_margins$h_water_source %>%
  mutate(
    h_water_source_2way = case_when(
      h_water_source %in% c('protected', 'piped') ~ 'improved',
      T ~ 'other'
    )
  )

census_margins %>%
  toJSON() %>%
  write_json(path = 'data/processed/census22.json')
write_rds(census_margins, here("data", "processed", "census22.rds"))

##################################
## Download and process DHS data #
##################################
# Change the configuration below
set_rdhs_config(
  email = "XXX@YYY.com",
  project = "XXXXXXXXX",
  config_path = "rdhs.json",
  global = FALSE
)
downloads_geo <- get_datasets("ZWGE72FL") # alternatively, download `ZWGE72FL.shp` and run ge <- readRDS(here("dhs/ZWGE72FL.shp"))
ge <- readRDS(downloads_geo$ZWGE72FL)
sf <- read_rds(here("data", "processed", "shapefile.rds"))

ge <- as.data.frame(ge)
ge <- sf::st_as_sf(
  ge,
  coords = c("LONGNUM", "LATNUM"),
  remove = FALSE,
  crs = 4326
)
ge_merged <- sf::st_join(ge, sf)

# Household recode
downloads_hh <- get_datasets("ZWPR72FL")
zim <- readRDS(downloads_hh$ZWPR72FL)
zim <- zim |>
  left_join(
    ge_merged |> select(hv001 = DHSCLUST, adm1_name, adm2_name),
    by = "hv001"
  )
table(zim$adm2_name)
saveRDS(zim, here("data/processed/DHS-with-admin2.rds"))

# Indivtes# Individual recode
downloads_ind <- get_datasets("ZWIR72FL")
zim_ind <- readRDS(downloads_ind$ZWIR72FL)
zim_ind <- zim_ind |>
  left_join(
    ge_merged |> select(v001 = DHSCLUST, adm1_name, adm2_name),
    by = "v001"
  )
saveRDS(zim_ind, here("data/processed/DHS-ind-with-admin2.rds"))

dhs_interviewer_raw <- read_sav(here('data/raw/DHS/ZWFW71FL.SAV'))
dhs_hh_adm2 <- readRDS(file = 'data/processed/DHS-with-admin2.rds') %>%
  select(-geometry) %>%
  as_tibble() #%>% select(hhid, adm2_name)

dhs <- dhs_hh_adm2 %>%
  rename(
    hhh_sex = hv219,
    hhh_age = hv220,
    hh_size = hv009,
    h_phone = hv221,
    h_cell = hv243a,
    h_num_child_under_5 = hv014,
    urbanicity = hv025,
    urbanicity_detailed = hv026,
    hh_weight = hv005
  ) %>%
  mutate(
    urbanicity = case_when(urbanicity == 1 ~ 'urban', T ~ 'rural'),
    urbanicity_detailed = case_when(
      urbanicity_detailed == 0 ~ 'capital, large city',
      urbanicity_detailed == 1 ~ 'small city',
      urbanicity_detailed == 2 ~ 'town',
      urbanicity_detailed == 3 ~ 'countryside',
      T ~ 'rural'
    ),
    adm1_name = str_to_title(adm1_name),
    adm2_name = case_when(
      adm2_name == 'Shurugwi Urban' ~ 'Shurugwi Town',
      T ~ adm2_name
    ),
    hhh_edu = case_when(
      hv106 == 3 ~ 'higher',
      hv106 == 2 ~ 'secondary',
      hv106 == 1 ~ 'primary',
      T ~ 'preprimary/other'
    ),
    h_water_source = case_when(
      hv201 %in% c(10, 11, 12, 13, 14) ~ 'piped',
      hv201 %in% c(21, 31, 41) ~ 'protected',
      T ~ 'unprotected/other'
    ),
    h_water_source_2way = case_when(
      h_water_source %in% c('protected', 'piped') ~ 'improved',
      T ~ 'other'
    ),
    h_toilet_type = case_when(
      hv205 %in% c(10, 11, 12, 13, 14, 15) ~ 'modernWaterCloset',
      hv205 %in% c(21) ~ 'ventilatedBuildInLatrine',
      hv205 == 22 ~ 'pitLatrineWithSlab',
      hv205 == 23 ~ 'pitLatrineWithNoSlab',
      hv205 == 31 ~ 'none',
      hv205 == 42 ~ 'bucket',
      hv205 == 96 ~ 'other'
    ),
    h_toilet_type_3way = case_when(
      h_toilet_type %in%
        c(
          'modernWaterCloset',
          'ventilatedBuildInLatrine',
          'pitLatrineWithSlab'
        ) ~
        'improved',
      h_toilet_type %in% c('pitLatrineWithNoSlab', 'none', 'bucket') ~
        'unimproved',
      h_toilet_type == 'other' ~ 'other'
    ),
    h_toilet_type_2way = case_when(
      h_toilet_type_3way == 'improved' ~ 'improved',
      T ~ 'unimproved'
    ),
    hhh_sex = ifelse(hhh_sex == 1, 'M', 'F'),
    h_cooking_fuel = case_when(
      hv226 %in% c(1, 2, 3, 4, 5) ~ 'clean',
      T ~ 'other'
    ),
    hh_size_bucket = case_when(
      hh_size <= 2 ~ '1-2',
      hh_size <= 4 ~ '3-4',
      hh_size <= 6 ~ '5-6',
      T ~ '7+'
    ),
    h_cell = case_when(h_cell == 1 ~ 'yes', T ~ 'no')
  ) %>%
  filter(hv101 == 1) %>%
  select(
    hhid,
    adm1_name,
    adm2_name,
    hh_size,
    hh_size_bucket,
    h_cooking_fuel,
    h_water_source,
    h_water_source_2way,
    h_toilet_type,
    h_toilet_type_3way,
    h_toilet_type_2way,
    hhh_sex,
    hhh_age,
    hhh_edu,
    h_phone,
    h_cell,
    urbanicity,
    urbanicity_detailed,
    h_num_child_under_5,
    hh_weight,
    hv226,
    hv201,
    hv205,
    hv106
  ) %>%
  distinct()

write_csv(dhs, 'data/processed/dhs.csv')

##################################
## Process MICS data #############
##################################
mics_data_dir <- 'data/raw/MICS6'
mics_ind_raw = read_sav(file.path(mics_data_dir, 'hl.sav'))
mics_hh_raw = read_sav(file.path(mics_data_dir, 'hh.sav'))

mics_ind_raw %>%
  group_by(ED5A, ED5B) %>%
  count() %>%
  print(n = 100)

# recode individual-level file to match survey data
mics_ind <- mics_ind_raw %>%
  filter(HL1 == 1) %>% #filter to only head of household
  dplyr::rename(age = HL6, sex_raw = HL4, urbanicity = HH6, adm1_name = HH7) %>%
  mutate(
    hhh_sex = case_when(sex_raw == 1 ~ 'M', T ~ 'F'),
    hhh_edu = case_when(
      ED5A %in% c(0, 98, 99) ~ 'preprimary/other',
      ED5A %in% c(1) ~ 'primary',
      ED5A %in% c(2, 3, 4, 5, 6) ~ 'secondary',
      ED5A %in% c(7, 8, 9, 10) ~ 'higher',
      T ~ 'preprimary/other'
    ),
    hhh_edu_3way = case_when(
      hhh_edu %in% c('preprimary/other', 'primary') ~ 'primary or less',
      T ~ hhh_edu
    ),
    urbanicity = tolower(to_factor(urbanicity)),
    adm1_name = to_factor(adm1_name)
  ) %>%
  dplyr::select(
    HH1,
    HH2,
    HL1,
    age,
    sex_raw,
    ED5A,
    urbanicity,
    hhh_edu,
    hhh_edu_3way,
    hhh_sex,
    adm1_name
  )

mics_ind %>%
  group_by(hhh_edu) %>%
  dplyr::count()

mics_hh_raw %>%
  group_by(EU4) %>%
  dplyr::count() %>%
  print(n = 100)

# recode hh
mics_hh <- mics_hh_raw %>%
  dplyr::rename(
    h_interview_language = HH15,
    h_religion = HC1A,
    hh_size = HH48,
    h_landline = HC7A,
    h_radio = HC7B,
    h_electricity = HC8,
    h_tv = HC9A,
    h_computer = HC11,
    h_cell = HC12,
    h_internet = HC13,
    weight_hh = hhweight
  ) %>%
  mutate(
    h_interview_language = to_factor(h_interview_language),
    h_landline = case_when(h_landline == 1 ~ 'yes', h_landline == 2 ~ 'no'),
    h_radio = case_when(h_radio == 1 ~ 'yes', T ~ 'no'),
    h_electricity = case_when(
      h_electricity %in% c(1, 2) ~ 'yes',
      h_electricity == 3 ~ 'no'
    ),
    h_tv = case_when(h_tv == 1 ~ 'yes', T ~ 'no'),
    h_computer = case_when(h_computer == 1 ~ 'yes', h_computer == 2 ~ 'no'),
    h_cell = case_when(h_cell == 1 ~ 'yes', T ~ 'no'),
    h_internet = case_when(h_internet == 1 ~ 'yes', T ~ 'no'),
    h_toilet_type = case_when(
      WS11 %in% c(11, 12, 13, 14, 18) ~ 'modernWaterCloset',
      WS11 == 21 ~ 'ventilatedBuildInLatrine',
      WS11 %in% c(22, 23) ~ 'outdoorsLatrineHole',
      T ~ 'other'
    ),
    h_toilet_type_3way = case_when(
      h_toilet_type %in% c('outdoorsLatrineHole') ~ 'unimproved',
      h_toilet_type %in% c('ventilatedBuildInLatrine', 'modernWaterCloset') ~
        'improved',
      T ~ 'other'
    ),
    h_toilet_type_2way = case_when(
      h_toilet_type %in% c('ventilatedBuildInLatrine', 'modernWaterCloset') ~
        'improved',
      T ~ 'unimproved'
    ),
    h_water_source = case_when(
      WS1 %in% c(11, 12, 13, 14) ~ 'piped',
      WS1 %in% c(21, 31, 41) ~ 'protected',
      T ~ 'unprotected/other'
    ),
    h_water_source_2way = case_when(
      h_water_source %in% c('protected', 'piped') ~ 'improved',
      T ~ 'other'
    ),
    h_cooking_fuel = case_when(EU4 %in% c(1, 2, 3) ~ 'clean', T ~ 'other'),
    hh_size_bucket = case_when(
      hh_size <= 2 ~ '1-2',
      hh_size <= 4 ~ '3-4',
      hh_size <= 6 ~ '5-6',
      T ~ '7+'
    )
  ) %>%
  dplyr::select(
    HH1,
    HH2,
    h_interview_language,
    h_landline,
    h_radio,
    h_electricity,
    h_tv,
    h_computer,
    h_cell,
    h_internet,
    h_toilet_type,
    h_toilet_type_3way,
    h_toilet_type_2way,
    h_water_source,
    h_water_source_2way,
    h_cooking_fuel,
    hh_size,
    hh_size_bucket,
    weight_hh,
    EU4,
    WS1,
    WS11
  )
mics_hh %>%
  group_by(hh_size_bucket) %>%
  summarize(weight_hh = sum(weight_hh)) %>%
  ungroup() %>%
  mutate(weight_hh / sum(weight_hh))

# merge individ and hh data
mics <- left_join(mics_hh, mics_ind, by = c('HH1', 'HH2')) %>%
  filter(weight_hh != 0) %>%
  mutate(
    hhh_adm1_by_edu_by_sex = paste0(adm1_name, ' x ', hhh_edu, ' x ', hhh_sex)
  )

# save MICS data
write_csv(mics, here('data/processed/mics.csv'))
