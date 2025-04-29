## This scripts deals with getting post-stratification cells and their weights using
### - raking dhs 2015 to census margins
### - ZimVAC 
# ----------------------
rq_packages <- c(
  "here", "haven","labelled","survey",
  "ggrepel","jsonlite","devtools","mlr3misc","glue",
  "readxl","tidyverse", "lubridate"
)
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))
# ----------------------
# dataset
dhs <- read_csv(here("data","processed","dhs.csv"))
census_margins <- read_rds(here('data/processed/census22.rds'))
zimvac23 <- read_csv(here("data","raw","ZimVAC","zimvac2023_survey.csv"))
survey <- read_csv(here("data/raw/mVAM/mVAM.csv")) %>% distinct()

## 1.1 raking (IPF) dhs -> census_margin
survey <- survey %>%
  mutate(rdd_flag = sample_source == 'RDD')

survey <- survey %>%
  mutate(hhh_edu_4way = case_when(hhh_edu %in% c('preprimary/other', 'primary') ~ 'primary or less'
                                  , T ~ hhh_edu
  )
  )
#-------------------
# rake popframe
# 1-create dummy popframe from MICS data -- add in adm2 rows
#--------------------
all_vars <- c('adm1_name'
              , 'hhh_edu', 'hhh_edu_4way'
              , 'hhh_sex'
              , 'h_toilet_type_2way'
              , 'h_water_source_2way'
              , 'hh_size_bucket'
              , 'h_cell'
              , 'h_cooking_fuel')


weightFrame <- function(raw, weighting_vars){
  
  wv_names <- unique(unlist(lapply(weighting_vars, function(wv){
    wv$var
  })))
  
  pop.margins <- lapply(weighting_vars, function(wv){
    #print(wv)
    wv_name <- ifelse(is.null(wv$name), wv$var, wv$name)
    if(wv$source == 'census'){
      census_margins[[wv_name]] %>%
        group_by_at(wv$var) %>%
        summarize(Freq = sum(Freq)) %>%
        ungroup() %>%
        mutate(Freq = Freq / sum(Freq))
    }else if(wv$source == 'mics_weighted'){
      mics_weighted %>% 
        group_by_at(wv$var) %>% 
        dplyr::summarize(Freq = sum(pop_weight), .groups = 'drop') %>%
        ungroup() %>%
        mutate(Freq = Freq / sum(Freq))
    }else if(wv$source == 'mics'){
      mics %>% 
        group_by_at(wv$var) %>% 
        dplyr::summarize(Freq = sum(weight_hh), .groups = 'drop') %>%
        ungroup() %>%
        mutate(Freq = Freq / sum(Freq))
    }else if(wv$source == 'wfp'){
      survey %>% 
        group_by_at(wv$var) %>% 
        dplyr::summarize(Freq = n(), .groups = 'drop') %>%
        ungroup() %>%
        mutate(Freq = Freq / sum(Freq))
    } else if(wv$source == 'wfp_rdd'){
      survey %>% 
        filter(sample_source == 'RDD') %>%
        group_by_at(wv$var) %>% 
        dplyr::summarize(Freq = n(), .groups = 'drop') %>%
        ungroup() %>%
        mutate(Freq = Freq / sum(Freq))
    }else if(wv$source == 'wfp_rdd_weighted'){
      wfp_targets$weighted %>% 
        filter(sample_source == 'RDD') %>%
        group_by_at(wv$var) %>% 
        dplyr::summarize(Freq = sum(pop_weight), .groups = 'drop') %>%
        ungroup() %>%
        mutate(Freq = Freq / sum(Freq))
    }else if(wv$source == 'zimvac22'){
      zimvac22[[wv_name]] %>%
        ungroup() %>%
        mutate(Freq = Freq / sum(Freq))
    }else if(wv$source == 'zimvac23'){
      zimvac23[[wv_name]] %>%
        ungroup() %>%
        mutate(Freq = Freq / sum(Freq))
    }else if(wv$source == 'zimvac21'){
      zimvac21[[wv_name]] %>%
        ungroup() %>%
        mutate(Freq = Freq / sum(Freq))
    }
  })
  print(pop.margins)
  
  samp.margins <- lapply(weighting_vars, function(wv){
    formulate(rhs = wv$var)
  })
  
  
  raw_svy <- svydesign(ids = ~1
                       , weight = raw$pop_weight
                       , data = raw %>% dplyr::select(-pop_weight)
  )
  svy <- rake(design = raw_svy
              , sample.margins = samp.margins
              , population.margins = pop.margins
              , control = list(epsilon = 0.000000001, maxit = 6000)
  )
  weighted <- cbind(svy$variables
                    , pop_weight = (1 / svy$prob)/mean(1 / svy$prob))
  
  
  # check that margins match expected
  weighted_margins <- lapply(wv_names, function(wv){
    weighted %>%
      group_by(var = wv, level = get(wv)) %>%
      dplyr::summarize(deff = 1 + var(pop_weight)
                       , avg_weight = mean(pop_weight)
                       , poll = sum(pop_weight)
      ) %>%
      ungroup() %>%
      mutate(poll = poll / sum(poll))
  }) %>%
    bind_rows()
  
  pop_targets <- lapply(pop.margins, function(pm){
    nms <- names(pm)[!names(pm) == 'Freq']
    lapply(nms, function(nm){
      pm %>% group_by(var = nm, level = get(nm)) %>%
        dplyr::summarise(pop = sum(Freq))
    }) %>%bind_rows()
  }) %>%
    bind_rows() %>% distinct()
  
  qc <- full_join(weighted_margins, pop_targets) %>%
    mutate(diff = pop - poll) %>%
    arrange(-abs(diff))
  
  return(list(weighted = weighted, qc = qc))
}


# make dummy frame by adding strata that DHS is missing
pop_frame_0 <- dhs %>%
  group_by(adm1_name, adm2_name
           , h_water_source_2way
           , hh_size_bucket, h_cooking_fuel
           , h_toilet_type_3way, h_toilet_type_2way
           ,hhh_sex, hhh_edu, h_cell) %>%
  summarize(pop_weight = sum(hh_weight)/100000
            , pop_weight = ifelse(pop_weight < 1,1, pop_weight))


## are any adm2s missing key strata in DHS data?
adm2_x_var <- lapply(c('h_water_source_2way', 'h_toilet_type_2way', 'hhh_sex', 'hhh_edu', 'hh_size_bucket','h_cooking_fuel', 'h_cell'), function(v){
  all_levels = pop_frame_0 %>% pull(get(v)) %>% unique()
  all_adm2 = pop_frame_0 %>% pull(adm2_name) %>% unique()
  left_join(data.frame(expand.grid(adm2_name = all_adm2, var = v, level = all_levels))
            , pop_frame_0 %>% 
              group_by(adm2_name, var= v, level = get(v)) %>%
              summarize(pop_weight = sum(pop_weight, na.rm= T))
  )
}) %>% bind_rows()
missing_strata <- adm2_x_var %>% filter(is.na(pop_weight))
missing_strata


pop_frame_1 <- pop_frame_0 %>% ungroup()
for(ii in 1:nrow(missing_strata)){
  cat(paste0(ii,'/', nrow(missing_strata)), '\n')
  pop_frame_1 <- bind_rows(pop_frame_1,
                           pop_frame_0 %>%
                             filter(get(missing_strata[ii,'var']) != missing_strata[ii,'level'], adm2_name == missing_strata[ii,'adm2_name']) %>%
                             mutate((!!missing_strata[ii,'var']) := missing_strata[ii,'level'], pop_weight = 0.01)
  )
}
pop_frame_1 <- pop_frame_1 %>% distinct()
pop_frame_1 %>% ungroup() %>% count()
pop_frame_1 %>% ungroup() %>% dplyr::select(-pop_weight) %>% distinct() %>% count()

# make sure we're not modifying things too much -- we don't want the (very small) weight of the missing strata to add up to something meaningful that changes the marginal distributions
left_join(
  pop_frame_0 %>%
    group_by(adm1_name) %>%
    summarize(pop_weight_orig = sum(pop_weight))
  , pop_frame_1 %>%
    group_by(adm1_name) %>%
    summarize(pop_weight_mod = sum(pop_weight))
) %>%
  mutate(pct_change=(pop_weight_mod- pop_weight_orig)/pop_weight_orig)

# check that it worked and that we're not missing any levels now
lapply(c('h_water_source_2way', 'h_toilet_type_2way', 'hhh_sex', 'hhh_edu', 'hh_size_bucket','h_cooking_fuel'), function(v){
  all_levels = pop_frame_1 %>% pull(get(v)) %>% unique()
  all_adm2 = pop_frame_1 %>% pull(adm2_name) %>% unique()
  left_join(data.frame(expand.grid(adm2_name = all_adm2, var = v, level = all_levels))
            , pop_frame_1 %>% 
              group_by(adm2_name, var= v, level = get(v)) %>%
              summarize(pop_weight = sum(pop_weight, na.rm= T))
  )
}) %>% bind_rows() %>% filter(is.na(pop_weight))


wv <- list(list(name = 'adm2_name', var = c('adm1_name','adm2_name'), source = 'census')
           , list(var = 'h_toilet_type_2way', source = 'census')
           , list(var = 'h_water_source_2way', source = 'census')
           , list(var = 'h_cell', source = 'census')
           , list(var = 'h_cooking_fuel', source = 'census')
           , list(var = 'hhh_sex', source = 'census')
)

popframe <- weightFrame(raw = pop_frame_1
                        , weighting_vars = wv)

popframe$qc

# Looks like this pop frame does not depend on zimvac year!
# write_csv(popframe$weighted, file = 'data/processed/popframe.csv')
write_csv(popframe$weighted, file = here(paste0('data/processed/popframe_dhs_census22.csv')))

popframe$qc %>%
  filter(var == 'adm2_name') %>%
  print(n =100)

popframe$weighted %>%
  group_by(h_cell) %>%
  summarize(pop_weight = sum(pop_weight)) %>%
  ungroup() %>%
  mutate(pop_weight = pop_weight / sum(pop_weight))
#-------------------------------------------------
## 1.2 using ZimVAC 2023
nrow(zimvac23)
zimvac23 |> dplyr::select('h_water_source_2way', 'h_toilet_type_2way', 'hhh_sex', 'hhh_edu', 'hh_size_bucket', 'h_cell') |> 
  tidyr::drop_na() |> nrow()
### note there are some missing values!

popframe_zimvac23 <- zimvac23 %>%
  group_by(adm1_name, adm2_name
           , h_water_source_2way
           , hh_size_bucket#, h_cooking_fuel
           , h_toilet_type_3way, h_toilet_type_2way
           ,hhh_sex, hhh_edu, h_cell) %>%
  summarise(n = n()) |> left_join(
    zimvac23 |> 
      group_by(adm1_name,adm2_name) |> 
      summarise(n_adm2 = n()) |>
      left_join((census_margins$adm2_name |> 
                   mutate(prop_adm2 = Freq/sum(Freq))),
                by = join_by(adm1_name, adm2_name))
  ) |> 
  mutate(
    prop_cell_byadm2 = n / n_adm2,
    pop_weight_byadm2 = n,
    pop_weight = prop_cell_byadm2 * prop_adm2*1000
  ) 
write_csv(popframe_zimvac23, file = here('data/processed/popframe_zimvac23ind_freq.csv'))
popframe_zimvac23 <- popframe_zimvac23|> select(-n,n_adm2,prop_cell_byadm2)
write_csv(popframe_zimvac23, file = here('data/processed/popframe_zimvac23ind.csv'))
