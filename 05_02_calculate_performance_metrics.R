# This script evaluates accuracy of imputed data

source(file = '05_01_performance_metrics.R')



# Calculate overall performance of each scenario ---------------------------------------------

# function to calculate performance metrics
performance <- function(df) {
  df %>% 
    filter(!is.na(comments)) %>%
    mutate(scenario = str_sub(scenario, 1, 2)) %>%
    group_by(scenario, prop, sim, flow_type, api, plotid) %>%
    nest() %>% 
    ungroup() %>%
    calc_my_perf() %>%
    select(-data)
}

# function to read data and save calculated performance indicators
save_preformance_data <- function(PATH) {
  dir <- dirname(PATH)
  file <- basename(PATH) %>% 
    str_remove('pred_phase3_') %>%
    str_replace('.rds', '_performance.rds')
  read_rds(PATH) %>%
    performance() %>%
    write_rds(paste0(dir, '/STATS/', file), compress = 'xz')
}


# Calculate performance indicators
dpac_files <- 
  list.files('Data/Output_Data/DPAC/FINAL/', full.names = TRUE, recursive = FALSE, pattern = '.rds')

walk(dpac_files, ~ save_preformance_data(.x))



# Calculate seasonal and yearly performances -------------------------------------------------
# only 10-year data with 3-day moving average API is used
 
# function to read data and save calculated seasonal performance indicators
seasonal_preformance <- function(PATH) {
  dir <- dirname(PATH)
  file <- basename(PATH) %>% 
    str_remove('pred_phase3_') %>%
    str_replace('.rds', '_performance_seasonal.rds')
  read_rds(PATH) %>%
    filter(!is.na(comments)) %>%
    group_by(scenario, prop, sim, flow_type, api, plotid, season) %>%
    nest() %>% 
    ungroup() %>%
    calc_my_perf() %>%
    mutate(number_of_points = map_dbl(data, ~ .x %>% nrow())) %>%
    select(-data) %>%
    write_rds(paste0(dir, '/STATS/SEASONAL/', file), compress = 'xz')
}

# function to read data and save calculated yearly performance indicators 
yearly_preformance <- function(PATH) {
  dir <- dirname(PATH)
  file <- basename(PATH) %>% 
    str_remove('pred_phase3_') %>%
    str_replace('.rds', '_performance_yearly.rds')
  read_rds(PATH) %>%
    filter(!is.na(comments)) %>%
    mutate(year = year(date)) %>%
    group_by(scenario, prop, sim, flow_type, api, plotid, year) %>%
    nest() %>% 
    ungroup() %>%
    calc_my_perf() %>%
    mutate(number_of_points = map_dbl(data, ~ .x %>% nrow())) %>%
    select(-data) %>%
    write_rds(paste0(dir, '/STATS/YEARLY/', file), compress = 'xz')
}


# Calculate performance indicators
dpac_YA_files <-
  list.files('Data/Output_Data/DPAC/FINAL/', full.names = TRUE, recursive = FALSE, pattern = 'YA.*rain3.rds')

walk(dpac_YA_files, ~ seasonal_preformance(.x))
walk(dpac_YA_files, ~ yearly_preformance(.x))


