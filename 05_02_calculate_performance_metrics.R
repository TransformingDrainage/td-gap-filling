# This script evaluates accuracy of imputed data

source(file = '05_01_performance_metrics.R')



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



# Calculate performance indicators ----------------------------------------
dpac_files <- list.files('Data/Output_Data/DPAC/FINAL/', full.names = TRUE, recursive = FALSE, pattern = '.rds')

walk(dpac_files, ~ save_preformance_data(.x))


