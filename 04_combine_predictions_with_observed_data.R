# This script combines predicted and original data

source(file = '00_project_settings.R')


# Read Data ---------------------------------------------------------------

dpac_files <- list.files('Data/Inter_Data/Phase3_Imputation/DPAC/predictions/FINAL/',
                         full.names = TRUE)

dpac_original <- 
  read_rds('Data/Input_Data/daily_tile_flow_complete.rds') %>%
  filter(siteid == 'DPAC') %>%
  select(-year, -siteid)



# Function to combine predicted and original data and save
combine_and_save <- function(FILE) {
  read_rds(FILE) %>%
    unnest(data) %>%
    left_join(dpac_original, by = c('plotid', 'date')) %>%
    select(scenario, prop, sim = simulation, flow_type, api, plotid, season, 
           date, flow, flow_pred, rain, everything()) %>%
    write_rds(paste0('Data/Output_Data/DPAC/FINAL/', basename(FILE)), 
              compress = 'xz')
}


# Save combined data
walk(dpac_files, ~ combine_and_save(.x))



