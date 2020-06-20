# This script combines predicted and original data

source(file = '00_project_settings.R')


# Read Data ---------------------------------------------------------------

df_Y2 <- read_rds('Data/Input_Data/RDS/DPAC_amp_MAR_imputed_data_Y2.rds')
df_Y4 <- read_rds('Data/Input_Data/RDS/DPAC_amp_MAR_imputed_data_Y4.rds')
df_Y6 <- read_rds('Data/Input_Data/RDS/DPAC_amp_MAR_imputed_data_Y6.rds')
df_Y11 <- read_rds('Data/Input_Data/RDS/DPAC_amp_MAR_imputed_data_Y11.rds')

df_original <- 
  read_rds('Data/Input_Data/RDS/daily_tile_flow_complete.rds') %>%
  select(-year, -comments, -siteid, -rain)


# Function to combine predicted and original data
add_original_data <- function(df) {
  df %>%
    unnest(data) %>%
    left_join(df_original, by = c('plotid', 'date'))
}


# Make dumb list to increase speed of unnesting
cases <- distinct(df_Y2, prop, flow_type, model_name)

combine_data <- function(df){
  start_tm <- proc.time()
  my_dummy <- vector('list', nrow(cases))
  for (i in seq_along(my_dummy)) {
    my_dummy[[i]] <-
      df %>%
      inner_join(cases[i, ]) %>%
      add_original_data()
  }
  print(proc.time() - start_tm)
  return(my_dummy)
}


# start converting data
Y2_all  <- combine_data(df_Y2)   # elapsed time = 17.55
Y4_all  <- combine_data(df_Y4)   # elapsed time = 42.78
Y6_all  <- combine_data(df_Y6)   # elapsed time = 53.72
Y11_all <- combine_data(df_Y11)  # elapsed time = 88.88

write_rds(Y2_all,  'Data/Output_Data/all_data_Y2.rds',  compress = 'xz')
write_rds(Y4_all,  'Data/Output_Data/all_data_Y4.rds',  compress = 'xz')
write_rds(Y6_all,  'Data/Output_Data/all_data_Y6.rds',  compress = 'xz')
write_rds(Y11_all, 'Data/Output_Data/all_data_Y11.rds', compress = 'xz')




