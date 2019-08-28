# This script imputes missing tile flow data based on our NEW METHOD

source(file = '00_project_settings.R')
library(zoo)


# Read Data ---------------------------------------------------------------

df_flow <- read_rds('Data/Input_Data/RDS/DPAC_amp_MAR_pred_module1.rds')
df_rain <- read_csv('Data/Input_Data/DPAC_rolling_ave_precip.csv')
df_slopes <- read_rds('Data/Input_Data/RDS/DPAC_amp_MAR_recession_slopes.rds')


# Combine data for analysis
# all data (11 years)
df_11Y <- 
  df_flow %>%
  unnest() %>%
  gather(flow_type, flow, starts_with('flow_')) %>%
  # remove comments regarding prediction from Module1 for non-imputed data
  mutate(comments = ifelse(flow_type == 'flow_amp', NA, comments)) %>%
  group_by(simulation, prop, plotid, flow_type) %>%
  nest(.key = flow_data) %>%
  full_join(df_slopes, by = c('simulation', 'prop', 'plotid', 'flow_type')) %>%
  rename(slope_data = data) %>%
  arrange(as.double(simulation), prop, plotid, flow_type)


# Create table of years to be picked for 2, 4, and 6 year simulations
years <-
  cbind(simulation = 1:1000,
        tibble(Y6 = list(2007:2012, 2008:2013, 2009:2014, 2010:2015, 2011:2016),
               Y4 = list(2007:2010, 2009:2012, 2010:2013, 2012:2015, 2013:2016),
               Y2 = list(2007:2008, 2009:2010, 2011:2012, 2012:2013, 2015:2016))
        ) %>%
  as.tibble() %>%
  mutate(simulation = as.character(simulation))

# 6 consequtive years (2007-2012, 2008-2013, 2009-2014, 2010-2015, 2011-2016)
df_6Y <-
  df_11Y %>%
  left_join(years, by = 'simulation') %>%
  mutate(flow_data = map2(flow_data, 
                          Y6, 
                          ~ .x %>% filter(year(date) %in% .y) %>% arrange(date)), 
         slope_data = map2(slope_data, 
                           Y6, 
                           ~ .x %>% filter(year(date) %in% .y) %>% arrange(date)))

# 4 consequtive years (2007-2010, 2009-2012, 2010-2013, 2012-2015, 2013-2016)
df_4Y <-
  df_11Y %>%
  left_join(years, by = 'simulation') %>%
  mutate(flow_data = map2(flow_data, 
                          Y4, 
                          ~ .x %>% filter(year(date) %in% .y) %>% arrange(date)), 
         slope_data = map2(slope_data, 
                           Y4, 
                           ~ .x %>% filter(year(date) %in% .y) %>% arrange(date)))
# 2 consequtive years (2007-2008, 2009-2010, 2011-2012, 2012-2013, 2015-2016)
df_2Y <-
  df_11Y %>%
  left_join(years, by = 'simulation') %>%
  mutate(flow_data = map2(flow_data, 
                          Y2, 
                          ~ .x %>% filter(year(date) %in% .y) %>% arrange(date)), 
         slope_data = map2(slope_data, 
                           Y2, 
                           ~ .x %>% filter(year(date) %in% .y) %>% arrange(date)))



# Develop Regression model ------------------------------------------------

# function to fit linear model using 3-day-ave and 3-day-weighted-ave precipitation
reg_model_3_day_avg <- function(df) {
  lm(flow ~ rain_3 - 1, data = df)
}
reg_model_3_day_weighted <- function(df) {
  lm(flow ~ rain_3_weighted - 1, data = df)
}



# Determine flow threshold ------------------------------------------------

# flow_limit = 0.30  # NEED TO justify
# rain_limit = 0.45  # NEED TO justify
rain_limit = 0  # Currently used value


# Select input data and save imputed data ---------------------------------

# 11 YEARS
df <- df_11Y
source('03_impute_missing_data.R')
write_rds(x = df_model, path = 'Data/Input_Data/RDS/DPAC_amp_MAR_regression_model_Y11.rds')
write_rds(x = df_pred, path = 'Data/Input_Data/RDS/DPAC_amp_MAR_imputed_data_Y11.rds')


# 6 YEARS
df <- df_6Y
source('03_impute_missing_data.R')
write_rds(x = df_model, path = 'Data/Input_Data/RDS/DPAC_amp_MAR_regression_model_Y6.rds')
write_rds(x = df_pred, path = 'Data/Input_Data/RDS/DPAC_amp_MAR_imputed_data_Y6.rds')


# 4 YEARS
df <- df_4Y
source('03_impute_missing_data.R')
write_rds(x = df_model, path = 'Data/Input_Data/RDS/DPAC_amp_MAR_regression_model_Y4.rds')
write_rds(x = df_pred, path = 'Data/Input_Data/RDS/DPAC_amp_MAR_imputed_data_Y4.rds')


# 2 YEARS
df <- df_2Y
source('03_impute_missing_data.R')
write_rds(x = df_model, path = 'Data/Input_Data/RDS/DPAC_amp_MAR_regression_model_Y2.rds')
write_rds(x = df_pred, path = 'Data/Input_Data/RDS/DPAC_amp_MAR_imputed_data_Y2.rds')


# Some seasons are missing recession slope because of not having recession limbs or missing points
df_model %>%
  left_join(df_slopes_ave,
            by = c('simulation', 'prop', 'flow_type', 'plotid', 'season')) %>%
  filter(is.na(ave_slope), points > 5) %>%
  filter(simulation == 801 & points == 19) %>%
  unnest(data) %>%
  mutate(year = year(date),
         date = update(date, year = 2012)) %>%
  ggplot(aes(date, flow)) + geom_point() + geom_line() + facet_grid(year ~ .) +
  theme_light()













