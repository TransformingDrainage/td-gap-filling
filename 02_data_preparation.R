# This script prepares different versions of data for imputation step.
# Versions differ from each other by artificially created missing values (so we can check predictions)

source(file = '00_project_settings.R')



# Read Data ---------------------------------------------------------------

df <- read_csv('Data/Input_Data/daily_tile_flow_with_dup_regression.csv')
df2 <- read_csv('Data/Input_Data/daily_tile_flow.csv')


# create the master version with years of interest for each site
  # DPAC 2006 - 2016
  # SERF_IA 2007 - 2017
original_input <- df %>%
  # add precipitation data
  left_join(df2, by = c('siteid', 'plotid', 'dwm', 'date', 'flow')) %>%
  filter(!(year(date) == 2017 & siteid == 'DPAC')) %>%
  # remove moths with no data in the begining of 2006 at DPAC (before study commenced)
  filter(date > ymd(20060615)) %>%
  select(siteid, plotid, year, date, flow_pred, precip_on_site, comments) %>%
  rename(flow = flow_pred, rain = precip_on_site)


# save the original data
original_input %T>%
  write_rds(path = 'Data/Input_Data/RDS/original_input.rds') %>%
  write_csv(path = 'Data/Input_Data/original_input.csv')

