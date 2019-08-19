# This script prepares different versions of data for imputation step.
# Versions differ from each other by artificially created missing values (so we can check predictions)

source(file = '00_project_settings.R')
library(mice)


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



# Amputation --------------------------------------------------------------

# Do the amputation for each site individually, and format as below

# DPAC ....................................................................
original_input %>% 
  filter(siteid == "DPAC") %>%
  filter(!is.na(flow)) %>%
  select(-comments, -siteid) %>%
  spread(plotid, flow) -> DPAC

# set parameters for amputation process

# percentage of missing data
my_prop <- seq(0.05, 0.45, 0.10)

# set 3 patterns of missing data: 
# (a) SW is missing, but not NE,
# (b) NE is missing, but SW is not,
# (c) both NE and SW are missing
my_pattern <- tibble(year = 1, date = 1, rain = 1, NE = c(1,0,0), SW = c(0,1,0)) 

# it's more frequent plots failing the same time (pattern C), than each one separately
# this can be done by assigning frequency to each pattern
my_freq <- c(0.1, 0.1, 0.8)

# this can be of use when implementing MNAR
my_weights <- tibble(year = 1, date = 1, rain = 1, NE = c(1,0,0), SW = c(0,1,0)) 

# this matrix helps to distribute missing values equally throught the period (no TAILS, etc)
my_odds <- matrix(1, nrow = 3, ncol = 4)

DPAC_amp <- vector('list', length = length(my_prop) * 200)
temp_amp <- vector('list', length = 11)
# Generating missing data at DPAC
for (i in seq_along(my_prop)) {
  for (j in 1:200) {
    list_num <- 200*(i-1) + j
    # apply percent to remove to each year separately
    for (k in 2006:2016) {
      DPAC %>% 
        filter(year == k) %>%
        ampute(patterns = my_pattern,
               freq = my_freq,
               prop = my_prop[i], 
               mech = 'MAR', 
               cont = FALSE,
               odds = my_odds) -> results
      temp_amp[[k-2005]]  <- results$amp
    }
    DPAC_amp[[list_num]] <- bind_rows(temp_amp) %>% 
      mutate(date = as.Date(date), prop = my_prop[i])
  }
}

# check the last output
DPAC_amp[[1000]]

# save amputated DPAC data
write_rds(DPAC_amp, path = 'Data/Input_Data/RDS/DPAC_amp_MAR.rds')
DPAC_amp %>% bind_rows() %>% write_csv('Data/Input_Data/DPAC_amp_MAR.csv')




