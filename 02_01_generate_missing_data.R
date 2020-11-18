# This script prepares different versions of amputed data for gap-filling process.
# Datasets differ from each other by generated missing values (so we can check predictions)
# Amputation of the data is done using "mice" package

source(file = '00_project_settings.R')



# Read Data ---------------------------------------------------------------

df <- read_rds('Data/Input_Data/daily_tile_flow_complete.rds')



# MAR Amputation Settings -------------------------------------------------
# Do the amputation for each site individually, and format as below

# .........................................
# Set up parameters for amputation process
# .........................................

# percentage of missing data
my_prop <- seq(0.05, 0.45, 0.10)

# set 3 patterns of missing data: 
# (a) SW is missing, but not NE,
# (b) NE is missing, but SW is not,
# (c) both NE and SW are missing
my_pattern <- tibble(year = 1, date = 1, rain = 1, NE = c(1,0,0), SW = c(0,1,0)) 

# it's more frequent plots failing the same time (pattern C), than each one separately
# this can be done by assigning frequency to each pattern
f = 0.9  # frequency of pattern number 3 (both missing)
my_freq <- c(1- f, 1- f, f)

# because of the missingness patterns used (a and b patterns cause splitting)
# we need to adjust proportions and frequencies of missing data, 
# so it is exactly as we designed
my_freq_adj <- my_freq/sum(my_freq)
my_prop_adj <- my_prop*sum(my_freq)


# this can be of use when implementing MNAR
my_weights <- tibble(year = 1, date = 1, rain = 1, NE = c(1,0,0), SW = c(0,1,0)) 

# this matrix helps to distribute missing values equally throught the period (no TAILS, etc)
my_odds <- matrix(1, nrow = 3, ncol = 4)



# Ampute DPAC Data ----------------------------------------------------
# DPAC has the most complete dataset
df %>% 
  filter(siteid == "DPAC") %>%
  filter(!is.na(flow)) %>%
  select(-siteid, -snowing) %>%
  spread(plotid, flow) -> DPAC

# ........................................
# Create dummy variables to hold the data
# ........................................

DPAC_amp <- vector('list', length = length(my_prop_adj) * 200)
temp_amp <- vector('list', length = 10)   # 10 corresponds to number of total years

# ..............................
# Generate missing data at DPAC
# ..............................

for (i in seq_along(my_prop_adj)) {
  for (j in 1:200) {
    list_num <- 200*(i-1) + j
    # apply percent to remove to each year separately
    for (k in 2007:2016) {
      DPAC %>% 
        filter(year == k) %>%
        ampute(patterns = my_pattern,
               freq = my_freq_adj,
               prop = my_prop_adj[i], 
               # Missing At Random (MAR) mechanism is used for amputation
               mech = 'MAR',   
               cont = FALSE,
               odds = my_odds) -> results
      temp_amp[[k-2006]]  <- results$amp
    }
    DPAC_amp[[list_num]] <- bind_rows(temp_amp) %>% 
      mutate(date = as.Date(date, origin), prop = my_prop[i])
  }
}

# check the last output
DPAC_amp[[1000]]

# Save amputated DPAC data
write_rds(DPAC_amp, path = 'Data/Inter_Data/Amputated_Subsets/DPAC_amp_MAR.rds', compress = 'xz')




# Ampute SERF Data ----------------------------------------------------
# SERF has two missing years at S2
df %>% 
  filter(siteid == "SERF") %>%
  filter(!is.na(flow)) %>%
  select(-siteid) %>%
  spread(plotid, flow) %>%
  # temporary imput missing value at S2 (for amputation to work)
  mutate(S2 = ifelse(is.na(S2), S5, S2)) -> SERF

# ........................................
# Create dummy variables to hold the data
# ........................................

SERF_amp <- vector('list', length = length(my_prop_adj) * 200)
temp_amp <- vector('list', length = 11)

# ..............................
# Generate missing data at SERF
# ..............................

for (i in seq_along(my_prop_adj)) {
  for (j in 1:200) {
    list_num <- 200*(i-1) + j
    # apply percent to remove to each year separately
    for (k in 2007:2017) {
      SERF %>% 
        filter(year == k) %>%
        ampute(patterns = my_pattern,
               freq = my_freq_adj,
               prop = my_prop_adj[i], 
               # Missing At Random (MAR) mechanism is used for amputation
               mech = 'MAR',   
               cont = FALSE,
               odds = my_odds) -> results
      temp_amp[[k-2006]]  <- results$amp
    }
    SERF_amp[[list_num]] <- bind_rows(temp_amp) %>% 
      mutate(date = as.Date(date, origin), prop = my_prop[i])
  }
}

# remove all temporary imputations from S2
SERF_amp_rm <- 
  SERF_amp %>%
  map(.f = ~ .x %>% mutate(S2 = ifelse(year %in% 2009:2010, NA_real_, S2))) 

# check the last output
SERF_amp[[1000]] %>% filter(year == 2009)
SERF_amp_rm[[1000]] %>% filter(year == 2009)

# Save amputated SERF data
write_rds(SERF_amp_rm, path = 'Data/Inter_Data/Amputated_Subsets/SERF_amp_MAR.rds', compress = 'xz')


