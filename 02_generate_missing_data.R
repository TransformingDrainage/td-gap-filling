# This script prepares different versions of amputed data for gap-filling process.
# Datasets differ from each other by generated missing values (so we can check predictions)
# Amputation of the data is done using "mice" package

source(file = '00_project_settings.R')


# Read Data ---------------------------------------------------------------

df <- read_csv('Data/Input_Data/daily_tile_flow_complete.csv')

# DPAC is chosen for analysis as it is the most complete dataset
df %>% 
  filter(siteid == "DPAC") %>%
  filter(!is.na(flow)) %>%
  select(-comments, -siteid) %>%
  spread(plotid, flow) -> DPAC



# MAR Amputation ----------------------------------------------------------
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

# because of the overlapping missingness pattern 
# we need to adjust proportions of missing data and frequencies
# so it is exactly as we designed
my_freq_adj <- my_freq/sum(my_freq)
my_prop_adj <- my_prop*sum(my_freq)


# this can be of use when implementing MNAR
my_weights <- tibble(year = 1, date = 1, rain = 1, NE = c(1,0,0), SW = c(0,1,0)) 

# this matrix helps to distribute missing values equally throught the period (no TAILS, etc)
my_odds <- matrix(1, nrow = 3, ncol = 4)


# ........................................
# Create dummy variables to hold the data
# ........................................

DPAC_amp <- vector('list', length = length(my_prop_adj) * 200)
temp_amp <- vector('list', length = 11)


# ..............................
# Generate missing data at DPAC
# ..............................

for (i in seq_along(my_prop_adj)) {
  for (j in 1:200) {
    list_num <- 200*(i-1) + j
    # apply percent to remove to each year separately
    for (k in 2006:2016) {
      DPAC %>% 
        filter(year == k) %>%
        ampute(patterns = my_pattern,
               freq = my_freq_adj,
               prop = my_prop_adj[i], 
               # Missing At Random (MAR) mechanism is used for amputation
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



# Save amputated DPAC data ------------------------------------------------
write_rds(DPAC_amp, path = 'Data/Input_Data/RDS/DPAC_amp_MAR.rds')
DPAC_amp %>% bind_rows() %>% write_csv('Data/Input_Data/DPAC_amp_MAR.csv')




