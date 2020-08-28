# This script prepares different sub-sets of amputed data for gap-filling process.
# It also creates sub-sets for 5 different senarios (2, 4, 6, 8, and 10 years of data)

source(file = '00_project_settings.R')



# Read Data ---------------------------------------------------------------

dpac <- read_rds('Data/Inter_Data/DPAC_amp_MAR.rds')
serf <- read_rds('Data/Inter_Data/SERF_amp_MAR.rds')



# Split data based on proportion missing values and subset by number of years
prop <- c('05', '15', '25', '35', '45')
year_col <- c('Y2', 'Y4', 'Y6', 'Y8')



# DPAC --------------------------------------------------------------------
# Create table of years to be picked for 2, 4, 6 and 8 year simulations
years <-
  cbind(simulation = 1:200,
        tibble(Y8 = list(2007:2014, 2007:2014, 2008:2015, 2009:2016, 2009:2016),
               Y6 = list(2007:2012, 2008:2013, 2009:2014, 2010:2015, 2011:2016),
               Y4 = list(2007:2010, 2009:2012, 2010:2013, 2012:2015, 2013:2016),
               Y2 = list(2007:2008, 2009:2010, 2011:2012, 2012:2013, 2015:2016))
  ) %>%
  as.tibble() %>%
  mutate(simulation = as.character(simulation))

# split 10 years of data
for (i in prop) {
  dpac %>%
    keep(~ median(.$prop) == paste0('0.', i)) %>%
    write_rds(paste0('Data/Inter_Data/DPAC/DPAC_Y', 'A_', i, '.rds'), compress = 'xz')
}

# subset years corresponding to different senarios within each split (proportion data)
for (i in prop) {
  dpac %>%
    keep(~ median(.$prop) == paste0('0.', i)) %>% 
    bind_rows(.id = 'simulation') %>% 
    group_by(simulation) %>%
    nest() %>%
    left_join(years, by = 'simulation') %>%
    ungroup() -> TEMP
  for (j in year_col) {
    TEMP %>%
      mutate(flow_data = map2(data, 
                              !!(sym(j)), 
                              ~ .x %>% filter(year %in% .y) %>% arrange(date))) %>%
      select(simulation, flow_data) %>%
      unnest() %>%
      write_rds(paste0('Data/Inter_Data/DPAC/DPAC_', j, '_', i, '.rds'), compress = 'xz')
  }
}


