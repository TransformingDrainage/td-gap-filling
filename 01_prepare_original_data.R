# This script prepares data for farther analysis by completing it.
# It includes predicting missing values by dup-regression, which is part of the gap-filling method proposed;
# But here it is used only to insure we have a complete input data for the sake of the paper.

source(file = '00_project_settings.R')



# Download Final Data from Google Drive -----------------------------------
drive_ls(as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), pattern = 'tile_flow') %>%
  drive_download('Data/Input_Data/Original/tile_flow.csv', overwrite = TRUE)



# Read Data ---------------------------------------------------------------
flow <- 
  read_csv('Data/Input_Data/Original/tile_flow.csv', guess_max = 80000)

weather <- 
  # instead of using the final weather file, we used old data file because it
  # already has precipitation from relative weather stations each year at SERF_IA
  # NOTE: at SERF_IA there were 3 sources for precipitation with different degree
  # of reliability for individual year, according to the site personals feedback
  read_csv('Data/Input_Data/Original/daily_tile_flow.csv')

snowfall <- 
  readxl::read_excel('Data/Input_Data/Original/DPAC_snowfall_2007-2018.xlsx',
                     skip = 7)



# Combine Snowfall and Weather Data ---------------------------------------
weather_snow <-
  snowfall %>% 
  mutate(siteid = 'DPAC',
         date = as.Date(Date),
         snowing = ifelse(as.numeric(SNOW) == 0 | is.na(as.numeric(SNOW)), 
                          0, 1)) %>%
  select(siteid, date, snowing) %>%
  right_join(weather, by = c("siteid", "date"))
  



# Combine Data ------------------------------------------------------------
data <-
  flow %>%
  filter(siteid %in% c('IA_Washington', 'IN_Randolph'),
         dwm_treatment == 'Free Drainage') %>%
  select(id = siteid, plotid, date, tile_flow, tile_flow_filled)  %>%
  full_join(weather_snow, by = c('plotid', 'date')) %>%
  select(id, siteid, plotid, year, date, tile_flow, tile_flow_filled, precip_on_site, snowing) 
  


# Select Data to Be Analyzed ----------------------------------------------
data_to_analyze <-
  data %>%
  select(siteid, plotid, year, date, flow = tile_flow_filled, rain = precip_on_site, snowing) %>%
  # rename SERF_IA to SERF for ease to type only
  mutate(siteid = ifelse(siteid == 'SERF_IA', 'SERF', siteid)) %>%
  # include only years of interest for each site
  filter(!(siteid == 'DPAC' & year < 2007)) %>%
  filter(!(siteid == 'DPAC' & year > 2016)) %>%
  filter(!(siteid == 'SERF' & year < 2006)) %>%
  filter(!(siteid == 'SERF' & year > 2017)) %>%
  # remove any data from S2 in 2009-2010
  mutate(flow = ifelse(plotid == 'S2' & year %in% 2009:2010, NA_real_, flow)) %>%
  arrange(siteid, plotid, date)



# Save Complete Data ------------------------------------------------------
data_to_analyze %T>%
  write_rds(path = 'Data/Input_Data/daily_tile_flow_complete.rds', compress = 'xz') %>%
  write_csv("Data/Input_Data/daily_tile_flow_complete.csv")


