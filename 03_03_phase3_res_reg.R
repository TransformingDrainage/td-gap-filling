# Phase 3 - Imputation
# This script imputes missing tile flow data based on our NEW METHOD

source(file = '00_project_settings.R')
library(zoo)


# Read Data ---------------------------------------------------------------
df_flow <- read_rds('Data/Inter_Data/Phase2_Imputation/DPAC/DPAC_Y2_05_pred_phase2.rds')
df_slopes <- read_rds('Data/Inter_Data/Phase3_Imputation/DPAC/DPAC_ave_recession_slopes.rds')
df_rain <- read_rds('Data/Inter_Data/Phase3_Imputation/rolling_ave_precip.rds') %>%
  select(-snowing)



# Develop Regression model ------------------------------------------------

# function to fit linear regression model using API
reg_model <- function(df) {
  lm(flow ~ api - 1, data = df)
}

# function for fitting model
model_fit <- function(df) {
  df %>%
    unnest(data) %>%
    gather(flow_type, flow, starts_with('flow_')) %>%
    # remove comments regarding prediction from Phase 2 for non-imputed data
    mutate(comments = ifelse(flow_type == 'flow_amp', NA, comments)) %>%
    group_by(simulation, prop, flow_type, plotid) %>%
    # calculate limiting tile flow as mean daily summer flow 
    mutate(min_flow = mean(flow[season == "Summer"], na.rm = TRUE)#,
           # # .............................................................. ASSUMPTION 
           # # THIS IS PART OF ACTUAL GAP FILLING USED IN TD PROJECT!
           # # correct limiting flow so it is not > 0.3 mm/day
           # min_flow = ifelse(min_flow > flow_limit, flow_limit, min_flow)
    ) %>%
    # add precipitation data
    left_join(RAIN, by = c('date')) %>%
    group_by(simulation, prop, flow_type, plotid, season) %>%
    nest() %>%
    # filter data to be used for regression 
    mutate(data_filtered = map(.x = data,
                               .f = ~ .x %>%
                                 # remove days when there was no rain OR tile flow was below minimum limit
                                 filter(flow > min_flow, api > 0)),
           # this give the number of point available to fit the model
           points = map_dbl(data_filtered, nrow)) %>%
    # fit the model
    mutate(model = map_if(.x = data_filtered, .p = points > 3, .f = reg_model)) %>% 
    select(-data_filtered) %>%
    # combine recession slope with flow data
    left_join(SLOPES, by = c('simulation', 'prop', 'flow_type', 'plotid', 'season')) 
}

# function for imputing missing flow data
model_predict <- function(df) {
  df %>%
    filter(points > 3, !is.na(slope)) %>%
    # add predictions of pick flows using actual data
    mutate(isnull = map_lgl(model, is.null),
           # predict pick flow only when there is a model available 
           data = ifelse(isnull == 1, 
                         data, 
                         map2(.x = model,
                              .y = data,
                              .f = ~ augment(.x, newdata = .y)))) %>%
    select(simulation:data, slope) %>%
    unnest(data) %>% 
    arrange(simulation, prop, flow_type, plotid, date) %>%
    # resolve the problem of skipping first data points when rain < rain_limit
    # for example after missing dates of precip there is a first record of 0 rain, 
    # we need to keep prediction of corresponding tile flow of 0, so we can predict down the timeline
    group_by(simulation, prop, flow_type, plotid) %>%
    # find the first non-NA value for precip within each site-plot
    mutate(is_rain = is.na(api),
           group = rep(seq_along(rle(is_rain)$length), rle(is_rain)$length)) %>%
    group_by(simulation, prop, flow_type, plotid, group) %>%
    # select only those first non-NA values when there was a measurement
    mutate(is_first = ifelse(row_number() == 1 & is_rain == FALSE, "Y", "N")) %>%
    ungroup() %>%
    # choose predictions for days when it was raining and no flow measured
    mutate(pred = ifelse(.fitted < 0, 0, .fitted),  # eliminate predicted negative flows
           flow_pred = ifelse(is.na(flow) & api > rain_limit, pred, flow),
           # add predictions for the first non-NA days when they are missing (due to rain < rain_limit)
           flow_pred = ifelse(is.na(flow_pred) & is_first == "Y", pred, flow_pred)) %>%
    select(simulation:api, flow_pred, slope) %>%
    # THIS IS WHERE YOU CAN ADD PREVIOUS DAYS FRECTION TO THE PREDICTED FLOWS
    
    # predict flow of falling limb
    group_by(simulation, prop, flow_type, plotid) %>%
    mutate(LOGIC = ifelse(is.na(flow_pred), "Y", "N"),
           group = rep(seq_along(rle(LOGIC)$length), rle(LOGIC)$length)) %>%
    group_by(simulation, prop, flow_type, plotid, group) %>%
    mutate(count = 1:n(),
           count = ifelse(LOGIC == "N", 0, count)) %>%
    group_by(simulation, prop, flow_type, plotid) %>%
    # limit predicted flow to measured max tile flow
    mutate(flow_max = max(flow, na.rm = T),
           flow_pred = ifelse(flow_pred > flow_max, flow_max, flow_pred),
           flow_max = NULL) %>% 
    select(-group, -LOGIC) %>%
    # apply recession equation (Qi = Qi-1 * e^k, where k is seasonal recession slope)
    mutate(flow_pred2 = na.locf(flow_pred, na.rm = FALSE),
           # use arithmetic mean slope
           flow_pred2_mean = round(flow_pred2 * exp(slope * count), 6),
           flow_pred = ifelse(is.na(flow_pred), flow_pred2_mean, flow_pred)) %>%
    select(-starts_with("flow_pred2"), -count) %>%
    # remove predictions of flow in days when there was no rainfall data (see CRAWF)
    mutate(flow_pred = ifelse(is.na(flow) & is.na(api), NA, flow_pred)) %>%
    select(-slope, -api, -min_flow) %>%
    mutate(comments = ifelse(is.na(flow) & !is.na(flow_pred), 'predicted via recession model', comments)) %>%
    select(simulation, prop, flow_type, plotid, season, date, flow_pred, comments) %>%
    group_by(simulation, prop, flow_type, plotid) %>%
    nest()
}


# set limiting rain
rain_limit = 0 


# Select input data and save imputed data ---------------------------------
dpac_files <- list.files('Data/Inter_Data/Phase2_Imputation/DPAC/', full.names = TRUE)

for (i in dpac_files) {
  SCENARIO <- str_extract(i, 'Y._.{2}')
  print(SCENARIO)
  # Filter slopes corresponding to the scenario
  SLOPES <- df_slopes %>% 
    filter(id == SCENARIO) %>%
    select(simulation:season, slope = ave_slope)
  for (j in 2:3) {
    RAIN <- df_rain %>%
      filter(siteid == 'DPAC') %>%
      select(date, api = paste0('rain_', j))
    # Fit model and predict missing values
    PREDICTIONS <- 
      read_rds(i) %>% 
      model_fit() %>%
      model_predict()
    # save predictions
    PREDICTIONS %>%
      mutate(scenario = SCENARIO,
             api = paste(j, 'day rolling ave')) %>%
      write_rds(paste0('Data/Inter_Data/Phase3_Imputation/DPAC/predictions/FINAL/DPAC_', 
                       SCENARIO, '_pred_phase3_rain', j, '.rds'),
                compress = 'xz')
  }
}



# Plot Predictions --------------------------------------------------------
# Read 5 subsets of data from complete 10-year scenario
df_plot <- 
  bind_rows(
    read_rds('Data/Inter_Data/Phase3_Imputation/DPAC/predictions/FINAL/DPAC_YA_05_pred_phase3_rain3.rds') %>%
      filter(simulation == 33),
    read_rds('Data/Inter_Data/Phase3_Imputation/DPAC/predictions/FINAL/DPAC_YA_25_pred_phase3_rain3.rds') %>%
      filter(simulation == 55),
    read_rds('Data/Inter_Data/Phase3_Imputation/DPAC/predictions/FINAL/DPAC_YA_45_pred_phase3_rain3.rds') %>%
      filter(simulation == 77)
  )

# Phase 3 predictions for 2016
df_plot %>%
  ungroup() %>%
  filter(flow_type == 'flow_pred') %>%    # select data that predicted by both modules
  unnest(data) %>%
  filter(year(date) == 2016 & month(date) < 7) %>%
  ggplot(aes(x = date, group = plotid)) +
  geom_point(aes(y = flow_pred), size = 1, alpha = 0.75) + 
  geom_point(data = . %>% filter(!is.na(comments)), 
             aes(y = flow_pred), colour = 'orange2', size = 2) +
  geom_point(data = . %>% filter(comments == 'predicted via recession model'), 
             aes(y = flow_pred), colour = 'slateblue2', size = 2) +
  geom_line(aes(y = flow_pred), alpha = 0.25) +
  scale_x_date(date_labels = '%b') +
  labs(x = NULL, y = 'Tile Flow, mm',
       subtitle = 'January-June 2016',
       title = 'Daily Drainage Predictions') +
  facet_grid(prop ~ plotid) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        text = element_text(size = 16))
ggsave('Figs/phase3/Phase3_predictions_2016_Jan_Jun.png',
       width = 16, height = 10)


