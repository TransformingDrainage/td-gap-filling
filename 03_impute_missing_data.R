# This code fits the model and gives predictions of missing drainage data

# Fit model to predict peak flows caused by precipitation event
df_model <-
  df %>%
  unnest(flow_data) %>%
  group_by(simulation, prop, flow_type, plotid) %>%
  # calculate limiting tile flow as mean daily summer flow 
  mutate(min_flow = mean(flow[season == "Summer"], na.rm = TRUE)#,
         # # .............................................................. ASSUMPTION 
         # # THIS IS PART OF ACTUAL GAP FILLING USED IN TD PROJECT!
         # # correct limitting flow so it is not > 0.3 mm/day
         # min_flow = ifelse(min_flow > flow_limit, flow_limit, min_flow)
  ) %>%
  # add precipitation data
  left_join(df_rain[-2], by = c('date')) %>%
  group_by(simulation, prop, flow_type, plotid, season) %>%
  nest() %>%
  # filter data to be used for regression 
  mutate(data_filtered = map(.x = data,
                             .f = ~ .x %>%
                               # remove days when there was no rain OR tile flow was below minimum limit
                               filter(flow > min_flow, rain_3 > 0)),
         # this give the number of point available to fit the model
         points = map_dbl(data_filtered, nrow)) %>%
  # fit the model
  mutate(reg_model_3_day_avg = map_if(.x = data_filtered, 
                                      .p = points > 3, 
                                      .f = reg_model_3_day_avg),
         reg_model_3_day_weighted = map_if(.x = data_filtered, 
                                           .p = points > 3,
                                           .f = reg_model_3_day_weighted)
  )



# Calculate Average Recession Slope ---------------------------------------

df_slopes_ave <- 
  df %>%
  unnest(slope_data) %>%
  # select "peak" POINTs, because they correspond to recession (falling) limb 
  filter(POINT == 'peak') %>%
  filter(!is.infinite(slope)) %>%
  # calculate average number of days between peak and first point of inflection
  # and the slope of recession limb 
  group_by(simulation, prop, flow_type, plotid, season) %>%
  summarise(ave_days = mean(days),
            # calculate trimmed, geometric and harmonic means since the distributions of slopes are right-skewed 
            ave_slope = mean(slope),
            ave_slope_trim = mean(slope, trim = 0.1),  # less conservative
            ave_slope_geom = -exp(mean(log(-slope))),  # more conservative
            ave_slope_harm = 1/(mean(1/slope))         # most conservative
  ) %>%
  ungroup() %>%
  select(simulation:ave_slope, -ave_days)



# Predict mssing data -----------------------------------------------------

df_pred <-
  df_model %>%
  # combine recession slope with flow data
  left_join(df_slopes_ave, by = c('simulation', 'prop', 'flow_type', 'plotid', 'season')) %>%
  filter(points > 3, !is.na(ave_slope)) %>%
  select(-data_filtered, -points) %>%
  gather(model_name, model, reg_model_3_day_avg:reg_model_3_day_weighted) %>%
  # add predictions of pick flows using actual data
  mutate(isnull = map_lgl(model, is.null),
         # predict pick flow only when there is a model available 
         data = ifelse(isnull == 1, 
                       data, 
                       map2(.x = model,
                            .y = data,
                            .f = ~ augment(.x, newdata = .y)))) %>%
  select(simulation:model_name) %>%
  unnest(data) %>% 
  arrange(simulation, prop, flow_type, model_name, plotid, date) %>%
  # resolve the problem of skipping first data points when rain < rain_limit
  # for example after missing dates of precip there is a first record of 0 rain, 
  # we need to keep prediction of corresponding tile flow of 0, so we can predict down the timeline
  group_by(simulation, prop, flow_type, model_name, plotid) %>%
  # find the first non-NA value for precip within each site-plot
  mutate(is_rain = is.na(rain_3),
         group = rep(seq_along(rle(is_rain)$length), rle(is_rain)$length)) %>%
  group_by(simulation, prop, flow_type, model_name, plotid, group) %>%
  # select only those first non-NA values when there was a measurement
  mutate(is_first = ifelse(row_number() == 1 & is_rain == FALSE, "Y", "N")) %>%
  ungroup() %>%
  # choose predictions for days when it was raining and no flow measured
  mutate(pred = ifelse(.fitted < 0, 0, .fitted),  # eliminate predicted negative flows
         flow_pred = ifelse(is.na(flow) & rain_3 > rain_limit, pred, flow),
         # add predictions for the first non-NA days when they are missing (due to rain < rain_limit)
         flow_pred = ifelse(is.na(flow_pred) & is_first == "Y", pred, flow_pred)) %>%
  select(simulation:rain_3, flow_pred, starts_with("ave")) %>%
  # predict flow of falling limb
  group_by(simulation, prop, flow_type, model_name, plotid) %>%
  mutate(LOGIC = ifelse(is.na(flow_pred), "Y", "N"),
         group = rep(seq_along(rle(LOGIC)$length), rle(LOGIC)$length)) %>%
  group_by(simulation, prop, flow_type, model_name, plotid, group) %>%
  mutate(count = 1:n(),
         count = ifelse(LOGIC == "N", 0, count)) %>%
  group_by(simulation, prop, flow_type, model_name, plotid) %>%
  # limit predicted flow to measured max tile flow
  mutate(flow_max = max(flow, na.rm = T),
         flow_pred = ifelse(flow_pred > flow_max, flow_max, flow_pred),
         flow_max = NULL) %>% 
  select(-group, -LOGIC) %>%
  # apply recession equation (Qi = Qi-1 * e^k, where k is seasonal recession slope)
  mutate(flow_pred2 = na.locf(flow_pred, na.rm = FALSE),
         # use arithmetic mean slope
         flow_pred2_mean = round(flow_pred2 * exp(ave_slope * count), 6),
         flow_pred = ifelse(is.na(flow_pred), flow_pred2_mean, flow_pred)) %>%
  select(-starts_with("flow_pred2"), -count) %>%
  # remove predicitoins of flow in days when there was no rainfall data (see CRAWF)
  mutate(flow_pred = ifelse(is.na(flow) & is.na(rain_3), NA, flow_pred)) %>%
  select(-starts_with("ave_slope"), -rain_3, -min_flow) %>%
  mutate(comments = ifelse(is.na(flow) & !is.na(flow_pred), 'predicted via recession model', comments)) %>%
  select(simulation, prop, flow_type, model_name, plotid, season, date, flow_pred, comments ) %>%
  group_by(simulation, prop, flow_type, model_name, plotid) %>%
  nest()