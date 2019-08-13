# This script imputes missing tile flow data based on our NEW METHOD

source(file = '00_project_settings.R')
library(zoo)



# function to calculate 3-day weighted rolling/moving average 
rollmean.weighted = function(x, W1 = 0.25, W2 = 0.45, W3 = 0.30) {
  if (sum(is.na(x)) == 3) {
    NA
  } else {
    replace(x[1], is.na(x[1]), 0) * W1 +
      replace(x[2], is.na(x[2]), 0) * W2 +
      replace(x[3], is.na(x[3]), 0) * W3 
  }
  sum(x*c(W1, W2, W3))
}



# Read Data ---------------------------------------------------------------

df <- read_rds('Data/Input_Data/RDS/original_input.rds')



# Determine rainfall threshold --------------------------------------------

rain_limit = 0.45  # NEED TO justify rainfall threshold
flow_limit = 0.30  # NEED TO justify

# On-site precipitation distribution
df %>%
  filter(rain > 0) %>%
  ggplot(aes(rain, col = siteid)) +
  geom_density() +
  geom_vline(aes(xintercept = mean(rain))) +
  theme_light() +
  coord_cartesian(xlim = c(0, 7.5))

# Rainfall to precipitation relationship
# but this should be EVENT-base
df %>%
  filter(rain > 0) %>%
  mutate(season = factor(quarter(date), labels = c("winter", "spring", "summer", "fall"))) %>%
  ggplot(aes(rain, flow, col = season)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = 'lm', se = F, col = 'black') +
  facet_grid(season ~ siteid) +
  theme_light() 
  


# Calculate 3-day moving average precipitation ----------------------------

# add 3-day moving ave precipitation 
tile_flow_data <- 
  df %>%
  group_by(siteid, plotid) %>%
  nest() %>%
  # assign reps
  group_by(siteid) %>%
  mutate(rep = 1:n_distinct(plotid)) %>%
  unnest() %>%
  # add season 
  mutate(season = factor(quarter(date), labels = c("winter", "spring", "summer", "fall")),
         rep = as.factor(rep)) %>%
  select(siteid, plotid, rep, year, season , date, flow, rain) %>%
  # calculate limiting tile flow as mean daily summer flow
  group_by(siteid, plotid, rep) %>%
  mutate(min_flow = mean(flow[season == "summer"], na.rm = TRUE),
         # correct limitting flow so it is not > 0.3 mm/day
         min_flow = ifelse(min_flow > flow_limit, flow_limit, min_flow)) %>%
  # calculate 3-day average precip
  group_by(siteid, plotid) %>%
  mutate(rain_3 = rollapplyr(rain, 3, mean, na.rm = TRUE, partial = TRUE),
         # calculated weighted moving average precip
         rain_3_weighted = rollapplyr(rain, width = 3, FUN = rollmean.weighted,
                                      partial = FALSE, fill = NA),
         # handle days which are out of the rolling range = first two recoreds in this case
         rain_3_weighted = ifelse(is.na(rain_3_weighted), rain_3, rain_3_weighted),
         # leave calculated 3-day avarage only for those days when it rained
         # OR rain was negligible (see the plot above)
         rain_3 = ifelse(rain > rain_limit, rain_3, 0),
         rain_3_weighted = ifelse(rain > rain_limit, rain_3_weighted, 0)) %>%
  group_by(siteid, plotid, rep) %>%
  nest()
  


# Develop "Seasonal" Regression model ------------------------------------- 
# predicting peak flows caused by precipitation event

# function to fit linear model
reg_model <- function(df) {
  lm(flow ~ rain_3 - 1, data = df)
}
reg_model_weighted <- function(df) {
  lm(flow ~ rain_3_weighted - 1, data = df)
}

tile_flow_reg_model <-
  tile_flow_data %>%
  # filter data to be used for regression 
  mutate(reg_data = map(.x = data,
                        .f = ~ .x %>%
                          # remove days when there was no rain OR tile flow was below minimum limit
                          filter(flow > min_flow, rain_3 > 0))) %>%
  # add season to nesting
  unnest(reg_data) %>%
  group_by(siteid, plotid, rep, season) %>%
  nest(.key = "reg_data") %>%
  # fit the model
  mutate(reg_model = map(reg_data, reg_model),
         reg_model_weighted = map(reg_data, reg_model_weighted))



# Calculate recession slope -----------------------------------------------
# slope of falling limb of the graph

# function to select peak and inflection points
peak_inflection <- function(df) {
  df %>%
    mutate(
      first =  ln_flow - lag(ln_flow),
      second = first - lag(first),
      point =  lead(ln_flow) - ln_flow,
      point1 = ifelse(is.na(point), "Pos", ifelse(point < 0, "Neg", "Pos")),
      point1a = ifelse(is.na(first), "Pos", ifelse(first < 0, "Neg", "Pos")),
      point2 = ifelse(is.na(second), "Pos", ifelse(second < 0, "Neg", "Pos"))
    ) %>%
    mutate(
      group1 = rep(seq_along(rle(point1)$length), rle(point1)$length),
      group1a = rep(seq_along(rle(point1a)$length), rle(point1a)$length),
      group2 = rep(seq_along(rle(point2)$length), rle(point2)$length)
    ) %>%
    # group1 = from START to END-1day or ressesion
    group_by(group1) %>%
    mutate(POINT = ifelse(point1 == "Neg" &
                            row_number() == 1, "peak", NA)) %>%
    group_by(group2) %>%
    mutate(POINT = ifelse(point2 == "Neg" &
                            row_number() == n(), "inf", POINT)) %>%
    ungroup() %>%
    select(-point1,-point1a,-point2) %>%
    mutate(group = ifelse(POINT == "peak", group1, group1a)) %>%
    select(-group1,-group1a,-group2) %>%
    filter(!is.na(POINT)) %>%  
    # remove single peaks|ubflections and double inflections
    group_by(group) %>%
    mutate(n = n(),
           count = 1:n()) %>%
    ungroup() %>%
    filter(n != 1,
           count < 3) %>%
    select(year, season, date, flow, rain, ln_flow, group, POINT) %>%
    # calculate duration of PEAK-TO-INFLECTION period and slope
    mutate(days_bw_pni = lead(date) - date,
           days = as.numeric(days_bw_pni),
           change = (lead(ln_flow) - ln_flow),
           slope = change/days)
}

# calculate average recession slope by season
recession_slope <-
  tile_flow_data %>%
  # log-transform flow data 
  mutate(data = map(.x = data, 
                    .f = ~ .x %>% 
                      # make sure to handle log(0) 
                      mutate(ln_flow = ifelse(flow == 0, NA, log(flow))))) %>%
  # find PEAK and INFLECTION points
  mutate(data = map(data, peak_inflection)) %>%
  unnest() %>%
  # select "peak" POINT to calculate slope of recessuin (falling) limb 
  filter(POINT == 'peak') %>%
  filter(!is.infinite(slope)) %>%
  # save intermediate data for plotting in Global Environment
  {. ->> recession_slope_dist} %>%
  # calculate average number of days between peak and first point of inflection
  # and the slope of recession limb 
  group_by(siteid, plotid, season) %>%
  summarise(ave_days = mean(days),
            # calculate trimmed, geometric and harmonic means since the distributions of slopes are right-skewed 
            ave_slope = mean(slope),
            ave_slope_trim = mean(slope, trim = 0.1),  # less conservative
            ave_slope_geom = -exp(mean(log(-slope))),  # more conservative
            ave_slope_harm = 1/(mean(1/slope))         # most conservative
  ) %>%
  ungroup()



# Predict mssing data -----------------------------------------------------

tile_flow_pred <-
  tile_flow_data %>%
  unnest() %>%
  group_by(siteid, plotid, rep, season) %>%
  nest() %>%
  # combine regression model and recession slope with flow data
  left_join(tile_flow_reg_model, by = c("siteid", "plotid", "rep", "season")) %>%
  left_join(recession_slope[1:8], by = c("siteid", "plotid", "season")) %>%
  select(-reg_data, -ave_days) %>% 
  gather(model_name, model, reg_model:reg_model_weighted) %>%
  # add predictions of pick flows using actual data
  mutate(isnull = map_lgl(model, is.null),
         # predict pick flow only when there is a model available 
         data = ifelse(isnull == 1, 
                       data, 
                       map2(.x = model,
                            .y = data,
                            .f = ~ augment(.x, newdata = .y)))) %>%
  select(siteid:model_name, starts_with("ave")) %>%
  unnest(data) %>% 
  # resolve the problem of skipping first data points when rain < rain_limit
  # for example after missing dates of precip there is a first record of 0 rain, 
  # we need to keep prediction of corresponding tile flow of 0, so we can predict down the timeline
  group_by(model_name, siteid, plotid) %>%
  # find the first non-NA value for precip within each site-plot
  mutate(is_rain = is.na(rain),
         group = rep(seq_along(rle(is_rain)$length), rle(is_rain)$length)) %>%
  group_by(model_name, siteid, plotid, group) %>%
  # select only those first non-NA values when there was a measurement
  mutate(is_first = ifelse(row_number() == 1 & is_rain == FALSE, "Y", "N")) %>%
  ungroup() %>%
  # choose predictions for days when it was raining and no flow measured
  mutate(pred = ifelse(.fitted < 0, 0, .fitted),  # eliminate predicted negative flows
         flow_pred = ifelse(is.na(flow) & rain > rain_limit, pred, flow),
         # add predictions for the first non-NA days when they are missing (due to rain < rain_limit)
         flow_pred = ifelse(is.na(flow_pred) & is_first == "Y", pred, flow_pred)) %>%
  select(siteid:rain, flow_pred, starts_with("ave")) %>%
  # predict flow of falling limb
  group_by(siteid, plotid, model_name) %>%
  mutate(LOGIC = ifelse(is.na(flow_pred), "Y", "N"),
         group = rep(seq_along(rle(LOGIC)$length), rle(LOGIC)$length)) %>%
  group_by(siteid, plotid, model_name, group) %>%
  mutate(count = 1:n(),
         count = ifelse(LOGIC == "N", 0, count)) %>%
  group_by(siteid, plotid, model_name) %>%
  # limit predicted flow to measured max tile flow
  mutate(flow_max = max(flow, na.rm = T),
         flow_pred = ifelse(flow_pred > flow_max, flow_max, flow_pred),
         flow_max = NULL) %>% 
  select(-group, -LOGIC) %>%
  # apply recession equation (Qi = Qi-1 * e^k, where k is seasonal recession slope)
  mutate(flow_pred2 = na.locf(flow_pred, na.rm = FALSE),
         # use arithmetic mean slope
         flow_pred2_mean = round(flow_pred2 * exp(ave_slope * count), 6),
         # use trimmed mean slope
         flow_pred2_trim = round(flow_pred2 * exp(ave_slope_trim * count), 6),
         # use geometric mean slope
         flow_pred2_geom = round(flow_pred2 * exp(ave_slope_geom * count), 6),
         flow_pred_mean = ifelse(is.na(flow_pred), flow_pred2_mean, flow_pred),
         flow_pred_trim = ifelse(is.na(flow_pred), flow_pred2_trim, flow_pred),
         flow_pred_geom = ifelse(is.na(flow_pred), flow_pred2_geom, flow_pred)) %>%
  select(-starts_with("flow_pred2"), -count) %>%
  # remove predicitoins of flow in days when there was no rainfall data (see CRAWF)
  mutate(flow_pred_mean = ifelse(is.na(flow) & is.na(rain), NA, flow_pred_mean),
         flow_pred_trim = ifelse(is.na(flow) & is.na(rain), NA, flow_pred_trim),
         flow_pred_geom = ifelse(is.na(flow) & is.na(rain), NA, flow_pred_geom)) %>%
  select(-starts_with("ave_slope"))


# List years with predicted data for each site
tile_flow_pred %>%
  filter(model_name == "reg_model_weighted") %>%
  filter(is.na(flow) & !is.na(flow_pred_mean)) %>%
  ungroup() %>% count(siteid, year, plotid)

# Count number of missing days in each site-plot-season
tile_flow_pred %>%
  arrange(model_name, siteid, plotid, date) %>%
  filter(model_name == 'reg_model_weighted') %>%
  mutate(season = quarter(date)) %>%
  group_by(siteid, plotid, year, season) %>%
  summarise(total = n(), 
            missing = sum(is.na(flow_pred_trim)),
            missing_percent = missing/total) %>%
  filter(year == 2009, plotid == "SW") %>%
  ungroup() %>%
  select(-total, -missing) %>%
  spread(plotid, missing_percent)

# Count number of predicted values
tile_flow_pred %>%
  arrange(model_name, siteid, plotid, date) %>%
  filter(model_name == 'reg_model_weighted') %>%
  mutate(season = quarter(date),
         pred = ifelse(is.na(flow) & !is.na(flow_pred_trim), "predicted", NA )) %>%
  group_by(siteid, plotid, year) %>%
  summarise(total = n(), 
            total_pred = sum(!is.na(pred))) %>% 
  ungroup() %>%
  filter(total_pred > 0) %>%
  arrange(desc(total_pred)) ->
  tile_flow_number_of_missing_days


# If number of predicted days per plot exceeds 152 (5 months) in a calendar year, remove predictions
# Based on the above code predictions for folowing site-plot-years should be removed
tile_flow_number_of_missing_days %>%
  filter(total_pred > 152) 
#   SiteID     PlotID    Year    Predicted    
#   SERF_IA      S2      2010       365
#   SERF_IA      S2      2009       301


# Select predicted data ---------------------------------------------------

tile_flow_pred %>% 
  ungroup() %>%
  filter(model_name == 'reg_model_weighted') %>%
  select(siteid, plotid, rep, year, date, rain, flow, flow_pred_trim) %>%
  # remove predictions for plot-years when the whole year was missing
  left_join(tile_flow_number_of_missing_days %>%
              filter(total_pred > 152), 
            by = c('siteid', 'plotid', 'year')) %>%
  mutate(flow_pred = ifelse(is.na(total_pred), flow_pred_trim, flow)) %>%
  select(-total, -total_pred) %>%
  # comment predicted values that were left after above filter
  mutate(comments = ifelse(is.na(flow) & !is.na(flow_pred), "predicted", "")) %>%
  # comment predicted values at STEP3
  left_join(df %>% 
              filter(!is.na(comments)) %>% 
              select(siteid, plotid, date, comments) %>%
              rename(comm = comments),
            by = c('siteid', 'plotid', 'date')) %>%
  mutate(comments = ifelse(!is.na(comm), comm, comments)) %>%
  select(-flow_pred_trim, - comm) %>%
  ungroup() -> tile_flow_prediction



