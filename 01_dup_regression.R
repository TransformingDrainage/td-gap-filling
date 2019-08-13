# This script fits linear regression model to replicated study data and 
# predictes missing value in one of the reps when missing

source(file = '00_project_settings.R')



# Read Data ---------------------------------------------------------------

df <- read_csv('Data/Input_Data/daily_tile_flow.csv')

# assign reps numbers (arbitrary)
reps <- 
  df %>%
  distinct(siteid, plotid, dwm) %>%
  group_by(siteid, dwm) %>%
  mutate(rep = 1:n()) %>%
  ungroup() %>%
  select(siteid, plotid, dwm, rep) %>%
  mutate(rep = paste0("rep_", rep))

# add replication numbers
tile_flow_reps <-
  df %>%
  # assign rep numbers
  left_join(reps, by = c("siteid", "plotid", "dwm")) %>%
  select(-year, -plotid, -precip_on_site) %>%
  spread(rep, flow)



# Predict missing data using replicates -----------------------------------

# function to fit linear model to replicated plots
rep_model_1 <- function(df) {
  add_predictions(data = df, 
                  model = lm(rep_1 ~ rep_2 - 1, data = df), 
                  var = "pred_1")
}

rep_model_2 <- function(df) {
  add_predictions(data = df, 
                  model = lm(rep_2 ~ rep_1 - 1, data = df), 
                  var = "pred_2")
}

# predict missing values by fiting models
tile_flow_fitted <- 
  tile_flow_reps %>%
  # add season as factor
  mutate(season = factor(quarter(date), labels = c("winter", "spring", "summer", "fall"))) %>%
  group_by(siteid, dwm, season) %>% 
  nest() %>%
  # add predictions 
  mutate(data = map(data, rep_model_1),
         data = map(data, rep_model_2)) %>%
  unnest(data) %>%
  select(-season) %>%
  # transform table so predictions are next to acctual measurements
  gather(key = key, value = flow, rep_1:pred_2) %>%
  separate(key, into = c("key", "rep_number")) %>%
  spread(key, flow) %>%
  rename(flow = rep, rep = rep_number) %>%
  # assign plot ids
  mutate(rep = paste0("rep_", rep)) %>%
  left_join(reps, by = c("siteid", "rep", "dwm")) %>%
  select(siteid, plotid, dwm, date, flow, flow_pred = pred) %>%
  # remove predictions of reps at DPAC in 2017
  mutate(flow_pred = ifelse(siteid == "DPAC" & year(date) == 2017, NA, flow_pred)) %>%
  # remove predictions of reps at S2 in SERF_IA in 2009-2010
  mutate(flow_pred = ifelse(siteid == "SERF_IA" & year(date) %in% 2009:2010, NA, flow_pred)) %>%
  
  # add comments for predicted values
  mutate(comments = ifelse(is.na(flow) & !is.na(flow_pred), "predicted via rep regression", NA),
         # replace predicted measurements with acctual 
         flow_pred = ifelse(is.na(comments), flow, flow_pred)) %>%
  arrange(siteid, plotid, date)



# Plot prediction models --------------------------------------------------

# Linear regression models and R2 by Season
model = y ~ x - 1
tile_flow_reps %>%
  # add season as factor
  mutate(season = factor(quarter(date), labels = c("winter", "spring", "summer", "fall"))) %>%
  ggplot(aes(rep_1, rep_2, col = season)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = 'lm', formula = model, se = FALSE, col = 'black') +
  ggpmisc::stat_poly_eq(formula = model,
                        eq.with.lhs = "italic(hat(y))~`=`~",
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                        label.y = 17, 
                        col = 'black',
                        parse = TRUE) + 
  facet_grid(season ~ siteid) +
  labs(col = 'Season') +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        text = element_text(size = 12))
ggsave('Figs/rep-regression/rep_regression_seasonal_models.png',
       width = 16, height = 10)

# DPAC predictions
tile_flow_fitted %>%
  mutate(year = year(date)) %>%
  filter(siteid == "DPAC" & year == 2016) %>%
  mutate(plotid = factor(plotid, levels = c('SW', 'NE'))) %>%
  ggplot(aes(x = date, group = plotid)) +
  geom_point(aes(y = flow_pred)) +
  geom_point(aes(y = flow), colour = 'slateblue') + 
  geom_line(aes(y = flow), colour = 'slateblue', alpha = 0.25) +
  labs(x = NULL, y = 'Tile Flow, mm',
       title = 'DPAC 2016',
       subtitle = 'Daily Drainage Predictions based on Replicated Plot Data') +
  facet_grid(plotid ~ .) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        text = element_text(size = 12))
ggsave('Figs/rep-regression/DPAC_tile_flow_predictions_2016.png',
       width = 16, height = 10)

# SERF_IA has no predictions for FD plots



# Save predicted data -----------------------------------------------------

tile_flow_fitted %>%
  # as an input for new Imputation Method
  write_csv("Data/Input_Data/daily_tile_flow_with_dup_regression.csv")


