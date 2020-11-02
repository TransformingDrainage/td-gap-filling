# Phase 2 - Replicate Regression
# This scripts predicts missing values using measurements from the replicated study

source(file = '00_project_settings.R')



# Create functions to fit models ------------------------------------------
rep_1_model <- function(df) {
  lm(rep_1 ~ rep_2 - 1, data = df)
}

rep_2_model <- function(df) {
  lm(rep_2 ~ rep_1 - 1, data = df)
}



# DPAC --------------------------------------------------------------------
# Prepare Data
dpac_keys <- tribble(~key,       ~plotid,   ~var,
                     "rep_1",     "NE",     "flow_amp",
                     "rep_2",     "SW",     "flow_amp",
                     "pred_1",    "NE",     "flow_pred",
                     "pred_2",    "SW",     "flow_pred")

dpac_files <- list.files('Data/Inter_Data/Amputated_Subsets/DPAC/', full.names = TRUE)

df_model_performance <- vector('list')



# Predict Using Rep Models ------------------------------------------------
for (i in dpac_files) {
  # Read amputated flow data
  df <- read_rds(i) %>%
    select(simulation, prop, date, 
           rep_1 = NE, 
           rep_2 = SW) %>%  # add season as factor
    mutate(season = factor(quarter(date), labels = c("Winter", "Spring", "Summer", "Fall"))) %>%
    group_by(simulation, prop, season) %>%
    nest()
  
  # Fit the models and get predictions
  df_model <- df %>% 
    mutate(model_1 = map(data, rep_1_model),
           model_2 = map(data, rep_2_model)) %>%
    mutate(data = map2(data, model_1, add_predictions, var = 'pred_1'),
           data = map2(data, model_2, add_predictions, var = 'pred_2'))
  
  # Rep reg model parameters
  df_model_performance[[i]] <- df_model %>%
    ungroup() %>%
    select(-data) %>%
    gather(model, fit, model_1:model_2) %>%
    # handle cases when model cannot be fitted (https://aosmith.rbind.io/2020/08/31/handling-errors/)
    # e.g. one rep has only 0 flows during the season, has no linear model fitted 
    mutate(glance = map(fit, possibly(glance, otherwise = NULL))) %>%
    mutate(map_df(fit, tidy)) %>%
    select(-fit) 
  
  # Select predictions for missing data only and transform the table
  df_pred_phase2 <- 
    df_model %>% 
    select(-starts_with('model')) %>%
    unnest(data) %>%
    gather(key, value, rep_1:pred_2) %>%
    full_join(dpac_keys) %>%
    select(-key) %>%
    spread(var, value) %>% 
    mutate(comments = ifelse(is.na(flow_amp) & !is.na(flow_pred), 
                             "predicted via rep regression", NA),
           # replace predicted measurements with actual 
           # NOTE: predictions were made for every point, 
           # including those that that were not amputated
           flow_pred = ifelse(is.na(comments), flow_amp, flow_pred)) %>%
    arrange(simulation, plotid, date)
  
  # Save rep-reg predicted data
  df_pred_phase2 %>%
    group_by(simulation, prop, plotid, season) %>%
    nest() %>%
    write_rds(paste0('Data/Inter_Data/Phase2_Imputation/DPAC/', 
                     str_extract(i, pattern = 'DPAC_Y.{4}'),
                     '_pred_phase2.rds'), 
              compress = 'xz')
}

# Save model fit parameters
df_model_performance %>%
  bind_rows(.id = 'id') %>%
  mutate(years = str_extract(id, pattern = 'Y.')) %>%
  select(years, prop, simulation, season, model, glance,
         # rename model fit variable names 
         parameter_term = term, 
         parameter_estimate = estimate,
         parameter_std.error = std.error,
         parameter_statistic = statistic,
         parameter_p.value = p.value) %>%
  unnest(glance) %>%
  select(-c(statistic, p.value, df)) %>%
  write_rds('Data/Inter_Data/Phase2_Imputation/DPAC_Phase2_model_parameters.rds',
            compress = 'xz')



# Plot prediction models --------------------------------------------------
# Read model fit data
dpac_model_performance <- 
  read_rds('Data/Inter_Data/Phase2_Imputation/DPAC_Phase2_model_parameters.rds')

# Plot R^2 of the fitted models
dpac_model_performance %>%
  arrange(simulation) %>%
  filter(model == "model_1") %>%
  ggplot(aes(x=as.factor(prop), y=r.squared)) +
  geom_boxplot() +
  facet_grid(season ~ .) +
  labs(x = 'Proportion of missing data',
       y = 'R^2 of model',
       title = "Destribution of R^2 of Replicate Regression Models by Season") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        text = element_text(size = 12))
ggsave('Figs/models/rep_regression_model_rsquares.png',
       width = 16, height = 10)


# Read 5 subsets of data from complete 10-year scenario
dpac_subset_YA <- 
  bind_rows(
    read_rds('DAta/Inter_Data/Phase2_Imputation/DPAC/DPAC_YA_05_pred_phase2.rds') %>%
      filter(simulation == 11),
    read_rds('DAta/Inter_Data/Phase2_Imputation/DPAC/DPAC_YA_15_pred_phase2.rds') %>%
      filter(simulation == 22),
    read_rds('DAta/Inter_Data/Phase2_Imputation/DPAC/DPAC_YA_25_pred_phase2.rds') %>%
      filter(simulation == 55),
    read_rds('DAta/Inter_Data/Phase2_Imputation/DPAC/DPAC_YA_35_pred_phase2.rds') %>%
      filter(simulation == 77),
    read_rds('DAta/Inter_Data/Phase2_Imputation/DPAC/DPAC_YA_45_pred_phase2.rds') %>%
      filter(simulation == 99)
  )
# Plot fitted linear model for each subset 
# showing the linear models fitted 
model = y ~ x - 1
dpac_subset_YA %>% 
  unnest(data) %>% 
  select(-flow_pred, -comments) %>% 
  spread(plotid, flow_amp) %>%
  ggplot(aes(NE, SW, col = season, group = prop)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = 'lm', formula = model, se = FALSE, col = 'black') +
  ggpmisc::stat_poly_eq(formula = model,
                        eq.with.lhs = "italic(hat(y))~`=`~",
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                        label.y = 0.95, label.x = .1, 
                        col = 'black', face = 'bold',
                        parse = TRUE) + 
  facet_grid(season ~ prop) +
  labs(col = 'Season') +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        text = element_text(size = 12))
ggsave('Figs/models/rep_regression_seasonal_models_5_subsets_from_YA.png',
       width = 16, height = 10)

# Phase 2 predictions for 2016
dpac_subset_YA %>%
  unnest(data) %>%
  filter(year(date) == 2016) %>%
  ggplot(aes(x = date, group = plotid)) +
  geom_point(data = . %>% filter(!is.na(comments)), 
             aes(y = flow_pred), colour = 'orange2') +
  geom_point(aes(y = flow_amp), size = 0.75, alpha = 0.75) + 
  geom_line(aes(y = flow_amp), alpha = 0.25) +
  scale_x_date(date_labels = '%b') +
  labs(x = NULL, y = 'Tile Flow, mm',
       subtitle = 'Year 2016',
       title = 'Daily Drainage Predictions based on Replicate Pregression Model') +
  facet_grid(prop ~ plotid) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        text = element_text(size = 12))
ggsave('Figs/predictions/Phase2_predictions_2016.png',
       width = 16, height = 10)

# Phase 2 predictions from Jan to Jun in 2016 
# only for 5, 25, and 45% missing data
dpac_subset_YA %>%
  filter(prop %in% c(0.05, 0.25, 0.45)) %>%
  unnest(data) %>%
  filter(year(date) == 2016 & month(date) < 7) %>%
  ggplot(aes(x = date, group = plotid)) +
  geom_point(data = . %>% filter(!is.na(comments)), 
             aes(y = flow_pred), colour = 'orange2', size = 2) +
  geom_point(aes(y = flow_amp), size = 1, alpha = 0.75) + 
  geom_line(aes(y = flow_amp), alpha = 0.25) +
  scale_x_date(date_labels = '%b') +
  labs(x = NULL, y = 'Tile Flow, mm',
       subtitle = 'January-June 2016',
       title = 'Daily Drainage Predictions based on Replicate Pregression Model') +
  facet_grid(prop ~ plotid) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        text = element_text(size = 16))
ggsave('Figs/predictions/Phase2_predictions_2016_Jan_Jun.png',
       width = 16, height = 10)


