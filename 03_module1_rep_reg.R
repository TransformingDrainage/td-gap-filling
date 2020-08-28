# Module 1 - Replicate Regression
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
    select(-data) %>%
    gather(model, fit, model_1:model_2) %>%
    mutate(glance = map(fit, glance)) %>%
    select(-fit) 
  
  # Select predictions for missing data only and transform the table
  df_pred_module1 <- 
    df_model %>% 
    select(-starts_with('model')) %>%
    unnest(data) %>%
    gather(key, value, rep_1:pred_2) %>%
    full_join(dpac_keys) %>%
    select(-key) %>%
    spread(var, value) %>% 
    mutate(comments = ifelse(is.na(flow_amp) & !is.na(flow_pred), "predicted via rep regression", NA),
           # replace predicted measurements with acctual 
           # NOTE: predictions were made for every point, including those that that were not amputated
           flow_pred = ifelse(is.na(comments), flow_amp, flow_pred)) %>%
    arrange(simulation, plotid, date)
  
  # Save rep-reg predicted data
  df_pred_module1 %>%
    group_by(simulation, prop, plotid, season) %>%
    nest() %>%
    write_rds(paste0('Data/Inter_Data/Phase2_Imputation/DPAC/', 
                     str_extract(i, pattern = 'DPAC_Y.{4}'),
                     '_pred_phase2.rds'), 
              compress = 'xz')
}



# Plot prediction models --------------------------------------------------

# Rep reg model parameters
dpac_model_performance <- 
  df_model_performance %>%
  bind_rows(.id = 'id') %>%
  mutate(years = str_extract(id, pattern = 'Y.')) %>%
  select(years, prop, simulation, season, model, glance) %>%
  unnest(glance)

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




# OLD CODE > NEED TO BE UPDATED -------------------------------------------

# Linear regression models and R2 by season at 5 different prop
model = y ~ x - 1
df_pred_module1[c(11, 222, 555, 777, 999), ] %>%
  unnest() %>%
  select(-flow_pred) %>%
  spread(plotid, flow_amp) %>%
  ggplot(aes(NE, SW, col = season, group = prop)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = 'lm', formula = model, se = FALSE, col = 'black') +
  ggpmisc::stat_poly_eq(formula = model,
                        eq.with.lhs = "italic(hat(y))~`=`~",
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                        label.y = 12, label.x = 4, 
                        col = 'black',
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
ggsave('Figs/models/rep_regression_seasonal_models.png',
       width = 16, height = 10)

# Predictions
df_pred_module1[c(11, 222, 555, 777, 999), ] %>%
  unnest() %>%
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
ggsave('Figs/predictions/rep_reg_predictions_2016.png',
       width = 16, height = 10)


# Predictions
df_pred_module1[c(11, 555, 999), ] %>%
  unnest() %>%
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
ggsave('Figs/predictions/rep_reg_predictions_2016_Jan_Jun.png',
       width = 16, height = 10)


