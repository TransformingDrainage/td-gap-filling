# Module 1 - Replicate Regression
# This scripts predicts missing values using measurements from the replicated study

source(file = '00_project_settings.R')



# Prepare Data ------------------------------------------------------------

df <- read_rds('Data/Input_Data/RDS/DPAC_amp_MAR.rds') %>% 
  bind_rows(.id = 'simulation') %>%
  select(simulation, prop, date, 
         rep_1 = NE, 
         rep_2 = SW) %>%  # add season as factor
  mutate(season = factor(quarter(date), labels = c("Winter", "Spring", "Summer", "Fall"))) %>%
  group_by(simulation, prop, season) %>%
  nest()



# Predict Using Rep Models ------------------------------------------------

# Create functions to fit models
rep_1_model <- function(df) {
  lm(rep_1 ~ rep_2 - 1, data = df)
  }

rep_2_model <- function(df) {
  lm(rep_2 ~ rep_1 - 1, data = df)
  }

# Fit the models and get predictions
df_model <- df %>% 
  mutate(model_1 = map(data, rep_1_model),
         model_2 = map(data, rep_2_model)) %>%
  mutate(data = map2(data, model_1, add_predictions, var = 'pred_1'),
         data = map2(data, model_2, add_predictions, var = 'pred_2'))


df_pred_module1 <- df_model %>% 
  unnest(data) %>%
  group_by(simulation, prop) %>%
  nest() %>%
  # transform table so predictions are next to acctual measurements
  mutate(data = map(data, ~ .x %>%
                      gather(key = key, value = flow_amp, rep_1:pred_2) %>%
                      separate(key, into = c("key", "rep_number")) %>%
                      spread(key, flow_amp) %>%
                      rename(flow_amp = rep, rep = rep_number, flow_pred = pred) %>%
                      mutate(plotid = ifelse(rep == 1, "NE", "SW")) %>%
                      mutate(comments = ifelse(is.na(flow_amp) & !is.na(flow_pred), "predicted via rep regression", NA),
                             # replace predicted measurements with acctual 
                             flow_pred = ifelse(is.na(comments), flow_amp, flow_pred)) %>%
                      arrange(plotid, date) %>%
                      select(plotid, season, date, flow_amp, flow_pred, comments)))
  
df_pred_module1$data[[1]] %>%
  filter(is.na(flow_pred))


# Plot prediction models --------------------------------------------------

# Rep reg model parameters
df_model_performance <- df_model %>%
  gather(model, fit, model_1:model_2) %>%
  mutate(glance = map(fit, glance)) %>%
  select(-data, -fit) %>%
  unnest(glance)

df_model_performance %>%
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


# Save rep-reg predicted data ---------------------------------------------
df_pred_module1 %>%
  unnest() %T>%
  write_csv('Data/Input_Data/DPAC_amp_MAR_pred_module1.csv') %>%
  group_by(simulation, prop, plotid, season) %>%
  nest() %>%
  write_rds('Data/Input_Data/RDS/DPAC_amp_MAR_pred_module1.rds')




