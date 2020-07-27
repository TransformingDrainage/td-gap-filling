# This script evaluates accuracy of imputed data

source(file = '00_project_settings.R')


# Read Data ---------------------------------------------------------------

df_Y2 <- read_rds('Data/Input_Data/RDS/DPAC_amp_MAR_imputed_data_Y2.rds')
df_Y4 <- read_rds('Data/Input_Data/RDS/DPAC_amp_MAR_imputed_data_Y4.rds')
df_Y6 <- read_rds('Data/Input_Data/RDS/DPAC_amp_MAR_imputed_data_Y6.rds')
df_Y11 <- read_rds('Data/Input_Data/RDS/DPAC_amp_MAR_imputed_data_Y11.rds')

df_original <- 
  read_rds('Data/Input_Data/RDS/daily_tile_flow_complete.rds') %>%
  select(-year, -comments, -siteid, -rain)


# Filter Data to select only imputed values
Y2 <- 
  df_Y2 %>%
  unnest(data) %>%
  filter(!is.na(comments)) %>%
  left_join(df_original, by = c('plotid', 'date')) %>%
  # calculate RMSE
  mutate(SE = (flow - flow_pred)^2) %>%
  group_by(simulation, prop, flow_type, model_name, plotid) %>%
  summarise(RMSE = sqrt(sum(SE)/n()))


Y4 <- 
  df_Y4 %>%
  unnest(data) %>%
  filter(!is.na(comments)) %>%
  left_join(df_original, by = c('plotid', 'date')) %>%
  # calculate RMSE
  mutate(SE = (flow - flow_pred)^2) %>%
  group_by(simulation, prop, flow_type, model_name, plotid) %>%
  summarise(RMSE = sqrt(sum(SE)/n()))


Y6 <- 
  df_Y6 %>%
  unnest(data) %>%
  filter(!is.na(comments)) %>%
  left_join(df_original, by = c('plotid', 'date')) %>%
  # calculate RMSE
  mutate(SE = (flow - flow_pred)^2) %>%
  group_by(simulation, prop, flow_type, model_name, plotid) %>%
  summarise(RMSE = sqrt(sum(SE)/n()))


Y11 <- 
  df_Y11 %>%
  unnest(data) %>%
  filter(!is.na(comments)) %>%
  left_join(df_original, by = c('plotid', 'date')) %>%
  # calculate RMSE
  mutate(SE = (flow - flow_pred)^2) %>%
  group_by(simulation, prop, flow_type, model_name, plotid) %>%
  summarise(RMSE = sqrt(sum(SE)/n()))




# PLOT

Y2 %>%
  ungroup() %>%
  ggplot(aes(as.factor(prop), RMSE, col = plotid)) +
  geom_boxplot() +
  facet_grid(flow_type ~ model_name) +
  theme_light()


Y4 %>%
  ungroup() %>%
  ggplot(aes(as.factor(prop), RMSE, col = plotid)) +
  geom_boxplot() +
  facet_grid(flow_type ~ model_name) +
  theme_light()


Y6 %>%
  ungroup() %>%
  ggplot(aes(as.factor(prop), RMSE, col = plotid)) +
  geom_boxplot() +
  facet_grid(flow_type ~ model_name) +
  theme_light()


Y11 %>%
  ungroup() %>%
  ggplot(aes(as.factor(prop), RMSE, col = plotid)) +
  geom_boxplot() +
  facet_grid(flow_type ~ model_name) +
  theme_light()




# Plot for Presentation ---------------------------------------------------

# RMSE for base model (3-day-average with step 3+4 for 11 years)
Y11 %>%
  ungroup() %>%
  filter(model_name == 'reg_model_3_day_avg',
         flow_type == 'flow_pred') %>%
  ggplot(aes(as.factor(prop), RMSE)) +
  geom_boxplot() +
  facet_grid(. ~ plotid) +  
  labs(title = 'RMSE of Predictions Made at Step 3 and 4 ',
       subtitle = 'Complete Dataset (11 years)',
       x = 'Proportion of Missing Data') +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        text = element_text(size = 16))
ggsave('Figs/predictions/RMSE_3_day_ave_boxplot.png',
       width = 16, height = 10)

# RMSE for models w/ and w/o step 3 (3-day-average for 11 years)
Y11 %>%
  ungroup() %>%
  filter(model_name == 'reg_model_3_day_avg') %>%
  mutate(flow_type = factor(flow_type,
                            levels = c('flow_pred', 'flow_amp'),
                            labels = c('Steps 3 + 4', 'Step 4'))) %>%
  ggplot(aes(as.factor(prop), RMSE, col = flow_type)) +
  geom_boxplot(size = 1) +
  facet_grid(. ~ plotid) +  
  labs(title = 'RMSE of Predictions with and without Step 3',
       subtitle = 'Complete Dataset (11 years)',
       x = 'Proportion of Missing Data',
       col = 'Prediction Steps') +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        text = element_text(size = 16))
ggsave('Figs/predictions/RMSE_3_day_ave_boxplot_BY_steps.png',
       width = 16, height = 10)


# RMSE for models with different averaging (step 3+4 for 11 years)
Y11 %>%
  ungroup() %>%
  filter(flow_type == 'flow_pred') %>%
  mutate(model_name = factor(model_name,
                            levels = c('reg_model_3_day_avg', 'reg_model_3_day_weighted'),
                            labels = c('3-day average', '3-day weighted average'))) %>%
  ggplot(aes(as.factor(prop), RMSE, col = model_name)) +
  geom_boxplot(size = 1) +
  facet_grid(. ~ plotid) +  
  labs(title = 'RMSE of Predictions with Different Rolling Averages',
       subtitle = 'Complete Dataset (11 years)',
       x = 'Proportion of Missing Data',
       col = 'Precipitation Averaging') +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        text = element_text(size = 16))
ggsave('Figs/predictions/RMSE_with_step_3_boxplot_BY_averaging.png',
       width = 16, height = 10)



# Look at seasonal distibution of error
Y11_seasonal <- 
  df_Y11 %>%
  unnest(data) %>%
  filter(!is.na(comments)) %>%
  left_join(df_original, by = c('plotid', 'date')) %>%
  # calculate RMSE
  mutate(SE = (flow - flow_pred)^2) %>%
  group_by(simulation, prop, flow_type, model_name, plotid, season) %>%
  summarise(RMSE = sqrt(sum(SE)/n()))


Y11_seasonal %>%
  ungroup() %>%
  filter(flow_type == 'flow_pred') %>%    # select data that prdicted by both modules
  filter(model_name == 'reg_model_3_day_avg') %>%
  ggplot(aes(as.factor(prop), RMSE)) +
  geom_boxplot(size = 1) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed", col = 'red') +
  facet_grid(plotid ~ season) +
  labs(title = 'RMSE of Predictions by Season',
       subtitle = 'Complete Dataset (11 years)',
       x = 'Proportion of Missing Data') +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        text = element_text(size = 16))
ggsave('Figs/predictions/RMSE_with_step_3_boxplot_BY_season.png',
       width = 16, height = 10)



# see how number of years effect predictions
list(Y2=Y2, Y4=Y4, Y6=Y6, Y11=Y11) %>%
  bind_rows(.id = 'years') %>%
  ungroup() %>%
  mutate(years = factor(years, levels = c('Y2', 'Y4', 'Y6', 'Y11'))) %>%
  filter(model_name == 'reg_model_3_day_avg',
         flow_type == 'flow_pred') %>%
  ggplot(aes(years, RMSE)) +
  geom_boxplot(size = 1) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed", col = 'red') +
  facet_grid(plotid ~ prop) +
  labs(title = 'RMSE for Models with Different Number of Years',
       subtitle = 'Step 3+4 with 3-day rolling average precipitation',
       x = 'Number of Available Years') +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        text = element_text(size = 16))
ggsave('Figs/predictions/RMSE_with_step_3_boxplot_BY_years.png',
       width = 16, height = 10)


list(Y2=Y2, Y4=Y4, Y6=Y6, Y11=Y11) %>%
  bind_rows(.id = 'years') %>%
  ungroup() %>%
  mutate(years = factor(years, levels = c('Y2', 'Y4', 'Y6', 'Y11'))) %>%
  filter(model_name == 'reg_model_3_day_avg',
         flow_type == 'flow_pred') %>%
  group_by(years, prop, plotid) %>%
  summarise(RMSE = mean(RMSE)) %>%
  ggplot(aes(x=prop, y=RMSE, group = years)) +
  geom_point() +
  geom_line(aes(linetype = years)) +
  facet_grid(~ plotid) +
  labs(title = 'Average RMSE for Models with Different Number of Years',
       subtitle = 'Step 3+4 with 3-day rolling average precipitation',
       linetype = 'Years',
       x = 'Proportion of Missing Data') +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        text = element_text(size = 16))
ggsave('Figs/predictions/RMSE_with_step_3_line_BY_prop.png',
       width = 16, height = 10)


list(Y2=Y2, Y4=Y4, Y6=Y6, Y11=Y11) %>%
  bind_rows(.id = 'years') %>%
  ungroup() %>%
  mutate(years = factor(years, levels = c('Y2', 'Y4', 'Y6', 'Y11'))) %>%
  filter(model_name == 'reg_model_3_day_avg',
         flow_type == 'flow_pred') %>%
  group_by(years, prop, plotid) %>%
  summarise(RMSE = mean(RMSE)) %>%
  ggplot(aes(x=years, y=RMSE, group = prop)) +
  geom_point() +
  geom_line(aes(linetype = as.factor(prop))) +
  facet_grid(~ plotid) +
  labs(title = 'Average RMSE for Models with Different Number of Years',
       subtitle = 'Step 3+4 with 3-day rolling average precipitation',
       linetype = 'Proportion of \nMissing Data',
       x = 'Number of Consequtive Years') +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        text = element_text(size = 16))
ggsave('Figs/predictions/RMSE_with_step_3_line_BY_years.png',
       width = 16, height = 10)
