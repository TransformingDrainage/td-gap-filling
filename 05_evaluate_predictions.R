# This script evaluates accuracy of imputed data

source(file = '05_01_performance_metrics.R')



# function to calculate performance metrics
performance <- function(df) {
  df %>% 
    filter(!is.na(comments)) %>%
    mutate(scenario = str_sub(scenario, 1, 2)) %>%
    group_by(scenario, prop, sim, flow_type, api, plotid) %>%
    nest() %>% 
    ungroup() %>%
    calc_my_perf() %>%
    select(-data)
}

# function to read data and save calculated performance indicators
save_preformance_data <- function(PATH) {
  dir <- dirname(PATH)
  file <- basename(PATH) %>% 
    str_remove('pred_phase3_') %>%
    str_replace('.rds', '_performance.rds')
  read_rds(PATH) %>%
    performance() %>%
    write_rds(paste0(dir, '/STATS/', file), compress = 'xz')
}



# Calculate performance indicators ----------------------------------------
dpac_files <- list.files('Data/Output_Data/DPAC/ORIGINAL/', full.names = TRUE, recursive = FALSE, pattern = '.rds')

walk(dpac_files, ~ save_preformance_data(.x))



# Read and Combine Performance data
read_preformance_data <- function(PATTERN = 'DPAC_Y') {
  list.files('Data/Output_Data/DPAC/ORIGINAL/STATS/', 
             full.names = TRUE, 
             pattern = PATTERN) %>%
    map(~ read_rds(.x))
}

Y2 <- read_preformance_data('Y2') %>%
  bind_rows()

YA <- read_preformance_data('YA') %>%
  bind_rows()

YAll <- read_preformance_data() %>%
  bind_rows()


# PLOT

YA %>%
  ggplot(aes(as.factor(prop), AE, col = api)) +
  geom_boxplot() +
  facet_grid(flow_type ~ plotid) +
  theme_light()




# Plot for Presentation ---------------------------------------------------

# RMSE for base model (3-day-average with step 3+4 for 11 years)
YA %>%
  filter(api == '3 day rolling ave',
         flow_type == 'flow_pred') %>%
  ggplot(aes(as.factor(prop), NRMSE)) +
  geom_boxplot() +
  facet_grid(. ~ plotid) +  
  labs(title = 'RMSE of Predictions Made at Step 3 and 4 ',
       subtitle = 'Complete Dataset (10 years)',
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
YA %>%
  filter(api == '3 day rolling ave') %>%
  mutate(flow_type = factor(flow_type,
                            levels = c('flow_pred', 'flow_amp'),
                            labels = c('Steps 3 + 4', 'Step 4'))) %>%
  ggplot(aes(as.factor(prop), NRMSE, col = flow_type)) +
  geom_boxplot(size = 1) +
  facet_grid(. ~ plotid) +  
  labs(title = 'RMSE of Predictions with and without Step 3',
       subtitle = 'Complete Dataset (10 years)',
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
YA %>%
  filter(flow_type == 'flow_pred') %>%
  gather(key, value, AE:IoA) %>%
  filter(key %in% c('NRMSE', 'NAE', 'NMAE')) %>%
  ggplot(aes(as.factor(prop), value, col = api)) +
  geom_boxplot(size = 1) +
  facet_grid(key ~ plotid, scales = 'free') +  
  labs(title = 'RMSE of Predictions with Different Rolling Averages',
       subtitle = 'Complete Dataset (10 years)',
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



# # Look at seasonal distibution of error
# Y11_seasonal <- 
#   df_Y11 %>%
#   unnest(data) %>%
#   filter(!is.na(comments)) %>%
#   left_join(df_original, by = c('plotid', 'date')) %>%
#   # calculate RMSE
#   mutate(SE = (flow - flow_pred)^2) %>%
#   group_by(simulation, prop, flow_type, model_name, plotid, season) %>%
#   summarise(RMSE = sqrt(sum(SE)/n()))
# 
# 
# Y11_seasonal %>%
#   ungroup() %>%
#   filter(flow_type == 'flow_pred') %>%    # select data that prdicted by both modules
#   filter(model_name == 'reg_model_3_day_avg') %>%
#   ggplot(aes(as.factor(prop), RMSE)) +
#   geom_boxplot(size = 1) +
#   stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
#                width = .75, linetype = "dashed", col = 'red') +
#   facet_grid(plotid ~ season) +
#   labs(title = 'RMSE of Predictions by Season',
#        subtitle = 'Complete Dataset (11 years)',
#        x = 'Proportion of Missing Data') +
#   theme_light() +
#   theme(plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'),
#         plot.subtitle = element_text(hjust = 0.5, size = 20),
#         axis.title = element_text(size = 18),
#         strip.text = element_text(size = 18),
#         text = element_text(size = 16))
# ggsave('Figs/predictions/RMSE_with_step_3_boxplot_BY_season.png',
#        width = 16, height = 10)



# see how number of years effect predictions
YAll %>%
  mutate(years = factor(scenario, levels = c('Y2', 'Y4', 'Y6', 'Y8', 'YA'), labels = c('Y2', 'Y4', 'Y6', 'Y8', 'Y10'))) %>%
  filter(api == '3 day rolling ave',
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


YAll %>%
  mutate(years = factor(scenario, levels = c('Y2', 'Y4', 'Y6', 'Y8', 'YA'), labels = c('Y2', 'Y4', 'Y6', 'Y8', 'Y10'))) %>%
  filter(api == '3 day rolling ave',
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


YAll %>%
  mutate(years = factor(scenario, levels = c('Y2', 'Y4', 'Y6', 'Y8', 'YA'), labels = c('Y2', 'Y4', 'Y6', 'Y8', 'Y10'))) %>%
  filter(api == '3 day rolling ave',
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
