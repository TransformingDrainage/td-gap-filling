# This script evaluates accuracy of imputed data

source(file = '00_project_settings.R')


# Read and Combine Performance metrics data
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










# Plot for Presentation ---------------------------------------------------

# Predictions data
df_plot <- read_rds('Data/Input_Data/RDS/DPAC_amp_MAR_imputed_data_Y11.rds') 
df_rain <- read_rds('Data/Input_Data/RDS/daily_tile_flow_complete.rds') %>%
  filter(siteid == 'DPAC') %>%
  select(date, rain) %>%
  unique()

# Predictions plot
df_plot %>%
  filter(simulation %in% c(333 ,911)) %>%
  filter(flow_type == 'flow_pred') %>%    # select data that prdicted by both modules
  filter(model_name == 'reg_model_3_day_avg') %>%
  unnest(data) %>%
  filter(year(date) == 2016 & month(date) < 7,
         plotid == 'NE') %>%
  select(-flow_type, -model_name) %>%
  left_join(df_original) %>%
  mutate(flow_pred = ifelse(is.na(comments), NA_real_, flow_pred)) %>%
  ungroup() %>%
  left_join(unique(df_rain), by = 'date') -> plot_data

plot_data %>%
  # filter(month(date) == 5) %>%
  ggplot(aes(x = date)) +
  # geom_col(aes(y = rain/10), fill = 'skyblue', alpha = 0.5) +
  geom_tile(aes(y = -1*(rain/10-15), # y = the center point of each bar
                height = rain/5,
                width = 1),
            alpha = 0.5, 
            fill = "skyblue",
            color = "white") +
  # geom_point(aes(y = flow), size = 1, alpha = 0.75) + 
  geom_line(aes(y = flow), size = 1, alpha = 0.25) + 
  geom_point(#data = . %>% filter(is.na(comments)), 
    aes(y = flow), colour = 'grey70', size = 2, shape = 1) +
  geom_point(data = . %>% filter(!is.na(comments)), 
             aes(y = flow_pred), colour = rgb(241, 180, 38, maxColorValue = 255), size = 3) +
  geom_point(data = . %>% filter(comments == 'predicted via recession model'), 
             aes(y = flow_pred), colour = rgb(44, 121, 181, maxColorValue = 255), size = 3) +
  # geom_line(aes(y = flow_pred), alpha = 0.25) +
  scale_x_date(date_labels = '%b-%Y') +
  # scale_y_continuous(sec.axis = sec_axis(name = 'Precipitation')) +
  labs(x = NULL, y = 'Daily  Drain Flow, mm') +
  # title = 'Daily Drainage Predictions - January-June 2016'
  facet_grid(prop ~ .) +
  theme_light() +
  theme(#plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'),
    #plot.subtitle = element_text(hjust = 0.5, size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 20),
    strip.background = element_rect(fill = rgb(56, 59, 59, maxColorValue = 255)),
    text = element_text(size = 16)) #+
# coord_cartesian(xlim = ymd(20160116, 20160226))
ggsave('Figs/all_predictions_2016_Jan_Jun.png',
       width = 10, height = 6)


# see how number of years effect predictions
list(Y2=Y2_perf, Y4=Y4_perf, Y6=Y6_perf, Y11=Y11_perf) %>%
  bind_rows(.id = 'years') %>%
  ungroup() %>%
  mutate(years = factor(years, 
                        levels = c('Y2', 'Y4', 'Y6', 'Y11'),
                        labels = c('2 Years', '4 Years', '6 Years', '11 Years'))) %>%
  filter(model_name == 'reg_model_3_day_avg',
         flow_type == 'flow_pred',
         plotid == "NE") %>%
  gather(PERF, value, O:IoA) %>%
  filter(PERF %in% c(#'MAE', 'NMAE', 'NRMSE',
    'RMSE')) %>%
  mutate(PERF = factor(PERF, levels = c('RMSE', 'NRMSE')),
         prop = as.character(prop)) %>%
  ggplot(aes(prop, value)) +
  geom_boxplot(size = 1) +
  facet_grid(. ~ years, scales = 'free_y') +
  labs(x = 'Proportion of Missing Data', 
       y = 'RMSE') +
  theme_light() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, vjust = 0.5),
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill = rgb(56, 59, 59, maxColorValue = 255)),
        text = element_text(size = 16)) 
ggsave('Figs/RMSE_BY_years.png',
       width = 10, height = 4.5)

list(Y2=Y2_perf, Y4=Y4_perf, Y6=Y6_perf, Y11=Y11_perf) %>%
  bind_rows(.id = 'years') %>%
  ungroup() %>%
  mutate(years = factor(years, 
                        levels = c('Y2', 'Y4', 'Y6', 'Y11'),
                        labels = c('2 Years', '4 Years', '6 Years', '11 Years'))) %>%
  filter(model_name == 'reg_model_3_day_avg',
         flow_type == 'flow_pred') %>%
  gather(PERF, value, O:IoA) %>%
  group_by(years, prop, plotid, PERF) %>% 
  summarise(value = mean(value)) %>%
  filter(PERF %in% c('AE', 
                     # 'NAE',
                     # 'MAE',
                     # 'NMAE',
                     'RMSE', #'NRMSE',
                     'IoA'),
         plotid == "NE") %>%
  ungroup() %>%
  mutate(prop = as.factor(prop),
         PERF = factor(PERF, levels = c('RMSE', 'AE', 'IoA'))) %>%
  ggplot(aes(x=prop, y=value, group = years)) +
  geom_point(#aes(shape = years), 
    size = 2) +
  geom_line(aes(linetype = years, col = years), size = 1) +
  facet_grid(PERF ~ ., scales = 'free_y', switch = 'y') +
  labs(linetype = 'Number of\nAvailable Years',
       col = 'Number of\nAvailable Years',
       x = 'Proportion of Missing Data',
       y = NULL) +
  theme_light() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(size = 16, #angle = 90, 
                                   vjust = 0.5),
        strip.text = element_text(size = 20, color = 'black'),
        strip.placement = 'outsite',
        strip.background = element_rect(fill = 'white'),
        text = element_text(size = 16))
ggsave('Figs/PERF_BY_prop.png',
       width = 8.5, height = 6)


list(Y2=Y2_perf, Y4=Y4_perf, Y6=Y6_perf, Y11=Y11_perf) %>%
  bind_rows(.id = 'years') %>%
  ungroup() %>%
  mutate(years = factor(years, 
                        levels = c('Y2', 'Y4', 'Y6', 'Y11'),
                        labels = c('2 Years', '4 Years', '6 Years', '11 Years'))) %>%
  filter(model_name == 'reg_model_3_day_avg',
         flow_type == 'flow_pred') %>%
  gather(PERF, value, O:IoA) %>%
  group_by(years, prop, plotid, PERF) %>% 
  summarise(value = mean(value)) %>%
  filter(PERF %in% c('AE', 
                     # 'NAE', 
                     # 'MAE',
                     # 'NMAE',
                     'RMSE', #'NRMSE',
                     'IoA'),
         plotid == "NE") %>%
  ungroup() %>%
  mutate(prop = as.factor(prop),
         PERF = factor(PERF, levels = c('RMSE', 'AE', 'IoA'))) %>%
  ggplot(aes(x=years, y=value, group = prop)) +
  geom_point(#aes(shape = years), 
    size = 2) +
  geom_line(aes(linetype = prop, col = prop), size = 1) +
  facet_grid(PERF ~ ., scales = 'free_y', switch = 'y') +
  labs(linetype = 'Proportion of\nMissing Data',
       col = 'Proportion of\nMissing Data',
       x = 'Number of Consecutive Years',
       y = NULL) +
  theme_light() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(size = 16, #angle = 90, 
                                   vjust = 0.5),
        strip.text = element_text(size = 20, color = 'black'),
        strip.placement = 'outsite',
        strip.background = element_rect(fill = 'white'),
        text = element_text(size = 16))
ggsave('Figs/PERF_BY_Years.png',
       width = 8, height = 6)


# ANOVA analysis
library(emmeans)

# selecet 3-day moving average model
list(Y2=Y2_perf, Y4=Y4_perf, Y6=Y6_perf, Y11=Y11_perf) %>%
  bind_rows(.id = 'years') %>%
  ungroup() %>%
  mutate(years = factor(years, levels = c('Y2', 'Y4', 'Y6', 'Y11'))) %>%
  filter(model_name == 'reg_model_3_day_avg',
         flow_type == 'flow_pred') %>%
  select(-flow_type, -model_name) %>%
  mutate(years = factor(years, levels = c('Y2','Y4','Y6','Y11')),
         prop = factor(prop)) -> df

lm <- lm(RMSE ~ years + prop + plotid, data = df)
car::Anova(lm)
emm <- emmeans(lm, ~ prop) #emmeans is current terminology/approach versus lsmeans
multcomp::cld(emm, reverse=TRUE)
emm <- emmeans(lm, ~ years) #emmeans is current terminology/approach versus lsmeans
multcomp::cld(emm, reverse=TRUE)


# https://cran.r-project.org/web/packages/emmeans/vignettes/basics.html
ref_grid(lm)
ref_grid(lm)@grid