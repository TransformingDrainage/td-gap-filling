# This script evaluates accuracy of imputed data

source(file = '00_project_settings.R')


# Performane measures ------------

# Mean Observation
perf_O <- function(df) {
  df %>%
    summarise(O = mean(flow)) %>%
    pull()
}

# Average Error
perf_AE <- function(df) {
  df %>% 
    mutate(AE = flow_pred - flow) %>%
    summarise(AE = sum(AE) / n()) %>%
    pull()
}

# Root Mean Square Error
perf_RMSE <- function(df) {
  df %>% 
    mutate(RMSE = (flow_pred - flow )^2) %>%
    summarise(RMSE = sum(RMSE) / n()) %>%
    mutate(RMSE = sqrt(RMSE)) %>%
    pull()
  
}

# Mean Absolute Error
perf_MAE <- function(df) {
  df %>%
    mutate(MAE = abs(flow_pred - flow)) %>%
    summarise(MAE = sum(MAE) / n()) %>%
    pull()
}

# Modeling Efficiency
perf_ME <- function(df) {
  df %>%
    mutate(O = mean(flow),
           term1 = (flow - O)^2,
           term2 = (flow_pred - flow)^2) %>%
    summarise(ME = (sum(term1) - sum(term2))/sum(term1)) %>%
    pull()
}

# Index of Agreement
perf_IoA <- function(df) {
  df %>%
    mutate(term1 = (flow_pred - flow)^2,
           O_ave = mean(flow),
           term2 = (abs(flow_pred - O_ave) + abs(flow - O_ave))^2) %>%
    summarise(IoA = 1 - sum(term1)/sum(term2)) %>%
    pull()
}

# read data of interst
read_my_data <- function(NAME) {
  if (NAME %in% c(2, 4, 6, 11)) {
    paste0('Data/Output_Data/all_data_Y', NAME, '.rds') %>%
      read_rds() %>%
      bind_rows() %>%
      group_by(simulation, prop, flow_type, model_name, plotid) %>%
      nest() %>% 
      ungroup()
  } else {
    cat(crayon::green('NAME should be one of the following values: 2, 4, 6, or 11'))
  }
}


# calculate performance measures 
calc_my_perf <- function(df) {
  df %>%
    #  use only those that does not have negative outcome
    mutate(O = map_dbl(data, perf_O), 
           AE = map_dbl(data, perf_AE),
           NAE = AE/O,
           MAE = map_dbl(data, perf_MAE),
           NMAE = MAE/O,
           RMSE = map_dbl(data, perf_RMSE),
           NRMSE = RMSE/O,
           # ME = map_dbl(data, perf_ME),
           IoA = map_dbl(data, perf_IoA))
}

# calculate
read_my_data(2) %>%
  calc_my_perf() %>%
  select(-data) -> Y2_perf

read_my_data(4) %>%
  calc_my_perf() %>%
  select(-data) -> Y4_perf

read_my_data(6) %>%
  calc_my_perf() %>%
  select(-data) -> Y6_perf

read_my_data(11) %>%
  calc_my_perf() %>%
  select(-data) -> Y11_perf





# PLOT

Y2_perf %>%
  ggplot(aes(as.factor(prop), RMSE, col = plotid)) +
  geom_boxplot() +
  facet_grid(flow_type ~ model_name) +
  theme_light()


Y4_perf %>%
  ggplot(aes(as.factor(prop), RMSE, col = plotid)) +
  geom_boxplot() +
  facet_grid(flow_type ~ model_name) +
  theme_light()


Y6_perf %>%
  ggplot(aes(as.factor(prop), RMSE, col = plotid)) +
  geom_boxplot() +
  facet_grid(flow_type ~ model_name) +
  theme_light()


Y11_perf %>%
  ggplot(aes(as.factor(prop), RMSE, col = plotid)) +
  geom_boxplot() +
  facet_grid(flow_type ~ model_name) +
  theme_light()




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


