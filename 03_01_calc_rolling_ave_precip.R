# This script compute rolling average precipitation

source(file = '00_project_settings.R')
library(zoo)



# Read Data ---------------------------------------------------------------

precip <- 
  read_rds('Data/Input_Data/daily_tile_flow_complete.rds') %>%
  distinct(siteid, date, rain, snowing) %>%
  # substitute rain with 0 for snowing days
  mutate(rain = ifelse(snowing == 1 & !is.na(snowing), 0, rain)) 


# Rainfall to precipitation relationship
# but this should be EVENT-base
read_rds('Data/Input_Data/daily_tile_flow_complete.rds') %>%
  filter(rain > 0) %>%
  mutate(season = factor(quarter(date), labels = c("winter", "spring", "summer", "fall"))) %>%
  ggplot(aes(rain, flow, col = season)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = 'lm', se = F, col = 'black') +
  ggpmisc::stat_poly_eq(formula = y ~ x,
                        eq.with.lhs = "italic(hat(y))~`=`~",
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                        label.y = 0.95, label.x = .95, 
                        col = 'black', face = 'bold',
                        parse = TRUE) +
  facet_grid(season ~ siteid) +
  theme_light() 


# Distribution of no-zero precipitation at DPAC -----------------------

df <- precip %>% filter(siteid == 'DPAC') %>% select(-siteid)

# find the peak
n <- which.max(density(df$rain[df$rain > 0])$y)
peak <- density(df$rain[df$rain > 0])$x[n]

df %>%
  filter(rain > 0) %>%
  ggplot(aes(rain)) +
  geom_density() +
  geom_vline(aes(xintercept = median(rain)), col = 'red') +
  geom_vline(aes(xintercept = mean(rain)), col = 'blue') +
  geom_vline(xintercept = peak) +
  geom_vline(xintercept = 0.5, linetype = 3) +
  geom_text(aes(x = median(rain), label = paste('Median = ', round(median(rain), 1))), 
            y = 0.06, angle = 90, nudge_x = -0.25) +
  geom_text(aes(x = mean(rain), label = paste('Mean = ', round(mean(rain), 1))), 
            y = 0.06, angle = 90, nudge_x = -0.25) +
  geom_text(label = paste('Peak = ', round(peak, 1)), 
            x = peak - 0.25, y = 0.06, angle = 90) +
  geom_text(label = paste('Limit Used = ', 0.5), 
            x = 0.5 - 0.25, y = 0.06, angle = 90) +
  theme_light() +
  coord_cartesian(xlim = c(0, 15))
ggsave('Figs/phase3/DPAC_precip_density_distribution.png',
       width = 10, height = 6)


# Distribution of no-zero precipitation at SERF -----------------------

df <- precip %>% filter(siteid == 'SERF') %>% select(-siteid)

# find the peak
n <- which.max(density(df$rain[df$rain > 0])$y)
peak <- density(df$rain[df$rain > 0])$x[n]

df %>%
  filter(rain > 0) %>%
  ggplot(aes(rain)) +
  geom_density() +
  geom_vline(aes(xintercept = median(rain)), col = 'red') +
  geom_vline(aes(xintercept = mean(rain)), col = 'blue') +
  geom_vline(xintercept = peak) +
  geom_vline(xintercept = 0.5, linetype = 3) +
  geom_text(aes(x = median(rain), label = paste('Median = ', round(median(rain), 1))), 
            y = 0.07, angle = 90, nudge_x = -0.25) +
  geom_text(aes(x = mean(rain), label = paste('Mean = ', round(mean(rain), 1))), 
            y = 0.07, angle = 90, nudge_x = -0.25) +
  geom_text(label = paste('Peak = ', round(peak, 1)), 
            x = peak - 0.25, y = 0.07, angle = 90) +
  geom_text(label = paste('Limit Used = ', 0.5), 
            x = 0.5 - 0.25, y = 0.07, angle = 90) +
  theme_light() +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 0.08))
ggsave('Figs/phase3/SERF_precip_density_distribution.png',
       width = 10, height = 6)


# Determine rainfall threshold --------------------------------------------

rain_limit = 0.5  # NEED TO justify rainfall threshold



# Calculate 3-day moving average precipitation ----------------------------

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

# calculate moving ave precipitation 
precip_rolling_ave <- 
  precip %>% 
  group_by(siteid) %>%
  mutate(rain_2 = rollapplyr(rain, 2, mean, na.rm = TRUE, partial = TRUE),
         # calculated 3-day moving average precip
         rain_3 = rollapplyr(rain, 3, mean, na.rm = TRUE, partial = TRUE),
         # calculated weighted moving average precip
         rain_3_weighted = rollapplyr(rain, width = 3, FUN = rollmean.weighted,
                                      partial = FALSE, fill = NA),
         # handle days which are out of the rolling range = first two recoreds in this case
         rain_3_weighted = ifelse(is.na(rain_3_weighted), rain_3, rain_3_weighted),
         # leave calculated moving average only for those days when it rained
         # OR rain was negligible (see the plot above)
         rain_2 = ifelse(rain > rain_limit, rain_2, 0),
         rain_3 = ifelse(rain > rain_limit, rain_3, 0),
         rain_3_weighted = ifelse(rain > rain_limit, rain_3_weighted, 0)) %>%
  ungroup() %>%
  select(siteid, date, everything())


# Save rainfall for use in regression model (Phase 3)
write_rds(precip_rolling_ave, 'Data/Inter_Data/Phase3_Imputation/rolling_ave_precip.rds', compress = 'xz')


