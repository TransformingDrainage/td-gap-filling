# This script compute recession slopes of tile flow data
# Slopes are calculated for both amputed and imputed (in step/module 1) data

source(file = '00_project_settings.R')



# Create functions to select peak and inflection points
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
    select(date, season, flow, ln_flow, group, POINT) %>%
    # calculate duration of PEAK-TO-INFLECTION period and slope
    mutate(days_bw_pni = lead(date) - date,
           days = as.numeric(days_bw_pni),
           change = (lead(ln_flow) - ln_flow),
           slope = change/days)
}



# Calculate Recession Slopes ----------------------------------------------
# Read input files
dpac_files <- list.files('Data/Inter_Data/Phase2_Imputation/DPAC/', full.names = TRUE)

for (i in dpac_files) {
  DNAME <- str_extract(i, pattern = 'DPAC_Y.{4}')
  read_rds(i) %>%
    unnest() %>%
    gather(flow_type, flow, starts_with('flow')) %>%
    arrange(simulation, prop, plotid, flow_type, date) %>%
    group_by(simulation, prop, plotid, flow_type) %>%
    nest() %>%
    # log-transform flow data 
    mutate(
      data = map(data,
                 ~ .x %>%
                   # ADD 2 EXTRA READINGS TO AVOID PROBLEMS IN PEAK/INFLECTION SELECTION
                   add_row(flow = c(0, 0), .before = TRUE) %>%
                   # make sure to handle log(0)
                   mutate(ln_flow = ifelse(flow == 0, NA, log(flow)))
      )
    ) %>%
    # find PEAK and INFLECTION points
    mutate(data = map(data, peak_inflection)) %>%
    arrange(plotid, as.integer(simulation)) %>%
    # save recession slopes so YOU DO NOT HAVE TO CALCULATE THEM AGAIN
    write_rds(paste0('Data/Inter_Data/Phase3_Imputation/DPAC/recession_slopes/', 
                     DNAME, '_recession_slopes.rds'),
              compress = 'xz')
}



# Calculate Ave Recession Slope -------------------------------------------
recession_slopes <- vector('list')
for (i in list.files('Data/Inter_Data/Phase3_Imputation/DPAC/recession_slopes', full.names = TRUE)) {
  recession_slopes[[i]] <- read_rds(i)
}

recession_slopes %>% 
  bind_rows(.id = 'id') %>%
  mutate(id = str_extract(id, 'Y._.{2}')) %>%
  # select "peak" POINTs, because they correspond to recession (falling) limb 
  mutate(data = map(data, ~ .x %>% filter(POINT == 'peak',
                                          !is.infinite(slope)) %>%
                      ungroup())) %>%
  unnest(data) %>%
  # save intermediate data for plotting in Global Environment
  {. ->> recession_slopes_to_plot} %>%
  # calculate average number of days between peak and first point of inflection
  # and the slope of recession limb 
  group_by(id, simulation, prop, flow_type, plotid, season) %>%
  summarise(ave_days = mean(days),
            # calculate trimmed, geometric and harmonic means since the distributions of slopes are right-skewed 
            ave_slope = mean(slope),
            ave_slope_trim = mean(slope, trim = 0.1),  # less conservative
            ave_slope_geom = -exp(mean(log(-slope))),  # more conservative
            ave_slope_harm = 1/(mean(1/slope))         # most conservative
  ) %>%
  ungroup() %>%
  write_rds('Data/Inter_Data/Phase3_Imputation/DPAC/DPAC_ave_recession_slopes.rds', 
            compress = 'xz')

# save slopes
write_rds(recession_slopes_to_plot, 'Data/Inter_Data/Phase3_Imputation/DPAC/DPAC_all_recession_slopes.rds', 
          compress = 'xz')


# Plot recession slopes
recession_slopes_to_plot %>%
  filter(flow_type == 'flow_pred') %>%
  ggplot(aes(flow, slope, col = days)) +
  geom_point() +
  facet_grid(~ plotid) +
  theme_light() +
  theme(text = element_text(size = 18))
ggsave('Figs/models/recession_slopes_by_flow.png',
       width = 12, height = 8)

recession_slopes_to_plot %>%
  filter(flow_type == 'flow_pred') %>%
  ggplot(aes(season, slope, col = season)) +
  geom_boxplot() +
  scale_y_reverse() +
  facet_grid(. ~ plotid) +
  theme_light() +
  theme(text = element_text(size = 18))
ggsave('Figs/models/recession_slopes_distribution.png',
       width = 12, height = 8)


recession_slopes_to_plot %>%
  group_by(id, simulation, prop, flow_type, plotid, season) %>%
  summarise(ave_days = mean(days),
            ave_slope = mean(slope),
            ave_slope_trim = mean(slope, trim = 0.1),  # less conservative
            ave_slope_geom = -exp(mean(log(-slope))),  # more conservative
            ave_slope_harm = 1/(mean(1/slope))         # most conservative
            ) %>% 
  ungroup() %>%
  gather(slope_type, slope, contains('_slope')) %>% 
  mutate(prop = as.factor(prop),
         slope_type = factor(slope_type, 
                             levels = c('ave_slope', 'ave_slope_trim', 'ave_slope_geom', 'ave_slope_harm'),
                             labels = c('Arithmetic Mean', 'Trimmed Mean, 10%', 'Geometric Mean', 'Harmonic Mean'))) %>%
  filter(flow_type == "flow_pred") %>%
  ggplot(aes(x=prop, y=slope)) + 
  geom_boxplot() + 
  scale_y_reverse() +
  facet_grid(season ~ slope_type) + 
  labs(title = 'Distribution of Mean Recession Slopes',
       subtitle = 'Based on Simulated DPAC Data',
       x = 'Proportion of Missing Data',
       y = 'Average Recession Slope') +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        strip.text = element_text(size = 14))
ggsave('Figs/models/ave_recession_slopes_distribution.png',
       width = 12, height = 8)
