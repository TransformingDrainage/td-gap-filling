# Performance measures ----------------------------------------------------

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
           ME = map_dbl(data, perf_ME),
           IoA = map_dbl(data, perf_IoA))
}