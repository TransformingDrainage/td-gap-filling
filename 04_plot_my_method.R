library(readr)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(wesanderson) # for colour pallets


# Modify data for plotting ------------------------------------------------

tile_flow_prediction %>%
  mutate(pred = ifelse(is.na(flow), flow_pred, NA)) %>%
  arrange(siteid, plotid, date) %>% 
  mutate(DATE = update(date, year = 2012)) %>%
  group_by(siteid) %>%
  # scale down rain to max tile flow
  mutate(RAIN = rain/max(rain, na.rm = TRUE)*max(flow_pred, na.rm = TRUE)) %>%
  ungroup() -> plot_tile_flow_data



# FIGURE 1. All Data Years ------------------------------------------------
plot_count <- 
  plot_tile_flow_data %>% 
  group_by(siteid) %>%
  summarise(count = n_distinct(plotid))

for (i in seq_along(plot_count$siteid)) {
  plot_tile_flow_data %>%
    left_join(plot_count, by = "siteid") %>%
    filter(siteid == plot_count$siteid[i]) %>%
    ggplot(aes(x = DATE, group = plotid)) +
    geom_col(aes(y = RAIN/count), fill = "skyblue", alpha = 0.45) +
    geom_line(aes(y = flow_pred, colour = plotid), size = 0.8) +
    geom_point(aes(y = flow_pred, colour = plotid), size = 1) +
    geom_point(aes(y = pred, colour = plotid), size = 2) +
    geom_point(data = . %>% filter(str_detect(comments, "predicted")), 
               aes(y = -1), shape = 15, col = "green", size = 1.1) +
    facet_grid(year ~ .) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b", name = NULL) +
    scale_y_continuous(name = "Tile Flow (mm)") +
    scale_colour_manual(values = c("#D67236", "#5B1A18", "#5F5647", "#F4BA7B")) +
    theme_bw()
  ggsave(filename = paste0("Output/Figs/model-predictions/", plot_count$siteid[i], "/",
                           plot_count$siteid[i], "_Fig1_All_Years.png"),
         width = 18, height = 10)
}


# Function to plot Fig 2 - sections of data 
section_plot <- function(DATA = plot_tile_flow_data, SITE, YEAR, 
                         GREEN_LINE = -0.5, RAIN_MULT = 1,
                         START = "0101", END = "1231") {
  DATA %>%
    filter(siteid == SITE) %>% 
    filter(year == YEAR) %>%
    ggplot(aes(x = DATE, group = plotid)) +
    geom_col(aes(y = RAIN * RAIN_MULT), fill = "skyblue", alpha = 0.45) +
    geom_line(aes(y = flow_pred, colour = plotid), size = 0.8) +
    geom_point(aes(y = flow_pred, colour = plotid), size = 1.2) +
    geom_point(aes(y = pred, colour = plotid), size = 2.5) +
    geom_point(aes(y = pred), colour = "white", size = 1.2, alpha = 0.9) +
    geom_point(data = . %>% filter(str_detect(comments, "predicted")), 
               aes(y = GREEN_LINE), shape = 15, col = "green", size = 1.1) +
    facet_grid(plotid ~ .) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b", name = NULL) +
    scale_y_continuous(name = "Tile Flow (mm)") +
    scale_colour_manual(values = c("#D67236", "#5B1A18", "#5F5647", "#F4BA7B")) +
    theme_bw() +
    labs(title = paste(SITE, YEAR)) +
    theme(text = element_text(size = 18),
          plot.title = element_text(size = 22, hjust = 0.5),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14)) +
    coord_cartesian(xlim = ymd(paste0(2012, START), paste0(2012, END)))
  
  ggsave(filename = paste0("Output/Figs/model-predictions/", SITE, "/",
                           SITE, "_Fig2_Tile_Flow_", YEAR, START, "-", YEAR, END, ".png"),
         width = 18, height = 10)
}



# Function to plot Fig 3 - annual tile flow
annual_flow_plot <- function(DATA = plot_tile_flow_data, SITE,
                             GROUP = "dwm") {
  plot_tile_flow_data %>%
    filter(siteid == SITE) %>% 
    mutate(year = as.factor(year),
           flow = ifelse(is.na(flow), 0, flow),
           flow_pred = ifelse(is.na(flow_pred), 0, flow_pred)) %>%
    group_by(siteid, plotid, dwm, year) %>%
    summarise(flow_pred = sum(flow_pred),
              flow = sum(flow, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(dwm = factor(dwm, levels = c("FD", "CD"))) %>%
    ggplot(aes(x = year, fill = dwm, group = get(GROUP))) +
    geom_col(aes(y = flow_pred), position = position_dodge(width = 0.9), width = 0.85, alpha = 0.5) +
    geom_col(aes(y = flow), position = position_dodge(width = 0.9), width = 0.85) +
    
    {if(GROUP == "plotid") geom_text(aes(label = plotid, y = I(-10)), size = 2, 
                                     position = position_dodge(width = 0.9))} +
    
    scale_fill_manual(values = c("skyblue2", "steelblue4")) +
    labs(x = NULL, y = "Annual Tile Flow (mm)", title = SITE) +
    theme_bw() +
    theme(text = element_text(size = 12), 
          plot.title = element_text(size = 16, hjust = 0.5),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
  ggsave(filename = paste0("Output/Figs/model-predictions/", SITE, "/",
                           SITE, "_Fig3_Annual_Tile_Flow.png"),
         width = 9, height = 5)
}



# Function to save data for each site
filled_flow_data <- function(SITE) {
  tile_flow_prediction %>%
    filter(siteid == SITE) %>%
    select(siteid, plotid, dwm, date, rain_mm = rain, flow_mm = flow_pred, comments) %>%
    write_csv(path = paste0("Output/Figs/model-predictions/", SITE, "/",
                            SITE, "_Tile_Flow_Filled.csv"))
}



# DPAC --------------------------------------------------------------------
# Fig 2 Tile Flow
# predictions in 2016
section_plot(SITE = "DPAC", YEAR = 2016, RAIN_MULT = 1,
             START = "1115") 

# Fig 3 Annual Tile Flow by Treatment
annual_flow_plot(SITE = "DPAC", GROUP = "plotid")

# Save data
filled_flow_data(SITE = "DPAC")



# SERF_IA -----------------------------------------------------------------
# Fig 2 Tile Flow
# predictions in 2007
section_plot(SITE = "SERF_IA", YEAR = 2007, RAIN_MULT = 1,
             START = "0305", END = "0515") 
# predictions in 2008
section_plot(SITE = "SERF_IA", YEAR = 2008, RAIN_MULT = 1,
             START = "0305", END = "0425") 
# predictions in 2009
section_plot(SITE = "SERF_IA", YEAR = 2009, RAIN_MULT = 1,
             START = "0301", END = "0425") 
# predictions in 2011
section_plot(SITE = "SERF_IA", YEAR = 2011, RAIN_MULT = 1,
             START = "0801") 
# predictions in 2012
section_plot(SITE = "SERF_IA", YEAR = 2012, RAIN_MULT = 1,
             END = "0410") 
section_plot(SITE = "SERF_IA", YEAR = 2012, RAIN_MULT = 1,
             START = "1110") 
# predictions in 2014
section_plot(SITE = "SERF_IA", YEAR = 2014, RAIN_MULT = 1,
             START = "0305", END = "0425") 
section_plot(SITE = "SERF_IA", YEAR = 2014, RAIN_MULT = 1,
             START = "1101") 
# predictions in 2015
section_plot(SITE = "SERF_IA", YEAR = 2015, RAIN_MULT = 1,
             START = "0301", END = "0425") 
# predictions in 2016
section_plot(SITE = "SERF_IA", YEAR = 2016, RAIN_MULT = 1,
             START = "1110")
# predictions in 2017
section_plot(SITE = "SERF_IA", YEAR = 2017, RAIN_MULT = 1,
             END = "0215") 
section_plot(SITE = "SERF_IA", YEAR = 2017, RAIN_MULT = 1,
             START = "0305", END = "0515") 

# Fig 3 Annual Tile Flow by Treatment
annual_flow_plot(SITE = "SERF_IA", GROUP = "plotid")

# Save data
filled_flow_data(SITE = "SERF_IA")




# Plot Resession Slopes ---------------------------------------------------
# show distribution of the slopes

# general plot
recession_slope_plot <-
  recession_slope_dist %>%
  mutate(id = paste(siteid, plotid)) %>%
  ggplot(aes(x = id, y = slope)) +
  geom_boxplot() +
  geom_point(data = . %>% filter(slope < -15), col = "red") +
  facet_grid(season ~ .) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# function to calculate sample size
give.n <- function(x){return(c(y = 0.75, label = length(x)))}

# recession slopes for each plot  
recession_slope_plot +
  stat_summary(fun.data = give.n, geom = "text", 
               position = position_dodge(width = 0.75))
ggsave(filename = "Output/Figs/recession_slope_distribution.png", width = 12, height = 8)

# zoomed in recession slopes with arithmetic mean slope for each plot-season
recession_slope_plot +  
  # add a line for mean
  stat_summary(fun.y = mean, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 0.8, linetype = "solid", colour = "deeppink2", 
               show.legend = FALSE) +
  stat_summary(fun.data = function(x){return(c(y = 2, label = length(x)))}, 
               geom = "text", 
               position = position_dodge(width = 0.75)) 
ggsave(filename = "Output/Figs/recession_slope_distribution_zoomed.png", width = 12, height = 8)


# zoomed in recession slopes with arithmetic mean slope for each plot-season
recession_slope_plot +  
  stat_summary(fun.y = mean, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 0.8, linetype = "solid", colour = "deeppink2", 
               show.legend = FALSE) +
  # add trimmed mean (10% from each side)
  stat_summary(fun.y = function(x){mean(x, trim = 0.1)}, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 0.8, linetype = "solid", colour = "deepskyblue3", 
               show.legend = FALSE) +
  # add geometric mean
  # stat_summary(fun.y = function(x){-psych::geometric.mean(-x, na.rm = T)}, geom = "errorbar", 
  #              aes(ymax = ..y.., ymin = ..y..),
  #              width = 0.75, size = 0.8, linetype = "solid", colour = "darkviolet", 
  #              show.legend = FALSE) +
  stat_summary(fun.data = function(x){return(c(y = 1, label = length(x)))}, 
               geom = "text", 
               position = position_dodge(width = 0.75)) +
  coord_cartesian(ylim = c(-4, 1.5))
ggsave(filename = "Output/Figs/recession_slope_distribution_zoomed_with_additional_means.png", 
       width = 12, height = 8)