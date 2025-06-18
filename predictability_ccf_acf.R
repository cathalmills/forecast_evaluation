
ribbon_colors <- c(
  "Trend A" = "#66C2A5",  # Soft Teal
  "Trend B" = "#E2C300",  # Soft Gold
  "Trend C" = "#C2A5CF"   # Muted Lavender
)

# Define subtly darker colors for lines
line_colors <- c(
  "Trend A" = "#00876C",  # Deep Teal
  "Trend B" = "#E2C305",  # Rich Gold
  "Trend C" = "#7B3294"   # Deep Purple
)

acf_by_location <- location_entropy_results_12_wk_rolling_window[
  ,
  {
    acf_vals <- acf(predictability, lag.max = 12, plot = FALSE)$acf
    # Drop lag 0 (acf[1]) and return as a data.table with lag labels
    .(lag = 1:12, autocorrelation = acf_vals[2:13])
  },
  by = location
]


acf_summary <- acf_by_location[
  ,
  .(
    mean_autocorrelation  = mean(autocorrelation, na.rm = TRUE),
    sd_autocorrelation    = sd(autocorrelation, na.rm = TRUE),
    median_autocorrelation = median(autocorrelation, na.rm = TRUE),
    lower_quartile        = quantile(autocorrelation, probs = 0.25, na.rm = TRUE),
    upper_quartile        = quantile(autocorrelation, probs = 0.75, na.rm = TRUE),
    min_autocorrelation   = min(autocorrelation, na.rm = TRUE),
    max_autocorrelation   = max(autocorrelation, na.rm = TRUE)
  ),
  by = lag
]
max(acf_summary$upper_quartile)
acf_plot <- ggplot(acf_summary)+
  geom_line(aes(x = lag, y = median_autocorrelation), color = line_colors[2],
            linewidth = 1.2)+
  geom_ribbon(aes(x = lag, ymin = lower_quartile, ymax = upper_quartile), fill = ribbon_colors[2],
              alpha = 0.2)+
  theme_light()+
  geom_hline(aes(yintercept = 0),
             linewidth = 1.0,
             linetype = "dashed")+
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(1, 12, by = 2),
                     limits = c(1, 12.2))+
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(-0.5, 1, by = 0.25),
                     limits = c(-0.5, 1))+
  
  theme_custom()+
  theme(axis.text.y = element_text(size=18))+
  labs(
    x = "Lag (windows)",
    y = "Autocorrelation",
  ) 
  
acf_plot


compute_ccf <- function(x, y, max_lag = 12) {
  ccf_vals <- ccf(x, y, lag.max = max_lag, plot = FALSE, type = "correlation")
  # We only keep positive lags: predictability(t - lag) → outcome(t)
  # Which is: lag > 0
  pos_lag_indices <- which(ccf_vals$lag >= 0)
  data.table(
    lag = ccf_vals$lag[pos_lag_indices],
    cross_correlation = ccf_vals$acf[pos_lag_indices]
  )
}
# Step 1: Calculate cross-correlation for each variable and location
ccf_long <- rbindlist(list(
  # 1. Predictability → median_med_Rt
  location_entropy_results_12_wk_rolling_window[
    , compute_ccf(rolling_mean_Rt, predictability), by = c("location")
  ][, variable := "R(t)"],
  
  # 2. Predictability → wis_scaled_relative_skill
  location_entropy_results_12_wk_rolling_window[
    , compute_ccf(rolling_mean_cases, predictability), by = c("location")
  ][, variable := "Cases"]
))


ccf_summary <- ccf_long[
  , .(
    mean_ccf   = mean(cross_correlation, na.rm = TRUE),
    sd_ccf     = sd(cross_correlation, na.rm = TRUE),
    median_ccf = median(cross_correlation, na.rm = TRUE),
    lower_q    = quantile(cross_correlation, 0.25, na.rm = TRUE),
    upper_q    = quantile(cross_correlation, 0.75, na.rm = TRUE),
    min_ccf    = min(cross_correlation, na.rm = TRUE),
    max_ccf    = max(cross_correlation, na.rm = TRUE)
  ),
  by = .(variable, lag)
]
ccf_summary
ccf_plot <- ggplot(ccf_summary)+
  geom_line(aes(x = lag, y = median_ccf, color = variable),
            linewidth = 1.2)+
  geom_ribbon(aes(x = lag, ymin = lower_q, ymax = upper_q, fill = variable),
              alpha = 0.2, show.legend = FALSE)+
  theme_light()+
  geom_hline(aes(yintercept = 0),
             linewidth = 1.0,
             linetype = "dashed")+
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 12, by = 2),
                     limits = c(0, 12.2))+
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(-0.5, 1, by = 0.25),
                     limits = c(-0.5, 1))+
  theme_custom()+
  theme(axis.text.y = element_text(size=18))+
  labs(
    x = "Lag (windows)",
    y = "Cross-correlation")+
  # )+scale_fill_manual("",
  #                   values = c("Cases"))+
  scale_color_manual("",
                     values = c("R(t)" = "#C2A5CF",
                                "Cases" = "#66C2A5"))+
  scale_fill_manual("",
                     values = c("R(t)" = "#C2A5CF",
                                "Cases" = "#66C2A5"))

ccf_plot
acf_ccf_plot <- ggarrange(acf_plot, NULL, ccf_plot,
          nrow = 3, heights = c(0.5, 0.1, 0.75))

#CCF





