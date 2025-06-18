low_high_predictability_colours <- alt_colors <- c("#D783B2", "#A1C9F4")
low_high_predictability_colours
#Predictability Analyses
#Past predictability is a predictor of current predictability which is a predictor of metrics
location_entropy_results_12_wk_rolling_window[, median(predictability), by = "location"]
summary(location_entropy_results_12_wk_rolling_window$predictability)

ggplot(location_entropy_results_12_wk_rolling_window)+
  geom_boxplot(aes(x = location_name, y = predictability), outlier.shape = NA)+
  coord_cartesian(ylim = c(0, 1)) + theme_bw()+
  coord_flip()
predictability_all_locations_over_time_boxplot <- ggplot(location_entropy_results_12_wk_rolling_window)+
  geom_boxplot(aes(x = max_time, y = predictability, group = cut(max_time, "1 week")),
               fill = "#3B9AB2", alpha = 0.6)+
  coord_cartesian(ylim = c(0, 1)) + theme_light()+
  theme_custom()+
  scale_x_date(date_labels = "%b %Y",
               breaks = seq(min(location_entropy_results_12_wk_rolling_window$max_time), 
                            max(location_entropy_results_12_wk_rolling_window$max_time), by = "1 month")) +
  rotate_x_axis_theme+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.position = "bottom",
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 20),  # Adjust legend text size
        legend.title = element_text(size = 20))+
  labs(y = "Predictability", x = "Date" )


max(location_entropy_results_12_wk_rolling_window)

Rt_all_locations_over_time_boxplot <- ggplot(location_entropy_results_12_wk_rolling_window)+
  geom_boxplot(aes(x = max_time, y = rolling_mean_Rt, group = cut(max_time, "1 week")),
               fill = "#FFB482")+
  theme_light()+
  theme_custom()+
  scale_x_date(date_labels = "%b %Y",
               breaks = seq(min(location_entropy_results_12_wk_rolling_window$max_time), 
                            max(location_entropy_results_12_wk_rolling_window$max_time), by = "1 month")) +
  rotate_x_axis_theme+
  geom_hline(aes(yintercept = 1),
             linewidth = 1.4,
             linetype = "dashed")+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.position = "bottom",
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 20),  # Adjust legend text size
        legend.title = element_text(size = 20))+
  labs(y = "R(t)", x = "Date" )





predictability_all_locations_over_time_boxplot <- ggplot(location_entropy_results_12_wk_rolling_window)+
  geom_boxplot(aes(x = max_time, y = predictability, group = cut(max_time, "1 week")),
               fill = "#3B9AB2", alpha = 0.6)+
  coord_cartesian(ylim = c(0, 1)) + theme_light()+
  theme_custom()+
  scale_x_date(date_labels = "%b %Y",
               breaks = seq(min(location_entropy_results_12_wk_rolling_window$max_time), 
                            max(location_entropy_results_12_wk_rolling_window$max_time), by = "1 month")) +
  rotate_x_axis_theme+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.position = "bottom",
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 20),  # Adjust legend text size
        legend.title = element_text(size = 20))+
  labs(y = "Predictability", x = "Date" )


max(location_entropy_results_12_wk_rolling_window)

predictability_individual_locations_over_time_tileplot <- ggplot(location_entropy_results_12_wk_rolling_window)+
  geom_raster(aes(x = max_time, y = location_name, fill = predictability))+
  theme_light()+
  theme_custom()+
  scale_x_date(date_labels = "%b %Y",
               breaks = seq(min(location_entropy_results_12_wk_rolling_window$max_time), 
                            max(location_entropy_results_12_wk_rolling_window$max_time), by = "1 month")) +
  rotate_x_axis_theme+
  scale_fill_viridis_c("Predictability",
                       option = "mako")+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.position = "bottom",
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 20),  # Adjust legend text size
        legend.title = element_text(size = 20))+
  labs(y = "State", x = "Date" )
predictability_individual_locations_over_time_tileplot
  # coord_flip()

ggplot(location_entropy_results_12_wk_rolling_window)+
  geom_raster(aes(x = max_time, 
                  y = location_name,
                  fill = predictability))+
  theme_light()+theme_custom()+
  labs(y = "State", x = "Date" )+
  scale_fill_viridis_c(option = "magma")+
  theme(
    axis.text.y = element_text(size=18,
                               angle = 45, hjust = 1),
    axis.text.x = element_text(size=18,
                               angle = 45, hjust = 1),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 20),  # Adjust legend text size
    legend.title = element_text(size = 20))+ 
  guides(fill = guide_colorbar(barwidth = 10,
                               title = "Predictability (1-PE)"))+
  theme(legend.position = "bottom")







#Predicatbility + Forecast performance -----
pointwise_12_wk_results <- state_covid_scores_pointwise_12_wk_rollings_window[[3]]
coverage_12_wk_results <- state_covid_scores_pointwise_12_wk_rollings_window[[4]]
tmp <- subset(coverage_12_wk_results, 
              quantile_level == 0.025 & interval_range == 95 )
tmp
# wide_coverage_12_wk_results <- dcast.data.table(coverage_12_wk_results,
#                  model + location + max_time + min_time  + target_type ~ quantile_level,
#                  value.var = c("quantile_coverage", 
#                                "quantile_coverage_deviation" ))
location_forecast_performance_12_wk_rolling_window
entropy_12_wk_scores_metrics_coverage_dt <- merge(location_forecast_performance_12_wk_rolling_window,
                                            pointwise_12_wk_results,
                                            by = c("location", "max_time", "min_time", "model", 
                                                   "target_type"))
entropy_12_wk_scores_metrics_coverage_dt
entropy_12_wk_scores_metrics_coverage_dt <- merge(entropy_12_wk_scores_metrics_coverage_dt, horizon_target_types, 
                                                  by = c("target_type"))

entropy_12_wk_scores_metrics_coverage_dt <- merge(entropy_12_wk_scores_metrics_coverage_dt, tmp, 
                                                  by = c("location", "max_time", "min_time", "model", "target_type"))

# entropy_12_wk_scores_metrics_coverage_dt <- merge(entropy_12_wk_scores_metrics_coverage_dt, wide_coverage_12_wk_results, 
#       by = c("location", "max_time", "min_time", "model", "target_type"))
# entropy_12_wk_scores_metrics_coverage_dt <- subset(entropy_12_wk_scores_metrics_coverage_dt,
#                                                    model == compare_against)
entropy_12_wk_scores_metrics_coverage_dt <- subset(entropy_12_wk_scores_metrics_coverage_dt,
                                                   compare_against == "COVIDhub-baseline")
entropy_12_wk_scores_metrics_coverage_dt[, cor(predictability, r2), by = c("model", "target_type")]
entropy_12_wk_scores_metrics_coverage_dt[, cor.test(predictability, interval_coverage), by = c("model", "target_type", "horizon")]
entropy_12_wk_scores_metrics_coverage_dt$model_factor <- factor(entropy_12_wk_scores_metrics_coverage_dt$model,
                                                                levels = covid_model_levels)
entropy_12_wk_scores_metrics_coverage_dt[, mean(r2), by = "model"]

#3) Pinball loss - predictability
tmp <- copy(trailing_window_pinball_loss_by_forecast_horizon_individual_locations)
#Merge in horizons + target_type to pinball loss for merging
tmp <- merge(tmp, horizon_target_types, by = "horizon")
entropy_12_wk_pinball_loss_dt <- merge(tmp,location_entropy_results_12_wk_rolling_window,
                                       by = c("location", "max_time"))
entropy_12_wk_pinball_loss_dt

#Test correlation between any variable and predictability! ----
library(data.table)
library(ggplot2)
library(knitr)

library(data.table)
library(ggplot2)
library(knitr)
require("kableExtra")

correlation_analysis <- function(dt, variable_name) {
  # Ensure we are working with a data.table
  dt <- as.data.table(dt)
  
  # Step 1: Run correlation tests by group, handle missing values and too-small samples
  results <- dt[, {
    x <- predictability
    y <- get(variable_name)
    
    if (length(na.omit(x)) > 2 && length(na.omit(y)) > 2 && sd(y, na.rm = TRUE) > 0) {
      test <- cor.test(x, y)
      list(correlation = test$estimate,
           p_value = test$p.value,
           lower_ci = test$conf.int[1],
           upper_ci = test$conf.int[2],
           n = .N)
    } else {
      list(correlation = NA_real_,
           p_value = NA_real_,
           lower_ci = NA_real_,
           upper_ci = NA_real_,
           n = .N)
    }
  }, by = .(model, target_type)]
  
  # Step 2: Bonferroni correction
  results[, p_bonf := p.adjust(p_value, method = "bonferroni")]
  
  # Step 3: Formatting (skip rows with NA correlation)
  results[!is.na(correlation), `:=`(
    cor_fmt = sprintf("%.2f", correlation),
    ci_fmt = sprintf("[%.2f, %.2f]", lower_ci, upper_ci),
    p_fmt = ifelse(p_bonf < 0.001, "P < 0.001", sprintf("P = %.3g", p_bonf))
  )]
  
  # Step 4: Show formatted summary table
  # print(kable(results[, .(model, target_type, p_fmt)], 
  #             col.names = c("Model", "Target Type", 
  #                           paste0("Correlation with ", variable_name, 
  #                                  " (95% CI, Bonferroni-adjusted p)"))))
  # # latex_table <- kable(
  #   results[, .(model, target_type, p_fmt)],
  #   format = "latex",
  #   booktabs = TRUE,
  #   col.names = c("Model", "Target Type", "Bonferroni-adjusted $p$"),
  #   escape = FALSE,
  #   caption = paste0("Correlations between predictability and ", variable_name, ".")
  # ) %>%
  #   kable_styling(latex_options = c("hold_position", "striped"), position = "center", full_width = FALSE)
  
  latex_table <- kable(
    results[, .(model, target_type, cor_fmt, ci_fmt, p_fmt)],
    format = "latex",
    booktabs = TRUE,
    col.names = c("Model", "Target Type", "r", "95\\% CI","Bonferroni-adjusted \\(p\\)"),
    escape = FALSE,
    caption = paste0("Correlations between predictability and ", variable_name, ".")
  ) %>%
    kable_styling(latex_options = c("hold_position", "striped"), position = "center", full_width = FALSE)
  
  # Step 5: Plot results (skip NAs)
  
  results$model_factor <-   
    factor(results$model,
           levels = covid_model_levels)
  plot <- ggplot(results[!is.na(correlation)]) +
    geom_point(aes(x = correlation, y = target_type, color = model_factor), position = position_dodge(width = 0.5), size = 3) +
    geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci, y = target_type, color = model_factor),
                   height = 0.2, position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_wrap(~ model_factor, scales = "free_y",
               labeller = labeller(model_factor = covid_model_labels))+
    theme(strip.text = element_text(size = 12)) +
    labs(x = paste0("Correlation"),
         y = "Target Type") +
    theme_light()+theme_custom()+
    scale_color_manual("Model", values = covid_model_colours,
                       labels = corrected_covid_model_names)+
      guides(colour = guide_legend(override.aes = list(linewidth = 2)))
    
  
  # print(plot)
  
  # Step 6: Text summary (skip NAs)
  if (nrow(results[!is.na(correlation)]) > 0) {
    top_result <- results[!is.na(correlation)][order(p_bonf)][1]
    
    summary_text <- paste0(
      "Correlations between predictability and ", variable_name,
      " ranged from ", sprintf("%.2f", min(results$correlation, na.rm = TRUE)),
      " to ", sprintf("%.2f", max(results$correlation, na.rm = TRUE)), ". ",
      sum(results$p_bonf < 0.05, na.rm = TRUE), " comparisons remained statistically significant after Bonferroni correction. ",
      "The strongest association was observed for the ", top_result$model,
      " model (", top_result$target_type, "), with a correlation of ",
      sprintf("%.2f", top_result$correlation), " (95% CI: ",
      sprintf("%.2f", top_result$lower_ci), "–",
      sprintf("%.2f", top_result$upper_ci), ", Bonferroni-adjusted p = ",
      sprintf("%.3g", top_result$p_bonf), ")."
    )
    
    cat("\n", summary_text, "\n")
  } else {
    cat("\nNo valid correlations were computed. Check for missing or insufficient data.\n")
  
  }
  return(list(latex_table, plot))
}
r2_predictability_results <- correlation_analysis(entropy_12_wk_scores_metrics_coverage_dt,
                     "r2")
r2_predictability_results
r2_predictability_table <- r2_predictability_results[[1]]
r2_predictability_table

r2_predictability_plot <- r2_predictability_results[[2]]
r2_predictability_plot
ggsave(r2_predictability_plot,
       filename = file.path(forecasting.covid_forecasts.out.dir,
                            "r2_predictability_plot.pdf"), h = 14, w = 22)



wis_scaled_relative_skill_predictability_results <- correlation_analysis(entropy_12_wk_scores_metrics_coverage_dt,
                                                  "wis_scaled_relative_skill")
wis_scaled_relative_skill_predictability_results
wis_scaled_relative_skill_predictability_table <- wis_scaled_relative_skill_predictability_results[[1]]
wis_scaled_relative_skill_predictability_table




entropy_12_wk_scores_metrics_coverage_dt_minus_baseline <- 
  entropy_12_wk_scores_metrics_coverage_dt[which(model != "COVIDhub-baseline")]
covid_model_levels
covid_model_levels_minus_baseline <- covid_model_levels[-c(2)]
corrected_covid_model_names_minus_baseline <- corrected_covid_model_names[-c(2)]
entropy_12_wk_scores_metrics_coverage_dt_minus_baseline$model_factor <- 
  factor(entropy_12_wk_scores_metrics_coverage_dt_minus_baseline$model,
         levels = covid_model_levels_minus_baseline)
wis_scaled_relative_skill_predictability_results <- correlation_analysis(entropy_12_wk_scores_metrics_coverage_dt,
                                                                         "wis_scaled_relative_skill")
wis_scaled_relative_skill_predictability_plot <- wis_scaled_relative_skill_predictability_results[[2]]+
  scale_colour_manual("Model", labels = corrected_covid_model_names_minus_baseline,
                      values = covid_model_colours)
wis_scaled_relative_skill_predictability_plot
ggsave(wis_scaled_relative_skill_predictability_plot,
       filename = file.path(forecasting.covid_forecasts.out.dir,
                            "wis_scaled_relative_skill_predictability_plot.pdf"), h = 14, w = 22)




interval_coverage_predictability_results <- correlation_analysis(entropy_12_wk_scores_metrics_coverage_dt,
                                                                         "interval_coverage")
interval_coverage_predictability_results
interval_coverage_predictability_table <- interval_coverage_predictability_results[[1]]
interval_coverage_predictability_table

interval_coverage_predictability_plot <- interval_coverage_predictability_results[[2]]
ggsave(interval_coverage_predictability_plot,
       filename = file.path(forecasting.covid_forecasts.out.dir,
                            "interval_coverage_predictability_plot.pdf"), h = 14, w = 22)



#1) Autocorrelation? ----
#Test autocorrelation
head(location_entropy_results_12_wk_rolling_window)
location_entropy_results_12_wk_rolling_window
# Ensure the data is ordered by time within each location
setorder(location_entropy_results_12_wk_rolling_window, location, max_time)






#BOXPLOTS ---
entropy_12_wk_scores_metrics_coverage_dt_minus_baseline <- 
  entropy_12_wk_scores_metrics_coverage_dt[which(model != "COVIDhub-baseline")]
covid_model_levels
covid_model_levels_minus_baseline <- covid_model_levels[-c(2)]
corrected_covid_model_names_minus_baseline <- corrected_covid_model_names[-c(2)]
entropy_12_wk_scores_metrics_coverage_dt_minus_baseline$model_factor <- 
  factor(entropy_12_wk_scores_metrics_coverage_dt_minus_baseline$model,
         levels = covid_model_levels_minus_baseline)

rwis_predictability_boxplot <- ggplot(entropy_12_wk_scores_metrics_coverage_dt_minus_baseline)+
  geom_boxplot(aes(x = model_factor, y = wis_scaled_relative_skill, fill = low_predictability_factor),
               outlier.shape = NA)+
  coord_cartesian(ylim = c(0, 4))+
  scale_x_discrete(labels = corrected_covid_model_names_minus_baseline)+
  geom_hline(aes(yintercept = 1),
             linewidth = 1.4,
             linetype = "dashed",
             color = "darkgreen")+
  theme_light()+theme_custom()+
  theme(axis.text.x = element_text(size=20,
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(size=20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.position = "bottom",
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 20),  # Adjust legend text size
        legend.title = element_text(size = 20))+
  labs(y = "rWIS", x = "Model" )+
  rotate_x_axis_theme+
  scale_fill_manual("Predictability < 0.3?",
                    values = low_high_predictability_colours,
                    labels = c("False", "True"))
rwis_predictability_boxplot


interval_coverage_predictability_boxplot <- ggplot(entropy_12_wk_scores_metrics_coverage_dt)+
  geom_boxplot(aes(x = model_factor, y = interval_coverage, fill = low_predictability_factor),
               outlier.shape = NA)+
  coord_cartesian(ylim = c(0, 1))+
  scale_x_discrete(labels = corrected_covid_model_names)+
  theme_light()+theme_custom()+
  theme(axis.text.x = element_text(size=20,
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(size=20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.position = "bottom",
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 20),  # Adjust legend text size
        legend.title = element_text(size = 20))+
  labs(y = "95% PI Coverage", x = "Model" )+
  rotate_x_axis_theme+
  scale_fill_manual("Predictability < 0.3?",
                    values = low_high_predictability_colours,
                    labels = c("False", "True"))
interval_coverage_predictability_boxplot



r2_predictability_boxplot <- ggplot(entropy_12_wk_scores_metrics_coverage_dt)+
  geom_boxplot(aes(x = model_factor, y = r2, fill = low_predictability_factor),
               outlier.shape = NA)+
  coord_cartesian(ylim = c(0, 1))+
  scale_x_discrete(labels = corrected_covid_model_names)+
  theme_light()+theme_custom()+
  theme(axis.text.x = element_text(size=20,
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(size=20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.position = "bottom",
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 20),  # Adjust legend text size
        legend.title = element_text(size = 20))+
  labs(y = expression(R^2), x = "Model" )+
  rotate_x_axis_theme+
  scale_fill_manual("Predictability < 0.3?",
                    values = low_high_predictability_colours,
                    labels = c("False", "True"))
r2_predictability_boxplot


entropy_12_wk_pinball_loss_dt$model_factor <- factor(entropy_12_wk_pinball_loss_dt$model,
                                                     levels = covid_model_levels)
entropy_12_wk_pinball_loss_dt



entropy_12_wk_pinball_loss_dt_minus_baseline <- 
  entropy_12_wk_pinball_loss_dt[which(model != "COVIDhub-baseline")]
entropy_12_wk_pinball_loss_dt_minus_baseline$model_factor <- 
  factor(entropy_12_wk_pinball_loss_dt_minus_baseline$model,
         levels = covid_model_levels_minus_baseline)
tmp <- subset(entropy_12_wk_pinball_loss_dt_minus_baseline,
              quantile_level == 0.9)
pinball_loss_predictability_boxplot <- ggplot(tmp)+
  geom_boxplot(aes(x = model_factor, y = relative_score, fill = low_predictability_factor),
               outlier.shape = NA)+
  coord_cartesian(ylim = c(0, 4))+
  scale_x_discrete(labels = corrected_covid_model_names_minus_baseline)+
  geom_hline(aes(yintercept = 1),
             linewidth = 1.4,
             linetype = "dashed",
             color = "darkgreen")+
  theme_light()+theme_custom()+
  theme(axis.text.x = element_text(size=20,
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(size=20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.position = "bottom",
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 20),  # Adjust legend text size
        legend.title = element_text(size = 20))+
  labs(y = expression(paste("rPL(", alpha, "= 0.1)")), x = "Model" )+
  rotate_x_axis_theme+
  scale_fill_manual("Predictability < 0.3?",
                    values = low_high_predictability_colours,
                    labels = c("False", "True"))



#


# Step 1: Compute ACFs up to lag 12 for each location
acf_by_location <- location_entropy_results_12_wk_rolling_window[
  ,
  {
    acf_vals <- acf(predictability, lag.max = 12, plot = FALSE)$acf
    # Drop lag 0 (acf[1]) and return as a data.table with lag labels
    .(lag = 1:12, autocorrelation = acf_vals[2:13])
  },
  by = location
]

# Step 2: Summarize autocorrelations across locations
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

ggplot(acf_summary, aes(x = lag)) +
  # Interquartile range (Q1 to Q3)
  geom_ribbon(aes(ymin = lower_quartile, ymax = upper_quartile),
              fill = "skyblue", alpha = 0.4) +
  # Optional: add standard deviation band
  geom_ribbon(aes(ymin = mean_autocorrelation - sd_autocorrelation,
                  ymax = mean_autocorrelation + sd_autocorrelation),
              fill = "lightgrey", alpha = 0.3) +
  # Mean line
  geom_line(aes(y = mean_autocorrelation), color = "blue", size = 1) +
  # Median line
  geom_line(aes(y = median_autocorrelation), color = "darkgreen", linetype = "dashed", size = 1) +
  labs(
    title = "Autocorrelation (ACF) of PE Summary Across Locations",
    x = "Lag",
    y = "Autocorrelation",
    caption = "Shaded regions show interquartile range (blue) and ±1 SD (grey)"
  ) +
  theme_minimal()





#Cross-correlation: Predictability in past X window predicting r2
#2) Epidemic dynamics and forecast ----
#Second, analysis of predictability + epidemic dynamics 

setorder(location_entropy_results_12_wk_rolling_window, location, max_time)

# Define the function to compute cross-correlation at lags 1:12
compute_ccf <- function(x, y, max_lag = 4) {
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
  entropy_12_wk_scores_metrics_coverage_dt[
    , compute_ccf(predictability, r2), by = c("location", "target_type", "model")
  ][, variable := "r2"],
  
  # 2. Predictability → wis_scaled_relative_skill
  entropy_12_wk_scores_metrics_coverage_dt[
    , compute_ccf(predictability, wis_scaled_relative_skill), by = c("location", "target_type", "model")
  ][, variable := "wis_scaled_relative_skill"],
  
  # 3. Predictability → relative_score
  entropy_12_wk_scores_metrics_coverage_dt[
    , compute_ccf(predictability, interval_coverage_0.9), by = c("location", "target_type", "model")
  ][, variable := "interval_coverage_0.9"],
  
  entropy_12_wk_scores_metrics_coverage_dt[
    , compute_ccf(predictability, interval_coverage_deviation_0.9), by = c("location", "target_type", "model")
  ][, variable := "interval_coverage_deviation_0.9"],

  entropy_12_wk_scores_metrics_coverage_dt[
    , compute_ccf(predictability, interval_coverage_0.5), by = c("location", "target_type", "model")
  ][, variable := "interval_coverage_0.5"],
  
  entropy_12_wk_scores_metrics_coverage_dt[
    , compute_ccf(predictability, interval_coverage_deviation_0.5), by = c("location", "target_type", "model")
  ][, variable := "interval_coverage_deviation_0.5"],
  
  
  entropy_12_wk_scores_metrics_coverage_dt[
    , compute_ccf(predictability, quantile_coverage_0.1), by = c("location", "target_type", "model")
  ][, variable := "quantile_coverage_0.1"],
  
  entropy_12_wk_scores_metrics_coverage_dt[
    , compute_ccf(predictability, quantile_coverage_deviation_0.1), by = c("location", "target_type", "model")
  ][, variable := "quantile_coverage_deviation_0.1"],

  
  entropy_12_wk_scores_metrics_coverage_dt[
    , compute_ccf(median_med_Rt, wis_scaled_relative_skill), by = c("location", "target_type", "model")
  ][, variable := "Rt_wis_skill"]
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
  by = .(variable, lag, model, target_type)
]

num_rows <- length(unique(ccf_summary$variable))


unique(ccf_summary$variable)
tmp <- subset(ccf_summary,
              model == "COVIDhub-4_week_ensemble")
tmp <- tmp[which(variable %in% c("r2", "wis_scaled_relative_skill", 
                                 "interval_coverage_0.9", "Rt_wis_skill"))]
tmp <- subset(tmp, target_type %in% covid_visualisation_target_types)
ggplot(tmp, aes(x = lag, y = mean_ccf, color = model, fill = model)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower_q, ymax = upper_q), alpha = 0.2, color = NA) +
  facet_wrap(target_type~ variable, scales = "free_y", ncol = 2) +
  labs(
    title = "Cross-Correlation of Past Predictability with Forecast/Transmission Metrics",
    x = "Lag (weeks)",
    y = "Cross-Correlation (Predictability → Current Value)",
    caption = "Shaded area: interquartile range across locations"
  ) +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_fill_manual("Model", values = covid_model_colours,
                    labels = corrected_covid_model_names)+
  scale_color_manual("Model", values = covid_model_colours,
                    labels = corrected_covid_model_names)





ggplot(ccf_summary, aes(x = lag, y = mean_ccf, color = variable, fill = variable)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower_q, ymax = upper_q), alpha = 0.2, color = NA) +
  facet_wrap(target_type~ model, scales = "free_y", nrow = num_rows) +
  labs(
    title = "Cross-Correlation of Past Predictability with Forecast/Transmission Metrics",
    x = "Lag (weeks)",
    y = "Cross-Correlation (Predictability → Current Value)",
    caption = "Shaded area: interquartile range across locations"
  ) +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_fill_manual("Model", values = covid_model_colours,
                    labels = corrected_covid_model_names)+
  scale_color_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)







rt_predictability_ccf_long <- rbindlist(list(
  # 1. Predictability → median_med_Rt
  location_entropy_results_12_wk_rolling_window[
    , compute_ccf(predictability, median_med_Rt, max_lag = 12), by = c("location")
  ][, variable := "R(t)"],
  
  location_entropy_results_12_wk_rolling_window[
    , compute_ccf(predictability, total_cases, max_lag = 12), by = c("location")
  ][, variable := "Total Cases"],
  
  location_entropy_results_12_wk_rolling_window[
    , compute_ccf(predictability, mean_cases, max_lag = 12), by = c("location")
  ][, variable := "Mean Cases"],
  
  location_entropy_results_12_wk_rolling_window[
  , compute_ccf(predictability, scaled_mean_cases, max_lag = 12), by = c("location")
  ][, variable := "Scaled Mean Cases"]))


ccf_rt_summary <- rt_predictability_ccf_long[
  , .(
    mean_ccf   = mean(cross_correlation, na.rm = TRUE),
    sd_ccf     = sd(cross_correlation, na.rm = TRUE),
    median_ccf = median(cross_correlation, na.rm = TRUE),
    lower_q    = quantile(cross_correlation, 0.25, na.rm = TRUE),
    upper_q    = quantile(cross_correlation, 0.75, na.rm = TRUE),
    min_ccf    = min(cross_correlation, na.rm = TRUE),
    max_ccf    = max(cross_correlation, na.rm = TRUE)
  ),
  by = .(variable, lag, location)
]
ccf_rt_summary
#Past high predictability as predictor of future high Rt
#Historically low predictability -> Future Higher cases?
p <- ggplot(ccf_rt_summary, aes(x = lag, y = mean_ccf, color = variable, fill = variable)) +
  facet_wrap(location ~. , scales = "free_y")+
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower_q, ymax = upper_q), alpha = 0.2, color = NA)
print(p)
ggplot(location_entropy_results_12_wk_rolling_window) + 
  geom_boxplot(aes(x = phase_Rt_factor, y = predictability),
               outlier.shape = NA)+
  ylim(c(0, 1))+
  theme(legend.position = "bottom")




pinball_ccf_long <- rbindlist(list(
  # 1. Predictability → median_med_Rt
  entropy_12_wk_pinball_loss_dt[
    , compute_ccf(predictability, relative_score), by = c("location", "target_type", "model", "quantile_level")
  ][, variable := "rPL"]
))
pinball_ccf_long
pinball_ccf_summary <- pinball_ccf_long[
  , .(
    mean_ccf   = mean(cross_correlation, na.rm = TRUE),
    sd_ccf     = sd(cross_correlation, na.rm = TRUE),
    median_ccf = median(cross_correlation, na.rm = TRUE),
    lower_q    = quantile(cross_correlation, 0.25, na.rm = TRUE),
    upper_q    = quantile(cross_correlation, 0.75, na.rm = TRUE),
    min_ccf    = min(cross_correlation, na.rm = TRUE),
    max_ccf    = max(cross_correlation, na.rm = TRUE)
  ),
  by = .(variable, lag, model, target_type, quantile_level)
]



visualisation_pinball_entropy_ccf_dt <- subset(pinball_ccf_summary,
              model == "COVIDhub-4_week_ensemble")
visualisation_pinball_entropy_ccf_dt <- subset(visualisation_pinball_entropy_ccf_dt, target_type %in% covid_visualisation_target_types)
visualisation_pinball_entropy_ccf_dt <- subset(visualisation_pinball_entropy_ccf_dt, quantile_level %in% new_covid_visualisation_quantiles)
ggplot(visualisation_pinball_entropy_ccf_dt, aes(x = lag, y = mean_ccf, color = model, fill = model)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower_q, ymax = upper_q), alpha = 0.2, color = NA) +
  facet_wrap(target_type~ quantile_level, scales = "free_y", nrow = 2) +
  labs(
    title = "Cross-Correlation of Past Predictability with Forecast/Transmission Metrics",
    x = "Lag (weeks)",
    y = "Cross-Correlation (Predictability → Current Value)",
    caption = "Shaded area: interquartile range across locations"
  ) +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_fill_manual("Model", values = covid_model_colours,
                    labels = corrected_covid_model_names)+
  scale_color_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)

#Higher Predictability -> Less valuable model, relative to baseline
