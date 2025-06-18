#1) Pick two onsets of waves
#Decisions would make!
#Aggregate WIS Scoring  ----
covid_visualisation_horizons <- c(1, 4)
covid_visualisation_target_types <- c("1 wk ahead inc case", "4 wk ahead inc case")
horizon_target_types <- unique(subset(state_level_covid_forecasts_dt, select = c("target_type", "horizon")))
all_quantiles <- unique(state_level_covid_forecasts_dt$quantile_level)
all_quantiles
new_covid_visualisation_quantiles <-1-c(0.025, 0.1, 0.25) #1-alpha
higher_cl_ratios <- all_quantiles[which(!(all_quantiles %in% new_covid_visualisation_quantiles))]
higher_cl_ratios
covid_target_types_horizons <- 
  data.table(horizon = covid_visualisation_horizons,
             target_type = covid_visualisation_target_types)
# covid_visualisation_quantiles <- c(0.1, 0.5, 0.9)
state_level_covid_forecasts_dt
#Overall performance of models
state_level_covid_forecasts_dt
wis_covid_aggregate_performance <- 
  all_time_points_forecast_evaluation_function(state_level_covid_forecasts_dt,
                                               forecast_representation = "quantile")
wis_covid_aggregate_performance
wis_covid_aggregate_performance$model <- factor(wis_covid_aggregate_performance$model,
                                                levels = covid_model_levels)
wis_covid_aggregate_performance <- 
  subset(wis_covid_aggregate_performance,
         compare_against == "COVIDhub-baseline")
wis_covid_aggregate_performance
visualisation_wis_covid_aggregate_performance <- 
  subset(wis_covid_aggregate_performance,
         target_type %in% covid_visualisation_target_types)
wis_covid_aggregate_performance
wis_covid_aggregate_performance[, horizon:= as.numeric(substr(target_type, 1, 1))]

ggplot(wis_covid_aggregate_performance)+
  geom_line(aes(x = horizon, y = wis_scaled_relative_skill, color = model))+
  theme_light()
theme_custom
all_horizons_wis_covid_aggregate_performance_all_locations_plot <- ggplot(wis_covid_aggregate_performance)+
  geom_col(aes(y = wis_scaled_relative_skill,
               x= model,
               fill = model))+
  scale_x_discrete(labels = corrected_covid_model_names)+
  geom_hline(aes(yintercept = 1),
             linewidth = 1.4,
             linetype = "dashed")+
  facet_wrap(target_type ~., 
             ncol = 1)+
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
  scale_fill_manual("Model", values = covid_model_colours,
                    labels = corrected_covid_model_names)
#Appendix plot
all_horizons_wis_covid_aggregate_performance_all_locations_plot

wis_covid_aggregate_performance_all_locations_plot <- ggplot(visualisation_wis_covid_aggregate_performance)+
  geom_col(aes(y = wis_scaled_relative_skill,
                x= model,
                fill = model))+
  scale_x_discrete(labels = corrected_covid_model_names)+
  geom_hline(aes(yintercept = 1),
             linewidth = 1.4,
             linetype = "dashed")+
  facet_wrap(target_type ~., 
             ncol = 1)+
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
  scale_fill_manual("Model", values = covid_model_colours,
                         labels = corrected_covid_model_names)
covid_model_colours
wis_covid_aggregate_performance_all_locations_plot
#Now, individual locations
wis_covid_aggregate_performance_individual_locations <- 
  all_time_points_location_specific_forecast_evaluation_function(state_level_covid_forecasts_dt,
                                                               forecast_representation = "quantile")
wis_covid_aggregate_performance_individual_locations <-merge(wis_covid_aggregate_performance_individual_locations,
      location_location_names, by = "location")
wis_covid_aggregate_performance_individual_locations <- 
  subset(wis_covid_aggregate_performance_individual_locations,
         compare_against == "COVIDhub-baseline")
setkeyv(wis_covid_aggregate_performance_individual_locations, "location_name")
wis_covid_aggregate_performance_individual_locations[, location_factor:= factor(location_name)]
wis_covid_aggregate_performance_individual_locations$model_factor <- 
  factor(wis_covid_aggregate_performance_individual_locations$model,
         levels = covid_model_levels)
wis_covid_aggregate_performance_individual_locations[, horizon:= as.numeric(substr(target_type, 1, 1))]

ggplot(wis_covid_aggregate_performance_individual_locations)+
  geom_line(aes(x = horizon, y = wis_scaled_relative_skill, color = model_factor))+
  facet_wrap(location ~., 
             scales = "free_y")+
  theme_light()+
  scale_color_manual("Model", values = covid_model_colours,
                    labels = corrected_covid_model_names)
visualisation_wis_covid_aggregate_performance_individual_locations <- 
  subset(wis_covid_aggregate_performance_individual_locations,
         target_type %in% covid_visualisation_target_types)
min_wis <- min(visualisation_wis_covid_aggregate_performance_individual_locations$wis_scaled_relative_skill)
max_wis <- max(visualisation_wis_covid_aggregate_performance_individual_locations$wis_scaled_relative_skill)
visualisation_wis_covid_aggregate_performance_individual_locations
wis_covid_aggregate_performance_individual_locations_plot <- 
  ggplot(visualisation_wis_covid_aggregate_performance_individual_locations)+
  geom_raster(aes(x = model_factor, y = location_factor, fill = wis_scaled_relative_skill))+
  facet_wrap(target_type ~., 
             scales = "free_x",
             ncol = 2)+
  scale_x_discrete(labels = corrected_covid_model_names)+
  theme_light()+theme_custom()+
  labs(y = "State", x = "Model" )+
  scale_fill_continuous_divergingx(palette = "RdBu",
                                   mid = 1,
                                   rev = 1,
                                   limits = c(min_wis, max_wis))+
  theme(
    axis.text.x = element_text(size=20,
                               angle = 45, hjust = 1),
    axis.text.y = element_text(size=20),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 20),  # Adjust legend text size
    legend.title = element_text(size = 20))+ 
  guides(fill = guide_colorbar(barheight = 10,
                               title = "rWIS"))
wis_covid_aggregate_performance_individual_locations_plot
?guide_colorbar
#Then, trailing window
?scale_fill_continuous_divergingx

#Pinball loss decomposition ----
#Trailing window pinball loss + corresponding Murphy diagrams
pinball_loss_decomposition_results <- 
  run_pinball_loss_decomposition_trailing_window_over_time(input_data = state_level_covid_forecasts_dt, #data
                                                           window_length = 12,
                                                           factor_model_levels = covid_model_levels,
                                                           plot_model_names = corrected_covid_model_names,
                                                           plot_model_colours = covid_model_colours,
                                                           focus_specific_quantiles = NULL,
                                                           real_time = NULL,
                                                           option_trailing_window_on = TRUE)
saveRDS(pinball_loss_decomposition_results,
        "pinball_loss_decomposition_results.RDS")
pinball_loss_decomposition_results[[]]
pinball_loss_decomposition_results[[2]]

#Trailing window QS results by location
trailing_window_pinball_loss_by_forecast_horizon_individual_locations <-
  pinball_loss_decomposition_results[[13]]
trailing_window_pinball_loss_by_forecast_horizon_individual_locations[[1]]
#Adding missing time column to window-specific results: (Future: WILL JUST ADD TO FUNCTION!)
setkeyv(state_level_covid_forecasts_dt, c("target_end_date", "horizon", "model"))
times <- unique(state_level_covid_forecasts_dt$target_end_date)
locations <- unique(state_level_covid_forecasts_dt$location)

runner_windows_list <- runner(times, k = 12)
runner_windows_list <- runner_windows_list[12:length(runner_windows_list)] 

max_times <- lapply(runner_windows_list, function(x) max(as.Date(x)))
max_times <- unlist(max_times)
max_times <- as.Date(max_times)
trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, max_time:= rep(max_times, 
                                                                                       each = length(unique(quantile_level))*length(unique(horizon))*
                                                                                         length(unique(model))*length(unique(location)))]
baseline_trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- 
  subset(trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
         model == "COVIDhub-baseline")
baseline_trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- 
  subset(baseline_trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
       select = c("horizon", "location", "mcb", "dsc", "score", "quantile_level",
                  "max_time"))
setnames(baseline_trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
         c("mcb", "dsc", "score"),
         c("baseline_mcb", "baseline_dsc", "baseline_score"))
trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- merge(trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
      baseline_trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
      by = c("location", "horizon", "quantile_level", "max_time"))

trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, relative_score:= score/baseline_score]
#Max index is current time point
#Sliding window with stride of 1 week

trailing_window_pinball_loss_by_forecast_horizon_individual_locations$horizon
dim(trailing_window_pinball_loss_by_forecast_horizon_individual_locations)
trailing_window_pinball_loss_by_forecast_horizon_individual_locations


#These don't make that much sense (want to maximise dsc, minimise score)
trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, frac_mcb:= mcb/score]
trailing_window_pinball_loss_by_forecast_horizon_individual_locations[which(unc > score)]
trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, frac_unc:= unc/score]

# ggplot(trailing_window_pinball_loss_by_forecast_horizon_individual_locations) + 
#   geom_tile(aes(x = max_time, y = location, fill = log(unc)))+
#   scale_fill_viridis_c(option = "magma")+
#   facet_wrap(model ~., )
trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, frac_dsc:= dsc/score]
trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, frac_unc:= unc/score]
trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, dsc_vs_baseline:= dsc/baseline_dsc]
trailing_window_pinball_loss_by_forecast_horizon_individual_locations[which(dsc <0), dsc:= 0]
# trailing_window_pinball_loss_by_forecast_horizon_individual_locations[which(skill_score <0), ]
trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, skill_score:= (dsc-mcb)/unc]


tmp <- subset(trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
              model== "COVIDhub-4_week_ensemble" & horizon == 1 & quantile_level %in% new_covid_visualisation_quantiles)

ggplot(tmp)+
  geom_raster(aes(x=max_time, y = location, fill = relative_score))+
  facet_wrap(horizon ~ quantile_level, )+
  scale_fill_continuous_divergingx(palette = "RdBu",
                                   mid = 1,
                                   rev = 1,
                                   limits = c(0.4, 4),
                                   oob=squish)
summary(trailing_window_pinball_loss_by_forecast_horizon_individual_locations$relative_score)
#Individual Horizons, just extreme quantiles
summary(tmp$relative_score)
1-as.numeric(as.character(tmp$quantile_level))

for(i in 1:length(unique(horizon_target_types$horizon))){
  single_horizon_trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- 
    subset(trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
           target_type = horizon_target_types$target_type[i])
  single_horizon_trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- 
    subset(single_horizon_trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
           quantile_level %in% new_covid_visualisation_quantiles)
  single_horizon_trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- 
    merge(single_horizon_trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
        location_location_names,
        by = "location")
  horizon_in_q <- horizon_target_types$target_type[i]
  for(j in 1:length(covid_model_names)){
    model_in_q <- covid_model_names[j]
    tmp <- subset(single_horizon_trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
           model == model_in_q)
    tmp[, alpha :=  1-as.numeric(as.character(quantile_level))]
    tmp[, alpha_label := paste0("alpha==", alpha)]
    if(model_in_q != "COVIDhub-baseline"){
    tmp_plot <- ggplot(tmp)+
      geom_raster(aes(x=max_time, y = location_name, fill = relative_score))+
      facet_wrap(alpha_label ~. , labeller =label_parsed)+
      scale_fill_continuous_divergingx(palette = "RdBu",
                                       mid = 1,
                                       rev = 1,
                                       limits = c(0, 3),
                                       oob=squish)+
      # scale_fill_continuous_divergingx(palette = "RdBu",
      #                                  mid = 1,
      #                                  rev = 1,
      #                                  limits = c(0, 4),
      #                                  oob=squish)+
      theme_light()+theme_custom()+
      labs(y = "State", x = "Date" )+
      theme(
      axis.text.y = element_text(size=18,
                                 angle = 45, hjust = 1),
      axis.text.x = element_text(size=18,
                                 angle = 45, hjust = 1),
      plot.title = element_text(size = 20, hjust = 0.5),
      legend.position = "right",
      legend.text = element_text(size = 20),  # Adjust legend text size
      legend.title = element_text(size = 20))+ 
    guides(fill = guide_colorbar(barheight = 10,
                                 title = "rPL"))+
    theme(legend.position = "bottom")
  ggsave(tmp_plot,
         filename = file.path(forecasting.covid_forecasts.out.dir, paste0("single_horizon_", i, "_individual_location_rpl_model_", model_in_q ,"_12_wk_rolling_window.pdf")),
         h = 24, w = 22)
    }
  }
}



#Individual Horizons, just extreme quantiles
for(i in 1:length(unique(horizon_target_types$horizon))){
  single_horizon_trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- 
    subset(trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
           target_type = horizon_target_types$target_type[i])
  single_horizon_trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- 
    subset(single_horizon_trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
           quantile_level %in% new_covid_visualisation_quantiles)
  single_horizon_trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- 
    merge(single_horizon_trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
          location_location_names,
          by = "location")
  horizon_in_q <- horizon_target_types$target_type[i]
  for(j in 1:length(covid_model_names)){
    model_in_q <- covid_model_names[j]
    tmp <- subset(single_horizon_trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
                  model == model_in_q)
    tmp[, alpha :=  1-as.numeric(as.character(quantile_level))]
    tmp[, alpha_label := paste0("alpha==", alpha)]
    if(model_in_q != "COVIDhub-baseline"){
      tmp_plot <- ggplot(tmp)+
        geom_raster(aes(x=max_time, y = location_name, fill = relative_score))+
        facet_wrap(alpha_label ~. , labeller =label_parsed)+
        scale_fill_continuous_divergingx(palette = "RdBu",
                                         mid = 1,
                                         rev = 1,
                                         limits = c(0, 4),
                                         oob=squish)+
        theme_light()+theme_custom()+
        labs(y = "State", x = "Date" )+
        theme(
          axis.text.y = element_text(size=18,
                                     angle = 45, hjust = 1),
          axis.text.x = element_text(size=18,
                                     angle = 45, hjust = 1),
          plot.title = element_text(size = 20, hjust = 0.5),
          legend.position = "right",
          legend.text = element_text(size = 20),  # Adjust legend text size
          legend.title = element_text(size = 20))+ 
        guides(fill = guide_colorbar(barheight = 10,
                                     title = "rPL"))+
        theme(legend.position = "bottom")
      ggsave(tmp_plot,
             filename = file.path(forecasting.covid_forecasts.out.dir, paste0("single_horizon_", i, "_individual_location_rpl_model_", model_in_q ,"_12_wk_rolling_window.pdf")),
             h = 26, w = 22)
    }
  }
}



#Individual Horizons, just extreme quantiles
for(i in 1:length(unique(horizon_target_types$horizon))){
  single_horizon_trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- 
    subset(trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
           target_type = horizon_target_types$target_type[i])
  single_horizon_trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- 
    subset(single_horizon_trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
           quantile_level %in% higher_cl_ratios)
  single_horizon_trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- 
    merge(single_horizon_trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
          location_location_names,
          by = "location")
  horizon_in_q <- horizon_target_types$target_type[i]
  for(j in 1:length(covid_model_names)){
    model_in_q <- covid_model_names[j]
    tmp <- subset(single_horizon_trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
                  model == model_in_q)
    # tmp[, alpha_label := paste0("alpha==", 1-quantile_level)]
    tmp[, alpha :=  1-as.numeric(as.character(quantile_level))]
    tmp[, alpha_label := paste0("alpha==", alpha)]
    
    if(model_in_q != "COVIDhub-baseline"){
      tmp_plot <- ggplot(tmp)+
        geom_raster(aes(x=max_time, y = location_name, fill = relative_score))+
        facet_wrap(alpha_label ~. , labeller =label_parsed)+
        scale_fill_continuous_divergingx(palette = "RdBu",
                                         mid = 1,
                                         rev = 1,
                                         limits = c(0, 4),
                                         oob=squish)+
        theme_light()+theme_custom()+
        labs(y = "State", x = "Date" )+
        theme(
          axis.text.y = element_text(size=18,
                                     angle = 45, hjust = 1),
          axis.text.x = element_text(size=18,
                                     angle = 45, hjust = 1),
          plot.title = element_text(size = 20, hjust = 0.5),
          legend.position = "right",
          legend.text = element_text(size = 20),  # Adjust legend text size
          legend.title = element_text(size = 20))+ 
        guides(fill = guide_colorbar(barheight = 10,
                                     title = "rPL"))+
        theme(legend.position = "bottom")
      ggsave(tmp_plot,
             filename = file.path(forecasting.covid_forecasts.out.dir, paste0("higher_cl_ratios_single_horizon_", i, "_individual_location_rpl_model_", model_in_q ,"_12_wk_rolling_window.pdf")),
             h = 28, w = 26)
    }
  }
}





for(j in 1:length(covid_model_names)){
  model_in_q <- covid_model_names[j]
  tmp <- 
    subset(trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
           model == model_in_q)
  tmp <- 
    merge(tmp,
          location_location_names,
          by = "location")
  tmp[, alpha :=  1-as.numeric(as.character(quantile_level))]
  tmp[, alpha_label := paste0("alpha==", alpha)]
  if(model_in_q != "COVIDhub-baseline"){
      tmp_plot <- ggplot(tmp)+
        geom_raster(aes(x=max_time, y = location_name, fill = relative_score))+
        facet_wrap(alpha_label ~. , labeller =label_parsed)+
        scale_fill_continuous_divergingx(palette = "RdBu",
                                         mid = 1,
                                         rev = 1,
                                         limits = c(0, 4),
                                         oob=squish)+
        theme_light()+theme_custom()+
        labs(y = "State", x = "Date" )+
        theme(
          axis.text.y = element_text(size=18,
                                     angle = 45, hjust = 1),
          axis.text.x = element_text(size=18,
                                     angle = 45, hjust = 1),
          plot.title = element_text(size = 20, hjust = 0.5),
          legend.position = "right",
          legend.text = element_text(size = 20),  # Adjust legend text size
          legend.title = element_text(size = 20))+ 
        guides(fill = guide_colorbar(barheight = 10,
                                     title = "rPL"))+
        theme(legend.position = "bottom")
      ggsave(tmp_plot,
             filename = file.path(forecasting.covid_forecasts.out.dir, paste0("single_model_all_horizons_", i, "_individual_location_rpl_model_", model_in_q ,"_12_wk_rolling_window.pdf")),
             h = 24, w = 22)
    }
}


summary(trailing_window_pinball_loss_by_forecast_horizon_individual_locations$skill_score)
# trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, .(median_skill = median(relative_score),
#        iqr = IQR(relative_score),
#        outperform_rate = mean(relative_score < 1)),
#    by = .(model, predictability_group)]

#Linking QS to predictability ----
#Relationship between Rt and predictability (will move later)
location_entropy_results_12_wk_rolling_window
ggplot(location_entropy_results_12_wk_rolling_window)+
  geom_boxplot(aes(x = phase_Rt, y = raw_perm_entropy))+
  facet_wrap(location ~. )
trailing_window_pinball_loss_predictability_dt <- merge(trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
      location_entropy_results_12_wk_rolling_window,
      by = c("location", "max_time"))
unique(trailing_window_pinball_loss_predictability_dt$model)
# summary(trailing_window_pinball_loss_predictability_dt[which(model != "COVIDhub-baseline"), 
#                                                        cor(raw_perm_entropy,
#                                                              dsc_vs_baseline), 
#                                                        by = c("model", "horizon", 
#                                                               "location","quantile_level")]$V1)

tmp2 <- trailing_window_pinball_loss_predictability_dt[, cor(raw_perm_entropy,
                                                             frac_dsc), 
                                                       by = c("model", "horizon", 
                                                              "location","quantile_level")]
tmp2[which(model == "COVIDhub-4_week_ensemble"), mean(V1),by = c("quantile_level")]

summary(trailing_window_pinball_loss_predictability_dt[, cor(raw_perm_entropy,
                                                             frac_dsc), 
                                                     by = c("model", "horizon", 
                                                            "location","quantile_level")]$V1)
which(is.na(trailing_window_pinball_loss_predictability_dt$raw_perm_entropy))
summary(trailing_window_pinball_loss_predictability_dt[which(model != "COVIDhub-baseline"), cor(raw_perm_entropy,
                                                                                        relative_score), 
                                               by = c("model", "horizon", 
                                                      "location","quantile_level")]$V1)
ggplot(trailing_window_pinball_loss_predictability_dt)+
  geom_boxplot(aes(x = low_predictability_factor, y = relative_score))+
  facet_wrap(model ~ quantile_level, scales = "free_y")+theme_light()
tmp <- trailing_window_pinball_loss_predictability_dt[which(model != "COVIDhub-baseline"), cor(raw_perm_entropy,
                                                            relative_score), 
                                                       by = c("model", "horizon", 
                                                              "location","quantile_level")]
tmp
tmp[, mean(V1), by = c("quantile_level")]

unique(tmp$model)
tmp <- subset(trailing_window_pinball_loss_predictability_dt,
              location == "01")
trailing_window_pinball_loss_predictability_dt
tmp <- copy(trailing_window_pinball_loss_predictability_dt)
tmp <- 
  subset(tmp,
         quantile_level %in% new_covid_visualisation_quantiles)
tmp <- subset(tmp, 
              model == "COVIDhub-4_week_ensemble")
ggplot(tmp)+
  geom_boxplot(aes(x = low_predictability_factor, y = relative_score))+
  facet_wrap(horizon ~ quantile_level, scales = "free_y",
             nrow = 4)+theme_light()

ggplot(tmp) + geom_line(aes(x = max_time, y = skill_score, color = model))+
  facet_wrap(quantile_level ~ horizon, nrow = 3, scales = "free_y")+
  theme_light()+
  scale_color_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)

visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- subset(visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
                                                                                              location == "01")
visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations

















higher_cl_visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- 
  subset(trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
         quantile_level %in% higher_cl_ratios)
higher_cl_ratios
higher_cl_visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, horizon:= paste0(horizon, " wk ahead inc case")]
time_summary_higher_cl_visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- 
  higher_cl_visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, list(median_relative_score = median(relative_score)),
                                                                                      by = c("model", "max_time", 
                                                                                             "quantile_level", "horizon")]
setnames(time_summary_higher_cl_visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
         "horizon", "target_type")
time_summary_higher_cl_visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, alpha :=  1-as.numeric(as.character(quantile_level))]
time_summary_higher_cl_visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, alpha_label := paste0("alpha==", alpha)]

# time_summary_higher_cl_visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, alpha_label := paste0("alpha==", 1-quantile_level)]
#Trailing-window-specific quantile scores, median over all locations

higher_cl_ratios_trailing_window_location_specific_pinball_loss_summarised_over_all_locations_plot <-
  ggplot(time_summary_higher_cl_visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations)+
  geom_line(aes(x = max_time, y = median_relative_score, color = model))+
  labs(x = "Date", y = "Median rPL")+
  facet_wrap(alpha_label ~ target_type, nrow = 4, ncol = 4, scales = "free_y",
             labeller = labeller(alpha_label = label_parsed,
                                 target_type = label_value))+
  theme_light()+theme_custom()+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(legend.position = "bottom")+
  scale_color_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)+
  rotate_x_axis_theme+  guides(colour = guide_legend(override.aes = list(linewidth = 2)))
higher_cl_ratios_trailing_window_location_specific_pinball_loss_summarised_over_all_locations_plot
ggsave(higher_cl_ratios_trailing_window_location_specific_pinball_loss_summarised_over_all_locations_plot,
       file = file.path(forecasting.covid_forecasts.out.dir,
                        "higher_cl_ratios_trailing_window_location_specific_pinball_loss_summarised_over_all_locations_plot.pdf"),
       h = 24, w = 22)









visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- 
  subset(trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
         quantile_level %in% new_covid_visualisation_quantiles)
visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, horizon:= paste0(horizon, " wk ahead inc case")]
time_summary_visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- 
  visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, list(median_relative_score = median(relative_score)),
                                                                                      by = c("model", "max_time", 
                                                                                             "quantile_level", "horizon")]
setnames(time_summary_visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
         "horizon", "target_type")
time_summary_visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, alpha :=  1-as.numeric(as.character(quantile_level))]
time_summary_visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, alpha_label := paste0("alpha==", alpha)]

# time_summary_visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, alpha_label := paste0("alpha==", 1-quantile_level)]
names(time_summary_visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations)
#Trailing-window-specific quantile scores, median over all locations
trailing_window_location_specific_pinball_loss_summarised_over_all_locations_plot <-
  ggplot(time_summary_visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations)+
  geom_line(aes(x = max_time, y = median_relative_score, color = model))+
  labs(x = "Date", y = "Median rPL")+
  facet_wrap(alpha_label ~ target_type, nrow = 3, scales = "free_y",
             labeller = labeller(alpha_label = label_parsed,
                                 target_type = label_value))+
  theme_light()+theme_custom()+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(legend.position = "bottom")+
  scale_color_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)+
  rotate_x_axis_theme+  guides(colour = guide_legend(override.aes = list(linewidth = 2)))
trailing_window_location_specific_pinball_loss_summarised_over_all_locations_plot
ggsave(trailing_window_location_specific_pinball_loss_summarised_over_all_locations_plot,
       file = file.path(forecasting.covid_forecasts.out.dir,
                        "trailing_window_location_specific_pinball_loss_summarised_over_all_locations_plot.pdf"),
       h = 20, w = 22)



# location_summary_visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- 
#   visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations[, list(median_relative_score = median(relative_score)),
#                                                                                       by = c("model", "location", 
#                                                                                              "quantile_level", "horizon")]

#Summary of trailing window, location-specific quantile scores scores
visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations
trailing_window_location_specific_pinball_loss_summarised_over_all_time_points_plot <-
  ggplot(visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations)+
  geom_boxplot(aes(x = model, y = relative_score, fill = model),
               outlier.shape =  NA)+
  facet_wrap(quantile_level ~ horizon, nrow = 3, scales = "free_y")+
  coord_cartesian(ylim = c(0, 10))+
  theme_light()+
  theme(legend.position = "bottom")+
  scale_fill_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)+
  guides(colour = guide_legend(override.aes = list(linewidth = 2)))
trailing_window_location_specific_pinball_loss_summarised_over_all_time_points_plot




summary_visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations
# visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations <- subset(visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations,
#        location == "01")

ggplot(visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations)+
  geom_line(aes(x = max_time, y = relative_score, color = model))+
  facet_wrap(quantile_level ~ horizon, nrow = 3, scales = "free_y")+
  theme_light()+
  scale_color_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)
dev.off()
ggplot(visualisation_trailing_window_pinball_loss_by_forecast_horizon_individual_locations)+
  geom_line(aes(x = max_time, y = frac_dsc, color = model))+
  facet_wrap(horizon ~ quantile_level, nrow = 3, scales = "free_y")+
  theme_light()+
  scale_color_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)

trailing_window_pinball_loss_by_forecast_horizon_all_locations <-
  pinball_loss_decomposition_results[[9]]
trailing_window_pinball_loss_by_forecast_horizon_all_locations$horizon

#Following are model-specific. 
#Thus, for each model, we will
# investigate how do these relate to predictability and Rt (and their changes)
#Do dsc correlate with R
trailing_window_pinball_loss_by_forecast_horizon_all_locations[, frac_mcb:= mcb/score]
trailing_window_pinball_loss_by_forecast_horizon_all_locations[, frac_dsc:= dsc/score]
trailing_window_pinball_loss_by_forecast_horizon_all_locations[, frac_unc:= unc/score]

visualisation_trailing_window_pinball_loss_by_forecast_horizon_all_locations <- 
  subset(trailing_window_pinball_loss_by_forecast_horizon_all_locations,
              quantile_level %in% covid_visualisation_quantiles)
visualisation_trailing_window_pinball_loss_by_forecast_horizon_all_locations <- 
  subset(visualisation_trailing_window_pinball_loss_by_forecast_horizon_all_locations,
         horizon %in% covid_visualisation_horizons)

ggplot(visualisation_trailing_window_pinball_loss_by_forecast_horizon_all_locations)+
  geom_line(aes(x = max_time, y = score, color = model))+
  facet_wrap(horizon ~ quantile_level, nrow = 3, scales = "free_y")+
  theme_light()+
  scale_color_manual("Model", values = covid_model_colours,
                    labels = corrected_covid_model_names)
ggplot(visualisation_trailing_window_pinball_loss_by_forecast_horizon_all_locations)+
  geom_line(aes(x = max_time, y = frac_dsc, color = model))+
  facet_wrap(horizon ~ quantile_level, nrow = 3, scales = "free_y")+
  theme_light()+
  scale_color_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)

ggplot(visualisation_trailing_window_pinball_loss_by_forecast_horizon_all_locations)+
  geom_line(aes(x = max_time, y = frac_mcb, color = model))+
  facet_wrap(horizon ~ quantile_level, nrow = 3, scales = "free_y")+
  theme_light()+
  scale_color_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)

ggplot(visualisation_trailing_window_pinball_loss_by_forecast_horizon_all_locations)+
  geom_line(aes(x = max_time, y = dsc, color = model))+
  facet_wrap(horizon ~ quantile_level, nrow = 2, ncol = 3, scales = "free_y")+
  theme_light()+
  scale_color_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)


tmp <- copy(pinball_loss_decomposition_results[[1]])

all_quantiles_pinball_decomposition_plots <- plot_selected_pinball_decomposition(pinball_loss_decomposition_results[[1]],
                                      chosen_quantiles = unique(state_level_covid_forecasts_dt$quantile_level),
                                      legend_on = NULL)
all_quantiles_pinball_decomposition_plots
ggsave(location_past_other_quantiles_pinball_decomposition_plots,
       filename = file.path(forecasting.covid_forecasts.out.dir,"all_quantiles_pinball_decomposition_plots.pdf"),
       h = 10, w = 24)


pinball_loss_results_all_locations <- pinball_loss_decomposition_results[[1]]



pinball_loss_results_all_locations
pinball_loss_plot_list <- list()
for(i in 1:length(covid_visualisation_horizons)){
  horizon_in_q <-covid_visualisation_horizons[i]
  target_type_in_q <-covid_visualisation_target_types[i]
  
  chosen_horizon_scores <- subset(pinball_loss_results_all_locations,
                                  horizon == horizon_in_q)
  chosen_horizon_scores <- subset(chosen_horizon_scores,
                                  quantile_level %in% new_covid_visualisation_quantiles)
  
  chosen_horizon_scores[, target_type:= target_type_in_q]
  chosen_horizon_scores$model <- factor(chosen_horizon_scores$model,
                                        levels = covid_model_levels)
  num_models <- length(unique(chosen_horizon_scores$model))
  chosen_horizon_isolines <- get_isolines_from_scores(chosen_horizon_scores,
                                                      num_models)
  # print(chosen_horizon_isolines)
  chosen_horizon_isolines <- subset(chosen_horizon_isolines,
                                    quantile_level %in% new_covid_visualisation_quantiles)
  pinball_loss_plot_list[[i]] <- 
    plot_pinball_loss_decomposition(chosen_horizon_scores,
                                    chosen_horizon_isolines,
                                    corrected_covid_model_names,
                                    covid_model_colours,
                                    specific_quantiles = new_covid_visualisation_quantiles)
}


#UNCOMMENT LATER:
# pinball_loss_plot_results_all_locations <- pinball_loss_decomposition_results[[2]]


covid_facet_pinball_loss_decomposition_diagram <- 
  ggarrange(NULL, pinball_loss_plot_list[[1]] + rotate_x_axis_theme, NULL ,pinball_loss_plot_list[[2]] + rotate_x_axis_theme,
            nrow = 4,ncol = 1,
            legend = "none",
            labels = c("1 week ahead", "","4 week ahead", ""),
            font.label = list(size = 18),
            heights = c(0.1, 0.75, 0.1,0.75))
# covid_facet_pinball_loss_decomposition_diagram <- ggarrange(NULL, covid_facet_pinball_loss_decomposition_diagram,
#                                                             nrow = 1, ncol = 2, widths = c(0.1, 1.0))
covid_facet_pinball_loss_decomposition_diagram
covid_facet_pinball_loss_decomposition_diagram <- ggarrange(NULL, NULL, NULL, covid_facet_pinball_loss_decomposition_diagram,
                                                            label.x = 0,    # Horizontal position: 0 (left) to 1 (right)
                                                            label.y = 1,
                                                            hjust = -0.25,
                                                            labels = c("A) User-specific decomposition (Pinball loss)", ""),
                                                            font.label = list(size = 20, face = "bold"),
                                                            nrow = 2, ncol = 2,
                                                          heights = c(0.1, 1.0),
                                                            widths = c(0.1, 1.0))
covid_facet_pinball_loss_decomposition_diagram



pinball_loss_plot_results_all_locations[[1]] + facet_wrap(quantile_level ~., scales = "free", ncol = 4)
# state_covid_quantile_scores$model <- fct_relevel(state_covid_quantile_scores$model, "COVIDhub-baseline", "COVIDhub-ensemble", "KITmetricslab")
pinball_loss_murphy_results_all_locations <- pinball_loss_decomposition_results[[3]]
setkeyv(pinball_loss_murphy_results_all_locations, "model")
covid_model_levels



pinball_loss_murphy_results_all_locations$model <- factor(pinball_loss_murphy_results_all_locations$model,
                                                          levels = covid_model_levels)
pinball_loss_murphy_results_all_locations
plot_murphy_diagram

visualisation_pinball_loss_murphy_results_all_locations <- 
  subset(pinball_loss_murphy_results_all_locations,
         horizon %in% covid_visualisation_horizons)
visualisation_pinball_loss_murphy_results_all_locations <- 
  subset(visualisation_pinball_loss_murphy_results_all_locations,
         quantile_level %in% new_covid_visualisation_quantiles)
visualisation_pinball_loss_murphy_results_all_locations
visualisation_pinball_loss_murphy_results_all_locations <- 
  merge(visualisation_pinball_loss_murphy_results_all_locations,
        covid_target_types_horizons,
        by = "horizon")
pinball_loss_murphy_plot_list <- list()
for(i in 1:length(covid_visualisation_horizons)){
  horizon_in_q <-covid_visualisation_horizons[i]
  target_type_in_q <-covid_visualisation_target_types[i]
  
  tmp_visualisation_pinball_loss_murphy_results_all_locations <- 
    subset(visualisation_pinball_loss_murphy_results_all_locations,
                                  horizon == horizon_in_q)
  # paste0(expression(paste(alpha)), " = ", tmp_visualisation_pinball_loss_murphy_results_all_locations$quantile_level)
  # tmp_visualisation_pinball_loss_murphy_results_all_locations$quantile_level_expression <- 
  #   paste0("\u03B1 = ", tmp_visualisation_pinball_loss_murphy_results_all_locations$quantile_level)  # \u03B1 is the Unicode for α
  # tmp_visualisation_pinball_loss_murphy_results_all_locations$alpha_label <- 
  #   sapply(tmp_visualisation_pinball_loss_murphy_results_all_locations$quantile_level, function(q) {
  #   bquote(alpha == .(q))
  # })
  # tmp_visualisation_pinball_loss_murphy_results_all_locations$quantile_level_expression <- 
  #   expression(tmp_visualisation_pinball_loss_murphy_results_all_locations$quantile_level)
  tmp_visualisation_pinball_loss_murphy_results_all_locations[, alpha :=  1-as.numeric(as.character(quantile_level))]
  tmp_visualisation_pinball_loss_murphy_results_all_locations[, alpha_label := paste0("alpha==", alpha)]
  
  # tmp_visualisation_pinball_loss_murphy_results_all_locations[, alpha_label := paste0("alpha==", quantile_level)]
  pinball_loss_murphy_plot_list[[i]] <- 
    ggplot(tmp_visualisation_pinball_loss_murphy_results_all_locations)+
    geom_line(aes(x = theta, y = mean_score, color = model), size = 0.5) +
    facet_wrap(alpha_label ~. , scales = "free",  labeller = label_parsed) +
    xlab(expression(paste("Threshold ", theta))) +
    ylab(expression(paste("Mean s(", alpha, ", ", theta, ")")))+
    theme_bw(base_size = 11) +
    xlim(c(0, 100000))+
    theme_light()+theme_custom()+
    theme(axis.text.x = element_text(size=18),
          axis.text.y = element_text(size=20),
          legend.position = "bottom",
          legend.key.size = unit(1.5, "cm"),
          legend.text = element_text(size = 20),  # Adjust legend text size
          legend.title = element_text(size = 20))+
    scale_color_manual("Model", values = covid_model_colours,
                       labels = corrected_covid_model_names)+
    guides(colour = guide_legend(override.aes = list(linewidth = 2)))
}
length(pinball_loss_murphy_plot_list)
# covid_facet_pinball_loss_murphy_diagram <- 
#   ggarrange(plotlist = pinball_loss_murphy_plot_list, nrow = 2,ncol = 1,
#             legend = "bottom",
#             # labels = c("1 week \n ahead", "4 week \n ahead "),
#             # font.label = list(size = 20, face = "bold"),
#             common.legend = TRUE)

covid_facet_pinball_loss_murphy_diagram <- 
  ggarrange(NULL, pinball_loss_murphy_plot_list[[1]] + theme(axis.text.x = element_text(size=16, angle = 45, hjust = 1))+theme(axis.text.y = element_text(size=16)), NULL ,
            pinball_loss_murphy_plot_list[[2]] + theme(axis.text.x = element_text(size=16, angle = 45, hjust = 1))+ theme(axis.text.y = element_text(size=16)),
            nrow = 4,ncol = 1,
            legend = "none",
            labels = c("1 week ahead", "","4 week ahead", ""),
            font.label = list(size = 18),
            heights = c(0.1, 0.75, 0.1,0.75))
covid_facet_pinball_loss_murphy_diagram
# covid_facet_pinball_loss_murphy_diagram <- ggarrange(NULL,covid_facet_pinball_loss_murphy_diagram,
#                                                     nrow = 2, ncol = 1, hjust = -0.5,
#                                                     labels = c("C.1"), heights = c(0.06, 1.0))
covid_facet_pinball_loss_murphy_diagram <- ggarrange(NULL,NULL,
                                                             NULL, covid_facet_pinball_loss_murphy_diagram,
                                                             labels = c("","C.1: User-specific Murphy diagrams", "", ""),
                                                             font.label = list(size = 20, face = "bold"),
                                                     hjust = -0.25,
                                                             widths = c(0.1, 1.0),
                                                             heights = c(0.07, 1.0), nrow = 2,
                                                             ncol = 2)
covid_facet_pinball_loss_murphy_diagram












#Higher C/L ratios
higher_cl_ratios_visualisation_pinball_loss_murphy_results_all_locations <- 
  subset(pinball_loss_murphy_results_all_locations,
         horizon %in% covid_visualisation_horizons)
higher_cl_ratios_visualisation_pinball_loss_murphy_results_all_locations <- 
  subset(higher_cl_ratios_visualisation_pinball_loss_murphy_results_all_locations,
         quantile_level %in% higher_cl_ratios)

higher_cl_ratios_visualisation_pinball_loss_murphy_results_all_locations <- 
  merge(higher_cl_ratios_visualisation_pinball_loss_murphy_results_all_locations,
        covid_target_types_horizons,
        by = "horizon")
higher_cl_ratios_pinball_loss_murphy_plot_list <- list()
for(i in 1:length(covid_visualisation_horizons)){
  horizon_in_q <-covid_visualisation_horizons[i]
  target_type_in_q <-covid_visualisation_target_types[i]
  
  tmp_higher_cl_ratios_visualisation_pinball_loss_murphy_results_all_locations <- 
    subset(higher_cl_ratios_visualisation_pinball_loss_murphy_results_all_locations,
           horizon == horizon_in_q)
  # paste0(expression(paste(alpha)), " = ", tmp_higher_cl_ratios_visualisation_pinball_loss_murphy_results_all_locations$quantile_level)
  # tmp_higher_cl_ratios_visualisation_pinball_loss_murphy_results_all_locations$quantile_level_expression <- 
  #   paste0("\u03B1 = ", tmp_higher_cl_ratios_visualisation_pinball_loss_murphy_results_all_locations$quantile_level)  # \u03B1 is the Unicode for α
  # tmp_higher_cl_ratios_visualisation_pinball_loss_murphy_results_all_locations$alpha_label <- 
  #   sapply(tmp_higher_cl_ratios_visualisation_pinball_loss_murphy_results_all_locations$quantile_level, function(q) {
  #   bquote(alpha == .(q))
  # })
  # tmp_higher_cl_ratios_visualisation_pinball_loss_murphy_results_all_locations$quantile_level_expression <- 
  #   expression(tmp_higher_cl_ratios_visualisation_pinball_loss_murphy_results_all_locations$quantile_level)
  tmp_higher_cl_ratios_visualisation_pinball_loss_murphy_results_all_locations[, alpha_label := paste0("alpha==", quantile_level)]
  higher_cl_ratios_pinball_loss_murphy_plot_list[[i]] <- 
    ggplot(tmp_higher_cl_ratios_visualisation_pinball_loss_murphy_results_all_locations)+
    geom_line(aes(x = theta, y = mean_score, color = model), size = 0.5) +
    facet_wrap(alpha_label ~. , scales = "free",  labeller = label_parsed) +
    xlab(expression(paste("Threshold ", theta))) +
    ylab(expression(paste("Mean s(", alpha, ", ", theta, ")")))+
    theme_bw(base_size = 11) +
    xlim(c(0, 100000))+
    theme_light()+theme_custom()+
    theme(axis.text.x = element_text(size=18),
          axis.text.y = element_text(size=20),
          legend.position = "bottom",
          legend.key.size = unit(1.5, "cm"),
          legend.text = element_text(size = 20),  # Adjust legend text size
          legend.title = element_text(size = 20))+
    scale_color_manual("Model", values = covid_model_colours,
                       labels = corrected_covid_model_names)+
    guides(colour = guide_legend(override.aes = list(linewidth = 2)))
}
length(pinball_loss_murphy_plot_list)
# covid_facet_pinball_loss_murphy_diagram <- 
#   ggarrange(plotlist = pinball_loss_murphy_plot_list, nrow = 2,ncol = 1,
#             legend = "bottom",
#             # labels = c("1 week \n ahead", "4 week \n ahead "),
#             # font.label = list(size = 20, face = "bold"),
#             common.legend = TRUE)

higher_cl_ratios_covid_facet_pinball_loss_murphy_diagram <- 
  ggarrange(NULL, higher_cl_ratios_pinball_loss_murphy_plot_list[[1]] + theme(axis.text.x = element_text(size=16, angle = 45, hjust = 1)), NULL ,
            higher_cl_ratios_pinball_loss_murphy_plot_list[[2]] + theme(axis.text.x = element_text(size=16, angle = 45, hjust = 1)),
            nrow = 4,ncol = 1,
            legend = "none",
            labels = c("1 week ahead", "","4 week ahead", ""),
            font.label = list(size = 18),
            heights = c(0.1, 0.75, 0.1,0.75))
higher_cl_ratios_covid_facet_pinball_loss_murphy_diagram
length(which(state_level_covid_forecasts_dt$observed >= 100000))/nrow(state_level_covid_forecasts_dt)
# covid_facet_pinball_loss_murphy_diagram <- ggarrange(NULL,covid_facet_pinball_loss_murphy_diagram,
#                                                     nrow = 2, ncol = 1, hjust = -0.5,
#                                                     labels = c("C.1"), heights = c(0.06, 1.0))
# covid_facet_pinball_loss_murphy_diagram <- ggarrange(NULL,NULL,
#                                                      NULL, covid_facet_pinball_loss_murphy_diagram,
#                                                      labels = c("","C.1: User-specific Murphy diagrams", "", ""),
#                                                      font.label = list(size = 20, face = "bold"),
#                                                      widths = c(0.1, 1.0),
#                                                      heights = c(0.1, 1.0), nrow = 2,
#                                                      ncol = 2)
# covid_facet_pinball_loss_murphy_diagram


#This is plot for across all time points

# covid_facet_pinball_loss_top_row_murphy_diagram <- ggplot(visualisation_pinball_loss_murphy_results_all_locations)+
#   geom_line(aes(x = theta, y = mean_score, color = model), size = 0.5) +
#   facet_wrap(target_type ~quantile_level, scales = "free") +
#   xlab(expression(paste("Threshold ", theta))) +
#   ylab("Mean elementary score") +
#   theme_bw(base_size = 11) +
#   xlim(c(0, 200000))+
#   theme_light()+theme_custom()+
#   theme(axis.text.x = element_text(size=18),
#         axis.text.y = element_text(size=20),
#         legend.position = "bottom",
#         legend.key.size = unit(1.5, "cm"),
#         legend.text = element_text(size = 20),  # Adjust legend text size
#         legend.title = element_text(size = 20))+
#   scale_color_manual("Model", values = covid_model_colours,
#                     labels = corrected_covid_model_names)
# covid_facet_pinball_loss_murphy_diagram
# pinball_loss_murphy_plots_all_locations <- pinball_loss_decomposition_results[[4]]
# pinball_loss_murphy_plots_all_locations[[1]] + xlim(c(0, 1e5))


#Take decision based on model with best historical "value" of model 
#Take decision based on model with best trailing window Pinball loss (Murphy scores)

#In reality, generally user will make decision from ensemble with prob = C/L alpha
require(rlang)
state_covid_scores_pe_12_wk_rollings_window
tmp <- subset(state_covid_forecasts, target_end_date >= "2021-01-01")
tmp <- subset(tmp, target_end_date <= "2021-06-26")
unique(tmp$target_end_date)
gc()
pe_args <- list(m.min = 2,
                m.max = 5,
                t.min = 1,
                t.max = 20)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# state_covid_scores_pe_4_wk_rollings_window_new <- 
#   scores_and_permutation_entropy_trailing_window_over_time(state_covid_forecasts2, #data
#                                                            window_length = 4,
#                                                            column_name = "observed",
#                                                            pe_args = pe_args,
#                                                            forecast_representation = "quantile")
# state_covid_scores_pe_4_wk_rollings_window_new

state_covid_scores_pe_26_wk_rollings_window <- 
  scores_and_permutation_entropy_trailing_window_over_time(subsetted_state_covid_forecasts_dt, #data
                                                           window_length = 26,
                                                           column_name = "observed",
                                                           pe_args = pe_args,
                                                           forecast_representation = "quantile")
state_covid_scores_pe_26_wk_rollings_window

saveRDS(state_covid_scores_pe_26_wk_rollings_window_new,
        file = file.path(forecasting.covid_forecasts.out.dir,
                         "state_covid_scores_pe_26_wk_rollings_window_16_03_24.RDS"))

saveRDS(state_covid_scores_pe_26_wk_rollings_window,
        file = file.path(forecasting.covid_forecasts.out.dir,
                         "state_covid_scores_pe_26_wk_rollings_window_19_03_24.RDS"))


subsetted_state_covid_forecasts_dt <- readRDS(file.path(forecasting.covid_forecasts.data.dir,
                                                        "subsetted_state_covid_forecasts_dt.RDS"))

dim(state_covid_forecasts2)[1]-dim(subsetted_state_covid_forecasts_dt)[1]


#Estimated in window
#Scores + pointwise metrics (e.g. rsquared + coverage) ----
scores_and_pointwise_trailing_window_over_time
state_covid_scores_pointwise_12_wk_rollings_window <- 
  scores_and_pointwise_trailing_window_over_time(x = state_level_covid_forecasts_dt, #data
                                                           window_length = 12,
                                                           column_name = "observed",
                                                           forecast_representation = "quantile")
head(state_covid_scores_pointwise_12_wk_rollings_window)
summary(state_covid_scores_pointwise_12_wk_rollings_window[[3]]$r2)
saveRDS(state_covid_scores_pointwise_12_wk_rollings_window,
        file = file.path(forecasting.covid_forecasts.out.dir,
                         "state_covid_scores_pointwise_12_wk_rollings_window.RDS"))
forecasting.covid_forecasts.out.dir
state_covid_scores_pointwise_12_wk_rollings_window <- 
  readRDS(file = file.path(forecasting.covid_forecasts.out.dir,
                 "state_covid_scores_pointwise_12_wk_rollings_window.RDS"))

#Scores + predictability ----
state_covid_scores_pe_12_wk_rollings_window <- 
  scores_and_permutation_entropy_trailing_window_over_time(state_level_covid_forecasts_dt, #data
                                                           window_length = 12,
                                                         column_name = "observed",
                                                         pe_args = pe_args,
                                                         forecast_representation = "quantile")
state_covid_scores_pe_12_wk_rollings_window
saveRDS(state_covid_scores_pe_12_wk_rollings_window,
        file = file.path(forecasting.covid_forecasts.out.dir,
                         "state_covid_scores_pe_12_wk_rollings_window_19_03_24.RDS"))


state_covid_scores_pe_12_wk_rollings_window <- readRDS(file = file.path(forecasting.covid_forecasts.out.dir,
                 "state_covid_scores_pe_12_wk_rollings_window_19_03_24.RDS"))





# saveRDS(state_covid_scores_pe_12_wk_rollings_window_new,
#         file = file.path(forecasting.covid_forecasts.out.dir,
#                          "state_covid_scores_pe_12_wk_rollings_window_16_03_24.RDS"))



rm(aggregate_forecast_perfmance_12_wk_rolling_window)

#26-Week Window -----
aggregate_forecast_performance_26_wk_rolling_window <- 
  state_covid_scores_pe_26_wk_rollings_window[[1]]
unique(aggregate_forecast_performance_26_wk_rolling_window$model)
aggregate_forecast_performance_26_wk_rolling_window <- 
  subset(aggregate_forecast_performance_26_wk_rolling_window,
         compare_against == "COVIDhub-baseline")

ggplot(aggregate_forecast_performance_26_wk_rolling_window)+
  geom_line(aes(x = max_time, y = wis_scaled_relative_skill,
                color = factor(model)))+
  facet_wrap(target_type ~., )+
  theme_minimal()

state_covid_scores_pe_26_wk_rollings_window_new[[1]]

#Entropy results
location_entropy_results_26_wk_rolling_window <- 
  state_covid_scores_pe_26_wk_rollings_window[[2]]
location_entropy_results_26_wk_rolling_window
location_entropy_results_26_wk_rolling_window[, max_time:= as.Date(max_time)]
location_entropy_results_26_wk_rolling_window[, predictability:= 1-raw_perm_entropy]
location_entropy_results_26_wk_rolling_window[, scaled_total_cases:= range01(total_cases), by = "location"]
location_entropy_results_26_wk_rolling_window[, YEAR:= year(max_time)]
location_entropy_results_26_wk_rolling_window[, scaled_mean_cases:= range01(mean_cases), by = "location"]
location_entropy_results_26_wk_rolling_window[, rolling_predictability:= 1-zoo::rollmean(raw_perm_entropy, 3, align = "right", fill = NA), by = "location"]
summary(location_entropy_results_26_wk_rolling_window[, cor(scaled_total_cases, predictability), by = c("location", "YEAR")]$V1)
summary(location_entropy_results_26_wk_rolling_window[, cor(mean_cases, predictability), by = "location"]$V1)
location_entropy_results_26_wk_rolling_window[, cor(mean_cases, predictability)]

ggplot(location_entropy_results_26_wk_rolling_window)+
  geom_point(aes(x = rolling_predictability, y = log(mean_cases)), color = "orange")+
  theme_bw()+
  facet_wrap(location ~., scales = "free_y")

#Plotting total cases (against predictability)
ggplot(location_entropy_results_26_wk_rolling_window)+
  geom_line(aes(x = max_time, y = rolling_predictability))+
  geom_line(aes(x = max_time, y = scaled_total_cases), color = "orange")+
  facet_wrap(location ~., )+theme_bw()

#Plotting cases vs median preds (for eyeballing)
tmp_state_covid_forecasts <- subset(state_covid_forecasts, quantile_level == 0.5)
tmp_state_covid_forecasts[, model_factor:= factor(model)]
ggplot(tmp_state_covid_forecasts)+
  geom_point(aes(x = target_end_date, y = observed), color = "black")+
  geom_line(aes(x = target_end_date, y = predicted, color = model_factor),
            alpha = 0.5)+
  facet_wrap(location ~., scales = "free_y")+theme_bw()+theme(legend.position = "bottom")

#Can visualise predictability across space + time, summarise average across space

location_forecast_performance_26_wk_rolling_window <- 
  state_covid_scores_pe_26_wk_rollings_window[[3]]

#Keeping comparison against baseline. Can inspect others later!
location_forecast_performance_26_wk_rolling_window <- 
  subset(location_forecast_performance_26_wk_rolling_window, 
         compare_against = "COVIDhub-baseline")
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
location_forecast_performance_26_wk_rolling_window
# location_forecast_performance_26_wk_rolling_window[, max_time:= as.Date(max_time)]
# location_forecast_performance_26_wk_rolling_window[, min_time:= as.Date(min_time)]

location_forecast_performance_26_wk_rolling_window
#Merge in predictability results
#Comparing forecasts with the same period in which predictability was assessed

location_forecast_performance_26_wk_rolling_window <- 
  merge(location_forecast_performance_26_wk_rolling_window,
        location_entropy_results_26_wk_rolling_window,
        by = c("location", "max_time", "min_time"))
head(location_forecast_performance_26_wk_rolling_window)
plot(location_forecast_performance_26_wk_rolling_window[which(!is.na(rolling_predictability)), cor(wis_scaled_relative_skill, rolling_predictability), 
                                                        by = c("model", "target_type")]$V1)
location_forecast_performance_26_wk_rolling_window
location_forecast_performance_26_wk_rolling_window[, cor(wis_relative_skill, predictability), 
                                                   by = c("model", "location", "target_type")]
location_forecast_performance_26_wk_rolling_window[, WORSE_RELATIVE:= as.factor(ifelse(wis_scaled_relative_skill > 1,
                                                                                       1, 0))]
location_forecast_performance_26_wk_rolling_window[, model_factor:= factor(model)]
ggplot(location_forecast_performance_26_wk_rolling_window) + 
  geom_boxplot(aes(x = WORSE_RELATIVE, y = predictability, color = model_factor))+
  facet_wrap(location ~., )+
  theme(legend.position = "bottom")
mean_wis_by_model <- location_forecast_performance_26_wk_rolling_window[, list(MEAN_WIS = mean(wis_scaled_relative_skill, na.rm = TRUE)), by = "model"]
mean_wis_by_model[order(MEAN_WIS, decreasing = FALSE)]
summary(location_forecast_performance_26_wk_rolling_window[, cor(wis_scaled_relative_skill, scaled_total_cases),
                                                   by = c("model", "location", "target_type")]$V1)
summary(location_forecast_performance_26_wk_rolling_window[, cor(wis_scaled_relative_skill, predictability),
                                                           by = c("location", "target_type")]$V1)
head(location_forecast_performance_26_wk_rolling_window)

gc()
dev.off()

tmp <- location_forecast_performance_26_wk_rolling_window[which(wis_scaled_relative_skill <= 20)]
tmp <- subset(tmp, target_type == "1 wk ahead inc case")
ggplot(tmp)+
  geom_line(aes(x = max_time, y = predictability), color = "purple4")+
  geom_line(aes(x = max_time, y = wis_scaled_relative_skill, color = model))+
  facet_wrap(location ~ target_type, )+theme_minimal()+
  theme(legend.position = "bottom")


#12-Week results ----
aggregate_forecast_performance_12_wk_rolling_window <- 
  state_covid_scores_pe_12_wk_rollings_window[[1]]
aggregate_forecast_performance_12_wk_rolling_window
aggregate_forecast_performance_12_wk_rolling_window
aggregate_forecast_performance_12_wk_rolling_window <- 
  subset(aggregate_forecast_performance_12_wk_rolling_window,
         compare_against == "COVIDhub-baseline")
aggregate_forecast_performance_12_wk_rolling_window$model_factor <- 
  factor(aggregate_forecast_performance_12_wk_rolling_window$model,
         levels = covid_model_levels)
visualisation_aggregate_forecast_performance_12_wk_rolling_window <- 
  subset(aggregate_forecast_performance_12_wk_rolling_window,
         target_type %in% covid_visualisation_target_types)
visualisation_aggregate_forecast_performance_12_wk_rolling_window

trailing_window_wis_covid_plot <- 
  ggplot(visualisation_aggregate_forecast_performance_12_wk_rolling_window)+
  geom_line(aes(x = max_time, y = wis_scaled_relative_skill,
                color = model_factor),
            linewidth = 1.3)+
  facet_wrap(target_type ~., 
             ncol = 2)+
  # geom_hline(aes(yintercept = 1),
  #            linewidth = 1.4,
  #            linetype = "dashed")+
  theme_light()+theme_custom()+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.position = "bottom",
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 20),  # Adjust legend text size
        legend.title = element_text(size = 20))+
  labs(y = "rWIS", x = "Date" )+
  scale_color_manual("Model", values = covid_model_colours,
                    labels = corrected_covid_model_names)
trailing_window_wis_covid_plot

#Entropy results
state_covid_scores_pe_12_wk_rollings_window[[2]]
location_entropy_results_12_wk_rolling_window <- 
  state_covid_scores_pe_12_wk_rollings_window[[2]]
location_entropy_results_12_wk_rolling_window
location_entropy_results_12_wk_rolling_window[, max_time:= as.Date(max_time)]
location_entropy_results_12_wk_rolling_window[, predictability:= 1-raw_perm_entropy]
location_entropy_results_12_wk_rolling_window[, scaled_total_cases:= range01(total_cases), by = "location"]
location_entropy_results_12_wk_rolling_window[, YEAR:= year(max_time)]
location_entropy_results_12_wk_rolling_window[, scaled_mean_cases:= range01(mean_cases), by = "location"]
location_entropy_results_12_wk_rolling_window[, scaled_mean_cases2:= scale(mean_cases), by = "location"]
setkeyv(location_entropy_results_12_wk_rolling_window, c("location", "max_time"))
location_entropy_results_12_wk_rolling_window[, rolling_predictability:= 1-zoo::rollmean(raw_perm_entropy, 3, align = "right", fill = NA), by = "location"]
summary(location_entropy_results_12_wk_rolling_window[, cor(scaled_total_cases, predictability), by = c("location", "YEAR")]$V1)
summary(location_entropy_results_12_wk_rolling_window[, cor(mean_cases, predictability), by = "location"]$V1)
location_entropy_results_12_wk_rolling_window[, cor(mean_cases, predictability, method = "spearman")]
location_entropy_results_12_wk_rolling_window[, WEEK:= week(max_time)]
location_entropy_results_12_wk_rolling_window[, YEAR:= year(max_time)]
location_entropy_results_12_wk_rolling_window[, location_factor:= factor(location)]
location_entropy_results_12_wk_rolling_window
setkeyv(location_entropy_results_12_wk_rolling_window, c("location", "max_time"))

tmp_rt_phases <- data.table(copy(rt_phases))

tmp_rt_phases[, date:= NULL]
tmp_rt_phases[, sum:= NULL]
tmp_rt_phases
setnames(tmp_rt_phases, c("median_med",
                          "lower_90_med",
                          "upper_90_med",
                          "phase"),
         c("median_med_Rt",
           "lower_90_med_Rt",
           "upper_90_med_Rt",
           "phase_Rt"))
tmp_rt_phases

  
rolling_Rt_results <- rolling_window_rt(as.data.table(rt_phases), "median_med")
rolling_Rt_results
merge(location_entropy_results_12_wk_rolling_window,
      rolling_Rt_results,
      by = c("location", "max_time", "min_time"))
location_entropy_results_12_wk_rolling_window <- merge(location_entropy_results_12_wk_rolling_window,
      rolling_Rt_results,
      by = c("location", "max_time", "min_time"))
location_entropy_results_12_wk_rolling_window[, cor.test(rolling_median_Rt,
                                                         predictability)]
location_entropy_results_12_wk_rolling_window[, cor.test(rolling_mean_Rt,
                                                         predictability)]

state_level_covid_forecasts_dt[, scaled_cases:= scale(observed), by = "location" ]
rolling_scaled_cases_results <- rolling_window_cases(as.data.table(state_level_covid_forecasts_dt), "scaled_cases")
location_entropy_results_12_wk_rolling_window <- 
  merge(location_entropy_results_12_wk_rolling_window,
      rolling_scaled_cases_results,
      by = c("location", "max_time", "min_time"))
location_entropy_results_12_wk_rolling_window

location_entropy_results_12_wk_rolling_window[, cor.test(rolling_median_scaled_cases,
                                                         predictability)]

# location_entropy_results_12_wk_rolling_window <- merge(location_entropy_results_12_wk_rolling_window,
#                                                        rolling_Rt_results,
#                                                        by = c("location", "max_time", "min_time"))

location_entropy_results_12_wk_rolling_window <- merge(location_entropy_results_12_wk_rolling_window,
      tmp_rt_phases,
      by.x = c("location", "max_time"),
      by.y = c("location", "target_end_date"))

location_entropy_results_12_wk_rolling_window[, phase_Rt_factor:= factor(phase_Rt)]
location_entropy_results_12_wk_rolling_window
setkeyv(location_entropy_results_12_wk_rolling_window, c("location", "max_time"))
location_entropy_results_12_wk_rolling_window[, RSI_RT:= RSI(median_med_Rt, 3), by = "location"] 
location_entropy_results_12_wk_rolling_window[, RSI_RT_12:= RSI(median_med_Rt, 12), by = "location"] 
location_entropy_results_12_wk_rolling_window[which(predictability <= 0.3), low_predictability:= TRUE]
location_entropy_results_12_wk_rolling_window[which(predictability > 0.3), low_predictability:= FALSE]
location_entropy_results_12_wk_rolling_window[, low_predictability_factor:= factor(low_predictability)]
#Greatest predictability when Rt at peak.
ggplot(location_entropy_results_12_wk_rolling_window)+
  geom_boxplot(aes(x = phase_Rt_factor, y = predictability, fill = phase_Rt_factor))+
  theme_light()+facet_wrap(location ~., )


ggplot(location_entropy_results_12_wk_rolling_window)+
  geom_boxplot(aes(x = phase_Rt_factor, y = predictability, fill = phase_Rt_factor))+
  theme_light()

ggplot(location_entropy_results_12_wk_rolling_window)+
  geom_tile(aes(x = max_time, y = location, fill = predictability))+
  scale_fill_viridis_c(option = "magma")+
  theme_light()

ggplot(location_entropy_results_12_wk_rolling_window)+
  geom_boxplot(aes(x = low_predictability_factor, 
                   y = RSI_RT))+
  theme_light()+facet_wrap(location ~., )

ggplot(location_entropy_results_12_wk_rolling_window)+
  geom_boxplot(aes(x = low_predictability_factor, 
                   y = RSI_RT))+
  theme_light()


# tmp_location_entropy_results_12_wk_rolling_window <-
#   subset(location_entropy_results_12_wk_rolling_window,
#          YEAR == 2021)
# gam_predictability_cases_fit_1 <- 
#   gam(rolling_predictability ~ s(mean_cases, by = location) + location_factor,
#     family = gaussian, data = tmp_location_entropy_results_12_wk_rolling_window)
# summary(gam_predictability_cases_fit_1)
# gam_activity_dlnm_temp_model_fit <- gam(TEMP_FILLED  ~ activity_basis + 
#                             s(MONTH_FACTOR, bs = "re") + 
#                             s(HOUR, bs = "tp"), 
#                           family = gaussian,
#                           data = dlnm_bh_activity_temp_dt)
# ggplot(tmp_location_entropy_results_12_wk_rolling_window)+
#   geom_point(aes(y = rolling_predictability, x = WEEK, color = "Predictability"))+
#   geom_point(aes(y = scaled_mean_cases, x = WEEK, color = "Cases"))+
#   theme_light()+
#   facet_wrap(location ~. , )

#No clear relationship between cases and predictability
setkeyv(location_entropy_results_12_wk_rolling_window, c("location", "max_time"))
summary(location_entropy_results_12_wk_rolling_window[, cor(predictability, median_med_Rt),
                                              by = "location"]$V1)
ggplot(location_entropy_results_12_wk_rolling_window)+
  geom_point(aes(x = rolling_predictability, y = log(mean_cases)), color = "orange")+
  theme_bw()+
  facet_wrap(location ~., scales = "free_y")
#predictability is magnitude-free
ggplot(location_entropy_results_12_wk_rolling_window)+
  geom_point(aes(x = median_med_Rt, y = predictability, scales = "free"))+
  facet_wrap(location ~., )+theme_light()

#Plotting Rt (alongside predictability)
ggplot(location_entropy_results_12_wk_rolling_window)+
  geom_line(aes(x = max_time, y = predictability,
                color = "Predictability"))+
  geom_line(aes(x = max_time, y = median_med_Rt,
                color = "Rt"))+
  facet_wrap(location ~., )+theme_bw()
#Plotting total cases (alongside predictability)
ggplot(location_entropy_results_12_wk_rolling_window)+
  geom_line(aes(x = max_time, y = predictability,
            color = "Predictability"))+
  geom_line(aes(x = max_time, y = scaled_total_cases,
                color = "Cases"))+
  facet_wrap(location ~., )+theme_bw()

#Plotting cases vs median preds (for eyeballing)
# tmp_state_covid_forecasts <- subset(state_covid_forecasts, quantile_level == 0.5)
# tmp_state_covid_forecasts[, model_factor:= factor(model)]
# ggplot(tmp_state_covid_forecasts)+
#   geom_point(aes(x = target_end_date, y = observed), color = "black")+
#   geom_line(aes(x = target_end_date, y = predicted, color = model_factor),
#             alpha = 0.5)+
#   facet_wrap(location ~., scales = "free_y")+theme_bw()+theme(legend.position = "bottom")

#Can visualise predictability across space + time, summarise average across space

location_forecast_performance_12_wk_rolling_window <- 
  state_covid_scores_pe_12_wk_rollings_window[[3]]
#Keeping comparison against baseline. Can inspect others later!
location_forecast_performance_12_wk_rolling_window <- 
  subset(location_forecast_performance_12_wk_rolling_window, 
         compare_against = "COVIDhub-baseline")
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
location_forecast_performance_12_wk_rolling_window
# location_forecast_performance_12_wk_rolling_window[, max_time:= as.Date(max_time)]
# location_forecast_performance_12_wk_rolling_window[, min_time:= as.Date(min_time)]

location_forecast_performance_12_wk_rolling_window
#Merge in predictability results
#Comparing forecasts with the same period in which predictability was assessed

location_forecast_performance_12_wk_rolling_window <- 
  merge(location_forecast_performance_12_wk_rolling_window,
        location_entropy_results_12_wk_rolling_window,
        by = c("location", "max_time", "min_time"))

location_forecast_performance_12_wk_rolling_window
# unique(location_forecast_performance_12_wk_rolling_window$location)
# plot(location_forecast_performance_12_wk_rolling_window[which(!is.na(rolling_predictability)), cor(wis_scaled_relative_skill, rolling_predictability), 
#                                                    by = c("model", "target_type")]$V1)
# location_forecast_performance_12_wk_rolling_window
summary(location_forecast_performance_12_wk_rolling_window[, cor(wis_scaled_relative_skill, rolling_mean_Rt), 
                                                   by = c("model",  "target_type")]$V1)
location_forecast_performance_12_wk_rolling_window[, WORSE_RELATIVE:= as.factor(ifelse(wis_scaled_relative_skill > 1,
                                                                           1, 0))]
location_forecast_performance_12_wk_rolling_window[, model_factor:= factor(model)]
location_forecast_performance_12_wk_rolling_window$predictability_factor <- 
  cut(location_forecast_performance_12_wk_rolling_window$predictability, 
                                breaks = seq(0, 1, length.out = 5),  # 4 levels → 5 breakpoints
                                labels = c("Low", "Medium-Low", "Medium-High", "High"),
                                include.lowest = TRUE)
location_forecast_performance_12_wk_rolling_window
location_forecast_performance_12_wk_rolling_window[, lagged_predictability:= lag(predictability, 1),
                                                   by = c("location", "max_time", "target_type","model")]
ggplot(location_forecast_performance_12_wk_rolling_window) + 
  geom_boxplot(aes(x = WORSE_RELATIVE, y = predictability, color = model_factor))+
  facet_wrap(location ~., )+
  theme(legend.position = "bottom")

ggplot(location_forecast_performance_12_wk_rolling_window) + 
  geom_boxplot(aes(x = low_predictability_factor, 
                   y = wis_scaled_relative_skill, color = model_factor))+
  ylim(c(0, 4))+
  # facet_wrap(location ~., )+
  theme(legend.position = "bottom")
location_forecast_performance_12_wk_rolling_window


unique(single_horizon_location_forecast_performance_12_wk_rolling_window$model)
single_horizon_location_forecast_performance_12_wk_rolling_window <-
  subset(single_horizon_location_forecast_performance_12_wk_rolling_window,
         model %in% c("COVIDhub-4_week_ensemble"))
single_horizon_location_forecast_performance_12_wk_rolling_window
summary(single_horizon_location_forecast_performance_12_wk_rolling_window$wis_scaled_relative_skill)
p1 <- ggplot(single_horizon_location_forecast_performance_12_wk_rolling_window)+
  geom_raster(aes(x = max_time, 
                  y = location_name,
                  fill = wis_scaled_relative_skill))+
  theme_light()+theme_custom()+
  labs(y = "State", x = "Date" )+
  scale_fill_viridis_c(option = "magma")+
  theme(
    axis.text.y = element_text(size=18,
                               angle = 45, hjust = 1),
    axis.text.x = element_text(size=18,
                               angle = 45, hjust = 1),    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 20),  # Adjust legend text size
    legend.title = element_text(size = 20))+ 
  guides(fill = guide_colorbar(barwidth = 10,
                               title = "rWIS"))+
  theme(legend.position = "bottom")
p1
p2 <- ggplot(single_horizon_location_forecast_performance_12_wk_rolling_window)+
  geom_raster(aes(x = max_time, 
                  y = location_name,
                  fill = scaled_mean_cases))+
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
                               title = "Scaled cases"))+
  theme(legend.position = "bottom")

p3 <- ggplot(single_horizon_location_forecast_performance_12_wk_rolling_window)+
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
p3
tileplots
tileplots <- ggarrange(p2, p1, p3,
          nrow = 1)
ggsave(tileplots,
       filename = file.path(forecasting.covid_forecasts.out.dir,"tile_plots_cases_rwis_pe.pdf"),
       h = 24, w = 26)
for(i in 1:length(unique(horizon_target_types$horizon))){
  single_horizon_location_forecast_performance_12_wk_rolling_window <- 
    subset(location_forecast_performance_12_wk_rolling_window,
           target_type = horizon_target_types$target_type[i])
  tmp_plot <- ggplot(single_horizon_location_forecast_performance_12_wk_rolling_window) + 
    geom_raster(aes(x = max_time, 
                    y = location_name,
                    fill = wis_scaled_relative_skill))+
    facet_wrap(model ~., )+
    theme_light()+theme_custom()+
    labs(y = "State", x = "Date" )+
    scale_fill_continuous_divergingx(palette = "RdBu",
                                     mid = 1,
                                     rev = 1,
                                     limits = c(0.4, 4),
                                     oob=squish)+
    theme(
      axis.text.y = element_text(size=18,
                                 angle = 45, hjust = 1),
      axis.text.x = element_text(size=20),
      plot.title = element_text(size = 20, hjust = 0.5),
      legend.position = "right",
      legend.text = element_text(size = 20),  # Adjust legend text size
      legend.title = element_text(size = 20))+ 
    guides(fill = guide_colorbar(barheight = 10,
                                 title = "rWIS"))+
    theme(legend.position = "bottom")
  ggsave(tmp_plot,
         filename = file.path(forecasting.covid_forecasts.out.dir, paste0("single_horizon_", i, "_individual_location_rwis_12_wk_rolling_window.pdf")),
                              h = 24, w = 22)
}
tmp_plot

ggplot(location_forecast_performance_12_wk_rolling_window) + 
  geom_boxplot(aes(x = low_predictability_factor, 
                   y = wis_scaled_relative_skill),
               outlier.shape = NA)+
  facet_wrap(model ~., )+
  ylim(c(0, 4))+
  # facet_wrap(location ~., )+
  theme(legend.position = "bottom")

#When each model outperforms the baseline? i.e. epidemic phase
ggplot(location_forecast_performance_12_wk_rolling_window) + 
  geom_boxplot(aes(y = RSI_RT, 
                 x = WORSE_RELATIVE),
             outlier.shape = NA)+
  # geom_smooth()+
  # ylim(c(0, 4))+
  facet_wrap(model ~., scales = "free")+
  # facet_wrap(location ~., )+
  theme(legend.position = "bottom")

corr_wis_predictability_by_rt_phase_and_location <- 
  location_forecast_performance_12_wk_rolling_window[, list(CORR = cor(wis_scaled_relative_skill, predictability)),
                                                   by = c("phase_Rt_factor", "location",
                                                          "target_type", "model")]
summary(corr_wis_predictability_by_rt_phase_and_location$CORR) 
#Answer = negligible!

#Visualise how WIS depends on Rt phase -> Not really!
location_forecast_performance_12_wk_rolling_window$target_type
tmp <- subset(location_forecast_performance_12_wk_rolling_window,
              target_type == "1 wk ahead inc case")
ggplot(tmp) + 
  geom_boxplot(aes(x = predictability_factor, y = wis_scaled_relative_skill),
               outlier.shape = NA)+
  ylim(c(0.5, 2))+
  facet_wrap(model ~., )+
  theme(legend.position = "bottom")

ggplot(tmp) + 
  geom_boxplot(aes(x = phase_Rt_factor, y = predictability),
               outlier.shape = NA)+
  ylim(c(0, 1))+
  theme(legend.position = "bottom")

ggplot(tmp) + 
  geom_boxplot(aes(x = phase_Rt_factor, y = wis_relative_skill),
               outlier.shape = NA)+
  ylim(c(0.5, 2))+
  facet_wrap(model_factor ~., )+
  theme(legend.position = "bottom")

ggplot(tmp) + 
  geom_boxplot(aes(x = phase_Rt_factor, y = wis_relative_skill),
               outlier.shape = NA)+
  ylim(c(0, 2))+
  facet_wrap(model_factor ~., )+
  theme(legend.position = "bottom")

ggplot(tmp) + 
  geom_boxplot(aes(x = WORSE_RELATIVE, color = model_factor))+
  facet_wrap(phase_Rt_factor ~., )+
  theme(legend.position = "bottom")

ggplot(tmp) + 
  geom_boxplot(aes(x = WORSE_RELATIVE, y = predictability, color = model_factor))+
  facet_wrap(phase_Rt_factor ~., )+
  theme(legend.position = "bottom")


ggplot(location_forecast_performance_12_wk_rolling_window) + 
  geom_boxplot(aes(x = WORSE_RELATIVE, y = predictability, color = model_factor))+
  facet_wrap(location ~., )+
  theme(legend.position = "bottom")


mean_wis_by_model <- location_forecast_performance_12_wk_rolling_window[, list(MEAN_WIS = mean(wis_scaled_relative_skill, na.rm = TRUE)), by = "model"]
mean_wis_by_model[order(MEAN_WIS, decreasing = FALSE)]
location_forecast_performance_12_wk_rolling_window[, cor(wis_scaled_relative_skill, scaled_total_cases),
                                                   by = c("model", "location", "target_type")]
summary(location_forecast_performance_12_wk_rolling_window[, cor(wis_scaled_relative_skill, predictability),
                                                           by = c("location", "target_type")]$V1)
head(location_forecast_performance_12_wk_rolling_window)

gc()
dev.off()
unique(location_forecast_performance_12_wk_rolling_window$location)
tmp <- location_forecast_performance_12_wk_rolling_window[which(wis_scaled_relative_skill <= 5)]
tmp <- tmp[which(target_type == "1 wk ahead inc case")]

location_forecast_performance_12_wk_rolling_window[which(wis_scaled_relative_skill > 20)]
ggplot(location_forecast_performance_12_wk_rolling_window)+
  geom_poij(aes(x = max_time, y = predictability,
                color = "Predictability"))+
  geom_line(aes(x = max_time, y = scaled_total_cases,
                color = "Cases"))+
  facet_wrap(location ~., )+theme_bw()

tmp$model <- factor(tmp$model, levels = covid_model_levels)
tmp <- tmp[which(target_type == "1 wk ahead inc case")]
tmp2 <- unique(subset(tmp, select = c("max_time","location",
                      "predictability", "scaled_mean_cases")))
ggplot(tmp)+
  geom_line(aes(x = max_time, y = wis_scaled_relative_skill, color = model))+
  geom_point(data = tmp2, aes(x = max_time, y = predictability), fill = "pink")+
  geom_point(data = tmp2, aes(x = max_time, y = scaled_mean_cases), fill = "black")+
  
  facet_wrap(location ~ ., 
             scales = "free")+theme_bw()+
  theme_minimal()+
  theme(legend.position = "bottom")+
  scale_color_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)

ggplot(tmp)+
  geom_line(aes(x = max_time, y = predictability), color = "purple4")+
  # geom_line(aes(x = max_time, y = wis_scaled_relative_skill, color = model))+
  facet_wrap(location ~ ., 
             scales = "free")+theme_bw()+
  theme_minimal()+
  theme(legend.position = "bottom")

ggplot(tmp) + geom_point(aes(x = predictability, y = wis_scaled_relative_skill))+
  facet_wrap(model ~., 
             scales = "free_y")+
  theme_light()


# location_forecast_performance_12_wk_rolling_window <- location_forecast_performance_12_wk_rolling_window %>%
#   mutate(model=as.factor(model))
# location_forecast_performance_12_wk_rolling_window$model <-
#   relevel(location_forecast_performance_12_wk_rolling_window$model,
#           ref = "COVIDhub-baseline")
# location_forecast_performance_12_wk_rolling_window
# tmp_baseline_scores <- subset(location_forecast_performance_12_wk_rolling_window,
#               model == "COVIDhub-baseline")
# tmp_baseline_scores
# tmp_baseline_scores <- subset(tmp_baseline_scores, select = c("location", "max_time"))
require(checkmate)

new_scores <- function(scores, metrics, ...) {
  scores <- as.data.table(scores, ...)
  class(scores) <- c("scores", class(scores))
  setattr(scores, "metrics", metrics)
  return(scores[])
}
as_scores <- function(scores, metrics) {
  assert_data_frame(scores)
  present_metrics <- metrics[metrics %in% colnames(scores)]
  scores <- new_scores(scores, present_metrics)
  assert_scores(scores)
  return(scores[])
}
assert_scores <- function(scores) {
  assert_data_frame(scores)
  assert_class(scores, "scores")
  # error if no metrics exists +
  # throw warning if any of the metrics is not in the data
  get_metrics(scores, error = TRUE)
  return(invisible(NULL))
}

#' @method `[` scores
#' @importFrom data.table setattr
#' @export
`[.scores` <- function(x, ...) {
  ret <- NextMethod()
  if (is.data.table(ret)) {
    setattr(ret, "metrics", attr(x, "metrics"))
  } else if (is.data.frame(ret)) {
    attr(ret, "metrics") <- attr(x, "metrics")
  }
  return(ret)
}

max_times <- unique(location_entropy_results_26_wk_rolling_window$max_time)
min_times <- unique(location_entropy_results_26_wk_rolling_window$min_time)
locations_scores_unsummarised <- state_covid_scores_pe_26_wk_rollings_window[[4]]
tmp <- get_metrics(tmp_all_locations_summary)
locations_scores_unsummarised <- as_scores(locations_scores_unsummarised,
                                           metrics = tmp)
locations_scores_unsummarised
location_level_summary_scores <- NULL
# location_level_pairwise_scores <- NULL
for(i in 1:length(max_times)){
  print(paste0("Time:", i))
  max_time_in_q <- max_times[i]
  min_time_in_q <- min_times[i]
  locations_scores_unsummarised_at_time_in_q <- 
    subset(locations_scores_unsummarised, target_end_date >= min_time_in_q & target_end_date <= max_time_in_q)
  # locations_scores_unsummarised_at_time_in_q <- data.table(locations_scores_unsummarised_at_time_in_q)
  #Summary scores
  tmp_location_level_summary_scores <- 
    locations_scores_unsummarised_at_time_in_q %>% 
    summarise_scores(by = c("model", "target_type", "location"))
  tmp_location_level_summary_scores$min_time <- min_time_in_q
  tmp_location_level_summary_scores$max_time <- max_time_in_q
  
  location_level_summary_scores <- rbind(location_level_summary_scores,
                                             tmp_location_level_summary_scores)
  #Pairwise Scores
  # tmp_location_level_pairwise_scores <- locations_scores_unsummarised_at_time_in_q %>%
  #   get_pairwise_comparisons(by = c("target_type", "location"),
  #                            compare = "model",
  #                            metric = "wis",
  #                            baseline = "COVIDhub-baseline")
  # location_level_pairwise_scores <- rbind(location_level_pairwise_scores,
  #                                             tmp_location_level_pairwise_scores)
}
baseline_location_level_summary_scores <- 
  subset(location_level_summary_scores,
         model == "COVIDhub-baseline")
baseline_location_level_summary_scores <- 
  subset(baseline_location_level_summary_scores,
         select = c("wis", "location", "min_time", "max_time"))
setnames(baseline_location_level_summary_scores,
         "wis", "baseline_wis")
location_level_summary_scores$baseline_wis <- rep(baseline_location_level_summary_scores$baseline_wis,
                                                  length(unique(location_level_summary_scores$model)))
location_level_summary_scores[, rwis:= wis/baseline_wis]
location_level_summary_scores[which(rwis < 1)]
location_level_summary_scores[which(rwis > 1)]

location_level_summary_scores <- 
  merge(location_level_summary_scores,
        location_entropy_results_26_wk_rolling_window,
        by = c("location", "max_time", "min_time"))
head(location_level_summary_scores)
plot(location_level_summary_scores[which(!is.na(rolling_predictability)), cor(rwis, rolling_predictability), 
                                                        by = c("model", "target_type")]$V1)
location_level_summary_scores
summary(location_level_summary_scores[, cor(rwis, predictability), 
                                                           by = c("model", "location", "target_type")]$V1)


location_level_summary_scores[which(rwis > 10)]
ggplot(location_level_summary_scores)+
  geom_line(aes(x = max_time, y = predictability), color = "purple4")+
  geom_line(aes(x = max_time, y = rwis, color = model))+
  coord_cartesian(ylim = c(0, 5))+
  facet_wrap(location ~ ., 
             scales = "free")+theme_bw()+
  theme_minimal()+
  theme(legend.position = "bottom")


location_level_summary_scores
location_level_summary_scores[, WORSE_RELATIVE:= as.factor(ifelse(rwis > 1, 1, 0))]
location_level_summary_scores[, model_factor:= factor(model)]
location_level_summary_scores[which(model == "COVIDhub-baseline"), WORSE_RELATIVE:= NA]
ggplot(location_level_summary_scores) + 
  geom_boxplot(aes(x = WORSE_RELATIVE, y = predictability, color = model_factor))+
  # facet_wrap(location ~., )+
  theme(legend.position = "bottom")



head(trailing_window_pinball_loss_by_forecast_horizon_all_locations)
head(location_entropy_results_12_wk_rolling_window)












#2) REV  ----

covid_all_models_rev_upper_ten_percent_results <- 
  all_models_rev_function(event_threshold = 0.9, #Fixed event threshold -> Can replace with upper observed quantile
                          event_threshold_type = "fixed_quantile",
                          forecast_representation = "quantile",
                          all_models_data = state_level_covid_forecasts_dt,
                          baseline_model = FALSE,
                          baseline_model_name = "COVIDhub-baseline", 
                          model_names = covid_model_names)

trailing_window_covid_all_models_rev_upper_ten_percent_results <- 
  trailing_window_all_models_rev_function(event_threshold = 0.9, #Fixed event threshold -> Can replace with upper observed quantile
                                          event_threshold_type = "fixed_quantile",
                                          forecast_representation = "quantile",
                                          all_models_data = state_level_covid_forecasts_dt,
                                          baseline_model = FALSE,
                                          baseline_model_name = "COVIDhub-baseline", 
                                          model_names = covid_model_names,
                                          window_length = 12)

trailing_window_covid_all_models_rev_upper_ten_percent_results_versus_baseline <- 
  trailing_window_all_models_rev_function(event_threshold = 0.9, #Fixed event threshold -> Can replace with upper observed quantile
                                          event_threshold_type = "fixed_quantile",
                                          forecast_representation = "quantile",
                                          all_models_data = state_level_covid_forecasts_dt,
                                          baseline_model = TRUE,
                                          baseline_model_name = "COVIDhub-baseline", 
                                          model_names = covid_model_names,
                                          window_length = 12)
trailing_window_covid_all_models_rev_upper_ten_percent_results[[3]]
trailing_window_covid_all_models_rev_upper_ten_percent_results[[1]]
trailing_window_location_specific_rev_upper_ten_percent_dt <- 
  trailing_window_covid_all_models_rev_upper_ten_percent_results[[1]]
trailing_window_location_specific_rev_upper_ten_percent_dt <- merge(trailing_window_location_specific_rev_upper_ten_percent_dt,
                                                        location_entropy_results_12_wk_rolling_window,
                                                        by = c("location", "max_time"))

trailing_window_location_agnostic_rev_upper_ten_percent_dt <- 
  trailing_window_covid_all_models_rev_upper_ten_percent_results[[1]]
trailing_window_location_agnostic_rev_upper_ten_percent_dt <- merge(trailing_window_location_agnostic_rev_upper_ten_percent_dt,
                                                                    location_entropy_results_12_wk_rolling_window,
                                                                    by = c("location", "max_time"))
tmp <- subset(trailing_window_location_agnostic_rev_upper_ten_percent_dt, model == "COVIDhub-4_week_ensemble")
tmp <- subset(tmp, target_type %in% covid_visualisation_target_types)
tmp <- subset(tmp, cost_loss_ratio %in% new_covid_visualisation_quantiles)

ggplot(tmp)+
  geom_boxplot(aes(x = low_predictability_factor, y = rev))+
  facet_wrap(target_type ~ cost_loss_ratio, nrow = 2)

ggplot(tmp)+
  geom_boxplot(aes(x = phase_Rt_factor, y = rev))+
  facet_wrap(target_type ~ cost_loss_ratio, nrow = 2)

merge(trailing_window_location_specific_rev_upper_ten_percent_dt,
                                                        location_entropy_results_12_wk_rolling_window,
                                                        by = c("location", "max_time"))

# trailing_window_location_specific_rev_upper_ten_percent_dt <- merge(trailing_window_location_specific_rev_upper_ten_percent_dt,
#                                                         location_entropy_results_12_wk_rolling_window,
#                                                         by = c("location", "max_time"))
# summary(trailing_window_pinball_loss_predictability_dt[, cor(raw_perm_entropy,
#                                                              dsc/(baseline_dsc + 1)), 
#                                                        by = c("model", "horizon", "location","quantile_level")]$V1)
# 








trailing_window_covid_all_models_rev_upper_ten_percent_results[[3]]
ggplot(trailing_window_covid_all_models_rev_upper_ten_percent_results[[3]])+
  geom_line(aes(x = cost_loss_ratio, y = rev, color = model))+
  theme_minimal()+
  facet_wrap(horizon ~ max_time, nrow = 4)+
  theme(legend.position = "bottom")+
  scale_color_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)
tmp <- copy(trailing_window_covid_all_models_rev_upper_ten_percent_results[[3]])
tmp <- subset(tmp, horizon %in% covid_visualisation_horizons) 
times <- unique(tmp$max_time)[seq(1, 62, by = 4)]
tmp <- subset(tmp, max_time %in% times)
ggplot(tmp)+
  geom_line(aes(x = cost_loss_ratio, y = rev, color = model))+
  theme_minimal()+
  facet_wrap(max_time ~ horizon)+
  theme(legend.position = "bottom")+
  scale_color_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)

covid_all_models_rev_upper_ten_percent_results_versus_baseline <- 
  all_models_rev_function(event_threshold = 0.9, #Fixed event threshold -> Can replace with upper observed quantile
                          event_threshold_type = "fixed_quantile",
                          forecast_representation = "quantile",
                          all_models_data = state_level_covid_forecasts_dt,
                          baseline_model = TRUE,
                          baseline_model_name = "COVIDhub-baseline", 
                          model_names = covid_model_names)
covid_all_models_rev_upper_fifteen_percent_results_versus_baseline <- 
  all_models_rev_function(event_threshold = 0.85, #Fixed event threshold -> Can replace with upper observed quantile
                          event_threshold_type = "fixed_quantile",
                          forecast_representation = "quantile",
                          all_models_data = state_level_covid_forecasts_dt,
                          baseline_model = TRUE,
                          baseline_model_name = "COVIDhub-baseline", 
                          model_names = covid_model_names)

covid_all_models_rev_upper_fifteen_percent_results <- 
  all_models_rev_function(event_threshold = 0.85, #Fixed event threshold -> Can replace with upper observed quantile
                          event_threshold_type = "fixed_quantile",
                          forecast_representation = "quantile",
                          all_models_data = state_level_covid_forecasts_dt,
                          baseline_model = FALSE,
                          baseline_model_name = "COVIDhub-baseline", 
                          model_names = covid_model_names)

covid_all_models_rev_upper_five_percent_results <- 
  all_models_rev_function(event_threshold = 0.95, #Fixed event threshold -> Can replace with upper observed quantile
                          event_threshold_type = "fixed_quantile",
                          forecast_representation = "quantile",
                          all_models_data = state_level_covid_forecasts_dt,
                          baseline_model = FALSE,
                          baseline_model_name = "COVIDhub-baseline", 
                          model_names = covid_model_names)
covid_all_models_rev_upper_five_percent_results
# tmp <- covid_all_models_rev_upper_five_percent_results[[3]]
# ggplot(tmp, aes(x = cost_loss_ratio, y = rev, color = model))+
#   geom_line() + theme_light()+facet_wrap(horizon ~., )+
#   scale_color_manual("Model", values = covid_model_colours,
#                      labels = corrected_covid_model_names)




#10% REV-----
covid_upper_ten_percent_events_rev_location_specific_results_versus_baseline_all_locations_plot <- 
  plot_rev_all_locations_location_specific_events(all_models_results = covid_all_models_rev_upper_ten_percent_results_versus_baseline,
                                                  plot_model_colours = covid_model_colours,
                                                  plot_model_names = corrected_covid_model_names,
                                                  plot_model_levels = covid_model_levels,
                                                  focus_specific_horizons = c(1, 4),
                                                  horizon_target_types = horizon_target_types)

covid_upper_ten_percent_events_rev_location_specific_results_versus_baseline_all_locations_plot



covid_upper_ten_percent_events_rev_location_agnostic_results_versus_baseline_all_locations_plot <- 
  plot_rev_all_locations_location_agnostic_events(all_models_results = covid_all_models_rev_upper_ten_percent_results_versus_baseline,
                                                  plot_model_colours = covid_model_colours,
                                                  plot_model_names = corrected_covid_model_names,
                                                  plot_model_levels = covid_model_levels,
                                                  focus_specific_horizons = c(1, 4),
                                                  horizon_target_types = horizon_target_types)
covid_upper_ten_percent_events_rev_location_agnostic_results_versus_baseline_all_locations_plot
covid_upper_ten_percent_events_rev_location_specific_results_versus_baseline_all_locations_plot
#How can I interpret positive REV even at high C/L ratios 

#10% Agnostic
covid_upper_ten_percent_events_rev_location_agnostic_results_all_locations_plot <- 
  plot_rev_all_locations_location_agnostic_events(all_models_results = covid_all_models_rev_upper_ten_percent_results,
                                                  plot_model_colours = covid_model_colours,
                                                  plot_model_names = corrected_covid_model_names,
                                                  plot_model_levels = covid_model_levels,
                                                  focus_specific_horizons = c(1, 4),
                                                  horizon_target_types = horizon_target_types)
covid_upper_ten_percent_events_rev_location_agnostic_results_all_locations_plot

covid_upper_ten_percent_events_rev_location_specific_results_all_locations_plot <- 
  plot_rev_all_locations_location_specific_events(all_models_results = covid_all_models_rev_upper_ten_percent_results,
                                                  plot_model_colours = covid_model_colours,
                                                  plot_model_names = corrected_covid_model_names,
                                                  plot_model_levels = covid_model_levels,
                                                  focus_specific_horizons = c(1, 4),
                                                  horizon_target_types = horizon_target_types)
covid_upper_ten_percent_events_rev_location_specific_results_all_locations_plot

covid_upper_ten_percent_events_rev_location_agnostic_results_all_locations_plot <- 
  plot_rev_all_locations_location_agnostic_events(all_models_results = covid_all_models_rev_upper_ten_percent_results,
                                                  plot_model_colours = covid_model_colours,
                                                  plot_model_names = corrected_covid_model_names,
                                                  plot_model_levels = covid_model_levels,
                                                  focus_specific_horizons = c(1, 4),
                                                  horizon_target_types = horizon_target_types)
covid_upper_ten_percent_events_rev_location_agnostic_results_all_locations_plot


#15% REV ----
covid_upper_fifteen_percent_events_rev_location_specific_results_versus_baseline_all_locations_plot <- 
  plot_rev_all_locations_location_specific_events(all_models_results = covid_all_models_rev_upper_fifteen_percent_results_versus_baseline,
                                                  plot_model_colours = covid_model_colours,
                                                  plot_model_names = corrected_covid_model_names,
                                                  plot_model_levels = covid_model_levels,
                                                  focus_specific_horizons = c(1, 4),
                                                  horizon_target_types = horizon_target_types)
covid_upper_fifteen_percent_events_rev_location_specific_results_versus_baseline_all_locations_plot



covid_upper_fifteen_percent_events_rev_location_agnostic_results_versus_baseline_all_locations_plot <- 
  plot_rev_all_locations_location_agnostic_events(all_models_results = covid_all_models_rev_upper_fifteen_percent_results_versus_baseline,
                                                  plot_model_colours = covid_model_colours,
                                                  plot_model_names = corrected_covid_model_names,
                                                  plot_model_levels = covid_model_levels,
                                                  focus_specific_horizons = c(1, 4),
                                                  horizon_target_types = horizon_target_types)
covid_upper_fifteen_percent_events_rev_location_agnostic_results_versus_baseline_all_locations_plot
#How can I interpret positive REV even at high C/L ratios 

#10% Agnostic
covid_upper_fifteen_percent_events_rev_location_agnostic_results_all_locations_plot <- 
  plot_rev_all_locations_location_agnostic_events(all_models_results = covid_all_models_rev_upper_fifteen_percent_results,
                                                  plot_model_colours = covid_model_colours,
                                                  plot_model_names = corrected_covid_model_names,
                                                  plot_model_levels = covid_model_levels,
                                                  focus_specific_horizons = c(1, 4),
                                                  horizon_target_types = horizon_target_types)
covid_upper_fifteen_percent_events_rev_location_agnostic_results_all_locations_plot

covid_upper_fifteen_percent_events_rev_location_specific_results_all_locations_plot <- 
  plot_rev_all_locations_location_specific_events(all_models_results = covid_all_models_rev_upper_fifteen_percent_results,
                                                  plot_model_colours = covid_model_colours,
                                                  plot_model_names = corrected_covid_model_names,
                                                  plot_model_levels = covid_model_levels,
                                                  focus_specific_horizons = c(1, 4),
                                                  horizon_target_types = horizon_target_types)
covid_upper_fifteen_percent_events_rev_location_specific_results_all_locations_plot

covid_upper_fifteen_percent_events_rev_location_agnostic_results_all_locations_plot <- 
  plot_rev_all_locations_location_agnostic_events(all_models_results = covid_all_models_rev_upper_fifteen_percent_results,
                                                  plot_model_colours = covid_model_colours,
                                                  plot_model_names = corrected_covid_model_names,
                                                  plot_model_levels = covid_model_levels,
                                                  focus_specific_horizons = c(1, 4),
                                                  horizon_target_types = horizon_target_types)
covid_upper_fifteen_percent_events_rev_location_agnostic_results_all_locations_plot


covid_upper_five_percent_events_rev_location_agnostic_results_all_locations_plot <- 
  plot_rev_all_locations_location_agnostic_events(all_models_results = covid_all_models_rev_upper_five_percent_results,
                                                  plot_model_colours = covid_model_colours,
                                                  plot_model_names = corrected_covid_model_names,
                                                  plot_model_levels = covid_model_levels,
                                                  focus_specific_horizons = c(1, 4),
                                                  horizon_target_types = horizon_target_types)
covid_upper_five_percent_events_rev_location_agnostic_results_all_locations_plot

covid_upper_fifteen_percent_events_rev_location_specific_results_versus_baseline_all_locations_plot <- 
  plot_rev_all_locations_location_specific_events(all_models_results = covid_all_models_rev_upper_fifteen_percent_results_versus_baseline,
                                                  plot_model_colours = covid_model_colours,
                                                  plot_model_names = corrected_covid_model_names,
                                                  plot_model_levels = covid_model_levels,
                                                  focus_specific_horizons = c(1,2,3, 4),
                                                  horizon_target_types = horizon_target_types)
covid_upper_fifteen_percent_events_rev_location_specific_results_versus_baseline_all_locations_plot



covid_upper_fifteen_percent_events_rev_location_specific_results_versus_baseline_all_locations_plot <- 
  plot_rev_all_locations_location_specific_events(all_models_results = covid_all_models_rev_upper_fifteen_percent_results_versus_baseline,
                                                  plot_model_colours = covid_model_colours,
                                                  plot_model_names = corrected_covid_model_names,
                                                  plot_model_levels = covid_model_levels,
                                                  focus_specific_horizons = c(1, 4),
                                                  horizon_target_types = horizon_target_types)
covid_upper_fifteen_percent_events_rev_location_specific_results_versus_baseline_all_locations_plot



# covid_upper_five_percent_events_rev_location_agnostic_results_versus_baseline_all_locations_plot <- 
#   plot_rev_all_locations_location_agnostic_events(all_models_results = covid_all_models_rev_upper_five_percent_results_versus_baseline,
#                                                   plot_model_colours = covid_model_colours,
#                                                   plot_model_names = corrected_covid_model_names,
#                                                   plot_model_levels = covid_model_levels,
#                                                   focus_specific_horizons = c(1, 4),
#                                                   horizon_target_types = horizon_target_types)
# covid_upper_five_percent_events_rev_location_agnostic_results_versus_baseline_all_locations_plot
#How can I interpret positive REV even at high C/L ratios 

covid_upper_five_percent_events_rev_location_agnostic_results_all_locations_plot <- 
  plot_rev_all_locations_location_agnostic_events(all_models_results = covid_all_models_rev_upper_five_percent_results,
                                                  plot_model_colours = covid_model_colours,
                                                  plot_model_names = corrected_covid_model_names,
                                                  plot_model_levels = covid_model_levels,
                                                  focus_specific_horizons = c(1, 4),
                                                  horizon_target_types = horizon_target_types)
covid_upper_five_percent_events_rev_location_agnostic_results_all_locations_plot

covid_upper_five_percent_events_rev_location_specific_results_all_locations_plot <- 
  plot_rev_all_locations_location_specific_events(all_models_results = covid_all_models_rev_upper_five_percent_results,
                                                  plot_model_colours = covid_model_colours,
                                                  plot_model_names = corrected_covid_model_names,
                                                  plot_model_levels = covid_model_levels,
                                                  focus_specific_horizons = c(1, 4),
                                                  horizon_target_types = horizon_target_types)
covid_upper_five_percent_events_rev_location_specific_results_all_locations_plot

covid_upper_five_percent_events_rev_location_agnostic_results_all_locations_plot <- 
  plot_rev_all_locations_location_agnostic_events(all_models_results = covid_all_models_rev_upper_five_percent_results,
                                                  plot_model_colours = covid_model_colours,
                                                  plot_model_names = corrected_covid_model_names,
                                                  plot_model_levels = covid_model_levels,
                                                  focus_specific_horizons = c(1, 4),
                                                  horizon_target_types = horizon_target_types)
covid_upper_five_percent_events_rev_location_agnostic_results_all_locations_plot





#Not needed:
# covid_upper_five_percent_events_rev_location_specific_results_all_locations_plot <- 
#   plot_rev_all_locations_location_specific_events(all_models_results = covid_all_models_rev_upper_five_percent_results,
#                                                   plot_model_colours = covid_model_colours,
#                                                   plot_model_names = corrected_covid_model_names,
#                                                   plot_model_levels = covid_model_levels,
#                                                   focus_specific_horizons = c(1, 4),
#                                                   horizon_target_types = horizon_target_types)
# covid_upper_five_percent_events_rev_location_specific_results_all_locations_plot
# covid_all_models_rev_upper_twenty_five_percent_results
# covid_upper_twenty_five_percent_events_rev_location_specific_results_all_locations_plot <- 
#   plot_rev_all_locations_location_specific_events(all_models_results = covid_all_models_rev_upper_twenty_five_percent_results,
#                                                   plot_model_colours = covid_model_colours,
#                                                   plot_model_names = corrected_covid_model_names,
#                                                   plot_model_levels = covid_model_levels,
#                                                   focus_specific_horizons = c(1, 4))
# covid_upper_twenty_five_percent_events_rev_location_specific_results_all_locations_plot






#3) LOAD IN BS ----
#Upper 10% (put rest in SI)
brier_score_specific_decomposition_plot_list <- readRDS(file.path(forecasting.covid_forecasts.local.dir,
                                                    "brier_score_specific_decomposition_plot_list.RDS"))
covid_facet_brier_score_specific_decomposition_diagram <- 
  ggarrange(NULL, NULL, NULL, NULL,
            NULL, brier_score_specific_decomposition_plot_list[[1]] + rotate_x_axis_theme+ coord_cartesian(xlim = c(0, 0.015)) + scale_x_continuous(labels = number_format(accuracy = 0.001),
                                                                                                                             breaks=seq(0, 0.016, by = 0.005))  +
              scale_y_continuous(labels = number_format(accuracy = 0.001))+  theme(axis.text.x = element_text(size=16,
                                                                                                              angle = 45, hjust = 1)), NULL ,
            brier_score_specific_decomposition_plot_list[[2]] + rotate_x_axis_theme +scale_x_continuous(labels = number_format(accuracy = 0.001)) + scale_y_continuous(labels = number_format(accuracy = 0.001)) + labs(title = "Upper 10% Events")+
              theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=16,
                                                                                      angle = 45, hjust = 1)),
            nrow = 2,ncol = 4,
            legend = "none",
            labels = c("1 week ahead", "","4 week ahead", "", "", "", "", ""),
            font.label = list(size = 18),
            heights = c(0.1, 1.0),
            widths = c(0.05, 0.75, 0.1,0.75))
covid_facet_brier_score_specific_decomposition_diagram  <- ggarrange(NULL,covid_facet_brier_score_specific_decomposition_diagram,
                                                                     NULL,
                                                                     heights = c(0.15, 0.75, 0.15),
                                                                     nrow = 3)
# labels = c("1 week ahead", "","4 week ahead", ""),
covid_facet_brier_score_specific_decomposition_diagram <- ggarrange(NULL, NULL,
                                                                    NULL, covid_facet_brier_score_specific_decomposition_diagram,
                                                                    labels = c("","B) Event-specific decomposition (Brier score)", "", "", ""),
                                                                    font.label = list(size = 20, face = "bold"),
                                                                    hjust = 0.2,
                                                                    widths = c(0.05, 1.0),
                                                                    heights = c(0.1, 1.0),  nrow = 2,
                                                                    ncol = 2)
covid_facet_brier_score_specific_decomposition_diagram


#Analagous decomposition for upper 10% location-agnostic
brier_score_agnostic_decomposition_plot_list <- readRDS(file.path(forecasting.covid_forecasts.local.dir,
                                                                  "brier_score_agnostic_decomposition_plot_list.RDS"))
covid_facet_brier_score_agnostic_decomposition_diagram <- 
  ggarrange(NULL, NULL, NULL, NULL,
            NULL, brier_score_agnostic_decomposition_plot_list[[1]] + rotate_x_axis_theme+ coord_cartesian(xlim = c(0, 0.015)) + scale_x_continuous(labels = number_format(accuracy = 0.001),
                                                                                                                                                    breaks=seq(0, 0.016, by = 0.005))  +
              scale_y_continuous(labels = number_format(accuracy = 0.001))+  theme(axis.text.x = element_text(size=16,
                                                                                                              angle = 45, hjust = 1)), NULL ,
            brier_score_agnostic_decomposition_plot_list[[2]] + rotate_x_axis_theme +scale_x_continuous(labels = number_format(accuracy = 0.001)) + scale_y_continuous(labels = number_format(accuracy = 0.001)) + labs(title = "Upper 10% Events")+
              theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=16,
                                                                                      angle = 45, hjust = 1)),
            nrow = 2,ncol = 4,
            legend = "none",
            labels = c("1 week ahead", "","4 week ahead", "", "", "", "", ""),
            font.label = list(size = 18),
            heights = c(0.1, 1.0),
            widths = c(0.1, 0.75, 0.1,0.75))
covid_facet_brier_score_agnostic_decomposition_diagram
# labels = c("1 week ahead", "","4 week ahead", ""),
covid_facet_brier_score_agnostic_decomposition_diagram <- ggarrange(NULL, NULL,
                                                                    NULL, covid_facet_brier_score_agnostic_decomposition_diagram,
                                                                    labels = c("","Event-specific decomposition (Brier score)", "", "", ""),
                                                                    font.label = list(size = 20, face = "bold"),
                                                                    hjust = 0.2,
                                                                    widths = c(0.1, 1.0),
                                                                    heights = c(0.1, 1.0),  nrow = 2,
                                                                    ncol = 2)
covid_facet_brier_score_agnostic_decomposition_diagram





brier_score_specific_murphy_plot_list <- readRDS(file.path(forecasting.covid_forecasts.local.dir,
                                                                  "brier_score_specific_murphy_plot_list.RDS"))
brier_score_specific_murphy_plot_list[[1]]
covid_facet_brier_score_specific_murphy_diagram <- 
  ggarrange(NULL, NULL, NULL, NULL,
            NULL, brier_score_specific_murphy_plot_list[[1]] + rotate_x_axis_theme+ 
              scale_x_continuous(labels = number_format(accuracy = 0.001))+
              scale_y_continuous(labels = number_format(accuracy = 0.001))+  
              theme(axis.text.x = element_text(size=16, angle = 45, hjust = 1)), NULL ,
            brier_score_specific_murphy_plot_list[[2]] + rotate_x_axis_theme +scale_x_continuous(labels = number_format(accuracy = 0.001)) + scale_y_continuous(labels = number_format(accuracy = 0.001)) + labs(title = "Upper 10% Events")+
              theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=16,
                                                                                      angle = 45, hjust = 1)),
            nrow = 2,ncol = 4,
            legend = "none",
            labels = c("1 week ahead", "","4 week ahead", "", "", "", "", ""),
            font.label = list(size = 18),
            heights = c(0.1, 1.0),
            widths = c(0.1, 0.75, 0.1,0.75))
covid_facet_brier_score_specific_murphy_diagram <- ggarrange(NULL, covid_facet_brier_score_specific_murphy_diagram,
                                                             NULL, heights = c(0.15, 0.75, 0.15), nrow = 3)
# labels = c("1 week ahead", "","4 week ahead", ""),
covid_facet_brier_score_specific_murphy_diagram <- ggarrange(NULL,NULL,
                                                             NULL, covid_facet_brier_score_specific_murphy_diagram,
                                                                    labels = c("","C.2: Event-specific Murphy diagrams", "", ""),
                                                                    font.label = list(size = 20, face = "bold"),
                                                                    widths = c(0.1, 1.0),
                                                             hjust = -0.25,
                                                                    heights = c(0.1, 1.0), nrow = 2,
                                                             ncol = 2)
covid_facet_brier_score_specific_murphy_diagram












#Analagous murphy for upper 10% location-agnostic

brier_score_agnostic_murphy_plot_list <- readRDS(file.path(forecasting.covid_forecasts.local.dir,
                                                           "brier_score_agnostic_murphy_plot_list.RDS"))
brier_score_agnostic_murphy_plot_list[[1]]
covid_facet_brier_score_agnostic_murphy_diagram <- 
  ggarrange(NULL, NULL, NULL, NULL,
            NULL, brier_score_agnostic_murphy_plot_list[[1]] + rotate_x_axis_theme+ 
              scale_x_continuous(labels = number_format(accuracy = 0.001))+
              scale_y_continuous(labels = number_format(accuracy = 0.001))+  
              theme(axis.text.x = element_text(size=16, angle = 45, hjust = 1)), NULL ,
            brier_score_agnostic_murphy_plot_list[[2]] + rotate_x_axis_theme +scale_x_continuous(labels = number_format(accuracy = 0.001)) + scale_y_continuous(labels = number_format(accuracy = 0.001)) + labs(title = "Upper 10% Events")+
              theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=16,
                                                                                      angle = 45, hjust = 1)),
            nrow = 2,ncol = 4,
            legend = "none",
            labels = c("1 week ahead", "","4 week ahead", "", "", "", "", ""),
            font.label = list(size = 18),
            heights = c(0.1, 1.0),
            widths = c(0.1, 0.75, 0.1,0.75))
covid_facet_brier_score_agnostic_murphy_diagram
# labels = c("1 week ahead", "","4 week ahead", ""),
covid_facet_brier_score_agnostic_murphy_diagram <- ggarrange(NULL,NULL,
                                                             NULL, covid_facet_brier_score_agnostic_murphy_diagram,
                                                             labels = c("","Event-specific Murphy diagrams", "", ""),
                                                             font.label = list(size = 20, face = "bold"),
                                                             widths = c(0.1, 1.0),
                                                             heights = c(0.1, 1.0), nrow = 2,
                                                             ncol = 2)
covid_facet_brier_score_agnostic_murphy_diagram









# 
# 
# brier_score_agnostic_murphy_plot_list <- readRDS(file.path(forecasting.covid_forecasts.local.dir,
#                                                   "brier_score_agnostic_murphy_plot_list.RDS"))
# 
# covid_facet_brier_score_agnostic_murphy_diagram <- 
#   ggarrange(NULL, 
#             brier_score_agnostic_murphy_plot_list[[1]]+rotate_x_axis_theme+theme(aspect.ratio = NULL), 
#             NULL ,
#             brier_score_agnostic_murphy_plot_list[[2]] + rotate_x_axis_theme+theme(aspect.ratio = NULL),
#             nrow = 1,ncol = 4,
#             legend = "none",
#             labels = c("1 week ahead", "","4 week ahead", ""),
#             font.label = list(size = 18),
#             widths = c(0.1, 0.75, 0.1,0.75))
# covid_facet_brier_score_agnostic_murphy_diagram
# covid_facet_brier_score_agnostic_murphy_diagram <- ggarrange(NULL,covid_facet_brier_score_agnostic_murphy_diagram,
#                                                     nrow = 2, ncol = 1, hjust = -0.5,
#                                                     labels = c("C.2"), heights = c(0.06, 1.0))
# 

#Location-specific upper 10%
brier_score_plot_list <- readRDS(file.path(forecasting.covid_forecasts.local.dir,
                                           "brier_score_plot_list.RDS"))
covid_facet_brier_score_decomposition_diagram <- 
  ggarrange(plotlist = brier_score_plot_list, nrow = 2,ncol = 1,
            legend = "none",
            labels = c("1 week \n ahead", "4 week \n ahead "),
            font.label = list(size = 18))
brier_score_plot_list
covid_facet_brier_score_decomposition_diagram

covid_facet_brier_score_decomposition_diagram <- 
  ggarrange(NULL, brier_score_plot_list[[1]] + rotate_x_axis_theme+ coord_cartesian(xlim = c(0, 0.015)) + scale_x_continuous(labels = number_format(accuracy = 0.001),
                                                                                        breaks=seq(0, 0.016, by = 0.005))  +
              scale_y_continuous(labels = number_format(accuracy = 0.001))+  theme(axis.text.x = element_text(size=16,
                                                                                                              angle = 45, hjust = 1)), NULL ,
            brier_score_plot_list[[2]] + rotate_x_axis_theme +scale_x_continuous(labels = number_format(accuracy = 0.001)) + scale_y_continuous(labels = number_format(accuracy = 0.001)) + labs(title = "Upper 10% Events")+
              theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=16,
                                                                                      angle = 45, hjust = 1)),
            nrow = 1,ncol = 4,
            legend = "none",
            labels = c("1 week ahead", "","4 week ahead", ""),
            font.label = list(size = 18),
            widths = c(0.1, 0.75, 0.1,0.75))
covid_facet_brier_score_decomposition_diagram
covid_facet_brier_score_decomposition_diagram <- ggarrange(NULL,
                                                           covid_facet_brier_score_decomposition_diagram,
                                                           labels = c("B) Event-specific evaluation (Brier score)", ""),
                                                           font.label = list(size = 20, face = "bold"),
                                                           hjust = 0.2,
                                                           heights = c(0.1, 1.0), ncol = 1, nrow = 2)
covid_facet_brier_score_decomposition_diagram



brier_score_murphy_plot_list <- readRDS(file.path(forecasting.covid_forecasts.local.dir,
                                           "brier_score_murphy_plot_list.RDS"))

covid_facet_brier_score_murphy_diagram <- 
  ggarrange(NULL, 
            brier_score_murphy_plot_list[[1]]+rotate_x_axis_theme+theme(aspect.ratio = NULL), 
            NULL ,
            brier_score_murphy_plot_list[[2]] + rotate_x_axis_theme+theme(aspect.ratio = NULL),
            nrow = 1,ncol = 4,
            legend = "none",
            labels = c("1 week ahead", "","4 week ahead", ""),
            font.label = list(size = 18),
            widths = c(0.1, 0.75, 0.1,0.75))

covid_facet_brier_score_murphy_diagram <- ggarrange(NULL,covid_facet_brier_score_murphy_diagram,
          nrow = 2, ncol = 1, hjust = -0.5,
          labels = c("C.2"), heights = c(0.06, 1.0))
print(covid_facet_brier_score_murphy_diagram)
# ggarrange(
#   NULL, NULL,
#   covid_facet_pinball_loss_decomposition_diagram,
#           covid_facet_brier_score_decomposition_diagram,
#           nrow = 2, ncol = 2,
#   legend = "none",
#   labels = c("User-specific (Pinball loss)", "Event-specific (Brier Score)", "","" ),
#   font.label = list(size = 20, face = "bold"),
#           widths = c(1, 0.5, 1, 0.5),
#   heights = c(0.1, 0.1, 0.75, 0.75))



#3) DTW on predictability, What to do with DSC etc????????

#4) Get dengue data


