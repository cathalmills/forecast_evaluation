#Predictability
ggarrange(NULL, 
          state_1_rwis_predictability_boxplot, NULL, 
          state_1_interval_coverage_predictability_boxplot,  NULL, 
          state_1_r2_predictability_boxplot,
          ncol = 1,
          heights = c())


#Brier decomposition
ggsave(state_1_extreme_before_wave_brier_decomposition_plot,
       filename = file.path(forecasting.covid_forecasts.out.dir,
                            "state_1_extreme_before_wave_brier_decomposition_plot.pdf"),
       h = 8, w = 20)
state_1_extreme_before_wave_brier_decomposition_plot
state_1_before_vs_during_brier_decomposition_plot <- 
  ggarrange(NULL,state_1_extreme_before_wave_brier_decomposition_plot + theme(legend.position = "none"),
            NULL, state_1_extreme_during_wave_brier_decomposition_plot + theme(legend.position = "bottom"),
            common.legend = TRUE, ncol = 1, heights = c(0.05, 0.5, 0.05, 0.75),
            legend = "bottom",
            labels = c("A) Before wave", "" ,"B) During wave", "", "", ""),
            font.label = list(size = 24, face = "bold"))

ggsave(state_1_before_vs_during_brier_decomposition_plot,
       filename = file.path(forecasting.covid_forecasts.out.dir,
                            "state_1_before_vs_during_brier_decomposition_plot.pdf"),
       h = 16, w = 20)

state_1_before_vs_during_brier_decomposition_plot


#Brier Murphy
state_1_before_vs_during_brier_murphy_plot <- 
  ggarrange(NULL,state_1_extreme_before_wave_brier_murphy_plot + theme(legend.position = "none"),
            NULL, state_1_extreme_during_wave_brier_murphy_plot + theme(legend.position = "bottom"),
            common.legend = TRUE, ncol = 1, heights = c(0.05, 0.5, 0.05, 0.75),
            legend = "bottom",
            labels = c("A) Before wave", "" ,"B) During wave", "", "", ""),
            font.label = list(size = 24, face = "bold"))

ggsave(state_1_before_vs_during_brier_murphy_plot,
       filename = file.path(forecasting.covid_forecasts.out.dir,
                            "state_1_before_vs_during_brier_murphy_plot.pdf"),
       h = 16, w = 20)

state_1_before_vs_during_brier_murphy_plot



#Pinball loss
location_past_pinball <- all_locations_past_list[[1]][[1]]
location_pinball_past_decomposition_diagram <- 
  plot_selected_pinball_decomposition(location_past_pinball,
                                      chosen_quantiles = c(0.9, 0.75))
location_pinball_past_decomposition_diagram
location_past_other_quantiles_pinball_decomposition_plots <- 
  plot_selected_pinball_decomposition(location_past_pinball,
                                      chosen_quantiles = unique(state_level_covid_forecasts_dt$quantile_level),
                                      legend_on = NULL)
location_past_other_quantiles_pinball_decomposition_plots
ggsave(location_past_other_quantiles_pinball_decomposition_plots,
       filename = file.path(forecasting.covid_forecasts.out.dir,"state_1_past_other_quantiles_pinball_decomposition_plots.pdf"),
       h = 10, w = 24)

#Pinball Murphy
all_quantiles_state_1_historical_pinball_murphy_diagram
all_quantiles_state_1_historical_pinball_murphy_diagram_restricted

ggsave(all_quantiles_state_1_historical_pinball_murphy_diagram,
       filename = file.path(forecasting.covid_forecasts.out.dir,"all_quantiles_state_1_historical_pinball_murphy_diagram.pdf"),
       h = 24, w = 24)


ggsave(all_quantiles_state_1_wave_pinball_murphy_diagram,
       filename = file.path(forecasting.covid_forecasts.out.dir,"all_quantiles_state_1_wave_pinball_murphy_diagram.pdf"),
       h = 24, w = 24)



location_wave_pinball <- all_locations_wave_list[[1]][[1]]
location_pinball_wave_decomposition_diagram <- 
  plot_selected_pinball_decomposition(location_wave_pinball,
                                      chosen_quantiles = c(0.9, 0.75))

location_wave_other_quantiles_pinball_decomposition_plots <- 
  plot_selected_pinball_decomposition(location_wave_pinball,
                                      chosen_quantiles = unique(state_level_covid_forecasts_dt$quantile_level),
                                      legend_on = TRUE)

before_vs_during_pl_decomposition_plot <- 
  ggarrange(NULL,location_past_other_quantiles_pinball_decomposition_plots + theme(legend.position = "bottom"),
          NULL, location_wave_other_quantiles_pinball_decomposition_plots + theme(legend.position = "bottom"),
          common.legend = TRUE, ncol = 1, heights = c(0.025, 0.5, 0.02, 0.65),
          legend = "bottom",
          labels = c("A) Before wave", "" ,"B) During wave", "", "", ""),
          font.label = list(size = 24, face = "bold"))
before_vs_during_pl_decomposition_plot
ggsave(before_vs_during_pl_decomposition_plot,
       filename = file.path(forecasting.covid_forecasts.out.dir,"state_1_before_vs_during_pl_decomposition_plot.pdf"),
       h = 22, w = 24)

#before and during rPL
state_1_before_and_during_rpl_plot_chosen_quantiles <- list_before_and_during_rpl_plots_chosen_quantiles[[1]]
state_1_before_and_during_rpl_plot_chosen_quantiles
ggsave(state_1_before_and_during_rpl_plot_chosen_quantiles,
       filename = file.path(forecasting.covid_forecasts.out.dir,"state_1_before_and_during_rpl_plot_chosen_quantiles.pdf"),
       h = 14, w = 24)

state_1_before_and_during_rpl_plot_higher_cl_ratios <- list_before_and_during_rpl_plots_higher_cl_ratios[[2]]
ggsave(state_1_before_and_during_rpl_plot_higher_cl_ratios,
       filename = file.path(forecasting.covid_forecasts.out.dir,"state_1_before_and_during_rpl_plot_higher_cl_ratios.pdf"),
       h = 14, w = 24)


# 
# all_locations_historical_plot_list[[1]][[6]]
# 
# all_locations_wave_plot_list[[1]][[5]]+facet_wrap(target_type ~. , nrow = 2)
# all_locations_wave_plot_list[[1]][[6]]+facet_wrap(target_type ~. , nrow = 2)


all_locations_wave_plot_list[[1]][[5]]
#REV before vs during
state_1_rev_row <- ggarrange(all_locations_wave_plot_list[[1]][[5]] +
                               theme(axis.text.x = element_text(size=16),
                                     axis.text.y = element_text(size=16),
                                     axis.title=element_text(size=18)) +
                       labs(title = "REV vs Constant Probability Model (y >= 20000)")+
                       theme(plot.title = element_text(size = 18,
                                                       face = "bold")),
                     NULL,
                     all_locations_wave_plot_list[[1]][[6]]+
                       theme(axis.text.x = element_text(size=16),
                             axis.text.y = element_text(size=16),
                             axis.title=element_text(size=18)) +
                       labs(title = "REV vs Baseline Model (y >= 20000)")+
                       theme(plot.title = element_text(size = 18,
                                                       face = "bold")),
                     nrow = 3, ncol = 1, heights = c(0.5, 0.025, 0.5),
                     legend = "none",
                     # common.legend = TRUE,
                     font.label = list(size = 20))
state_1_rev_row
state_1_rev_row <- ggarrange(state_1_rev_row, NULL,
                     nrow = 1, widths = c(0.9, 0.02),
                     legend = "none")
state_1_rev_row
state_1_fixed_during_wave_brier_murphy_plot
state_1_brier_rev_panel <- ggarrange(state_1_fixed_during_wave_brier_decomposition_plot,
          NULL,
          state_1_fixed_during_wave_brier_murphy_plot,
          NULL,
          legend = "none",
          align = "h",
          state_1_rev_row, nrow = 1,
          widths = c(0.3, 0.05, 0.4,0.1 ,0.75))
state_1_brier_rev_panel
state_1_brier_rev_panel <- ggarrange(NULL,
                                     state_1_brier_rev_panel,
                                     hjust = -0.25,
                                                 labels = c("C) Event-specific evaluation", ""), ncol = 1, nrow = 2,
                                                 font.label = list(size = 24, face = "bold"),
                                                 heights = c(0.075, 0.9))
state_1_brier_rev_panel

all


#Real-time evaluation plots





cases_predictability_panel
state_1_cases_predictability_panel <- ggarrange(list_before_and_during_plot_cases[[1]],
                                        list_before_and_during_predictability_plots[[1]],
                                        align = "v",
                                        legend = "bottom",
                                        nrow = 2,
                                        common.legend =  TRUE)
tmp <- list_before_and_during_wis_plots[[1]]+theme(legend.position = "none")

state_1_before_during_panel <- ggarrange(NULL, NULL, NULL, state_1_cases_predictability_panel, NULL, tmp,
          ncol = 3,
          nrow = 2,
          heights = c(0.075, 0.9), widths = c(0.6, 0.1, 0.6),
          hjust = -0.25,
          labels = c("A) Cases and predictability", "" ,"B) Weighted Interval Score", "", "", ""),
          font.label = list(size = 24, face = "bold"))
state_1_before_during_panel
#During wave
location_pinball_wave_murphy_digram  <- all_locations_wave_plot_list[[1]][[2]]
location_pinball_wave_murphy_digram
location_pinball_wave_murphy_digram
state_1_location_pinball_wave_panel <- ggarrange(location_pinball_wave_decomposition_diagram,
                                                                                   NULL,
                                                                                   location_pinball_wave_murphy_digram,
                                                                                   nrow = 1,
                                                                                   align = "h",
                                                                                   widths = c(0.4, 0.05, 0.65))
state_1_location_pinball_wave_panel <- ggarrange(NULL,
                                                 state_1_location_pinball_wave_panel,
                                                 hjust = -0.25,
                                                 labels = c("D) User-specific evaluation", ""), ncol = 1, nrow = 2,
                                                 font.label = list(size = 24, face = "bold"),
                                                 heights = c(0.075, 0.9))
state_1_location_pinball_wave_panel



# state_1_brier_rev_panel <- ggarrange(state_1_fixed_during_wave_brier_decomposition_plot,
#           NULL,
#           state_1_fixed_during_wave_brier_murphy_plot, 
#           NULL,
#           all_locations_wave_plot_list[[1]][[6]]+facet_wrap(target_type ~., nrow = 2)+
#             theme(legend.position = "none"),
#           widths = c(0.3, 0.15,0.4, 0.15, 0.6),
#           nrow = 1,
#           ncol = 5)
# 
# state_1_brier_rev_panel <- ggarrange(NULL,state_1_brier_rev_panel,
#                                      labels = c("C)", ""), ncol = 1, nrow = 2,
#                                      font.label = list(size = 24, face = "bold"),
#                                      heights = c(0.075, 0.9))
# 
# state_1_brier_rev_panel




patchwork::pat

# figure_four_bottom_row <- ggarrange(state_1_extreme_during_wave_brier_murphy_plot + theme(legend.position = "none"),
#                                     NULL,
#                                     state_1_extreme_during_wave_brier_decomposition_plot + theme(legend.position = "bottom"), legend = "bottom",
#                                     align = "hv",
#                                     widths = c(0.5, 0.1,0.75), nrow = 1)
# figure_four_bottom_row
# state_1_extreme_during_wave_brier_decomposition_plot
state_1_extreme_during_wave_brier_murphy_plot

state_1_extreme_brier_panel <- ggarrange(state_1_extreme_during_wave_brier_murphy_plot, NULL, state_1_extreme_during_wave_brier_decomposition_plot,
                                    nrow = 1, ncol = 3, 
                                    widths = c(0.7, 0.04, 0.83))
state_1_extreme_brier_panel <- ggarrange(NULL,
                                         state_1_extreme_brier_panel,
                                         hjust = -0.25,
                                                 labels = c("E) Wave-specific extreme events", ""), ncol = 1, nrow = 2,
                                                 font.label = list(size = 24, face = "bold"),
                                                 heights = c(0.075, 0.9))
state_1_extreme_brier_panel





#Top row:
state_1_before_during_panel
#Second row:
state_1_brier_rev_panel
#Third row:
state_1_location_pinball_wave_panel
#Bottom row:
state_1_extreme_brier_panel


state_1_brier_rev_panel
state_1_plot <- ggarrange(state_1_before_during_panel,
          NULL,
          state_1_brier_rev_panel,
          NULL,
          state_1_location_pinball_wave_panel,
          NULL,
          state_1_extreme_brier_panel,
          NULL,
          ncol = 1, nrow = 8,
          heights = c(0.435, 0.03, 0.625, 0.03, 0.54, 0.03, 0.465, 0.01))
state_1_plot
ggsave(state_1_plot,
       filename = file.path(forecasting.covid_forecasts.out.dir,"figure_four_new.pdf"),
       h = 27, w = 24)

