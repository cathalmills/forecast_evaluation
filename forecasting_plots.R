require(patchwork)

#SI Plots ---- 
#Plot for aggregate performance (across all states + time pts) for four horizons
all_horizons_wis_covid_aggregate_performance_all_locations_plot
past_predictability_ensemble_rpl_plot

ggsave(all_horizons_wis_covid_aggregate_performance_all_locations_plot,
       filename = file.path(forecasting.covid_forecasts.out.dir,
                            "all_horizons_wis_covid_aggregate_performance_all_locations_plot.pdf"),
       h = 24, w = 24)

#Trailing-window-specific quantile scores, median over all locations
higher_cl_ratios_trailing_window_location_specific_pinball_loss_summarised_over_all_locations_plot
ggsave(trailing_window_location_specific_pinball_loss_summarised_over_all_locations_plot,
       file = file.path(forecasting.covid_forecasts.out.dir,
                        "trailing_window_location_specific_pinball_loss_summarised_over_all_locations_plot.pdf"),
       h = 24, w = 22)
higher_cl_ratios_trailing_window_location_specific_pinball_loss_summarised_over_all_locations_plot


higher_cl_ratios_covid_facet_pinball_loss_murphy_diagram
ggsave(higher_cl_ratios_covid_facet_pinball_loss_murphy_diagram,
       file = file.path(forecasting.covid_forecasts.out.dir,
                        "higher_cl_ratios_covid_facet_pinball_loss_murphy_diagram.pdf"),
       h = 14, w = 22)
ggsave(covid_facet_pinball_loss_murphy_diagram,
       file = file.path(forecasting.covid_forecasts.out.dir,
                        "covid_facet_pinball_loss_murphy_diagram.pdf"),
       h = 14, w = 22)

#Trailing window pinball loss across all horizons
rpinball_loss_boxplot_by_model_across_horizons_focused_quantiles

#Different quantiles, rPL (all horizons)
rpinball_loss_boxplot_by_model_and_predictability_across_horizons


#Predictability vs R(t)
predictability_rt_plots <- 
  ggarrange(predictability_all_locations_over_time_boxplot, Rt_all_locations_over_time_boxplot,
          nrow = 2)

ggsave(predictability_rt_plots,
       filename = file.path(forecasting.covid_forecasts.out.dir,"predictability_rt_plots.pdf"),
       h = 16, w = 22)

ggsave(predictability_individual_locations_over_time_tileplot,
       filename = file.path(forecasting.covid_forecasts.out.dir,"predictability_individual_locations_over_time_tileplot.pdf"),
       h = 24, w = 22)




#SI REV
ggarrange(covid_upper_ten_percent_events_rev_location_agnostic_results_all_locations_plot,
          covid_upper_ten_percent_events_rev_location_agnostic_results_versus_baseline_all_locations_plot,
          nrow = 1, common.legend = TRUE, ncol = 2,
          legend = "bottom",
          labels = c("Versus constant probability model",
                     "Versus baseline model"))

ggsave(covid_upper_ten_percent_events_rev_location_agnostic_results_all_locations_plot, 
       file = file.path(forecasting.covid_forecasts.out.dir,
                        "covid_upper_ten_percent_events_rev_location_agnostic_results_all_locations_plot.pdf"),
       h = 14, w= 22)
ggsave(covid_upper_ten_percent_events_rev_location_agnostic_results_versus_baseline_all_locations_plot, 
       file = file.path(forecasting.covid_forecasts.out.dir,
                        "covid_upper_ten_percent_events_rev_location_agnostic_results_versus_baseline_all_locations_plot.pdf"),
                        h = 14, w= 22)
ggsave(covid_upper_ten_percent_events_rev_location_specific_results_versus_baseline_all_locations_plot, 
       file = file.path(forecasting.covid_forecasts.out.dir,
                        "covid_upper_ten_percent_events_rev_location_specific_results_versus_baseline_all_locations_plot.pdf"),
       h = 14, w= 22)
covid_upper_ten_percent_events_rev_location_specific_results_versus_baseline_all_locations_plot
ggsave(covid_upper_ten_percent_events_rev_location_specific_results_all_locations_plot, 
       file = file.path(forecasting.covid_forecasts.out.dir,
                        "covid_upper_ten_percent_events_rev_location_specific_results_all_locations_plot.pdf"),
       h = 14, w= 22)
covid_upper_ten_percent_events_rev_location_specific_results_all_locations_plot


ggsave(covid_upper_fifteen_percent_events_rev_location_agnostic_results_all_locations_plot, 
       file = file.path(forecasting.covid_forecasts.out.dir,
                        "covid_upper_fifteen_percent_events_rev_location_agnostic_results_all_locations_plot.pdf"),
       h = 14, w= 22)
ggsave(covid_upper_fifteen_percent_events_rev_location_agnostic_results_versus_baseline_all_locations_plot, 
       file = file.path(forecasting.covid_forecasts.out.dir,
                        "covid_upper_fifteen_percent_events_rev_location_agnostic_results_versus_baseline_all_locations_plot.pdf"),
       h = 14, w= 22)
ggsave(covid_upper_fifteen_percent_events_rev_location_specific_results_versus_baseline_all_locations_plot, 
       file = file.path(forecasting.covid_forecasts.out.dir,
                        "covid_upper_fifteen_percent_events_rev_location_specific_results_versus_baseline_all_locations_plot.pdf"),
       h = 14, w= 22)
ggsave(covid_upper_fifteen_percent_events_rev_location_specific_results_all_locations_plot, 
       file = file.path(forecasting.covid_forecasts.out.dir,
                        "covid_upper_fifteen_percent_events_rev_location_specific_results_all_locations_plot.pdf"),
       h = 14, w= 22)





covid_upper_fifteen_percent_events_rev_location_agnostic_results_all_locations_plot
covid_upper_fifteen_percent_events_rev_location_agnostic_results_versus_baseline_all_locations_plot
covid_upper_fifteen_percent_events_rev_location_specific_results_all_locations_plot
covid_upper_fifteen_percent_events_rev_location_specific_results_versus_baseline_all_locations_plot

covid_upper_five_percent_events_rev_location_agnostic_results_all_locations_plot
covid_upper_five_percent_events_rev_location_specific_results_all_locations_plot




#Main plots
covid_wis_plot_top_row <- 
  ggarrange(wis_covid_aggregate_performance_all_locations_plot +
               theme(legend.position = "none"),
          wis_covid_aggregate_performance_individual_locations_plot,
          ncol = 2,
          widths = c(0.75, 1),
          labels = c("A)", "B)"),
          font.label = list(size = 24, face = "bold"))

covid_wis_plot_bottom_row <- 
  ggarrange(trailing_window_wis_covid_plot,
            labels = c("C)"),
            font.label = list(size = 24, face = "bold"))


covid_wis_plot <- ggarrange(covid_wis_plot_top_row,
          covid_wis_plot_bottom_row,
          nrow = 2,
          heights = c(1, 0.6))
covid_wis_plot
forecasting.covid_forecasts.out.dir
ggsave(covid_wis_plot,
       filename = file.path(forecasting.covid_forecasts.out.dir,
                            "covid_wis_plot.pdf"),
       h = 21, w = 24)



#Pinball loss DECOMPOSITION
#1wk 4wk in rows

# covid_facet_brier_score_decomposition_diagram <- ggarrange(NULL,
#                                                            covid_facet_brier_score_decomposition_diagram,
#                                                             labels = c("b) Event-specific (Brier score)", ""),
#                                                             font.label = list(size = 24, face = "bold"),
#                                                            hjust = -1.0,
#                                                             heights = c(0.1, 1.0), ncol = 1, nrow = 2)
# covid_facet_brier_score_decomposition_diagram
#SI: PINBALL LOSS OVER TIME

#BS DECOMPOSITION
#BS OVER TIME
covid_facet_brier_score_decomposition_diagram
covid_facet_pinball_loss_decomposition_diagram
user_and_event_specific_evaluation_top_row <- ggarrange(NULL, covid_facet_pinball_loss_decomposition_diagram,
                                                        NULL,
          covid_facet_brier_score_specific_decomposition_diagram,
          NULL,
          nrow = 1,
          widths = c(0.06,1, 0.1, 0.7, 0.06))

user_and_event_specific_evaluation_top_row




#User-and event-specific
covid_facet_brier_score_specific_murphy_diagram
user_and_event_specific_murphy_diagrams <- ggarrange(NULL, covid_facet_pinball_loss_murphy_diagram, 
                                                     NULL,
                                                     covid_facet_brier_score_specific_murphy_diagram,
                                                     NULL,
                                                     nrow = 1, widths = c(0.05, 1.05, 0.08, 0.75, 0.075))
print(user_and_event_specific_murphy_diagrams)
dev.off()
rev_row <- ggarrange(NULL,
                     covid_upper_ten_percent_events_rev_location_agnostic_results_all_locations_plot +
            labs(title = "REV vs Constant Probability Model (Upper 10% Events)")+
              theme(axis.text.y = element_text(size=18))+
              theme(axis.text.x = element_text(size=18))+
              guides(color = guide_legend(ncol = 2))+
            theme(plot.title = element_text(size = 18)),
          NULL,
          covid_upper_ten_percent_events_rev_location_specific_results_versus_baseline_all_locations_plot+
            labs(title = "REV vs Baseline Model (Upper 10% Events)")+
            theme(axis.text.y = element_text(size=18))+
            theme(axis.text.x = element_text(size=18))+
            theme(plot.title = element_text(size = 18))+
            guides(color = guide_legend(ncol = 2)),
          nrow = 4, ncol = 1, heights = c(0.1, 0.5, 0.025, 0.5),
          labels = c("C.3: Relative Economic Value (REV)", ""),
          legend = "right",
          common.legend = TRUE,
          font.label = list(size = 20))
rev_row <- ggarrange(NULL,rev_row, NULL,
                     nrow = 1, widths = c(0.1, 0.7, 0.1))
rev_row



# 
# 
# 
# covid_upper_ten_percent_events_rev_location_agnostic_results_all_locations_plot <- 
#   ggarrange(NULL,
#             covid_upper_ten_percent_events_rev_location_agnostic_results_all_locations_plot +
#               rotate_x_axis_theme+
#               guides(color = guide_legend(nrow = 1))+
#               labs(title = "REV (Upper 10% Events)")+theme(plot.title = element_text(size = 18)),
#             labels = c("C.3", ""),
#             legend = "bottom",
#             font.label = list(size = 18),
#             nrow = 2, heights = c(0.1, 1))
# covid_upper_ten_percent_events_rev_location_agnostic_results_all_locations_plot <-
#   ggarrange(NULL,covid_upper_ten_percent_events_rev_location_agnostic_results_all_locations_plot,
#             NULL, nrow = 1, widths = c(0.175, 0.75, 0.175),
#             ncol = 3, 
#             legend = "bottom")
# covid_upper_ten_percent_events_rev_location_agnostic_results_all_locations_plot
# # covid_upper_five_percent_events_rev_location_agnostic_results_all_locations_plot <- 
#   covid_upper_five_percent_events_rev_location_agnostic_results_all_locations_plot +rotate_x_axis_theme+
#               labs(title = "REV (Upper 5% Events)")+theme(plot.title = element_text(size = 18))
# rev_top_row <- ggarrange(NULL, covid_upper_ten_percent_events_rev_location_agnostic_results_all_locations_plot+theme(legend.position = "none"), NULL,
#           widths = c(0.15, 0.75, 0.15),nrow = 1)
# rev_bottom_row <- ggarrange(NULL, covid_upper_five_percent_events_rev_location_agnostic_results_all_locations_plot, NULL,
#           widths = c(0.15, 0.75, 0.15),nrow = 1, legend = "bottom")
# rev_plots <- 
#   ggarrange(rev_top_row+theme(legend.position = "none"),
#             rev_bottom_row,
#             nrow = 2, legend = "right",
#             heights = c(0.5, 0.8))
# print(rev_plots)
# dev.off()
# # covid_upper_ten_percent_events_rev_location_agnostic_results_all_locations_plot <- covid_upper_ten_percent_events_rev_location_agnostic_results_all_locations_plot + theme(legend.position = "none")
# rev_plots <- 
#   ggarrange(covid_upper_ten_percent_events_rev_location_agnostic_results_all_locations_plot+rotate_x_axis_theme+theme(legend.position = "none"),
#           covid_upper_five_percent_events_rev_location_agnostic_results_all_locations_plot ,
#           nrow = 2, legend = "bottom",
#           heights = c(0.5, 1.15))
# rev_plots
# user_and_event_specific_row <- 
#   ggarrange(NULL,
#             user_and_event_specific_murphy_diagrams,
#             labels = c("C.1", ""),
#             font.label = list(size = 18),
#             nrow = 2, heights = c(0.05, 1))
# user_and_event_specific_row
rev_row
user_and_event_specific_murphy_diagrams
user_and_event_specific_row <- ggarrange(user_and_event_specific_murphy_diagrams,
                                         rev_row,
                                         nrow = 2, heights = c(1.0, 1.1))
user_and_event_specific_row
user_and_event_specific_row <- ggarrange(NULL,
                                         user_and_event_specific_row,
                                         labels = c("C) User- and event-specific evaluation", ""),
                                         font.label = list(size = 22, face = "bold"),
                                         nrow = 2, heights = c(0.04, 1))
user_and_event_specific_row
figure_three_top_two_rows <- ggarrange(user_and_event_specific_evaluation_top_row,
                                       NULL,
          user_and_event_specific_row, 
          ncol = 1,
          nrow = 3,
          heights = c(0.5, 0.01, 0.9))
figure_three_top_two_rows
ggsave(figure_three_top_two_rows,
       file = file.path(forecasting.covid_forecasts.out.dir,
                        "figure_three_top_two_rows.pdf"),
       h= 24, w = 24)



figure_three_bottom_right_pe_plots <- ggarrange(pe_acf_plot + scale_x_continuous(breaks = seq(0, 12, 2)) + scale_y_continuous(breaks = seq( -0.5, 1.0, 0.25)), ccf_rt_cases_across_locations, nrow = 2,
                                                heights = c(0.75, 1.0))
figure_three_bottom_right_pe_plots <- 
  ggarrange(NULL, figure_three_bottom_right_pe_plots,
            labels = c("D.2", ""),
            font.label = list(size = 18),
            heights = c(0.05, 1.0),
            nrow = 2, ncol = 1)

# figure_three_bottom_left_pe_plots <- 
#   ggarrange(rwis_boxplot_by_model_and_predictability_across_horizons + rotate_x_axis_theme,
#           rpinball_loss_boxplot_by_model_and_predictability_across_horizons_single_quantile + rotate_x_axis_theme,
#           r2_boxplot_by_model_and_predictability_across_horizons + rotate_x_axis_theme,
#           coverage_boxplot_by_model_and_predictability_across_horizons + rotate_x_axis_theme,
#           nrow = 2, ncol = 2,
#           labels = c("", ""),
#           font.label = list(size = 18),
#           common.legend= TRUE,
#           legend = "bottom")
# 
# figure_three_bottom_left_pe_plots
figure_three_bottom_left_pe_plots <- ggarrange(rwis_predictability_boxplot,
                                               pinball_loss_predictability_boxplot,
                                               r2_predictability_boxplot, interval_coverage_predictability_boxplot,
                                               align = "v",
                                               nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")
figure_three_bottom_left_pe_plots <- 
  ggarrange(NULL, figure_three_bottom_left_pe_plots,
            labels = c("D.1: Predictability vs forecast metrics", ""),
            font.label = list(size = 18),
            hjust = -0.2,
            heights = c(0.05, 1.0),
            nrow = 2, ncol = 1)
figure_three_bottom_left_pe_plots


figure_three_bottom_right_pe_plots <- 
  ggarrange(NULL, acf_ccf_plot,
            hjust = -0.2,
            labels = c("D.2: Predictability vs epidemic dynamics", ""),
            font.label = list(size = 18),
            heights = c(0.05, 1.0),
            nrow = 2, ncol = 1)

figure_three_bottom_row <- ggarrange(figure_three_bottom_left_pe_plots, NULL, figure_three_bottom_right_pe_plots, nrow = 1,
          widths = c(0.7, 0.1,0.4))

figure_three_bottom_row <- ggarrange(NULL,figure_three_bottom_row,
                                     heights = c(0.1, 1.0),
                                     nrow = 2,
                                     labels = c("D) Predictability analyses", ""),
                                     font.label = list(size = 24, face = "bold"))
figure_three_bottom_row <- ggarrange(NULL, figure_three_bottom_row,
                                     NULL,
                                     nrow = 1, ncol = 3,
                                     widths = c(0.06, 0.7, 0.06))


figure_three_complete <- ggarrange(figure_three_top_two_rows,
          figure_three_bottom_row,
          heights = c(1.095, 0.43),
          ncol = 1,
          nrow = 2)
ggsave(figure_three_complete,
       file = file.path(forecasting.covid_forecasts.out.dir,
                        "figure_three_final.pdf"),
       h= 26, w = 23)

#

