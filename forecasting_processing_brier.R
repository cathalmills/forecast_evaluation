




















#REAL-TIME ANALYSES ----
state_1_extreme_before_wave_brier_decomposition_plot_list <- readRDS(file.path(forecasting.covid_forecasts.local.dir,
                                                                     "state_1_extreme_before_wave_brier_decomposition_plot_list.RDS"))
state_1_extreme_before_wave_brier_decomposition_plot_list


#State 1

state_1_extreme_before_wave_brier_decomposition_plot <- ggarrange(NULL, NULL, NULL, NULL, NULL,
                                                                  state_1_extreme_before_wave_brier_decomposition_plot_list[[1]] + theme(axis.text.x = element_text(size=16)), NULL ,
                                                                  state_1_extreme_before_wave_brier_decomposition_plot_list[[2]] + theme(axis.text.x = element_text(size=16)),
                                                                  ncol = 4,nrow = 2,
                                                                  labels = c("1 week ahead (Brier score)", "","4 week ahead (Brier score)", ""),
                                                                  legend = "none",
                                                                  font.label = list(size = 18),
                                                                  widths = c(0.1, 0.75, 0.1,0.75),
                                                                  heights = c(0.1, 0.9))

state_1_extreme_before_wave_brier_decomposition_plot

state_1_extreme_during_wave_brier_decomposition_plot_list <- readRDS(file.path(forecasting.covid_forecasts.local.dir,
                                                                               "state_1_extreme_during_wave_brier_decomposition_plot_list.RDS"))

state_1_extreme_during_wave_brier_decomposition_plot <- ggarrange(NULL, NULL, NULL, NULL, NULL,
                                                                  state_1_extreme_during_wave_brier_decomposition_plot_list[[1]] + theme(axis.text.x = element_text(size=16))+guides(color = guide_legend(ncol = 1)), NULL ,
                                                                  state_1_extreme_during_wave_brier_decomposition_plot_list[[2]] + theme(axis.text.x = element_text(size=16))+guides(color = guide_legend(ncol = 1)),
                                                                  ncol = 4,nrow = 2,
                                                                  labels = c("1 week ahead \n(Brier score)", "","4 week ahead \n(Brier score)", ""),
                                                                  legend = "right",
                                                                  common.legend = TRUE,
                                                                  font.label = list(size = 18),
                                                                  widths = c(0.1, 0.75, 0.1,0.75),
                                                                  heights = c(0.175, 0.9))

state_1_extreme_during_wave_brier_decomposition_plot












state_1_extreme_before_wave_brier_murphy_plot_list <- readRDS(file.path(forecasting.covid_forecasts.local.dir,
                                                                               "state_1_extreme_before_wave_brier_murphy_plot_list.RDS"))
state_1_extreme_before_wave_brier_murphy_plot_list


#State 1

state_1_extreme_before_wave_brier_murphy_plot <- ggarrange(NULL, NULL, NULL, NULL, NULL,
                                                                  state_1_extreme_before_wave_brier_murphy_plot_list[[1]] + theme(axis.text.x = element_text(size=16)), NULL ,
                                                                  state_1_extreme_before_wave_brier_murphy_plot_list[[2]] + theme(axis.text.x = element_text(size=16)),
                                                                  ncol = 4,nrow = 2,
                                                                  labels = c("1 week ahead (Brier score)", "","4 week ahead (Brier score)", ""),
                                                                  legend = "none",
                                                                  font.label = list(size = 18),
                                                                  widths = c(0.1, 0.75, 0.1,0.75),
                                                                  heights = c(0.1, 0.9))

state_1_extreme_before_wave_brier_murphy_plot

state_1_extreme_during_wave_brier_murphy_plot_list <- readRDS(file.path(forecasting.covid_forecasts.local.dir,
                                                                               "state_1_extreme_during_wave_brier_murphy_plot_list.RDS"))

state_1_extreme_during_wave_brier_murphy_plot <- ggarrange(NULL, NULL, NULL, NULL, NULL,
                                                                  state_1_extreme_during_wave_brier_murphy_plot_list[[1]] + theme(axis.text.x = element_text(size=16))+
                                                             scale_y_continuous(labels = label_number(accuracy = 0.01),
                                                                                breaks = seq(0, 0.3, 0.05)), NULL ,
                                                                  state_1_extreme_during_wave_brier_murphy_plot_list[[2]] + 
                                                             scale_y_continuous(labels = label_number(accuracy = 0.01),
                                                                                breaks = seq(0, 0.3, 0.05))+
                                                             theme(axis.text.x = element_text(size=16)),
                                                                  ncol = 4,nrow = 2,
                                                                  labels = c("1 week ahead \n(Event-specific Murphy diagram)", "", "","4 week ahead  \n(Event-specific Murphy diagram)"),
                                                                  legend = "none",
                                                           hjust = -0.15,
                                                                  # common.legend = TRUE,
                                                                  font.label = list(size = 18),
                                                                  widths = c(0.1, 0.75, 0.175,0.75),
                                                                  heights = c(0.175, 0.9))
state_1_extreme_during_wave_brier_murphy_plot
scale_y_continuous(labels = label_number(accuracy = 0.01),
                   breaks = seq(0, 0.3, 0.05))





#FIXED EVENT ----
state_1_fixed_before_wave_brier_decomposition_plot_list <- readRDS(file.path(forecasting.covid_forecasts.local.dir,
                                                                               "state_1_fixed_before_wave_brier_decomposition_plot_list.RDS"))
state_1_fixed_before_wave_brier_decomposition_plot_list


#State 1

state_1_fixed_before_wave_brier_decomposition_plot <- ggarrange(NULL, 
                                                                state_1_fixed_before_wave_brier_decomposition_plot_list[[1]] + theme(axis.text.x = element_text(size=16)), NULL ,
                                                                state_1_fixed_before_wave_brier_decomposition_plot_list[[2]] + theme(axis.text.x = element_text(size=16)),
                                                                  nrow = 4,ncol = 1,
                                                                  labels = c("1 week ahead \n  (Brier score)", "","4 week ahead \n  (Brier score)", ""),
                                                                  legend = "none",
                                                                  font.label = list(size = 18),
                                                                  heights = c(0.15, 0.75, 0.175,0.75))

state_1_fixed_before_wave_brier_decomposition_plot









state_1_fixed_before_wave_brier_murphy_plot_list <- readRDS(file.path(forecasting.covid_forecasts.local.dir,
                                                                        "state_1_fixed_before_wave_brier_murphy_plot_list.RDS"))
state_1_fixed_before_wave_brier_murphy_plot_list


#State 1

state_1_fixed_before_wave_brier_murphy_plot <- ggarrange(NULL, NULL, NULL, NULL, NULL,
                                                           state_1_fixed_before_wave_brier_murphy_plot_list[[1]] + theme(axis.text.x = element_text(size=16)), NULL ,
                                                           state_1_fixed_before_wave_brier_murphy_plot_list[[2]] + theme(axis.text.x = element_text(size=16)),
                                                           ncol = 4,nrow = 2,
                                                           labels = c("1 week ahead (Brier score)", "","4 week ahead (Brier score)", ""),
                                                           legend = "none",
                                                           font.label = list(size = 18),
                                                           widths = c(0.1, 0.75, 0.1,0.75),
                                                           heights = c(0.1, 0.9))

state_1_fixed_before_wave_brier_murphy_plot


# 
# 
# 
# figure_four_bottom_row <- ggarrange(state_1_extreme_during_wave_brier_murphy_plot + theme(legend.position = "none"),
#           NULL,
#           state_1_extreme_during_wave_brier_decomposition_plot + theme(legend.position = "bottom"), legend = "bottom",
#           common.legend = TRUE,
#           widths = c(0.5, 0.1,0.75), nrow = 1)
# figure_four_bottom_row
# 
# 






state_1_fixed_before_wave_brier_murphy_plot <- ggarrange(NULL, NULL, NULL, NULL, NULL,
                                                         state_1_fixed_before_wave_brier_murphy_plot_list[[1]] + theme(axis.text.x = element_text(size=16)), 
                                                         NULL ,
                                                         state_1_fixed_before_wave_brier_murphy_plot_list[[2]] + theme(axis.text.x = element_text(size=16)),
                                                         ncol = 8,nrow = 2,
                                                         labels = c("1 week ahead (Brier score)", "","4 week ahead (Brier score)", ""),
                                                         legend = "none",
                                                         font.label = list(size = 18),
                                                         widths = c(0.1, 0.75, 0.1,0.75),
                                                         heights = c(0.1, 0.9))
state_1_fixed_before_wave_brier_murphy_plot

# state_1_extreme_during_wave_brier_murphy_plot_list[[1]] + theme(axis.text.x = element_text(size=16))+ state_1_extreme_during_wave_brier_murphy_plot_list[[2]] + theme(axis.text.x = element_text(size=16))))
#   
# (state_1_extreme_during_wave_brier_murphy_plot_list[[1]]+ theme(legend.position = "bottom")+
#     theme(axis.text.x = element_text(size=16))|state_1_extreme_during_wave_brier_murphy_plot_list[[2]]+theme(axis.text.x = element_text(size=16))+ theme(legend.position = "bottom"))+
#   plot_layout(guides = "auto") + theme(legend.position = 'bottom')
# 
















#During wave: Murphy
state_1_fixed_during_wave_brier_decomposition_plot_list <- readRDS(file.path(forecasting.covid_forecasts.local.dir,
                                                                             "state_1_fixed_during_wave_brier_decomposition_plot_list.RDS"))

state_1_fixed_during_wave_brier_decomposition_plot <- ggarrange(NULL, 
                                                                state_1_fixed_during_wave_brier_decomposition_plot_list[[1]] + theme(axis.text.x = element_text(size=16)), 
                                                                NULL ,
                                                                state_1_fixed_during_wave_brier_decomposition_plot_list[[2]] + theme(axis.text.x = element_text(size=16)),
                                                                nrow = 4,ncol = 1,
                                                                labels = c("1 week ahead \n  (Brier score)", "","4 week ahead \n  (Brier score)", ""),
                                                                legend = "none",
                                                                vjust = 1.2,
                                                                font.label = list(size = 18),
                                                                heights = c(0.175, 0.75, 0.175,0.75))

state_1_fixed_during_wave_brier_decomposition_plot


#During wave: Murphy
state_1_fixed_during_wave_brier_murphy_plot_list <- readRDS(file.path(forecasting.covid_forecasts.local.dir,
                                                                      "state_1_fixed_during_wave_brier_murphy_plot_list.RDS"))

state_1_fixed_during_wave_brier_murphy_plot <- ggarrange(NULL, 
                                                         state_1_fixed_during_wave_brier_murphy_plot_list[[1]] +
                                                           theme(axis.text.x = element_text(size=16)), NULL ,
                                                         state_1_fixed_during_wave_brier_murphy_plot_list[[2]] + 
                                                           scale_y_continuous(labels = label_number(accuracy = 0.01))+
                                                           theme(axis.text.x = element_text(size=16)),
                                                         nrow = 4,ncol = 1,
                                                         labels = c("1 week ahead \n(Event-specific Murphy diagram)", "","4 week ahead \n(Event-specific Murphy diagram)", ""),
                                                         legend = "none",
                                                         vjust = 1.2,
                                                         font.label = list(size = 18),
                                                         heights = c(0.175, 0.75, 0.175,0.75))

state_1_fixed_during_wave_brier_murphy_plot