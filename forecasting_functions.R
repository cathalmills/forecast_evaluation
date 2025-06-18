require(fs)
require(remotes)
# install_github("tidyverse/dplyr", lib = dev_lib, upgrade = "always")
# install.packages("triptych")

# remotes::install_github("epiforecasts/scoringutils", 
#                         lib = dev_lib,
#                         upgrade = "always",
#                         dependencies = TRUE)
dev_lib <- path_home_r("alternative_packages/")
dev_lib
library("scoringutils",
        lib.loc = dev_lib)
install.packages("triptych_0.1.3.tar.gz", repos = NULL, type = "source")
install.packages(file.path("alternative_packages/triptych_0.1.3.zip"), repos = NULL, type = "source")
head(state_level_covid_forecasts_dt)
models_dt
pointwise_metrics_quantile_predictions <- function(models_dt){
  models_dt <- data.table(models_dt) #Ensure it's a data.table
  results_dt <- models_dt[which(quantile_level == 0.5), 
                          list(r2 = caret::R2(predicted, observed),
                               rmse = caret::RMSE(predicted, observed),
                               mae = caret::MAE(predicted, observed)),
                          by = c("model", "target_type")]
  return(results_dt)
}
rotate_x_axis_theme <- theme(axis.text.x = element_text(size=18,
                                                        angle = 45, hjust = 1))



# install.packages("packrat")
# packrat::disable()
# packrat::init("~/alternative_packages")
# install.packages("renv")
# renv::activate()
# renv::restore() # install dependencies
# renv::status() # check environment


#Use this after finished mobility:
#Add in scoringutils, have been using version 2.0 for this project ()
# required_packages <- 
#   c("ggplot2", "data.table",
#     "scoringutils", "scoringRules", "statcomp",
#     "pdc", "entropy", "runner", "dplyr")
required_packages <- 
  c("ggplot2", "data.table",
    "scoringRules", "statcomp", "extraDistr",
    "bmixture", "EnvStats", "evd",
    "pdc", "entropy", "philentropy","runner", "dplyr")

options(repos = c(CRAN = "http://cran.us.r-project.org"))
# Function to check and install packages
suppressMessages(
  for (package in required_packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, repos = "http://cran.us.r-project.org")
    }
    library(package, character.only = TRUE)
  }
)


#Set up COVID + Dengue s.t weekly (target_end_date), location

#Dengue
#Detailed communication + understanding of proper scoring rules
# 1) Scoring Rule Decomposition
all_time_points_location_specific_forecast_evaluation_function <- function(x,
                                                         forecast_representation){
  setkeyv(x, c("target_end_date", "target_type", "model"))
  locations <- unique(x$location)
  all_scores_all_locations_summary <- NULL
  for(i in 1:length(locations)){
    print(paste0("Location ", i))
    location_in_q <- locations[i]
    location_forecast_data <- subset(x,
                                        location == location_in_q)
  #Forecast skill across all time points + all locations
  if(forecast_representation == "quantile"){
    models_dt <- as_forecast_quantile(location_forecast_data,
                                      observed = "observed",
                                      predicted = "predicted",
                                      model = "model",
                                      quantile_level = "quantile_level",
                                      forecast_unit = c("model", "location", "target_end_date",
                                                        "target_type", "horizon"))
    tmp_all_scores_all_locations_summary <- models_dt %>%
      score() %>%
      get_pairwise_comparisons(by = "target_type",
                               compare = "model",
                               metric = "wis",
                               baseline = "COVIDhub-baseline")
    tmp_all_scores_all_locations_summary <- data.table(tmp_all_scores_all_locations_summary)
    tmp_all_scores_all_locations_summary[, location:= location_in_q]
    all_scores_all_locations_summary <- 
      rbind(all_scores_all_locations_summary,
            tmp_all_scores_all_locations_summary)
  }
  else if(forecast_representation == "sample"){
    models_dt <- as_forecast_sample(target_type_forecast_data,
                                    observed = "observed",
                                    predicted = "predicted",
                                    model = "model",
                                    sample_id = "sampled_id",
                                    forecast_unit = c("model", "location", "target_end_date",
                                                      "target_type", "horizon"))
  }
  }
  
  
  return(all_scores_all_locations_summary)
}
plot_aggregate_wis_function <- function(location_aggregate_wis){
  location_aggregate_wis$model <- factor(location_aggregate_wis$model,
                                                            levels = covid_model_levels)
  location_aggregate_wis <- 
    subset(location_aggregate_wis,
           compare_against == "COVIDhub-baseline")
  visualisation_location_aggregate_wis <- 
    subset(location_aggregate_wis,
           target_type %in% covid_visualisation_target_types)
  
  location_aggregate_wis_plot <- ggplot(visualisation_location_aggregate_wis)+
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
  return(location_aggregate_wis_plot)
}
all_time_points_forecast_evaluation_function <- function(x,
                                                         forecast_representation){
  setkeyv(x, c("target_end_date", "target_type", "model"))
  #Forecast skill across all time points + all locations
  if(forecast_representation == "quantile"){
    models_dt <- as_forecast_quantile(x,
                                      observed = "observed",
                                      predicted = "predicted",
                                      model = "model",
                                      quantile_level = "quantile_level",
                                      forecast_unit = c("model", "location", "target_end_date",
                                                        "target_type", "horizon"))
    all_scores_all_locations_summary <- models_dt %>%
      score() %>%
      get_pairwise_comparisons(by = c("target_type"),
                               compare = "model",
                               metric = "wis",
                               baseline = "COVIDhub-baseline")
  }
  else if(forecast_representation == "sample"){
    models_dt <- as_forecast_sample(x,
                                    observed = "observed",
                                    predicted = "predicted",
                                    model = "model",
                                    sample_id = "sampled_id",
                                    forecast_unit = c("model", "location", "target_end_date",
                                                      "target_type", "horizon"))
  }
  

  return(all_scores_all_locations_summary)
}



#Pinball loss
quantile_score <- function(y_true, y_pred, alpha) {
  return((1 * (y_true < y_pred) - alpha) * (y_pred - y_true))
}

elementary_quantile_score <- function(y_true, y_pred, theta, alpha) {
  ((y_true < y_pred) - alpha) * ((theta < y_pred) - (theta < y_true))
}

get_thetas <- function(predicted, observed, n = 1001) {
  tmp <- c(predicted, observed)
  max_val <- max(tmp)
  min_val <- min(tmp)
  thetas <- seq(
    from = min_val - 0.01 * (max_val - min_val),
    to = max_val + 0.01 * (max_val - min_val),
    length.out = n
  )
  return(thetas)
}

get_elementary_scores <- function(observed, predicted, quantile_level, n) {
  thetas <- get_thetas(predicted, observed, n)
  
  scores <- sapply(
    thetas,
    function(t) mean(elementary_quantile_score(observed, predicted, t, quantile_level))
  )
  
  return(data.frame(theta = thetas, mean_score = scores))
}

quantile_score <- function(y_true, y_pred, alpha) {
  return((1 * (y_true < y_pred) - alpha) * (y_pred - y_true))
}
murphydiag <- function(data, digits = 1) {
  score_label <- data %>%
    mutate(qs = quantile_score(observed, predicted, quantile_level)) %>%
    group_by(model, quantile_level) %>%
    summarize(
      mean_qs = mean(qs),
      label = paste0(unique(model), " (", format(round(mean_qs, digits = digits), nsmall = 1),
                     ")",
                     collapse = "\n"
      ),
      .groups = "drop"
    )
  
  df <- data %>%
    group_by(model, quantile_level) %>%
    summarize(get_elementary_scores(observed, predicted, quantile_level, n = 500), .groups = "drop")
  
  df <- df %>%
    left_join(score_label, by = c("model", "quantile_level"))
  
  return(df)
}

plot_murphy_diagram <- function(df) {
  murphy_plot <- ggplot(df) +
    geom_line(aes(x = theta, y = mean_score, color = model), size = 0.5) +
    facet_wrap("quantile_level", scales = "free") +
    xlab(expression(paste("Threshold ", theta))) +
    ylab("Mean elementary score") +
    theme_bw(base_size = 11) +
    theme(
      legend.title = element_text(size = 6, face = "bold"),
      legend.text = element_text(size = 6),
      legend.title.align = 0,
      legend.text.align = 0,
      legend.key.size = unit(0.2, "lines"),
      legend.background = element_blank(),
      panel.grid.major = element_line(size = 0.05),
      panel.grid.minor = element_line(size = 0.05),
      aspect.ratio = 1
    )
  return(murphy_plot)
}


#Reliability Functions ----
reldiag <- function(x, y, alpha = 0.5, resampling = TRUE, n_resamples = 99,
                    region_level = 0.9, resample_log = FALSE, digits = 2) {
  pava <- function(x, y) {
    # In case of ties, isotone::gpava uses the conditional mean instead of quantile, try e.g.,
    # gpava(c(-1,-1,-1),c(-1,0,0),solver = weighted.median,ties = "secondary")
    # New fix: Use ranking of predictor values and break ties by ordering the
    # corresponding instances in order of decreasing observations
    ranking <- match(1:length(x), order(x, y, decreasing = c(FALSE, TRUE)))
    gpava(ranking, y, solver = weighted.fractile, p = alpha, ties = "secondary")$x
  }
  score <- function(x, y) mean((as.numeric(x >= y) - alpha) * (x - y))
  marg <- function(x) quantile(x, alpha, type = 1)
  identif <- function(x, y) as.numeric(x > y) - alpha
  
  ord_x <- order(x)
  x <- x[ord_x]
  y <- y[ord_x]
  
  x_rc <- pava(x, y)
  
  res <- y - x
  
  s <- score(x, y)
  c_rc_ucond <- optim(par = 0, fn = function(c) score(x + c, y),
                      method = "Brent", lower = min(res), upper = max(res))$par
  s_rc_ucond <- score(x + c_rc_ucond, y)
  s_rc <- score(x_rc, y)
  s_mg <- score(marg(y), y)
  
  mcb <- s - s_rc
  umcb <- s - s_rc_ucond
  cmcb <- s_rc_ucond - s_rc
  dsc <- s_mg - s_rc
  unc <- s_mg
  
  # The Score is exactly equal to uMCB + cMCB - DSC + UNC.
  # However, when rounding the values there may be slight discrepancies between the rounded values.
  # We avoid this for didactic reasons by computing the score from the rounded values.
  s <- sum(round(c(umcb, cmcb, -dsc, unc), digits))
  
  # test: mean identification zero? (t-test)
  # v = identif(x,y)
  # t = sqrt(length(v)) * mean(v)/sd(v)
  # pval_ucond = 1 - abs(pt(t,length(v)-1) - 0.5)*2
  
  # Unconditional calibration test
  # Coverage test: One-sided Binomial tests with Bonferroni correction
  eps <- 10^-10 # avoid numerical artifacts by assuming that values with an
  # absolute difference of less than eps are identical
  hard_cov <- sum(y < x - eps)
  soft_cov <- sum(y < x + eps)
  
  pval_hard <- dbinom(hard_cov, length(y), alpha) + pbinom(hard_cov, length(y), alpha, FALSE)
  pval_soft <- pbinom(soft_cov, size = length(y), prob = alpha)
  pval_ucond <- min(pval_hard, pval_soft, 0.5) * 2
  # print(paste0("p-Values: hard ",pval_hard,", soft ",pval_soft))
  
  if (resampling) {
    n_samples <- n_resamples + 1 # total number of samples including observed sample
    low <- floor(n_samples * (1 - region_level) / 2)
    up <- n_samples - low
    pval_digits <- ceiling(log(n_samples, 10))
    
    if (resample_log) {
      res_log <- log(y) - log(x)
      resamples <- sapply(1:n_resamples, function(i) exp(log(x) + sample(res_log, length(y), replace = TRUE)))
    } else {
      resamples <- sapply(1:n_resamples, function(i) x + sample(res, length(y), replace = TRUE))
    }
    
    x_rc_resamples <- apply(resamples, 2, function(y) pava(x, y))
    x_rc_resamples_sorted <- apply(cbind(x_rc, x_rc_resamples), 1, sort) - marg(res)
    # includes observed values + bias corrected (shifted by mean residual)
    
    ran_x <- range(x)
    
    mcb_resamples <- sapply(1:n_resamples, function(i) score(x, resamples[, i]) - score(x_rc_resamples[, i], resamples[, i]))
    mcb_bounds <- sort(c(mcb, mcb_resamples))[c(low, up)]
    
    rank_obs <- tail(rank(c(mcb_resamples, mcb)), 1)
    pval <- 1 - (rank_obs - 1) / (n_resamples + 1)
    
    lower <- x_rc_resamples_sorted[low, ]
    upper <- x_rc_resamples_sorted[up, ]
  } else {
    lower <- NA
    upper <- NA
    pval <- NA
  }
  
  results <- data.frame(
    quantile = alpha, x = x, y = y, x_rc = x_rc,
    lower = lower, upper = upper,
    digits = digits, score = s,
    umcb = umcb, cmcb = cmcb, mcb = mcb, dsc = dsc, unc = unc,
    pval_cond = pval, pval_ucond = pval_ucond
  )
}
# 
# tmp <- subset(state_level_covid_forecasts_dt,
#               quantile_level == 0.025 & target_end_date <= "2021-07-17" 
#               & target_end_date >= "2021-05-01")
# 49-11
# tmp[, lagged_target_end_date:= lag(target_end_date)]
# setkeyv(tmp, c("target_end_date", "horizon", "model"))
# which(unique(state_level_covid_forecasts_dt$target_end_date) == "2021-07-17")
# unique(tmp$location)[47]
# tmp <- tmp[, length(predicted), by = c("model", "location", "horizon", "location_name")]


#Isolines from horizon-specific scores
get_isolines_from_scores <- function(scores,
                                     num_models){
  iso <- scores %>%
    group_by(quantile_level) %>%
    summarize(
      mcb_best = floor(min(mcb)),
      dsc_best = ceiling(max(dsc)),
      mcb_worst = ceiling(max(mcb)),
      dsc_worst = floor(min(dsc)),
      unc = unique(unc),
      score_best = mcb_best - dsc_best + unc,
      score_worst = mcb_worst - dsc_worst + unc,
      y_best = dsc_best - mcb_best,
      y_worst = dsc_worst - mcb_worst
    )
  iso <- data.table(iso)
  #Ensuring no duplicated rows
  iso <- unique(iso,
                by = "quantile_level")
  if(nrow(iso) == 0){
    output_iso <- NULL
  }
  else{
    output_iso <- iso %>%
    group_by(quantile_level) %>%
    summarize(
      intercept = seq(y_worst + unc %% 1, y_best + unc %% 1, by = round((score_worst - score_best) / num_models)
      ),
      slope = 1,
      unc = unique(unc),
      .groups = "drop"
    ) %>%
    mutate(
      score = (unc - intercept),
      label = score
    )
  }
  return(output_iso)
}

# tmp_iso <- get_isolines_from_scores(tmp_state_covid_quantile_scores,
#                                     num_covid_models)
# tmp_iso



plot_selected_pinball_decomposition <- function(results,
                                                chosen_quantiles,
                                                legend_on = NULL){
  #Pinball loss
  location_pinball_dt <- results[[1]]
  location_pinball_loss_plot_list <- list()
  for(j in 1:length(covid_visualisation_horizons)){
    horizon_in_q <-covid_visualisation_horizons[j]
    target_type_in_q <-covid_visualisation_target_types[j]
    
    chosen_horizon_scores <- subset(location_pinball_dt,
                                    horizon == horizon_in_q)
    chosen_horizon_scores <- subset(chosen_horizon_scores,
                                    quantile_level %in% chosen_quantiles)
    
    chosen_horizon_scores[, target_type:= target_type_in_q]
    chosen_horizon_scores$model <- factor(chosen_horizon_scores$model,
                                          levels = covid_model_levels)
    num_models <- length(unique(chosen_horizon_scores$model))
    chosen_horizon_isolines <- get_isolines_from_scores(chosen_horizon_scores,
                                                        num_models)
    chosen_horizon_isolines <- subset(chosen_horizon_isolines,
                                      quantile_level %in% chosen_quantiles)
    location_pinball_loss_plot_list[[j]] <- 
      plot_pinball_loss_decomposition(chosen_horizon_scores,
                                      chosen_horizon_isolines,
                                      corrected_covid_model_names,
                                      covid_model_colours,
                                      specific_quantiles = chosen_quantiles)
  }
  decomposition_diagram <- 
    ggarrange(NULL, location_pinball_loss_plot_list[[1]] + theme(axis.text.x = element_text(size=16, angle = 45, hjust = 1)), NULL ,
              location_pinball_loss_plot_list[[2]] + theme(axis.text.x = element_text(size=16, angle = 45, hjust = 1)),
              nrow = 4,ncol = 1,
              legend = "none",
              labels = c("1 week ahead (Pinball loss)", "","4 week ahead (Pinball loss)", ""),
              font.label = list(size = 18),
              heights = c(0.1, 0.75, 0.1,0.75))
  if(!is.null(legend_on)){
    decomposition_diagram <- 
      ggarrange(NULL, location_pinball_loss_plot_list[[1]] + theme(axis.text.x = element_text(size=16, angle = 45, hjust = 1)), NULL ,
                location_pinball_loss_plot_list[[2]] + theme(axis.text.x = element_text(size=16, angle = 45, hjust = 1)),
                nrow = 4,ncol = 1,
                legend = "bottom", common.legend = TRUE,
                labels = c("1 week ahead (Pinball loss)", "","4 week ahead (Pinball loss)", ""),
                font.label = list(size = 18),
                heights = c(0.1, 0.75, 0.1,0.75))
  }
  return(decomposition_diagram)
}

plot_selected_pinball_murphy_diagrams <- function(results,
                                                  chosen_quantiles = NULL,
                                                  restrict_murphy_event_axis_bounds = NULL){
  #results = output of pinball loss function
  #chosen_quantiles = if want to focus on given C/L ratio(s)
  
  #Can customise later for different horizons!
  pinball_loss_murphy_results_dt <- results[[3]]
  if(!is.null(chosen_quantiles)){
    pinball_loss_murphy_results_dt <- 
      subset(pinball_loss_murphy_results_dt,
             quantile_level %in% chosen_quantiles)
  }
  setkeyv(pinball_loss_murphy_results_dt, "model")
  pinball_loss_murphy_results_dt$model <- factor(pinball_loss_murphy_results_dt$model,
                                                            levels = covid_model_levels)

  
  visualisation_pinball_loss_murphy_results_dt <- 
    subset(pinball_loss_murphy_results_dt,
           horizon %in% covid_visualisation_horizons)
  
  visualisation_pinball_loss_murphy_results_dt <- 
    merge(visualisation_pinball_loss_murphy_results_dt,
          covid_target_types_horizons,
          by = "horizon")
  plot_list <- list()
  tmp <- visualisation_pinball_loss_murphy_results_dt[, list(max_theta_by_horizon = max(theta, na.rm=TRUE)), by = "horizon"]
  max_theta <- min(tmp$max_theta_by_horizon)
  for(i in 1:length(covid_visualisation_horizons)){
    horizon_in_q <-covid_visualisation_horizons[i]
    target_type_in_q <-covid_visualisation_target_types[i]
    
    tmp_visualisation_pinball_loss_murphy_results_dt <- 
      subset(visualisation_pinball_loss_murphy_results_dt,
             horizon == horizon_in_q & theta <= max_theta)
    tmp_visualisation_pinball_loss_murphy_results_dt[, alpha :=  1-as.numeric(as.character(quantile_level))]
    tmp_visualisation_pinball_loss_murphy_results_dt[, alpha_label := paste0("alpha==", alpha)]
    
    if(is.null(restrict_murphy_event_axis_bounds)){
      restrict_murphy_event_axis_bounds <- c(0, max(tmp_visualisation_pinball_loss_murphy_results_dt$theta))
    }
    
    plot_list[[i]] <- 
      ggplot(tmp_visualisation_pinball_loss_murphy_results_dt)+
      geom_line(aes(x = theta, y = mean_score, color = model), size = 0.5) +
      facet_wrap(alpha_label ~. , scales = "free_y",  labeller = label_parsed) +
      scale_y_continuous(labels = label_number(accuracy = 0.01))+
      xlab(expression(paste("Threshold ", theta))) +
      ylab(expression(paste("Mean s(", alpha, ", ", theta, ")")))+
      theme_bw(base_size = 11) +
      xlim(c(as.numeric(restrict_murphy_event_axis_bounds[1]),
             as.numeric(restrict_murphy_event_axis_bounds[2])))+
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
  murphy_diagrams <- 
    ggarrange(NULL, plot_list[[1]] + theme(axis.text.x = element_text(size=16, angle = 45, hjust = 1)), NULL ,
              plot_list[[2]] + theme(axis.text.x = element_text(size=16, angle = 45, hjust = 1)),
              nrow = 4,ncol = 1,
              legend = "none",
              labels = c("1 week ahead (User-specific Murphy diagram)", "","4 week ahead (User-specific Murphy diagram)", ""),
              font.label = list(size = 18),
              heights = c(0.1, 0.75, 0.1,0.75))
  # murphy_diagrams <- ggarrange(NULL,murphy_diagrams,
  #                                                      nrow = 2, ncol = 1, hjust = -0.5,
  #                                                      labels = c("C.1"), heights = c(0.06, 1.0))
  return(murphy_diagrams)
}
plot_pinball_loss_decomposition <- function(scores,
                                            isolines,
                                            plot_model_names,
                                            plot_model_colours,
                                            specific_quantiles = NULL){
  # scores <- scores %>% ungroup()
  scores <- data.table(scores)
  if(!is.null(specific_quantiles)){
    scores <- subset(scores,
                     quantile_level %in% specific_quantiles)
  }
  tmp_scores <- copy(scores)
  tmp_scores[, alpha :=  1-as.numeric(as.character(quantile_level))]
  tmp_scores[, alpha_label := paste0("alpha==", alpha)]
  
  # tmp_scores[, alpha := 1-quantile_level]
  # tmp_scores[, alpha_label := paste0("alpha==", alpha)]
  tmp_isolines <- data.table(copy(isolines))
  tmp_isolines[, alpha :=  1-as.numeric(as.character(quantile_level))]
  tmp_isolines[, alpha_label := paste0("alpha==", alpha)]
  
  specific_quantiles <- unique(tmp_scores$quantile_level) #For facet_wrap
  pinball_loss_decomposed_plot <- 
      ggplot(data = tmp_scores) +
      geom_abline(
        data = tmp_isolines, aes(intercept = intercept, slope = slope), color = "lightgray", alpha = 0.5,
        size = 0.5
      ) +
      geom_point(aes(x = mcb, y = dsc, color = model),
                 size = 5) +
    facet_wrap(alpha_label ~. ,  scales = "free", ncol = length(specific_quantiles),
               label = label_parsed) +
      xlab("MCB") +
      ylab("DSC") +
    theme_light()+
    theme_custom()+
      theme(
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size = 20),  # Adjust legend text size
        legend.title = element_text(size = 20), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
      ) +   scale_color_manual("Model", values = plot_model_colours,
                               labels = plot_model_names)
    
  return(pinball_loss_decomposed_plot)
}

# plot_pinball_loss_decomposition(tmp_state_covid_quantile_scores,
#                                 tmp_iso,
#                                 corrected_covid_model_names,
#                                 covid_model_colours)
# 
# tmp_horizon_specific_isolines <- 
#   get_isolines_from_scores(tmp_horizon_specific_quantile_scores)
# subsetted_state_covid_forecasts_dt
# tmp_horizon_specific_data <- subset(state_level_covid_forecasts_dt,
#                                     horizon == 1)
# 
# #i) Starting with all locations
# #Decomposition
# tmp_horizon_specific_reliability_results <- 
#   tmp_horizon_specific_data %>%
#   group_by(model, quantile_level) %>%
#   summarize(reldiag(predicted, observed, 
#                     alpha = unique(quantile_level), resampling = FALSE, digits = 1))
# tmp_horizon_specific_reliability_results
run_pinball_loss_decomposition_trailing_window_over_time <- 
  function(input_data, #data
           window_length = NULL,
           factor_model_levels,
           plot_model_names,
           plot_model_colours,
           focus_specific_quantiles = NULL,
           real_time = NULL,
           option_trailing_window_on = FALSE,
           min_time_for_aggregate_performance = NULL){
    #focus_specific_quantiles is an option to limit the number of quantiles 
    #real_time is an option to set what the current date is (evaluating all forecasts previous to that date)
    #option_trailing_window_on is an option to turn on/off additional trailing window evaluations
    if(!is.null(real_time)){
      input_data <- subset(input_data, forecast_date < real_time)
    }
    if(!is.null(focus_specific_quantiles)){
      input_data <- subset(input_data, quantile_level %in% focus_specific_quantiles)
    }
    setkeyv(input_data, c("target_end_date", "horizon", "model"))
    times <- unique(input_data$target_end_date)
    locations <- unique(input_data$location)
    runner_windows_list <- runner(times, k = window_length)
    #Max index is current time point
    #Sliding window with stride of 1 week
    runner_windows_list <- runner_windows_list[window_length:length(runner_windows_list)] 
    
    
    #Horizon-specific scores across all locations
    quantile_scores_by_forecast_horizon_all_locations <- NULL
    pinball_loss_all_locations_plot_by_forecast_horizon_list <- list()
    
    #Horizon-specific scores across for individual locations
    quantile_scores_by_forecast_horizon_individual_locations <- NULL
    pinball_loss_individual_locations_plot_by_forecast_horizon_list <- list()
    
    #Murphy diagram results + plot
    murphy_results_by_forecast_horizon_all_locations <- NULL
    murphy_plot_by_forecast_horizon_all_locations <- list()
    
    murphy_results_by_forecast_horizon_individual_locations <- NULL
    murphy_plot_by_forecast_horizon_individual_locations <- list()
    
    trailing_window_quantile_scores_by_forecast_horizon_all_locations <- NULL
    trailing_window_pinball_loss_all_locations_plot_by_forecast_horizon_list <- list()
    
    trailing_window_murphy_results_by_forecast_horizon_all_locations <- NULL
    trailing_window_murphy_plot_by_forecast_horizon_all_locations <- list()
    
    
    trailing_window_quantile_scores_by_forecast_horizon_individual_locations <- NULL
    trailing_window_pinball_loss_individual_locations_plot_by_forecast_horizon_list <- list()
    
    
    trailing_window_murphy_results_by_forecast_horizon_individual_locations <- NULL
    trailing_window_murphy_plot_by_forecast_horizon_individual_locations <- list()
    #
    
    #Uncomment: Start
    for(i in 1:length(unique(input_data$horizon))){
      horizon_specific_pinball_loss_individual_locations_plot_by_forecast_horizon_list <- list()
      horizon_specific_murphy_plot_by_forecast_horizon_individual_locations <- list()
      horizon_in_q <- unique(input_data$horizon)[i]
      print(paste0("Horizon:", horizon_in_q))

      tmp_horizon_specific_data <- subset(input_data,
                                          horizon == horizon_in_q)
      if(!is.null(min_time_for_aggregate_performance)){ #For trailing window, want aggregate in window
        tmp_horizon_specific_data <- subset(tmp_horizon_specific_data,
                                            target_end_date >= min_time_for_aggregate_performance)
      }
      #i) Starting with all locations
      #Decomposition
      tmp_horizon_specific_reliability_results <-
        tmp_horizon_specific_data %>%
        group_by(model, quantile_level) %>%
        summarize(reldiag(predicted, observed,
                        alpha = unique(quantile_level), resampling = FALSE, digits = 1))
      tmp_horizon_specific_quantile_scores <-
        tmp_horizon_specific_reliability_results %>%
        group_by(quantile_level, model) %>%
        distinct(across(score:pval_ucond))
      tmp_horizon_specific_quantile_scores$quantile_level <-
        as.factor(tmp_horizon_specific_quantile_scores$quantile_level)
      tmp_horizon_specific_quantile_scores$model <-
        factor(tmp_horizon_specific_quantile_scores$model,
                  levels = factor_model_levels)
      tmp_horizon_specific_quantile_scores <- data.table(tmp_horizon_specific_quantile_scores)
      tmp_horizon_specific_quantile_scores[, horizon:= horizon_in_q]

      #Number of models at horizon
      tmp_num_models <- length(unique(tmp_horizon_specific_quantile_scores$model))
      tmp_horizon_specific_isolines <-
        get_isolines_from_scores(tmp_horizon_specific_quantile_scores,
                                 tmp_num_models)
      #Store in data.table (e.g. if want to only plot specific quantiles/horizons after running function)
      quantile_scores_by_forecast_horizon_all_locations <-
        rbind(quantile_scores_by_forecast_horizon_all_locations,
              tmp_horizon_specific_quantile_scores)
    #   #Storing plot
      tmp_plot <- plot_pinball_loss_decomposition(tmp_horizon_specific_quantile_scores,
                                                  tmp_horizon_specific_isolines,
                                                  plot_model_names,
                                                  plot_model_colours)
      pinball_loss_all_locations_plot_by_forecast_horizon_list[[i]] <- tmp_plot



      #Murphy diagram
      tmp_murphy_results_by_forecast_horizon_all_locations <-
        murphydiag(tmp_horizon_specific_data)
      tmp_murphy_results_by_forecast_horizon_all_locations <-
        data.table(tmp_murphy_results_by_forecast_horizon_all_locations)
      tmp_murphy_results_by_forecast_horizon_all_locations[, horizon:= horizon_in_q]

      murphy_results_by_forecast_horizon_all_locations <-
        rbind(murphy_results_by_forecast_horizon_all_locations,
              tmp_murphy_results_by_forecast_horizon_all_locations)
      tmp_murphy_plot <-
        plot_murphy_diagram(tmp_murphy_results_by_forecast_horizon_all_locations)+
        scale_color_manual("Model", values = plot_model_colours,
                               labels = plot_model_names)
      murphy_plot_by_forecast_horizon_all_locations[[i]] <- tmp_murphy_plot

      #ii) Individual locations
      for(j in 1:length(unique(tmp_horizon_specific_data$location))){
        location_in_q <- unique(tmp_horizon_specific_data$location)[j]
        print(paste0("Location: ", location_in_q, ", Horizon:", horizon_in_q))

        #Subset horizon-specific data to an individual location
        individual_location_tmp_horizon_specific_data <-
          subset(tmp_horizon_specific_data,
                 location == location_in_q)
    #     #Get reliability results
        individual_location_tmp_horizon_specific_reliability_results <-
          individual_location_tmp_horizon_specific_data %>%
          group_by(model, quantile_level) %>%
          summarize(reldiag(predicted, observed,
                            alpha = unique(quantile_level), resampling = FALSE, digits = 1))
        individual_location_tmp_horizon_specific_quantile_scores <-
          individual_location_tmp_horizon_specific_reliability_results %>%
          group_by(quantile_level, model) %>%
          distinct(across(score:pval_ucond))
        individual_location_tmp_horizon_specific_quantile_scores <-
          data.table(individual_location_tmp_horizon_specific_quantile_scores)
        individual_location_tmp_horizon_specific_quantile_scores[, horizon:= horizon_in_q]
        individual_location_tmp_horizon_specific_quantile_scores[, location:= location_in_q]

        individual_location_tmp_horizon_specific_quantile_scores$quantile_level <-
          as.factor(individual_location_tmp_horizon_specific_quantile_scores$quantile_level)
        individual_location_tmp_horizon_specific_quantile_scores$model <-
          factor(individual_location_tmp_horizon_specific_quantile_scores$model,
                    levels = factor_model_levels)

        tmp_num_models <- length(unique(individual_location_tmp_horizon_specific_quantile_scores$model))

        individual_location_tmp_horizon_specific_isolines <-
          get_isolines_from_scores(individual_location_tmp_horizon_specific_quantile_scores,
                                   tmp_num_models)

        #Store in data.table (e.g. if want to only plot specific quantiles/horizons after running function)
        #This is for each location specifically
        quantile_scores_by_forecast_horizon_individual_locations <-
          rbind(quantile_scores_by_forecast_horizon_individual_locations,
                individual_location_tmp_horizon_specific_quantile_scores)


        #Storing plot for each horizon(i) and location(j)
        tmp_plot <- plot_pinball_loss_decomposition(individual_location_tmp_horizon_specific_quantile_scores,
                                                              individual_location_tmp_horizon_specific_isolines,
                                                              plot_model_names,
                                                              plot_model_colours)
        horizon_specific_pinball_loss_individual_locations_plot_by_forecast_horizon_list[[j]] <-
          tmp_plot

        #Murphy diagram
        tmp_murphy_results_by_forecast_horizon_individual_locations <-
          murphydiag(individual_location_tmp_horizon_specific_data)
        tmp_murphy_results_by_forecast_horizon_individual_locations <-
          data.table(tmp_murphy_results_by_forecast_horizon_individual_locations)
        tmp_murphy_results_by_forecast_horizon_individual_locations[, horizon:= horizon_in_q]
        tmp_murphy_results_by_forecast_horizon_individual_locations[, location:= location_in_q]

        murphy_results_by_forecast_horizon_individual_locations <-
          rbind(murphy_results_by_forecast_horizon_individual_locations,
                tmp_murphy_results_by_forecast_horizon_individual_locations)

        tmp_murphy_plot <-
          plot_murphy_diagram(tmp_murphy_results_by_forecast_horizon_individual_locations)+
          scale_color_manual("Model", values = plot_model_colours,
                             labels = plot_model_names)

        #Location j
        horizon_specific_murphy_plot_by_forecast_horizon_individual_locations[[j]] <-
          tmp_murphy_plot
      }
      pinball_loss_individual_locations_plot_by_forecast_horizon_list[[i]] <- horizon_specific_pinball_loss_individual_locations_plot_by_forecast_horizon_list
      murphy_plot_by_forecast_horizon_individual_locations[[i]] <- horizon_specific_murphy_plot_by_forecast_horizon_individual_locations
      }



    quantile_scores_by_forecast_horizon_all_locations <- data.table(quantile_scores_by_forecast_horizon_all_locations)
    #Uncomment: Start
    
    if(option_trailing_window_on == TRUE){
    #1st, computing scores across all locations in each rolling window
    
    trailing_window_horizon_specific_murphy_plot_by_forecast_horizon_individual_locations <- list()
    trailing_window_pinball_loss_individual_locations_plot_by_forecast_horizon_list <- list()
      
    trailing_window_quantile_scores_by_forecast_horizon_individual_locations <- NULL
    trailing_window_murphy_results_by_forecast_horizon_individual_locations <- NULL
    
    trailing_window_quantile_scores_by_forecast_horizon_individual_locations <- NULL
    trailing_window_murphy_results_by_forecast_horizon_individual_locations <- NULL

    for(k in seq(1, length(runner_windows_list), 1)){
      if(k%%50 == 0){gc()}
      times_in_q <- runner_windows_list[[k]]
      min_time_in_q <- min(as.Date(times_in_q))
      max_time_in_q <- max(as.Date(times_in_q))
      print(paste0("max_time: ", max_time_in_q))
      all_data_at_time_in_q <-
        subset(input_data,
               target_end_date <= max_time_in_q &
                 target_end_date >= min_time_in_q)
      
      #List for all locations' results
      window_specific_pinball_loss_plot_list <- list()
      window_specific_murphy_plot_list <- list()
      
      #List for individual locations' results
      single_horizon_trailing_window_horizon_specific_murphy_plot_by_forecast_horizon_individual_locations <- list()
      single_horizon_trailing_window_pinball_loss_individual_locations_plot_by_forecast_horizon_list <- list()
      
      for(h in 1:length(unique(all_data_at_time_in_q$horizon))){
        horizon_in_q <- unique(all_data_at_time_in_q$horizon)[h]
        tmp_horizon_specific_data <- subset(all_data_at_time_in_q, 
                                            horizon == horizon_in_q)
        tmp_horizon_specific_reliability_results <- 
        tmp_horizon_specific_data %>%
        group_by(model, quantile_level) %>%
        summarize(reldiag(predicted, observed, 
                          alpha = unique(quantile_level), resampling = FALSE, digits = 1))
        tmp_horizon_specific_quantile_scores <- 
          tmp_horizon_specific_reliability_results %>%
          group_by(quantile_level, model) %>%
          distinct(across(score:pval_ucond))
        tmp_horizon_specific_quantile_scores$quantile_level <- 
          as.factor(tmp_horizon_specific_quantile_scores$quantile_level)
        tmp_horizon_specific_quantile_scores$model <- 
          factor(tmp_horizon_specific_quantile_scores$model,
                    levels = factor_model_levels)
        tmp_num_models <- length(unique(tmp_horizon_specific_quantile_scores$model))
        
        #Isolines for plot (for time, turning off, can plot afterwards!)
        # tmp_horizon_specific_isolines <- 
        #   get_isolines_from_scores(tmp_horizon_specific_quantile_scores,
        #                            tmp_num_models)
        tmp_horizon_specific_quantile_scores <- data.table(tmp_horizon_specific_quantile_scores)
        tmp_horizon_specific_quantile_scores[, min_time:= min_time_in_q]
        tmp_horizon_specific_quantile_scores[, max_time:= max_time_in_q]
        tmp_horizon_specific_quantile_scores[, horizon:= horizon_in_q]
        
      #Store in data.table (e.g. if want to only plot specific quantiles/horizons after running function)
      trailing_window_quantile_scores_by_forecast_horizon_all_locations <- 
        rbind(trailing_window_quantile_scores_by_forecast_horizon_all_locations,
              tmp_horizon_specific_quantile_scores)
      
      #Storing plot (for time, turning off, can plot afterwards!)
      # window_specific_pinball_loss_plot_list[[h]] <- plot_pinball_loss_decomposition(tmp_horizon_specific_quantile_scores,
      #                                                                                        tmp_horizon_specific_isolines,
      #                                                                                        plot_model_names,
      #                                                                                        plot_model_colours)

      
      
      tmp_trailing_window_murphy_results_by_forecast_horizon_all_locations <- 
        murphydiag(tmp_horizon_specific_data)
      tmp_trailing_window_murphy_results_by_forecast_horizon_all_locations <- 
        data.table(tmp_trailing_window_murphy_results_by_forecast_horizon_all_locations)
      tmp_trailing_window_murphy_results_by_forecast_horizon_all_locations[, horizon:= horizon_in_q]
      tmp_trailing_window_murphy_results_by_forecast_horizon_all_locations[, max_time:= max_time_in_q]
      tmp_trailing_window_murphy_results_by_forecast_horizon_all_locations[, min_time:= min_time_in_q]
      
      trailing_window_murphy_results_by_forecast_horizon_all_locations <- 
        rbind(trailing_window_murphy_results_by_forecast_horizon_all_locations,
              tmp_trailing_window_murphy_results_by_forecast_horizon_all_locations)
      
      tmp_murphy_plot <- 
        plot_murphy_diagram(tmp_trailing_window_murphy_results_by_forecast_horizon_all_locations)+
        scale_color_manual("Model", values = plot_model_colours,
                           labels = plot_model_names)
      
      window_specific_murphy_plot_list[[h]] <- 
        tmp_murphy_plot
      
      tmp_trailing_window_horizon_specific_pinball_loss_plot_by_forecast_horizon_individual_locations <-
        list()
      tmp_trailing_window_horizon_specific_murphy_plot_by_forecast_horizon_individual_locations <-
        list()
      for(j in 1:length(unique(tmp_horizon_specific_data$location))){
        location_in_q <- unique(tmp_horizon_specific_data$location)[j]
        # print(paste0("Location: ", location_in_q, ", Horizon:", horizon_in_q))
        
        #Subset horizon-specific data to an individual location
        individual_location_tmp_horizon_specific_data <-
          subset(tmp_horizon_specific_data,
                 location == location_in_q)
        #Get reliability results
        individual_location_tmp_horizon_specific_reliability_results <- 
          individual_location_tmp_horizon_specific_data %>%
          group_by(model, quantile_level) %>%
          summarize(reldiag(predicted, observed, 
                            alpha = unique(quantile_level), resampling = FALSE, digits = 1))
        individual_location_tmp_horizon_specific_quantile_scores <- 
          individual_location_tmp_horizon_specific_reliability_results %>%
          group_by(quantile_level, model) %>%
          distinct(across(score:pval_ucond))
        individual_location_tmp_horizon_specific_quantile_scores <- 
          data.table(individual_location_tmp_horizon_specific_quantile_scores)
        individual_location_tmp_horizon_specific_quantile_scores[, horizon:= horizon_in_q]
        individual_location_tmp_horizon_specific_quantile_scores[, location:= location_in_q]
        

        
        individual_location_tmp_horizon_specific_quantile_scores$quantile_level <- 
          as.factor(individual_location_tmp_horizon_specific_quantile_scores$quantile_level)
        individual_location_tmp_horizon_specific_quantile_scores$model <- 
          factor(individual_location_tmp_horizon_specific_quantile_scores$model,
                 levels = factor_model_levels)
        tmp_num_models <- length(unique(individual_location_tmp_horizon_specific_quantile_scores$model))
        
        # individual_location_tmp_horizon_specific_isolines <- 
        #   get_isolines_from_scores(individual_location_tmp_horizon_specific_quantile_scores,
        #                            tmp_num_models)
        
        #Store in data.table (e.g. if want to only plot specific quantiles/horizons after running function)
        #This is for each location specifically
        trailing_window_quantile_scores_by_forecast_horizon_individual_locations <- 
          rbind(trailing_window_quantile_scores_by_forecast_horizon_individual_locations,
                individual_location_tmp_horizon_specific_quantile_scores)
        
        
        #Storing plot for each horizon(i) and location(j) (if needed, can do after instead!)
        # tmp_plot <- plot_pinball_loss_decomposition(individual_location_tmp_horizon_specific_quantile_scores,
        #                                             individual_location_tmp_horizon_specific_isolines,
        #                                             plot_model_names,
        #                                             plot_model_colours)
        # tmp_trailing_window_horizon_specific_pinball_loss_plot_by_forecast_horizon_individual_locations[[j]] <- 
        #   tmp_plot
        
        #Murphy diagram        
        tmp_murphy_results_by_forecast_horizon_individual_locations <- 
          murphydiag(individual_location_tmp_horizon_specific_data)
        tmp_murphy_results_by_forecast_horizon_individual_locations <- 
          data.table(tmp_murphy_results_by_forecast_horizon_individual_locations)
        tmp_murphy_results_by_forecast_horizon_individual_locations[, horizon:= horizon_in_q]
        tmp_murphy_results_by_forecast_horizon_individual_locations[, location:= location_in_q]
        
        
        trailing_window_murphy_results_by_forecast_horizon_individual_locations <- 
          rbind(trailing_window_murphy_results_by_forecast_horizon_individual_locations,
                tmp_murphy_results_by_forecast_horizon_individual_locations)
        
        tmp_murphy_plot <- 
          plot_murphy_diagram(tmp_murphy_results_by_forecast_horizon_individual_locations)+
          scale_color_manual("Model", values = plot_model_colours,
                             labels = plot_model_names)
        
        #Location j
        tmp_trailing_window_horizon_specific_murphy_plot_by_forecast_horizon_individual_locations[[j]] <- 
          tmp_murphy_plot
      }
      single_horizon_trailing_window_horizon_specific_murphy_plot_by_forecast_horizon_individual_locations[[h]] <-
        tmp_trailing_window_horizon_specific_murphy_plot_by_forecast_horizon_individual_locations
      single_horizon_trailing_window_pinball_loss_individual_locations_plot_by_forecast_horizon_list[[h]] <-
        tmp_trailing_window_horizon_specific_pinball_loss_plot_by_forecast_horizon_individual_locations
      #k = window
      #h = horizon
      }
      
      trailing_window_pinball_loss_all_locations_plot_by_forecast_horizon_list[[k]] <- 
        window_specific_pinball_loss_plot_list
      trailing_window_murphy_plot_by_forecast_horizon_all_locations[[k]] <- 
        window_specific_murphy_plot_list
      
      trailing_window_pinball_loss_individual_locations_plot_by_forecast_horizon_list[[k]] <- 
        single_horizon_trailing_window_horizon_specific_murphy_plot_by_forecast_horizon_individual_locations
      
      trailing_window_horizon_specific_murphy_plot_by_forecast_horizon_individual_locations[[k]] <- 
        single_horizon_trailing_window_horizon_specific_murphy_plot_by_forecast_horizon_individual_locations
        
    }
    }
    return(list(quantile_scores_by_forecast_horizon_all_locations, 
                pinball_loss_all_locations_plot_by_forecast_horizon_list,
                
                murphy_results_by_forecast_horizon_all_locations,
                murphy_plot_by_forecast_horizon_all_locations,
                
                quantile_scores_by_forecast_horizon_individual_locations,
                pinball_loss_individual_locations_plot_by_forecast_horizon_list,
                
                murphy_results_by_forecast_horizon_individual_locations,
                murphy_plot_by_forecast_horizon_individual_locations,
                
                
                trailing_window_quantile_scores_by_forecast_horizon_all_locations,
                trailing_window_pinball_loss_all_locations_plot_by_forecast_horizon_list,
                
                trailing_window_murphy_results_by_forecast_horizon_all_locations,
                trailing_window_murphy_plot_by_forecast_horizon_all_locations,
                
                trailing_window_quantile_scores_by_forecast_horizon_individual_locations,
                trailing_window_pinball_loss_individual_locations_plot_by_forecast_horizon_list,
                
                trailing_window_murphy_results_by_forecast_horizon_individual_locations,
                trailing_window_murphy_plot_by_forecast_horizon_individual_locations
                ))
}

# add_relative_pinball_loss_to_dt <- function(pinball_results,
#                                             baseline_model,
#                                             trailing_window_option = NULL,
#                                             single_location_option = NULL){
#   baseline_results <- subset(pinball_results,
#                               model == baseline_model)
#   setnames(baseline_results, "score", "baseline_score")
#   baseline_results[, relative_score:= score/baseline_score]
#   
#   if(!is.null(trailing_window_option)){
#     if(is.null(single_location_option)){
#       baseline_results <- subset(baseline_results,
#                                  select == c("relative_score", "model", "location"))
#       pinball_results <- merge(pinball_results, 
#                                baseline_results,
#                                 by = c("max_time", "location", "model"))
#     }  
#     else{
#       baseline_results <- subset(baseline_results,
#                                  select == c("relative_score", "model"))
#       pinball_results <- merge(pinball_results, 
#                                baseline_results,
#                                by = c("max_time", "location", "model"))
#     }
#   }
#   if{!is.null(single_location)}
#     
#   }
# }

add_relative_pinball_loss_to_single_location_trailing_window <- function(pinball_results_list){
  pinball_trailing <- pinball_results_list[[9]]
  baseline_pinball_trailing <- 
    subset(pinball_trailing,
           model == "COVIDhub-baseline")
  baseline_pinball_trailing <- 
    subset(baseline_pinball_trailing,
           select = c("horizon", "mcb", "dsc", "score", "quantile_level",
                      "max_time"))
  setnames(baseline_pinball_trailing,
           c("mcb", "dsc", "score"),
           c("baseline_mcb", "baseline_dsc", "baseline_score"))
  pinball_trailing <- merge(pinball_trailing,
               baseline_pinball_trailing,
               by = c("horizon", "quantile_level", "max_time"))
  pinball_trailing[, relative_score:= score/baseline_score]
  return(pinball_trailing)
}
  
# length(pinball_loss_decomposition_results)
# tmp <- subset(subsetted_state_covid_forecasts_dt, location_name != "National")
#Pinball Loss: Murphy diagram
# run_pinball_loss_murphy_diagram_trailing_window_over_time <- 
#   function(input_data, #data
#            window_length = NULL,
#            plot_model_names,
#            plot_model_colours,
#            focus_specific_quantiles = NULL,
#            real_time = NULL,
#            option_trailing_window_on = FALSE){
#     #focus_specific_quantiles is an option to limit the number of quantiles 
#     #real_time is an option to set what the current date is (evaluating all forecasts previous to that date)
#     #option_trailing_window_on is an option to turn on/off additional trailing window evaluations
#     if(!is.null(real_time)){
#       input_data <- subset(input_data, forecast_date < real_time)
#     }
#     if(!is.null(focus_specific_quantiles)){
#       input_data <- subset(input_data, quantile_level %in% focus_specific_quantiles)
#     }
#     setkeyv(input_data, c("target_end_date", "horizon", "model"))
#     times <- unique(input_data$target_end_date)
#     locations <- unique(input_data$location)
#     runner_windows_list <- runner(times, k = window_length)
#     #Max index is current time point
#     #Sliding window with stride of 1 week
#     runner_windows_list <- runner_windows_list[window_length:length(runner_windows_list)] 
#     
#     
#     #Horizon-specific scores across all locations
#     quantile_scores_by_forecast_horizon_all_locations <- NULL
#     pinball_loss_all_locations_plot_by_forecast_horizon_list <- list()
#     
#     #Horizon-specific scores across for individual locations
#     quantile_scores_by_forecast_horizon_individual_locations <- NULL
#     pinball_loss_individual_locations_plot_by_forecast_horizon_list <- list()
#     for(i in 1:length(unique(input_data$horizon))){
#       horizon_specific_pinball_loss_individual_locations_plot_by_forecast_horizon_list <- list()
#       
#       horizon_in_q <- unique(input_data$horizon)[i]
#       print(paste0("Horizon:", horizon_in_q))
#       
#       tmp_horizon_specific_data <- subset(input_data,
#                                           horizon == horizon_in_q)
#       
#       #i) Starting with all locations
#       tmp_horizon_specific_reliability_results <- 
#         tmp_horizon_specific_data %>%
#         group_by(model, quantile_level) %>%
#         summarize(reldiag(predicted, observed, 
#                           alpha = unique(quantile_level), resampling = FALSE, digits = 1))
#       tmp_horizon_specific_quantile_scores <- 
#         tmp_horizon_specific_reliability_results %>%
#         group_by(quantile_level, model) %>%
#         distinct(across(score:pval_ucond))
#       tmp_horizon_specific_quantile_scores$quantile_level <- 
#         as.factor(tmp_horizon_specific_quantile_scores$quantile_level)
#       tmp_horizon_specific_quantile_scores$model <- 
#         as.factor(tmp_horizon_specific_quantile_scores$model)
#       tmp_horizon_specific_quantile_scores <- data.table(tmp_horizon_specific_quantile_scores)
#       
#       tmp_horizon_specific_isolines <- 
#         get_isolines_from_scores(tmp_horizon_specific_quantile_scores)
#       #Store in data.table (e.g. if want to only plot specific quantiles/horizons after running function)
#       quantile_scores_by_forecast_horizon_all_locations <- 
#         rbind(quantile_scores_by_forecast_horizon_all_locations,
#               tmp_horizon_specific_quantile_scores)
#       
#       #Storing plot
#       tmp_plot <- plot_pinball_loss_decomposition(tmp_horizon_specific_quantile_scores,
#                                                   tmp_horizon_specific_isolines,
#                                                   plot_model_names,
#                                                   plot_model_colours)
#       pinball_loss_all_locations_plot_by_forecast_horizon_list[[i]] <- tmp_plot
#       
#       
#       #ii) Individual locations
#       for(j in 1:length(unique(tmp_horizon_specific_data$location))){
#         location_in_q <- unique(tmp_horizon_specific_data$location)[j]
#         print(paste0("Location: ", location_in_q, ", Horizon:", horizon_in_q))
#         
#         #Subset horizon-specific data to an individual location
#         individual_location_tmp_horizon_specific_data <-
#           subset(tmp_horizon_specific_data,
#                  location == location_in_q)
#         #Get reliability results
#         individual_location_tmp_horizon_specific_reliability_results <- 
#           individual_location_tmp_horizon_specific_data %>%
#           group_by(model, quantile_level) %>%
#           summarize(reldiag(predicted, observed, 
#                             alpha = unique(quantile_level), resampling = FALSE, digits = 1))
#         individual_location_tmp_horizon_specific_quantile_scores <- 
#           individual_location_tmp_horizon_specific_reliability_results %>%
#           group_by(quantile_level, model) %>%
#           distinct(across(score:pval_ucond))
#         individual_location_tmp_horizon_specific_quantile_scores <- 
#           data.table(individual_location_tmp_horizon_specific_quantile_scores)
#         individual_location_tmp_horizon_specific_quantile_scores$quantile_level <- 
#           as.factor(individual_location_tmp_horizon_specific_quantile_scores$quantile_level)
#         individual_location_tmp_horizon_specific_quantile_scores$model <- 
#           as.factor(individual_location_tmp_horizon_specific_quantile_scores$model)
#         individual_location_tmp_horizon_specific_isolines <- 
#           get_isolines_from_scores(individual_location_tmp_horizon_specific_quantile_scores)
#         
#         #Store in data.table (e.g. if want to only plot specific quantiles/horizons after running function)
#         #This is for each location specifically
#         quantile_scores_by_forecast_horizon_individual_locations <- 
#           rbind(quantile_scores_by_forecast_horizon_individual_locations,
#                 individual_location_tmp_horizon_specific_quantile_scores)
#         
#         #Storing plot for each horizon(i) and location(j)
#         tmp_plot <- plot_pinball_loss_decomposition(individual_location_tmp_horizon_specific_quantile_scores,
#                                                     individual_location_tmp_horizon_specific_isolines,
#                                                     plot_model_names,
#                                                     plot_model_colours)
#         horizon_specific_pinball_loss_individual_locations_plot_by_forecast_horizon_list[[j]] <- 
#           tmp_plot
#       }
#       pinball_loss_individual_locations_plot_by_forecast_horizon_list[[i]] <- horizon_specific_pinball_loss_individual_locations_plot_by_forecast_horizon_list
#     }
#     
#   }
# tmp <- subset(tmp, horizon == 1)
# pinball_loss_murphy_dt <- murphydiag(tmp)
# 
# ymax <- max(pinball_loss_murphy_dt$mean_score)
# xmax <- max(pinball_loss_murphy_dt$theta)
# x_max <- 250000
# p1 <- pinball_loss_murphy_dt %>%
#   filter(quantile_level == 0.25) %>%
#   plot_murphy_diagram() +
#   xlab(NULL) +
#   scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y))
# 
# p2 <- pinball_loss_murphy_dt %>%
#   filter(quantile_level == 0.5) %>%
#   plot_murphy_diagram() +
#   theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ylab(NULL)
# 
# p3 <- pinball_loss_murphy_dt %>%
#   filter(quantile_level == 0.75) %>%
#   plot_murphy_diagram() +
#   theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + xlab(NULL) + ylab(NULL)
# 
# ggarrange(p1, p2, p3, nrow = 1)
#Shows difficulties in Murphy diagram for specific event
#Therefore, let's also look at decomposition :)

# define isolines
# iso <- state_covid_quantile_scores %>%
#   group_by(quantile_level, horizon) %>%
#   summarize(
#     mcb_best = floor(min(mcb)),
#     dsc_best = ceiling(max(dsc)),
#     mcb_worst = ceiling(max(mcb)),
#     dsc_worst = floor(min(dsc)),
#     unc = unique(unc),
#     score_best = mcb_best - dsc_best + unc,
#     score_worst = mcb_worst - dsc_worst + unc,
#     y_best = dsc_best - mcb_best,
#     y_worst = dsc_worst - mcb_worst
#   )
# iso

# facet_lims <- iso %>%
#   group_by(quantile) %>%
#   summarize(
#     x_lim = 1.25 * mcb_worst,
#     y_lim = 1.025 * dsc_best
#   )

# iso$unc %% 1
# iso <- iso[which(iso$horizon == 1), ]
# iso
# iso <- iso[which(iso$horizon == 1), ]
# iso <- iso %>% ungroup() %>% group_by(quantile_level)
# round(iso$score_worst - iso$score_best)
# iso <- iso %>%
#   group_by(quantile_level) %>%
#   summarize(
#     intercept = seq(y_worst + unc %% 1, y_best + unc %% 1, by = round((score_worst - score_best) / num_covid_models)
#     ),
#     slope = 1,
#     unc = unique(unc),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     score = (unc - intercept),
#     label = score
#   )
# # manually remove scores from isolines if there is overlap
# iso
# iso$label[c(
#   1, 2, 4, 8, 9, 10,
#   13, 16, 18,
#   20, 24, 27, 28
# )] <- NA
# state_covid_quantile_scores
# tmp_state_covid_quantile_scores <- copy(state_covid_quantile_scores)
# tmp_state_covid_quantile_scores <- data.table(state_covid_quantile_scores)
# tmp_state_covid_quantile_scores <- subset(tmp_state_covid_quantile_scores,
#                                           horizon == 1)
# 
# # tmp_state_covid_quantile_scores <- subset(tmp_state_covid_quantile_scores,
# #                                           quantile_level == 0.5)
# state_covid_quantile_scores
# ggplot(data = tmp_state_covid_quantile_scores) +
#   # facet_wrap(quantile_level ~ horizon, scales = "free", ncol = 4) +
#   facet_wrap(quantile_level ~.,  scales = "free", ncol = 4) +
#   # geom_blank(data = facet_lims, aes(x = x_lim, y = y_lim)) +
#   geom_abline(
#     data = iso, aes(intercept = intercept, slope = slope), color = "lightgray", alpha = 0.5,
#     size = 0.5
#   ) +
#   # geom_labelabline(
#   #   data = iso, aes(intercept = intercept, slope = slope, label = label), color = "gray50",
#   #   hjust = 0.85, size = 7 * 0.36, text_only = TRUE, boxcolour = NA, straight = TRUE
#   # ) +
#   geom_point(aes(x = mcb, y = dsc, color = model)) +
#   # geom_text_repel(aes(x = mcb, y = dsc, label = model),
#   #                 max.overlaps = NA, size = 8 * 0.36, nudge_x = 0,
#   #                 direction = "both", segment.color = "transparent", box.padding = 0.15, force = 1, point.padding = 0.75,
#   #                 seed = 4
#   # ) +
#   xlab("MCB") +
#   ylab("DSC") +
#   theme_light()+
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     aspect.ratio = 1,
#     legend.position = "bottom"
#     # axis.text.x = element_blank(),
#     # axis.ticks.x = element_blank(),
#     # axis.text.y = element_blank(),
#     # axis.ticks.y = element_blank()
#   ) +   scale_color_manual("Model", values = covid_model_colours,
#                            labels = corrected_covid_model_names)
# 
# 
# 
# #x = data.frame/list/matrix of forecasts + observations (y_var)
# mcbdsc(x, y_var = "y", ..., y = NULL, score = "MR_score")
# #Brier
# mcbdsc(x, y_var = "y", ..., y = NULL, score = "MR_score")
# 
# #CRPS Decomposition
# crps_ecdf <- function(y, grid_vals, ecdf) {
#   
#   x <- lapply(seq_len(length(y)), function(i) grid_vals)
#   p <- lapply(seq_len(nrow(ecdf)), function(i) ecdf[i,])#as.list( t(ecdf) )
#   w <- lapply(p, function(x) c(x[1], diff(x)))
#   
#   crps0 <- function(y, p, w, x) 2 * sum(w * ((y < x) - p + 0.5 * w) * (x - y))
#   mapply(crps0, y = y, p, w = w, x = x)
# }
# 
# crps_unc <- function(obs){
#   obs_ensemble <- matrix(rep(obs, length(obs)), nrow = length(obs), ncol = length(obs), byrow = TRUE)
#   return(mean(crps_sample(obs, dat = obs_ensemble)))
# }
# 
# 
# gaussian_crps <- function(y, mu, sigma){
#   return(mean(scoringRules::crps_norm(y, mean = mu, sd = sigma)))
# }
# 
# bounds_norm_minmax <- function(y, mu, sigma, epsilon, delta){
#   average  <- mean(y)
#   a <- min(y)
#   b <- max(y)
#   n <- length(y)
#   tmp  = rep(NA,n)
#   for ( i in 1:n){
#     integrand1 = function(x) {return((pnorm(x,mean=mu[i],sd=sigma[i]))^2)}
#     integrand2 = function(x) {return((1-pnorm(x,mean=mu[i],sd=sigma[i]))^2)}
#     tmp[i]= integrate(integrand1,lower=-Inf,upper=a)$value + integrate(integrand2,lower=b,upper=Inf)$value
#   }
#   if (mean(tmp) <= epsilon){
#     return(c(a, b))
#   } else {
#     while (mean(tmp)>epsilon){
#       a = a - delta
#       b = b + delta
#       for ( i in 1:n){
#         integrand1 = function(x) {return((pnorm(x,mean=mu[i],sd=sigma[i]))^2)}
#         integrand2 = function(x) {return((1-pnorm(x,mean=mu[i],sd=sigma[i]))^2)}
#         tmp[i]=integrate(integrand1,lower=-Inf,upper=a)$value +integrate(integrand2,lower=b,upper=Inf)$value
#       }
#     }
#     return(c(a, b))
#   }
# }
# 
# 
# func_crps_normAB <- function(y, mu, sigma, a, b){
#   n = length(y)
#   tmp  = rep(NA,n)
#   for ( i in 1:n){
#     integrand1 = function(x) {return((pnorm(x,mean=mu[i],sd=sigma[i]))^2)}
#     integrand2 = function(x) {return((pnorm(x,mean=mu[i],sd=sigma[i]) - 1)^2)}
#     tmp[i]=integrate(integrand1,lower=a,upper=y[i])$value + integrate(integrand2,lower=y[i],upper=b)$value
#   }
#   return(mean(tmp))
# }
# 
# func_crps_normAB2 <- function(y, mu, sigma, a, b){
#   return(mean(scoringRules::crps_cnorm(y, location = mu, scale = sigma, lower = a, upper = b)))
# }
# 
# 
# 
# 
# bounds_norm_mix <- function(y,idr_preds, h, epsilon, delta){
#   
#   a <- min(y)
#   b <- max(y)
#   n <- length(y)
#   tmp  = rep(NA,n)
#   for ( i in 1:n){
#     mu <- idr_preds[[i]]$points
#     weights <- diff(c(0, idr_preds[[i]]$cdf))
#     # extraDistr::pmixnorm(grid_vals[k], mean = mean, alpha = weights, sd = rep(h, length(mean)))
#     integrand1 = function(x) {return((extraDistr::pmixnorm(x,mean=mu,sd=rep(h, length(mu)), alpha = weights))^2)}
#     integrand2 = function(x) {return((1-extraDistr::pmixnorm(x,mean=mu,sd=rep(h, length(mu)), alpha = weights))^2)}
#     tmp[i]= integrate(integrand1,lower=-Inf,upper=a)$value + integrate(integrand2,lower=b,upper=Inf)$value
#   }
#   if (mean(tmp) <= epsilon){
#     return(c(a, b))
#   } else {
#     while (mean(tmp)>epsilon){
#       a = a - delta
#       b = b + delta
#       for ( i in 1:n){
#         mu <- idr_preds[[i]]$points
#         weights <- diff(c(0, idr_preds[[i]]$cdf))
#         integrand1 = function(x) {return((extraDistr::pmixnorm(x,mean=mu,sd=rep(h, length(mu)), alpha = weights))^2)}
#         integrand2 = function(x) {return((1-extraDistr::pmixnorm(x,mean=mu,sd=rep(h, length(mu)), alpha = weights))^2)}
#         tmp[i]=integrate(integrand1,lower=-Inf,upper=a)$value +integrate(integrand2,lower=b,upper=Inf)$value
#       }
#     }
#     return(c(a, b))
#   }
# }
# 
# mix_pt <- function(x_mean, h, weights, df){
#   return(sum(weights * pt(x_mean / h, df = df)))
# }
# 
# mix_pt2 <- function(x, mean, h, weights, df){
#   n <- length(mean)
#   tmp <- 0
#   for (j in 1:n){
#     tmp <- tmp + weights[j]*pt((x-mean[j]) / h, df = df)
#   }
#   return(tmp)
# }
# 
# bounds_t_mix <- function(y,idr_preds, h, df, epsilon, delta){
#   #average  <- mean(y)
#   #print(average)
#   #t <- max(max(y)-average,average-min(y))
#   #print(t)
#   a <- min(y)
#   b <- max(y)
#   n <- length(y)
#   tmp  = rep(NA,n)
#   for ( i in 1:n){
#     mu <- idr_preds[[i]]$points
#     weights <- diff(c(0, idr_preds[[i]]$cdf))
#     
#     # extraDistr::pmixnorm(grid_vals[k], mean = mean, alpha = weights, sd = rep(h, length(mean)))
#     integrand1 = function(x) {return((mix_pt2(x,mu,h, weights, df))^2)}
#     integrand2 = function(x) {return((1-mix_pt2(x,mu,h, weights, df))^2)}
#     tmp[i]= integrate(integrand1,lower=-Inf,upper=a)$value + integrate(integrand2,lower=b,upper=Inf)$value
#   }
#   if (mean(tmp) <= epsilon){
#     return(c(a, b))
#   } else {
#     while (mean(tmp)>epsilon){
#       a = a - delta
#       b = b + delta
#       for ( i in 1:n){
#         mu <- idr_preds[[i]]$points
#         weights <- diff(c(0, idr_preds[[i]]$cdf))
#         integrand1 = function(x) {return((mix_pt2(x,mu,h, weights, df))^2)}
#         integrand2 = function(x) {return((1-mix_pt2(x,mu,h, weights, df))^2)}
#         tmp[i]=integrate(integrand1,lower=-Inf,upper=a)$value +integrate(integrand2,lower=b,upper=Inf)$value
#       }
#     }
#     return(c(a, b))
#   }
# }
# 
# 
# 
# bounds_norm_mix_cp <- function(y,ens, h, epsilon, delta){
#   a <- min(y)
#   b <- max(y)
#   n <- length(y)
#   tmp  = rep(NA,n)
#   for ( i in 1:n){
#     mean <- sort(unique(ens[i,]))
#     colnames(mean) <- NULL
#     mean <- unlist(mean)
#     ecdf_fun <- ecdf(ens[i,])
#     ecdf_vals <- ecdf_fun(mean)
#     weights <-  diff(c(0, ecdf_vals))
#     sd <- rep(h, length(mean))
#     #mu <- mean[i,]
#     #sd <- sig[i,]
#     #print(i)
#     #weights <- diff(c(0, idr_preds[[i]]$cdf))
#     # extraDistr::pmixnorm(grid_vals[k], mean = mean, alpha = weights, sd = rep(h, length(mean)))
#     integrand1 = function(x) {return((extraDistr::pmixnorm(x,mean=mean,sd=sd, alpha = weights))^2)}
#     integrand2 = function(x) {return((1-extraDistr::pmixnorm(x,mean=mean,sd=sd, alpha = weights))^2)}
#     tmp[i]= integrate(integrand1,lower=-Inf,upper=a)$value + integrate(integrand2,lower=b,upper=Inf)$value
#   }
#   if (mean(tmp) <= epsilon){
#     return(c(a, b))
#   } else {
#     while (mean(tmp)>epsilon){
#       a = a - delta
#       b = b + delta
#       for ( i in 1:n){
#         mean <- sort(unique(ens[i,]))
#         colnames(mean) <- NULL
#         mean <- unlist(mean)
#         ecdf_fun <- ecdf(ens[i,])
#         ecdf_vals <- ecdf_fun(mean)
#         weights <-  diff(c(0, ecdf_vals))
#         sd <- rep(h, length(mean))
#         integrand1 = function(x) {return((extraDistr::pmixnorm(x,mean=mean,sd=sd, alpha = weights))^2)}
#         integrand2 = function(x) {return((1-extraDistr::pmixnorm(x,mean=mean,sd=sd, alpha = weights))^2)}
#         tmp[i]=integrate(integrand1,lower=-Inf,upper=a)$value +integrate(integrand2,lower=b,upper=Inf)$value
#       }
#     }
#     return(c(a, b))
#   }
# }
# 
# # y_test,ens, h, df, epsilon, delta
# 
# bounds_t_mix_cp <- function(y,ens, h, df, epsilon, delta){
#   #average  <- mean(y)
#   #print(average)
#   #t <- max(max(y)-average,average-min(y))
#   #print(t)
#   a <- min(y)
#   b <- max(y)
#   n <- length(y)
#   
#   tmp  = rep(NA,n)
#   for ( i in 1:n){
#     mean <- sort(unique(ens[i,]))
#     colnames(mean) <- NULL
#     mean <- unlist(mean)
#     ecdf_fun <- ecdf(ens[i,])
#     ecdf_vals <- ecdf_fun(mean)
#     weights <-  diff(c(0, ecdf_vals))
#     #sd <- rep(h, length(mean))
#     
#     # extraDistr::pmixnorm(grid_vals[k], mean = mean, alpha = weights, sd = rep(h, length(mean)))
#     integrand1 = function(x) {return((mix_pt2(x,mean,h, weights, df))^2)}
#     integrand2 = function(x) {return((1-mix_pt2(x,mean,h, weights, df))^2)}
#     tmp[i]= integrate(integrand1,lower=-Inf,upper=a)$value + integrate(integrand2,lower=b,upper= Inf)$value
#   }
#   
#   if (mean(tmp) <= epsilon){
#     return(c(a, b))
#   } else {
#     while (mean(tmp)>epsilon){
#       a = a - delta
#       b = b + delta
#       for ( i in 1:n){
#         mean <- sort(unique(ens[i,]))
#         colnames(mean) <- NULL
#         mean <- unlist(mean)
#         ecdf_fun <- ecdf(ens[i,])
#         ecdf_vals <- ecdf_fun(mean)
#         weights <-  diff(c(0, ecdf_vals))
#         integrand1 = function(x) {return((mix_pt2(x,mean,h, weights, df))^2)}
#         integrand2 = function(x) {return((1-mix_pt2(x,mean,h, weights, df))^2)}
#         tmp[i]=integrate(integrand1,lower=-Inf,upper=a)$value +integrate(integrand2,lower=b,upper=Inf)$value
#       }
#     }
#     return(c(a, b))
#   }
# }
# 
# 
# bounds_norm_mix_mc <- function(y,mean, sig, epsilon, delta){
#   #average  <- mean(y)
#   #print(average)
#   #t <- max(max(y)-average,average-min(y))
#   #print(t)
#   a <- min(y)
#   b <- max(y)
#   n <- length(y)
#   tmp  = rep(NA,n)
#   for ( i in 1:n){
#     mu <- mean[i,]
#     sd <- sig[i,]
#     #print(i)
#     #weights <- diff(c(0, idr_preds[[i]]$cdf))
#     # extraDistr::pmixnorm(grid_vals[k], mean = mean, alpha = weights, sd = rep(h, length(mean)))
#     integrand1 = function(x) {return((extraDistr::pmixnorm(x,mean=mu,sd=sd, alpha = rep(1, length(mu))/length(mu)))^2)}
#     integrand2 = function(x) {return((1-extraDistr::pmixnorm(x,mean=mu,sd=sd, alpha = rep(1, length(mu))/ length(mu)))^2)}
#     tmp[i]= integrate(integrand1,lower=-Inf,upper=a)$value + integrate(integrand2,lower=b,upper=Inf)$value
#   }
#   if (mean(tmp) <= epsilon){
#     return(c(a, b))
#   } else {
#     while (mean(tmp)>epsilon){
#       print(mean(tmp))
#       a = a - delta
#       b = b + delta
#       for ( i in 1:n){
#         mu <- mean[i,]
#         sd <- sig[i,]
#         integrand1 = function(x) {return((extraDistr::pmixnorm(x,mean=mu,sd=sd, alpha = rep(1, length(mu))/ length(mu)))^2)}
#         integrand2 = function(x) {return((1-extraDistr::pmixnorm(x,mean=mu,sd=sd, alpha = rep(1, length(mu))/ length(mu)))^2)}
#         tmp[i]=integrate(integrand1,lower=-Inf,upper=a)$value +integrate(integrand2,lower=b,upper=Inf)$value
#       }
#     }
#     return(c(a, b))
#   }
# }
# 
# 
# 
# 
# 
# 
# # 2) CRPS over time + across space
# # 3) Forecast skill card
# 
# 
# #Not using:
# compute_fsc <- function(prob_matrix, true_outcomes) {
#   # prob_matrix: A matrix where each row is a forecasted probability distribution
#   # true_outcomes: A vector of the true observed categories (indices)
#   
#   K <- ncol(prob_matrix)  # Number of categories
#   N <- nrow(prob_matrix)  # Number of forecasts
#   FSC <- matrix(0, nrow = K, ncol = K)
#   
#   for (i in 1:N) {
#     forecasted_class <- which.max(prob_matrix[i, ])  # Most probable category
#     observed_class <- true_outcomes[i]               # True observed category
#     
#     FSC[observed_class, forecasted_class] <- FSC[observed_class, forecasted_class] + 1
#   }
#   
#   return(FSC / rowSums(FSC))  # Normalize to probabilities per row
# }
# 
# 
# 
# #Using:
# validated <- as_forecast_quantile(example_quantile)
# validated
# 
# scores <- score(validated) %>% 
#   get_pairwise_comparisons(by = c("target_type", "target_end_date"),
#                            compare = "model",
#                            metric = "wis",
#                            baseline = "EuroCOVIDhub-baseline")
# scores
# get_pairwise_comparisons(tmp,
#                          by = c("model", "target_type"),
#                          metric = "")
# mean(scores[which(model == "EuroCOVIDhub-ensemble" 
#              & target_type == "Cases")]$wis)
# # Metric-based decision-making -----
# 
# #How packages work (to be deleted)
# require(triptych)
# tr <- triptych(ex_binary)
# ?triptych::ex_binary
# ?scoringutils::plot_quantile_coverage
# ex_binary
# ?triptych
# rel <- reliability(ex_binary)
# rel[c(1, 3, 6, 9)] |>
#   autoplot() +
#   ggplot2::guides(colour = ggplot2::guide_legend("Forecast"))
# 
# regions(tr$murphy)
# plot(tr)
# state_level_covid_forecasts_dt$target_end_date
# state_level_covid_forecasts_dt
# quantile_forecasts_dt <- subset(state_level_covid_forecasts_dt,
#                              horizon == 1 & location == "01" & target_end_date == "2020-08-15" &
#                                model == "COVIDhub-4_week_ensemble")
# setkeyv(quantile_forecasts_dt, "quantile_level")
# # quantile_forecasts <-list(c(unique(quantile_forecasts$quantile_level)), c(quantile_forecasts$predicted))
# quantile_forecasts_dt
# quantile_forecasts_dt$quantile_level[1]
# quantile_forecasts <- c(quantile_forecasts_dt$quantile_level[1] = quantile_forecasts_dt$predicted[1],
#                            quantile_forecasts_dt$quantile_level[2] = quantile_forecasts_dt$predicted[2],
#                            quantile_forecasts_dt$quantile_level[3] = quantile_forecasts_dt$predicted[3],
#                            quantile_forecasts_dt$quantile_level[4] = quantile_forecasts_dt$predicted[4],
#                            quantile_forecasts_dt$quantile_level[5] = quantile_forecasts_dt$predicted[4],
#                            quantile_forecasts_dt$quantile_level[6] = quantile_forecasts_dt$predicted[6],
#                            quantile_forecasts_dt$quantile_level[7] = quantile_forecasts_dt$predicted[7])
# quantile_forecasts <- split(quantile_forecasts_dt$predicted, (quantile_forecasts_dt$quantile_level))
# names(quantile_forecasts) <- as.numeric(names(quantile_forecasts))
# # names(quantile_forecasts)[[1]] <- quantile_forecasts_dt$quantile_level
# prob_forecast <- sapply(quantile_forecasts, function(q) {
#   p <- which(q >= 2)
#   print(as.numeric(names(q)))
#   # if (length(p) == 0) return(0)
#   return(1 - names(q)[min(p)])
# })
# prob_forecast
# names(quantile_forecasts)
# prob_forecast





# 1) REV -----
#Can apply REV to i) whole dataset (aggregate value), 
#                 ii) given location (overall spatial variation), 
#                 iii) rolling windows (temporal variation), 
#                 iv) rolling windows + given location (spatiotemporal variations)
#Functions below are just applied to single dataset, can then apply to several windowed datasets
#Rolling window may not contain as relevant data for extreme events
#Need confusion values to compute REV
compute_confusion_values_by_forecasting_model <- function(cost_loss_ratio,
                                            event_threshold, #Fixed event threshold -> Can replace with upper observed quantile
                                            event_threshold_type,
                                            observed_data,
                                            forecast_representation,
                                            trigger_threshold,
                                            trailing_window_dates = NULL){
  #event_threshold = event in question
  #trigger_threshold = probability above which we classify event
  #For perfectly calibrated model, should = C/L
  
  
  if(event_threshold_type == "fixed"){#i.e. above certain level of cases
    observed_data[, location_specific_threshold:= event_threshold]
    # print(length(which(observed_data$observed >= observed_data$location_specific_threshold)))
    observed_data[, location_agnostic_threshold:= event_threshold]
  }
  if(event_threshold_type == "fixed_quantile"){ #i.e. for given quantile (e.g. upper 10% of events is event_threshold = 0.9)
    location_specific_thresholds <- observed_data[, list(location_specific_threshold = quantile(observed,
                                                                                                event_threshold)),
                                                  by = "location"]
    observed_data <- merge(observed_data, location_specific_thresholds, by = "location")
    # observed_data[, location_specific_threshold:= quantile(observed, event_threshold), by = "location"]
    observed_data[, location_agnostic_threshold:= quantile(observed, event_threshold)]
  }
  
  if(!is.null(trailing_window_dates)){
    observed_data <- subset(observed_data, target_end_date %in% trailing_window_dates)
  }
  #Can include later:
  # if(event_threshold_type == "time_varying"){
  #   setorder(observed_data, location, target_end_date)
  #   # Compute rolling upper X% quantile for each time point and location
  #   observed_data[, threshold:= sapply(target_end_date, function(time) {
  #     quantile(observed[target_end_date <= time], probs = event_threshold, na.rm = TRUE)
  #   }), by = location]
  # }
  
  
  if(forecast_representation == "quantile"){
    #For perfectly calibrated model, Focus on C/L = quantile of interest (i.e. events predicted w/p C/L)
    #Here, allow there to be a user-defined trigger threshold (as in weather forecasting)
    #Will loop over trigger_thresholds
    observed_data <- subset(observed_data, quantile_level == trigger_threshold)
    
    #Event occurred or not?
    observed_data[, LOCATION_SPECIFIC_BINARY_EVENT_INDICATOR:= ifelse(observed >= location_specific_threshold, 1, 0)]
    #Now, whether observed event/non-event equals the predicted event
    observed_data[, LOCATION_SPECIFIC_BINARY_EVENT_PREDICTION:= ifelse(predicted >= location_specific_threshold, 1, 0)]
    # print(paste0("Location-specific event rate: ", sum(observed_data$LOCATION_SPECIFIC_BINARY_EVENT_INDICATOR)/(nrow(observed_data)) * length(unique(observed_data$location))))
    
    
    #Event occurred or not?
    observed_data[, LOCATION_AGNOSTIC_BINARY_EVENT_INDICATOR:= ifelse(observed >= location_agnostic_threshold, 1, 0)]
    # print(paste0("Location-agnostic event rate: ", sum(observed_data$LOCATION_AGNOSTIC_BINARY_EVENT_INDICATOR)/(nrow(observed_data))))
    
    #Now, whether observed event/non-event equals the predicted event
    observed_data[, LOCATION_AGNOSTIC_BINARY_EVENT_PREDICTION:= ifelse(predicted >= location_agnostic_threshold, 1, 0)]
    
  } 
  
  else if(forecast_representation == "sample"){
    #NOT USED (YET!)
    observed_data <- 
      observed_data[, list(LOCATION_SPECIFIC_PROBABILITY_EVENT = length(which(predicted >= location_specific_threshold))/length(predicted),
                           LOCATION_AGNOSTIC_PROBABILITY_EVENT = length(which(predicted >= location_agnostic_threshold))/length(predicted),
                           observed = unique(observed)),
                    by = c("target_end_date", "location")]
    observed_data[, LOCATION_SPECIFIC_BINARY_EVENT_INDICATOR:= ifelse(observed >= location_specific_threshold, 1, 0)]
    observed_data[, LOCATION_SPECIFIC_BINARY_EVENT_PREDICTION:= ifelse(LOCATION_SPECIFIC_PROBABILITY_EVENT >= trigger_threshold, 1, 0)]
    
    observed_data[, LOCATION_AGNOSTIC_BINARY_EVENT_INDICATOR:= ifelse(observed >= location_agnostic_threshold, 1, 0)]
    observed_data[, LOCATION_AGNOSTIC_BINARY_EVENT_PREDICTION:= ifelse(LOCATION_AGNOSTIC_PROBABILITY_EVENT >= trigger_threshold, 1, 0)]
    
  }
  
  
  # Compute TP, FP, FN, TN
  # observed_data[, `:=`(
  #   TP = as.integer(BINARY_EVENT_PREDICTION & BINARY_EVENT_INDICATOR),  # True Positive
  #   FP = as.integer(BINARY_EVENT_PREDICTION & !BINARY_EVENT_INDICATOR), # False Positive
  #   FN = as.integer(!BINARY_EVENT_PREDICTION & BINARY_EVENT_INDICATOR), # False Negative
  #   TN = as.integer(!BINARY_EVENT_PREDICTION & !BINARY_EVENT_INDICATOR) # True Negative
  # )]
  # IF
  #Compute confusuion matrix values
  # observed_data[, TP:= ifelse(LOCATION_AGNOSTIC_BINARY_EVENT_PREDICTION == 1 & LOCATION_AGNOSTIC_BINARY_EVENT_INDICATOR == 1, 1, 0)]
  # observed_data[, FP:= ifelse(LOCATION_AGNOSTIC_BINARY_EVENT_PREDICTION == 1 & LOCATION_AGNOSTIC_BINARY_EVENT_INDICATOR == 0, 1, 0)]
  # observed_data[, FN:= ifelse(LOCATION_AGNOSTIC_BINARY_EVENT_PREDICTION == 0 & LOCATION_AGNOSTIC_BINARY_EVENT_INDICATOR == 1, 1, 0)]
  # observed_data[, TN:= ifelse(LOCATION_AGNOSTIC_BINARY_EVENT_PREDICTION == 0 & LOCATION_AGNOSTIC_BINARY_EVENT_INDICATOR == 0, 1, 0)]
  # 
  # observed_data[, LOCATION_SPECIFIC_TP:= ifelse(LOCATION_SPECIFIC_BINARY_EVENT_PREDICTION == 1 & LOCATION_SPECIFIC_BINARY_EVENT_INDICATOR == 1, 1, 0)]
  # observed_data[, LOCATION_SPECIFIC_FP:= ifelse(LOCATION_SPECIFIC_BINARY_EVENT_PREDICTION == 1 & LOCATION_SPECIFIC_BINARY_EVENT_INDICATOR == 0, 1, 0)]
  # observed_data[, LOCATION_SPECIFIC_FN:= ifelse(LOCATION_SPECIFIC_BINARY_EVENT_PREDICTION == 0 & LOCATION_SPECIFIC_BINARY_EVENT_INDICATOR == 1, 1, 0)]
  # observed_data[, LOCATION_SPECIFIC_TN:= ifelse(LOCATION_SPECIFIC_BINARY_EVENT_PREDICTION == 0 & LOCATION_SPECIFIC_BINARY_EVENT_INDICATOR == 0, 1, 0)]
  
  summary_location_specific_by_location <- observed_data[, list(
    True_Positives = sum(LOCATION_SPECIFIC_BINARY_EVENT_PREDICTION == 1 & LOCATION_SPECIFIC_BINARY_EVENT_INDICATOR == 1),
    False_Positives = sum(LOCATION_SPECIFIC_BINARY_EVENT_PREDICTION == 1 & LOCATION_SPECIFIC_BINARY_EVENT_INDICATOR == 0),
    False_Negatives = sum(LOCATION_SPECIFIC_BINARY_EVENT_PREDICTION == 0 & LOCATION_SPECIFIC_BINARY_EVENT_INDICATOR == 1),
    True_Negatives = sum(LOCATION_SPECIFIC_BINARY_EVENT_PREDICTION == 0 & LOCATION_SPECIFIC_BINARY_EVENT_INDICATOR == 0),
    Num_Observations = length(LOCATION_SPECIFIC_BINARY_EVENT_PREDICTION)
  ), by = "location"]
  
  summary_location_specific_all_locations <- observed_data[, list(
    True_Positives = sum(LOCATION_SPECIFIC_BINARY_EVENT_PREDICTION == 1 & LOCATION_SPECIFIC_BINARY_EVENT_INDICATOR == 1),
    False_Positives = sum(LOCATION_SPECIFIC_BINARY_EVENT_PREDICTION == 1 & LOCATION_SPECIFIC_BINARY_EVENT_INDICATOR == 0),
    False_Negatives = sum(LOCATION_SPECIFIC_BINARY_EVENT_PREDICTION == 0 & LOCATION_SPECIFIC_BINARY_EVENT_INDICATOR == 1),
    True_Negatives = sum(LOCATION_SPECIFIC_BINARY_EVENT_PREDICTION == 0 & LOCATION_SPECIFIC_BINARY_EVENT_INDICATOR == 0),
    Num_Observations = length(LOCATION_SPECIFIC_BINARY_EVENT_PREDICTION)
  )]
  # print(paste0("Location-specific event rate: ", 
  #              (summary_location_specific_all_locations$True_Positives + 
  #                 summary_location_specific_all_locations$False_Negatives)/(summary_location_specific_all_locations$Num_Observations)))
  summary_location_agnostic_all_locations <- observed_data[, list(
    True_Positives = sum(LOCATION_AGNOSTIC_BINARY_EVENT_PREDICTION == 1 & LOCATION_AGNOSTIC_BINARY_EVENT_INDICATOR == 1),
    False_Positives = sum(LOCATION_AGNOSTIC_BINARY_EVENT_PREDICTION == 1 & LOCATION_AGNOSTIC_BINARY_EVENT_INDICATOR == 0),
    False_Negatives = sum(LOCATION_AGNOSTIC_BINARY_EVENT_PREDICTION == 0 & LOCATION_AGNOSTIC_BINARY_EVENT_INDICATOR == 1),
    True_Negatives = sum(LOCATION_AGNOSTIC_BINARY_EVENT_PREDICTION == 0 & LOCATION_AGNOSTIC_BINARY_EVENT_INDICATOR == 0),
    Num_Observations = length(LOCATION_AGNOSTIC_BINARY_EVENT_PREDICTION)
  )]
  
  
  # Summarize by location
  # summary_location_specific_by_location <- observed_data[, list(
  #   True_Positives = sum(LOCATION_SPECIFIC_TP),
  #   False_Positives = sum(LOCATION_SPECIFIC_FP),
  #   False_Negatives = sum(LOCATION_SPECIFIC_FN),
  #   True_Negatives = sum(LOCATION_SPECIFIC_TN),
  #   Num_Observations = length(LOCATION_SPECIFIC_TP)
  # ), by = "location"]
  # #Location-specific event rate
  # # print(paste0("Location-specific event rate: ", observed_data$LOCATION_SPECIFIC_TP + observed_data$LOCATION_SPECIFIC_FN))
  # 
  # summary_location_specific_overall <- observed_data[, list(
  #   True_Positives = sum(LOCATION_SPECIFIC_TP),
  #   False_Positives = sum(LOCATION_SPECIFIC_FP),
  #   False_Negatives = sum(LOCATION_SPECIFIC_FN),
  #   True_Negatives = sum(LOCATION_SPECIFIC_TN),
  #   Num_Observations = length(LOCATION_SPECIFIC_TP)
  # )]
  # # print(summary_by_location)
  # # Overall summary
  # summary_agnostic_overall <- observed_data[, list(
  #   True_Positives = sum(TP),
  #   False_Positives = sum(FP),
  #   False_Negatives = sum(FN),
  #   True_Negatives = sum(TN),
  #   Num_Observations = length(TP)
  # )]
  return(list(summary_location_specific_by_location,
              summary_location_specific_all_locations,
              summary_location_agnostic_all_locations))
}


#Could adapt to having moving trigger threshold
#Currently only use trigger threshold = 1-C/L ratio
compute_relative_economic_value <- function(event_threshold, #Fixed event threshold -> Can replace with upper observed quantile
                                            event_threshold_type,
                                            observed_data,
                                            forecast_representation,
                                            baseline_model = FALSE,
                                            baseline_data = NULL) {
  
  #Counting any negative values
  numerator_counter <- 0
  denominator_counter <- 0
  
  all_locs_location_agnostic_numerator_counter <- 0
  all_locs_location_agnostic_denominator_counter <- 0
  
  all_locs_location_specific_numerator_counter <- 0
  all_locs_location_specific_denominator_counter <- 0
  
  #This function uses base rate as baseline
  
  #Could also use a baseline model
  
  
  if(forecast_representation == "quantile"){
    #Crude: Only consider C/L ratios equal to the quantile
    #Future: Could interpolate.
    #Setting trigger thresholds equal to 1-C/L ratio (i.e. classify event if event predicted w/p C/L)
    cost_loss_ratios <- sort(unique(observed_data$quantile_level), decreasing = FALSE)
    trigger_thresholds <- rev(cost_loss_ratios)
  }
  if(forecast_representation == "sample"){
    cost_loss_ratios <- seq(0, 1, length.out = 100) #Sequence of C/L ratios for which we compute REV
    trigger_thresholds <- seq(0, 1, length.out = 100)
    
  }
  # E_f <- rep(NA, length(cost_loss_ratios))
  # E_c <- rep(NA, length(cost_loss_ratios))
  # E_p <- rep(NA, length(cost_loss_ratios))
  rev_results_by_location_dt <- NULL
  rev_results_all_locations_dt <- NULL
  rev_location_specific_results_across_all_locations <- NULL
  for(i in 1:length(cost_loss_ratios)){
    #Looping over all C/L ratios and extracting REV for different settings
    c_l_ratio <- cost_loss_ratios[i]
    
    #Extract results for a single C/L ratio and single event type
    
    # print("here1?")
    # for(k in 1:length(trigger_thresholds)){
      # trigger_threshold_in_q <- trigger_thresholds[k]
      # trigger_threshold_in_q <- c_l_ratio
      trigger_threshold_in_q <- trigger_thresholds[i]
      
      # print(paste0("trigger_threshold_in_q: ",trigger_threshold_in_q))
      confusion_results <- 
        compute_confusion_values_by_forecasting_model(cost_loss_ratio = c_l_ratio,
                                                      event_threshold = event_threshold, #Fixed event threshold -> Can replace with upper observed quantile
                                                      event_threshold_type = event_threshold_type,
                                                      observed_data = observed_data,
                                                      forecast_representation = forecast_representation,
                                                      trigger_threshold = trigger_threshold_in_q)
      # print(confusion_results)
      if(baseline_model == TRUE){#If using baseline model
        baseline_confusion_results <- 
          compute_confusion_values_by_forecasting_model(cost_loss_ratio = c_l_ratio,
                                                        event_threshold = event_threshold, #Fixed event threshold -> Can replace with upper observed quantile
                                                        event_threshold_type = event_threshold_type,
                                                        observed_data = baseline_data,
                                                        forecast_representation = forecast_representation,
                                                        trigger_threshold = trigger_threshold_in_q)
        baseline_confusion_results_by_location <- baseline_confusion_results[[1]] 
        baseline_confusion_results_location_specific_all_locations <- baseline_confusion_results[[3]]
      }
      
      
      
      #1) REV for individual locations (using location-specific events)
      confusion_results_by_location <- confusion_results[[1]] 
      locations <- unique(confusion_results_by_location$location)
      rev_by_location <- rep(NA, length(locations))
      denominators <- rep(NA, length(locations))
      numerators <- rep(NA, length(locations))
      for(j in 1:length(locations)){
        # print(paste0("Location ", j))
        #j loops over number of locations
        location_in_q <- unique(locations)[j]
        #Subset to location in question
        confusion_results_for_single_location <- 
          subset(confusion_results_by_location,
                 location == location_in_q) 
        
        # print(confusion_results_for_single_location)
        #Convert to proportions for REV
        num_observations <- confusion_results_for_single_location$Num_Observations
        tp <- confusion_results_for_single_location$True_Positives/num_observations
        fp <- confusion_results_for_single_location$False_Positives/num_observations
        fn <- confusion_results_for_single_location$False_Negatives/num_observations
        tn <- confusion_results_for_single_location$True_Negatives/num_observations
        # print(round(tp + fn + fp + tn))
        stopifnot(round(tp + fn + fp + tn) == 1)
        
        #Stop if negative confusion matrix alues
        x <- c(tp, fp, fn, tn)
        stopifnot(length(which(x < 0)) == 0)
        # print(paste0("tp + fn: ", tp + fn))
        # print(paste0("tp + fp: ", tp + fp))
        # print(paste0("fn: ", fn))
        #perfect_forecast: (tp + fn)*C/L
        
        #Note we divide by loss straight away (assuming only know c_l ratio, not explicit costs/losses)
        e_perfect <- (tp + fn)*c_l_ratio
        
        
        # print(paste0("e_perfect: ", e_perfect))
        if(baseline_model == TRUE){ #i.e. if using baseline model for reference
          baseline_confusion_results_for_single_location <- 
            subset(baseline_confusion_results_by_location,
                   location == location_in_q) 
          num_observations <- confusion_results_for_single_location$Num_Observations
          baseline_tp <- baseline_confusion_results_for_single_location$True_Positives/num_observations
          baseline_fp <- baseline_confusion_results_for_single_location$False_Positives/num_observations
          baseline_fn <- baseline_confusion_results_for_single_location$False_Negatives/num_observations
          baseline_tn <- baseline_confusion_results_for_single_location$True_Negatives/num_observations
          stopifnot(round(baseline_tp + baseline_fn + baseline_fp + baseline_tn) == 1)
          e_baseline <- (baseline_tp + baseline_fp)*c_l_ratio + baseline_fn
        }
        else if(baseline_model == FALSE){
          #Historical average
          e_baseline <- min(tp+fn, c_l_ratio)
        }
        # print(paste0("e_baseline: ", e_baseline))
        
        e_model <- (tp + fp)*c_l_ratio + fn
        # print(paste0("e_model: ", e_model))
        
        numerators[j] <- e_baseline - e_model
        denominators[j] <- e_baseline - e_perfect
        if(denominators[j] == 0) { #If baseline model provides no value
          denominator_counter = denominator_counter + 1
          rev_by_location[j] <- NA  # Assign NA or alternative handling
        }
        else{
          rev_by_location[j] <- (e_baseline - e_model)/(e_baseline - e_perfect)
        }
        if(numerators[j] < 0){ #If model provides no benefit, set to 0
          numerator_counter = numerator_counter + 1
          rev_by_location[j] <- 0
        }
          
        # rev_by_location[j] <- 
        #   (min(tp + fn, c_l_ratio) - 
        #      (tp + fp)*c_l_ratio + fn)/(min(tp + fn, c_l_ratio) - (tp + fn)*c_l_ratio)
      }
    
    #Data.table to store results for each location for a given c_l ratio
    rev_by_locations_c_l_ratio_dt <- 
      data.table(cost_loss_ratio = rep(c_l_ratio, length(locations)),
                 location = locations,
                 trigger_threshold = trigger_threshold_in_q,
                 rev = rev_by_location,
                 numerator = numerators,
                 denominator = denominators,
                 baseline_expense = e_baseline,
                 model_expense = e_model,
                 perfect_expense = e_perfect)
    rev_results_by_location_dt <- 
      rbind(rev_results_by_location_dt,
            rev_by_locations_c_l_ratio_dt)
    
    #3) REV across all locations (location-agnostic events)
    confusion_results_all_locations <- confusion_results[[3]]
    num_observations <- confusion_results_all_locations$Num_Observations
    tp <- confusion_results_all_locations$True_Positives/num_observations
    fp <- confusion_results_all_locations$False_Positives/num_observations
    fn <- confusion_results_all_locations$False_Negatives/num_observations
    tn <- confusion_results_all_locations$True_Negatives/num_observations
    stopifnot(round(tp + fn + fp + tn) == 1)
    # print(paste0("TP + FN:", tp + fn))
    x <- c(tp, fp, fn, tn)
    stopifnot(length(which(x < 0)) == 0) #If any of these are less than 0, you're in trouble!
    
    e_perfect <- (tp + fn)*c_l_ratio
    
    
    
    if(baseline_model){
      baseline_confusion_results_all_locations <- baseline_confusion_results[[3]]
      num_observations <- baseline_confusion_results_all_locations$Num_Observations
      baseline_tp <- baseline_confusion_results_all_locations$True_Positives/num_observations
      baseline_fp <- baseline_confusion_results_all_locations$False_Positives/num_observations
      baseline_fn <- baseline_confusion_results_all_locations$False_Negatives/num_observations
      baseline_tn <- baseline_confusion_results_all_locations$True_Negatives/num_observations
      stopifnot(round(baseline_tp + baseline_fn + baseline_fp + baseline_tn) == 1)
      e_baseline <- (baseline_tp + baseline_fp)*c_l_ratio + baseline_fn
    }
    else if(!(baseline_model)){
      #Historical average (climatological)
      e_baseline <- min(tp+fn, c_l_ratio)
    }
    
    e_model <- (tp + fp)*c_l_ratio + fn
    
    denominator_in_q <- e_baseline - e_perfect
    numerator_in_q <- e_baseline - e_model
    rev_all_locations <- (e_baseline - e_model)/(e_baseline - e_perfect)
    

    if(denominator_in_q == 0) {
      all_locs_location_agnostic_denominator_counter = all_locs_location_agnostic_denominator_counter + 1
      rev_all_locations_location_agnostic_events <- NA  # Assign NA or alternative handling
    }
    else{
      rev_all_locations_location_agnostic_events <- (e_baseline - e_model)/(e_baseline - e_perfect)
    }
    if(numerator_in_q < 0){
      all_locs_location_agnostic_numerator_counter = all_locs_location_agnostic_numerator_counter + 1
      rev_all_locations_location_agnostic_events <- 0
    }
    
    # print(paste0("REV all locations: ", rev_all_locations))
    # rev_all_locations <- (min(tp + fn, c_l_ratio) - (tp + fp)*c_l_ratio + fn)/(min(tp + fn, c_l_ratio) - (tp + fn)*c_l_ratio)
    rev_all_locations_c_l_ratio_dt <- 
      data.table(cost_loss_ratio  = c_l_ratio,
                 trigger_threshold = trigger_threshold_in_q,
                 rev = rev_all_locations_location_agnostic_events,
                 denominator = denominator_in_q,
                 numerator_in_q = numerator_in_q,
                 baseline_expense = e_baseline,
                 model_expense = e_model,
                 perfect_expense = e_perfect
                 )
    rev_results_all_locations_dt <- 
      rbind(rev_results_all_locations_dt,
            rev_all_locations_c_l_ratio_dt)
    
    
    
    
    #3) REV across all locations (location-specific events)
    
    confusion_results_all_locations_events_location_specific <- confusion_results[[2]]
    num_observations <- confusion_results_all_locations_events_location_specific$Num_Observations
    tp <- confusion_results_all_locations_events_location_specific$True_Positives/num_observations
    fp <- confusion_results_all_locations_events_location_specific$False_Positives/num_observations
    fn <- confusion_results_all_locations_events_location_specific$False_Negatives/num_observations
    tn <- confusion_results_all_locations_events_location_specific$True_Negatives/num_observations
    stopifnot(round(tp + fn + fp + tn) == 1)
    # print(paste0("TP + FN:", tp + fn))
    x <- c(tp, fp, fn, tn)
    stopifnot(length(which(x < 0)) == 0)
    
    e_perfect <- (tp + fn)*c_l_ratio
    if(baseline_model == TRUE){
      baseline_confusion_results_all_locations_events_location_specific <- baseline_confusion_results[[2]]
      num_observations <- baseline_confusion_results_all_locations_events_location_specific$Num_Observations
      baseline_tp <- baseline_confusion_results_all_locations_events_location_specific$True_Positives/num_observations
      baseline_fp <- baseline_confusion_results_all_locations_events_location_specific$False_Positives/num_observations
      baseline_fn <- baseline_confusion_results_all_locations_events_location_specific$False_Negatives/num_observations
      baseline_tn <- baseline_confusion_results_all_locations_events_location_specific$True_Negatives/num_observations
      stopifnot(round(baseline_tp + baseline_fn + baseline_fp + baseline_tn) == 1)
      e_baseline <- (baseline_tp + baseline_fp)*c_l_ratio + baseline_fn
    }
    else if(baseline_model == FALSE){
      #Historical average
      e_baseline <- min(tp+fn, c_l_ratio)
    }
    
    e_model <- (tp + fp)*c_l_ratio + fn
    # denominators[j] <- e_baseline - e_perfect
    # numerators[j] <- e_baseline - e_model
    denominator_in_q <- e_baseline - e_perfect
    numerator_in_q <- e_baseline - e_model
    rev_all_locations_location_specific_events <- (e_baseline - e_model)/(e_baseline - e_perfect)
    
    if(denominator_in_q == 0) {
      all_locs_location_specific_denominator_counter = all_locs_location_specific_denominator_counter + 1
      rev_all_locations_location_specific_events <- NA  # Assign NA or alternative handling
    }
    else{
      rev_all_locations_location_specific_events <- (e_baseline - e_model)/(e_baseline - e_perfect)
    }
    if(numerator_in_q < 0){
      all_locs_location_specific_numerator_counter = all_locs_location_specific_numerator_counter + 1
      rev_all_locations_location_specific_events <- 0
    }
    # print(paste0("REV all locations: ", rev_all_locations))
    # rev_all_locations <- (min(tp + fn, c_l_ratio) - (tp + fp)*c_l_ratio + fn)/(min(tp + fn, c_l_ratio) - (tp + fn)*c_l_ratio)
    rev_all_locations_location_specific_c_l_ratio_dt <- 
      data.table(cost_loss_ratio  = c_l_ratio,
                 trigger_threshold = trigger_threshold_in_q,
                 rev = rev_all_locations_location_specific_events,
                 denominator = denominator_in_q,
                 numerator_in_q = numerator_in_q)
    
    rev_location_specific_results_across_all_locations <- 
      rbind(rev_location_specific_results_across_all_locations,
            rev_all_locations_location_specific_c_l_ratio_dt)
    
    
  }
# }

  return(list(rev_results_by_location_dt,
              rev_results_all_locations_dt,
              rev_location_specific_results_across_all_locations,
              numerator_counter,
              denominator_counter,
              all_locs_location_agnostic_numerator_counter,
              all_locs_location_agnostic_denominator_counter,
              all_locs_location_specific_numerator_counter,
              all_locs_location_specific_denominator_counter))
}

all_models_rev_function <- function(event_threshold, #Fixed event threshold -> Can replace with upper observed quantile
                                    event_threshold_type,
                                    forecast_representation,
                                    all_models_data,
                                    baseline_model,
                                    baseline_model_name,
                                    model_names){
  all_models_data$model <- factor(all_models_data$model, levels = model_names)
  baseline_forecast_data <- subset(all_models_data, model == baseline_model_name)
  
  model_names <- unique(all_models_data$model)
  all_models_location_specific_individual_locations <- NULL
  all_models_location_agnostic_all_locations <- NULL
  
  all_models_location_specific_all_locations <- NULL
  
  for(i in 1:length(unique(model_names))){
    model_in_q <- model_names[i]
    model_in_q_forecast_data <- subset(all_models_data, model == model_in_q)
    for(j in 1:length(unique(all_models_data$horizon))){
      horizon_in_q <- unique(all_models_data$horizon)[j]
      horizon_specific_model_in_q_forecast_data <- subset(model_in_q_forecast_data, 
                                         horizon == horizon_in_q)
      horizon_specific_baseline_forecast_data <- subset(baseline_forecast_data, 
                                                        horizon == horizon_in_q)
      
      model_in_q_rev_results <- compute_relative_economic_value(event_threshold = event_threshold, #Fixed event threshold -> Can replace with upper observed quantile
                                                         event_threshold_type = event_threshold_type,
                                                         observed_data = horizon_specific_model_in_q_forecast_data,
                                                         forecast_representation = forecast_representation,
                                                         baseline_model = baseline_model,
                                                         baseline_data = horizon_specific_baseline_forecast_data)
      tmp_all_models_location_specific_individual_locations <- model_in_q_rev_results[[1]]
      tmp_all_models_location_specific_individual_locations[, model:= model_in_q]
      tmp_all_models_location_specific_individual_locations[, horizon:= horizon_in_q]
      
      tmp_all_models_location_agnostic_all_locations <- model_in_q_rev_results[[2]]
      tmp_all_models_location_agnostic_all_locations[, model:= model_in_q]
      tmp_all_models_location_agnostic_all_locations[, horizon:= horizon_in_q]
      
      tmp_all_models_location_specific_all_locations <- model_in_q_rev_results[[3]]
      tmp_all_models_location_specific_all_locations[, model:= model_in_q]
      tmp_all_models_location_specific_all_locations[, horizon:= horizon_in_q]
      
      
      all_models_location_specific_individual_locations <- rbind(all_models_location_specific_individual_locations,
                                                          tmp_all_models_location_specific_individual_locations)
      all_models_location_specific_all_locations <- rbind(all_models_location_specific_all_locations,
                                                          tmp_all_models_location_specific_all_locations)
      all_models_location_agnostic_all_locations <- rbind(all_models_location_agnostic_all_locations,
                                                          tmp_all_models_location_agnostic_all_locations)
      
    }
    #Trailing window
    
  }
  return(list(all_models_location_specific_individual_locations,
              all_models_location_agnostic_all_locations,
              all_models_location_specific_all_locations))
}


trailing_window_all_models_rev_function <- function(event_threshold, #Fixed event threshold -> Can replace with upper observed quantile
                                    event_threshold_type,
                                    forecast_representation,
                                    all_models_data,
                                    baseline_model,
                                    baseline_model_name,
                                    model_names,
                                    window_length){
  all_models_data$model <- factor(all_models_data$model, levels = model_names)
  setkeyv(all_models_data, c("target_end_date", "target_type", "model"))
  times <- unique(all_models_data$target_end_date)
  locations <- unique(all_models_data$location)
  runner_windows_list <- runner(times, k = window_length)
  #Sliding window with stride of 1 week
  runner_windows_list <- runner_windows_list[window_length:length(runner_windows_list)] 
  
  
  baseline_forecast_data <- subset(all_models_data, model == baseline_model_name)
  
  model_names <- unique(all_models_data$model)
  all_models_location_specific_individual_locations <- NULL
  all_models_location_agnostic_all_locations <- NULL
  
  all_models_location_specific_all_locations <- NULL
  
  
  for(t in seq(1, length(runner_windows_list), 1)){
    if(j%%50 == 0){gc()}
    times_in_q <- runner_windows_list[[t]]
    # print(times_in_q)
    min_time_in_q <- min(as.Date(times_in_q))
    max_time_in_q <- max(as.Date(times_in_q))
    print(paste0("Max date: ", max_time))
    windowed_models_data <- subset(all_models_data,
                                   target_end_date >= min_time_in_q 
                                   & target_end_date <= max_time_in_q)
    windowed_baseline_forecast_data <- subset(baseline_forecast_data,
                                   target_end_date >= min_time 
                                   & target_end_date <= max_time)
    
    for(i in 1:length(unique(model_names))){
    model_in_q <- model_names[i]
    model_in_q_forecast_data <- subset(windowed_models_data, model == model_in_q)
    for(j in 1:length(unique(model_in_q_forecast_data$horizon))){
      horizon_in_q <- unique(model_in_q_forecast_data$horizon)[j]
      horizon_specific_model_in_q_forecast_data <- subset(model_in_q_forecast_data, 
                                                          horizon == horizon_in_q)
      horizon_specific_baseline_forecast_data <- subset(windowed_baseline_forecast_data, 
                                                        horizon == horizon_in_q)
      
      
      #Using upper 10% of events within that 
      model_in_q_rev_results <- compute_relative_economic_value(event_threshold = event_threshold, #Fixed event threshold -> Can replace with upper observed quantile
                                                                event_threshold_type = event_threshold_type,
                                                                observed_data = horizon_specific_model_in_q_forecast_data,
                                                                forecast_representation = forecast_representation,
                                                                baseline_model = baseline_model,
                                                                baseline_data = horizon_specific_baseline_forecast_data)
      tmp_all_models_location_specific_individual_locations <- model_in_q_rev_results[[1]]
      tmp_all_models_location_specific_individual_locations[, model:= model_in_q]
      tmp_all_models_location_specific_individual_locations[, horizon:= horizon_in_q]
      tmp_all_models_location_specific_individual_locations[, max_time:= max_time_in_q]
      tmp_all_models_location_specific_individual_locations[, min_time:= min_time_in_q]
      
      tmp_all_models_location_agnostic_all_locations <- model_in_q_rev_results[[2]]
      tmp_all_models_location_agnostic_all_locations[, model:= model_in_q]
      tmp_all_models_location_agnostic_all_locations[, horizon:= horizon_in_q]
      tmp_all_models_location_agnostic_all_locations[, max_time:= max_time_in_q]
      tmp_all_models_location_agnostic_all_locations[, min_time:= min_time_in_q]
      
      tmp_all_models_location_specific_all_locations <- model_in_q_rev_results[[3]]
      tmp_all_models_location_specific_all_locations[, model:= model_in_q]
      tmp_all_models_location_specific_all_locations[, horizon:= horizon_in_q]
      tmp_all_models_location_specific_all_locations[, max_time:= max_time_in_q]
      tmp_all_models_location_specific_all_locations[, min_time:= min_time_in_q]
      
      
      all_models_location_specific_individual_locations <- rbind(all_models_location_specific_individual_locations,
                                                                 tmp_all_models_location_specific_individual_locations)
      all_models_location_specific_all_locations <- rbind(all_models_location_specific_all_locations,
                                                          tmp_all_models_location_specific_all_locations)
      all_models_location_agnostic_all_locations <- rbind(all_models_location_agnostic_all_locations,
                                                          tmp_all_models_location_agnostic_all_locations)
      
    }
    #Trailing window
    }
  }
  return(list(all_models_location_specific_individual_locations,
              all_models_location_agnostic_all_locations,
              all_models_location_specific_all_locations))
}


#REV Plotting Functions ----


plot_rev_individual_locations_location_specific_events <- function(all_models_results,
                                                                   plot_model_colours,
                                                                   plot_model_names,
                                                                   plot_model_levels,
                                                                   focus_specific_horizons = NULL){
  rev_location_specific_results_individual_locations <- all_models_results[1]
  if(!is.null(focus_specific_horizons)){
    rev_location_specific_results_individual_locations <- 
      subset(rev_location_specific_results_individual_locations, 
             horizon %in% focus_specific_horizons)
  }
  tmp_rev_results_individual_locations$model <- 
    factor(tmp_rev_results_individual_locations$model,
           levels = plot_model_levels)
  
  setkeyv(tmp_rev_results_individual_locations, c("horizon","location","cost_loss_ratio"))
  zero_rev_dt <- unique(subset(tmp_rev_results_individual_locations,
                               select = c("location", "model", "horizon")))
  zero_rev_dt[, cost_loss_ratio:=0]
  zero_rev_dt[, rev:=0]
  tmp_rev_results_individual_locations <- rbind(tmp_rev_results_individual_locations, 
                                                zero_rev_dt,
                                                fill = TRUE)
  #Loop over horizons and create separate plots for each horizon
  # rev_tile_plot_location_facets <- ggplot(tmp)+
  #   geom_tile(aes(x = model, y = horizon , fill = rev))+
  #   facet_wrap(location ~., )+
  #   scale_fill_viridis_c()
  # 
  # rev_tile_plot_location_facets <- ggplot(tmp)+
  #   geom_tile(aes(x = model, y = location , fill = rev))+
  #   facet_wrap(horizon ~., )+
  #   scale_fill_viridis_c()
  horizons <- unique(tmp_rev_results_individual_locations$horizon)
  for(i in 1:length(horizons)){
    horizon_in_q <- unique(horizons)[i]
  tmp <- tmp_rev_results_individual_locations[which(horizon == horizon_in_q)]
  
  #Single horizon REV boxplot across C/L ratios (facets = locations)
  boxplot_summary_across_individual_locations_and_cl_ratios <- 
    ggplot(tmp)+
    geom_boxplot(aes(x = model, y = rev , fill = model))+
    coord_flip()+
    facet_wrap(cost_loss_ratio ~., )
  
  #Single horizon REV plot across C/L ratios (facets = locations)
  rev_canonical_plot_by_location <- ggplot(tmp)+
    geom_line(aes(x  = cost_loss_ratio, y = rev, color = model))+
    theme_light()+
    facet_wrap(location ~., )+
    scale_color_manual("Model", values = covid_model_colours,
                       labels = corrected_covid_model_names)
  
  }
}
plot_rev_all_locations_location_specific_events <- function(all_models_results,
                                                            plot_model_colours,
                                                            plot_model_names,
                                                            plot_model_levels,
                                                            focus_specific_horizons = NULL,
                                                            restrict_x_axis = NULL,
                                                            horizon_target_types = NULL,
                                                            turn_off_facet_option = NULL){
  
  rev_location_specific_results_all_locations <- all_models_results[[3]]
  if(!is.null(focus_specific_horizons)){
    rev_location_specific_results_all_locations <- 
      subset(rev_location_specific_results_all_locations, 
             horizon %in% focus_specific_horizons)
  }
  rev_location_specific_results_all_locations$model <- 
    factor(rev_location_specific_results_all_locations$model,
           levels = plot_model_levels)
  # rev_location_specific_results_all_locations <- rev_location_specific_results_all_locations[rev_location_specific_results_all_locations[, .I[which.max(rev)], by = cost_loss_ratio]$V1]
  rev_location_specific_results_all_locations
  horizons <- unique(rev_location_specific_results_all_locations$horizon)
  num_horizons <- length(unique(rev_location_specific_results_all_locations$horizon))
  zero_rev_dt <- data.table(rev = 0, cost_loss_ratio = 0, model = rep(unique(rev_location_specific_results_all_locations$model),
                                                                      num_horizons))
  zero_rev_dt[, horizon:= rep(horizons, each = length(unique(model)))]
  rev_location_specific_results_all_locations <- rbind(rev_location_specific_results_all_locations,
                                                           zero_rev_dt, fill = TRUE)
  if(!is.null(horizon_target_types)){
    rev_location_specific_results_all_locations <- merge(rev_location_specific_results_all_locations,
                                                       horizon_target_types,
                                                       by = "horizon")
  }
  if(!is.null(turn_off_facet_option)){
    results_plot_list <- list()
    for(i in 1:length(unique(rev_location_specific_results_all_locations$horizon))){
    results_plot_list[[i]] <- ggplot(rev_location_specific_results_all_locations)+
      geom_line(aes(x = cost_loss_ratio, y = rev, color = model),
                linewidth = 1.2)+
      # geom_point(aes(x = cost_loss_ratio, y = rev, color = model),
      #            size = 3.0)+
      # facet_wrap(target_type ~., ncol = 2,
      #            scales = "free_x")+theme_light()+
      theme_custom()+
      coord_cartesian(ylim = c(0, 1))+
      ylab("REV") + xlab(expression(paste(alpha))) +
      labs(title = "Upper 10% Events")+
      scale_color_manual("Model", values = plot_model_colours,
                         labels = plot_model_names)+
      guides(colour = guide_legend(override.aes = list(linewidth = 2)))+ theme(panel.spacing = unit(2, "lines"))
    }
    results_plot <-  ggarrange(NULL, results_plot_list[[1]] + theme(axis.text.x = element_text(size=16 )), NULL ,
                               results_plot_list[[2]] + theme(axis.text.x = element_text(size=16)),
              nrow = 4,ncol = 1,
              legend = "none",
              labels = c("1 week ahead (REV)", "","4 week ahead (REV)", ""),
              font.label = list(size = 18),
              heights = c(0.1, 0.75, 0.1,0.75))
  }
  else{
    results_plot <- ggplot(rev_location_specific_results_all_locations)+
      geom_line(aes(x = cost_loss_ratio, y = rev, color = model,
                    linetype = model),
                alpha = 0.7,
                linewidth = 1.2)+
      geom_point(aes(x = cost_loss_ratio, y = rev, color = model),
                 alpha = 0.7,
                 size = 3.0)+
      facet_wrap(target_type ~., ncol = 2,
                 scales = "free_x")+theme_light()+
      theme_custom()+
      coord_cartesian(ylim = c(0, 1))+
      ylab("REV") + xlab(expression(paste(alpha))) +
      labs(title = "Upper 10% Events")+
      scale_color_manual("Model", values = plot_model_colours,
                         labels = plot_model_names)+
      guides(colour = guide_legend(override.aes = list(linewidth = 2)))+ theme(panel.spacing = unit(2, "lines"))
  }

  
  
  if(!is.null(restrict_x_axis)){
    results_plot <- results_plot + coord_cartesian(xlim = (c(0, restrict_x_axis)))
  }
  return(results_plot)
}





plot_rev_all_locations_location_agnostic_events <- function(all_models_results,
                                                            plot_model_colours,
                                                            plot_model_names,
                                                            plot_model_levels,
                                                            focus_specific_horizons = NULL,
                                                            restrict_x_axis = NULL,
                                                            horizon_target_types = NULL){
  
  rev_location_agnostic_results_all_locations <- all_models_results[[2]]
  if(!is.null(focus_specific_horizons)){
    rev_location_agnostic_results_all_locations <- 
      subset(rev_location_agnostic_results_all_locations, 
             horizon %in% focus_specific_horizons)
  }
  rev_location_agnostic_results_all_locations$model <- 
    factor(rev_location_agnostic_results_all_locations$model,
           levels = plot_model_levels)
  # rev_location_agnostic_results_all_locations <- rev_location_agnostic_results_all_locations[rev_location_agnostic_results_all_locations[, .I[which.max(rev)], by = cost_loss_ratio]$V1]
  rev_location_agnostic_results_all_locations
  horizons <- unique(rev_location_agnostic_results_all_locations$horizon)
  num_horizons <- length(unique(rev_location_agnostic_results_all_locations$horizon))
  zero_rev_dt <- data.table(rev = 0, cost_loss_ratio = 0, model = rep(unique(rev_location_agnostic_results_all_locations$model),
                                                                      num_horizons))
  zero_rev_dt[, horizon:= rep(horizons, each = length(unique(model)))]
  rev_location_agnostic_results_all_locations <- rbind(rev_location_agnostic_results_all_locations,
                                                       zero_rev_dt, fill = TRUE)
  rev_location_agnostic_results_all_locations <- merge(rev_location_agnostic_results_all_locations,
                                                       horizon_target_types,
                                                       by = "horizon")
  results_plot <- ggplot(rev_location_agnostic_results_all_locations)+
    geom_line(aes(x = cost_loss_ratio, y = rev, color = model),
              linewidth = 1)+
    # geom_point(aes(x = cost_loss_ratio, y = rev, color = model),
    #           size = 3.0)+
    facet_wrap(target_type ~., ncol = 2,
               scales = "free_x")+theme_light()+
    theme_custom()+
    coord_cartesian(ylim = c(0, 1))+
    ylab("REV") + xlab(expression(paste(alpha))) +
    labs(title = "Upper 10% Events")+
    scale_color_manual("Model", values = plot_model_colours,
                       labels = plot_model_names)+
    guides(colour = guide_legend(override.aes = list(linewidth = 2)))+
    theme(position = "bottom")+ theme(panel.spacing = unit(2, "lines"))
  if(!is.null(restrict_x_axis)){
    results_plot <- results_plot + coord_cartesian(xlim = (c(0, restrict_x_axis)))
  }
  return(results_plot)
}




#Don't need rest
covid_all_models_rev_upper_ten_percent_results
tmp_state_covid_forecasts2 <- subset(state_covid_forecasts2, target_end_date <= "2022-01-02")
tmp_state_covid_forecasts2 <- data.table(tmp_state_covid_forecasts2)
unique(tmp_state_covid_forecasts2$quantile_level)
tmp_state_covid_forecasts2 <- subset(tmp_state_covid_forecasts2, horizon == 1)
tmp_state_covid_forecasts2[, target_end_date:= as.Date(target_end_date)]
unique(tmp_state_covid_forecasts2$model)
tmp_baseline_state_covid_forecasts2 <- subset(state_level_covid_forecasts_dt, model == "COVIDhub-baseline")
tmp_state_covid_forecasts2 <- subset(state_level_covid_forecasts_dt, model == "COVIDhub-4_week_ensemble")
rev_args <- list()
rev_args$event_threshold <- 0.9
tmp_rev_results <- compute_relative_economic_value(event_threshold = 0.9, #Fixed event threshold -> Can replace with upper observed quantile
                                event_threshold_type = "fixed_quantile",
                                observed_data = tmp_state_covid_forecasts2,
                                forecast_representation = "quantile",
                                baseline_model = TRUE,
                                baseline_data = subset(state_level_covid_forecasts_dt,
                                                       model == "COVIDhub-baseline"))

tmp_rev_results_individual_locations <- covid_all_models_rev_upper_twenty_five_percent_results[[1]]
tmp_rev_results_individual_locations$model <- 
  factor(tmp_rev_results_individual_locations$model,
         levels = covid_model_levels)

setkeyv(tmp_rev_results_individual_locations, c("horizon","location","cost_loss_ratio"))
tmp_rev_results_individual_locations
# tmp_rev_results_individual_locations <- tmp_rev_results_individual_locations[tmp_rev_results_individual_locations[, .I[which.max(rev)], by = c("location", "cost_loss_ratio")]$V1]
# tmp_rev_results_individual_locations
#Numerator = E_clim - E_model
tmp_rev_results_individual_locations[which(numerator < 0)]
tmp_rev_results_individual_locations[which(denominator < 0)]
zero_rev_dt <- unique(subset(tmp_rev_results_individual_locations,
              select = c("location", "model", "horizon")))
zero_rev_dt[, cost_loss_ratio:=0]
zero_rev_dt[, rev:=0]
zero_rev_dt[, numerator:= NA]
zero_rev_dt[, denominator:= NA]
zero_rev_dt[, trigger_threshold:= NA]
tmp_rev_results_individual_locations <- rbind(tmp_rev_results_individual_locations, 
                                              zero_rev_dt,
                                              fill = TRUE)
tmp <- tmp_rev_results_individual_locations[which(cost_loss_ratio == 0.5)]
tmp <- tmp_rev_results_individual_locations[which(horizon == 1)]

#
ggplot(tmp)+
  geom_boxplot(aes(x = model, y = rev , fill = model))+
  coord_flip()+
  facet_wrap(cost_loss_ratio ~., )

ggplot(tmp)+
  geom_tile(aes(x = model, y = horizon , fill = rev))+
  facet_wrap(location ~., )+
  scale_fill_viridis_c()

ggplot(tmp)+
  geom_tile(aes(x = model, y = location , fill = rev))+
  facet_wrap(horizon ~., )+
  scale_fill_viridis_c()

ggplot(tmp)+
  geom_line(aes(x  = horizon, y = rev, color = model))+
  theme_light()+
  facet_wrap(location ~., )+
  scale_color_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)


tmp <- subset(tmp_rev_results_individual_locations,
              horizon == 1)
ggplot(tmp)+
  geom_line(aes(x = cost_loss_ratio, y = rev, color = model))+
  facet_wrap(location ~., scales = "free_y")+
  scale_color_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)
max_rev_results_individual_locations <- tmp_rev_results_individual_locations[tmp_rev_results_individual_locations[, .I[which.max(rev)], by = c("location")]$V1]
max_rev_results_individual_locations




tmp_rev_results_all_locations <- covid_all_models_rev_upper_ten_percent_results[[2]]
tmp_rev_results_all_locations$model <- 
  factor(tmp_rev_results_all_locations$model,
         levels = covid_model_levels)

# tmp_rev_results_all_locations <- tmp_rev_results_all_locations[tmp_rev_results_all_locations[, .I[which.max(rev)], by = cost_loss_ratio]$V1]
zero_rev_dt <- data.table(rev = 0, cost_loss_ratio = 0)
tmp_rev_results_all_locations <- rbind(tmp_rev_results_all_locations,
      zero_rev_dt, fill = TRUE)
tmp_rev_results_all_locations$model
ggplot(tmp_rev_results_all_locations)+
  geom_line(aes(x = cost_loss_ratio, y = rev, color = model))+
  facet_wrap(horizon ~., )+
  scale_color_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)


tmp_rev_location_specific_results_all_locations <- covid_all_models_rev_upper_ten_percent_results[[3]]
tmp_rev_location_specific_results_all_locations$model <- 
  factor(tmp_rev_location_specific_results_all_locations$model,
         levels = covid_model_levels)
# tmp_rev_location_specific_results_all_locations <- tmp_rev_location_specific_results_all_locations[tmp_rev_location_specific_results_all_locations[, .I[which.max(rev)], by = cost_loss_ratio]$V1]
tmp_rev_location_specific_results_all_locations
zero_rev_dt <- data.table(rev = 0, cost_loss_ratio = 0, model = rep(unique(tmp_rev_location_specific_results_all_locations$model),
                                                                    4))
zero_rev_dt
zero_rev_dt[, horizon:= rep(seq(1, 4, 1), each = length(unique(model)))]
tmp_rev_location_specific_results_all_locations <- rbind(tmp_rev_location_specific_results_all_locations,
                                       zero_rev_dt, fill = TRUE)
tmp_rev_location_specific_results_all_locations
tmp_rev_location_specific_results_all_locations
ggplot(tmp_rev_location_specific_results_all_locations)+
  geom_line(aes(x = cost_loss_ratio, y = rev, color = model))+
  facet_wrap(horizon ~., )+theme_light()+
  # xlim(c(0, 0.25))+
  scale_color_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)
tmp <- tmp_rev_location_specific_results_all_locations[which(cost_loss_ratio == 0.1)]
ggplot(tmp)+
  geom_line(aes(x  = horizon, y = rev, color = model))+
  theme_light()+
  scale_color_manual("Model", values = covid_model_colours,
                     labels = corrected_covid_model_names)

ggplot(tmp)+
  geom_tile(aes(x = model, y = horizon, fill = rev))+
  scale_fill_viridis_c()
#Does it make sense to compute REV for upper 10% of window's observations? Probably
trailing_window_specific_events_rev_function <- function(event_threshold, #Fixed event threshold -> Can replace with upper observed quantile
                                         event_threshold_type,
                                         forecast_representation,
                                         all_models_data,
                                         baseline_model = FALSE,
                                         baseline_model_name,
                                         model_names){
  #1st, computing scores across all locations in each rolling window
  for(j in seq(1, length(runner_windows_list), 1)){
    if(j%%50 == 0){gc()}
    times_in_q <- runner_windows_list[[j]]
    # print(times_in_q)
    min_time <- min(as.Date(times_in_q))
    max_time <- max(as.Date(times_in_q))
    print(paste0("max_time: ", max_time))
    all_data_at_time_in_q <-
      subset(all_models_data,
             target_end_date <= max_time &
               target_end_date >= min_time)
    tmp_score_summary <- data.table(tmp_score_summary)
    tmp_score_summary[, max_time:= as.Date(max_time)]
    tmp_score_summary[, min_time:= as.Date(min_time)]
    
    aggregate_score_results <-
      rbind(aggregate_score_results,
            tmp_score_summary)
    
  }
  
}
# 2) Murphy ----
# For triptych, see ex_binary: 
# We need column y for observed binary observation
  # and columns for different models' probabilistic forecasts of the event
# For quantile-based forecasts, use lowest quantile for which we predicted event?


# 3) Diagonal Score ---
#Computes DES for a single observation 
  # (can take mean for location, rolling window, location + window,
  #     all data)
compute_diagonal_elementary_score <- function(obs, ens, th, tau) {
  # obs: continuous observation (scalar)
  # ens: ensemble forecast (vector), e.g. samples
  # th: tau%-percentile of the climatology used as a threshold, theta (y > theta)
  # tau: quantile level in [0,1] alpha
  #Ens = samples from ensemble
  obs_ev <- (obs > th)  # Dichotomous observation (TRUE/FALSE)
  p <- mean(ens > th)   # Probability forecast (propn of samples greater than threshold)
  
  
  #Eq. 24 (BEN-BOUALLEGUE ET AL)
  ds <- obs_ev * (p <= (1 - tau)) * tau + (1 - obs_ev) * (p > (1 - tau)) * (1 - tau)
  
  return(ds)
}
compute_diagonal_score <- function(diagonal_elementary_score_vector){
  diagonal_score <- mean(diagonal_elementary_score_vector)
  return(diagonal_score)
}

# 4) ROC + AUC


#Predictability alongside scores ----
#1) Source: https://github.com/Emergent-Epidemics/infectious_disease_predictability/blob/master/R%20Code/limits_acc_functions.R
# Goal = 1) Overall historical all regions (vary rolling window length) -> Summarise all regions simultaneously. Do a pre-2023 analysis
#        2)  Overall historical by region (vary rolling window length) -> Same computation, show each region separately
      # 3) Historical by year (vary rolling window length) -> Apply to each year separately
#       4) Historical by year + region (vary rolling window length) Is forecast horizon spatially + temporally consistent?
#       5) Historical @ quantiles of epi curve, @ peak
#       6) Historical by year + region (fixed rolling window length) 

        

#Function for relative entropy
rel_ent <- function(x, m.min = 2, m.max = 6, 
                    t.min = 1, t.max = 3, 
                    min_time_steps = 6, do_mc_ent = FALSE){
  #m is the embedding dimension (d in the paper), often between 3 and 7
  #t is the time delay (tau in paper)
  #x is the data (can be all data or rolling window data)
  #min_time_steps is the minimum number of time steps
  if(length(x) < min_time_steps){
    stop("x must be at least min_time_steps time steps long")
  }
  test <- entropyHeuristic(X = x, m.min = m.min, m.max = m.max, t.min = t.min, t.max = t.max)
  ent <- min(test$entropy.values[,3], na.rm = TRUE)
  
  ent_check <- c()
  for(t in (length(x)-min_time_steps):length(x)){
    #entropyHeuristic = Automatically choose embedding dimension, loaded from package
    test.t <- entropyHeuristic(X = x[1:t], m.min = m.min, m.max = m.max, t.min = t.min, t.max = t.max)
    ent.t <- min(test.t$entropy.values[,3], na.rm = TRUE)
    ent_check <- c(ent_check, ent.t)
  }
  
  if(is.na(mean(abs(diff(ent_check)), na.rm = TRUE)) == TRUE){
    ent <- NA
  }
  
  if(is.na(mean(abs(diff(ent_check)), na.rm = TRUE)) == FALSE){
    if(mean(abs(diff(ent_check)), na.rm = TRUE) > 1e-2){
      ent <- NA
    }
  }
  
  ents <- c()
  ms <- c()
  taus <- c()
  if(do_mc_ent == TRUE){
    for(i in 1:min_time_steps){
      test.i <- entropyHeuristic(X = sample(x, length(x), replace = TRUE), m.min = m.min, m.max = m.max, t.min = t.min, t.max = t.max)
      ent.i <- min(test.i$entropy.values[,3], na.rm = TRUE)
      m.i <- test.i$m
      tau.i <- test.i$t
      ents <- c(ents, ent.i)
      ms <- c(ms, m.i)
      taus <- c(taus, tau.i)
    }
    use.rel.ent <- which.max(ents)
    rel.ent <- ents[use.rel.ent]
    rel.ent.m <- ms[use.rel.ent]
    rel.ent.tau <- taus[use.rel.ent]
    if(is.finite(rel.ent) == FALSE){
      rel.ent <- NA
      rel.ent.m <- NA
      rel.ent.tau <- NA
    }
  }else{
    rel.ent <- NA
    rel.ent.m <- NA
    rel.ent.tau <- NA
  }
  m <- test$m
  if(length(m) == 0){
    m <- NA
  }
  
  tau <- test$t
  if(length(tau) == 0){
    tau <- NA
  }
  
  return(list("rel.ent" = rel.ent,"ent" = ent, "ent.wave" = NA, 
              "n" = length(na.omit(x)), "d" = m, "tau" = tau, 
              "rel.ent.d" = rel.ent.m, "rel.ent.tau" = rel.ent.tau))
}


# if(is.na(ent.i$d) == TRUE) next
# ent.i <- rel_ent(x = x.i, m.min = 2, m.max = 5, t.min = 1, t.max = 20, do_mc_ent = FALSE)
# ent_w.i <- entropy(weighted_ordinal_pattern_distribution(x.i, ent.i$d))/log(factorial(ent.i$d))
# ent_raw.i <- entropy(codebook(x.i, m = ent.i$d, t = 1))/log(factorial(ent.i$d))
# 
# entropy(codebook())




pe_args <- list(m.min = 2,
                m.max = 5,
                t.min = 1,
                t.max = 20)
#Don't need n_iter (was used in Shapiro + Petri for selecting 1,000 random starting points)



#Should be really everything in same trailing window function

#1) Trailing/rolling window PE
  # note that most recent predictability may not be best guide for future
    # epidemics are autoregressive, yet also seasonal + evolving! 

#Forecasts in that window scored
#Or trailing scores in the window?
scores_and_permutation_entropy_trailing_window_over_time <- 
  #In real-time, could use data up to time t for x, 
  # and look at aggregate performance up to time t
  #  and/or most recent performance
  
  #Function for calculating PE in trailing windows of specified length
  function(x, #data
           window_length,
           column_name = "observed",
           pe_args,
           forecast_representation,
           real_time = NULL,
           min_time_for_aggregate_performance = NULL){
    #real_time is an option to set what the current date is (evaluating all forecasts previous to that date)
    if(!is.null(real_time)){
      x <- subset(x, forecast_date < real_time)
    }
  # print(times)
  setkeyv(x, c("target_end_date", "target_type", "model"))
  times <- unique(x$target_end_date)
  locations <- unique(x$location)
  runner_windows_list <- runner(times, k = window_length)
  #Max index is current time point
  #Sliding window with stride of 1 week
  runner_windows_list <- runner_windows_list[window_length:length(runner_windows_list)] 
  
  #Forecast skill across all time points + all locations
  if(forecast_representation == "quantile"){
    models_dt <- as_forecast_quantile(x,
                                      observed = "observed",
                                      predicted = "predicted",
                                      model = "model",
                                      quantile_level = "quantile_level",
                                      forecast_unit = c("model", "location", "target_end_date",
                                                        "target_type", "horizon"))
  }
  else if(forecast_representation == "sample"){
    models_dt <- as_forecast_sample(x,
                                    observed = "observed",
                                    predicted = "predicted",
                                    model = "model",
                                    sample_id = "sampled_id",
                                    forecast_unit = c("model", "location", "target_end_date",
                                                      "target_type", "horizon"))
  }
  if(!is.null(min_time_for_aggregate_performance)){ #For trailing window, want aggregate in window
    within_window_models_dt <- subset(models_dt,
                                        target_end_date >= min_time_for_aggregate_performance)
    all_scores_all_locations_summary <- within_window_models_dt %>%
      score() %>%
      get_pairwise_comparisons(by = c("target_type", "target_end_date"),
                               compare = "model",
                               metric = "wis",
                               baseline = "COVIDhub-baseline")
    
    #Not doing pairwise comparison
    all_scores_individual_locations_summary <- within_window_models_dt %>%
      score() 
    
  }
  else{
  all_scores_all_locations_summary <- models_dt %>%
    score() %>%
    get_pairwise_comparisons(by = c("target_type", "target_end_date"),
                             compare = "model",
                             metric = "wis",
                             baseline = "COVIDhub-baseline")
  
  #Not doing pairwise comparison
  all_scores_individual_locations_summary <- models_dt %>%
    score() 
  }
  
  # tmp_score_summary <- models_dt %>%
  #   score() %>%
  #   summarise_scores(by = c("model", "target_type"))
  
  # %>%
  #   get_pairwise_comparisons(by = c("target_type", "target_end_date", "location"),
  #                            compare = "model",
  #                            metric = "wis",
  #                            baseline = "COVIDhub-baseline")
  print("First two pairwise scores done")
  all_scores_all_locations_summary <- data.table(all_scores_all_locations_summary)
  all_scores_individual_locations_summary <- data.table(all_scores_individual_locations_summary)
  #Setting up data.tables for mean score within each window and trailing score
  trailing_scores_summary <- NULL
  aggregate_score_results <- NULL
  
  
  
  #1st, computing scores across all locations in each rolling window
  for(j in seq(1, length(runner_windows_list), 1)){
    if(j%%50 == 0){gc()}
    times_in_q <- runner_windows_list[[j]]
    # print(times_in_q)
    min_time <- min(as.Date(times_in_q))
    max_time <- max(as.Date(times_in_q))
    print(paste0("max_time: ", max_time))
    all_data_at_time_in_q <-
      subset(x,
             target_end_date <= max_time &
               target_end_date >= min_time)
    # print(all_data_at_time_in_q)
    tmp_trailing_scores_summary <- subset(all_scores_all_locations_summary,
                                      target_end_date <= max_time &
                                        target_end_date >= min_time)
    tmp_trailing_scores_summary[, max_time:= as.Date(max_time)]
    tmp_trailing_scores_summary[, min_time:= as.Date(min_time)]
    
    trailing_scores_summary <- rbind(trailing_scores_summary,
                                     tmp_trailing_scores_summary)
    #Forecast skill across the window (all locations)
    if(forecast_representation == "quantile"){
      models_dt <- as_forecast_quantile(all_data_at_time_in_q,
                                        observed = "observed",
                                        predicted = "predicted",
                                        model = "model",
                                        quantile_level = "quantile_level",
                                        forecast_unit = c("model", "location", "target_end_date",
                                                          "target_type", "horizon"))
    }
    else if(forecast_representation == "sample"){
      models_dt <- as_forecast_sample(all_data_at_time_in_q,
                                        observed = "observed",
                                        predicted = "predicted",
                                        model = "model",
                                        sample_id = "sampled_id",
                                        forecast_unit = c("model", "location", "target_end_date",
                                                          "target_type", "horizon"))
    }

    # tmp_score_summary <- models_dt %>%
    #   score() %>%
    #   summarise_scores(by = c("model", "target_type"))
    
    
    #This is the score summary in rolling windows
    tmp_score_summary <- models_dt %>%
      score() %>%
      get_pairwise_comparisons(by = c("target_type"),
                               compare = "model",
                               metric = "wis",
                               baseline = "COVIDhub-baseline")    
    tmp_score_summary <- data.table(tmp_score_summary)
    tmp_score_summary[, max_time:= as.Date(max_time)]
    tmp_score_summary[, min_time:= as.Date(min_time)]

    aggregate_score_results <-
      rbind(aggregate_score_results,
            tmp_score_summary)


  }

  #PE is calculated for each location separately
  #Can also calculate corresponding forecast scores 
  all_locations_entropy_results <- NULL
  all_locations_score_results <- NULL
  trailing_scores_by_location <- NULL
  for(l in 1:length(unique(locations))){
    print(paste0("Location ", l))
    location_in_q <- unique(locations)[l]
    location_data <- subset(x, 
                            location == location_in_q)
    setkeyv(location_data, c("target_end_date", "target_type", "model"))
    location_times <- unique(location_data$target_end_date)
    
    location_runner_windows_list <- runner(location_times, k = window_length)
    #Max index is current time point
    #Sliding window with stride of 1 week
    location_runner_windows_list <- location_runner_windows_list[window_length:length(location_runner_windows_list)] 
    
    location_entropy_results <- NULL
    location_score_summary <- NULL
    
    #Loop over each list of window indices
    for(i in seq(1, length(location_runner_windows_list), 1)){
      # print(i)
      if(i%%50 == 0){gc()}
      
      times_in_q <- location_runner_windows_list[[i]]
      # print(times_in_q)
      min_time <- min(times_in_q)
      max_time <- max(times_in_q)
      # tmp_scores_by_location <- subset(all_scores_individual_locations_summary,
      #                                           target_end_date <= max_time &
      #                                             target_end_date >= min_time & location == location_in_q)
      # tmp_trailing_scores_by_location <- tmp_scores_by_location %>% get_pairwise_comparisons(by = c("target_type"),
      #                                                                                          compare = "model",
      #                                                                                          metric = "wis",
      #                                                                                          baseline = "COVIDhub-baseline")
      # # tmp_trailing_scores_by_location <- subset(all_scores_individual_locations_summary,
      # #                                   target_end_date <= max_time &
      # #                                     target_end_date >= min_time & location == location_in_q)
      # trailing_scores_by_location <- rbind(trailing_scores_by_location,
      #                                  tmp_trailing_scores_summary)
      
      location_data_at_time_in_q <- 
        subset(location_data,
               target_end_date <= max_time &
                 target_end_date >= min_time)
      
      #Subset to observed data:
      true_location_data_at_time_in_q <-
        unique(subset(location_data_at_time_in_q,
                      select = c("target_end_date", column_name)))
      
      #Then, PE
      x.i <- as.numeric(unlist(unname(true_location_data_at_time_in_q)))
      x.i <- x.i[which(!is.na(x.i))]
      if(length(x.i) < 10){
        next
      }

      
      #Relative entropy
      ent.i <- rel_ent(x = x.i, 
                       m.min = pe_args$m.min, 
                       m.max = pe_args$m.max, 
                       t.min = pe_args$t.min, 
                       t.max = pe_args$t.max, 
                       do_mc_ent = FALSE) #Selecting d (embedding dimension for measuring pattern)
      # print(ent_i)
      
      #Checking for any erros
      if(is.na(ent.i$d)){
        next
      }
      
      #Weighted version (for checking, not as appopriate)
      ent_w.i <- entropy(weighted_ordinal_pattern_distribution(x.i, ent.i$d))/log(factorial(ent.i$d))
      
      #Main result
      ent_raw.i <- entropy(codebook(x.i, m = ent.i$d, t = 1))/log(factorial(ent.i$d))
      
      #Store in data.table
      location_entropy_results_at_time_in_q <- 
        data.table(location = location_in_q,
                   min_time = as.Date(min_time),
                   embedding_dimension = ent.i$d, #Match to forecast horizon
                   max_time = as.Date(max_time),
                   raw_perm_entropy = ent_raw.i,
                   weighted_perm_entropy = ent_w.i,
                   pdc_perm_entropy = ent.i$ent,
                   total_cases = sum(x.i, na.rm = TRUE),
                   mean_cases = mean(x.i, na.rm = TRUE))
      
      location_entropy_results <- rbind(location_entropy_results,
                                        location_entropy_results_at_time_in_q)
      
      
      #Forecast skill across individual models for each location separately
      #These are all forecasts for time period within window for which predictability was just assessed
      
      models_dt <- scoringutils::as_forecast_quantile(location_data_at_time_in_q,
                                        observed = "observed",
                                        predicted = "predicted",
                                        model = "model",
                                        quantile_level = "quantile_level",
                                        forecast_unit = c("model", "location", "target_end_date",
                                                          "target_type"))
      # tmp_score_summary <- models_dt %>%
      #   score() %>%
      #   summarise_scores(by = c("model", "location", "target_type"))
      
      #We are at only one location -> so just by = c("target_type")!
      tmp_score_summary <- models_dt %>%
        score() %>%
        get_pairwise_comparisons(by = c("target_type"),
                                 compare = "model",
                                 metric = "wis",
                                 baseline = "COVIDhub-baseline")
      tmp_score_summary <- data.table(tmp_score_summary)
      tmp_score_summary[, max_time:= as.Date(max_time)]
      tmp_score_summary[, min_time:= as.Date(min_time)]
      tmp_score_summary[, location:= location_in_q]
      location_score_summary <- rbind(location_score_summary,
                             tmp_score_summary)
      

    }
    all_locations_entropy_results <- rbind(all_locations_entropy_results,
                                   location_entropy_results)
    all_locations_score_results <- rbind(all_locations_score_results,
                                         location_score_summary)
  }
  return(list(aggregate_score_results, #Aggregate performance in the rolling windows (mean across all locations)
              all_locations_entropy_results, #Permutation entropy by individual location
              all_locations_score_results, #Individual location score summaries
              all_scores_individual_locations_summary, #Scores for computing trailing average after
              trailing_scores_summary)) #Trailing average of scores (as opposed to the mean score in the trailing window)
}

cleaned_state_and_national_covid_forecasts[, length(observed), by = c("location", "target_end_date")]
max(cleaned_state_and_national_covid_forecasts$target_end_date)
tmp <- unique(subset(cleaned_state_and_national_covid_forecasts, select = c("location", "target_end_date", "observed")))
unique(tmp[, length(observed), by = "target_end_date"]$V1)
unique(state_covid_forecasts2$target_end_date)
tmp_state_and_national_covid_forecasts <- subset(state_covid_forecasts2,
                                                 target_end_date <= "2020-12-13")
tmp_state_and_national_covid_forecasts
tmp <- as_forecast_quantile(tmp_state_and_national_covid_forecasts,
                                  observed = "observed",
                                  predicted = "predicted",
                                  model = "model",
                                  quantile_level = "quantile_level",
                                  forecast_unit = c("model", "location", "target_end_date",
                                                    "target_type", "horizon"))
tmp_all_locations_summary <- tmp %>%
  score()
class(tmp_all_locations_summary)
tmp_all_locations_summary <- tmp %>%
  score() %>%
  get_pairwise_comparisons(by = c("target_type", "target_end_date"),
                           compare = "model",
                           metric = "wis",
                           baseline = "COVIDhub-baseline")
tmp_all_locations_summary
tmp_all_scores_individual_locations_summary <- tmp %>%
  score() %>%
  get_pairwise_comparisons(by = c("target_type", "target_end_date", "location"),
                           compare = "model",
                           metric = "wis",
                           baseline = "COVIDhub-baseline")
class(tmp_all_scores_individual_locations_summary)
tmp_state_and_national_covid_forecasts <- subset(state_covid_forecasts2,
                                                 target_end_date <= "2020-12-13")
scores_pe_12_wk_rollings_window_new <- scores_and_permutation_entropy_trailing_window_over_time(tmp_state_and_national_covid_forecasts, #data
                                                                                            window_length = 12,
                                                                                            column_name = "observed",
                                                                                            pe_args = pe_args,
                                                                                            forecast_representation = "quantile")
scores_pe_12_wk_rollings_window
#Calculate entropy(x_i, )




scores_and_pointwise_trailing_window_over_time <- 
  #In real-time, could use data up to time t for x, 
  # and look at aggregate performance up to time t
  #  and/or most recent performance
  
  #Function for calculating PE in trailing windows of specified length
  function(x, #data
           window_length,
           column_name = "observed",
           forecast_representation,
           real_time = NULL){
    #real_time is an option to set what the current date is (evaluating all forecasts previous to that date)
    if(!is.null(real_time)){
      x <- subset(x, forecast_date < real_time)
    }
    # print(times)
    setkeyv(x, c("target_end_date", "target_type", "model"))
    times <- unique(x$target_end_date)
    locations <- unique(x$location)
    runner_windows_list <- runner(times, k = window_length)
    #Max index is current time point
    #Sliding window with stride of 1 week
    runner_windows_list <- runner_windows_list[window_length:length(runner_windows_list)] 
    
    #Forecast skill across all time points + all locations
    if(forecast_representation == "quantile"){
      models_dt <- as_forecast_quantile(x,
                                        observed = "observed",
                                        predicted = "predicted",
                                        model = "model",
                                        quantile_level = "quantile_level",
                                        forecast_unit = c("model", "location", "target_end_date",
                                                          "target_type", "horizon"))
    }
    else if(forecast_representation == "sample"){
      models_dt <- as_forecast_sample(x,
                                      observed = "observed",
                                      predicted = "predicted",
                                      model = "model",
                                      sample_id = "sampled_id",
                                      forecast_unit = c("model", "location", "target_end_date",
                                                        "target_type", "horizon"))
    }
    
    all_scores_all_locations_summary <- models_dt %>%
      score() %>%
      get_pairwise_comparisons(by = c("target_type", "target_end_date"),
                               compare = "model",
                               metric = "wis",
                               baseline = "COVIDhub-baseline")
    
    #Not doing pairwise comparison
    all_scores_individual_locations_summary <- models_dt %>%
      score() 
    
    # tmp_score_summary <- models_dt %>%
    #   score() %>%
    #   summarise_scores(by = c("model", "target_type"))
    
    # %>%
    #   get_pairwise_comparisons(by = c("target_type", "target_end_date", "location"),
    #                            compare = "model",
    #                            metric = "wis",
    #                            baseline = "COVIDhub-baseline")
    print("First two pairwise scores done")
    all_scores_all_locations_summary <- data.table(all_scores_all_locations_summary)
    all_scores_individual_locations_summary <- data.table(all_scores_individual_locations_summary)
    #Setting up data.tables for mean score within each window and trailing score
    trailing_scores_summary <- NULL
    aggregate_score_results <- NULL
    
    
    
    #1st, computing scores across all locations in each rolling window
    for(j in seq(1, length(runner_windows_list), 1)){
      if(j%%50 == 0){gc()}
      times_in_q <- runner_windows_list[[j]]
      # print(times_in_q)
      min_time <- min(as.Date(times_in_q))
      max_time <- max(as.Date(times_in_q))
      print(paste0("max_time: ", max_time))
      all_data_at_time_in_q <-
        subset(x,
               target_end_date <= max_time &
                 target_end_date >= min_time)
      # print(all_data_at_time_in_q)
      tmp_trailing_scores_summary <- subset(all_scores_all_locations_summary,
                                            target_end_date <= max_time &
                                              target_end_date >= min_time)
      tmp_trailing_scores_summary[, max_time:= as.Date(max_time)]
      tmp_trailing_scores_summary[, min_time:= as.Date(min_time)]
      
      trailing_scores_summary <- rbind(trailing_scores_summary,
                                       tmp_trailing_scores_summary)
      #Forecast skill across the window (all locations)
      if(forecast_representation == "quantile"){
        models_dt <- as_forecast_quantile(all_data_at_time_in_q,
                                          observed = "observed",
                                          predicted = "predicted",
                                          model = "model",
                                          quantile_level = "quantile_level",
                                          forecast_unit = c("model", "location", "target_end_date",
                                                            "target_type", "horizon"))
      }
      else if(forecast_representation == "sample"){
        models_dt <- as_forecast_sample(all_data_at_time_in_q,
                                        observed = "observed",
                                        predicted = "predicted",
                                        model = "model",
                                        sample_id = "sampled_id",
                                        forecast_unit = c("model", "location", "target_end_date",
                                                          "target_type", "horizon"))
      }
      
      # tmp_score_summary <- models_dt %>%
      #   score() %>%
      #   summarise_scores(by = c("model", "target_type"))
      
      
      #This is the score summary in rolling windows
      tmp_score_summary <- models_dt %>%
        score() %>%
        get_pairwise_comparisons(by = c("target_type"),
                                 compare = "model",
                                 metric = "wis",
                                 baseline = "COVIDhub-baseline")    
      tmp_score_summary <- data.table(tmp_score_summary)
      tmp_score_summary[, max_time:= as.Date(max_time)]
      tmp_score_summary[, min_time:= as.Date(min_time)]
      
      aggregate_score_results <-
        rbind(aggregate_score_results,
              tmp_score_summary)
      
      
    }
    
    #Calculated for each location separately
    all_locations_score_results <- NULL
    all_locations_pointwise_metrics_results <- NULL
    all_locations_coverage_results <- NULL
    
    trailing_scores_by_location <- NULL
    for(l in 1:length(unique(locations))){
      print(paste0("Location ", l))
      location_in_q <- unique(locations)[l]
      location_data <- subset(x, 
                              location == location_in_q)
      setkeyv(location_data, c("target_end_date", "target_type", "model"))
      location_times <- unique(location_data$target_end_date)
      
      location_runner_windows_list <- runner(location_times, k = window_length)
      #Max index is current time point
      #Sliding window with stride of 1 week
      location_runner_windows_list <- location_runner_windows_list[window_length:length(location_runner_windows_list)] 
      
      #Single location all scores, pointwise metrics, + coverage
      location_score_summary <- NULL
      location_pointwise_metrics <- NULL
      location_coverage_summary <- NULL

      #Loop over each list of window indices
      for(i in seq(1, length(location_runner_windows_list), 1)){
        # print(i)
        if(i%%50 == 0){gc()}
        
        times_in_q <- location_runner_windows_list[[i]]
        # print(times_in_q)
        min_time <- min(times_in_q)
        max_time <- max(times_in_q)
        location_data_at_time_in_q <- 
          subset(location_data,
                 target_end_date <= max_time &
                   target_end_date >= min_time)
        
        #Subset to observed data:
        true_location_data_at_time_in_q <-
          unique(subset(location_data_at_time_in_q,
                        select = c("target_end_date", column_name)))

        #Forecast skill across individual models for each location separately
        #These are all forecasts for time period within window for which predictability was just assessed
        
        models_dt <- scoringutils::as_forecast_quantile(location_data_at_time_in_q,
                                                        observed = "observed",
                                                        predicted = "predicted",
                                                        model = "model",
                                                        quantile_level = "quantile_level",
                                                        forecast_unit = c("model", "location", "target_end_date",
                                                                          "target_type"))

        #We are at only one location -> so just by = c("target_type")!
        tmp_score_summary <- models_dt %>%
          score() %>%
          get_pairwise_comparisons(by = c("target_type"),
                                   compare = "model",
                                   metric = "wis",
                                   baseline = "COVIDhub-baseline")
        tmp_score_summary <- data.table(tmp_score_summary)
        tmp_score_summary[, max_time:= as.Date(max_time)]
        tmp_score_summary[, min_time:= as.Date(min_time)]
        tmp_score_summary[, location:= location_in_q]
        location_score_summary <- rbind(location_score_summary,
                                        tmp_score_summary)
        
        
        tmp_pointwise_metrics <- pointwise_metrics_quantile_predictions(models_dt)
        tmp_pointwise_metrics[, max_time:= as.Date(max_time)]
        tmp_pointwise_metrics[, min_time:= as.Date(min_time)]
        tmp_pointwise_metrics[, location:= location_in_q]
        
        location_pointwise_metrics <- rbind(location_pointwise_metrics,
                                            tmp_pointwise_metrics)
        tmp_location_coverage <- models_dt %>%
          get_coverage(by = c("model", "target_type"))
        tmp_location_coverage <- data.table(tmp_location_coverage)
        tmp_location_coverage[, max_time:= as.Date(max_time)]
        tmp_location_coverage[, min_time:= as.Date(min_time)]
        tmp_location_coverage[, location:= location_in_q]
        
        location_coverage_summary <- rbind(location_coverage_summary,
                                           tmp_location_coverage)
      }
      all_locations_score_results <- rbind(all_locations_score_results,
                                           location_score_summary)
      all_locations_pointwise_metrics_results <- 
        rbind(all_locations_pointwise_metrics_results,
              location_pointwise_metrics)
      all_locations_coverage_results <- 
        rbind(all_locations_coverage_results,
              location_coverage_summary)
  
    }
    return(list(all_locations_score_results, #Individual location score summaries
                all_scores_individual_locations_summary,
                all_locations_pointwise_metrics_results,
                all_locations_coverage_results)) #Scores for computing trailing average after
}

# tmp <- subset(state_level_covid_forecasts_dt, target_end_date >= "2021-11-27")
# models_dt <- scoringutils::as_forecast_quantile(tmp,
#                                                 observed = "observed",
#                                                 predicted = "predicted",
#                                                 model = "model",
#                                                 quantile_level = "quantile_level",
#                                                 forecast_unit = c("model", "location", "target_end_date",
#                                                                   "target_type"))
plot_rolling_window_rwis <- function(window_results){
  performance_window_dt <- 
    window_results
  performance_window_dt <- 
    subset(performance_window_dt,
           compare_against == "COVIDhub-baseline")
  performance_window_dt$model_factor <- 
    factor(performance_window_dt$model,
           levels = covid_model_levels)
  visualisation_performance_window_dt <- 
    subset(performance_window_dt,
           target_type %in% covid_visualisation_target_types)
  visualisation_performance_window_dt
  
  trailing_window_wis_covid_plot <- 
    ggplot(visualisation_performance_window_dt)+
    geom_line(aes(x = max_time, y = wis_scaled_relative_skill,
                  color = model_factor),
              linewidth = 1.3)+
    facet_wrap(target_type ~., 
               ncol = 2)+
    # geom_hline(aes(yintercept = 1),
    #            linewidth = 1.4,
    #            linetype = "dashed")+
    theme_light()+theme_custom()+
    scale_x_date(date_labels = "%b %Y") +
    theme(axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          plot.title = element_text(size = 20, hjust = 0.5),
          legend.position = "bottom",
          legend.key.size = unit(1.5, "cm"),
          legend.text = element_text(size = 20),  # Adjust legend text size
          legend.title = element_text(size = 20))+
    labs(y = "rWIS", x = "Date" )+
    theme(panel.spacing = unit(1, "cm"))+  # increase to desired spacing
    scale_color_manual("Model", values = covid_model_colours,
                       labels = corrected_covid_model_names)
  return(trailing_window_wis_covid_plot)
}


plot_rolling_window_rpl <- function(window_results,
                                    chosen_quantiles){
  performance_window_dt <- 
    window_results
  performance_window_dt$model_factor <- 
    factor(performance_window_dt$model,
           levels = covid_model_levels)
  performance_window_dt <- merge(performance_window_dt,
        horizon_target_types,
        by = "horizon")
  visualisation_performance_window_dt <- 
    subset(performance_window_dt,
           target_type %in% covid_visualisation_target_types)
  visualisation_performance_window_dt <- 
    subset(visualisation_performance_window_dt,
           quantile_level %in% chosen_quantiles)
  # print(visualisation_performance_window_dt)
  visualisation_performance_window_dt[, alpha :=  1-as.numeric(as.character(quantile_level))]
  visualisation_performance_window_dt[, alpha_label := paste0("alpha==", alpha)]
  
  trailing_window_pl_covid_plot <- 
    ggplot(visualisation_performance_window_dt)+
    geom_line(aes(x = max_time, y = relative_score,
                  color = model_factor),
              linewidth = 1.3)+
    facet_wrap(target_type ~ alpha, 
               nrow = 2,  
               scales = "free_y")+
    # geom_hline(aes(yintercept = 1),
    #            linewidth = 1.4,
    #            linetype = "dashed")+
    theme_light()+theme_custom()+
    scale_x_date(date_labels = "%b %Y") +
    theme(axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          plot.title = element_text(size = 20, hjust = 0.5),
          legend.position = "bottom",
          legend.key.size = unit(1.5, "cm"),
          legend.text = element_text(size = 20),  # Adjust legend text size
          legend.title = element_text(size = 20))+
    labs(y = "rPL", x = "Date" )+
    theme(panel.spacing = unit(1, "cm"))+  # increase to desired spacing
    scale_color_manual("Model", values = covid_model_colours,
                       labels = corrected_covid_model_names)
  return(trailing_window_pl_covid_plot)
}

plot_rolling_window_predictability <- function(window_results){
  trailing_window_predictability_plot <- 
    ggplot(window_results)+
    geom_line(aes(x = max_time, y = predictability),
              linewidth = 1.3)+
    geom_point(aes(x =  max_time, y = predictability), size = 3)+
  
    # facet_wrap(target_type ~ quantile_level, 
    #            ncol = 2)+
    # geom_hline(aes(yintercept = 1),
    #            linewidth = 1.4,
    #            linetype = "dashed")+
    theme_light()+theme_custom()+
    scale_x_date(date_labels = "%b %Y") +
    theme(axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          plot.title = element_text(size = 20, hjust = 0.5),
          legend.position = "bottom",
          legend.key.size = unit(1.5, "cm"),
          legend.text = element_text(size = 20),  # Adjust legend text size
          legend.title = element_text(size = 20))+
    labs(y = "Predictability", x = "Date" )
  return(trailing_window_predictability_plot)
}


rolling_window_rt <- function(rt_results,
                              column_name){
    all_location_Rt_summary <- NULL
    for(l in 1:length(unique(locations))){
      print(paste0("Location ", l))
      location_in_q <- unique(locations)[l]
      location_data <- subset(rt_results, 
                              location == location_in_q)
      setkeyv(location_data, c("target_end_date"))
      location_times <- unique(location_data$target_end_date)
      location_runner_windows_list <- runner(location_times, k = window_length)
      #Max index is current time point
      #Sliding window with stride of 1 week
      location_runner_windows_list <- location_runner_windows_list[window_length:length(location_runner_windows_list)] 
      location_Rt_summary <- NULL
      #Loop over each list of window indices
      for(i in seq(1, length(location_runner_windows_list), 1)){
        # print(i)
        if(i%%50 == 0){gc()}
        
        times_in_q <- location_runner_windows_list[[i]]
        # print(times_in_q)
        min_time <- min(times_in_q)
        max_time <- max(times_in_q)
        location_data_at_time_in_q <- 
          subset(location_data,
                 target_end_date <= max_time &
                   target_end_date >= min_time)
        
        #Subset to observed data:
        true_location_data_at_time_in_q <-
          unique(subset(location_data_at_time_in_q,
                        select = c(column_name, "target_end_date")))
        true_location_data_at_time_in_q[, target_end_date:= NULL]
        tmp_rolling_median_Rt <- median(as.numeric(unlist(true_location_data_at_time_in_q)))
        tmp_rolling_mean_Rt <- mean(as.numeric(unlist(true_location_data_at_time_in_q)))
        
        tmp_location_Rt_summary <- data.table(rolling_median_Rt = tmp_rolling_median_Rt,
                                              rolling_mean_Rt = tmp_rolling_mean_Rt,
                                          max_time = max_time,
                                          min_time = min_time,
                                          location = location_in_q)
        tmp_location_Rt_summary[, max_time:= as.Date(max_time)]
        tmp_location_Rt_summary[, min_time:= as.Date(min_time)]
        tmp_location_Rt_summary[, location:= location_in_q]
        location_Rt_summary <- rbind(location_Rt_summary,
                                        tmp_location_Rt_summary)
        
      }
      all_location_Rt_summary <- rbind(all_location_Rt_summary,
                                           location_Rt_summary)
    }
    return(all_location_Rt_summary)
}

rolling_window_cases <- function(cases_results,
                              column_name){
  all_location_cases_summary <- NULL
  for(l in 1:length(unique(locations))){
    print(paste0("Location ", l))
    location_in_q <- unique(locations)[l]
    location_data <- subset(cases_results, 
                            location == location_in_q)
    setkeyv(location_data, c("target_end_date"))
    location_times <- unique(location_data$target_end_date)
    location_runner_windows_list <- runner(location_times, k = window_length)
    #Max index is current time point
    #Sliding window with stride of 1 week
    location_runner_windows_list <- location_runner_windows_list[window_length:length(location_runner_windows_list)] 
    location_cases_summary <- NULL
    #Loop over each list of window indices
    for(i in seq(1, length(location_runner_windows_list), 1)){
      # print(i)
      if(i%%50 == 0){gc()}
      
      times_in_q <- location_runner_windows_list[[i]]
      # print(times_in_q)
      min_time <- min(times_in_q)
      max_time <- max(times_in_q)
      location_data_at_time_in_q <- 
        subset(location_data,
               target_end_date <= max_time &
                 target_end_date >= min_time)
      
      #Subset to observed data:
      true_location_data_at_time_in_q <-
        unique(subset(location_data_at_time_in_q,
                      select = c(column_name, "target_end_date")))
      true_location_data_at_time_in_q[, target_end_date:= NULL]
      tmp_rolling_mean_scaled_cases <- mean(as.numeric(unlist(true_location_data_at_time_in_q)))
      tmp_rolling_median_scaled_cases <- median(as.numeric(unlist(true_location_data_at_time_in_q)))
      tmp_rolling_sum_scaled_cases <- sum(as.numeric(unlist(true_location_data_at_time_in_q)))
      true_raw_case_data_at_time_in_q <-
        unique(subset(location_data_at_time_in_q,
                      select = c("observed", "target_end_date")))
      true_raw_case_data_at_time_in_q[, target_end_date:= NULL]
      
      tmp_rolling_mean_cases <- mean(as.numeric(unlist(true_raw_case_data_at_time_in_q)))
      
      
      tmp_location_cases_summary <- data.table(rolling_median_scaled_cases = tmp_rolling_median_scaled_cases,
                                               rolling_sum_scaled_cases = tmp_rolling_sum_scaled_cases,
                                               rolling_mean_scaled_cases = tmp_rolling_mean_scaled_cases,
                                               rolling_mean_cases = tmp_rolling_mean_cases,
                                            max_time = max_time,
                                            min_time = min_time,
                                            location = location_in_q)
      tmp_location_cases_summary[, max_time:= as.Date(max_time)]
      tmp_location_cases_summary[, min_time:= as.Date(min_time)]
      tmp_location_cases_summary[, location:= location_in_q]
      location_cases_summary <- rbind(location_cases_summary,
                                   tmp_location_cases_summary)
      
    }
    all_location_cases_summary <- rbind(all_location_cases_summary,
                                     location_cases_summary)
  }
  return(all_location_cases_summary)
}


#Merge in R(t) with predictability and process
processing_predictability_results <- function(pe_results,
                                              rolling_Rt,
                                              rolling_cases){
  pe_results[, max_time:= as.Date(max_time)]
  pe_results[, predictability:= 1-raw_perm_entropy]
  pe_results[, scaled_total_cases:= range01(total_cases), by = "location"]
  pe_results[, YEAR:= year(max_time)]
  pe_results[, scaled_mean_cases:= range01(mean_cases), by = "location"]
  setkeyv(pe_results, c("location", "max_time"))
  pe_results[, WEEK:= week(max_time)]
  pe_results[, YEAR:= year(max_time)]
  pe_results[, location_factor:= factor(location)]
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
  pe_results <- merge(pe_results,
                      tmp_rt_phases,
                      by.x = c("location", "max_time"),
                      by.y = c("location", "target_end_date"))
  pe_results[, phase_Rt_factor:= factor(phase_Rt)]
  setkeyv(pe_results, c("location", "max_time"))
  pe_results[which(predictability <= 0.3), low_predictability:= TRUE]
  pe_results[which(predictability > 0.3), low_predictability:= FALSE]
  pe_results <- 
    merge(pe_results,
          rolling_Rt,
          by = c("location", "max_time", "min_time"))
  pe_results <- 
    merge(pe_results,
          rolling_cases,
          by = c("location", "max_time", "min_time"))
  return(pe_results)
}





