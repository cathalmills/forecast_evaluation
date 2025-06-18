setkeyv(entropy_12_wk_scores_metrics_coverage_dt,
        c("location", "model" ,"target_type", "max_time"))
entropy_12_wk_scores_metrics_coverage_dt
entropy_12_wk_scores_metrics_coverage_dt[, lag1_predictability := shift(predictability, n = 1, type = "lag"), by = c("location", "model" ,"target_type")]
entropy_12_wk_scores_metrics_coverage_dt[, lag2_predictability := shift(predictability, n = 2, type = "lag"), by = c("location", "model" ,"target_type")]
entropy_12_wk_scores_metrics_coverage_dt[, lag3_predictability := shift(predictability, n = 3, type = "lag"), by = c("location", "model" ,"target_type")]
entropy_12_wk_scores_metrics_coverage_dt[, lag4_predictability2 := shift(predictability, n = 4, type = "lag"), by = c("location", "model" ,"target_type")]
entropy_12_wk_scores_metrics_coverage_dt[which(lag4_predictability!=lag4_predictability2)]



tmp <- entropy_12_wk_scores_metrics_coverage_dt[which(!is.na(lag4_predictability))]
tmp$location_factor <- factor(tmp$location)
tmp$model_factor <- factor(tmp$location)
tmp$target_type_factor <- factor(tmp$target_type)

gam_model <- gam(wis_scaled_relative_skill ~ s(lag1_predictability) + 
                   s(lag2_predictability) + 
                   s(model_factor, bs = "re")+
                   s(target_type_factor, bs = "re")+
                   s(location_factor, bs = "re")+
                   s(lag3_predictability)+
                   s(lag4_predictability),
                 data = tmp, 
                 method = "REML")
plot(gam_model)


dt <- copy(entropy_12_wk_scores_metrics_coverage_dt)
max_lag <- 12
for (k in 1:max_lag) {
  dt[, paste0("lag", k) := shift(predictability, k), by = location]
}


dt$
  lag_cols <- paste0("lag", 1:max_lag)
long_dt <- melt(dt, 
                id.vars = c("wis_scaled_relative_skill", "location", "model" ,"target_type", "max_time"),
                measure.vars = lag_cols,
                variable.name = "lag",
                value.name = "predictability_lagged")

# Convert lag from factor like "lag1" to numeric
long_dt[, lag := as.numeric(gsub("lag", "", lag))]

# Remove missing lags
long_dt <- long_dt[!is.na(predictability_lagged)]


long_dt$location_factor <- factor(long_dt$location)
long_dt$model_factor <- factor(long_dt$location)
long_dt$target_type_factor <- factor(long_dt$target_type)
long_dt
gam_model <- gam(wis_scaled_relative_skill ~ te(lag, predictability_lagged) + 
                   s(max_time, bs = "cr") + 
                   s(model_factor, bs = "re")+
                   s(target_type_factor, bs = "re")+
                   s(location_factor, bs = "re"),
                 data = long_dt,
                 method = "REML")