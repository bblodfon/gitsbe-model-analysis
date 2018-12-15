measure_stats_ags = read.table("statistical_measures_AGS.txt")

# Plot ROC
models_fpr = measure_stats_ags[,"models_fpr"]
names(models_fpr) = rownames(measure_stats_ags)
models_sensitivity = measure_stats_ags[,"models_sensitivity"]
names(models_sensitivity) = rownames(measure_stats_ags)
plot(models_fpr, models_sensitivity, xlim = c(0,1), ylim = c(0,1), type = "o", main = "ROC curve")

# or try first some ordering:
models_fpr_sorted = sort(models_fpr)
models_sensitivity_fpr_sorted = models_sensitivity[names(models_fpr_sorted)]
plot(models_fpr_sorted, models_sensitivity_fpr_sorted, xlim = c(0,1), ylim = c(0,1), type = "o", main = "ROC curve")

# Plot models precision
plot(measure_stats_ags[,"models_precision"])

# Others?