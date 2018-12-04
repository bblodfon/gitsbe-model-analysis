make.color.bar.plot = function(color.vector, number.vector, title, xlab) {
  bp = barplot(rep(1,length(color.vector)), col = color.vector,
               axes = FALSE, xlab = xlab, main = title)
  axis(1, bp, number.vector)
}

make.barplot.on.model.stats = function(model_synergies_stats, modelsNum) {
  par(mar = c(5, 4, 4, 2) + 0.1) # the default
  color = "blue"
  bp = barplot(model_synergies_stats, col = color,
               ylim = c(0, 0.5 * modelsNum),
               main = "Model Synergy Predictions",
               xlab = "Number of maximum correctly predicted synergies",
               ylab = "Number of models")

  for (i in 1:length(model_synergies_stats)) {
    text(bp[i], model_synergies_stats[i],
         model_synergies_stats[i], col = "red", pos = 3)
  }

  legend.title = "AGS cell line"
  legend("topright", legend = legend.title, col = color, lwd = 15 , cex = 1.3)
}

make.barplot.on.synergy.subset.stats =
  function(synergy.subset.stats, modelsNum) {
  # fit in the names of the drug combinations
  par(mar = c(15, 4, 4, 2) + 0.1) # c(bottom, left, top, right)

  # remove some subsets based on the number of models that predicted them
  #synergy.subset.stats = synergy.subset.stats[ !synergy.subset.stats < 10 ]

  color = "green"
  bp = barplot(synergy.subset.stats, col = color, space = 0.5, las = 2,
               main = "Model Synergy Predictions per Observed Synergy Subset",
               ylab = "Number of models",
               ylim = c(0, modelsNum))

  for (i in 1:length(synergy.subset.stats)) {
    text(bp[i], synergy.subset.stats[i], labels = synergy.subset.stats[i], col = "red", pos = 3)
    # with rotation of the text labels and some placement adjustment
    # text(bp[i]+0.4, synergy.subset.stats[i]+50, labels = synergy.subset.stats[i], col = "red", pos = 3, srt = 90)
  }

  legend.title = "A498 cell line"
  legend("topright", legend = legend.title, col = color, lwd = 15 , cex = 1.3)
}