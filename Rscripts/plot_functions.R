make.grid.and.output.to.file = function() {
  folder = "AGS"
  output_file = "/home/ioanniz/model_prediction_analysis/AGS/allllll.png"

  png(filename= output_file,
      width = 1051, height = 900)
  rl = lapply(list(paste(folder,"/","normal.png", sep = ""),
                    paste(folder,"/","random.png", sep = ""),
                    paste(folder,"/","normal_bliss.png", sep = ""),
                    paste(folder,"/","random_bliss.png", sep = "")
  ), png::readPNG)
  gl = lapply(rl, grid::rasterGrob)
  do.call(gridExtra::grid.arrange, gl)
  dev.off()
}

make.barplot.on.model.stats = function(model_synergies_stats, modelsNum) {
  #png(filename="/home/ioanniz/model_prediction_analysis/UACC62/random_bliss.png")
  par(mar = c(5, 4, 4, 2) + 0.1) # the default
  color = "blue"
  bp = barplot(model_synergies_stats, col = color,
               #ylim = c(0, 600),
               ylim = c(0, 0.5 * modelsNum),
               main = "Model Synergy Predictions",
               xlab = "Number of maximum correctly predicted synergies",
               ylab = "Number of models")
  #bp
  for (i in 1:length(model_synergies_stats)) {
    text(bp[i], model_synergies_stats[i], model_synergies_stats[i], col = "red", pos = 3)
  }

  legend.title = "AGS cell line"
  legend("topright", legend = legend.title, col = color, lwd = 15 , cex = 1.3)

  #dev.off()
}

make.barplot.on.synergy.subset.stats = function(synergy.subset.stats, modelsNum) {
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

make.color.bar.plot = function(color.vector, number.vector, title, xlab) {
  bp = barplot(rep(1,length(color.vector)), col = color.vector,
               axes = FALSE, xlab = xlab, main = title)
  axis(1, bp, number.vector)
}