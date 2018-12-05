make.color.bar.plot = function(color.vector, number.vector, title, xlab) {
  bp = barplot(rep(1,length(color.vector)), col = color.vector,
               axes = FALSE, xlab = xlab, main = title)
  axis(1, bp, number.vector)
}

make.barplot.on.model.stats = function(model.synergies.stats, cell.line) {
  y.axis.values = pretty(model.synergies.stats)
  bp = barplot(model.synergies.stats, col = "blue",
               ylim = c(0, max(y.axis.values) + 500), yaxt = "n",
               main = paste0("True Positive Synergy Predictions",
                             " (", cell.line, ")"),
               xlab = "Number of maximum correctly predicted synergies",
               ylab = "Number of models")
  axis(2, at = y.axis.values, las = 1)

  add.numbers.above.the.bars(model.synergies.stats, bp, color = "red")
}

make.barplot.on.synergy.subset.stats =
  function(synergy.subset.stats, threshold.for.subset.removal, bottom.margin,
           cell.line) {
  # If the number of models that predicted a specific synergy set is less
  # than the 'threshold.for.subset.removal' then discard it
  synergy.subset.stats = synergy.subset.stats[
    !synergy.subset.stats < threshold.for.subset.removal
  ]

  # To fit in the names of the drug combinations, specify in inches the bottom
  # margin of the plot depending on the maximum size of a synergy subset:
  # size: 1 => bottom.margin = 4
  # size: 2 => bottom.margin = 6
  # size: 3 => bottom.margin = 9
  # size: 4 => bottom.margin = 12, etc.
  par(mar = c(bottom.margin, 4, 4, 2)) # c(bottom, left, top, right)

  y.axis.values = pretty(synergy.subset.stats)
  bp = barplot(synergy.subset.stats, col = "green", space = 0.5, las = 2,
               main = paste0("Model Synergy Predictions per Observed Synergy",
                              " Subset", " (", cell.line, ")"),
               ylab = "Number of models", yaxt = "n",
               ylim = c(0, max(y.axis.values) + 500))
  axis(2, at = y.axis.values, las = 1)

  add.numbers.above.the.bars(synergy.subset.stats, bp, color = "red")
}

add.numbers.above.the.bars = function(table.stats, bp, color) {
  for (i in 1:length(table.stats)) {
    text(x = bp[i], y = table.stats[i], labels = table.stats[i],
         col = color, pos = 3)
  }
}