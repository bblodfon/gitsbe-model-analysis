# Don't use it to visualize more than 50 colors
make.color.bar.plot = function(color.vector, number.vector, title, xlab = "") {
  bp = barplot(rep(1,length(color.vector)), col = color.vector,
               axes = FALSE, xlab = xlab, main = title, border = NA)
  axis(1, bp, number.vector)
}

make.barplot.on.model.stats = function(model.synergies.stats, cell.line) {
  y.axis.values = pretty(model.synergies.stats)
  bp = barplot(model.synergies.stats, col = "cornflowerblue",
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

make.multiple.density.plot = function(densities, xlab) {
  plot(NA, xlim = range(sapply(densities, "[", "x")),
       ylim = range(sapply(densities, "[", "y")),
       main = "Density Estimation for the Average State Difference",
       xlab = "Activity state (absolute difference value)",
       ylab = "Density")
  mapply(lines, densities, col = 1:length(densities))

  legend("topright", legend = names(densities), fill = 1:length(densities))
}

plot.network = function(net, diff, layout) {
  num.of.intervals = 2000
  colors = c("tomato", "grey", "gold")

  # make the color of each node match the corresponding diff value
  color.palette = colorRampPalette(colors, interpolate = "spline")
  color.values = color.palette(num.of.intervals)
  # check the colors
  # plot(x = 1:2000, y = 1:2000, cex = 10, pch = 20, col = color.values)

  diff.extra = c(diff, -1, 1)

  # 2000 equal-sized intervals for values between [-1,1]
  # for significance up to the 3rd decimal
  interval.ids.extra = as.numeric(
    cut(diff.extra, breaks = num.of.intervals, include.lowest = TRUE)
  )

  # remove the last two values
  interval.ids = interval.ids.extra[1:(length(interval.ids.extra) - 2)]
  diff.colors = color.values[interval.ids]
  names(diff.colors) = names(diff)

  # re-order based on the net object's node sequence
  node.names = V(net)$name
  V(net)$color = diff.colors[node.names]

  # plot the network
  par(mar = c(0, 0, 0, 0))
  plot(net, asp = 0, layout = layout)
  legend(x = -1.1, y = -0.7, pch = 21, col = "#777777",
        legend = c("More inhibited","No difference", "More activated"),
        title = expression(bold("Good model activity state")),
        pt.bg = colors, pt.cex = 2, cex = 0.8, bty = "n", ncol = 1)
}
