# Don't use this function to visualize more than 50 colors
make.color.bar.plot = function(color.vector, number.vector, title, xlab = "") {
  bp = barplot(rep(1,length(color.vector)), col = color.vector,
               axes = FALSE, xlab = xlab, main = title, border = NA)
  axis(1, bp, number.vector)
}

# models.stats is the result of `table(num.vector)`
make.barplot.on.models.stats =
  function(models.stats, cell.line, title, xlab, ylab, cont.values = FALSE) {

    # Find is there is just one `NaN` category
    there.is.one.NaN.category = FALSE
    nan.index = which(is.nan(as.numeric(names(models.stats))))
    if (length(nan.index) == 1) {
      there.is.one.NaN.category = TRUE
      nan.value = models.stats[nan.index]
    }

    # If there is just one `NaN` category, put it first in the `models.stats`
    # for presentation purposes in the barplot
    if (there.is.one.NaN.category) {
      models.stats = c(nan.value, models.stats[names(models.stats) != "NaN"])
    }

    x.axis.values =
      get.x.axis.values(models.stats, there.is.one.NaN.category, cont.values)
    y.axis.values = pretty(models.stats)

    bp = barplot(models.stats, col = "cornflowerblue",
                 names.arg = x.axis.values, yaxt = "n",
                 ylim = c(0, max(y.axis.values) + 500),
                 main = paste0(title, " (", cell.line, ")"),
                 xlab = xlab, ylab = ylab)
    axis(2, at = y.axis.values, las = 1)

    add.numbers.above.the.bars(models.stats, bp, color = "red")

    # If there is just one `NaN` category, label it in the plot
    if (there.is.one.NaN.category) {
      text(x = bp[1], y = nan.value/2, labels = names(nan.value),
           col = "yellow", srt = 90, font = 2)
    }
}

get.x.axis.values =
  function(models.stats, there.is.one.NaN.category, cont.values) {
    if (there.is.one.NaN.category) {
      # replace `NaN` value with empty space at the beginning of the x axis
      x.values = c(" ", names(models.stats)[names(models.stats) != "NaN"])
    } else x.values = names(models.stats)

    if (cont.values) {
      return(round(as.numeric(x.values), digits = 3))
    } else {
      return(x.values)
    }
}

make.barplot.on.synergy.subset.stats =
  function(synergy.subset.stats, threshold.for.subset.removal, bottom.margin,
           cell.line) {
    # If the number of models that predicted a specific synergy set is less
    # than the `threshold.for.subset.removal` then discard it
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

add.numbers.above.the.bars = function(stats, bp, color) {
  for (i in 1:length(stats)) {
    text(x = bp[i], y = stats[i], labels = stats[i],
         col = color, pos = 3)
  }
}

make.multiple.density.plot = function(densities, legend.title) {
  plot(NA, xlim = range(sapply(densities, "[", "x")),
       ylim = range(sapply(densities, "[", "y")),
       main = "Density Estimation for the Average State Difference",
       xlab = "Activity state (absolute difference value)",
       ylab = "Density")
  mapply(lines, densities, col = 1:length(densities))

  legend("topright", legend = names(densities), fill = 1:length(densities),
         title = legend.title)
}

plot.network = function(net, diff, layout, title) {
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
  par(mar = c(0, 0, 1, 0)) # c(bottom, left, top, right)
  plot(net, asp = 0, layout = layout, main = title)
  legend(x = -1.1, y = -0.7, pch = 21, col = "#777777",
        legend = c("More inhibited","No difference", "More activated"),
        title = expression(bold("Good model activity state")),
        pt.bg = colors, pt.cex = 2, cex = 0.8, bty = "n", ncol = 1)
}
