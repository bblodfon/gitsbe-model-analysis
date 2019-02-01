colors.100 = c("#000000", "#0089A3", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941",
               "#006FA6", "#A30059", "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC",
               "#B79762", "#004D43", "#8FB0FF", "#997D87", "#5A0007", "#809693",
               "#FEFFE6", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
               "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9",
               "#B903AA", "#D16100", "#DDEFFF", "#000035", "#7B4F4B", "#A1C299",
               "#300018", "#0AA6D8", "#013349", "#00846F", "#372101", "#FFB500",
               "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09",
               "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68",
               "#7A87A1", "#788D66", "#885578", "#FAD09F", "#FF8A9A", "#D157A0",
               "#BEC459", "#456648", "#0086ED", "#886F4C", "#34362D", "#B4A8BD",
               "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81",
               "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757",
               "#C8A1A1", "#1E6E00", "#7900D7", "#A77500", "#6367A9", "#A05837",
               "#6B002C", "#772600", "#D790FF", "#9B9700", "#549E79", "#FFF69F",
               "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
               "#5B4534", "#FDE8DC", "#404E55", "#FFFF00")

# Don't use this function to visualize more than 42 colors!
make.color.bar.plot = function(color.vector, number.vector, title, xlab = "") {
  bp = barplot(rep(1,length(color.vector)), col = color.vector,
               axes = FALSE, xlab = xlab, main = title, border = NA)
  axis(1, bp, number.vector)
}

plot.mcc.classes.hist = function(models.mcc.no.nan.sorted, models.cluster.ids,
                                 num.of.classes, mcc.class.ids) {
  min.x.value = round(min(models.mcc.no.nan.sorted) - 0.1, digits = 1)
  max.x.value = round(max(models.mcc.no.nan.sorted) + 0.1, digits = 1)
  rainbow.colors = rainbow(num.of.classes)

  ahist(models.mcc.no.nan.sorted, k = num.of.classes,
        main = "Model MCC-Classification", xlab = "MCC value",
        sub = paste("n =", length(models.mcc.no.nan.sorted), "models, k =",
                    num.of.classes, "classes"),
        col = rainbow.colors, col.stick = rainbow.colors[models.cluster.ids],
        xlim = c(min.x.value, max.x.value))

  legend("topright", legend = mcc.class.ids, title = "MCC Classes",
         col = rainbow.colors, lty = 1, lwd = 10)
}

# models.stats is the result of `table(num.vector)`
# Use `threshold` when there too many categories and the graph appears too dense
# `cont.values` is used for trimming the digits of continuous values on the x-axis
make.barplot.on.models.stats =
  function(models.stats, cell.line, title, xlab, ylab, cont.values = FALSE, threshold = 0) {

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

    # prune some bars :)
    models.stats = models.stats[models.stats > threshold]

    # If number of `NaN` values are lower then the `threshold` and
    # as such will be pruned, there will be no `NaN` bar in the plot
    if (there.is.one.NaN.category && nan.value <= threshold)
      there.is.one.NaN.category = FALSE

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

# `densities` is a list, each element holding the results from executing
# the `density` function to a (different) vector
make.multiple.density.plot =
  function(densities, legend.title, title, x.axis.label, legend.size = 1) {
    stopifnot(length(densities) <= 100)

    # take colors from the 100 distict color set
    color.palette = colors.100[1:length(densities)]

    plot(NA, xlim = range(sapply(densities, "[", "x")),
             ylim = range(sapply(densities, "[", "y")),
             main = title, xlab = x.axis.label, ylab = "Density")
    mapply(lines, densities, col = color.palette)

    legend("topright", legend = names(densities), fill = color.palette,
           title = legend.title, cex = legend.size)
}

# plot network using the `threejs` library
plot.network.3d = function(net, diff) {

  # colors for nodes (to be interpolated) matching one-to-one the diff values
  col = c("tomato", "grey", "gold")
  vertex.color = get.node.colors(net, diff, col)
  V(net)$label = names(vertex.color)
  names(vertex.color) = NULL

  net = delete_edge_attr(net, "width")
  graphjs(net, vertex.color = vertex.color, width = "100%")
}

# plot network using the `visNetwork` library
plot.network.vis = function(net, diff, layout, title) {
  data = toVisNetworkData(net)
  nodes = data$nodes
  edges = data$edges

  # colors for nodes (to be interpolated) matching one-to-one the diff values
  col = c("tomato", "grey", "gold")
  nodes$color = get.node.colors(net, diff, col)

  # set visualization graph attributes
  nodes$size = 30
  nodes$physics = FALSE
  nodes$shape = "dot"
  scale.factor = 60
  nodes$x = layout[,2] * scale.factor
  nodes$y = layout[,1] * scale.factor

  edges$smooth = FALSE
  edges$physics = FALSE
  edges$arrows = "to"

  # set legend properties
  legend.nodes = data.frame(
    label = c("More inhibited","No difference", "More activated"), color = col
  )

  # plot the network
  visNetwork(nodes, edges, main = title, width = "100%") %>%
    visLegend(addNodes = legend.nodes, useGroups = FALSE,
              main = "Good model activity state", zoom = FALSE)
}

# plot network using the `igraph` library
plot.network = function(net, diff, layout, title) {
  # colors for nodes (to be interpolated) matching one-to-one the diff values
  col = c("tomato", "grey", "gold")
  V(net)$color = get.node.colors(net, diff, col)

  # plot the network
  par(mar = c(0, 0, 1, 0)) # c(bottom, left, top, right)
  plot(net, asp = 0, layout = layout, main = title)
  legend(x = -1.1, y = -0.7, pch = 21, col = "#777777",
        legend = c("More inhibited","No difference", "More activated"),
        title = expression(bold("Good model activity state")),
        pt.bg = col, pt.cex = 2, cex = 0.8, bty = "n", ncol = 1)
}

# `net` is an igraph network object with node labels as: `V(net)$name`
get.node.colors = function(net, diff, col) {
  # 2000 equal-sized intervals for values between [-1,1]
  # for significance up to the 3rd decimal
  num.of.intervals = 2000

  # make the color of each node match the corresponding diff value
  color.palette = colorRampPalette(col, interpolate = "spline")
  color.values = color.palette(num.of.intervals)
  # check the colors
  # plot(x = 1:2000, y = 1:2000, cex = 10, pch = 20, col = color.values)

  diff.extra = c(diff, -1, 1)

  interval.ids.extra = as.numeric(
    cut(diff.extra, breaks = num.of.intervals, include.lowest = TRUE)
  )

  # remove the last two values
  interval.ids = interval.ids.extra[1:(length(interval.ids.extra) - 2)]
  diff.colors = color.values[interval.ids]
  names(diff.colors) = names(diff)

  # re-order based on the net object's node sequence
  node.names = V(net)$name
  return(diff.colors[node.names])
}

# `file.format` can be one of: {pdf, svg, png, tiff}
plot.string.to.file = function(file, file.format, plot.string){
  if (file.format == "pdf")
    pdf(file)
  else if (file.format == "svg")
    svg(file, width = 7, height = 7)
  else if (file.format == "png")
    png(file, width = 7, height = 7, units = 'in', res = 300)
  else if (file.format == "tiff")
    tiff(file, width = 7, height = 7, units = 'in', res = 300)

  eval(parse(text = plot.string))
  dev.off()
}
