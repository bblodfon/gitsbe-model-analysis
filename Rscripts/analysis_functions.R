# Input: two vectors with same column names
# Output: a data frame with 2 vectors:
#   1) the first input vector only pruned to its unique values
#   2) a second vector with the average values for each unique
#       value of the first (the matching is done by column name)
get.average.over.unique.values = function(vec1, vec2) {
  stopifnot(names(vec1) == names(vec2))

  vec1.sorted = sort(vec1)
  vec1.sorted.unique = sort(unique(vec1))
  vec2.avg.values = numeric(length = length(vec1.sorted.unique))
  sd.values = numeric(length = length(vec1.sorted.unique))

  index = 0
  for (value in vec1.sorted.unique) {
    index = index + 1
    vec2.avg.values[index] = mean(vec2[
      (names(vec1.sorted[vec1.sorted == value]))
    ])
    sd.values[index] = sd(vec2[
      (names(vec1.sorted[vec1.sorted == value]))
    ])
  }

  # In case of NA elements in sd calculation
  # (one element vectors), replace with 0
  sd.values[is.na(sd.values)] = 0

  res = cbind(vec1.sorted.unique, vec2.avg.values, sd.values)
  colnames(res) = c(deparse(substitute(vec1)), deparse(substitute(vec2)), "sd")

  return(res)
}

# input are vectors with 1 and 0's with same `names` attribute
get.percentage.of.matches = function(num.vec.1, num.vec.2) {
  stopifnot(names(num.vec.1) == names(num.vec.2))

  total = length(num.vec.1)
  diff = num.vec.1 - num.vec.2
  matches = sum(diff == 0)
  matches.percentage = matches / total

  return(matches.percentage)
}

count.models.that.predict.synergy.set =
  function(drug.comb.set, model.predictions) {
    synergy.vector = unlist(drug.comb.set)
    if (length(synergy.vector) == 0) {
      count = sum(apply(model.predictions, 1, function(x) {
        all(x != 1, na.rm = T)
      }))
    } else if (length(synergy.vector) == 1) {
      count = sum(model.predictions[, synergy.vector], na.rm = T)
    } else {
      count = sum(apply(model.predictions[, synergy.vector], 1, function(x) {
        all(x == 1)
      }), na.rm = T)
    }

    return(count)
}

# `num.low` is the number of true positives for the 'bad' models
# `num.high` is the number of true positives for the 'good' models
get.avg.activity.diff.based.on.tp.predictions =
  function(models, models.synergies.tp, models.stable.state, num.low, num.high) {
    if (num.low >= num.high) {
      stop("`num.low` needs to be smaller than `num.high`")
    }

    good.models = models[models.synergies.tp == num.high]
    bad.models  = models[models.synergies.tp == num.low]

    # `good.models` != `bad.models` (disjoing sets of models)
    stopifnot(!(good.models %in% bad.models))

    good.avg.activity = apply(models.stable.state[good.models, ], 2, mean)
    bad.avg.activity = apply(models.stable.state[bad.models, ], 2, mean)

    return(good.avg.activity - bad.avg.activity)
}

# `class.id.low` is the `mcc.interval` id for the 'bad' models
# `class.id.high` is the `mcc.interval` id for the 'good' models
get.avg.activity.diff.based.on.mcc.classification =
  function(models, models.mcc, mcc.intervals, models.stable.state,
           class.id.low, class.id.high) {
    if (class.id.low >= class.id.high) {
      stop("`class.id.low` needs to be smaller than `class.id.high`")
    }

    mcc.interval.low = mcc.intervals[class.id.low, ]
    mcc.interval.high = mcc.intervals[class.id.high, ]

    # find the 'good' models
    max.value = max(mcc.intervals, na.rm = TRUE)
    if (mcc.interval.high[2] == max.value) {
      good.models =
        get.models.based.on.mcc.interval(models, models.mcc, mcc.interval.high,
                                         include.high.value = TRUE)
    } else {
      good.models =
        get.models.based.on.mcc.interval(models, models.mcc, mcc.interval.high)
    }

    # find the 'bad' models
    if (is.na(mcc.interval.low[1])) {
      # the `NaN` MCC scored models (can only be 'bad' ones)
      bad.models = models[is.na(models.mcc)]
    } else {
      bad.models =
        get.models.based.on.mcc.interval(models, models.mcc, mcc.interval.low)
    }

    # `good.models` != `bad.models` (disjoing sets of models)
    stopifnot(!(good.models %in% bad.models))

    good.avg.activity = apply(models.stable.state[good.models, ], 2, mean)
    bad.avg.activity = apply(models.stable.state[bad.models, ], 2, mean)

    return(good.avg.activity - bad.avg.activity)
}

get.models.based.on.mcc.interval =
  function(models, models.mcc, mcc.interval, include.high.value = FALSE) {
    res = sapply(models.mcc, is.between, low.thres = mcc.interval[1],
                 high.thres = mcc.interval[2], include.high.value)
    # exclude the NA values
    res.pruned = res[!is.na(res)]
    models.pruned = models[!is.na(res)]

    return(models.pruned[res.pruned])
}

# checks if `value` is in [low.thres,high.thres) (standard behaviour) or [a,b]
is.between = function(value, low.thres, high.thres, include.high.value = FALSE) {
  if (include.high.value) return(value >= low.thres & value <= high.thres)
  else return(value >= low.thres & value < high.thres)
}

# Example use: `drug.comb` = "AK-PD"
get.avg.activity.diff.based.on.specific.synergy.prediction =
  function(model.predictions, models.stable.state, drug.comb) {
    good.models = rownames(model.predictions)[
      model.predictions[, drug.comb] == 1 & !is.na(model.predictions[, drug.comb])
    ]
    bad.models  = rownames(model.predictions)[
      model.predictions[, drug.comb] == 0 & !is.na(model.predictions[, drug.comb])
      ]
    # na.models = rownames(model.predictions)[is.na(model.predictions[, drug.comb])]

    good.avg.activity = apply(models.stable.state[good.models, ], 2, mean)
    bad.avg.activity = apply(models.stable.state[bad.models, ], 2, mean)

    return(good.avg.activity - bad.avg.activity)
}

# To get meaningful results, one set must be a subset of the other
# Example use:
# synergy.set.str = "A-B,A-D,B-D,P-S"
# synergy.subset.str = "A-B,B-D,P-S"
get.avg.activity.diff.based.on.diff.synergy.set.prediction =
  function(synergy.set.str, synergy.subset.str, model.predictions,
           models.stable.state) {

    synergy.set = unlist(strsplit(synergy.set.str, split = ","))
    synergy.subset = unlist(strsplit(synergy.subset.str, split = ","))

    # some checks
    stopifnot(length(synergy.subset) > 0,
              length(synergy.set) > length(synergy.subset))
    stopifnot(all(synergy.subset %in% synergy.set))

    # find models that predict the `synergy.set`
    if (length(synergy.set) == 1) {
      models.synergy.set = rownames(model.predictions)[
        model.predictions[, synergy.set] == 1 &
        !is.na(model.predictions[, synergy.set])]
    } else {
      models.synergy.set = rownames(model.predictions)[
        apply(model.predictions[, synergy.set], 1,
              function(x) all(x == 1 & !is.na(x)))]
    }

    # find models that predict the `synergy.subset`
    if (length(synergy.subset) == 1) {
      models.synergy.subset = rownames(model.predictions)[
        model.predictions[, synergy.subset] == 1 &
        !is.na(model.predictions[, synergy.subset])]
    } else {
      models.synergy.subset = rownames(model.predictions)[
        apply(model.predictions[, synergy.subset], 1,
              function(x) all(x == 1 & !is.na(x)))]
    }

    common.models = intersect(models.synergy.set, models.synergy.subset)

    # print(paste0(length(models.synergy.set), " models predicted the synergy set ",
    #       synergy.set.str, ", ", length(models.synergy.subset),
    #       " models predicted the synergy subset ", synergy.subset.str, " and ",
    #       length(common.models), " models are common"))

    good.models = common.models
    bad.models  = outersect(models.synergy.set, models.synergy.subset)

    stopifnot(all(!(good.models %in% bad.models)))

    good.avg.activity = apply(models.stable.state[good.models, ], 2, mean)
    bad.avg.activity = apply(models.stable.state[bad.models, ], 2, mean)

    return(good.avg.activity - bad.avg.activity)
}

# The opposite of `intersect` function {base}
outersect = function(x, y) {
  sort(c(setdiff(x, y), setdiff(y, x)))
}

# inputs are vectors of same size and one-to-one value correspondence
calculate.mcc = function(tp, tn, fp, fn, p, n) {
  return(
    (tp * tn - fp * fn) / sqrt((tp + fp) * p * n * (tn + fn))
  )
}

get.mcc.intervals = function(mcc.values, interval.size) {
  min.mcc = min(mcc.values, na.rm = TRUE)
  max.mcc = max(mcc.values, na.rm = TRUE)
  mcc.points = seq(-1.0, 1.0, interval.size)
  mcc.points.pruned = mcc.points[min.mcc < (mcc.points + interval.size) &
                                 mcc.points < (max.mcc + interval.size)]

  mcc.intervals =
    matrix(numeric(), nrow = length(mcc.points.pruned) - 1, ncol = 2)
  for (i in 1:nrow(mcc.intervals)) {
    mcc.intervals[i, 1] = mcc.points.pruned[i]
    mcc.intervals[i, 2] = mcc.points.pruned[i + 1]
  }

  return(mcc.intervals)
}

get.mcc.classes = function(mcc.intervals) {
  number.of.intervals = nrow(mcc.intervals)
  mcc.classes = character(number.of.intervals)

  for (i in 1:number.of.intervals) {
    mcc.interval = mcc.intervals[i,]
    low.value = mcc.interval[1]
    high.value = mcc.interval[2]
    if (is.na(low.value)) mcc.classes[i] = "NaN"
    else if (i != number.of.intervals)
      mcc.classes[i] = paste0("[", low.value, ", " , high.value, ")")
    else
      mcc.classes[i] = paste0("[", low.value, ", " , high.value, "]")
  }

  return(mcc.classes)
}

# adds one more row to the `biomarkers.synergy.res` data.frame with the
# performance-related biomarkers
add.performance.biomarkers =
  function(biomarkers.synergy.res, biomarkers.active, biomarkers.inhibited) {
    # initialize `row` data.frame
    node.names = colnames(biomarkers.synergy.res)
    row = as.data.frame(matrix(0, ncol = length(node.names), nrow = 1))
    colnames(row) = node.names
    rownames(row) = "PERF"

    # add biomarkers
    row[colnames(row) %in% biomarkers.active] = 1
    row[colnames(row) %in% biomarkers.inhibited] = -1

    res = rbind(row, biomarkers.synergy.res)
    return(res)
}
