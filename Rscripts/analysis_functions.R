# Input: two vectors with same column names
# Output: a data frame with 2 vectors:
#   1) the first input vector only pruned to its unique values
#   2) a second vector with the average values for each unique
#       value of the first (the matching is done by column name)
get.average.over.unique.values = function(vec1, vec2) {
  stopifnot(sum(names(vec1) == names(vec2)) == length(vec1))

  vec1.sorted = sort(vec1)
  vec1.sorted.unique = sort(unique(vec1))
  vec2.avg.values = numeric(length = length(vec1.sorted.unique))
  sd.values = numeric(length = length(vec1.sorted.unique))

  index = 0
  for (value in vec1.sorted.unique) {
    index = index + 1
    vec2.avg.values[index] =
      mean(vec2[(names(vec1.sorted[vec1.sorted == value]))])
    sd.values[index]       =
      sd(vec2[(names(vec1.sorted[vec1.sorted == value]))])
  }

  # In case of NA elements in sd calculation
  # (one element vectors), replace with 0
  sd.values[is.na(sd.values)] = 0

  res = cbind(vec1.sorted.unique, vec2.avg.values, sd.values)
  colnames(res) = c(deparse(substitute(vec1)), deparse(substitute(vec2)), "sd")

  return(res)
}

get.percentage.of.matches = function(num.vec.1, num.vec.2) {
  # check that all columns (node names) are equal
  stopifnot(all.equal(names(num.vec.1), names(num.vec.2)))

  total = length(num.vec.1)
  diff = num.vec.1 - num.vec.2
  matches = sum(diff == 0)
  matches.percentage = matches / total

  return(matches.percentage)
}

get.diff.synergies.predicted =
  function(models, model.synergies, models.stable.state) {
  bad.models = models[model.synergies == 0]
  good.models = models[model.synergies == max(model.synergies)]

  bad.average = apply(models.stable.state[bad.models, ], 2, mean)
  good.average = apply(models.stable.state[good.models, ], 2, mean)

  return(good.average - bad.average)
}

get.diff.specific.synergy =
  function(drugComb, modelData, models.stable.state) {
  bad.models  = rownames(modelData)[
    modelData[, drugComb] == 0 & !is.na(modelData[, drugComb])
  ]
  good.models = rownames(modelData)[
    modelData[, drugComb] == 1 & !is.na(modelData[, drugComb])
  ]
  # na.models = rownames(modelData)[is.na(modelData[, drugComb])]

  bad.average = apply(models.stable.state[bad.models, ], 2, mean)
  good.average = apply(models.stable.state[good.models, ], 2, mean)

  return(good.average - bad.average)
}

# Example (to get meaningful results)
# set1 = all.synergy.subsets["AK-BI,BI-D1,PK-ST"]
# set2 = all.synergy.subsets["AK-BI,AK-D1,BI-D1,PK-ST"]
get.diff.from.models.predicting.diff.synergy.sets =
  function(set1, set2, model.data, models.stable.state) {
  # length(set1), length(set2) >= 2
  synergy.set.1 = unlist(set1)
  synergy.set.2 = unlist(set2)

  models.set.1 = rownames(model.data)[
    apply(model.data[, synergy.set.1], 1, function(x) all(x == 1 & !is.na(x)))
  ]
  models.set.2 = rownames(model.data)[
    apply(model.data[, synergy.set.2], 1, function(x) all(x == 1 & !is.na(x)))
  ]

  # have the first set of models as the largest
  if (length(models.set.1) < length(models.set.2)) {
    models.set.3 = models.set.1
    models.set.1 = models.set.2
    models.set.2 = models.set.3
  }

  models.set.1.not.in.set.2 = models.set.1[! models.set.1 %in% models.set.2]
  common.models = models.set.1[models.set.1 %in% models.set.2]

  bad.models  = models.set.1.not.in.set.2
  good.models = common.models

  bad.average = apply(models.stable.state[bad.models, ], 2, mean)
  good.average = apply(models.stable.state[good.models, ], 2, mean)

  return(good.average - bad.average)
}

count.models.that.predict.subset.of.synergies =
  function(synergy.subset, model.data) {
  synergy.vector = unlist(synergy.subset)
  if (length(synergy.vector) == 0) {
    count = sum(apply(model.data, 1, function(x) { all(x != 1, na.rm = T) }))
  } else if (length(synergy.vector) == 1) {
    count = sum(model.data[, synergy.vector], na.rm = T)
  } else {
    count = sum(apply(model.data[, synergy.vector], 1,
                    function(x) all(x == 1)), na.rm = T)
  }

  return(count)
}
