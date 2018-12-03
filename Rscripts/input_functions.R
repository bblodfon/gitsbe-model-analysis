get.model.predictions = function(model.predictions.file) {
  print(paste("Reading model predictions file:", model.predictions.file))

  lines = readLines(model.predictions.file)
  lines[1] = sub("ModelName\t|#ModelName\t", "", lines[1])
  tmp.file = "model_predictions.tab"
  writeLines(lines, tmp.file)
  model.data = read.table("model_predictions.tab",  check.names = F)

  if (file.exists(tmp.file)) invisible(file.remove(tmp.file))
  for (i in 1:length(colnames(model.data))) {
    colnames(model.data)[i] = gsub("\\[|\\]", "", colnames(model.data)[i])
  }

  return(model.data)
}

get.observed.synergies =
  function(observed.synergies.file, drug.combinations.tested) {
  print(paste("Reading observed synergies file:", observed.synergies.file))

  lines = readLines(observed.synergies.file)
  synergy.observations.data = gsub("~", "-", lines)

  print(paste("Number of synergies observed: ",
              length(synergy.observations.data)))

  check.observations.data.validity(synergy.observations.data,
                                   drug.combinations.tested)

  return(synergy.observations.data)
}

get.consensus.steady.state = function(steady.state.file) {
  print(paste("Reading consensus steady state file:", steady.state.file))

  lines = readLines(steady.state.file)
  lines = remove.commented.and.empty.lines(lines)
  consensus.steady.state = build.consensus.steady.state.vector(lines)

  return(consensus.steady.state)
}

remove.commented.and.empty.lines = function(lines) {
  commented.or.empty.lines = character(0)
  for (line in lines) {
    if (startsWith(line, "#") || trimws(line) == "") {
      commented.or.empty.lines = c(commented.or.empty.lines, line)
    }
  }
  pruned.lines = lines[!lines %in% commented.or.empty.lines]
  return(pruned.lines)
}

build.consensus.steady.state.vector = function(lines) {
  node.names = character(0)
  activity.states = character(0)
  for (line in lines) {
    values = strsplit(line, "\t")[[1]]
    node.names = c(node.names, values[1])
    activity.states = c(activity.states, values[2])
  }
  activity.states = as.numeric(activity.states)
  stopifnot(length(activity.states) == length(node.names))

  names(activity.states) = node.names
  return(activity.states)
}

prune.to.common.nodes.and.reorder = function(consensus.steady.state, nodes) {
  pruned.consensus.steady.state = consensus.steady.state[
    names(consensus.steady.state) %in% nodes
  ]
  reordered.consensus.steady.state = pruned.consensus.steady.state[
    order(match(names(pruned.consensus.steady.state), nodes))
  ]
  stopifnot(names(reordered.consensus.steady.state) == nodes)

  return(reordered.consensus.steady.state)
}

get.stable.state.from.models.dir = function(models.dir) {
  files = list.files(models.dir)
  model.stable.states = character(length(files))

  node.names = get.node.names(models.dir)

  i = 0
  for (file in files) {
    i = i + 1
    lines = readLines(paste0(models.dir, "/", file))
    model.stable.states[i] = gsub("stablestate: ", "", lines[4])
  }

  models.stable.state = data.frame(model.stable.states, row.names = files)
  df = apply(models.stable.state, 1, function(x) {
    as.numeric(strsplit(as.character(x[1]), "")[[1]])
  })
  rownames(df) = node.names

  return(t(df))
}

get.equations.from.models.dir = function(models.dir,
                                         removeEquationsHavingLinkOperator) {
  files = list.files(models.dir)
  node.names = get.node.names(models.dir)

  datalist = list(length(files))

  # get the equations
  i=0
  for (file in files) {
    i=i+1
    lines = readLines(paste0(models.dir, "/", file))
    equations = grep("equation:", lines, value = TRUE)
    values = sapply(equations, function(equation) {
      assign.value.to.equation(equation)})
    datalist[[i]] = values
  }

  df = do.call(rbind, datalist)

  rownames(df) = files
  colnames(df) = node.names

  if (removeEquationsHavingLinkOperator) {
    # keep only the equations (columns) that
    # have the "and not" or "or not" link
    # operator, i.e. those that can change in
    # the "link" mutations
    df = df[, colSums(is.na(df)) < nrow(df)]
  } else {
    # keep all equations and put a value of 0.5
    # for those that don't have a link operator
    df[is.na(df)] = 0.5
  }

  return(df)
}

get.fitness.from.models.dir = function(models.dir) {
  files = list.files(models.dir)
  model.fitness = character(length(files))

  i = 0
  for (file in files) {
    i = i + 1
    lines = readLines(paste0(models.dir, "/", file))
    model.fitness[i] = gsub("fitness: ", "", lines[3])
  }

  model.fitness = as.numeric(model.fitness)
  names(model.fitness) = files

  return(model.fitness)
}

get.node.names = function(models.dir) {
  # use the first .gitsbe model file to derive the node names
  file.lines = readLines(paste0(models.dir, "/", list.files(models.dir)[1]))
  node.names = gsub("mapping: (.*) =.*", "\\1",
                    grep("mapping:", file.lines, value = TRUE))
  return(node.names)
}

get.model.names = function(models.dir) {
  return(list.files(models.dir))
}

# 'or not' -> 1, 'and not' -> 0, else -> NA
assign.value.to.equation = function(equation) {
  if (grepl(".*or not.*", equation)) {
    return(1)
  } else if (grepl(".*and not.*", equation)) {
    return(0)
  } else return(NA)
}

is.correct.synergy = function(drug.comb, synergy.observations.data) {
  return(is.element(drug.comb, synergy.observations.data) |
           is.element(get.alt.drugname(drug.comb), synergy.observations.data))
}

check.observations.data.validity =
  function(synergy.observations.data, drug.comb.tested) {
  for (drug.comb in synergy.observations.data) {
    if (!is.element(drug.comb, drug.comb.tested) &
        !is.element(get.alt.drugname(drug.comb), drug.comb.tested)) {
      stop(paste("Drug Combination: ", drug.comb,
                 "is not listed in the model predictions file"), call. = F)
    }
  }
}

get.alt.drugname = function(drug.comb) {
  drug.list = unlist(strsplit(drug.comb,"-"))
  drug.comb.alt = paste0(drug.list[2], "-", drug.list[1])
  return(drug.comb.alt)
}