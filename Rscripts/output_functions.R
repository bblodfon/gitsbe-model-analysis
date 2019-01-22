# `digits.to.keep` refers to digits after decimal point '.'
specify.decimal = function(number, digits.to.keep) {
  trimws(format(round(number, digits.to.keep), nsmall = digits.to.keep))
}

print.model.and.drug.stats = function(drug.combs, models, nodes) {
  print(paste("Drug combinations tested:", drug.combs,
              "Number of models:", models,
              "Number of nodes:", nodes))
}

print.mcc.classification.info = function(mcc.classes) {
  number.of.mcc.classes = length(mcc.classes)
  print(paste0("MCC values are split into ",
               number.of.mcc.classes, " classes:"))

  for (i in 1:number.of.mcc.classes) {
    print(paste0(i, ". ", mcc.classes[i]))
  }
}

# `vector.names.str` tell us what `names(vec)` actually is, to put it on
# the print message
pretty.print.vector.names = function(vec, vector.names.str = "nodes") {
  if (length(vec) == 1) {
    vector.names.str = substr(vector.names.str, start = 1,
                              stop = nchar(vector.names.str) - 1)
  }
  print(paste0(length(vec), " ", vector.names.str, ": ",
               paste0(names(vec), collapse = ",")))
}

# `vector.values.str` tell us what the `vec` values are, to put it on
# the print message
pretty.print.vector.values = function(vec, vector.values.str = "nodes") {
  if (length(vec) == 1) {
    vector.values.str = substr(vector.values.str, start = 1,
                              stop = nchar(vector.values.str) - 1)
  }
  print(paste0(length(vec), " ", vector.values.str, ": ",
               paste0(vec, collapse = ",")))
}

# get the common `names` from two vectors and print an appropriate message
# `vector.names.str` tell us what `names(vec)` actually is, to put it on
# the print message
get.common.names = function(vec1, vec2, vector.names.str = "nodes") {
  common.names = intersect(names(vec1), names(vec2))
  common.names.number = length(common.names)

  if (common.names.number == 0) {
    print(paste0("No common ", vector.names.str))
    return(NULL)
  }
  else {
    print(paste0(common.names.number, " ", vector.names.str, ": ",
          paste0(common.names, collapse = ",")))
    return(common.names)
  }
}

# get the common values from two vectors and print an appropriate message
# `vector.values.str` tell us what the `vec` values are, to put it on
# the print message
get.common.values = function(vec1, vec2, vector.values.str = "nodes") {
  common.values = intersect(names(vec1), names(vec2))
  common.values.number = length(common.values)

  if (common.values.number == 0) {
    print(paste0("No common ", vector.values.str))
    return(NULL)
  }
  else {
    print(paste0(common.values.number, " ", vector.values.str, ": ",
                 paste0(common.values, collapse = ",")))
    return(common.values)
  }
}

print.biomarkers.for.specific.synergy =
  function(drug.comb, biomarkers.active, biomarkers.inhibited) {
    print(paste("Biomarkers for", drug.comb, "synergy prediction"))
    print.empty.line()

    print("Active biomarkers")
    pretty.print.vector.names(biomarkers.active)
    print.empty.line()

    print("Inhibited biomarkers")
    pretty.print.vector.names(biomarkers.inhibited)
    print.empty.line()
}

print.empty.line = function() {
  cat("\n")
}

save.vector.to.file = function(vector, file, with.row.names = FALSE) {
  write.table(vector, file = file, quote = FALSE, col.names = FALSE,
              row.names = with.row.names, sep = "\t")
}

save.df.to.file = function(df, file) {
  write.table(df, file = file, quote = FALSE, col.names = TRUE,
              row.names = TRUE, sep = "\t")
}

# we keep the biomarkers already stored in the respective file and just add the
# new ones
update.biomarkers.files =
  function(biomarkers.dir, drug.comb, biomarkers.active.new, biomarkers.inhibited.new) {
    # update the active biomarkers
    active.biomarkers.file =
      paste0(biomarkers.dir, drug.comb, "_biomarkers_active")

    if (file.size(active.biomarkers.file) == 0) {
      save.vector.to.file(vector = biomarkers.active.new,
                          file = active.biomarkers.file, with.row.names = TRUE)
    } else {
      biomarkers.active.prev =
        read.table(active.biomarkers.file, stringsAsFactors = FALSE)
      biomarkers.active.prev.names = biomarkers.active.prev[,1]
      biomarkers.active.new.names = names(biomarkers.active.new)

      biomarkers.active.to.add = biomarkers.active.new[
        !(biomarkers.active.new.names %in% biomarkers.active.prev.names)
      ]

      biomarkers.active = add.vector.to.data.frame(biomarkers.active.prev,
                                                   biomarkers.active.to.add)
      biomarkers.active = transform(biomarkers.active, V2 = as.numeric(V2))
      save.vector.to.file(vector = biomarkers.active,
                          file = active.biomarkers.file)
    }

    # update the inhibited biomarkers
    inhibited.biomarkers.file =
      paste0(biomarkers.dir, drug.comb, "_biomarkers_inhibited")

    if (file.size(inhibited.biomarkers.file) == 0) {
      save.vector.to.file(vector = biomarkers.inhibited.new,
                          file = inhibited.biomarkers.file, with.row.names = TRUE)
    } else {
      biomarkers.inhibited.prev =
        read.table(inhibited.biomarkers.file, stringsAsFactors = FALSE)
      biomarkers.inhibited.prev.names = biomarkers.inhibited.prev[,1]
      biomarkers.inhibited.new.names = names(biomarkers.inhibited.new)

      biomarkers.inhibited.to.add = biomarkers.inhibited.new[
        !(biomarkers.inhibited.new.names %in% biomarkers.inhibited.prev.names)
      ]

      biomarkers.inhibited = add.vector.to.data.frame(biomarkers.inhibited.prev,
                                                      biomarkers.inhibited.to.add)
      biomarkers.inhibited = transform(biomarkers.inhibited, V2 = as.numeric(V2))
      save.vector.to.file(vector = biomarkers.inhibited,
                          file = inhibited.biomarkers.file)
    }
}

add.vector.to.data.frame = function(df, vec) {
  if (length(vec) == 0) return(df)
  for (i in 1:length(vec)) {
    value = vec[i]
    name = names(vec)[i]
    df = rbind(df, c(name, value))
  }
  return(df)
}

output.data.to.file = function(dir.to.save, filename, data, with.col.names) {
  output.file = paste0(dir.to.save, "/", filename)
  write.table(data, output.file, append = FALSE, sep = "\t", dec = ".",
              col.names = with.col.names, quote = FALSE)
}