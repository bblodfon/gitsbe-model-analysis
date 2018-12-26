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
  print(paste0(length(vec), " ", vector.names.str, ": ",
              paste0(names(vec), collapse = ",")))
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

output.vector.to.file = function(cell.line, string, vec, with.row.names) {
  output.file = paste0(getwd(), "/", cell.line, "/", string, ".txt")
  write.table(vec, file = output.file, quote = FALSE, col.names = FALSE,
              row.names = with.row.names, sep = "\t")
}

output.data.to.file = function(dir.to.save, filename, data, with.col.names) {
  output.file = paste0(dir.to.save, "/", filename)
  write.table(data, output.file, append = FALSE, sep = "\t", dec = ".",
              col.names = with.col.names, quote = FALSE)
}