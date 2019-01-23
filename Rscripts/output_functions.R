# `digits.to.keep` refers to digits after decimal point '.'
specify.decimal = function(number, digits.to.keep) {
  trimws(format(round(number, digits.to.keep), nsmall = digits.to.keep))
}

# nice printing of a string in an R notebook
# use `with.gt = FALSE` when you want to nicely print multiple lines
# ('gt' is the '>' symbol)
pretty.print.string = function(string, with.gt = TRUE) {
  if (with.gt)
    cat(paste0("> ", string))
  else
    cat(string)
}

# prints a bold string only when `html.output` is enabled. Otherwise,
# prints a normal string
pretty.print.bold.string =
  function(string, with.gt = TRUE, html.output = TRUE) {
    if (html.output) {
      bold.string = paste0("<b>", string, "</b>")
      if (with.gt)
        cat(paste0("> ", bold.string))
      else
        cat(bold.string)
    } else {
      pretty.print.string(string, with.gt = with.gt)
    }
}

print.empty.line = function(html.output = FALSE) {
  if (html.output)
    cat("<br/>")
  else
    cat("\n")
}

print.model.and.drug.stats =
  function(drug.combs, models, nodes, html.output) {
    pretty.print.string(paste("Drug combinations tested:", drug.combs))
    print.empty.line(html.output)
    pretty.print.string(paste("Number of models:", models), with.gt = FALSE)
    print.empty.line(html.output)
    pretty.print.string(paste("Number of nodes:", nodes), with.gt = FALSE)
}

print.mcc.classification.info = function(mcc.classes, html.output) {
  number.of.mcc.classes = length(mcc.classes)
  pretty.print.string(paste0("MCC values are split into ",
                      number.of.mcc.classes, " classes:"))
  print.empty.line(html.output)
  for (i in 1:number.of.mcc.classes) {
    pretty.print.string(paste0(i, ". ", mcc.classes[i]), with.gt = FALSE)
    print.empty.line(html.output)
  }
}

# `vector.names.str` tell us what `names(vec)` actually is, to put it on
# the print message
pretty.print.vector.names = function(vec, vector.names.str = "nodes",
                                     seperator = ", ", with.gt = TRUE) {
  if (length(vec) == 1) {
    vector.names.str = substr(vector.names.str, start = 1,
                              stop = nchar(vector.names.str) - 1)
  }
  pretty.print.string(paste0(length(vec), " ", vector.names.str, ": ",
                      paste0(names(vec), collapse = seperator)), with.gt)
}

# `vector.values.str` tell us what the `vec` values are, to put it on
# the print message
pretty.print.vector.values = function(vec, vector.values.str = "nodes",
                                      seperator = ", ", with.gt = TRUE) {
  if (length(vec) == 1) {
    vector.values.str = substr(vector.values.str, start = 1,
                              stop = nchar(vector.values.str) - 1)
  }
  pretty.print.string(paste0(length(vec), " ", vector.values.str, ": ",
                      paste0(vec, collapse = seperator)), with.gt)
}

# get the common `names` from two vectors and print an appropriate message
# `vector.names.str` tell us what `names(vec)` actually is, to put it on
# the print message
get.common.names = function(vec1, vec2, vector.names.str = "nodes",
                            with.gt = TRUE) {
  common.names = intersect(names(vec1), names(vec2))
  common.names.number = length(common.names)

  if (common.names.number == 0) {
    str = paste0("No common ", vector.names.str)
    pretty.print.string(str, with.gt = with.gt)
    return(NULL)
  }
  else {
    pretty.print.vector.names(common.names, with.gt = with.gt)
    return(common.names)
  }
}

# get the common values from two vectors and print an appropriate message
# `vector.values.str` tell us what the `vec` values are, to put it on
# the print message
get.common.values = function(vec1, vec2, vector.values.str = "nodes",
                             with.gt = TRUE) {
  common.values = intersect(vec1, vec2)
  common.values.number = length(common.values)

  if (common.values.number == 0) {
    str = paste0("No common ", vector.values.str)
    pretty.print.string(str, with.gt = with.gt)
    return(NULL)
  }
  else {
    pretty.print.vector.values(common.values, with.gt = with.gt)
    return(common.values)
  }
}

print.biomarkers.per.predicted.synergy =
  function(biomakrers.dir, drug.comb, predicted.synergies, html.output = TRUE) {
    pretty.print.string("")
    for (drug.comb in predicted.synergies) {
      # get the active biomarkers
      active.biomarkers.file =
        paste0(biomarkers.dir, drug.comb, "_biomarkers_active")
      if (file.size(active.biomarkers.file) == 0) {
        biomarkers.active.names = NULL
      } else {
        biomarkers.active =
          read.table(active.biomarkers.file, stringsAsFactors = FALSE)
        biomarkers.active.names = biomarkers.active[,1]
      }

      # get the inhibited biomarkers
      inhibited.biomarkers.file =
        paste0(biomarkers.dir, drug.comb, "_biomarkers_inhibited")
      if (file.size(inhibited.biomarkers.file) == 0) {
        biomarkers.inhibited.names = NULL
      } else {
        biomarkers.inhibited =
          read.table(inhibited.biomarkers.file, stringsAsFactors = FALSE)
        biomarkers.inhibited.names = biomarkers.inhibited[,1]
      }

      # print biomarkers
      str = paste("Biomarkers for", drug.comb, "synergy prediction")
      pretty.print.bold.string(str, with.gt = FALSE, html.output = html.output)
      print.empty.line(html.output)
      print.empty.line(html.output)

      pretty.print.bold.string("Active biomarkers", with.gt = FALSE,
                               html.output = html.output)
      print.empty.line(html.output)
      pretty.print.vector.values(biomarkers.active.names, with.gt = FALSE)
      print.empty.line(html.output)
      print.empty.line(html.output)

      pretty.print.bold.string("Inhibited biomarkers", with.gt = FALSE,
                               html.output = html.output)
      print.empty.line(html.output)
      pretty.print.vector.values(biomarkers.inhibited.names, with.gt = FALSE)
      print.empty.line(html.output)
      print.empty.line(html.output)
    }
}

save.vector.to.file = function(vector, file, with.row.names = FALSE) {
  write.table(vector, file = file, quote = FALSE, col.names = FALSE,
              row.names = with.row.names, sep = "\t")
}

save.df.to.file = function(df, file) {
  write.table(df, file = file, quote = FALSE, col.names = TRUE,
              row.names = TRUE, sep = "\t")
}

# we keep the biomarkers already stored in the respective file
# and just add the new ones
update.biomarkers.files =
  function(biomarkers.dir, drug.comb, biomarkers.active.new,
           biomarkers.inhibited.new) {
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

# `df` is (n x 2) dimensions
add.vector.to.data.frame = function(df, vec) {
  if (length(vec) == 0) return(df)
  for (i in 1:length(vec)) {
    value = vec[i]
    name = names(vec)[i]
    df = rbind(df, c(name, value))
  }
  return(df)
}
