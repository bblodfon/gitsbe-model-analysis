specify.decimal = function(number, digits.to.keep) {
  trimws(format(round(number, digits.to.keep), nsmall = digits.to.keep))
}

print.mcc.classification.info = function(mcc.classes) {
  number.of.mcc.classes = length(mcc.classes)
  print(paste0("MCC values are split into ",
               number.of.mcc.classes, " classes:"))

  for (i in 1:number.of.mcc.classes) {
    print(paste0(i, ". ", mcc.classes[i]))
  }
}

output.diff.to.file = function(cell.line, string, diff) {
  drug.comb = string
  output.file = paste0(getwd(), "/", cell.line, "/", cell.line, "_", drug.comb,
                      "_diff")
  write.table(diff, file = output.file, quote = FALSE, col.names = FALSE)
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

plot.pdf = function(file, plot.string) {
  pdf(file);
  eval(parse(text = plot.string));
  dev.off()
}

plot.svg = function(file, plot.string) {
  svg(file, width = 7, height = 7);
  eval(parse(text = plot.string));
  dev.off()
}

plot.png = function(file, plot.string) {
  png(file, width = 7, height = 7, units = 'in', res = 300);
  eval(parse(text = plot.string));
  dev.off()
}

plot.tiff = function(file, plot.string) {
  tiff(file, width = 7, height = 7, units = 'in', res = 300);
  eval(parse(text = plot.string));
  dev.off()
}