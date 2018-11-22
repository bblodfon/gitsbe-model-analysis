print.empty.line = function() {
  cat("\n")
}

output.diff.to.file = function(cell.line, string, diff) {
  drug.comb = string
  output.file = paste(getwd(), "/", cell.line, "/", cell.line, "_", drug.comb,
                      "_diff", sep = "")
  write.table(diff, file = output.file, quote = FALSE, col.names = FALSE)
}

output.vector.to.file = function(cell.line, string, vec, with.row.names) {
  output.file = paste(getwd(), "/", cell.line, "/", string, ".txt", sep = "")
  write.table(vec, file = output.file, quote = FALSE, col.names = FALSE,
              row.names = with.row.names, sep = "\t")
}

output.data.to.file = function(dir.to.save, filename, data, with.col.names) {
  output.file = paste(dir.to.save, "/", filename, sep = "")
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