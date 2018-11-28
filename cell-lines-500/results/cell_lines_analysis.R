# empty workspace (if needed)
rm(list = ls(all = T))

require(RColorBrewer)

setwd("~/Dropbox/work/model_prediction_performance_analysis_May_2018/results/")

cell_lines = c("AGS", "A498", "colo205", "DU145", "MDA-MB-468", "SF295", "SW620", "UACC62")

for (cell_line in cell_lines) {
  # get stable states data for each cell line
  filename_ss = paste(cell_line, "_models_stable_state.txt", sep = "")
  assign(paste(cell_line, "_ss", sep = ""), read.table(filename_ss))
  
  # get equations data for each cell line
  filename_eq = paste(cell_line, "_models_equations.txt", sep = "")
  assign(paste(cell_line, "_eq", sep = ""), read.table(filename_eq))
  
  # get model predictions data for each cell_line
  filename_pred = paste(cell_line, "_models_predictions.txt", sep = "")
  assign(paste(cell_line, "_pred", sep = ""), read.table(filename_pred,  check.names = FALSE))
}

# Order matters (as in cell_lines)
cell_lines_ss = do.call("rbind", list(AGS_ss, A498_ss, colo205_ss, DU145_ss, 
                              `MDA-MB-468_ss`, SF295_ss, SW620_ss, UACC62_ss))
cell_lines_eq = do.call("rbind", list(AGS_eq, A498_eq, colo205_eq, DU145_eq, 
                              `MDA-MB-468_eq`, SF295_eq, SW620_eq, UACC62_eq))
cell_lines_pred = do.call("rbind", list(AGS_pred, A498_pred, colo205_pred, DU145_pred, 
                                        `MDA-MB-468_pred`, SF295_pred, SW620_pred, UACC62_pred))

################
### HEATMAPS ###
################
num_colors = length(cell_lines)
color_samples = brewer.pal(num_colors, name = "Set1")
display.brewer.pal(num_colors, name = "Set1") 
row_colors = c(rep(color_samples[1], nrow(AGS_ss)),          rep(color_samples[2], nrow(A498_ss)),
               rep(color_samples[3], nrow(colo205_ss)),      rep(color_samples[4], nrow(DU145_ss)),
               rep(color_samples[5], nrow(`MDA-MB-468_ss`)), rep(color_samples[6], nrow(SF295_ss)),
               rep(color_samples[7], nrow(SW620_ss)),        rep(color_samples[8], nrow(UACC62_ss)))

png("stable_states_heatmap2.png", width = 7, height = 7, units = 'in', res = 300);
heatmap(as.matrix(cell_lines_ss), RowSideColors = row_colors, scale = "none", labRow = NA, cexCol=0.4, margins=c(8,2))
legend(x = 7, y = 1.56, xpd = TRUE, legend = cell_lines, col = color_samples, lty = 1, lwd = 10, cex = 0.6)
legend(x = 0.8, y = 0.8, legend = cell_lines, col = color_samples, lty = 1, lwd = 10, cex = 0.6)
dev.off()

png("equations_heatmap2.png", width = 7, height = 7, units = 'in', res = 300);
heatmap(as.matrix(cell_lines_eq), RowSideColors = row_colors, scale = "none", labRow = NA, cexCol=0.4, margins=c(8,2))
legend(x = 7, y = 1.56, xpd = TRUE, legend = cell_lines, col = color_samples, lty = 1, lwd = 10, cex = 0.6)
legend("topright", legend = cell_lines, col = color_samples, lty = 1, lwd = 10, cex = 0.6)
dev.off()






#### testing stuff
cell_lines_test = c('AGS','A498','colo205')
num_colors = length(cell_lines_test)
color_samples = brewer.pal(num_colors, name = "Set1")
display.brewer.pal(num_colors, name = "Set1")

names = c(rep(cell_lines_test[1], 500), rep(cell_lines_test[2], 500), rep(cell_lines_test[3], 500))
row_colors = c(rep(color_samples[1], 500), rep(color_samples[2], 500), rep(color_samples[3], 500))

heatmap(as.matrix(AGS_ss), RowSideColors = row_colors, scale = "none", labRow = NA, cexCol=0.4, margins=c(8,2))
legend(x = 2.8, y = 1.55, xpd = TRUE, legend = cell_lines_test, col = color_samples, lty = 1, lwd = 10, cex = 0.7)







