# Figure 4 without datapoints
load("simulation_output_F.RData")
load("gammamodelsF.RData")

require(MASS)

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}



eggcol <- t_col("grey", percent = 50)
crowcol <- t_col("brown", percent = 50)
nymcol <- t_col("brown", percent = 80)
malecol <- t_col("darkgreen", percent = 40)
femcol <- t_col("blue4", percent = 40)

a = (t1[which.max(rowSums(sta_A4F))] - ((coef(alt1AF)[1] - 1) / coef(alt1AF)[2]))
b = ((t1[which.max(rowSums(sta_A4AF))] - a) - ((coef(alt2AF)[1] - 1) / coef(alt2AF)[2])) / 2


# phenology

tiff(filename = "Fig4New.tif",
     width = 2200*2, height = 1700*2, units = "px", res = 500)


###
par(mfrow = c(3, 1), oma = c(6, 6, 1, 1))

par(mar = c(2, 2, 2, 2) + 0.1)
plot(seq(0, longF, stepy) - (a + b), rowSums(sta_AF) / 1000, type = "l", ylab = "", xlab = "",
     xlim = c(-558, 3060), yaxt = "n", xaxt = "n", cex.lab = 2, cex.axis = 2, lwd = 2)

axis(2, at = seq(0, 1, 0.2), labels = FALSE)
axis(3, at = c(-(a + b), 0), labels = c("Eggs laid", "Jan 1st"), cex.axis = 1.8)
axis(1, at = c(seq(-900, -180, 360), 0, seq(180, 3060, 360)), labels = FALSE)

lines(seq(0, longF, stepy) - (a + b), rowSums(sta_A5F) / 10000, lwd = 2)
lines(seq(0, longF, stepy) - (a + b), rowSums(sta_A5AF) / 300000, lwd = 2)
polygon(c(seq(0, longF, stepy) - (a + b), - (a + b)), c(rowSums(sta_AF) / 1000, 0), col = eggcol, border = NA)
polygon(c(seq(0, longF, stepy) - (a + b), - (a + b)), c(rowSums(sta_A5F) / 10000, 0), col = eggcol, border = NA)
polygon(c(seq(0, longF, stepy) - (a + b), 3420), c(rowSums(sta_A5AF) / 300000, 0), col = eggcol, border = NA)
mtext("A", cex = 1.7, adj = -0.07)


plot(seq(0, longF, stepy) - (a + b), rowSums(sta_A1F) / 1000, type = "l", ylab = "", xlab = "",
     xlim = c(-558, 3060), yaxt = "n", xaxt = "n", cex.lab = 2, cex.axis = 2, lwd = 2)

axis(2, at = seq(0, 1, 0.2), labels = FALSE)
axis(3, at = c(-(a + b), 0), labels = c("Eggs laid", "Jan 1st"), cex.axis = 1.8)
axis(1, at = c(seq(-900, -180, 360), 0, seq(180, 3060, 360)), labels = FALSE)

lines(seq(0, longF, stepy) - (a + b), rowSums(sta_A1AF) / 17000, lwd = 2)
polygon(c(seq(0, longF, stepy) - (a + b), - (a + b)), c(rowSums(sta_A1F) / 1000, 0), col = crowcol, border = NA)
polygon(c(seq(0, longF, stepy) - (a + b), - (a + b)), c(rowSums(sta_A1AF) / 17000, 0), col = crowcol, border = NA)

lines(seq(0, longF, stepy) - (a + b), rowSums(sta_A2F) / 600, lwd = 2, lty = 3)
lines(seq(0, longF, stepy) - (a + b), rowSums(sta_A3F) / 650, lwd = 2, lty = 3)
polygon(c(seq(0, longF, stepy) - (a + b), - (a + b)), c(rowSums(sta_A2F) / 600, 0), col = nymcol, border = NA)
polygon(c(seq(0, longF, stepy) - (a + b), - (a + b)), c(rowSums(sta_A3F) / 650, 0), col = nymcol, border = NA)

lines(seq(0, longF, stepy) - (a + b), rowSums(sta_A2AF) / 9000, lwd = 2, lty = 3)
lines(seq(0, longF, stepy) - (a + b), rowSums(sta_A3AF) / 12000, lwd = 2, lty = 3)
polygon(c(seq(0, longF, stepy) - (a + b), - (a + b)), c(rowSums(sta_A2AF) / 9000, 0), col = nymcol, border = NA)
polygon(c(seq(0, longF, stepy) - (a + b), - (a + b)), c(rowSums(sta_A3AF) / 12000, 0), col = nymcol, border = NA)


mtext("B", cex = 1.7, adj = -0.07)



plot(seq(0, longF, stepy) - (a + b), rowSums(sta_A4F) / 45, type = "l", ylab = "", xlab = "",
     xlim = c(-558, 3060), yaxt = "n", xaxt = "n", cex.lab = 2, cex.axis = 2, lwd = 2, ylim = c(0, 0.75))

axis(2, at = seq(0, 1, 0.2), labels = FALSE)
axis(3, at = c(-(a + b), 0), labels = c("Eggs laid", "Jan 1st"), cex.axis = 1.8)
axis(1, at = c(seq(-900, -180, 360), 0, seq(180, 3060, 360)), cex.axis = 2.2)

lines(seq(0, longF, stepy) - (a + b), rowSums(sta_A4aF) / 600, lwd = 2)
lines(seq(0, longF, stepy) - (a + b), rowSums(sta_A4AF) / 1100, lwd = 2)
lines(seq(0, longF, stepy) - (a + b), rowSums(sta_A4AaF) / 19000, lwd = 2)

polygon(c(seq(0, longF, stepy) - (a + b), - (a + b)), c(rowSums(sta_A4F) / 45, 0), col = malecol, border = NA)
polygon(c(seq(0, longF, stepy) - (a + b), - (a + b)), c(rowSums(sta_A4aF) / 600, 0), col = femcol, border = NA)

polygon(c(seq(0, longF, stepy) - (a + b), - (a + b)), c(rowSums(sta_A4AF) / 1100, 0), col = malecol, border = NA)
polygon(c(seq(0, longF, stepy) - (a + b), 3420), c(rowSums(sta_A4AaF) / 19000, 0), col = femcol, border = NA)

#points(dataG1A$DD2, dataG1A$prop, col = "green3", lwd = 2)
#points(dataG2A$DD2, dataG2A$prop, col = "green3", lwd = 2)

abline(v = (coef(alt1AF)[1] - 1) / coef(alt1AF)[2], col = "blue", lwd = 2, lty = 2)
abline(v = t1[which.max(rowSums(sta_A4F))] - (a + b), lwd = 2, lty = 2)

abline(v = (coef(alt2AF)[1] - 1) / coef(alt2AF)[2], col = "brown", lwd = 2, lty = 2)
abline(v = t1[which.max(rowSums(sta_A4AF))] - (a + b), lwd = 2, lty = 2)
mtext("C", cex = 1.7, adj = -0.07)

title(ylab = "Relative abundance", outer = TRUE, cex.lab = 3, line = 3.5)
title(xlab = "Degree days (F)", outer = TRUE, cex.lab = 3)

dev.off()


