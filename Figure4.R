# Figure 4 without datapoints

load("gamma_models.RData")
load("simulation_output.RData")

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

a = (t1[which.max(rowSums(sta_A4))] - ((coef(alt1A)[1] - 1) / coef(alt1A)[2]))
b = ((t1[which.max(rowSums(sta_A4A))] - a) - ((coef(alt2A)[1] - 1) / coef(alt2A)[2])) / 2


# phenology
par(mfrow = c(3, 1), oma = c(6, 6, 1, 1))

par(mar = c(2, 2, 2, 2) + 0.1)
plot(seq(0, long, stepy) - (a + b), rowSums(sta_A) / 1000, type = "l", ylab = "", xlab = "",
     xlim = c(-310, 1700), yaxt = "n", xaxt = "n", cex.lab = 2, cex.axis = 2, lwd = 2)

axis(2, at = seq(0, 1, 0.2), labels = FALSE)
axis(3, at = c(-(a + b), 0), labels = c("Eggs laid", "Jan 1st"), cex.axis = 1.5)
axis(1, at = c(seq(-500, -100, 200), 0, seq(100, 1700, 200)), labels = FALSE)

lines(seq(0, long, stepy) - (a + b), rowSums(sta_A5) / 10000, lwd = 2)
lines(seq(0, long, stepy) - (a + b), rowSums(sta_A5A) / 300000, lwd = 2)
polygon(c(seq(0, long, stepy) - (a + b), - (a + b)), c(rowSums(sta_A) / 1000, 0), col = eggcol, border = NA)
polygon(c(seq(0, long, stepy) - (a + b), - (a + b)), c(rowSums(sta_A5) / 10000, 0), col = eggcol, border = NA)
polygon(c(seq(0, long, stepy) - (a + b), 1900), c(rowSums(sta_A5A) / 300000, 0), col = eggcol, border = NA)
mtext("A", cex = 1.7, adj = -0.07)


plot(seq(0, long, stepy) - (a + b), rowSums(sta_A1) / 1000, type = "l", ylab = "", xlab = "",
     xlim = c(-310, 1700), yaxt = "n", xaxt = "n", cex.lab = 2, cex.axis = 2, lwd = 2)

axis(2, at = seq(0, 1, 0.2), labels = FALSE)
axis(3, at = c(-(a + b), 0), labels = c("Eggs laid", "Jan 1st"), cex.axis = 1.5)
axis(1, at = c(seq(-500, -100, 200), 0, seq(100, 1700, 200)), labels = FALSE)

lines(seq(0, long, stepy) - (a + b), rowSums(sta_A1A) / 17000, lwd = 2)
polygon(c(seq(0, long, stepy) - (a + b), - (a + b)), c(rowSums(sta_A1) / 1000, 0), col = crowcol, border = NA)
polygon(c(seq(0, long, stepy) - (a + b), - (a + b)), c(rowSums(sta_A1A) / 17000, 0), col = crowcol, border = NA)

lines(seq(0, long, stepy) - (a + b), rowSums(sta_A2) / 600, lwd = 2, lty = 3)
lines(seq(0, long, stepy) - (a + b), rowSums(sta_A3) / 650, lwd = 2, lty = 3)
polygon(c(seq(0, long, stepy) - (a + b), - (a + b)), c(rowSums(sta_A2) / 600, 0), col = nymcol, border = NA)
polygon(c(seq(0, long, stepy) - (a + b), - (a + b)), c(rowSums(sta_A3) / 650, 0), col = nymcol, border = NA)

lines(seq(0, long, stepy) - (a + b), rowSums(sta_A2A) / 9000, lwd = 2, lty = 3)
lines(seq(0, long, stepy) - (a + b), rowSums(sta_A3A) / 12000, lwd = 2, lty = 3)
polygon(c(seq(0, long, stepy) - (a + b), - (a + b)), c(rowSums(sta_A2A) / 9000, 0), col = nymcol, border = NA)
polygon(c(seq(0, long, stepy) - (a + b), - (a + b)), c(rowSums(sta_A3A) / 12000, 0), col = nymcol, border = NA)


mtext("B", cex = 1.7, adj = -0.07)



plot(seq(0, long, stepy) - (a + b), rowSums(sta_A4) / 45, type = "l", ylab = "", xlab = "",
     xlim = c(-310, 1700), yaxt = "n", xaxt = "n", cex.lab = 2, cex.axis = 2, lwd = 2, ylim = c(0, 0.75))

axis(2, at = seq(0, 1, 0.2), labels = FALSE)
axis(3, at = c(-(a + b), 0), labels = c("Eggs laid", "Jan 1st"), cex.axis = 1.5)
axis(1, at = c(seq(-500, -100, 200), 0, seq(100, 1700, 200)), cex.axis = 2)

lines(seq(0, long, stepy) - (a + b), rowSums(sta_A4a) / 600, lwd = 2)
lines(seq(0, long, stepy) - (a + b), rowSums(sta_A4A) / 1100, lwd = 2)
lines(seq(0, long, stepy) - (a + b), rowSums(sta_A4Aa) / 19000, lwd = 2)

polygon(c(seq(0, long, stepy) - (a + b), - (a + b)), c(rowSums(sta_A4) / 45, 0), col = malecol, border = NA)
polygon(c(seq(0, long, stepy) - (a + b), - (a + b)), c(rowSums(sta_A4a) / 600, 0), col = femcol, border = NA)

polygon(c(seq(0, long, stepy) - (a + b), - (a + b)), c(rowSums(sta_A4A) / 1100, 0), col = malecol, border = NA)
polygon(c(seq(0, long, stepy) - (a + b), 1900), c(rowSums(sta_A4Aa) / 19000, 0), col = femcol, border = NA)

#points(dataG1A$DD2, dataG1A$prop, col = "green3", lwd = 2)
#points(dataG2A$DD2, dataG2A$prop, col = "green3", lwd = 2)

abline(v = (coef(alt1A)[1] - 1) / coef(alt1A)[2], col = "blue", lwd = 2, lty = 2)
abline(v = t1[which.max(rowSums(sta_A4))] - (a + b), lwd = 2, lty = 2)

abline(v = (coef(alt2A)[1] - 1) / coef(alt2A)[2], col = "brown", lwd = 2, lty = 2)
abline(v = t1[which.max(rowSums(sta_A4A))] - (a + b), lwd = 2, lty = 2)
mtext("C", cex = 1.7, adj = -0.07)

title(ylab = "Relative number", outer = TRUE, cex.lab = 2.5)
title(xlab = "Degree days", outer = TRUE, cex.lab = 2.5)