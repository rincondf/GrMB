load("datasetF.RData")

require(mixtools) # package required to distinguish between mixed probability distributions

myvec2F <- rep(datasetF$DD2, round(datasetF$prop*10)) # vector with frequencies of degree-days using rounded counts

testA2F <- gammamixEM(myvec2F, lambda = c(0.8, 0.2), alpha = c(1.4, 15.5),
                     beta = c(0.009, 0.02), k = 2, maxrestarts = 3, maxit = 10000) # algorithm that distiguished mixed distributions

# Determination of the cut pint by finding the intersection between the distributions

NormIntersectA <- function(x, shape1, scale1, shape2, scale2) {
  (dgamma(x, shape = shape1, scale = scale1) - dgamma(x, shape = shape2, scale = scale2))^2
}

IntersectFA2F <- optimize(NormIntersectA, interval =  c(864, 1080), shape1 = testA2F$gamma.pars[1, 1], 
                         scale1 = testA2F$gamma.pars[2, 1], shape2 = testA2F$gamma.pars[1, 2], 
                         scale2 = testA2F$gamma.pars[2, 2])

# Figure with proportions across degree-days, the distributions and the cut point (intersection of the two)

plot(datasetF$DD2, datasetF$prop/100, ylim = c(0, 0.009), xlab = "Degree-days (F)", ylab = "",  cex.lab = 2, cex.axis = 1.8, lwd = 2, yaxt = "n")
axis(2, at = seq(0, 0.009, 0.001), labels = FALSE, cex.axis = 1.8, las = 2)
title(ylab = "Relative captures", line = 2, cex.lab = 2)


lines(seq(0, 3500), dgamma(seq(0, 3500), shape = testA2F$gamma.pars[1, 1], scale = testA2F$gamma.pars[2, 1]), lwd = 2, col = "blue")
lines(seq(0, 3500), dgamma(seq(0, 3500), shape = testA2F$gamma.pars[1, 2], scale = testA2F$gamma.pars[2, 2]), lwd = 2, col = "brown")
abline(v = IntersectFA2F$minimum, col = "darkgreen", lwd = 2, lty = 3)


# 2. Parameter estimation for each distribution: We could stick to the parameters provided by the analysis above, but I often prefer to use the
# mixtool analysis only to find the cut point and refine the parameter estiomation for each generation only using the corresponding proportions.

# a. Generation 1

# create a new dataset with only counts before the cut point
dataG1FA <- subset(datasetF, datasetF$DD2 < IntersectFA2F$minimum)

# again find proportions per site and date. Remember that the ones we have in the full datset use the total for the whole season as denominator.
# We need a new one using the total for the generation 1.

y1AF <- array(NA, c(40, 6, 3))

for(j in 1: 3) {
  for(i in 1: 6) {
    a <- subset(dataG1FA$MBperWeek, dataG1FA$location == unique(dataG1FA$location)[j] & dataG1FA$year == unique(dataG1FA$year)[i])
    if(length(a) < 1) a <- NA
    y1AF[1: length(a), i, j] <- a/sum(a)
  }
  
}

y1AF

# create a new column with proportions
dataG1FA$prop <- c(y1AF)[!is.na(c(y1AF))]

# Simpliest way to estimate parameters

require(MASS)
alt1AF <- fitdistr(rep(dataG1FA$DD2, round(dataG1FA$prop * 100)), densfun = "gamma")

# Figure that compares the model fit with the full dataset (blue) and the one using only counts for generation 1 (red)

plot(dataG1FA$DD2, dataG1FA$prop, ylim = c(0, 0.8))
lines(seq(0, 2000), dgamma(seq(0, 2000), shape = testA2F$gamma.pars[1, 1], scale = testA2F$gamma.pars[2, 1])*100, lwd = 2, col = "blue")
lines(seq(0, 2000), dgamma(seq(0, 2000), shape = coef(alt1AF)[1], rate = coef(alt1AF)[2])*100, lwd = 2, col = "red")


# As in practice cumulative porportions are more useful , we calculate cumulative proportions per site and date

y1AaF <- array(NA, c(40, 6, 3))

for(j in 1: 3) {
  for(i in 1: 6) {
    a <- subset(dataG1FA$MBperWeek, dataG1FA$location == unique(dataG1FA$location)[j] & dataG1FA$year == unique(dataG1FA$year)[i])
    if(length(a) < 1) a <- NA
    a <- a/sum(a)
    y1AaF[1: length(a), i, j] <- cumsum(a)
  }
  
}

y1AaF

# Create a new column with cumulative proportions
dataG1FA$cprop <- c(y1AaF)[!is.na(c(y1AaF))]

# plot cumulative poeportions with the cumulative version of the gamma distribution for generation 1
plot(dataG1FA$DD2, dataG1FA$cprop, ylim = c(0, 1))
lines(seq(0, 3500), pgamma(seq(0, 3500), shape = coef(alt1AF)[1], rate = coef(alt1AF)[2]), lwd = 2, col = "blue")




# b. Generation 2

# create a new datset with only counts before the cut point
dataG2FA <- subset(datasetF, datasetF$DD2 > IntersectFA2F$minimum)

# Find proportions per site and date.
y2AF <- array(NA, c(40, 6, 3))

for(j in 1: 3) {
  for(i in 1: 6) {
    a <- subset(dataG2FA$MBperWeek, dataG2FA$location == unique(dataG2FA$location)[j] & dataG2FA$year == unique(dataG2FA$year)[i])
    if(length(a) < 1) a <- NA
    y2AF[1: length(a), i, j] <- a/sum(a)
  }
  
}

y2AF

# Create a new column for proportions
dataG2FA$prop <- c(y2AF)[!is.na(c(y2AF))]

# Patrameter estimation
alt2AF <- fitdistr(rep(dataG2FA$DD2, round(dataG2FA$prop * 10)), densfun = "gamma")

# Figure comparing models using thje full datset and only using generation 2.
plot(dataG2FA$DD2, dataG2FA$prop, ylim = c(0, 0.8))
lines(seq(0, 3500), dgamma(seq(0, 3500), shape = testA2F$gamma.pars[1, 2], scale = testA2F$gamma.pars[2, 2])*100, lwd = 2, col = "blue")
lines(seq(0, 3500), dgamma(seq(0, 3500), shape = coef(alt2AF)[1], rate = coef(alt2AF)[2])*100, lwd = 2, col = "red")

# Find cumulative poprportions
y2AaF <- array(NA, c(40, 6, 3))

for(j in 1: 3) {
  for(i in 1: 6) {
    a <- subset(dataG2FA$MBperWeek, dataG2FA$location == unique(dataG2FA$location)[j] & dataG2FA$year == unique(dataG2FA$year)[i])
    if(length(a) < 1) a <- NA
    a <- a/sum(a)
    y2AaF[1: length(a), i, j] <- cumsum(a)
  }
  
}

y2AaF

# create new column with cumulative proportions
dataG2FA$cprop <- c(y2AaF)[!is.na(c(y2AaF))]

# Figure with cumualtive proportions and tjhe cumulative version of the gamma distribution.
plot(dataG2FA$DD2, dataG2FA$cprop, ylim = c(0, 1))
lines(seq(0, 3500), pgamma(seq(0, 3500), shape = coef(alt2AF)[1], rate = coef(alt2AF)[2]), lwd = 2, col = "brown")

######

dds_simF <- seq(1, 3500)
captures1 <- rep(NA, 3500)

for(i in 1: (3500 - 1)) {
  captures1[i] <- sum(dgamma(seq(dds_simF[i], dds_simF[i+1], 0.01), 
                             shape = coef(alt1AF)[1], rate = coef(alt1AF)[2]))
}


captures2 <- rep(NA, 3500)

for(i in 1: (3500 - 1)) {
  captures2[i] <- sum(dgamma(seq(dds_simF[i], dds_simF[i+1], 0.01), 
                             shape = coef(alt2AF)[1], rate = coef(alt2AF)[2]))
}


# Figure with everything together

tiff(filename="test.tiff", width = 4000, height = 1500, res=300)

par(mar = c(5, 4, 2, 2) + 0.1)
plot(dataG1FA$DD2, dataG1FA$prop, ylim = c(0, 1), xlim = c(0, 3500), xlab = "Degree days (F)", ylab = "",  cex.lab = 2, cex.axis = 1.8, lwd = 2, yaxt = "n")
axis(2, at = seq(0, 1, 0.2), cex.axis = 1.8, labels = NA)

lines(dds_simF, captures1, lwd = 2, col = "blue")
#lines(dds_sim2, captures1a, lwd = 2, col = "blue", lty = 4)
points(dataG2FA$DD2, dataG2FA$prop, lwd = 2)
lines(dds_simF, captures2, lwd = 2, col = "brown")
#lines(dds_sim2, captures2a, lwd = 2, col = "brown", lty = 4)

abline(v = IntersectFA2F$minimum, lwd = 2, col = "grey29", lty = 2)

title(ylab = "Proportion captured", line = 2, cex.lab = 2)
dev.off()


save(dataG1FA, dataG2FA, file = "datapointsF.RData")
save(alt1AF, alt2AF, file = "gammamodelsF.RData")


