load("simulation_output_F.RData")
load("gammamodelsF.RData")

a = (t1[which.max(rowSums(sta_A4F))] - ((coef(alt1AF)[1] - 1) / coef(alt1AF)[2]))
b = ((t1[which.max(rowSums(sta_A4AF))] - a) - ((coef(alt2AF)[1] - 1) / coef(alt2AF)[2])) / 2

crwD <- data.frame(x = seq(0, longF, stepy) - (a + b), y = rowSums(sta_A1F))

library(SuppDists)
library(ExtDist)
library(bbmle)


estimat <- function(DDs, prs, method){
  LL1 <- function(gamma, delta, a, b) {
    -sum(pr * log(dJohnsonSB_ab(x = x, gamma = gamma, delta = delta, a = a, b = b)))
  }
  
  if(method == "L-BFGS-B"){
    MLL <- mle2(LL1, start = list(gamma = -0.5, delta = 2, a = min(DDs) - 1, b = max(DDs) + 1), 
                data = list(x = DDs, pr = prs),
                lower = list(gamma = -Inf, delta = 0, a = -Inf, b = max(DDs)), 
                upper = list(gamma = Inf, delta = Inf, a = min(DDs), b = Inf), method = "L-BFGS-B")
  }
  
  if(method == "Nelder-Mead"){
    MLL <- mle2(LL1, start = list(gamma = -0.5, delta = 2, a = min(DDs) - 1, b = max(DDs) + 1), 
                data = list(x = DDs, pr = prs),
                method = "Nelder-Mead")
  }
  
  MLL
}

MLL2 <- estimat(DDs = crwD$x, prs = crwD$y, method = "Nelder-Mead")
summary(MLL2)

funres2 <- function(DDs){
  xi = coef(MLL2)[3]
  lambda = coef(MLL2)[4] - coef(MLL2)[3]
  dJohnsonSB(DDs, gamma = coef(MLL2)[1],
             delta = coef(MLL2)[2],
             xi = xi,
             lambda = lambda)
}


funres2a <- function(DDs){
  xi = coef(MLL2)[3]
  lambda = coef(MLL2)[4] - coef(MLL2)[3]
  pJohnsonSB(DDs, gamma = coef(MLL2)[1],
             delta = coef(MLL2)[2],
             xi = xi,
             lambda = lambda)
}


plot(crwD$x, crwD$y/364081.8, type = "l", xlim = c(-558, 3060))
lines(crwD$x, funres2(crwD$x), col = "red")

plot(crwD$x, cumsum(crwD$y/sum(crwD$y)), type = "l", xlim = c(-558, 3060))
lines(crwD$x, funres2a(crwD$x), col = "red")

crawlers1st <- data.frame(DDs = unique(round(crwD$x)),
                          simModelRel = (crwD$y[c(grep(".5591", as.character(crwD$x)), 
                                             grep(".3158", as.character(crwD$x)))] / 364081.8) * 10000,
                          JohnsonSBRel = funres2(unique(round(crwD$x))) * 10000,
                          simModelCum = cumsum((crwD$y / sum(crwD$y[c(grep(".5591", as.character(crwD$x)), 
                                                                      grep(".3158", as.character(crwD$x)))]))[c(grep(".5591", as.character(crwD$x)), 
                                                        grep(".3158", as.character(crwD$x)))]),
                          JohnsonSBCum = funres2a(unique(round(crwD$x))))





plot(crawlers1st$DDs, crawlers1st$simModelRel, type = "l", xlim = c(-558, 3060))
lines(crawlers1st$DDs, crawlers1st$JohnsonSBRel, col = "red")

plot(crawlers1st$DDs, crawlers1st$simModelCum, type = "l", xlim = c(-558, 3060))
lines(crawlers1st$DDs, crawlers1st$JohnsonSBCum, col = "red")

###########


n2D <- data.frame(x = seq(0, longF, stepy) - (a + b), y = rowSums(sta_A2F))

MLL2n2 <- estimat(DDs = n2D$x, prs = n2D$y, method = "Nelder-Mead")
summary(MLL2n2)



funres2n2 <- function(DDs){
  xi = coef(MLL2n2)[3]
  lambda = coef(MLL2n2)[4] - coef(MLL2n2)[3]
  dJohnsonSB(DDs, gamma = coef(MLL2n2)[1],
             delta = coef(MLL2n2)[2],
             xi = xi,
             lambda = lambda)
}


funres2an2 <- function(DDs){
  xi = coef(MLL2n2)[3]
  lambda = coef(MLL2n2)[4] - coef(MLL2n2)[3]
  pJohnsonSB(DDs, gamma = coef(MLL2n2)[1],
             delta = coef(MLL2n2)[2],
             xi = xi,
             lambda = lambda)
}

plot(n2D$x, n2D$y/213902, type = "l", xlim = c(-558, 3060))
lines(n2D$x, funres2n2(n2D$x), col = "red")

plot(n2D$x, cumsum(n2D$y/sum(n2D$y)), type = "l", xlim = c(-558, 3060))
lines(n2D$x, funres2an2(n2D$x), col = "red")





nym2nd <- data.frame(DDs = unique(round(n2D$x)),
                     simModelRel = (n2D$y[c(grep(".5591", as.character(n2D$x)), 
                                             grep(".3158", as.character(n2D$x)))] / 213902) * 10000,
                     JohnsonSBRel = funres2n2(unique(round(n2D$x))) * 10000,
                     simModelCum = cumsum((n2D$y / sum(n2D$y[c(grep(".5591", as.character(n2D$x)), 
                                                                 grep(".3158", as.character(n2D$x)))]))[c(grep(".5591", as.character(n2D$x)), 
                                                                                                          grep(".3158", as.character(n2D$x)))]),
                     JohnsonSBCum = funres2an2(unique(round(n2D$x))))

plot(nym2nd$DDs, nym2nd$simModelRel, type = "l", xlim = c(-558, 3060))
lines(nym2nd$DDs, nym2nd$JohnsonSBRel, col = "red")

plot(nym2nd$DDs, nym2nd$simModelCum, type = "l", xlim = c(-558, 3060))
lines(nym2nd$DDs, nym2nd$JohnsonSBCum, col = "red")



###########


n3D <- data.frame(x = seq(0, longF, stepy) - (a + b), y = rowSums(sta_A3F))

MLL2n3 <- estimat(DDs = n3D$x, prs = n3D$y, method = "Nelder-Mead")
summary(MLL2n3)



funres2n3 <- function(DDs){
  xi = coef(MLL2n3)[3]
  lambda = coef(MLL2n3)[4] - coef(MLL2n3)[3]
  dJohnsonSB(DDs, gamma = coef(MLL2n3)[1],
             delta = coef(MLL2n3)[2],
             xi = xi,
             lambda = lambda)
}


funres2an3 <- function(DDs){
  xi = coef(MLL2n3)[3]
  lambda = coef(MLL2n3)[4] - coef(MLL2n3)[3]
  pJohnsonSB(DDs, gamma = coef(MLL2n3)[1],
             delta = coef(MLL2n3)[2],
             xi = xi,
             lambda = lambda)
}


max(n3D$y)/max(funres2n3(n3D$x))

plot(n3D$x, n3D$y/327440.1, type = "l", xlim = c(-558, 3060))
lines(n3D$x, funres2n3(n2D$x), col = "red")

plot(n3D$x, cumsum(n3D$y/sum(n3D$y)), type = "l", xlim = c(-558, 3060))
lines(n3D$x, funres2an3(n3D$x), col = "red")


nym3rd <- data.frame(DDs = unique(round(n3D$x)),
                     simModelRel = (n3D$y[c(grep(".5591", as.character(n3D$x)), 
                                            grep(".3158", as.character(n3D$x)))] / 327440.1) * 10000,
                     JohnsonSBRel = funres2n3(unique(round(n3D$x))) * 10000,
                     simModelCum = cumsum((n3D$y / sum(n3D$y[c(grep(".5591", as.character(n3D$x)), 
                                                               grep(".3158", as.character(n3D$x)))]))[c(grep(".5591", as.character(n3D$x)), 
                                                                                                       grep(".3158", as.character(n3D$x)))]),
                     JohnsonSBCum = funres2an3(unique(round(n3D$x))))

plot(nym3rd$DDs, nym3rd$simModelRel, type = "l", xlim = c(-558, 3060))
lines(nym3rd$DDs, nym3rd$JohnsonSBRel, col = "red")

plot(nym3rd$DDs, nym3rd$simModelCum, type = "l", xlim = c(-558, 3060))
lines(nym3rd$DDs, nym3rd$JohnsonSBCum, col = "red")




###########


malD <- data.frame(x = seq(0, longF, stepy) - (a + b), y = rowSums(sta_A4F))

MLL2ma <- estimat(DDs = malD$x, prs = malD$y, method = "Nelder-Mead")
summary(MLL2ma)



funres2ma <- function(DDs){
  xi = coef(MLL2ma)[3]
  lambda = coef(MLL2ma)[4] - coef(MLL2ma)[3]
  dJohnsonSB(DDs, gamma = coef(MLL2ma)[1],
             delta = coef(MLL2ma)[2],
             xi = xi,
             lambda = lambda)
}


funres2ama <- function(DDs){
  xi = coef(MLL2ma)[3]
  lambda = coef(MLL2ma)[4] - coef(MLL2ma)[3]
  pJohnsonSB(DDs, gamma = coef(MLL2ma)[1],
             delta = coef(MLL2ma)[2],
             xi = xi,
             lambda = lambda)
}


max(malD$y)/max(funres2ma(malD$x))

plot(malD$x, malD$y/18751.47, type = "l", xlim = c(-558, 3060))
lines(malD$x, funres2ma(malD$x), col = "red")

plot(malD$x, cumsum(malD$y/sum(malD$y)), type = "l", xlim = c(-558, 3060))
lines(malD$x, funres2ama(malD$x), col = "red")


males <- data.frame(DDs = unique(round(malD$x)),
                     simModelRel = (malD$y[c(grep(".5591", as.character(malD$x)), 
                                            grep(".3158", as.character(malD$x)))] / 18751.47) * 10000,
                     JohnsonSBRel = funres2ma(unique(round(malD$x))) * 10000,
                     simModelCum = cumsum((malD$y / sum(malD$y[c(grep(".5591", as.character(malD$x)), 
                                                               grep(".3158", as.character(malD$x)))]))[c(grep(".5591", as.character(malD$x)), 
                                                                                                       grep(".3158", as.character(malD$x)))]),
                     JohnsonSBCum = funres2ama(unique(round(malD$x))))

plot(males$DDs, males$simModelRel, type = "l", xlim = c(-558, 3060))
lines(males$DDs, males$JohnsonSBRel, col = "red")

plot(males$DDs, males$simModelCum, type = "l", xlim = c(-558, 3060))
lines(males$DDs, males$JohnsonSBCum, col = "red")



###########

estimatF <- function(DDs, prs){
  LL1 <- function(gamma, delta, a) {
    -sum(pr * log(dJohnsonSB_ab(x = x, gamma = gamma, delta = delta, a = a, b = 1977346.3)))
  }
  
  MLL <- mle2(LL1, start = list(gamma = -0.5, delta = 2, a = min(DDs) - 1), 
              data = list(x = DDs, pr = prs),
              method = "Nelder-Mead")
  
  MLL
}


femD <- data.frame(x = seq(0, longF, stepy) - (a + b), y = rowSums(sta_A4aF))

MLL2fem <- estimatF(DDs = femD$x, prs = femD$y)
summary(MLL2fem)



funres2fem <- function(DDs){
  xi = coef(MLL2fem)[3]
  lambda = 1977346.3 - coef(MLL2fem)[3]
  dJohnsonSB(DDs, gamma = coef(MLL2fem)[1],
             delta = coef(MLL2fem)[2],
             xi = xi,
             lambda = lambda)
}


funres2afem <- function(DDs){
  xi = coef(MLL2fem)[3]
  lambda = 1977346.3 - coef(MLL2fem)[3]
  pJohnsonSB(DDs, gamma = coef(MLL2fem)[1],
             delta = coef(MLL2fem)[2],
             xi = xi,
             lambda = lambda)
}


max(femD$y)/max(funres2fem(femD$x))

plot(femD$x, femD$y/462320.9, type = "l", xlim = c(-558, 3060))
lines(femD$x, funres2fem(femD$x), col = "red")

plot(femD$x, cumsum(femD$y/sum(femD$y)), type = "l", xlim = c(-558, 3060))
lines(femD$x, funres2afem(femD$x), col = "red")


females <- data.frame(DDs = unique(round(femD$x)),
                    simModelRel = (femD$y[c(grep(".5591", as.character(femD$x)), 
                                            grep(".3158", as.character(femD$x)))] / 462320.9) * 10000,
                    JohnsonSBRel = funres2fem(unique(round(femD$x))) * 10000,
                    simModelCum = cumsum((femD$y / sum(femD$y[c(grep(".5591", as.character(femD$x)), 
                                                                grep(".3158", as.character(femD$x)))]))[c(grep(".5591", as.character(femD$x)), 
                                                                                                         grep(".3158", as.character(femD$x)))]),
                    JohnsonSBCum = funres2afem(unique(round(femD$x))))

plot(females$DDs, females$simModelRel, type = "l", xlim = c(-558, 6060))
lines(females$DDs, females$JohnsonSBRel, col = "red")

plot(females$DDs, females$simModelCum, type = "l", xlim = c(-558, 3060))
lines(females$DDs, females$JohnsonSBCum, col = "red")



##############

RelAbundanceSM <- data.frame(FDDs = unique(round(crwD$x)),
                             Crawlers = crawlers1st$simModelRel,
                             Nymph2 = nym2nd$simModelRel,
                             Nymph3 = nym3rd$simModelRel,
                             Males = males$simModelRel,
                             Females = females$simModelRel)

CumEmergenceSM <- data.frame(FDDs = unique(round(crwD$x)),
                             Crawlers = crawlers1st$simModelCum,
                             Nymph2 = nym2nd$simModelCum,
                             Nymph3 = nym3rd$simModelCum,
                             Males = males$simModelCum,
                             Females = females$simModelCum)


RelAbundanceJSB <- data.frame(FDDs = unique(round(crwD$x)),
                              Crawlers = crawlers1st$JohnsonSBRel,
                              Nymph2 = nym2nd$JohnsonSBRel,
                              Nymph3 = nym3rd$JohnsonSBRel,
                              Males = males$JohnsonSBRel,
                              Females = females$JohnsonSBRel)

CumEmergenceJSB <- data.frame(FDDs = unique(round(crwD$x)),
                              Crawlers = crawlers1st$JohnsonSBCum,
                              Nymph2 = nym2nd$JohnsonSBCum,
                              Nymph3 = nym3rd$JohnsonSBCum,
                              Males = males$JohnsonSBCum,
                              Females = females$JohnsonSBCum)


par(mar = c(5.5, 5, 3, 2) + 0.1)
plot(RelAbundanceSM$FDDs, 
     RelAbundanceSM$Crawlers, type = "l", ylab = "", xlab = "",
     xlim = c(-558, 3300), ylim = c(0, 25), yaxt = "n", xaxt = "n", cex.lab = 2, cex.axis = 2, lwd = 2)

axis(2, at = seq(0, 25), labels = FALSE)
axis(3, at = c(-(a + b), 0), labels = c("Egg laying peak", "Jan 1st"), cex.axis = 1.8)
axis(1, at = c(seq(-900, -180, 360), 0, seq(180, 3060, 360)), cex.axis = 2.2)

lines(RelAbundanceSM$FDDs, RelAbundanceSM$Nymph2, lwd = 2, lty = 2)
lines(RelAbundanceSM$FDDs, RelAbundanceSM$Nymph3, lwd = 2, lty = 2)
lines(RelAbundanceSM$FDDs, RelAbundanceSM$Males, lwd = 2)
lines(RelAbundanceSM$FDDs, RelAbundanceSM$Females, lwd = 2)

polygon(c(RelAbundanceSM$FDDs, 0), c(RelAbundanceSM$Crawlers, 0), col = crowcol, border = NA)
polygon(c(RelAbundanceSM$FDDs, 0), c(RelAbundanceSM$Nymph2, 0), col = nymcol, border = NA)
polygon(c(RelAbundanceSM$FDDs, 0), c(RelAbundanceSM$Nymph3, 0), col = nymcol, border = NA)

polygon(c(RelAbundanceSM$FDDs, 0), c(RelAbundanceSM$Males, 0), col = malecol, border = NA)
polygon(c(RelAbundanceSM$FDDs, 0), c(RelAbundanceSM$Females, 0), col = femcol, border = NA)

abline(v = 0, lwd = 2, lty = 3)
abline(v = -(a + b), lwd = 2, lty = 3)

title(ylab = "Relative abundance", cex.lab = 3, line = 3)
title(xlab = "Degree days (F)", cex.lab = 3, line  = 4)



#############



par(mar = c(5.5, 7, 3, 2) + 0.1)
plot(CumEmergenceSM$FDDs, 
     CumEmergenceSM$Crawlers, type = "l", ylab = "", xlab = "",
     xlim = c(-558, 3300), ylim = c(0, 1), yaxt = "n", xaxt = "n", cex.lab = 2, cex.axis = 2, lwd = 2, col = crowcol)

axis(2, at = seq(0, 1, 0.2), cex.axis = 2.2, las = 2)
axis(3, at = c(-(a + b), 0), labels = c("Egg laying peak", "Jan 1st"), cex.axis = 1.8)
axis(1, at = c(seq(-900, -180, 360), 0, seq(180, 3060, 360)), cex.axis = 2.2)

lines(CumEmergenceSM$FDDs, CumEmergenceSM$Nymph2, lwd = 2, lty = 2, col = nymcol)
lines(CumEmergenceSM$FDDs, CumEmergenceSM$Nymph3, lwd = 2, lty = 2, col = nymcol)
lines(CumEmergenceSM$FDDs, CumEmergenceSM$Males, lwd = 2, col = malecol)
lines(CumEmergenceSM$FDDs, CumEmergenceSM$Females, lwd = 2, col = femcol)


abline(v = 0, lwd = 2, lty = 3)
abline(v = -(a + b), lwd = 2, lty = 3)

title(ylab = "Cumulative emergence", cex.lab = 3, line = 4.5)
title(xlab = "Degree days (F)", cex.lab = 3, line  = 4)






##############


eggsL <- round((seq(0, longF, stepy) - (a + b))[(which.max(rowSums(sta_A5AF)))])

RelAbundanceSMV2 <- data.frame(FDDs = seq(1, 3330), # 3330 = eggsL + portion of dev with neg DDs
                               Crawlers = c(RelAbundanceSM$Crawlers[which(RelAbundanceSM$FDDs > 0 & RelAbundanceSM$FDDs < eggsL)], 
                                            RelAbundanceSM$Crawlers[which(RelAbundanceSM$FDDs < 0)]),
                               Nymph2 = c(RelAbundanceSM$Nymph2[which(RelAbundanceSM$FDDs > 0 & RelAbundanceSM$FDDs < eggsL)], 
                                          RelAbundanceSM$Nymph2[which(RelAbundanceSM$FDDs < 0)]),
                               Nymph3 = RelAbundanceSM$Nymph3[which(RelAbundanceSM$FDDs > 0 & RelAbundanceSM$FDDs <= 3330)],
                               Males = RelAbundanceSM$Males[which(RelAbundanceSM$FDDs > 0 & RelAbundanceSM$FDDs <= 3330)],
                               Females = RelAbundanceSM$Females[which(RelAbundanceSM$FDDs > 0 & RelAbundanceSM$FDDs <= 3330)])


par(mar = c(5.5, 5, 3, 2) + 0.1)
plot(RelAbundanceSMV2$FDDs, 
     RelAbundanceSMV2$Crawlers, type = "l", ylab = "", xlab = "",
     xlim = c(-100, 3300), ylim = c(0, 25), yaxt = "n", xaxt = "n", cex.lab = 2, cex.axis = 2, lwd = 2)

axis(2, at = seq(0, 25), labels = FALSE)
axis(3, at = c(0, eggsL), labels = c("Jan 1st", "Summer egg laying peak"), cex.axis = 1.8)
axis(1, at = c(0, seq(180, 3060, 360)), cex.axis = 2.2)

lines(RelAbundanceSMV2$FDDs, RelAbundanceSMV2$Nymph2, lwd = 2, lty = 2)
lines(RelAbundanceSMV2$FDDs, RelAbundanceSMV2$Nymph3, lwd = 2, lty = 2)
lines(RelAbundanceSMV2$FDDs, RelAbundanceSMV2$Males, lwd = 2)
lines(RelAbundanceSMV2$FDDs, RelAbundanceSMV2$Females, lwd = 2)

polygon(c(0, RelAbundanceSMV2$FDDs, 3331), c(0, RelAbundanceSMV2$Crawlers, 0), col = crowcol, border = NA)
polygon(c(0, RelAbundanceSMV2$FDDs, 3331), c(0, RelAbundanceSMV2$Nymph2, 0), col = nymcol, border = NA)
polygon(c(RelAbundanceSMV2$FDDs, 0), c(RelAbundanceSMV2$Nymph3, 0), col = nymcol, border = NA)

polygon(c(RelAbundanceSMV2$FDDs, 0), c(RelAbundanceSMV2$Males, 0), col = malecol, border = NA)
polygon(c(0, RelAbundanceSMV2$FDDs, 3331), c(0, RelAbundanceSMV2$Females, 0), col = femcol, border = NA)

abline(v = 0, lwd = 2, lty = 3)
abline(v = eggsL, lwd = 2, lty = 3)

title(ylab = "Relative abundance", cex.lab = 3, line = 3)
title(xlab = "Degree days (F)", cex.lab = 3, line  = 4)



######



CumEmergenceSMV2 <- data.frame(FDDs = seq(1, 3330), # 3330 = eggsL + portion of dev with neg DDs
                               Crawlers = c(CumEmergenceSM$Crawlers[which(CumEmergenceSM$FDDs > 0 & CumEmergenceSM$FDDs < eggsL)], 
                                            CumEmergenceSM$Crawlers[which(CumEmergenceSM$FDDs < 0)]),
                               Nymph2 = c(CumEmergenceSM$Nymph2[which(CumEmergenceSM$FDDs > 0 & CumEmergenceSM$FDDs < eggsL)], 
                                          CumEmergenceSM$Nymph2[which(CumEmergenceSM$FDDs < 0)]),
                               Nymph3 = CumEmergenceSM$Nymph3[which(CumEmergenceSM$FDDs > 0 & CumEmergenceSM$FDDs <= 3330)],
                               Males = CumEmergenceSM$Males[which(CumEmergenceSM$FDDs > 0 & CumEmergenceSM$FDDs <= 3330)],
                               Females = CumEmergenceSM$Females[which(CumEmergenceSM$FDDs > 0 & CumEmergenceSM$FDDs <= 3330)])


par(mar = c(5.5, 7, 3, 2) + 0.1)
plot(CumEmergenceSMV2$FDDs, 
     CumEmergenceSMV2$Crawlers, type = "l", ylab = "", xlab = "",
     xlim = c(-100, 3300), ylim = c(0, 1), yaxt = "n", xaxt = "n", cex.lab = 2, cex.axis = 2, lwd = 2, col = crowcol)

axis(2, at = seq(0, 1, 0.2), cex.axis = 2.2, las = 2)
axis(3, at = c(0, eggsL), labels = c("Jan 1st", "Summer egg laying peak"), cex.axis = 1.8)
axis(1, at = c(0, seq(180, 3060, 360)), cex.axis = 2.2)

lines(CumEmergenceSMV2$FDDs, CumEmergenceSMV2$Nymph2, lwd = 2, lty = 2, col = nymcol)
abline(v = eggsL, lwd = 3, col = "white")
lines(CumEmergenceSMV2$FDDs, CumEmergenceSMV2$Nymph3, lwd = 2, lty = 2, col = nymcol)
lines(CumEmergenceSMV2$FDDs, CumEmergenceSMV2$Males, lwd = 2, col = malecol)
lines(CumEmergenceSMV2$FDDs, CumEmergenceSMV2$Females, lwd = 2, col = femcol)

abline(v = 0, lwd = 2, lty = 3)
abline(v = eggsL, lwd = 2, lty = 3)

title(ylab = "Cumulative emergence", cex.lab = 3, line = 4.5)
title(xlab = "Degree days (F)", cex.lab = 3, line  = 4)

