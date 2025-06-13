library(SuppDists)
library(ExtDist)
library(bbmle)

load("simulation_output_F.RData")
load("gammamodelsF.RData")

a = (t1[which.max(rowSums(sta_A4F))] - ((coef(alt1AF)[1] - 1) / coef(alt1AF)[2]))
b = ((t1[which.max(rowSums(sta_A4AF))] - a) - ((coef(alt2AF)[1] - 1) / coef(alt2AF)[2])) / 2

crwD2 <- data.frame(x = seq(0, longF, stepy) - (a + b), y = rowSums(sta_A1AF))

estimatC2 <- function(DDs, prs){
  LL1 <- function(gamma, delta, a) {
    -sum(pr * log(dJohnsonSB_ab(x = x, gamma = gamma, delta = delta, a = a, b = 2500000)))
  }
  
  MLL <- mle2(LL1, start = list(gamma = -0.5, delta = 2, a = min(DDs) - 1), 
              data = list(x = DDs, pr = prs),
              method = "Nelder-Mead")
  
  MLL
}


MLL22 <- estimatC2(DDs = crwD2$x, prs = crwD2$y)
summary(MLL22)

funres22 <- function(DDs){
  xi = coef(MLL22)[3]
  lambda = 1600000 - coef(MLL22)[3]
  dJohnsonSB(DDs, gamma = coef(MLL22)[1],
             delta = coef(MLL22)[2],
             xi = xi,
             lambda = lambda)
}


funres2a2 <- function(DDs){
  xi = coef(MLL22)[3]
  lambda = 1600000 - coef(MLL22)[3]
  pJohnsonSB(DDs, gamma = coef(MLL22)[1],
             delta = coef(MLL22)[2],
             xi = xi,
             lambda = lambda)
}



plot(crwD2$x, crwD2$y/(max(crwD2$y)/max(funres22(crwD2$x))), type = "l", xlim = c(-558, 3060))
lines(crwD2$x, funres22(crwD2$x), col = "red")

plot(crwD2$x, cumsum(crwD2$y/sum(crwD2$y)), type = "l", xlim = c(-558, 3060))
lines(crwD2$x, funres2a2(crwD2$x), col = "red")

crawlers1st2 <- data.frame(DDs = unique(round(crwD2$x)),
                           simModelRel = (crwD2$y[c(grep(".5591", as.character(crwD2$x)), 
                                                   grep(".3158", as.character(crwD2$x)))] / (max(crwD2$y)/max(funres22(crwD2$x)))) * 10000,
                           JohnsonSBRel = funres22(unique(round(crwD2$x))) * 10000,
                           simModelCum = cumsum((crwD2$y / sum(crwD2$y[c(grep(".5591", as.character(crwD2$x)), 
                                                                       grep(".3158", as.character(crwD2$x)))]))[c(grep(".5591", as.character(crwD2$x)), 
                                                                                                                 grep(".3158", as.character(crwD2$x)))]),
                           JohnsonSBCum = funres2a2(unique(round(crwD2$x))))





plot(crawlers1st2$DDs, crawlers1st2$simModelRel, type = "l", xlim = c(-558, 3060))
lines(crawlers1st2$DDs, crawlers1st2$JohnsonSBRel, col = "red")

plot(crawlers1st2$DDs, crawlers1st2$simModelCum, type = "l", xlim = c(-558, 3060))
lines(crawlers1st2$DDs, crawlers1st2$JohnsonSBCum, col = "red")

###########


n2D2 <- data.frame(x = seq(0, longF, stepy) - (a + b), y = rowSums(sta_A2AF))

MLL2n22 <- estimatC2(DDs = n2D2$x, prs = n2D2$y)
summary(MLL2n22)



funres2n22 <- function(DDs){
  xi = coef(MLL2n22)[3]
  lambda = 2500000 - coef(MLL2n22)[3]
  dJohnsonSB(DDs, gamma = coef(MLL2n22)[1],
             delta = coef(MLL2n22)[2],
             xi = xi,
             lambda = lambda)
}


funres2an22 <- function(DDs){
  xi = coef(MLL2n22)[3]
  lambda = 2500000 - coef(MLL2n22)[3]
  pJohnsonSB(DDs, gamma = coef(MLL2n22)[1],
             delta = coef(MLL2n22)[2],
             xi = xi,
             lambda = lambda)
}



plot(n2D2$x, n2D2$y/(max(n2D2$y)/max(funres2n22(n2D2$x))), type = "l", xlim = c(-558, 3060))
lines(n2D2$x, funres2n22(n2D2$x), col = "red")

plot(n2D2$x, cumsum(n2D2$y/sum(n2D2$y)), type = "l", xlim = c(-558, 3060))
lines(n2D2$x, funres2an22(n2D2$x), col = "red")





nym2nd2 <- data.frame(DDs = unique(round(n2D2$x)),
                     simModelRel = (n2D2$y[c(grep(".5591", as.character(n2D2$x)), 
                                            grep(".3158", as.character(n2D2$x)))] / (max(n2D2$y)/max(funres2n22(n2D2$x)))) * 10000,
                     JohnsonSBRel = funres2n22(unique(round(n2D2$x))) * 10000,
                     simModelCum = cumsum((n2D2$y / sum(n2D2$y[c(grep(".5591", as.character(n2D2$x)), 
                                                               grep(".3158", as.character(n2D2$x)))]))[c(grep(".5591", as.character(n2D2$x)), 
                                                                                                        grep(".3158", as.character(n2D2$x)))]),
                     JohnsonSBCum = funres2an22(unique(round(n2D2$x))))

plot(nym2nd2$DDs, nym2nd2$simModelRel, type = "l", xlim = c(-558, 3060))
lines(nym2nd2$DDs, nym2nd2$JohnsonSBRel, col = "red")

plot(nym2nd2$DDs, nym2nd2$simModelCum, type = "l", xlim = c(-558, 3060))
lines(nym2nd2$DDs, nym2nd2$JohnsonSBCum, col = "red")



###########


n3D2 <- data.frame(x = seq(0, longF, stepy) - (a + b), y = rowSums(sta_A3AF))

MLL2n32 <- estimatC2(DDs = n3D2$x, prs = n3D2$y)
summary(MLL2n32)



funres2n32 <- function(DDs){
  xi = coef(MLL2n32)[3]
  lambda = 2500000 - coef(MLL2n32)[3]
  dJohnsonSB(DDs, gamma = coef(MLL2n32)[1],
             delta = coef(MLL2n32)[2],
             xi = xi,
             lambda = lambda)
}


funres2an32 <- function(DDs){
  xi = coef(MLL2n32)[3]
  lambda = 2500000 - coef(MLL2n32)[3]
  pJohnsonSB(DDs, gamma = coef(MLL2n32)[1],
             delta = coef(MLL2n32)[2],
             xi = xi,
             lambda = lambda)
}




plot(n3D2$x, n3D2$y/(max(n3D2$y)/max(funres2n32(n3D2$x))), type = "l", xlim = c(-558, 3060))
lines(n3D2$x, funres2n32(n3D2$x), col = "red")

plot(n3D2$x, cumsum(n3D2$y/sum(n3D2$y)), type = "l", xlim = c(-558, 3060))
lines(n3D2$x, funres2an32(n3D2$x), col = "red")


nym3rd2 <- data.frame(DDs = unique(round(n3D2$x)),
                     simModelRel = (n3D2$y[c(grep(".5591", as.character(n3D2$x)), 
                                            grep(".3158", as.character(n3D2$x)))] / (max(n3D2$y)/max(funres2n32(n3D2$x)))) * 10000,
                     JohnsonSBRel = funres2n32(unique(round(n3D2$x))) * 10000,
                     simModelCum = cumsum((n3D2$y / sum(n3D2$y[c(grep(".5591", as.character(n3D2$x)), 
                                                               grep(".3158", as.character(n3D2$x)))]))[c(grep(".5591", as.character(n3D2$x)), 
                                                                                                        grep(".3158", as.character(n3D2$x)))]),
                     JohnsonSBCum = funres2an32(unique(round(n3D2$x))))

plot(nym3rd2$DDs, nym3rd2$simModelRel, type = "l", xlim = c(-558, 3060))
lines(nym3rd2$DDs, nym3rd2$JohnsonSBRel, col = "red")

plot(nym3rd2$DDs, nym3rd2$simModelCum, type = "l", xlim = c(-558, 3060))
lines(nym3rd2$DDs, nym3rd2$JohnsonSBCum, col = "red")




###########


malD2 <- data.frame(x = seq(0, longF, stepy) - (a + b), y = rowSums(sta_A4AF))

MLL2ma2 <- estimatC2(DDs = malD2$x, prs = malD2$y)
summary(MLL2ma2)



funres2ma2 <- function(DDs){
  xi = coef(MLL2ma2)[3]
  lambda = 2500000 - coef(MLL2ma2)[3]
  dJohnsonSB(DDs, gamma = coef(MLL2ma2)[1],
             delta = coef(MLL2ma2)[2],
             xi = xi,
             lambda = lambda)
}


funres2ama2 <- function(DDs){
  xi = coef(MLL2ma2)[3]
  lambda = 2500000 - coef(MLL2ma2)[3]
  pJohnsonSB(DDs, gamma = coef(MLL2ma2)[1],
             delta = coef(MLL2ma2)[2],
             xi = xi,
             lambda = lambda)
}




plot(malD2$x, malD2$y/(max(malD2$y)/max(funres2ma2(malD2$x))), type = "l", xlim = c(-558, 3060))
lines(malD2$x, funres2ma2(malD2$x), col = "red")

plot(malD2$x, cumsum(malD2$y/sum(malD2$y)), type = "l", xlim = c(-558, 3060))
lines(malD2$x, funres2ama2(malD2$x), col = "red")


males2 <- data.frame(DDs = unique(round(malD2$x)),
                    simModelRel = (malD2$y[c(grep(".5591", as.character(malD2$x)), 
                                            grep(".3158", as.character(malD2$x)))] / (max(malD2$y)/max(funres2ma2(malD2$x)))) * 10000,
                    JohnsonSBRel = funres2ma2(unique(round(malD2$x))) * 10000,
                    simModelCum = cumsum((malD2$y / sum(malD2$y[c(grep(".5591", as.character(malD2$x)), 
                                                                grep(".3158", as.character(malD2$x)))]))[c(grep(".5591", as.character(malD2$x)), 
                                                                                                          grep(".3158", as.character(malD2$x)))]),
                    JohnsonSBCum = funres2ama2(unique(round(malD2$x))))

plot(males2$DDs, males2$simModelRel, type = "l", xlim = c(-558, 3060))
lines(males2$DDs, males2$JohnsonSBRel, col = "red")

plot(males2$DDs, males2$simModelCum, type = "l", xlim = c(-558, 3060))
lines(males2$DDs, males2$JohnsonSBCum, col = "red")



###########

femD2 <- data.frame(x = seq(0, longF, stepy) - (a + b), y = rowSums(sta_A4AaF))

MLL2fem2 <- estimatC2(DDs = femD2$x, prs = femD2$y)
summary(MLL2fem2)



funres2fem2 <- function(DDs){
  xi = coef(MLL2fem2)[3]
  lambda = 2500000 - coef(MLL2fem2)[3]
  dJohnsonSB(DDs, gamma = coef(MLL2fem2)[1],
             delta = coef(MLL2fem2)[2],
             xi = xi,
             lambda = lambda)
}


funres2afem2 <- function(DDs){
  xi = coef(MLL2fem2)[3]
  lambda = 2500000 - coef(MLL2fem2)[3]
  pJohnsonSB(DDs, gamma = coef(MLL2fem2)[1],
             delta = coef(MLL2fem2)[2],
             xi = xi,
             lambda = lambda)
}




plot(femD2$x, femD2$y/(max(femD2$y)/max(funres2fem2(femD2$x))), type = "l", xlim = c(-558, 3060))
lines(femD2$x, funres2fem2(femD2$x), col = "red")

plot(femD2$x, cumsum(femD2$y/sum(femD2$y)), type = "l", xlim = c(-558, 3060))
lines(femD2$x, funres2afem2(femD2$x), col = "red")


females2 <- data.frame(DDs = unique(round(femD2$x)),
                      simModelRel = (femD2$y[c(grep(".5591", as.character(femD2$x)), 
                                              grep(".3158", as.character(femD2$x)))] / (max(femD2$y)/max(funres2fem2(femD2$x)))) * 10000,
                      JohnsonSBRel = funres2fem2(unique(round(femD2$x))) * 10000,
                      simModelCum = cumsum((femD2$y / sum(femD2$y[c(grep(".5591", as.character(femD2$x)), 
                                                                  grep(".3158", as.character(femD2$x)))]))[c(grep(".5591", as.character(femD2$x)), 
                                                                                                            grep(".3158", as.character(femD2$x)))]),
                      JohnsonSBCum = funres2afem2(unique(round(femD2$x))))

plot(females2$DDs, females2$simModelRel, type = "l", xlim = c(-558, 6060))
lines(females2$DDs, females2$JohnsonSBRel, col = "red")

plot(females2$DDs, females2$simModelCum, type = "l", xlim = c(-558, 3060))
lines(females2$DDs, females2$JohnsonSBCum, col = "red")

#########################
##########################



RelAbundanceSM2 <- data.frame(FDDs = unique(round(crwD2$x)),
                             Crawlers = crawlers1st2$simModelRel,
                             Nymph2 = nym2nd2$simModelRel,
                             Nymph3 = nym3rd2$simModelRel,
                             Males = males2$simModelRel,
                             Females = females2$simModelRel)

CumEmergenceSM2 <- data.frame(FDDs = unique(round(crwD2$x)),
                             Crawlers = crawlers1st2$simModelCum,
                             Nymph2 = nym2nd2$simModelCum,
                             Nymph3 = nym3rd2$simModelCum,
                             Males = males2$simModelCum,
                             Females = females2$simModelCum)


RelAbundanceJSB2 <- data.frame(FDDs = unique(round(crwD2$x)),
                              Crawlers = crawlers1st2$JohnsonSBRel,
                              Nymph2 = nym2nd2$JohnsonSBRel,
                              Nymph3 = nym3rd2$JohnsonSBRel,
                              Males = males2$JohnsonSBRel,
                              Females = females2$JohnsonSBRel)

CumEmergenceJSB2 <- data.frame(FDDs = unique(round(crwD2$x)),
                              Crawlers = crawlers1st2$JohnsonSBCum,
                              Nymph2 = nym2nd2$JohnsonSBCum,
                              Nymph3 = nym3rd2$JohnsonSBCum,
                              Males = males2$JohnsonSBCum,
                              Females = females2$JohnsonSBCum)



par(mar = c(5.5, 5, 3, 2) + 0.1)
plot(RelAbundanceSM2$FDDs, 
     RelAbundanceSM2$Crawlers, type = "l", ylab = "", xlab = "",
     xlim = c(-558, 3300), ylim = c(0, 10), yaxt = "n", xaxt = "n", cex.lab = 2, cex.axis = 2, lwd = 2)

axis(2, at = seq(0, 25), labels = FALSE)
axis(3, at = c(-(a + b), 0), labels = c("Egg laying peak", "Jan 1st"), cex.axis = 1.8)
axis(1, at = c(seq(-900, -180, 360), 0, seq(180, 3060, 360)), cex.axis = 2.2)

lines(RelAbundanceSM2$FDDs, RelAbundanceSM2$Nymph2, lwd = 2, lty = 2)
lines(RelAbundanceSM2$FDDs, RelAbundanceSM2$Nymph3, lwd = 2, lty = 2)
lines(RelAbundanceSM2$FDDs, RelAbundanceSM2$Males, lwd = 2)
lines(RelAbundanceSM2$FDDs, RelAbundanceSM2$Females, lwd = 2)

polygon(c(RelAbundanceSM2$FDDs, 0), c(RelAbundanceSM2$Crawlers, 0), col = crowcol, border = NA)
polygon(c(RelAbundanceSM2$FDDs, 0), c(RelAbundanceSM2$Nymph2, 0), col = nymcol, border = NA)
polygon(c(RelAbundanceSM2$FDDs, 0), c(RelAbundanceSM2$Nymph3, 0), col = nymcol, border = NA)

polygon(c(RelAbundanceSM2$FDDs, 0), c(RelAbundanceSM2$Males, 0), col = malecol, border = NA)
polygon(c(RelAbundanceSM2$FDDs, 0), c(RelAbundanceSM2$Females, 0), col = femcol, border = NA)

abline(v = 0, lwd = 2, lty = 3)
abline(v = -(a + b), lwd = 2, lty = 3)

title(ylab = "Relative abundance", cex.lab = 3, line = 3)
title(xlab = "Degree days (F)", cex.lab = 3, line  = 4)



#############




par(mar = c(5.5, 7, 3, 2) + 0.1)
plot(CumEmergenceSM2$FDDs, 
     CumEmergenceSM2$Crawlers, type = "l", ylab = "", xlab = "",
     xlim = c(-558, 3300), ylim = c(0, 1), yaxt = "n", xaxt = "n", cex.lab = 2, cex.axis = 2, lwd = 2, col = crowcol)

axis(2, at = seq(0, 1, 0.2), cex.axis = 2.2, las = 2)
axis(3, at = c(-(a + b), 0), labels = c("Egg laying peak", "Jan 1st"), cex.axis = 1.8)
axis(1, at = c(seq(-900, -180, 360), 0, seq(180, 3060, 360)), cex.axis = 2.2)

lines(CumEmergenceSM2$FDDs, CumEmergenceSM2$Nymph2, lwd = 2, lty = 2, col = nymcol)
lines(CumEmergenceSM2$FDDs, CumEmergenceSM2$Nymph3, lwd = 2, lty = 2, col = nymcol)
lines(CumEmergenceSM2$FDDs, CumEmergenceSM2$Males, lwd = 2, col = malecol)
lines(CumEmergenceSM2$FDDs, CumEmergenceSM2$Females, lwd = 2, col = femcol)


abline(v = 0, lwd = 2, lty = 3)
abline(v = -(a + b), lwd = 2, lty = 3)

title(ylab = "Cumulative emergence", cex.lab = 3, line = 4.5)
title(xlab = "Degree days (F)", cex.lab = 3, line  = 4)



##############
##########


par(mar = c(5.5, 5, 3, 2) + 0.1)
plot(RelAbundanceJSB2$FDDs, 
     RelAbundanceJSB2$Crawlers, type = "l", ylab = "", xlab = "",
     xlim = c(-558, 3300), ylim = c(0, 10), yaxt = "n", xaxt = "n", cex.lab = 2, cex.axis = 2, lwd = 2)

axis(2, at = seq(0, 25), labels = FALSE)
axis(3, at = c(-(a + b), 0), labels = c("Egg laying peak", "Jan 1st"), cex.axis = 1.8)
axis(1, at = c(seq(-900, -180, 360), 0, seq(180, 3060, 360)), cex.axis = 2.2)

lines(RelAbundanceJSB2$FDDs, RelAbundanceJSB2$Nymph2, lwd = 2, lty = 2)
lines(RelAbundanceJSB2$FDDs, RelAbundanceJSB2$Nymph3, lwd = 2, lty = 2)
lines(RelAbundanceJSB2$FDDs, RelAbundanceJSB2$Males, lwd = 2)
lines(RelAbundanceJSB2$FDDs, RelAbundanceJSB2$Females, lwd = 2)

polygon(c(RelAbundanceJSB2$FDDs, 0), c(RelAbundanceJSB2$Crawlers, 0), col = crowcol, border = NA)
polygon(c(RelAbundanceJSB2$FDDs, 0), c(RelAbundanceJSB2$Nymph2, 0), col = nymcol, border = NA)
polygon(c(RelAbundanceJSB2$FDDs, 0), c(RelAbundanceJSB2$Nymph3, 0), col = nymcol, border = NA)

polygon(c(RelAbundanceJSB2$FDDs, 0), c(RelAbundanceJSB2$Males, 0), col = malecol, border = NA)
polygon(c(RelAbundanceJSB2$FDDs, 0), c(RelAbundanceJSB2$Females, 0), col = femcol, border = NA)

abline(v = 0, lwd = 2, lty = 3)
abline(v = -(a + b), lwd = 2, lty = 3)

title(ylab = "Relative abundance", cex.lab = 3, line = 3)
title(xlab = "Degree days (F)", cex.lab = 3, line  = 4)



#############




par(mar = c(5.5, 7, 3, 2) + 0.1)
plot(CumEmergenceJSB2$FDDs, 
     CumEmergenceJSB2$Crawlers, type = "l", ylab = "", xlab = "",
     xlim = c(-558, 3300), ylim = c(0, 1), yaxt = "n", xaxt = "n", cex.lab = 2, cex.axis = 2, lwd = 2, col = crowcol)

axis(2, at = seq(0, 1, 0.2), cex.axis = 2.2, las = 2)
axis(3, at = c(-(a + b), 0), labels = c("Egg laying peak", "Jan 1st"), cex.axis = 1.8)
axis(1, at = c(seq(-900, -180, 360), 0, seq(180, 3060, 360)), cex.axis = 2.2)

lines(CumEmergenceJSB2$FDDs, CumEmergenceJSB2$Nymph2, lwd = 2, lty = 2, col = nymcol)
lines(CumEmergenceJSB2$FDDs, CumEmergenceJSB2$Nymph3, lwd = 2, lty = 2, col = nymcol)
lines(CumEmergenceJSB2$FDDs, CumEmergenceJSB2$Males, lwd = 2, col = malecol)
lines(CumEmergenceJSB2$FDDs, CumEmergenceJSB2$Females, lwd = 2, col = femcol)


abline(v = 0, lwd = 2, lty = 3)
abline(v = -(a + b), lwd = 2, lty = 3)

title(ylab = "Cumulative emergence", cex.lab = 3, line = 4.5)
title(xlab = "Degree days (F)", cex.lab = 3, line  = 4)




save(MLL2, MLL22, MLL2n2, MLL2n22, MLL2n3, MLL2n32, MLL2ma, MLL2ma2, MLL2fem, MLL2fem2, file = "Ph_models.RData")
save(RelAbundanceSM, RelAbundanceSMV2, RelAbundanceJSB, RelAbundanceJSBV2, RelAbundanceSM2, RelAbundanceJSB2, 
     CumEmergenceSM, CumEmergenceSMV2, CumEmergenceJSB, CumEmergenceJSBV2, CumEmergenceSM2, CumEmergenceJSB2, file = "data_phen.RData")
