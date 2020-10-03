library(readr)
# DATA
x  <- read_csv("~/pwt91.csv")  # PWT 9.1
x <- x[ which(x$year > 1953 ), ] # Drops data < 1954
x$c <- x$csh_c # Consumption share
x$c[x$c<0 | x$c>1] <- NA # Out of bound consumption share -> NA
x$Y <- x$rgdpo # Output-method real GDP
x$Ye <- x$rgdpe # Expenditure-method real GDP (Kaleckian assumption)
x$om <- x$labsh # Wage share 
x$l <- x$avh # Per-worker hours worked (lots of missing data)
x$Ks <- x$rkna # Real capital services (annual flow)
x$W <- x$om * x$Y # Real wages
x$C <- x$c * x$Ye # Consumption spending
x$K <- x$rnna # Real capital stock
x$N <- x$pop # Population
x$E <- x$emp # Engaged workers (employment)
x$L <- x$avh * x$E # Labor time (estimated of annual hours worked)
x <- x[ , c(1, 4, 53:64)] # Selects variables
str(x)
summary(x)
x0 <- x

# Weights
N0 <- rep(tapply(x0$N, x$year, sum, na.rm=TRUE), 182) # Aggregates global population (year series)
Y0 <- rep(tapply(x0$Y, x$year, sum, na.rm=TRUE), 182) # Aggregates global real GDP (year series)
x0$theta <- x0$Y/Y0 # Share of country's real GDP in global real GDP (year series)
x0$delta <- x0$N/N0 # Share of country's population in global population (year series)
summary(x0$theta)
summary(x0$delta)
# Test of weights
x0[ which(x0$theta>.346), ] # USA
x0[ which(x0$delta>.29), ] # CHN

# STRUCTURAL PARAMETERS FOR EACH COUNTRY/YEAR
x$yN <- x$Y/x$N # Per capita productivity
x$yE <- x$Y/x$E # Per worker productivity
x$yL <- x$Y/x$L # Per hour productivity
x$wN <- x$om*x$Y/x$N # Wage per capita
x$wE <- (x$om*x$Y)/x$E # Wage per worker
x$wL <- (x$om*x$Y)/x$L # Wage per hour
x$s <- (1 - x$om)/x$om # Exploitation rate
x$ss <- (1 - x$c)/x$c # Exploitation rate (Kalecki)
x$h <- x$Ks/x$W # Capital composition (flow-flow)
x$rho <- x$Y/x$K # Capital productivity (flow-stock)
x$r <- (1 - x$om)*x$Y/x$K # Profit rate
x$ka <- (1 - x$c)*x$Y/x$K # Accumulation rate (Kalecki profit rate)
str(x)

# GLOBAL PRIMARY VARIABLES (YEAR SERIES)
N <- tapply(x$N, x$year, sum, na.rm=TRUE) # Aggregates population (year series)
E <- tapply(x$E, x$year, sum, na.rm=TRUE) # Aggregates employment (year series)
L <- tapply(x$L, x$year, sum, na.rm=TRUE) # Aggregates labor hours (year series)
Y <- tapply(x$Y, x$year, sum, na.rm=TRUE) # Aggregates real GDP (year series)
Ye <- tapply(x$Ye, x$year, sum, na.rm=TRUE) # Aggregates real GDP exp. (year series)
W <- tapply(x$W, x$year, sum, na.rm=TRUE) # Aggregates wages (year series)
C <- tapply(x$C, x$year, sum, na.rm=TRUE) # Aggregates consumption spend. (year series)
K <- tapply(x$K, x$year, sum, na.rm=TRUE) # Aggregates capital stock (year series)
Ks <- tapply(x$Ks, x$year, sum, na.rm=TRUE) # Aggregates capital services flow (year series)

# GLOBAL STRUCTURES (YEAR SERIES)
yN <- Y/N # Global per-capita productivity
yE <- Y/E # Global per-worker productivity
yL <- Y/L # Global per-hour productivity
wN <- W/N # Global per-capita wage
wE <- W/E # Global per-worker wage
wL <- W/L # Global per-hour wage
s <- (Y-W)/W # Global exploitation rate
ss <- (Y - C)/C # Global exploitation rate (Kalecki)
h <- Ks/W # Global capital composition (flow-flow)
rho <- Y/K # Global capital productivity (flow-stock)
r <- (Y-W)/K # Global profit rate
ka <- (Y-C)/K # Global accumulation rate
year <- c(1954:2017)
g <- data.frame(year, N, E, L, Y, Ye, W, C, K, yN, yE, yL, wN, wE, wL, 
                s, ss, h, rho, r, ka) # Global data set: levels and structural proportions
str(g)

# GLOBAL STRUCTURAL PROPORTIONS (YEAR SERIES) PLOTS
# Productivity
byN <- coef(lm(log(g$yN) ~ g$year))[2]
bwN <- coef(lm(log(g$wN) ~ g$year))[2]
plot(g$year, g$yN, type="l", ylim=c(2000, 15000), 
     main="Global per-capita productivity and wage", 
     xlab="Year", ylab="2011 USD")
lines(g$year, g$wN, lty=2)
mtext(bquote(hat(y[N]) ==.(round(byN, 4))~","~ hat(w[N]) ==.(round(bwN, 4))))
legend("topleft", bty="n", lty=c(1, 2), c(expression(paste(y[N]), paste(w[N]))))

byE <- coef(lm(log(g$yE) ~ g$year))[2]
bwE <- coef(lm(log(g$wE) ~ g$year))[2]
plot(g$year, g$yE, type="l", ylim=c(5000, 35000), 
     main="Global per-worker productivity and wage", 
     xlab="Year", ylab="2011 USD")
lines(g$year, g$wE, lty=2)
mtext(bquote(hat(y[E]) ==.(round(byE, 4))~","~ hat(w[E]) ==.(round(bwE, 4))))
legend("topleft", bty="n", lty=c(1, 2), c(expression(paste(y[E]), paste(w[E]))))

byL <- coef(lm(log(g$yL[-c(1:16)]) ~ g$year[-c(1:16)]))[2]
bwL <- coef(lm(log(g$wL[-c(1:16)]) ~ g$year[-c(1:16)]))[2]
plot(g$year[-c(1:16)], g$yL[-c(1:16)], type="l", ylim=c(0, 20), 
     main="Global per-hour productivity and wage", 
     xlab="Year", ylab="2011 USD")
lines(g$year[-c(1:16)], g$wL[-c(1:16)], lty=2)
mtext(bquote(hat(y[L])  ==.(round(byL, 4))~","~ hat(w[L])  ==.(round(bwL, 4))))
legend("topleft", bty="n", lty=c(1, 2), c(expression(paste(y[L]), paste(w[L]))))

# Exploitation
bs <- coef(lm(log(g$s) ~ g$year))[2]
bss <- coef(lm(log(g$ss) ~ g$year))[2]
plot(g$year, g$s, type="l", ylim=c(0.4, 1),
     main="Global exploitation rates", 
     xlab="Year", ylab=" ")
lines(g$year, g$ss, lty=2)
mtext(bquote(hat(sigma)  ==.(round(bs, 4))~","~ hat(sigma[e]) ==.(round(bss, 4))))
legend("topleft", bty="n", lty=c(1, 2), c(expression(paste(sigma), paste(sigma[e] ))))

# Capital composition
bh <- coef(lm(log(g$h) ~ g$year))[2]
plot(g$year, g$h, type="l", 
     main="Global flow-flow capital composition", 
     xlab="Year", ylab="h")
mtext(bquote(hat(h) ==.(round(bh, 4))))

# Output-capital ratio
brho <- coef(lm(log(g$rho) ~ g$year))[2]
plot(g$year, g$rho, type="l", 
     main="Global output-capital ratio", 
     xlab="Year", ylab=expression(paste(rho)))
mtext(bquote(hat(rho) ==.(round(brho, 4))))

# Profitability
br <- coef(lm(log(g$r) ~ g$year))[2]
bka <- coef(lm(log(g$ka) ~ g$year))[2]
plot(g$year, g$r, type="l", ylim=c(0.08, .13),
     main="Global profit rates", 
     xlab="Year", ylab=" ")
lines(g$year, g$ka, lty=2)
mtext(bquote(hat(r)  ==.(round(br, 4))~","~ hat(kappa) ==.(round(bka, 4))))
legend("topleft", bty="n", lty=c(1, 2), c(expression(paste(r), paste(kappa))))

# INTERNATIONAL
yN75<-log(na.omit(subset(x$yN, x$year==1975)))
yN95<-log(na.omit(subset(x$yN, x$year==1995)))
yN15<-log(na.omit(subset(x$yN, x$year==2015)))

yE75<-log(na.omit(subset(x$yE, x$year==1975)))
yE95<-log(na.omit(subset(x$yE, x$year==1995)))
yE15<-log(na.omit(subset(x$yE, x$year==2015)))

yL75<-log(na.omit(subset(x$yL, x$year==1975)))
yL95<-log(na.omit(subset(x$yL, x$year==1995)))
yL15<-log(na.omit(subset(x$yL, x$year==2015)))

wN75<-log(na.omit(subset(x$wN, x$year==1975)))
wN95<-log(na.omit(subset(x$wN, x$year==1995)))
wN15<-log(na.omit(subset(x$wN, x$year==2015)))

wE75<-log(na.omit(subset(x$wE, x$year==1975)))
wE95<-log(na.omit(subset(x$wE, x$year==1995)))
wE15<-log(na.omit(subset(x$wE, x$year==2015)))

wL75<-log(na.omit(subset(x$wL, x$year==1975)))
wL95<-log(na.omit(subset(x$wL, x$year==1995)))
wL15<-log(na.omit(subset(x$wL, x$year==2015)))

s75<-log(na.omit(subset(x$s, x$year==1975)))
s95<-log(na.omit(subset(x$s, x$year==1995)))
s15<-log(na.omit(subset(x$s, x$year==2015)))

ss75<-log(na.omit(subset(x$ss, x$year==1975)))
ss95<-log(na.omit(subset(x$ss, x$year==1995)))
ss15<-log(na.omit(subset(x$ss, x$year==2015)))

h75<-log(na.omit(subset(x$h, x$year==1975)))
h95<-log(na.omit(subset(x$h, x$year==1995)))
h15<-log(na.omit(subset(x$h, x$year==2015)))

rho75<-log(na.omit(subset(x$rho, x$year==1975)))
rho95<-log(na.omit(subset(x$rho, x$year==1995)))
rho15<-log(na.omit(subset(x$rho, x$year==2015)))

r75<-log(na.omit(subset(x$r, x$year==1975)))
r95<-log(na.omit(subset(x$r, x$year==1995)))
r15<-log(na.omit(subset(x$r, x$year==2015)))

ka75<-log(na.omit(subset(x$ka, x$year==1975)))
ka95<-log(na.omit(subset(x$ka, x$year==1995)))
ka15<-log(na.omit(subset(x$ka, x$year==2015)))

# Multi plot function:
# The input of the function MUST be a numeric list
plot.multi.dens <- function(s)
{
        junk.x = NULL
        junk.y = NULL
        for(i in 1:length(s))
        {
                junk.x = c(junk.x, density(s[[i]])$x)
                junk.y = c(junk.y, density(s[[i]])$y)
        }
        xr <- range(junk.x)
        yr <- range(junk.y)
        plot(density(s[[1]]), xlim = xr, ylim = yr, main = "", xlab = "")
        for(i in 1:length(s))
        {
                lines(density(s[[i]]), xlim = xr, ylim = yr, lty=i)
        }
}

library(Hmisc)
# Density plots:
plot.multi.dens(list(yN75,yN95,yN15))
title(main = expression(paste(log(y[N]), ": Per-capita productivity")))
legend("topleft", bty="n", legend=c("1975","1995","2015"), lty =(1:3))

plot.multi.dens(list(yE75,yE95,yE15))
title(main = expression(paste(log(y[E]), ": Per-worker productivity")))
legend("topleft", bty="n", legend=c("1975","1995","2015"), lty =(1:3))

plot.multi.dens(list(yL75,yL95,yL15))
title(main = expression(paste(log(y[L]), ": Per-hour productivity")))
legend("topleft", bty="n", legend=c("1975","1995","2015"), lty =(1:3))

plot.multi.dens(list(wN75,wN95,wN15))
title(main = expression(paste(log(w[N]), ": Per-capita wage")))
legend("topleft", bty="n", legend=c("1975","1995","2015"), lty =(1:3))

plot.multi.dens(list(wE75,wE95,wE15))
title(main = expression(paste(log(w[E]), ": Per-worker wage")))
legend("topleft", bty="n", legend=c("1975","1995","2015"), lty =(1:3))

plot.multi.dens(list(wL75,wL95,wL15))
title(main = expression(paste(log(w[L]), ": Per-hour wage")))
legend("topleft", bty="n", legend=c("1975","1995","2015"), lty =(1:3))

plot.multi.dens(list(s75,s95,s15))
title(main = expression(paste(log(sigma), ": Exploitation rate")))
legend("topleft", bty="n", legend=c("1975","1995","2015"), lty =(1:3))

plot.multi.dens(list(ss75,ss95,ss15))
title(main = expression(paste(log(sigma[e]), ": Expenditure-exploitation rate")))
legend("topleft", bty="n", legend=c("1975","1995","2015"), lty =(1:3))

plot.multi.dens(list(h75,h95,h15))
title(main = expression(paste(log(h), ": Capital composition")))
legend("topleft", bty="n", legend=c("1975","1995","2015"), lty =(1:3))

plot.multi.dens(list(rho75,rho95,rho15))
title(main = expression(paste(log(rho), ": Output-capital ratio")))
legend("topleft", bty="n", legend=c("1975","1995","2015"), lty =(1:3))

plot.multi.dens(list(r75,r95,r15))
title(main = expression(paste(log(r), ": Profit rate")))
legend("topleft", bty="n", legend=c("1975","1995","2015"), lty =(1:3))

plot.multi.dens(list(ka75,ka95,ka15))
title(main = expression(paste(log(kappa), ": Accumulation rate")))
legend("topleft", bty="n", legend=c("1975","1995","2015"), lty =(1:3))

# UNWEIGHTED/COUNTRY INTERNATIONAL (YEAR SERIES) PLOTS
library(plotrix)
# Productivity
iyN <- tapply(x$yN, x$year, mean, na.rm=TRUE)
summary(iyN)
biyN <- coef(lm(log(iyN[c(20:64)]) ~ year[c(20:64)]))[2]
seiyN <- tapply(x$yN, x$year, std.error, na.rm=TRUE)
miyN <- tapply(x$yN, x$year, median, na.rm=TRUE)
ciH <- iyN + 2*seiyN
ciL <- iyN - 2*seiyN
plot(year[c(20:64)], iyN[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) and median of per-capita output ", y[N])), 
     xlab="Year", ylab="2011 USD/capita", ylim=c(4000, 25000), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iyN[c(20:64)], lty=1)
lines(year[c(20:64)], miyN[c(20:64)], lty=2)
legend("top", bty="n", 
       legend=c(expression(paste(bar(y[N]))), expression(paste(tilde(y[N])))), 
       lty =c(1,2))
mtext(bquote(bar(y[N])^T  == .(round(mean(iyN[c(20:64)]), 1))~ "
"~se(bar(y[N]))^T ==.(round(mean(seiyN[c(20:64)]), 1))~" 
"~tilde(y[N])^T  == .(round(mean(miyN[c(20:64)]), 1))~" 
"~hat(bar(y[N]))  ==.(round(biyN, 4))))

iyE <- tapply(x$yE, x$year, mean, na.rm=TRUE)
summary(iyE)
biyE <- coef(lm(log(iyE[c(20:64)]) ~ year[c(20:64)]))[2]
seiyE <- tapply(x$yE, x$year, std.error, na.rm=TRUE)
miyE <- tapply(x$yE, x$year, median, na.rm=TRUE)
ciH <- iyE + 2*seiyE
ciL <- iyE - 2*seiyE
plot(year[c(20:64)], iyE[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) and median of per-worker output ", y[E])), 
     xlab="Year", ylab="2011 USD/worker", ylim=c(0, 55000), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iyE[c(20:64)], lty=1)
lines(year[c(20:64)], miyE[c(20:64)], lty=2)
legend("top", bty="n", 
       legend=c(expression(paste(bar(y[E]))), expression(paste(tilde(y[E])))), 
       lty =c(1,2))
mtext(bquote(bar(y[E])^T  == .(round(mean(iyE[c(20:64)]), 1))~ "
"~se(bar(y[E]))^T ==.(round(mean(seiyE[c(20:64)]), 1))~" 
"~tilde(y[E])^T  == .(round(mean(miyE[c(20:64)]), 1))~" 
"~hat(bar(y[E]))  ==.(round(biyE, 4))))

iyL <- tapply(x$yL, x$year, mean, na.rm=TRUE)
summary(iyL)
biyL <- coef(lm(log(iyL[c(20:64)]) ~ year[c(20:64)]))[2]
seiyL <- tapply(x$yL, x$year, std.error, na.rm=TRUE)
miyL <- tapply(x$yL, x$year, median, na.rm=TRUE)
ciH <- iyL + 2*seiyL
ciL <- iyL - 2*seiyL
plot(year[c(20:64)], iyL[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) and median of per-hour output ", y[L])), 
     xlab="Year", ylab="2011 USD/hour", ylim=c(10, 40), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iyL[c(20:64)], lty=1)
lines(year[c(20:64)], miyL[c(20:64)], lty=2)
legend("top", bty="n", 
       legend=c(expression(paste(bar(y[L]))), expression(paste(tilde(y[L])))), 
       lty =c(1,2))
mtext(bquote(bar(y[L])^T  == .(round(mean(iyL[c(20:64)]), 1))~ "
"~se(bar(y[L]))^T ==.(round(mean(seiyL[c(20:64)]), 1))~" 
"~tilde(y[L])^T  == .(round(mean(miyL[c(20:64)]), 1))~" 
"~hat(bar(y[L]))  ==.(round(biyL, 4))))

# Wage rates
iwN <- tapply(x$wN, x$year, mean, na.rm=TRUE)
summary(iwN)
biwN <- coef(lm(log(iwN[c(20:64)]) ~ year[c(20:64)]))[2]
seiwN <- tapply(x$wN, x$year, std.error, na.rm=TRUE)
miwN <- tapply(x$wN, x$year, median, na.rm=TRUE)
ciH <- iwN + 2*seiwN
ciL <- iwN - 2*seiwN
plot(year[c(20:64)], iwN[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) and median of per-capita wage ", w[N])), 
     xlab="Year", ylab="2011 USD/capita", ylim=c(1500, 17500), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iwN[c(20:64)], lty=1)
lines(year[c(20:64)], miwN[c(20:64)], lty=2)
# lines(year[c(20:64)], iwN[c(20:64)] + 2*seiwN[c(20:64)], lty=2)
# lines(year[c(20:64)], iwN[c(20:64)] - 2*seiwN[c(20:64)], lty=2)
legend("top", bty="n", 
       legend=c(expression(paste(bar(w[N]))), expression(paste(tilde(w[N])))), 
       lty =c(1,2))
mtext(bquote(bar(w[N])^T  == .(round(mean(iwN[c(20:64)]), 1))~ "
"~se(bar(w[N]))^T ==.(round(mean(seiwN[c(20:64)]), 1))~" 
"~tilde(w[N])^T  == .(round(mean(miwN[c(20:64)]), 1))~" 
"~hat(bar(w[N]))  ==.(round(biwN, 4))))

iwE <- tapply(x$wE, x$year, mean, na.rm=TRUE)
summary(iwE)
biwE <- coef(lm(log(iwE[c(20:64)]) ~ year[c(20:64)]))[2]
seiwE <- tapply(x$wE, x$year, std.error, na.rm=TRUE)
miwE <- tapply(x$wE, x$year, median, na.rm=TRUE)
ciH <- iwE + 2*seiwE
ciL <- iwE - 2*seiwE
plot(year[c(20:64)], iwE[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) and median of per-worker wage ", w[E])), 
     xlab="Year", ylab="2011 USD/worker", ylim=c(5000, 34000), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iwE[c(20:64)], lty=1)
lines(year[c(20:64)], miwE[c(20:64)], lty=2)
legend("top", bty="n", 
       legend=c(expression(paste(bar(w[E]))), expression(paste(tilde(w[E])))), 
       lty =c(1,2))
mtext(bquote(bar(w[E])^T  == .(round(mean(iwE[c(20:64)]), 1))~ "
"~se(bar(w[E]))^T ==.(round(mean(seiwE[c(20:64)]), 1))~" 
"~tilde(w[E])^T  == .(round(mean(miwE[c(20:64)]), 1))~" 
"~hat(bar(w[E]))  ==.(round(biwE, 4))))

iwL <- tapply(x$wL, x$year, mean, na.rm=TRUE)
summary(iwL)
biwL <- coef(lm(log(iwL[c(20:64)]) ~ year[c(20:64)]))[2]
seiwL <- tapply(x$wL, x$year, std.error, na.rm=TRUE)
miwL <- tapply(x$wL, x$year, median, na.rm=TRUE)
ciH <- iwL + 2*seiwL
ciL <- iwL - 2*seiwL
plot(year[c(20:64)], iwL[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) and median of per-hour wage ", w[L])), 
     xlab="Year", ylab="2011 USD/hour", ylim=c(4, 25), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iwL[c(20:64)], lty=1)
lines(year[c(20:64)], miwL[c(20:64)], lty=2)
legend("top", bty="n", 
       legend=c(expression(paste(bar(w[L]))), expression(paste(tilde(w[L])))), 
       lty =c(1,2))
mtext(bquote(bar(w[L])^T  == .(round(mean(iwL[c(20:64)]), 2))~ "
"~se(bar(w[L]))^T ==.(round(mean(seiwL[c(20:64)]), 2))~" 
"~tilde(w[L])^T  == .(round(mean(miwL[c(20:64)]), 2))~" 
"~hat(bar(w[L]))  ==.(round(biwL, 4))))

# Exploitation
is <- tapply(x$s, x$year, mean, na.rm=TRUE)
summary(is)
bis <- coef(lm(log(is[c(20:64)]) ~ year[c(20:64)]))[2]
seis <- tapply(x$s, x$year, std.error, na.rm=TRUE)
mis <- tapply(x$s, x$year, median, na.rm=TRUE)
ciH <- is + 2*seis
ciL <- is - 2*seis
plot(year[c(20:64)], is[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) and median of exploitation rate ", sigma)), 
     xlab="Year", ylab=" ", ylim=c(.6, 1.4), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], is[c(20:64)], lty=1)
lines(year[c(20:64)], mis[c(20:64)], lty=2)
legend("top", bty="n", 
       legend=c(expression(paste(bar(sigma))), expression(paste(tilde(sigma)))), 
       lty =c(1,2))
mtext(bquote(bar(sigma)^T  == .(round(mean(is[c(20:64)]), 3))~ "
"~se(bar(sigma))^T ==.(round(mean(seis[c(20:64)]), 3))~" 
"~tilde(sigma)^T  == .(round(mean(mis[c(20:64)]), 3))~" 
"~hat(bar(sigma))  ==.(round(bis, 4))))

iss <- tapply(x$ss, x$year, mean, na.rm=TRUE)
summary(iss)
biss <- coef(lm(log(iss[c(20:64)]) ~ year[c(20:64)]))[2]
seiss <- tapply(x$ss, x$year, std.error, na.rm=TRUE)
miss <- tapply(x$ss, x$year, median, na.rm=TRUE)
ciH <- iss + 2*seiss
ciL <- iss - 2*seiss
plot(year[c(20:64)], iss[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) and median of exploitation rate[e] ", sigma[e])), 
     xlab="Year", ylab=" ", ylim=c(.3, 1.5), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iss[c(20:64)], lty=1)
lines(year[c(20:64)], miss[c(20:64)], lty=2)
legend("top", bty="n", 
       legend=c(expression(paste(bar(sigma[e]))), expression(paste(tilde(sigma[e])))), 
       lty =c(1,2))
mtext(bquote(bar(sigma[e])^T  == .(round(mean(iss[c(20:64)]), 3))~ "
"~se(bar(sigma[e]))^T ==.(round(mean(seiss[c(20:64)]), 3))~" 
"~tilde(sigma[e])^T  == .(round(mean(miss[c(20:64)]), 3))~" 
"~hat(bar(sigma[e]))  ==.(round(biss, 4))))

# Capital composition
ih <- tapply(x$h, x$year, mean, na.rm=TRUE)
summary(ih)
bih <- coef(lm(log(ih[c(20:64)]) ~ year[c(20:64)]))[2]
seih <- tapply(x$h, x$year, std.error, na.rm=TRUE)
mih <- tapply(x$h, x$year, median, na.rm=TRUE)
ciH <- ih + 2*seih
ciL <- ih - 2*seih
plot(year[c(20:64)], ih[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) and median of capital composition ", h)), 
     xlab="Year", ylab=" ", ylim=c(-0.00001, 2.4e-04), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], ih[c(20:64)], lty=1)
lines(year[c(20:64)], mih[c(20:64)], lty=2)
legend("top", bty="n", 
       legend=c(expression(paste(bar(h))), expression(paste(tilde(h)))), 
       lty =c(1,2))
mtext(bquote(bar(h)^T  == .(round(mean(ih[c(20:64)]), 6))~ "
"~se(bar(h))^T ==.(round(mean(seih[c(20:64)]), 6))~" 
"~tilde(h)^T  == .(round(mean(mih[c(20:64)]), 6))~" 
"~hat(bar(h))  ==.(round(bih, 6))))

# Output-capital ratio
irho <- tapply(x$rho, x$year, mean, na.rm=TRUE)
summary(irho)
birho <- coef(lm(log(irho[c(20:64)]) ~ year[c(20:64)]))[2]
seirho <- tapply(x$rho, x$year, std.error, na.rm=TRUE)
mirho <- tapply(x$rho, x$year, median, na.rm=TRUE)
ciH <- irho + 2*seirho
ciL <- irho - 2*seirho
plot(year[c(20:64)], irho[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) and median of output-capital ratio ", rho)), 
     xlab="Year", ylab=" ", ylim=c(.2, .7), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], irho[c(20:64)], lty=1)
lines(year[c(20:64)], mirho[c(20:64)], lty=2)
legend("top", bty="n", 
       legend=c(expression(paste(bar(rho))), expression(paste(tilde(rho)))), 
       lty =c(1,2))
mtext(bquote(bar(rho)^T  == .(round(mean(irho[c(20:64)]), 4))~ "
"~se(bar(rho))^T ==.(round(mean(seirho[c(20:64)]), 4))~" 
"~tilde(rho)^T  == .(round(mean(mirho[c(20:64)]), 4))~" 
"~hat(bar(rho))  ==.(round(birho, 5))))

# Profitability
ir <- tapply(x$r, x$year, mean, na.rm=TRUE)
summary(ir)
bir <- coef(lm(log(ir[c(20:64)]) ~ year[c(20:64)]))[2]
seir <- tapply(x$r, x$year, std.error, na.rm=TRUE)
mir <- tapply(x$r, x$year, median, na.rm=TRUE)
ciH <- ir + 2*seir
ciL <- ir - 2*seir
plot(year[c(20:64)], ir[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) and median of profit rate ", r)), 
     xlab="Year", ylab=" ", ylim=c(.07, .26), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], ir[c(20:64)], lty=1)
lines(year[c(20:64)], mir[c(20:64)], lty=2)
legend("top", bty="n", 
       legend=c(expression(paste(bar(r))), expression(paste(tilde(r)))), 
       lty =c(1,2))
mtext(bquote(bar(r)^T  == .(round(mean(ir[c(20:64)]), 4))~ "
"~se(bar(r))^T ==.(round(mean(seir[c(20:64)]), 4))~" 
"~tilde(r)^T  == .(round(mean(mir[c(20:64)]), 4))~" 
"~hat(bar(r))  ==.(round(bir, 5))))

ika <- tapply(x$ka, x$year, mean, na.rm=TRUE)
summary(ika)
bika <- coef(lm(log(ika[c(20:64)]) ~ year[c(20:64)]))[2]
seika <- tapply(x$ka, x$year, std.error, na.rm=TRUE)
mika <- tapply(x$ka, x$year, median, na.rm=TRUE)
ciH <- ika + 2*seika
ciL <- ika - 2*seika
plot(year[c(20:64)], ika[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) and median of accumulation rate ", kappa)), 
     xlab="Year", ylab=" ", ylim=c(0, .3), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], ika[c(20:64)], lty=1)
lines(year[c(20:64)], mika[c(20:64)], lty=2)
legend("top", bty="n", 
       legend=c(expression(paste(bar(kappa))), expression(paste(tilde(kappa)))), 
       lty =c(1,2))
mtext(bquote(bar(kappa)^T  == .(round(mean(ika[c(20:64)]), 4))~ "
"~se(bar(kappa))^T ==.(round(mean(seika[c(20:64)]), 4))~" 
"~tilde(kappa)^T  == .(round(mean(mika[c(20:64)]), 4))~" 
"~hat(bar(ka))  ==.(round(bika, 5))))

# Comparisons: mean (+/-CI) vs median
iyN <- tapply(x$yN, x$year, mean, na.rm=TRUE)
miyN <- tapply(x$yN, x$year, median, na.rm=TRUE)
seiyN <- tapply(x$yN, x$year, std.error, na.rm=TRUE)
ciH <- iyN + 2*seiyN
ciL <- iyN - 2*seiyN
biyN <- coef(lm(log(iyN[c(20:64)]) ~ year[c(20:64)]))[2]
plot(year[c(20:64)], iyN[c(20:64)], type="l", lty=0, 
     main=expression(paste("Means (95% CI) and medians of ", y[N], " and ", w[N])), 
     xlab="Year", ylab="2011 USD", ylim=c(900, 30000))
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(1, 0, 0, alpha=.25), border=NA)
lines(year[c(20:64)], iyN[c(20:64)], lty=1, col="red")
lines(year[c(20:64)], miyN[c(20:64)], lty=2, col="red")
iwN <- tapply(x$wN, x$year, mean, na.rm=TRUE)
miwN <- tapply(x$wN, x$year, median, na.rm=TRUE)
seiwN <- tapply(x$wN, x$year, std.error, na.rm=TRUE)
ciH <- iwN + 2*seiwN
ciL <- iwN - 2*seiwN
biwN <- coef(lm(log(iwN[c(20:64)]) ~ year[c(20:64)]))[2]
lines(year[c(20:64)], iwN[c(20:64)], lty=0)
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(0, 0, 1, .25), border=NA)
lines(year[c(20:64)], iwN[c(20:64)], lty=1, col="blue")
lines(year[c(20:64)], miwN[c(20:64)], lty=2, col="blue")
legend("top", bty="n", legend=c(expression(paste(bar(y[N]))), 
                                expression(paste(bar(w[N]))), 
                                expression(paste(tilde(y[N]))), 
                                expression(paste(tilde(w[N])))), 
       lty =c(1,1, 2, 2), lwd = c(1, 1, 2, 2), col=c("red", "blue", "red", "blue"))
mtext(bquote(hat(bar(y[N]))  ==.(round(biyN, 4))~","~ hat(bar(w[N])) ==.(round(biwN, 4))))

iyE <- tapply(x$yE, x$year, mean, na.rm=TRUE)
miyE <- tapply(x$yE, x$year, median, na.rm=TRUE)
seiyE <- tapply(x$yE, x$year, std.error, na.rm=TRUE)
ciH <- iyE + 2*seiyE
ciL <- iyE - 2*seiyE
biyE <- coef(lm(log(iyE[c(20:64)]) ~ year[c(20:64)]))[2]
plot(year[c(20:64)], iyE[c(20:64)], type="l", lty=0, 
     main=expression(paste("Means (95% CI) and medians of ", y[E], 
                           " and ", w[E])), 
     xlab="Year", ylab="2011 USD", ylim=c(5000, 60000))
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(1, 0, 0, alpha=.25), border=NA)
lines(year[c(20:64)], iyE[c(20:64)], lty=1, col="red")
lines(year[c(20:64)], miyE[c(20:64)], lty=2, col="red")
iwE <- tapply(x$wE, x$year, mean, na.rm=TRUE)
miwE <- tapply(x$wE, x$year, median, na.rm=TRUE)
seiwE <- tapply(x$wE, x$year, std.error, na.rm=TRUE)
ciH <- iwE + 2*seiwE
ciL <- iwE - 2*seiwE
biwE <- coef(lm(log(iwE[c(20:64)]) ~ year[c(20:64)]))[2]
lines(year[c(20:64)], iwE[c(20:64)], lty=0)
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(0, 0, 1, .25), border=NA)
lines(year[c(20:64)], iwE[c(20:64)], lty=1, col="blue")
lines(year[c(20:64)], miwE[c(20:64)], lty=2, col="blue")
legend("top", bty="n", legend=c(expression(paste(bar(y[E]))), 
                                expression(paste(bar(w[E]))), 
                                expression(paste(tilde(y[E]))), 
                                expression(paste(tilde(w[E])))), 
       lty =c(1,1, 2, 2), lwd = c(1, 1, 2, 2), col=c("red", "blue", "red", "blue"))
mtext(bquote(hat(bar(y[E]))  ==.(round(biyE, 4))~","~ hat(bar(w[E])) ==.(round(biwE, 4))))

iyL <- tapply(x$yL, x$year, mean, na.rm=TRUE)
miyL <- tapply(x$yL, x$year, median, na.rm=TRUE)
seiyL <- tapply(x$yL, x$year, std.error, na.rm=TRUE)
ciH <- iyL + 2*seiyL
ciL <- iyL - 2*seiyL
biyL <- coef(lm(log(iyL[c(20:64)]) ~ year[c(20:64)]))[2]
plot(year[c(20:64)], iyL[c(20:64)], type="l", lty=0, 
     main=expression(paste("Means (95% CI) and medians of ", y[L], 
                           " and ", w[L])), 
     xlab="Year", ylab="2011 USD", ylim=c(4, 41))
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(1, 0, 0, alpha=.25), border=NA)
lines(year[c(20:64)], iyL[c(20:64)], lty=1, col="red")
lines(year[c(20:64)], miyL[c(20:64)], lty=2, col="red")
iwL <- tapply(x$wL, x$year, mean, na.rm=TRUE)
miwL <- tapply(x$wL, x$year, median, na.rm=TRUE)
seiwL <- tapply(x$wL, x$year, std.error, na.rm=TRUE)
ciH <- iwL + 2*seiwL
ciL <- iwL - 2*seiwL
biwL <- coef(lm(log(iwL[c(20:64)]) ~ year[c(20:64)]))[2]
lines(year[c(20:64)], iwL[c(20:64)], lty=0)
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(0, 0, 1, .25), border=NA)
lines(year[c(20:64)], iwL[c(20:64)], lty=1, col="blue")
lines(year[c(20:64)], miwL[c(20:64)], lty=2, col="blue")
legend("topleft", bty="n", legend=c(expression(paste(bar(y[L]))), 
                                expression(paste(bar(w[L]))), 
                                expression(paste(tilde(y[L]))), 
                                expression(paste(tilde(w[L])))), 
       lty =c(1,1, 2, 2), lwd = c(1, 1, 2, 2), col=c("red", "blue", "red", "blue"))
mtext(bquote(hat(bar(y[L]))  ==.(round(biyL, 4))~","~ hat(bar(w[L])) ==.(round(biwL, 4))))

is <- tapply(x$s, x$year, mean, na.rm=TRUE)
mis <- tapply(x$s, x$year, median, na.rm=TRUE)
seis <- tapply(x$s, x$year, std.error, na.rm=TRUE)
ciH <- is + 2*seis
ciL <- is - 2*seis
bis <- coef(lm(log(is[c(20:64)]) ~ year[c(20:64)]))[2]
plot(year[c(20:64)], is[c(20:64)], type="l", lty=0, 
     main=expression(paste("Means (95% CI) and medians of ", 
                           sigma, " and ", sigma[e])), 
     xlab="Year", ylab=" ", ylim=c(0.4, 1.7))
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(1, 0, 0, alpha=.25), border=NA)
lines(year[c(20:64)], is[c(20:64)], lty=1, col="red")
lines(year[c(20:64)], mis[c(20:64)], lty=2, col="red")
iss <- tapply(x$ss, x$year, mean, na.rm=TRUE)
miss <- tapply(x$ss, x$year, median, na.rm=TRUE)
seiss <- tapply(x$ss, x$year, std.error, na.rm=TRUE)
ciH <- iss + 2*seiss
ciL <- iss - 2*seiss
biss <- coef(lm(log(iss[c(20:64)]) ~ year[c(20:64)]))[2]
lines(year[c(20:64)], iss[c(20:64)], lty=0)
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(0, 0, 1, .25), border=NA)
lines(year[c(20:64)], iss[c(20:64)], lty=1, col="blue")
lines(year[c(20:64)], miss[c(20:64)], lty=2, col="blue")
legend("top", bty="n", legend=c(expression(paste(bar(sigma))), 
                                    expression(paste(bar(sigma[e]))), 
                                    expression(paste(tilde(sigma))), 
                                    expression(paste(tilde(sigma[e])))), 
       lty =c(1,1, 2, 2), col=c("red", "blue", "red", "blue"))
mtext(bquote(hat(bar(sigma))  ==.(round(bis, 4))~","~ hat(bar(sigma[e])) ==.(round(biss, 4))))

ir <- tapply(x$r, x$year, mean, na.rm=TRUE)
mir <- tapply(x$r, x$year, median, na.rm=TRUE)
seir <- tapply(x$r, x$year, std.error, na.rm=TRUE)
ciH <- ir + 2*seir
ciL <- ir - 2*seir
bir <- coef(lm(log(ir[c(20:64)]) ~ year[c(20:64)]))[2]
plot(year[c(20:64)], ir[c(20:64)], type="l", lty=0, 
     main=expression(paste("Means (95% CI) and medians of ", 
                           r, " and ", kappa)), 
     xlab="Year", ylab=" ", ylim=c(0.05, .3))
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(1, 0, 0, alpha=.25), border=NA)
lines(year[c(20:64)], ir[c(20:64)], lty=1, col="red")
lines(year[c(20:64)], mir[c(20:64)], lty=2, col="red")
ika <- tapply(x$ka, x$year, mean, na.rm=TRUE)
mika <- tapply(x$ka, x$year, median, na.rm=TRUE)
seika <- tapply(x$ka, x$year, std.error, na.rm=TRUE)
ciH <- ika + 2*seika
ciL <- ika - 2*seika
bika <- coef(lm(log(ika[c(20:64)]) ~ year[c(20:64)]))[2]
lines(year[c(20:64)], ika[c(20:64)], lty=0)
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(0, 0, 1, .25), border=NA)
lines(year[c(20:64)], ika[c(20:64)], lty=1, col="blue")
lines(year[c(20:64)], mika[c(20:64)], lty=2, col="blue")
legend("top", bty="n", legend=c(expression(paste(bar(r))), 
                                expression(paste(bar(kappa))), 
                                expression(paste(tilde(r))), 
                                expression(paste(tilde(kappa)))), 
       lty =c(1,1, 2, 2), lwd = c(1, 1, 2, 2), col=c("red", "blue", "red", "blue"))
mtext(bquote(hat(bar(r))  ==.(round(bir, 4))~","~ hat(bar(kappa)) ==.(round(bika, 4))))

# OUTPUT-WEIGHTED/COUNTRY STRUCTURAL PROPORTIONS (YEAR SERIES) PLOTS
# summary(x$yN)[7]
# 180 is not correct.  Rough number. Correct one should be the number of 
# countries included each given year in the aggregate, i.e. each year: 182-NA's

# ALSO, FIX ylim()

# Productivity
iyN <- tapply(x$yN*x0$theta*180, x$year, mean, na.rm=TRUE)
biyN <- coef(lm(log(iyN[c(20:64)]) ~ year[c(20:64)]))[2]
seiyN <- tapply(x$yN*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- iyN + 2*seiyN
ciL <- iyN - 2*seiyN
plot(year[c(20:64)], iyN[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of per-capita output ", y[N])), 
     xlab="Year", ylab="2011 USD/capita", ylim=c(0, 50000), sub = "Y-weighted Int'l. Data: PWT.")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iyN[c(20:64)], lty=1)
mtext(bquote(bar(y[N])^T  == .(round(mean(iyN[c(20:64)]), 1))~ "
"~se(bar(y[N]))^T ==.(round(mean(seiyN[c(20:64)]), 1))~" 
"~hat(bar(y[N]))  ==.(round(biyN, 4))))

iyE <- tapply(x$yE*x0$theta*180, x$year, mean, na.rm=TRUE)
summary(iyE)
biyE <- coef(lm(log(iyE[c(20:64)]) ~ year[c(20:64)]))[2]
seiyE <- tapply(x$yE*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- iyE + 2*seiyE
ciL <- iyE - 2*seiyE
plot(year[c(20:64)], iyE[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of per-worker output ", y[E])), 
     xlab="Year", ylab="2011 USD/worker", ylim=c(5000, 140000), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iyE[c(20:64)], lty=1)
mtext(bquote(bar(y[E])^T  == .(round(mean(iyE[c(20:64)]), 1))~ "
"~se(bar(y[E]))^T ==.(round(mean(seiyE[c(20:64)]), 1))~" 
"~hat(bar(y[E]))  ==.(round(biyE, 4))))

iyL <- tapply(x$yL*x0$theta*180, x$year, mean, na.rm=TRUE)
summary(iyL)
biyL <- coef(lm(log(iyL[c(20:64)]) ~ year[c(20:64)]))[2]
seiyL <- tapply(x$yL*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- iyL + 2*seiyL
ciL <- iyL - 2*seiyL
plot(year[c(20:64)], iyL[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of per-hour output ", y[L])), 
     xlab="Year", ylab="2011 USD/hour", ylim=c(0, 200), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iyL[c(20:64)], lty=1)
mtext(bquote(bar(y[L])^T  == .(round(mean(iyL[c(20:64)]), 1))~ "
"~se(bar(y[L]))^T ==.(round(mean(seiyL[c(20:64)]), 1))~" 
"~hat(bar(y[L]))  ==.(round(biyL, 4))))

# Note that, when Y weighted, productivity looks stagnant, perhaps because the rich countries growth
# is anemic compared to the productivity growth of (say) CHN?

# Wage rates
iwN <- tapply(x$wN*x0$theta*180, x$year, mean, na.rm=TRUE)
summary(iwN)
biwN <- coef(lm(log(iwN[c(20:64)]) ~ year[c(20:64)]))[2]
seiwN <- tapply(x$wN*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- iwN + 2*seiwN
ciL <- iwN - 2*seiwN
plot(year[c(20:64)], iwN[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of per-capita wage ", w[N])), 
     xlab="Year", ylab="2011 USD/capita", ylim=c(0, 40000), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iwN[c(20:64)], lty=1)
# lines(year[c(20:64)], iwN[c(20:64)] + 2*seiwN[c(20:64)], lty=2)
# lines(year[c(20:64)], iwN[c(20:64)] - 2*seiwN[c(20:64)], lty=2)
mtext(bquote(bar(w[N])^T  == .(round(mean(iwN[c(20:64)]), 1))~ "
"~se(bar(w[N]))^T ==.(round(mean(seiwN[c(20:64)]), 1))~" 
"~hat(bar(w[N]))  ==.(round(biwN, 4))))

iwE <- tapply(x$wE*x0$theta*180, x$year, mean, na.rm=TRUE)
summary(iwE)
biwE <- coef(lm(log(iwE[c(20:64)]) ~ year[c(20:64)]))[2]
seiwE <- tapply(x$wE*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- iwE + 2*seiwE
ciL <- iwE - 2*seiwE
plot(year[c(20:64)], iwE[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of per-worker wage ", w[E])), 
     xlab="Year", ylab="2011 USD/worker", ylim=c(0, 100000), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iwE[c(20:64)], lty=1)
mtext(bquote(bar(w[E])^T  == .(round(mean(iwE[c(20:64)]), 1))~ "
"~se(bar(w[E]))^T ==.(round(mean(seiwE[c(20:64)]), 1))~" 
"~hat(bar(w[E]))  ==.(round(biwE, 4))))

iwL <- tapply(x$wL*x0$theta*180, x$year, mean, na.rm=TRUE)
summary(iwL)
biwL <- coef(lm(log(iwL[c(20:64)]) ~ year[c(20:64)]))[2]
seiwL <- tapply(x$wL*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- iwL + 2*seiwL
ciL <- iwL - 2*seiwL
plot(year[c(20:64)], iwL[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of per-hour wage ", w[L])), 
     xlab="Year", ylab="2011 USD/hour", ylim=c(-.1, .6), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iwL[c(20:64)], lty=1)
mtext(bquote(bar(w[L])^T  == .(round(mean(iwL[c(20:64)]), 2))~ "
"~se(bar(w[L]))^T ==.(round(mean(seiwL[c(20:64)]), 2))~" 
"~tilde(w[L])^T  == .(round(mean(miwL[c(20:64)]), 2))~" 
"~hat(bar(w[L]))  ==.(round(biwL, 4))))

# Exploitation
is <- tapply(x$s*x0$theta*180, x$year, mean, na.rm=TRUE)
summary(is)
bis <- coef(lm(log(is[c(20:64)]) ~ year[c(20:64)]))[2]
seis <- tapply(x$s*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- is + 2*seis
ciL <- is - 2*seis
plot(year[c(20:64)], is[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of exploitation rate ", sigma)), 
     xlab="Year", ylab=" ", ylim=c(-.001, .015), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], is[c(20:64)], lty=1)
mtext(bquote(bar(sigma)^T  == .(round(mean(is[c(20:64)]), 3))~ "
"~se(bar(sigma))^T ==.(round(mean(seis[c(20:64)]), 3))~" 
"~hat(bar(sigma))  ==.(round(bis, 4))))

# Stagnant rate of exploitation as exploitation rates in poorer countries (CHN) are lower?

iss <- tapply(x$ss*x0$theta*180, x$year, mean, na.rm=TRUE)
summary(iss)
biss <- coef(lm(log(iss[c(20:64)]) ~ year[c(20:64)]))[2]
seiss <- tapply(x$ss*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- iss + 2*seiss
ciL <- iss - 2*seiss
plot(year[c(20:64)], iss[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of exploitation rate[e] ", sigma[e])), 
     xlab="Year", ylab=" ", ylim=c(-.001, .012), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iss[c(20:64)], lty=1)
mtext(bquote(bar(sigma[e])^T  == .(round(mean(iss[c(20:64)]), 3))~ "
"~se(bar(sigma[e]))^T ==.(round(mean(seiss[c(20:64)]), 3))~" 
"~hat(bar(sigma[e]))  ==.(round(biss, 4))))

# Poor countries are financially constrained.

# Capital composition
ih <- tapply(x$h*x0$theta*180, x$year, mean, na.rm=TRUE)
summary(ih)
bih <- coef(lm(log(ih[c(20:64)]) ~ year[c(20:64)]))[2]
seih <- tapply(x$h*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- ih + 2*seih
ciL <- ih - 2*seih
plot(year[c(20:64)], ih[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of capital composition ", h)), 
     xlab="Year", ylab=" ", ylim=c(1e-8, 3.5e-8), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], ih[c(20:64)], lty=1)
mtext(bquote(bar(h)^T  == .(round(mean(ih[c(20:64)]), 10))~ "
"~se(bar(h))^T ==.(round(mean(seih[c(20:64)]), 10))~" 
"~hat(bar(h))  ==.(round(bih, 6))))

# Output-capital ratio
irho <- tapply(x$rho*x0$theta*180, x$year, mean, na.rm=TRUE)
summary(irho)
birho <- coef(lm(log(irho[c(20:64)]) ~ year[c(20:64)]))[2]
seirho <- tapply(x$rho*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- irho + 2*seirho
ciL <- irho - 2*seirho
plot(year[c(20:64)], irho[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of output-capital ratio ", rho)), 
     xlab="Year", ylab=" ", ylim=c(0, .004), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], irho[c(20:64)], lty=1)
mtext(bquote(bar(rho)^T  == .(round(mean(irho[c(20:64)]), 4))~ "
"~se(bar(rho))^T ==.(round(mean(seirho[c(20:64)]), 4))~" 
"~hat(bar(rho))  ==.(round(birho, 5))))

# Profitability
ir <- tapply(x$r*x0$theta*180, x$year, mean, na.rm=TRUE)
summary(ir)
bir <- coef(lm(log(ir[c(20:64)]) ~ year[c(20:64)]))[2]
seir <- tapply(x$r*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- ir + 2*seir
ciL <- ir - 2*seir
plot(year[c(20:64)], ir[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of profit rate ", r)), 
     xlab="Year", ylab=" ", ylim=c(0, .0025), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], ir[c(20:64)], lty=1)
mtext(bquote(bar(r)^T  == .(round(mean(ir[c(20:64)]), 4))~ "
"~se(bar(r))^T ==.(round(mean(seir[c(20:64)]), 4))~" 
"~hat(bar(r))  ==.(round(bir, 5))))

ika <- tapply(x$ka*x0$theta*180, x$year, mean, na.rm=TRUE)
summary(ika)
bika <- coef(lm(log(ika[c(20:64)]) ~ year[c(20:64)]))[2]
seika <- tapply(x$ka*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- ika + 2*seika
ciL <- ika - 2*seika
plot(year[c(20:64)], ika[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of accumulation rate ", kappa)), 
     xlab="Year", ylab=" ", ylim=c(0, .002), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], ika[c(20:64)], lty=1)
mtext(bquote(bar(kappa)^T  == .(round(mean(ika[c(20:64)]), 4))~ "
"~se(bar(kappa))^T ==.(round(mean(seika[c(20:64)]), 4))~" 
"~hat(bar(ka))  ==.(round(bika, 5))))

# Comparisons: means (+/-CI)
iyN <- tapply(x$yN*x0$theta*180, x$year, mean, na.rm=TRUE)
summary(iyN)
seiyN <- tapply(x$yN*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- iyN + 2*seiyN
ciL <- iyN - 2*seiyN
biyN <- coef(lm(log(iyN) ~ year))[2]
plot(year[c(20:64)], iyN[c(20:64)], type="l", lty=0, 
     main=expression(paste("Means (95% CI) of ", y[N], " and ", w[N])), 
     xlab="Year", ylab="2011 USD", ylim=c(0, 325))
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(1, 0, 0, alpha=.25), border=NA)
lines(year[c(20:64)], iyN[c(20:64)], lty=1, col="red")
iwN <- tapply(x$wN*x0$theta*180, x$year, mean, na.rm=TRUE)
seiwN <- tapply(x$wN*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- iwN + 2*seiwN
ciL <- iwN - 2*seiwN
biwN <- coef(lm(log(iwN) ~ year))[2]
lines(year[c(20:64)], iwN[c(20:64)], lty=0)
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(0, 0, 1, .25), border=NA)
lines(year[c(20:64)], iwN[c(20:64)], lty=1, col="blue")
legend("top", bty="n", legend=c(expression(paste(bar(y[N]))), 
                                expression(paste(bar(w[N])))), 
       lty =c(1,1), lwd = c(1, 1), col=c("red", "blue"))
mtext(bquote(hat(bar(y[N]))  ==.(round(biyN, 4))~","~ hat(bar(w[N])) ==.(round(biwN, 4))))

iyE <- tapply(x$yE*x0$theta*180, x$year, mean, na.rm=TRUE)
seiyE <- tapply(x$yE*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- iyE + 2*seiyE
ciL <- iyE - 2*seiyE
biyE <- coef(lm(log(iyE) ~ year))[2]
plot(year[c(20:64)], iyE[c(20:64)], type="l", lty=0, 
     main=expression(paste("Means (95% CI) of ", y[E], 
                           " and ", w[E])), 
     xlab="Year", ylab="2011 USD", ylim=c(0, 800))
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(1, 0, 0, alpha=.25), border=NA)
lines(year[c(20:64)], iyE[c(20:64)], lty=1, col="red")
iwE <- tapply(x$wE*x0$theta*180, x$year, mean, na.rm=TRUE)
seiwE <- tapply(x$wE*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- iwE + 2*seiwE
ciL <- iwE - 2*seiwE
biwE <- coef(lm(log(iwE) ~ year))[2]
lines(year[c(20:64)], iwE[c(20:64)], lty=0)
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(0, 0, 1, .25), border=NA)
lines(year[c(20:64)], iwE[c(20:64)], lty=1, col="blue")
legend("top", bty="n", legend=c(expression(paste(bar(y[E]))), 
                                expression(paste(bar(w[E])))), 
       lty =c(1,1, 2, 2), lwd = c(1, 1, 2, 2), col=c("red", "blue", "red", "blue"))
mtext(bquote(hat(bar(y[E]))  ==.(round(biyE, 4))~","~ hat(bar(w[E])) ==.(round(biwE, 4))))

iyL <- tapply(x$yL*x0$theta*180, x$year, mean, na.rm=TRUE)
seiyL <- tapply(x$yL*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- iyL + 2*seiyL
ciL <- iyL - 2*seiyL
biyL <- coef(lm(log(iyL) ~ year))[2]
plot(year[c(20:64)], iyL[c(20:64)], type="l", lty=0, 
     main=expression(paste("Means (95% CI) of ", y[L], 
                           " and ", w[L])), 
     xlab="Year", ylab="2011 USD", ylim=c(0, 1))
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(1, 0, 0, alpha=.25), border=NA)
lines(year[c(20:64)], iyL[c(20:64)], lty=1, col="red")
iwL <- tapply(x$wL*x0$theta*180, x$year, mean, na.rm=TRUE)
seiwL <- tapply(x$wL*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- iwL + 2*seiwL
ciL <- iwL - 2*seiwL
biwL <- coef(lm(log(iwL) ~ year))[2]
lines(year[c(20:64)], iwL[c(20:64)], lty=0)
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(0, 0, 1, .25), border=NA)
lines(year[c(20:64)], iwL[c(20:64)], lty=1, col="blue")
legend("topleft", bty="n", legend=c(expression(paste(bar(y[L]))), 
                                    expression(paste(bar(w[L])))), 
       lty =c(1,1, 2, 2), lwd = c(1, 1, 2, 2), col=c("red", "blue", "red", "blue"))
mtext(bquote(hat(bar(y[L]))  ==.(round(biyL, 4))~","~ hat(bar(w[L])) ==.(round(biwL, 4))))

is <- tapply(x$s*x0$theta*180, x$year, mean, na.rm=TRUE)
seis <- tapply(x$s*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- is + 2*seis
ciL <- is - 2*seis
bis <- coef(lm(log(is) ~ year))[2]
plot(year[c(20:64)], is[c(20:64)], type="l", lty=0, 
     main=expression(paste("Means (95% CI) of ", 
                           sigma, " and ", sigma[e])), 
     xlab="Year", ylab=" ", ylim=c(0, .0125))
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(1, 0, 0, alpha=.25), border=NA)
lines(year[c(20:64)], is[c(20:64)], lty=1, col="red")
iss <- tapply(x$ss*x0$theta*180, x$year, mean, na.rm=TRUE)
seiss <- tapply(x$ss*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- iss + 2*seiss
ciL <- iss - 2*seiss
biss <- coef(lm(log(iss) ~ year))[2]
lines(year[c(20:64)], iss[c(20:64)], lty=0)
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(0, 0, 1, .25), border=NA)
lines(year[c(20:64)], iss[c(20:64)], lty=1, col="blue")
legend("top", bty="n", legend=c(expression(paste(bar(sigma))), 
                                expression(paste(bar(sigma[e])))), 
       lty =c(1,1, 2, 2), col=c("red", "blue", "red", "blue"))
mtext(bquote(hat(bar(sigma))  ==.(round(bis, 4))~","~ hat(bar(sigma[e])) ==.(round(biss, 4))))

ir <- tapply(x$r*x0$theta*180, x$year, mean, na.rm=TRUE)
seir <- tapply(x$r*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- ir + 2*seir
ciL <- ir - 2*seir
bir <- coef(lm(log(ir) ~ year))[2]
plot(year[c(20:64)], ir[c(20:64)], type="l", lty=0, 
     main=expression(paste("Means (95% CI) of ", 
                           r, " and ", kappa)), 
     xlab="Year", ylab=" ", ylim=c(0, .002))
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(1, 0, 0, alpha=.25), border=NA)
lines(year[c(20:64)], ir[c(20:64)], lty=1, col="red")
ika <- tapply(x$ka*x0$theta*180, x$year, mean, na.rm=TRUE)
seika <- tapply(x$ka*x0$theta*180, x$year, std.error, na.rm=TRUE)
ciH <- ika + 2*seika
ciL <- ika - 2*seika
bika <- coef(lm(log(ika) ~ year))[2]
lines(year[c(20:64)], ika[c(20:64)], lty=0)
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(0, 0, 1, .25), border=NA)
lines(year[c(20:64)], ika[c(20:64)], lty=1, col="blue")
legend("top", bty="n", legend=c(expression(paste(bar(r))), 
                                expression(paste(bar(kappa)))), 
       lty =c(1,1, 2, 2), lwd = c(1, 1, 2, 2), col=c("red", "blue", "red", "blue"))
mtext(bquote(hat(bar(r))  ==.(round(bir, 4))~","~ hat(bar(kappa)) ==.(round(bika, 4))))


# POPULATION-WEIGHTED/COUNTRY STRUCTURAL PROPORTIONS (YEAR SERIES) PLOTS

# Same problem: summary(x$yN)[7]
# 180 is not correct.  Rough number. Correct one should be the number of 
# countries included each given year in the aggregate, i.e. each year: 182-NA's

# ALSO, FIX ylim()

# Productivity
iyN <- tapply(x$yN*x0$delta*180, x$year, mean, na.rm=TRUE)
biyN <- coef(lm(log(iyN[c(20:64)]) ~ year[c(20:64)]))[2]
seiyN <- tapply(x$yN*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- iyN + 2*seiyN
ciL <- iyN - 2*seiyN
plot(year[c(20:64)], iyN[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of per-capita output ", y[N])), 
     xlab="Year", ylab="2011 USD/capita", ylim=c(0, 50000), sub = "Y-weighted Int'l. Data: PWT.")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iyN[c(20:64)], lty=1)
mtext(bquote(bar(y[N])^T  == .(round(mean(iyN[c(20:64)]), 1))~ "
"~se(bar(y[N]))^T ==.(round(mean(seiyN[c(20:64)]), 1))~" 
"~hat(bar(y[N]))  ==.(round(biyN, 4))))

iyE <- tapply(x$yE*x0$delta*180, x$year, mean, na.rm=TRUE)
summary(iyE)
biyE <- coef(lm(log(iyE[c(20:64)]) ~ year[c(20:64)]))[2]
seiyE <- tapply(x$yE*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- iyE + 2*seiyE
ciL <- iyE - 2*seiyE
plot(year[c(20:64)], iyE[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of per-worker output ", y[E])), 
     xlab="Year", ylab="2011 USD/worker", ylim=c(5000, 140000), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iyE[c(20:64)], lty=1)
mtext(bquote(bar(y[E])^T  == .(round(mean(iyE[c(20:64)]), 1))~ "
"~se(bar(y[E]))^T ==.(round(mean(seiyE[c(20:64)]), 1))~" 
"~hat(bar(y[E]))  ==.(round(biyE, 4))))

iyL <- tapply(x$yL*x0$delta*180, x$year, mean, na.rm=TRUE)
summary(iyL)
biyL <- coef(lm(log(iyL[c(20:64)]) ~ year[c(20:64)]))[2]
seiyL <- tapply(x$yL*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- iyL + 2*seiyL
ciL <- iyL - 2*seiyL
plot(year[c(20:64)], iyL[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of per-hour output ", y[L])), 
     xlab="Year", ylab="2011 USD/hour", ylim=c(0, 200), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iyL[c(20:64)], lty=1)
mtext(bquote(bar(y[L])^T  == .(round(mean(iyL[c(20:64)]), 1))~ "
"~se(bar(y[L]))^T ==.(round(mean(seiyL[c(20:64)]), 1))~" 
"~hat(bar(y[L]))  ==.(round(biyL, 4))))

# Wage rates
iwN <- tapply(x$wN*x0$delta*180, x$year, mean, na.rm=TRUE)
summary(iwN)
biwN <- coef(lm(log(iwN[c(20:64)]) ~ year[c(20:64)]))[2]
seiwN <- tapply(x$wN*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- iwN + 2*seiwN
ciL <- iwN - 2*seiwN
plot(year[c(20:64)], iwN[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of per-capita wage ", w[N])), 
     xlab="Year", ylab="2011 USD/capita", ylim=c(0, 40000), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iwN[c(20:64)], lty=1)
# lines(year[c(20:64)], iwN[c(20:64)] + 2*seiwN[c(20:64)], lty=2)
# lines(year[c(20:64)], iwN[c(20:64)] - 2*seiwN[c(20:64)], lty=2)
mtext(bquote(bar(w[N])^T  == .(round(mean(iwN[c(20:64)]), 1))~ "
"~se(bar(w[N]))^T ==.(round(mean(seiwN[c(20:64)]), 1))~" 
"~hat(bar(w[N]))  ==.(round(biwN, 4))))

iwE <- tapply(x$wE*x0$delta*180, x$year, mean, na.rm=TRUE)
summary(iwE)
biwE <- coef(lm(log(iwE[c(20:64)]) ~ year[c(20:64)]))[2]
seiwE <- tapply(x$wE*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- iwE + 2*seiwE
ciL <- iwE - 2*seiwE
plot(year[c(20:64)], iwE[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of per-worker wage ", w[E])), 
     xlab="Year", ylab="2011 USD/worker", ylim=c(0, 100000), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iwE[c(20:64)], lty=1)
mtext(bquote(bar(w[E])^T  == .(round(mean(iwE[c(20:64)]), 1))~ "
"~se(bar(w[E]))^T ==.(round(mean(seiwE[c(20:64)]), 1))~" 
"~hat(bar(w[E]))  ==.(round(biwE, 4))))

iwL <- tapply(x$wL*x0$delta*180, x$year, mean, na.rm=TRUE)
summary(iwL)
biwL <- coef(lm(log(iwL[c(20:64)]) ~ year[c(20:64)]))[2]
seiwL <- tapply(x$wL*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- iwL + 2*seiwL
ciL <- iwL - 2*seiwL
plot(year[c(20:64)], iwL[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of per-hour wage ", w[L])), 
     xlab="Year", ylab="2011 USD/hour", ylim=c(-.1, .6), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iwL[c(20:64)], lty=1)
mtext(bquote(bar(w[L])^T  == .(round(mean(iwL[c(20:64)]), 2))~ "
"~se(bar(w[L]))^T ==.(round(mean(seiwL[c(20:64)]), 2))~" 
"~tilde(w[L])^T  == .(round(mean(miwL[c(20:64)]), 2))~" 
"~hat(bar(w[L]))  ==.(round(biwL, 4))))

# Exploitation
is <- tapply(x$s*x0$delta*180, x$year, mean, na.rm=TRUE)
summary(is)
bis <- coef(lm(log(is[c(20:64)]) ~ year[c(20:64)]))[2]
seis <- tapply(x$s*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- is + 2*seis
ciL <- is - 2*seis
plot(year[c(20:64)], is[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of exploitation rate ", sigma)), 
     xlab="Year", ylab=" ", ylim=c(-.001, .015), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], is[c(20:64)], lty=1)
mtext(bquote(bar(sigma)^T  == .(round(mean(is[c(20:64)]), 3))~ "
"~se(bar(sigma))^T ==.(round(mean(seis[c(20:64)]), 3))~" 
"~hat(bar(sigma))  ==.(round(bis, 4))))

# Stagnant rate of exploitation as exploitation rates in poorer countries (CHN) are lower?

iss <- tapply(x$ss*x0$delta*180, x$year, mean, na.rm=TRUE)
summary(iss)
biss <- coef(lm(log(iss[c(20:64)]) ~ year[c(20:64)]))[2]
seiss <- tapply(x$ss*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- iss + 2*seiss
ciL <- iss - 2*seiss
plot(year[c(20:64)], iss[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of exploitation rate[e] ", sigma[e])), 
     xlab="Year", ylab=" ", ylim=c(-.001, .012), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], iss[c(20:64)], lty=1)
mtext(bquote(bar(sigma[e])^T  == .(round(mean(iss[c(20:64)]), 3))~ "
"~se(bar(sigma[e]))^T ==.(round(mean(seiss[c(20:64)]), 3))~" 
"~hat(bar(sigma[e]))  ==.(round(biss, 4))))

# Poor countries are financially constrained.

# Capital composition
ih <- tapply(x$h*x0$delta*180, x$year, mean, na.rm=TRUE)
summary(ih)
bih <- coef(lm(log(ih[c(20:64)]) ~ year[c(20:64)]))[2]
seih <- tapply(x$h*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- ih + 2*seih
ciL <- ih - 2*seih
plot(year[c(20:64)], ih[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of capital composition ", h)), 
     xlab="Year", ylab=" ", ylim=c(1e-8, 3.5e-8), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], ih[c(20:64)], lty=1)
mtext(bquote(bar(h)^T  == .(round(mean(ih[c(20:64)]), 10))~ "
"~se(bar(h))^T ==.(round(mean(seih[c(20:64)]), 10))~" 
"~hat(bar(h))  ==.(round(bih, 6))))

# Output-capital ratio
irho <- tapply(x$rho*x0$delta*180, x$year, mean, na.rm=TRUE)
summary(irho)
birho <- coef(lm(log(irho[c(20:64)]) ~ year[c(20:64)]))[2]
seirho <- tapply(x$rho*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- irho + 2*seirho
ciL <- irho - 2*seirho
plot(year[c(20:64)], irho[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of output-capital ratio ", rho)), 
     xlab="Year", ylab=" ", ylim=c(0, .004), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], irho[c(20:64)], lty=1)
mtext(bquote(bar(rho)^T  == .(round(mean(irho[c(20:64)]), 4))~ "
"~se(bar(rho))^T ==.(round(mean(seirho[c(20:64)]), 4))~" 
"~hat(bar(rho))  ==.(round(birho, 5))))

# Profitability
ir <- tapply(x$r*x0$delta*180, x$year, mean, na.rm=TRUE)
summary(ir)
bir <- coef(lm(log(ir[c(20:64)]) ~ year[c(20:64)]))[2]
seir <- tapply(x$r*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- ir + 2*seir
ciL <- ir - 2*seir
plot(year[c(20:64)], ir[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of profit rate ", r)), 
     xlab="Year", ylab=" ", ylim=c(0, .0025), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], ir[c(20:64)], lty=1)
mtext(bquote(bar(r)^T  == .(round(mean(ir[c(20:64)]), 4))~ "
"~se(bar(r))^T ==.(round(mean(seir[c(20:64)]), 4))~" 
"~hat(bar(r))  ==.(round(bir, 5))))

ika <- tapply(x$ka*x0$delta*180, x$year, mean, na.rm=TRUE)
summary(ika)
bika <- coef(lm(log(ika[c(20:64)]) ~ year[c(20:64)]))[2]
seika <- tapply(x$ka*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- ika + 2*seika
ciL <- ika - 2*seika
plot(year[c(20:64)], ika[c(20:64)], type="l", lty=0, 
     main=expression(paste("Mean (95% CI) of accumulation rate ", kappa)), 
     xlab="Year", ylab=" ", ylim=c(0, .002), sub = "Data: PWT")
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col="gray90", border=NA)
lines(year[c(20:64)], ika[c(20:64)], lty=1)
mtext(bquote(bar(kappa)^T  == .(round(mean(ika[c(20:64)]), 4))~ "
"~se(bar(kappa))^T ==.(round(mean(seika[c(20:64)]), 4))~" 
"~hat(bar(ka))  ==.(round(bika, 5))))

# Comparisons: means (+/-CI)
iyN <- tapply(x$yN*x0$delta*180, x$year, mean, na.rm=TRUE)
summary(iyN)
seiyN <- tapply(x$yN*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- iyN + 2*seiyN
ciL <- iyN - 2*seiyN
biyN <- coef(lm(log(iyN) ~ year))[2]
plot(year[c(20:64)], iyN[c(20:64)], type="l", lty=0, 
     main=expression(paste("Means (95% CI) of ", y[N], " and ", w[N])), 
     xlab="Year", ylab="2011 USD", ylim=c(0, 325))
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(1, 0, 0, alpha=.25), border=NA)
lines(year[c(20:64)], iyN[c(20:64)], lty=1, col="red")
iwN <- tapply(x$wN*x0$delta*180, x$year, mean, na.rm=TRUE)
seiwN <- tapply(x$wN*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- iwN + 2*seiwN
ciL <- iwN - 2*seiwN
biwN <- coef(lm(log(iwN) ~ year))[2]
lines(year[c(20:64)], iwN[c(20:64)], lty=0)
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(0, 0, 1, .25), border=NA)
lines(year[c(20:64)], iwN[c(20:64)], lty=1, col="blue")
legend("top", bty="n", legend=c(expression(paste(bar(y[N]))), 
                                expression(paste(bar(w[N])))), 
       lty =c(1,1), lwd = c(1, 1), col=c("red", "blue"))
mtext(bquote(hat(bar(y[N]))  ==.(round(biyN, 4))~","~ hat(bar(w[N])) ==.(round(biwN, 4))))

iyE <- tapply(x$yE*x0$delta*180, x$year, mean, na.rm=TRUE)
seiyE <- tapply(x$yE*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- iyE + 2*seiyE
ciL <- iyE - 2*seiyE
biyE <- coef(lm(log(iyE) ~ year))[2]
plot(year[c(20:64)], iyE[c(20:64)], type="l", lty=0, 
     main=expression(paste("Means (95% CI) of ", y[E], 
                           " and ", w[E])), 
     xlab="Year", ylab="2011 USD", ylim=c(0, 800))
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(1, 0, 0, alpha=.25), border=NA)
lines(year[c(20:64)], iyE[c(20:64)], lty=1, col="red")
iwE <- tapply(x$wE*x0$delta*180, x$year, mean, na.rm=TRUE)
seiwE <- tapply(x$wE*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- iwE + 2*seiwE
ciL <- iwE - 2*seiwE
biwE <- coef(lm(log(iwE) ~ year))[2]
lines(year[c(20:64)], iwE[c(20:64)], lty=0)
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(0, 0, 1, .25), border=NA)
lines(year[c(20:64)], iwE[c(20:64)], lty=1, col="blue")
legend("top", bty="n", legend=c(expression(paste(bar(y[E]))), 
                                expression(paste(bar(w[E])))), 
       lty =c(1,1, 2, 2), lwd = c(1, 1, 2, 2), col=c("red", "blue", "red", "blue"))
mtext(bquote(hat(bar(y[E]))  ==.(round(biyE, 4))~","~ hat(bar(w[E])) ==.(round(biwE, 4))))

iyL <- tapply(x$yL*x0$delta*180, x$year, mean, na.rm=TRUE)
seiyL <- tapply(x$yL*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- iyL + 2*seiyL
ciL <- iyL - 2*seiyL
biyL <- coef(lm(log(iyL) ~ year))[2]
plot(year[c(20:64)], iyL[c(20:64)], type="l", lty=0, 
     main=expression(paste("Means (95% CI) of ", y[L], 
                           " and ", w[L])), 
     xlab="Year", ylab="2011 USD", ylim=c(0, 1))
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(1, 0, 0, alpha=.25), border=NA)
lines(year[c(20:64)], iyL[c(20:64)], lty=1, col="red")
iwL <- tapply(x$wL*x0$delta*180, x$year, mean, na.rm=TRUE)
seiwL <- tapply(x$wL*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- iwL + 2*seiwL
ciL <- iwL - 2*seiwL
biwL <- coef(lm(log(iwL) ~ year))[2]
lines(year[c(20:64)], iwL[c(20:64)], lty=0)
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(0, 0, 1, .25), border=NA)
lines(year[c(20:64)], iwL[c(20:64)], lty=1, col="blue")
legend("topleft", bty="n", legend=c(expression(paste(bar(y[L]))), 
                                    expression(paste(bar(w[L])))), 
       lty =c(1,1, 2, 2), lwd = c(1, 1, 2, 2), col=c("red", "blue", "red", "blue"))
mtext(bquote(hat(bar(y[L]))  ==.(round(biyL, 4))~","~ hat(bar(w[L])) ==.(round(biwL, 4))))

is <- tapply(x$s*x0$delta*180, x$year, mean, na.rm=TRUE)
seis <- tapply(x$s*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- is + 2*seis
ciL <- is - 2*seis
bis <- coef(lm(log(is) ~ year))[2]
plot(year[c(20:64)], is[c(20:64)], type="l", lty=0, 
     main=expression(paste("Means (95% CI) of ", 
                           sigma, " and ", sigma[e])), 
     xlab="Year", ylab=" ", ylim=c(0, .0125))
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(1, 0, 0, alpha=.25), border=NA)
lines(year[c(20:64)], is[c(20:64)], lty=1, col="red")
iss <- tapply(x$ss*x0$delta*180, x$year, mean, na.rm=TRUE)
seiss <- tapply(x$ss*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- iss + 2*seiss
ciL <- iss - 2*seiss
biss <- coef(lm(log(iss) ~ year))[2]
lines(year[c(20:64)], iss[c(20:64)], lty=0)
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(0, 0, 1, .25), border=NA)
lines(year[c(20:64)], iss[c(20:64)], lty=1, col="blue")
legend("top", bty="n", legend=c(expression(paste(bar(sigma))), 
                                expression(paste(bar(sigma[e])))), 
       lty =c(1,1, 2, 2), col=c("red", "blue", "red", "blue"))
mtext(bquote(hat(bar(sigma))  ==.(round(bis, 4))~","~ hat(bar(sigma[e])) ==.(round(biss, 4))))

ir <- tapply(x$r*x0$delta*180, x$year, mean, na.rm=TRUE)
seir <- tapply(x$r*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- ir + 2*seir
ciL <- ir - 2*seir
bir <- coef(lm(log(ir) ~ year))[2]
plot(year[c(20:64)], ir[c(20:64)], type="l", lty=0, 
     main=expression(paste("Means (95% CI) of ", 
                           r, " and ", kappa)), 
     xlab="Year", ylab=" ", ylim=c(0, .002))
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(1, 0, 0, alpha=.25), border=NA)
lines(year[c(20:64)], ir[c(20:64)], lty=1, col="red")
ika <- tapply(x$ka*x0$delta*180, x$year, mean, na.rm=TRUE)
seika <- tapply(x$ka*x0$delta*180, x$year, std.error, na.rm=TRUE)
ciH <- ika + 2*seika
ciL <- ika - 2*seika
bika <- coef(lm(log(ika) ~ year))[2]
lines(year[c(20:64)], ika[c(20:64)], lty=0)
polygon(c(year[c(20:64)], rev(year[c(20:64)])), 
        c(ciL[c(20:64)], rev(ciH[c(20:64)])), 
        col=rgb(0, 0, 1, .25), border=NA)
lines(year[c(20:64)], ika[c(20:64)], lty=1, col="blue")
legend("top", bty="n", legend=c(expression(paste(bar(r))), 
                                expression(paste(bar(kappa)))), 
       lty =c(1,1, 2, 2), lwd = c(1, 1, 2, 2), col=c("red", "blue", "red", "blue"))
mtext(bquote(hat(bar(r))  ==.(round(bir, 4))~","~ hat(bar(kappa)) ==.(round(bika, 4))))

# GROUPS OF COUNTRIES AS CLASSIFIED BY WB, ETC.
