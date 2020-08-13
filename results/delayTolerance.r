pdf("delayTolerance.pdf", width=4, height=3.5, family="Times")
par(mar=c(4, 4, 2, 0.5))
library("segmented")

## Delay Tolerance
data <- read.table("results-delayTolerance-150km-cutoff-12h-3-demand-delayTolerance.txt", sep=",", header=T)

delayTolerance <- data$delayTolerance / 3600
throughput.0 <- data$flow.0/(1e9 * data$delayTolerance)
throughput.1 <- data$flow.1/(1e9 * data$delayTolerance)
throughput.2 <- data$flow.2/(1e9 * data$delayTolerance)

flow.0 <- data$flow.0 / (8*1e15)
flow.1 <- data$flow.1 / (8*1e15)
flow.2 <- data$flow.2 / (8*1e15)

### Resulting bandwidth

# plot(delayTolerance, throughput.0, axes=F, xlab='Delay Tolerance (hours)', ylab='Flow', col=rgb(0,100,0,50,maxColorValue=255), pch=16)
# lines(lowess(delayTolerance, throughput.0), col=rgb(0,100,0,255,maxColorValue=255), lwd=2)
# lines(delayTolerance, predict(nls(throughput.0 ~ SSweibull(delayTolerance, Asym, Drop, lrc, pwr)), list(delayTolerance=delayTolerance)))
# 
# points(delayTolerance, throughput.1, col=rgb(100,0,0,50,maxColorValue=255), pch=16)
# lines(lowess(delayTolerance, throughput.1),col=rgb(100,0,0,255,maxColorValue=255), lwd=2)
# lines(delayTolerance, predict(nls(throughput.1 ~ SSweibull(delayTolerance, Asym, Drop, lrc, pwr)), list(delayTolerance=delayTolerance)))
# 
# points(delayTolerance, throughput.2, col=rgb(0,0,100,50,maxColorValue=255), pch=16)
# lines(lowess(delayTolerance, throughput.2), col=rgb(0,0,100,255,maxColorValue=255), lwd=2)
# lines(delayTolerance, predict(nls(throughput.2 ~ SSweibull(delayTolerance, Asym, Drop, lrc, pwr)), list(delayTolerance=delayTolerance)))
# 
# lines(delayTolerance, throughput.0 + throughput.1 + throughput.2, col=rgb(100,100,100,255,maxColorValue=255), lwd=2)
# 
# axis(1, at=c(10, 100, 170, 240, 360, 480), labels=c("10 h", "100 h", "7 d","10 d", "14 d", "20 d"))
# axis(2)

### Amount of data transferred

plot(delayTolerance, flow.0, axes=F, xlim=c(30,432), xlab='Delay Tolerance (hours)', ylab='Amount of data transferred (TB)', col=rgb(0,255,0,100,maxColorValue=255), pch=15)
points(delayTolerance, flow.1, col=rgb(255,0,0,100,maxColorValue=255), pch=16)
points(delayTolerance, flow.2, col=rgb(0,0,255,100,maxColorValue=255), pch=17)

lines(delayTolerance, predict(segmented(glm(flow.0 ~ delayTolerance), seg.Z = ~delayTolerance, psi=150)), col=rgb(0,255,0,255,maxColorValue=255), lwd=3)
lines(delayTolerance, predict(segmented(glm(flow.1 ~ delayTolerance), seg.Z = ~delayTolerance, psi=230)), col=rgb(255,0,0,255,maxColorValue=255), lwd=3)
lines(delayTolerance, predict(segmented(glm(flow.2 ~ delayTolerance), seg.Z = ~delayTolerance, psi=230)), col=rgb(0,0,255,255,maxColorValue=255), lwd=3)

axis(1, at=c(10, 100, 170, 240, 360, 432), labels=c("10 h", "100 h", "7 d","10 d", "14 d", "18 d"), cex.axis=0.8)
axis(2, cex.axis=0.8)

dev.off()

# lines(data$delayTolerance/3600, data$flow.0 + data$flow.1 + data$flow.2)
# 
# ## Weibull regression
# plot(delayTolerance, flow.0, axes=F, xlab='Delay Tolerance (hours)', ylab='Flow', col=rgb(0,100,0,50,maxColorValue=255), pch=16)
# model <- nls(flow.0 ~ SSweibull(delayTolerance, Asym, Drop, lrc, pwr))
# xv <- 10:479
# yv <- predict(model, list(delayTolerance=xv))
# lines(xv, yv)
# 
# ## Generalzed additive model
# plot(delayTolerance, flow.0, axes=F, xlab='Delay Tolerance (hours)', ylab='Data transferred (TB)', col=rgb(0,100,0,50,maxColorValue=255), pch=16)
# model <- gam(flow.0 ~ s(delayTolerance))
# xv <- 10:479
# yv <- predict(model, list(delayTolerance=xv))
# lines(xv, yv)
# 
# # Non-linear least squares
# model <- nls(flow.0 ~ a*(1-exp(-c*delayTolerance)), start=list(a=8000,c=0.001))
# 
# ## Link leakage
# data.ll <- read.table("results-linkleakage-150km-cutoff-12h-3-demand-delayTolerance-7d.txt", sep=",", header=T)
# 
# throughput.ll.0    <- data.ll$flow.0/(1e9 * data.ll$delayTolerance)
# throughput.ll.1    <- data.ll$flow.1/(1e9 * data.ll$delayTolerance)
# throughput.ll.2    <- data.ll$flow.2/(1e9 * data.ll$delayTolerance)
# 
# flow.ll.0 <- data.ll$flow.0 / 1e12
# flow.ll.1 <- data.ll$flow.1 / 1e12
# flow.ll.2 <- data.ll$flow.2 / 1e12
# 
# linkLeakage <- data.ll$linkLeakage
# 
# plot(linkLeakage, flow.ll.0, axes=F, xlab='Delay Tolerance (hours)', ylab='Flow', col=rgb(0,100,0,50,maxColorValue=255), pch=16)
# lines(linkLeakage, predict(lm(flow.ll.0 ~ linkLeakage)))
# # lines(lowess(data.ll$linkLeakage, flow.ll.0),col=rgb(0,100,0,255,maxColorValue=255), lwd=2)
# # lines(linkLeakage, predict(gam(flow.ll.0 ~ s(linkLeakage)), list(linkLeakage=linkLeakage)))
# # lines(linkLeakage, predict(nls(flow.ll.0 ~ SSweibull(linkLeakage, Asym, Drop, lrc, pwr)), list(linkLeakage=linkLeakage)))
# 
# points(linkLeakage, flow.ll.1, col=rgb(100,0,0,50,maxColorValue=255), pch=16)
# lines(linkLeakage, predict(lm(flow.ll.1 ~ linkLeakage)))
# # lines(lowess(linkLeakage, flow.ll.1),col=rgb(100,0,0,255,maxColorValue=255), lwd=2)
# # lines(linkLeakage, predict(nls(flow.ll.1 ~ SSweibull(linkLeakage, Asym, Drop, lrc, pwr)), list(linkLeakage=linkLeakage)))
# # lines(linkLeakage, predict(lm(flow.ll.1 ~ 1/exp(linkLeakage)), list(linkLeakage=linkLeakage)))
# 
# points(linkLeakage, flow.ll.2, col=rgb(0,0,100,50,maxColorValue=255), pch=16)
# lines(linkLeakage, predict(lm(flow.ll.2 ~ linkLeakage)))
# # lines(linkLeakage, predict(nls(flow.ll.2 ~ SSweibull(linkLeakage, Asym, Drop, lrc, pwr)), list(linkLeakage=linkLeakage)))
# # lines(lowess(data.ll$linkLeakage, flow.ll.2),col=rgb(0,0,100,255,maxColorValue=255), lwd=2)
# 
# axis(1)
# axis(2)