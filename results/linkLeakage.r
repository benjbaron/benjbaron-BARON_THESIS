pdf("linkLeakage.pdf", width=4, height=3.5, family="Times")
par(mar=c(4, 4, 2, 0.5))
library("segmented")

options("scipen"=100, "digits"=1)
data.ll <- read.table("results-linkleakage-150km-cutoff-12h-3-demand-delayTolerance-8d-linkLeakage.txt", sep=",", header=T)

throughput.ll.0    <- data.ll$flow.0/(1e9 * data.ll$delayTolerance)
throughput.ll.1    <- data.ll$flow.1/(1e9 * data.ll$delayTolerance)
throughput.ll.2    <- data.ll$flow.2/(1e9 * data.ll$delayTolerance)

flow.ll.0 <- data.ll$flow.0 / (8*1e15)
flow.ll.1 <- data.ll$flow.1 / (8*1e15)
flow.ll.2 <- data.ll$flow.2 / (8*1e15)

linkLeakage <- data.ll$linkLeakage

plot(linkLeakage, flow.ll.0, axes=F, xlab='Logical link leakage', ylab='Amount of data transferred (TB)', col=rgb(0,255,0,100,maxColorValue=255), pch=15, log='x')
points(linkLeakage, flow.ll.1, col=rgb(255,0,0,100,maxColorValue=255), pch=16)
points(linkLeakage, flow.ll.2, col=rgb(0,0,255,100,maxColorValue=255), pch=17)

lines(linkLeakage, predict(lm(flow.ll.0 ~ linkLeakage)), col=rgb(0,255,0,255,maxColorValue=255), lwd=3)
lines(linkLeakage, predict(lm(flow.ll.1 ~ linkLeakage)), col=rgb(255,0,0,255,maxColorValue=255), lwd=3)
lines(linkLeakage, predict(lm(flow.ll.2 ~ linkLeakage)), col=rgb(0,0,255,255,maxColorValue=255), lwd=3)

axis(1, at=c(0.0001, 0.001, 0.01, 0.1, 1), labels=c("0.0001", "0.001", "0.01", "0.1", "1"), cex.axis=0.8)
axis(2, cex.axis=0.8)

dev.off()