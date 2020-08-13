pdf("Pathcount.pdf", width=4, height=3.5, family="Times")
par(mar=c(4, 4, 2, 0.5))

data <- read.table("pathcount.txt", sep=",", header=T)
plot(data$cutoff*6, data$road, axes=F, log="y", xlim=c(30,160), xlab='Borne supérieure sur le temps de trajet (minute)', ylab='Nombre de routes (échelle log)', ylim=c(2,30000), col=rgb(255,0,0,254,maxColorValue=255), pch=16)
points(data$cutoff*6, data$overlay, col=rgb(0,0,255,254,maxColorValue=255), pch=15)
legend("topleft", c("Réseau routier", "Réseau de délestage"), col=c(rgb(255,0,0,254,maxColorValue=255), rgb(0,0,255,254,maxColorValue=255)), pch=c(16,15), bty="n")

axis(1, cex.axis=0.8)
axis(2, cex.axis=0.8)
