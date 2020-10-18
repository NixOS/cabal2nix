# cabal2nix/doc/analyze-hackage.r

library(data.table)

hackage <- within(as.data.table(read.csv("hackage.csv", header=T, stringsAsFactors=F)), {
    package <- ordered(package)
})
setkey(hackage, package)

t <- hackage[,list(versions=length(version)),by=package][order(versions, decreasing=T)]

pdf("hackage-version-boxplot.pdf",width=8, height=1.25)
opar <- par(mai=c(0.4,0.1,0.1,0.1))
boxplot(t$versions, outline=T, horizontal=TRUE, log="x", range=0)
par(opar)
dev.off()
