# cabal2nix/doc/create-workspace.r

library(data.table)
library(rjson)
library(foreach)
library(doParallel)

cores <- detectCores()
options(cores=cores, mc.cores=cores)
cl <- makeForkCluster(cores)
registerDoParallel(cl)
setDefaultCluster(cl)

builds <- foreach (i=list.files(".", "*.json"), .combine="rbind") %dopar% {
    print(i)
    l <- fromJSON(file=i)
    pkgset <- names(l)
    data.table(pkgset, pkg=names(l[[pkgset]]), out=unlist(l[[pkgset]]))
}

t <- foreach (out=unique(builds$out), .combine="rbind") %dopar% {
    size=system(paste("( du -sb", out, "| cut -f1 ) 2>/dev/null || true"), intern=TRUE)
    size <- if (identical(size, character(0))) "" else size
    data.table(out, size=as.integer(size))
}

builds <- merge(builds, t, by="out")

nrow(builds)
length(unique(builds$out))

builds[,list(attributes=length(pkg)), by=pkgset]

t <- builds[pkgset=="ghc7102" & pkg != "ghc", size/1e6]
summary(t)
sd(t, na.rm=T)

t <- builds[pkg != "ghc" & !is.na(size)]
t$size <- t$size / 1e6
t[, list(n=length(size), median=mean(size), mean=mean(size), sd=mean(size)))
  , by=pkgset]
