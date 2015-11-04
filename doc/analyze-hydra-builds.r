# cabal2nix/doc/analyze-hydra-builds.r
#
# Generate the input file "builds.csv" by running
#
#     sudo -u hydra psql -c "Copy (select b.project, b.jobset, b.job, b.timestamp, b.drvpath, b.system, b.buildstatus, b.size, b.closuresize, bs.type, bs.starttime, bs.stoptime, bs.machine from builds b left join buildsteps bs on b.id=bs.build and b.drvpath=bs.drvpath where finished=1 and iscachedbuild=0) to stdout With CSV HEADER;" >builds.csv
#
# on Hydra.

library(data.table)

builds <- within(as.data.table(read.csv("builds.csv", header=T, stringsAsFactors=T)), {
    timestamp <- as.POSIXct(timestamp, origin="1970-01-01")
    starttime <- as.POSIXct(starttime, origin="1970-01-01")
    stoptime <- as.POSIXct(stoptime, origin="1970-01-01")
    runtime <- difftime(stoptime, starttime, units="mins")
    machine <- sub("^(hydra|root)@", "", machine)
})
builds[machine=="", machine:="localhost"]
setkey(builds, project, jobset, job, timestamp)

## Determine the average build runtime of all successful Haskell builds that
## ran on "work".

hs <- builds[project=="nixpkgs" & jobset=="haskell-updates",]
hs <- hs[machine=="work" & buildstatus==0 & system == "x86_64-linux" & runtime > 0,]
hs <- hs[,job:=sub("\\.x86_64-linux$", "", job)]
hs <- hs[,job:=sub("^haskell(ng)?Packages\\.", "", job)]
hs <- hs[,job:=sub(".*git-?annex.*", "git-annex", job, ignore.case=T)]
hs <- hs[,job:=sub("idris_plain$", "idris", job)]
hs <- hs[!grep("haskell(-ng)?\\.compiler\\.", job),]
hs <- hs[!grep("^ghc\\.ghc", job),]
hs <- hs[!grep("ghc[0-9]+$", job),]
hs <- hs[!duplicated(hs$drvpath)]
hs <- hs[
        , list(builds=length(runtime),runtime=sum(runtime),runtime.median=median(runtime),runtime.mean=mean(runtime),runtime.sd=sd(runtime))
        , by=job
        ]
hs <- hs[builds>10,]
head(hs[order(runtime.mean, decreasing=T)], n=30)
head(hs[order(builds, decreasing=T)], n=20)
summary(as.numeric(hs$runtime.mean,units="mins"))
quantile(as.numeric(hs$runtime.mean,units="mins"), 0.82)

## Determine the number of builds per day.

hs <- builds[ project=="nixpkgs" & jobset=="haskell-updates" & starttime >= as.POSIXct("2015-01-01")
            , list(builds=length(timestamp))
            , by=list(day=as.Date(timestamp))
            ]
hs <- merge(hs, data.table(day=seq(min(hs$day), max(hs$day), by="days"), ignore=0), by="day", all=T)
hs[is.na(builds)] <- 0
hs$ignore <- NULL
summary(hs$builds)
quantile(hs$builds, .8)
