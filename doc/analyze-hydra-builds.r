# cabal2nix/doc/analyze-hydra-builds.r
#
# Generate the input file "builds.csv" by running
#
#     sudo -u hydra psql -c "Copy (select * from Builds) to stdout With CSV HEADER;" >builds.csv
#
# on Hydra.

library(data.table)

builds <- within(as.data.table(read.csv("builds.csv", header=T, stringsAsFactors=F)), {
    timestamp <- as.POSIXct(timestamp, origin="1970-01-01")
    starttime <- as.POSIXct(starttime, origin="1970-01-01")
    stoptime <- as.POSIXct(stoptime, origin="1970-01-01")
    runtime <- difftime(stoptime, starttime, units="secs")
    finished <- finished == 1
    project <- factor(project)
    jobset <- factor(jobset)
    system <- factor(system)
    license <- factor(license)
    iscurrent <- iscurrent == 1
    iscachedbuild <- iscachedbuild == 1
    keep <- keep == 1
    ischannel <- ischannel == 1
    name <- sub("-[0-9].*$", "", sub("^/nix/store/[^-]+-(.*).*\\.drv$", "\\1", drvpath))
})
setkey(builds, id, project, jobset, job, timestamp)

hs <- builds[ project=="nixpkgs" & jobset %in% c("haskell-updates", "stackage")
            | project %in% c( "cabal2nix", "funcmp", "hackage-db", "hledger", "hsdns"
                            , "hspec", "hsyslog", "language-nix", "nix-paths"
                            )
            ,
            ]
hs <- hs[!duplicated(hs$drvpath)]
hs <- hs[finished==TRUE,]
hs <- hs[runtime > 0,]
hs <- hs[!grep("haskell(-ng)?\\.compiler\\.", job),]
hs <- hs[!grep("\\.ghc(Plain)?$.", job),]
hs <- hs[!grep("^ghc\\.ghc", job),]
hs <- hs[ starttime >= as.POSIXct("2015-01-01")
        , list(builds=length(id),runtime.median=median(runtime),runtime.mean=mean(runtime),runtime.sd=sd(runtime))
        , by=job
        ]
hs <- hs[builds>10,]

head(hs[order(runtime.median, decreasing=T)], n=20)

head(hs[order(builds, decreasing=T)], n=20)
