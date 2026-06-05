
### produce summary.rda for Appendix C

load("data.rda")
library("mvtnorm")
library("copula")
set.seed(290875)

method <- factor(c("NP_NP", "mvord"))

i <- which(args$nc == 0)
tmp <- expand.grid(i = i, method = method)

args <- args[tmp$i,]
args$method <- tmp$method

ev <- expression({
    tm <- sapply(ret[i], function(x) x$Time %||% NA)
    args[args$method == m, "Time"] <- unlist(tm)
    rmse <- sapply(i, function(i) {
        if (is.null(ret[[i]]$Est)) return(NA)
        R <- as.array(invchol2cor(ret[[i]]$Est))[,,1]
        RL <- as.array(invchol2cor(attr(d[[i]], "L")))[,,1]
        sqrt(sum((R[lower.tri(R)] - RL[lower.tri(RL)])^2))
    })
    args[args$method == m, "FrobD_R"] <- rmse
})

for (m in levels(method)) {
    if (m %in% c("mvord", "copula")) next()
    load(paste0("ret_eff_", m, ".rda"))
    eval(ev)
}

load("ret_eff_mvord.rda")
tm <- sapply(ret[i], function(x) x$Time %||% NA)
args[args$method == "mvord", "Time"] <- unlist(tm)
rmse <- sapply(i, function(i) {
    if (is.null(ret[[i]]$Est)) return(NA)
    R <- ret[[i]]$Est
    RL <- as.array(invchol2cor(attr(d[[i]], "L")))[,,1]
    sqrt(sum((R[lower.tri(R)] - RL[lower.tri(RL)])^2))
})
args[args$method == "mvord", "FrobD_R"] <- rmse

v <- c("N", "J")#, "ncat")
args[,v] <- lapply(v, function(var) {
    lev <- sort(unique(args[[var]]))
    if (var == "J") lev <- rev(lev)
    lab <- paste(var, lev, sep = "=")
    factor(args[[var]], levels = lev, labels = lab)
})

args$method <- factor(as.character(args$method), levels = c("mvord", "NP_NP"), labels = c("cML", "NPN"))

save(args, file = "summary.rda")
