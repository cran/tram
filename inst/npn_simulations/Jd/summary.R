
load("data.rda")
library("mvtnorm")
library("copula")
set.seed(290875)

N <- 10000
Z <- matrix(rnorm(N * max(args$J)), ncol = N)

args$Frob_L <- sapply(1:nrow(args), function(i) {
    sqrt(sum((as.array(attr(d[[i]], "L"))[,,1])^2))
})
args$Frob_R <- sapply(1:nrow(args), function(i) {
    sqrt(sum((as.array(invchol2cor(attr(d[[i]], "L")))[,,1])^2))
})
lltrue <- sapply(1:nrow(args), function(i) {
    Y <- solve(invcholD(attr(d[[i]], "L")), Z[1:args[i, "J"],,drop = FALSE])
    ldmvnorm(obs = Y, invchol = invcholD(attr(d[[i]], "L")))
})
args[, "lltrue"] <- lltrue

args$Est <- args$SD <- args$Time <- 0
vars <- c("FrobD_L", "FrobD_R", "Time", "ll", "lltrue")

method <- factor(c("P_P", "P_P_m", "P_P_s", "NP_P", "NP_NP", "copula", "mvord"))

tmp <- expand.grid(i = 1:nrow(args), method = method)

args <- args[tmp$i,]
args$method <- tmp$method


ev <- expression({
    tm <- sapply(ret, function(x) x$Time %||% NA)
    args[args$method == m, "Time"] <- unlist(tm)
    rmse <- sapply(1:length(ret), function(i) {
        if (is.null(ret[[i]]$Est)) return(NA)
        sqrt(sum((as.array(ret[[i]]$Est)[,,1] - 
                  as.array(attr(d[[i]], "L"))[,,1])^2))
    })
    args[args$method == m, "FrobD_L"] <- rmse
    rmse <- sapply(1:length(ret), function(i) {
        if (is.null(ret[[i]]$Est)) return(NA)
        R <- as.array(invchol2cor(ret[[i]]$Est))[,,1]
        RL <- as.array(invchol2cor(attr(d[[i]], "L")))[,,1]
        sqrt(sum((R - RL)^2))
    })
    args[args$method == m, "FrobD_R"] <- rmse
    ll <- sapply(1:length(ret), function(i) {
        if (is.null(ret[[i]]$Est)) return(NA)
        Y <- solve(invcholD(attr(d[[i]], "L")), Z[1:args[i, "J"],,drop = FALSE])
        ldmvnorm(obs = Y, invchol = invcholD(ret[[i]]$Est))
    })
    args[args$method == m, "ll"] <- ll
})

for (m in levels(method)) {
    if (m %in% c("mvord", "copula")) next()
    load(paste0("ret_eff_", m, ".rda"))
    eval(ev)
}

load("ret_eff_copula.rda")
tm <- sapply(ret, function(x) x$Time %||% NA)
args[args$method == "copula", "Time"] <- unlist(tm)
rmse <- sapply(1:length(ret), function(i) {
    if (is.null(ret[[i]]$Est)) return(NA)
    R <- getSigma(normalCopula(param = ret[[i]]$Est, dim = args[i, "J"], dis = "un"))
    RL <- as.array(invchol2cor(attr(d[[i]], "L")))[,,1]
    sqrt(sum((R - RL)^2))
})
args[args$method == "copula", "FrobD_R"] <- rmse
ll <- sapply(1:length(ret), function(i) {
    if (is.null(ret[[i]]$Est)) return(NA)
    Y <- solve(invcholD(attr(d[[i]], "L")), Z[1:args[i, "J"],,drop = FALSE])
    R <- getSigma(normalCopula(param = ret[[i]]$Est, dim = args[i, "J"], dis = "un"))
    sum(dmvnorm(x = t(Y), sigma = R, log = TRUE))
})
args[args$method == "copula", "ll"] <- ll

load("ret_eff_mvord.rda")
tm <- sapply(ret, function(x) x$Time %||% NA)
args[args$method == "mvord", "Time"] <- unlist(tm)
rmse <- sapply(1:length(ret), function(i) {
    if (is.null(ret[[i]]$Est)) return(NA)
    R <- ret[[i]]$Est
    RL <- as.array(invchol2cor(attr(d[[i]], "L")))[,,1]
    sqrt(sum((R - RL)^2))
})
args[args$method == "mvord", "FrobD_R"] <- rmse
ll <- sapply(1:length(ret), function(i) {
    if (is.null(ret[[i]]$Est)) return(NA)
    Y <- solve(invcholD(attr(d[[i]], "L")), Z[1:args[i, "J"],,drop = FALSE])
    R <- ret[[i]]$Est
    sum(dmvnorm(x = t(Y), sigma = R, log = TRUE))
})
args[args$method == "mvord", "ll"] <- ll


save(args, file = "summary.rda")
