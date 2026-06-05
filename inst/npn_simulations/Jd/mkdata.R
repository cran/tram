
set.seed(29)
source("../dgp.R")

Nsim <- 100

args <- expand.grid(N = c(10, 30, 50), J = c(3, 6, 9), nc = c(1, .5, 0), Nsim = 1:Nsim)
args$sim <- 1:nrow(args)
args$seed <- floor(runif(nrow(args)) * 10^6)
args$id <- 1:nrow(args)
d <- vector(mode = "list", length = nrow(args))

for (i in 1:nrow(args)) {
    nc <- floor(args[i, "J"] * args[i, "nc"])
    d[[i]] <- dgp(n = args[i, "N"], df = 2:(args[i, "J"]+1), J = args[i, "J"], 
                  ncat = rep(c(Inf, 4), c(nc, args[i, "J"] - nc)))
}

print(length(d))

save(args, d, file = "data.rda")
