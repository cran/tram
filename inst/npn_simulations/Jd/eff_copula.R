
source("setup.R")
library("copula")

set.seed(2908)

load("data.rda")

run <- function(i) {
    print(args[i,])

    ret <- vector(mode = "list", length = 2)
    names(ret) <- c("Time", "Est")
    nc <- normalCopula(dim = args[i, "J"], dispstr = "un")

    ### only continuous data
    if (any(sapply(d[[i]], is.factor))) return(ret)

    ret[["Time"]] <- system.time(mm <- try(fitCopula(nc, data = pobs(d[[i]]), method = "mpl")))["user.self"]
    if (is.null(mm) || inherits(mm, "try-error")) {
        ret[["Time"]] <- NA
        return(ret)
    }
    ret[["Est"]] <- coef(mm)
    return(ret)
}

ret <- mclapply(1:nrow(args), run, mc.cores = MC)

save(ret, file = "ret_eff_copula.rda")
