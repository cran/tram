
source("setup.R")
library("tram")
source("../fit.R")

load("data.rda")

op <- mltoptim(trace = FALSE, 
               nloptr = list(algorithm = "NLOPT_LD_MMA", xtol_rel = 1.0e-4, 
                             maxeval = 1000L))[3:1]

run <- function(i) {
    print(args[i,])

    ret <- vector(mode = "list", length = 2)
    names(ret) <- c("Time", "Est")

    ret[["Time"]] <- system.time(mm <- try(fit(d[[i]], as.R.ordered = TRUE, se = FALSE, optim = op)))["user.self"]
    if (inherits(mm, "try-error")) {
        ret[["Time"]] <- NA
        return(ret)
    }

    ret[["Est"]] <- mm
    return(ret)
}

ret <- mclapply(1:nrow(args), run, mc.cores = MC)

save(ret, file = "ret_eff_NP_NP.rda")
