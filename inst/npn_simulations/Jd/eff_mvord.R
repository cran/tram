
source("setup.R")
library("mvord")

load("data.rda")

run <- function(i) {
    print(args[i,])

    ret <- vector(mode = "list", length = 2)
    names(ret) <- c("Time", "Est")

    tmp <- d[[i]]
    dJ <- sum(sapply(d[[i]], is.factor))
    if (dJ < ncol(tmp)) return(ret)

    fm <- as.formula(paste("MMO2(", paste0("Y", 1:ncol(tmp), collapse = ","), ") ~ 1"))
    ret[["Time"]] <- system.time(mm <- try(mvord(fm, data = tmp, 
                                 control = mvord.control(solver="nlminb"))))["user.self"]
    if (is.null(mm) || inherits(mm, "try-error")) {
        ret[["Time"]] <- NA
        return(ret)
    } else {
        ret[["Est"]] <- error_structure(mm)[[1]]
        return(ret)
    }
}

ret <- mclapply(1:nrow(args), run, mc.cores = MC)

save(ret, file = "ret_eff_mvord.rda")
