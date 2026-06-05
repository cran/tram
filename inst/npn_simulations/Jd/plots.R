
library("lattice")

load("summary.rda")

v <- c("N", "J", "nc")
args[,v] <- lapply(v, function(var) {
    lev <- sort(unique(args[[var]]))
    lab <- paste(var, lev, sep = ":")
    factor(args[[var]], levels = lev, labels = lab)
})

#failed <- is.na(args$FrobD_R)
#xtabs(~ method + N + J + nc, data = args[failed,,drop = FALSE])

#args <- subset(args, # method != "copula" & 
#                     ll > -1e6)
#args$method <- args$method[, drop = TRUE]

pdf("figures.pdf", width = 10, height = 10)

bwplot(I(ll - lltrue) ~ method | N + J + nc, data = args, scales = list(y = list(relation = "free")))

bwplot(I(FrobD_R / Frob_R) ~ method | N + J + nc, data = args, scales = list(y = list(relation = "free")))

bwplot(I(log(Time)) ~ method | N + J + nc, data = args, scales = list(y = list(relation = "free")))

bwplot(I(FrobD_L / Frob_L) ~ method | N + J + nc, data = args, subset = method !=
"copula", scales = list(y = list(relation = "free")))

dev.off()
