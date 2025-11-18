## ----survtram-pkgs, echo = FALSE, results = "hide", message = FALSE, warning = FALSE----
# required packages
pkgs <- c("mlt", "tram",  "trtf", "SparseGrid", "ATR", "tramME", "multcomp",
  "coin", "TH.data", "survival", "colorspace", "xtable", "english")
pkgs <- sapply(pkgs, require, character.only = TRUE)

## ----fail, results = "asis", echo = FALSE--------------------------------
if (any(!pkgs))
{
    cat(paste("Package(s)", paste(names(pkgs)[!pkgs], collapse = ", "), 
        "not available, stop processing.",
        "\\end{document}\n"))
    knitr::knit_exit()
}
if (!interactive() && .Platform$OS.type != "unix")
{
    cat(paste("Vignette only compiled under Unix alikes.",
        "\\end{document}\n"))
    knitr::knit_exit()
}

## ----setup, echo = FALSE, results = "hide", message = FALSE, warning = FALSE----
set.seed(290875)

## plotting
xlab <- "Time (in days)"
lxlab <- paste0(xlab, " on log-scale")
ylabS <- "Probability of survival"
ylablHaz <- "Log-cumulative hazard"
ylabcumHR <- expression(Lambda[1](t)/Lambda[0](t))
ylimS <- c(0, 1)
ylimHR <- c(0, 1.6)
q <- 0:2204
xlim <- c(0, max(q))
lwd <- 1.3

## color
acol <- sequential_hcl(6, "BluYl")[1:5]
col <- acol[c(2, (length(acol)) - 1)]
lcol <- lighten(col, amount = .4) ## lighten color for overlaid lines 

## aux
perm_test_biv.stram <-  function(object, seed = 1) {
  stopifnot(inherits(object, "stram"))
  fixed <- c(trt = 0, scl = 0)
  lhs <- object$call[[2]][[3]]
  if (!(length(lhs) == 3 & lhs[[2]] == lhs[[3]]))
    stop("Bivariate score perm test not applicable")
  names(fixed) <- names(coef(object))
  m0 <- update(object, fixed = fixed) ## uncond. model
  r <- resid(m0, what = "shifting")
  rs <- resid(m0, what = "scaling")
  set.seed(seed)
  
  formula <- as.formula(paste("r + rs ~", lhs[[2]]))
  pvalue(independence_test(formula, data = m0$data))
}

## formatting
big.mark <- "'"
frmt0 <- round
frmt <- function(digits, x, math = FALSE) {
  if (!is.numeric(x)) return(x)
    ret <- formatC(round(x, digits), digits = digits, format = "f", big.mark = big.mark) 
    if (math) ret <- paste("$", ret, "$")
    if (is.matrix(x)) {
        ret <- matrix(ret, nrow = nrow(x))
        dimnames(ret) <- dimnames(x)
    }
    ret
}

frmt1 <- function(x, math = FALSE) frmt(1, x = x, math = math)
frmt2 <- function(x, math = FALSE) frmt(2, x = x, math = math)
frmt3 <- function(x, math = FALSE) frmt(3, x = x, math = math)

## logLik
frmtll <- function(x, math = FALSE, mark = FALSE) {
  if (!inherits(x, "logLik") && !is.numeric(x) && all(!is.na(x))) x <- logLik(x)
    if (is.na(x)) return("")
  ret <- frmt2(abs(x), math = FALSE)
  if (x < 0) ret <- paste0(ifelse(math, "$-$", "-"), ret)
  if (mark) ret <- paste0("{\\color{darkgray}", ret, "}")
  ret
}

## data
load(system.file("rda", "Primary_endpoint_data.rda", package = "TH.data"))

## randomization arm
levs <- levels(CAOsurv$randarm)
trt <- with(CAOsurv, paste0("randarm", levs[2], collapse = ""))
nd1 <- data.frame(randarm = factor(levs, levels = levs))

## strata
CAOsurv$strat <- with(CAOsurv, interaction(strat_t, strat_n))
slevs <- levels(CAOsurv$strat)
nd2 <- data.frame(randarm = nd1$randarm[1], strat = factor(slevs, levels = slevs))

## for pretty legends
lslevs <- gsub("\\.", " : ", slevs)
lslevs <- gsub("cT4", "cT4    ", lslevs)

## id
CAOsurv$id <- factor(1:nrow(CAOsurv))

# ### lattice
# trellis.par.set(
#   list(
#     plot.symbol = list(col = 1, pch = 20, cex = 0.7),
#     box.rectangle = list(col = 1),
#     box.umbrella = list(lty = 1, col = 1),
#     strip.background = list(col = "white")
#   )
# )
# 
# ltheme <- canonical.theme(color = FALSE)     ## in-built B&W theme
# ltheme$strip.background$col <- "transparent" ## change strip bg
# lattice.options(default.theme = ltheme)

## knitr
library("knitr")
knitr::opts_chunk$set(results = "hide", echo = FALSE, purl = TRUE, 
  tidy = FALSE, size = "small", error = FALSE, warning = FALSE, message = FALSE,
  fig.scap = NA, fig.align = "center", fig.width = 6, fig.height = 3.3, 
  out.width = NULL, out.height = NULL, dev = c("pdf", "postscript"))
opts_knit$set(global.par = TRUE)
knitr::render_sweave()  # use Sweave environments
knitr::set_header(highlight = "")  # do not \usepackage{Sweave}
options(prompt = "R> ", continue = "+  ", useFancyQuotes = FALSE)  # JSS style
options(width = 75, digits = 4)

## format
frmtI <- function(x, math = FALSE) {
  if (is.character(x)) return(x)
  ret <- trimws(formatC(x, format = "fg", width = 7, big.mark = big.mark))
  if (x < 0) ret <- paste0(ifelse(math, "$-$", "-"), ret)
  if (math) ret <- paste("$", ret, "$")
  if (is.matrix(x)) {
    ret <- matrix(ret, nrow = nrow(x))
    dimnames(ret) <- dimnames(x)
  }
  ret
}
    
frmtN <- function(x, bound = 10, all = TRUE) { ## => all because of consistency
    ret <- round(x)
    if (all | x <= bound) return(as.character(english::english(ret)))
    ret
}

frmtd <- function(date) format(date, format = "%b~%-d")
frmtD <- function(date) format(date, format = "%B~%-d")

toUpper <- function(x) gsub("\\b([[:lower:]])([[:lower:]]+)", "\\U\\1\\L\\2",
                            x, perl = TRUE)

## print results
frmtp <- function(pv) paste("$p$ =", frmt3(pv))
frmtCI <- function(x, phantom = FALSE, math = FALSE) {
  if (!isTRUE(nrow(x) > 1)) phantom <- FALSE
  if (phantom) phantom <- any(x[,2] < 0) ## switch all to phantom if one is negative
  FUN <- function(x) {
     ciL <- frmt3(x[1], math = FALSE)
     ciL <- ifelse(math & phantom & ciL > 0,
       paste0("$\\phantom{-}", ciL, "$"), paste0("$", ciL, "$"))
     ciU <- frmt3(x[2], math = FALSE)
     ciU <- ifelse(math & phantom & ciU > 0,
       paste0("$\\phantom{-}", ciU, "$"), paste0("$", ciU, "$"))
    paste(ciL, "to", ciU)
  }
  if (is.matrix(x)) ret <- apply(x, 1, FUN)
  else ret <- FUN(x)
  return(ret)
}



## table with results from "tram"
ret.tram <- function(object, seed = 25, math = TRUE) {
mod0 <- object
if (inherits(object, "Survreg")) object <- as.mlt(object)

## Estimate
cf <- coef(object)
ibs <- grep("Bs", names(cf))
if (length(ibs) > 1) cf <- cf[-ibs]

if (inherits(object, "Compris")) cf <- cf[c(grep("^Event_EoI", names(cf), value = TRUE), "Event_DepC.Event_EoI.(Intercept)")]

if (inherits(object, "fmlt")) cf <- c(cf, coef(object, addparm = TRUE))

ncf <- names(cf)

## SE
if (inherits(object, "fmlt")) vc <- vcov(object, addparm = TRUE)
else vc <- vcov(object)
se <- sqrt(diag(vc))[ncf]

## CIs
if (inherits(object, "tram") & !inherits(object, "stram")) {
  st <- score_test(object)
  ci <- st$conf.int
  nci <- "95\\%-Score CI"
} else {
  ciWald <- function(cf, se) cf + c(-1, 1) * qnorm(1-0.05/2) * se
  ci <- t(sapply(1:length(cf), function(i) ciWald(cf[i], se[i])))
  nci <- "95\\%-Wald CI"
}

# ci <- confint(object)[ncf, ]

ncf <- sapply(ncf, function(x)
  switch(x, "(Intercept)" = "$\\eparm_1$", 
    "log(iDFS)" = "$\\eparm_2$",
    "randarm5-FU + Oxaliplatin" = "$\\eshiftparm$",
    "Event_EoI.Event_EoI.randarm5-FU + Oxaliplatin" = "$\\eshiftparm_\\rY$",
    "scl_randarm5-FU + Oxaliplatin" = "$\\escaleparm$", 
    "gamma1" = "$\\mparm$", 
    "Event_DepC.Event_EoI.(Intercept)" = "$\\lparm$", 
    "logrho" = "$\\log(\\lparm)$"))

ret.cfs <- cbind("Coefficient" = ncf, "Estimate" = frmt3(cf, math = math),
  "Std. Error" = frmt3(se), "95\\%-CI" = frmtCI(ci, phantom = TRUE, math = math))
colnames(ret.cfs)[which(colnames(ret.cfs) == "95\\%-CI")] <- nci

if (inherits(object, "tramME")) ret.cfs <- rbind(ret.cfs, c("$\\tau^2$", frmt3(VarCorr(object)[[1]]$var), "", ""))

ll <- logLik(object)

object <- mod0

if (inherits(object, "tram")) {
p.lrt <- summary(object)$p.value

if (inherits(object, "stram")) {
  p.pst <- perm_test_biv.stram(object)
  ret.mod <- cbind("Bivariate Permutation Score Test" = frmtp(p.pst), " " = NA)
} else {
  if (inherits(mod0, "Survreg")) st <- score_test(mod0)
  p.st <- st$p.value
  p.pt <- (pt <- perm_test(object))$p.value
  ret.mod <- cbind("Score Test" = frmtp(p.st),"Permutation Score Test" = frmtp(p.pt))
}
ret.mod <- cbind(cbind("Log-Likelihood" = frmtll(ll, math = math),
  "Likelihood Ratio Test" = frmtp(p.lrt)), ret.mod)
}

if (inherits(object, c("tramME", "mtram", "fmlt", "mmlt")))
  ret.mod <- cbind("Log-Likelihood" = frmtll(ll, math = math), " " = NA, " " = NA, " " = NA)

ret.mod <- rbind(colnames(ret.mod), ret.mod)
ns <- colnames(ret.cfs)
ret <- rbind(ret.cfs, ret.mod)
colnames(ret) <- ns
return(ret)
}

print.tram <- function(object) {
  ret <- ret.tram(object)
  cat("\\begin{center}")
  cat("\n")
  # cat("\\small")
  cat("\n")
  print(xtable(ret, align = "rrrrr"), 
  add.to.row = list(pos = list(nrow(ret) - 2, nrow(ret) - 1, nrow(ret)),
    command = c("\\\\[-1.2ex] \\toprule ", "\\midrule ", "\\\\[-2ex] ")),
  booktabs = TRUE, floating = FALSE, sanitize.text.function = function(x){x}, 
  include.rownames = FALSE, scale = .8)
cat("\n")
cat("\\end{center}")
}

## Formatting
is.neg <- function(x) x < 0

lHR <- function(model, frmt = function(x) {frmt3(x, math = FALSE)}) {
stopifnot(inherits(model, c("tram", "mlt", "tramME")))
cf <- coef(model)[trt]
if (inherits(model, "tram")) cf <- c(1, -1)[model$negative + 1] * cf
frmt(cf)
}

HR <- function(model,  frmt = function(x) {frmt3(x, math = FALSE)})
  frmt(exp(lHR(model, frmt = function(x) {x})))



## ----pars, include = FALSE-----------------------------------------------
par_main <- expression(par(mgp = c(2.5, 1, 0), mar = c(4, 4, 1.5, 4), las = 1))
par_surv <- expression(par(mgp = c(2.5, 1, 0), mar = c(6, 6, .5, 4), las = 1))

## ----packages, echo = FALSE----------------------------------------------
library("tram")

## ----risk-tab------------------------------------------------------------
risktab <- function(ti, st) { ## time-index and survival time
  nrisk <- NULL
  for (t in ti) nrisk <- c(nrisk, sum(st >= t))
  return(nrisk)
}

plot.risktab <- function(tvar, ti = seq(min(q), max(q), by = 500),
  cex = .8, at = -450) {
mtext(levs[1], 1, line = 4, at = at, cex = cex)
mtext(risktab(ti, CAOsurv[CAOsurv$randarm == levs[1], tvar]),
  side = 1, line = 4, at = ti, cex = cex)
mtext(levs[2], 1, line = 5, at = at, cex = cex)
mtext(risktab(ti, CAOsurv[CAOsurv$randarm == levs[2], tvar]),
  side = 1, line = 5, at = ti, cex = cex)
}

## ----surv-OS-------------------------------------------------------------
surv_OS <- survfit(OS ~ randarm, data = CAOsurv) ## KM

## ----surv-iDFS-----------------------------------------------------------
surv_iDFS <- survfit(iDFS ~ randarm, data = CAOsurv) ## Turnbull

## ----CAO-table, results = 'hide'-----------------------------------------
tab <- xtabs( ~ strat + randarm, data = CAOsurv)
tab <- rbind(tab, "Total" = colSums(tab))

## ----surv-iDFS-plot------------------------------------------------------
eval(par_surv)
plot(surv_iDFS, ylim = ylimS, xlim = xlim,
  col = lcol, lwd = lwd, xlab = xlab, ylab = ylabS)
legend("bottomright", legend = levs, col = col, bty = "n", lty = 1, lwd = 1, cex = .8)
plot.risktab(tvar = "iDFStime")

## ----surv-OS-plot--------------------------------------------------------
plot(surv_OS, ylim = ylimS, xlim = xlim,
  col = lcol, lwd = lwd, xlab = xlab, ylab = ylabS)
legend("bottomright", legend = levs, col = col, bty = "n", lty = 1, lwd = 1, cex = .8)
plot.risktab(tvar = "OStime")

## ----WEI-model-fit, echo = FALSE, cache = TRUE---------------------------
mw <- 
Survreg(iDFS ~ randarm, data = CAOsurv, dist = "weibull")

## ----WEI-summary, cache = TRUE, results = "hide", fig.show = 'hide'------
summary(mw)
coef(mw, as.survreg = TRUE) ## same interpretation as "survreg"
score_test(mw)
perm_test(mw)
# plot(as.mlt(mw), type = "survivor", newdata = nd1, col = col)

## ----COX-model-fit, echo = FALSE, cache = TRUE---------------------------
mc <-
Coxph(iDFS ~ randarm, data = CAOsurv)

## ----COX-summary, cache = TRUE, results = "hide", fig.show = "hide"------
summary(mc)
score_test(mc)
perm_test(mc)
# plot(as.mlt(mc), type = "survivor", newdata = nd1, col = col)

## ----COX-lHaz-try--------------------------------------------------------
cb <- confband(as.mlt(mc), newdata =  nd1[1,, drop = FALSE], K = 20, cheat = 100)

## ----COX-lHaz-plot-------------------------------------------------------
eval(par_main)
cb <- cb[cb[,"q"] > 0,] ### remove time = 0
plot(cb[, "q"], cb[, "Estimate"], log = "x", type = "n",
  xlab = lxlab, ylab = ylablHaz, xlim = xlimlHaz <- range(cb[, "q"]),
  ylim = range(cb[, -1]))

polygon(c(cb[, "q"], rev(cb[, "q"])), c(cb[, "lwr"], rev(cb[, "upr"])),
  border = NA, col = rgb(.1, .1, .1, .1))
lines(cb[, "q"], cb[, "Estimate"], lwd = lwd)

## ----fastopt-------------------------------------------------------------
fastopt <- mltoptim(abstol = 1e-3, reltol = 1e-3)
fastoptH <- mltoptim(abstol = 1e-3, reltol = 1e-3, hessian = TRUE)

## ----STRAT-model-fit, cache = TRUE---------------------------------------
mcst <- 
Coxph(iDFS | strat ~ randarm, data = CAOsurv, optim = fastopt)

## ----STRAT-summary, cache = TRUE, results = "hide", fig.show = "hide"----
summary(mcst)
score_test(mcst)
perm_test(mcst)

## ----STRAT-lHaz-plot-----------------------------------------------------
plot(as.mlt(mcst), newdata = nd2, q = q[q > 0], type = "logcumhazard", log = "x",
  lty = lty <- 1:4, xlab = lxlab, ylab = ylablHaz, xlim = xlimlHaz,
  col = 1, lwd = lwd)

legend("bottomright", legend = lslevs, title = "Stratum", 
  lty = lty, lwd = lwd, col = 1, bty = "n")

## ----SCOX-model-fit, cache = TRUE----------------------------------------
mcs <- 
Coxph(iDFS ~ randarm | randarm, data = CAOsurv)

## ----SCOX-summary, cache = TRUE, results = "hide", fig.show = "hide"-----
summary(mcs)
confint(mcs)
perm_test_biv.stram(mcs)
# plot(as.mlt(mcs), type = "survivor", newdata = nd1, col = col)

## ----SCOX-survplot, fig.show = "hide"------------------------------------
eval(par_surv)
plot(surv_iDFS, ylim = ylimS, xlim = xlim,
  col = lcol, lwd = lwd, xlab = xlab, ylab = ylabS)
legend("bottomright", legend = levs, col = col, bty = "n", lty = 1, lwd = 1, cex = .8)
plot.risktab(tvar = "iDFStime")
plot(as.mlt(mcs), type = "survivor", newdata = nd1, col = col, add = TRUE)

## ----SCOX-HR-plot, echo = FALSE------------------------------------------
qHR <- seq(50, max(q), by = 1)
cumhaz <- predict(mcs, type = "cumhazard", newdata = nd1, q = qHR)
cumhr <- unname(cumhaz[, 2] / cumhaz[, 1])
plot(qHR, cumhr, type = "l", ylab = ylabcumHR, xlab = xlab,
  ylim = ylimHR, xlim = xlimHR <- range(qHR), lwd = lwd)

abline(h = exp(coef(mc)), lty = 2, lwd = 1) ## constant HR
abline(h = 1, lty = 3) ## HR = 1

## ----TCOX-model-fit, cache = TRUE----------------------------------------
mcv <- 
Coxph(iDFS | randarm ~ 1, data = CAOsurv)

## ----TCOX-summary, cache = TRUE, results = "hide", fig.show = 'hide'-----
logLik(mcv)

## ----TCOX-HR, cache = TRUE-----------------------------------------------
mcv <- as.mlt(mcv)

## grid (was n = 500, qmvnorm failed)
s <- mkgrid(mcv, 39)
s$iDFS <- s$iDFS[s$iDFS >= min(xlimHR) & s$iDFS <= max(xlimHR)]
nd3 <- expand.grid(s)

## confint
K <- model.matrix(mcv$model, data = nd3)
Kyes <- K[nd3$randarm == levels(nd3$randarm)[2],]
Kyes[,grep("Intercept", colnames(Kyes))] <- 0  
gh <- glht(parm(coef(mcv), vcov(mcv)), Kyes)
ci <- exp(confint(gh)$confint)
coxy <- s$iDFS

## confint for constant HR
ci2 <- exp(confint(mc))

## ----TCOX-HR-plot--------------------------------------------------------
plot(coxy, ci[, "Estimate"], ylim = ylimHR, type = "n",
  xlim = xlimHR, xlab = xlab, ylab = ylabcumHR)
polygon(c(coxy, rev(coxy)), c(ci[,"lwr"], rev(ci[, "upr"])),
        border = NA, col = rgb(.1, .1, .1, .1))
lines(coxy, ci[, "Estimate"], lty = 1, lwd = lwd)

## constant HR
polygon(c(coxy[c(1, length(coxy))], rev(coxy[c(1, length(coxy))])),
  rep(ci2, c(2, 2)), border = NA, col = rgb(.1, .1, .1, .1))
abline(h = exp(coef(mc)), lty = 2, lwd = 1)

## HR = 1
abline(h = 1, lty = 3)

## ----DEPCENS-preproc, echo = FALSE---------------------------------------
## DepC: loss of follow-up (everyone else is admin censored) Mail TH 23-06-12
patnr_lofu <-c(1012, 2003, 3002, 3003, 6018, 7001, 7003, 7005, 7008, 7012, 10003,
              10012, 11018, 12003, 12014, 13028, 14002, 15001, 16001, 16004, 16005,
              16007, 16009, 18016, 18025, 21011, 21013, 21014, 21022, 21023, 21026,
              21027, 21029, 21043, 22003, 23008, 24008, 24021, 25001, 25004, 25005,
              25006, 26005, 26018, 27005, 27030, 27034, 29002, 30006, 30011, 31003,
              31004, 31005, 34001, 35011, 35014, 36017, 41004, 42001, 42003, 42005,
              42007, 42010, 44004, 44005, 45002, 45003, 45009, 45011, 46003, 49001,
              49003, 49011, 49012, 49015, 50001, 50003, 50004, 50007, 50011, 52004,
              54004, 56006, 56008, 59002, 59005, 68001, 70010, 71002, 73009, 74004,
              75002, 75004, 75005, 80003, 81001, 84005, 84007, 86002) 
ilofu <- with(CAOsurv, which(patnr %in% patnr_lofu))
CAOsurv$DepCevent <- CAOsurv$OSevent
CAOsurv$DepCevent <- factor(as.numeric(CAOsurv$DepCevent), levels = 0:2,
  labels = c("AdminC", "EoI", "DepC"))
CAOsurv$DepCevent[ilofu] <- "DepC"

## ----DEPCENS-table, results = 'hide'-------------------------------------
CAOsurv$nDepCevent <- factor(as.character(CAOsurv$DepCevent),
  levels = c("AdminC", "EoI", "DepC"), 
  labels = c("Administrative censoring", "Event of interest", "Loss of follow-up"))
tab <- xtabs(~ nDepCevent + randarm, data = CAOsurv)
tab

## ----DEPCENS-model-fit, cache = TRUE-------------------------------------
md <- 
Coxph(Surv(OStime, event = DepCevent) ~ randarm, data = CAOsurv, 
      optim = fastoptH)

## ----DEPCENS-summary, cache = TRUE, results = "hide", fig.show = 'hide'----
summary(md)
confint(md)

## ----COXME-install, echo = TRUE, eval = FALSE----------------------------
# install.packages("tramME")
# library("tramME")

## ----COXME-load----------------------------------------------------------
library("tramME")

## ----COXME-model-fit, cache = TRUE---------------------------------------
mcME <- 
CoxphME(iDFS ~ randarm + (1 | Block), data = CAOsurv)

## ----COXME-summary, cache = TRUE, results = "hide", fig.show = 'hide'----
summary(mcME)
confint(mcME)

## ----COXME-margsurv, eval = FALSE----------------------------------------
# ## computationally intensive
# if (!file.exists("ME-margdist.rda")) {
# mod <- mcME
# 
# ## A function to evaluate the joint cdf of the response and the random effects:
# ## Takes a vector of random effect and covariates values, evaluates the conditional
# ## distribution at these values and multiplies it with the pdf of the random effects
# jointCDF <- function(re, nd, mod) {
# nd <- nd[rep(1, length(re)), ]
# nd$Block <- seq(nrow(nd)) ## to take vector-valued REs
# pr <- predict(mod, newdata = nd, ranef = re, type = "distribution") *
# dnorm(re, 0, sd = sqrt(varcov(mod)[[1]][1, 1]))
# c(pr)
# }
# ## Marginalize the joint cdf by integrating out the random effects
# ## using adaptive quadrature
# marginalCDF <- function(nd, mod) {
# nd$cdf <- integrate(jointCDF, lower = -Inf, upper = Inf, nd = nd, mod = mod)$value
# nd
# }
# ## Set up the grid on which we evaluate the marginal distribution
# nd <- expand.grid(iDFS = 1:max(CAOsurv$DFStime), randarm = unique(CAOsurv$randarm))
# ## Calls marginalCDF on each row of nd
# ## (done in parallel to speed up computations)
# mp <- parallel::mclapply(split(nd, seq(nrow(nd))),
#   marginalCDF, mod = mod, mc.cores = 4)
# mp <- do.call("rbind", mp)
# save(mp, file = "ME-margdist.rda")
# } else load("ME-margdist.rda")
# mp$surv <- with(mp, 1 - cdf)
# 
# <<plot-surv-iDFS, eval = FALSE>>
# with(mp[mp$randarm == levs[1], ], lines(iDFS, surv, col = col[1], lwd = lwd))
# with(mp[mp$randarm == levs[2], ], lines(iDFS, surv, col = col[2], lwd = lwd))
# legend("bottomright", legend = levs, col = col, bty = "n", lty = 1, lwd = 1, cex = .8)

## ----MCOX-preproc, echo = FALSE------------------------------------------
### convert "exact" event dates to interval-censoring (+/- two days)
tmp <- CAOsurv$iDFS
exact <- tmp[, 3] == 1
tmp[exact, 2] <- tmp[exact, 1] + 2
tmp[exact, 1] <- pmax(tmp[exact, 1] - 2, 0)
tmp[exact, 3] <- 3
CAOsurv$iDFS2 <- tmp

## ----MCOX-model-fit, cache = TRUE----------------------------------------
mmc <- 
mtram(Coxph(iDFS2 ~ randarm, data = CAOsurv),
  formula = ~ (1 | Block), data = CAOsurv, optim = fastoptH)

## ----MCOX-FUN------------------------------------------------------------
## marginal HR from "mtram"
## <FIXME> reset seed on.exit </FIXME>
mHR.mtram <- function(object, with_confint = FALSE, seed = 1) {
  stopifnot(inherits(object, "mtram"))
  cf <- coef(object)
  cf <- cf[-grep("Bs", names(cf))]
  stopifnot(length(cf) == 2)
  mlHR <- cf[1] / sqrt(1 + cf["gamma1"]^2)
  ret <- mHR <- exp(mlHR)
  if (with_confint) {
    set.seed(seed)
    S <- vcov(object)
    rbeta <- rmvnorm(10000, mean = coef(object), sigma = S)
    s <- rbeta[,ncol(rbeta)]
    rbeta <- rbeta[,-ncol(rbeta)] / sqrt(s^2 + 1)
    ci <- quantile(exp(rbeta[, ncol(rbeta)]), prob = c(.025, .975))
    ret <- c(mHR, ci)
    ret <- as.array(t(ret))
  }
  return(ret)
}

## ----MCOX-summary, cache = TRUE, results = "hide", fig.show = 'hide'-----
coef(mmc)
sqrt(diag(vcov(mmc)))
(ci_MCOX <- mHR.mtram(mmc, with_confint = TRUE))

## ----HTECOX-model-fit, cache = TRUE--------------------------------------
ma <- 
CoxphME(iDFS ~ randarm + s(age, by = as.ordered(randarm),
    fx = TRUE, k = 6), data = CAOsurv)
nd <- model.frame(ma)[rep(2, 100), ]
nd$age <- seq(min(CAOsurv$age), max(CAOsurv$age), length.out = 100)
xx <- model.matrix(ma, data = nd, type = "X", keep_sign = FALSE)$X
ip <- grep("randarm", names(bb <- coef(ma, with_baseline = TRUE)))
vc <- vcov(ma, parm = names(bb)[ip])
bb <- bb[ip]

## NOTE: unadjusted
cb <- exp(confint(multcomp::glht(multcomp::parm(bb, vc), linfct = xx),
                  calpha = univariate_calpha())$confint)

## ----HTECOX-summary, cache = TRUE, results = "hide", fig.show = 'hide'----
summary(ma)

## ----HTECOX-HR-plot------------------------------------------------------
## Plot HR
plot(nd$age, cb[, "Estimate"], type = "n", ylab = "Hazard ratio", xlab = "Age (in years)",
     ylim = ylimHR)
polygon(c(nd$age, rev(nd$age)), c(cb[, "lwr"], rev(cb[, "upr"])),
        border = NA, col = rgb(.1, .1, .1, .1))
lines(nd$age, cb[, "Estimate"], lwd = lwd)
abline(h = 1, lty = 3)
rug(CAOsurv$age, lwd = 2, col = rgb(.1, .1, .1, .1))

## ----TRT-load-install, echo = TRUE, eval = FALSE-------------------------
# install.packages("trtf")
# library("trtf")

## ----TRT-load------------------------------------------------------------
library("trtf")
set.seed(4)

## ----TRTF-model-fit, cache = TRUE, warning = FALSE-----------------------
tr <-
trafotree(Coxph(iDFS ~ randarm, data = CAOsurv),
  formula = iDFS ~ randarm | age, data = CAOsurv,
  control = ctree_control(teststat = "maximum", alpha = .1,
    minbucket = 40))

## ----TRT-results, results = "hide", fig.show = 'hide'--------------------
logLik(tr)

## ----TRTF-surv-plot, fig.width = 10, fig.height = 6----------------------
library("ATR")
plot(rotate(tr), tp_args = list(newdata = nd1, type = "survivor", col = col, lwd = lwd),
  terminal_panel = trtf:::node_mlt)

## ----FRAILTY-model-fit, cache = TRUE-------------------------------------
mf <- 
Coxph(iDFS ~ randarm, data = CAOsurv, frailty = "Gamma")

## ----FRAILTY-summary, cache = TRUE, results = "hide", fig.show = 'hide'----
logLik(mf)
coef(mf)[trt]
coef(mf, addparm = TRUE)
confint(mf, parm = c(trt, "logrho"))

## ----LOGIT-model-fit-----------------------------------------------------
ml <- 
Colr(iDFS ~ randarm, data = CAOsurv)

## ----SS, echo = TRUE-----------------------------------------------------
m <- as.mlt(Survreg(OS ~ randarm + age, data = CAOsurv, 
  dist = "weibull", support = c(.1, 80 * 365)))

## ----SS-nd---------------------------------------------------------------
N <- 500
nd <- with(CAOsurv, data.frame(randarm = gl(2, N, labels = levels(randarm)),
  age = rnorm(N, mean = mean(age), sd = sd(age))))

## ----SS-setup, echo = FALSE----------------------------------------------
`coef<-` <- mlt::`coef<-` ## masked by tramME?

## ----SS-lOR, echo = TRUE-------------------------------------------------
cf <- coef(m)
cf["randarm5-FU + Oxaliplatin"] <- .25
coef(m) <- cf
nd$T <- as.Surv(simulate(m, newdata = nd, K = 1000))

## ----SS-simC, echo = TRUE------------------------------------------------
cf["(Intercept)"] <- cf["(Intercept)"] + qlogis(.8)
coef(m) <- cf
nd$C <- as.Surv(simulate(m, newdata = nd, K = 1000))

## ----SS-OS, echo = TRUE, results = "asis"--------------------------------
nd$nOS <- with(nd, Surv(time = pmin(T[, "time"], C[, "time"]),
  event = T[,"time"] < C[,"time"]))

## ----SS-refit, eval = FALSE, results = "hide", fig.show = "hide"---------
# table(nd$nOS[,"status"])
# levels(nd$randarm)[2] <- "innovative"
# plot(survfit(nOS ~ randarm, data = nd), col = col, xlab = "Time (in days)",
#   ylab = "Probability of survival")
# legend("topright", legend = levels(nd$randarm), col = col, lty = 1, bty = "n")
# Survreg(nOS ~ randarm + age, data = nd, dist = "weibull")

