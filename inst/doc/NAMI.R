## ----nami-pkgs, echo = FALSE, results = "hide", message = FALSE, warning = FALSE----
pkgs <- c("tram", "TH.data", "multcomp", "survival", "Stat2Data", "tramME")
pkgs <- sapply(pkgs, require, character.only = TRUE)

## ----nami-citation, echo = FALSE----------------------------------------------
year <- substr(packageDescription("tram")$Date, 1, 4)
version <- packageDescription("tram")$Version

## ----fail, results = "asis", echo = FALSE-------------------------------------
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

## ----nami-setup, echo = FALSE, results = "hide", message = FALSE, warning = FALSE----
knitr::opts_chunk$set(echo = TRUE, results = 'markup', error = FALSE,
                      warning = FALSE, message = FALSE,
                      tidy = FALSE, cache = FALSE, size = "small",
                      fig.width = 6, fig.height = 4, fig.align = "center",
                      out.width = NULL, ###'.6\\linewidth', 
                      out.height = NULL,
                      fig.scap = NA)
knitr::render_sweave()  # use Sweave environments
knitr::set_header(highlight = '')  # do not \usepackage{Sweave}
## R settings
options(prompt = "R> ", continue = "+  ", useFancyQuotes = FALSE)  # JSS style
options(width = 75)

frmt1 <- function(x, digits = 1) {
    if (!is.numeric(x)) return(x)
    formatC(round(x, digits = digits), digits = digits, format = "f") 
}
frmt3 <- function(x) 
    frmt1(x, digits = 3)
frmtci <- function(x, digits = 3) {
    if (!is.numeric(x)) return(x)
    if (length(x) != 2) stop("not a confidence interval")
    return(paste("(", frmt1(x[1], digits = digits), 
                 ",", frmt1(x[2], digits = digits), ")"))
}

## discrete nonparanormal likelihood relies on Monte Carlo, set seed
set.seed(221224L)

## ----nami-demo, eval = FALSE---------------------------------------------
# install.packages("tram")
# demo("NAMI", package = "tram")

## ----immun, echo = FALSE, message = FALSE, results = "hide"--------------
immun <- structure(list(y = c(21.699999999999999, 23.899999999999999, 
22.699999999999999, 23.399999999999999, 26.800000000000001, 24.800000000000001, 
23.399999999999999, 25.100000000000001, 24.100000000000001, 23.300000000000001, 
25.399999999999999, 25.100000000000001, 23.800000000000001, 23.100000000000001, 
24, 24.199999999999999, 27.399999999999999, 23.300000000000001, 
22.600000000000001, 23.300000000000001, 24.800000000000001, 23.899999999999999, 
22.199999999999999, 21.399999999999999, 22.800000000000001, 22.300000000000001, 
22.399999999999999, 30.399999999999999, 30.600000000000001, 21.699999999999999, 
24.800000000000001, 25.600000000000001, 21.399999999999999, 24.300000000000001, 
23.5, 25.800000000000001, 21.600000000000001, 22.899999999999999, 
23.800000000000001, 22.600000000000001, 24.199999999999999, 24.300000000000001, 
25.699999999999999, 23.199999999999999, 24.600000000000001, 24.5, 
22.699999999999999, 26.300000000000001, 27.199999999999999, 27.100000000000001, 
22.699999999999999, 24.600000000000001, 23, 23.199999999999999, 
23.899999999999999, 23, 20.800000000000001, 23.399999999999999, 
24.300000000000001, 24.399999999999999, 22.600000000000001, 22.100000000000001, 
22.199999999999999, 24.100000000000001, 28.100000000000001, 23.399999999999999, 
26.800000000000001, 24, 25.899999999999999, 24.699999999999999, 
24.100000000000001, 26.899999999999999, 23.899999999999999, 24.399999999999999, 
25.199999999999999, 22.899999999999999, 25.699999999999999, 24.300000000000001, 
25.199999999999999, 24.100000000000001), w = structure(c(1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), levels = c("0", 
"100"), class = "factor"), x = c(19, 22, 19.899999999999999, 
20.800000000000001, 22.899999999999999, 21, 21.800000000000001, 
22.600000000000001, 21.5, 22.600000000000001, 21.5, 22, 20.699999999999999, 
21.699999999999999, 20.800000000000001, 22.300000000000001, 21.800000000000001, 
19.800000000000001, 20.699999999999999, 20.600000000000001, 19.899999999999999, 
20.199999999999999, 20.199999999999999, 19, 19.600000000000001, 
18.699999999999999, 18.699999999999999, 21.699999999999999, 21.300000000000001, 
19.199999999999999, 21, 21.100000000000001, 18.699999999999999, 
20.300000000000001, 21.600000000000001, 21.899999999999999, 18.100000000000001, 
19.199999999999999, 20, 18.899999999999999, 20.399999999999999, 
20.5, 21.300000000000001, 19.199999999999999, 19.399999999999999, 
20.199999999999999, 18.199999999999999, 21, 21.5, 22.100000000000001, 
19.600000000000001, 21.399999999999999, 19.199999999999999, 21, 
20.899999999999999, 19.600000000000001, 19, 21.300000000000001, 
22.199999999999999, 21.699999999999999, 20.800000000000001, 20.600000000000001, 
19.199999999999999, 20.800000000000001, 23.600000000000001, 21.199999999999999, 
22.600000000000001, 21.100000000000001, 23.300000000000001, 22.300000000000001, 
21.600000000000001, 22.600000000000001, 21.199999999999999, 23.899999999999999, 
23.800000000000001, 20.699999999999999, 22.199999999999999, 22.5, 
22.199999999999999, 22.699999999999999)), row.names = c("1.5", 
"2.10", "3.15", "4.20", "5.25", "6.30", "7.35", "8.40", "33.165", 
"34.170", "35.175", "36.180", "37.185", "38.190", "39.195", "40.200", 
"57.285", "58.290", "59.295", "60.300", "61.305", "62.310", "63.315", 
"64.320", "89.445", "90.450", "91.455", "92.460", "93.465", "94.470", 
"95.475", "96.480", "121.605", "122.610", "123.615", "124.620", 
"125.625", "126.630", "127.635", "128.640", "153.765", "154.770", 
"155.775", "156.780", "157.785", "158.790", "159.795", "160.800", 
"177.885", "178.890", "179.895", "180.900", "181.905", "182.910", 
"183.915", "184.920", "209.1045", "210.1050", "211.1055", "212.1060", 
"213.1065", "214.1070", "215.1075", "216.1080", "233.1165", "234.1170", 
"235.1175", "236.1180", "237.1185", "238.1190", "239.1195", "240.1200", 
"265.1325", "266.1330", "267.1335", "268.1340", "269.1345", "270.1350", 
"271.1355", "272.1360"), class = "data.frame")

## ----marginal_outcome, echo = TRUE, message = FALSE, results = "hide"----
m0 <- Lm(y ~ w, data = immun)

## ----cf0_cont, echo = TRUE, message = FALSE------------------------------
coef(m0)		### marginal Cohen's d
sqrt(vcov(m0))		### observed
sqrt(2/nrow(immun) * (coef(m0)^2/4 + 2)) ### expected
confint(m0)		### Wald

## ----ami_cont, echo = TRUE, message = FALSE, results = "hide"------------
m1 <- BoxCox(x ~ 1, data = immun)
m <- Mmlt(m0, m1, formula = ~ 1, data = immun)

## ----cfseci_cont, echo = TRUE, message = FALSE---------------------------
(cf1 <- coef(m)["y.w100"])	### marginal adjusted Cohen's d
sqrt(diag(vcov(m))["y.w100"])	### observed

## ----setheo_cont, echo = TRUE, message = FALSE---------------------------
lambda <- c(unclass(coef(m, type = "Lambdapar")))
sqrt(2/nrow(immun) * ((1 + lambda^2)*cf1^2 + 8)/(4*(1 + lambda^2))) ### expected

## ----ci_cont, echo = TRUE, message = FALSE-------------------------------
confint(m)["y.w100",]

## ----corr_cont, echo = TRUE, message = FALSE-----------------------------
coef(m, type = "Corr")

## ----r2_cont, echo = TRUE, message = FALSE-------------------------------
Omega <- as.array(coef(m, type = "Lambda"))[,,1]
1 - Omega[nrow(Omega), ncol(Omega)]^(-2)

## ----cfsec_cont, echo = TRUE, message = FALSE----------------------------
mad <- BoxCoxME(y ~ w + s(x), data = immun)

## ----hx_smooth, echo = TRUE, fig.width = 4, fig.height = 3.5, out.width='.6\\linewidth'----
plot(smooth_terms(mad))

## ----hy_cont, echo = TRUE, fig.width = 4, fig.height = 3.5, out.width='.6\\linewidth'----
plot(mad) 

## ----boxcox_cont, echo = TRUE, message = FALSE---------------------------
m0 <- BoxCox(y ~ w, data = immun)
m <- Mmlt(m0, m1, formula = ~ 1, data = immun)

## ----r2_boxcox, echo = TRUE, message = FALSE-----------------------------
Omega <- as.array(coef(m, type = "Lambda"))[,,1]
1 - Omega[nrow(Omega), ncol(Omega)]^(-2)

## ----coef_boxcox_cont, echo = TRUE, message = FALSE----------------------
(cf1 <- coef(m)["y.w100"])
confint(m)["y.w100",]

## ----trt, echo = TRUE, message = FALSE-----------------------------------
pnorm(cf1 / sqrt(2))

## ----CAOdata, echo = FALSE, message = FALSE------------------------------
load(system.file("rda", "Primary_endpoint_data.rda", package = "TH.data"))

## ----CAOno, echo = FALSE, message = FALSE--------------------------------
rt <- table(CAOsurv$randarm)

## ----CAOoutcome, echo = TRUE, message = FALSE----------------------------
CAOsurv$ypT0ypN0 <- factor(CAOsurv$path_stad == "ypT0ypN0")

## ----CAO-glm, echo = TRUE, message = FALSE-------------------------------
mg_w <- glm(ypT0ypN0 ~ randarm,
            data = CAOsurv, family = binomial())
exp(coef(mg_w)["randarm5-FU + Oxaliplatin"])
exp(confint(glht(mg_w), calpha = univariate_calpha())$confint[2,-1])

## ----CAO-marg, echo = TRUE, message = FALSE------------------------------
mpCR <- Polr(ypT0ypN0 ~ randarm, data = CAOsurv, na.action = na.pass, 
    method = "logistic")
exp(coef(mpCR)["randarm5-FU + Oxaliplatin"])

## ----CAO-margci, echo = TRUE, message = FALSE----------------------------
exp(confint(glht(mpCR, coef. = function(...) coef(..., fixed = FALSE)), 
    calpha = univariate_calpha())$confint)

## ----CAO-covariates, echo = TRUE-----------------------------------------
mage <- BoxCox(age ~ 1, data = CAOsurv)
msex <- Polr(geschlecht ~ 1, data = CAOsurv, method = "probit")
CAOsurv$ecog_o <- as.ordered(CAOsurv$ecog_b)
mecog <- Polr(ecog_o ~ 1, data = CAOsurv, na.action = na.pass, 
              method = "probit")
mentf <- Polr(bentf ~ 1, data = CAOsurv, na.action = na.pass, 
              method = "probit")
mT <- Polr(strat_t ~ 1, data = CAOsurv, method = "probit")
mN <- Polr(strat_n ~ 1, data = CAOsurv, method = "probit")

## ----CAO-Mmlt, echo = TRUE-----------------------------------------------
### results in the paper were produced using M = 250
### to reduce CRAN checking times, we use M = 50 here
m <- Mmlt(mage, msex, mecog, mentf, mT, mN, mpCR,
          data = CAOsurv, args = list(type = "ghalton", M = 50))
prm <- "ypT0ypN0.randarm5-FU + Oxaliplatin"
exp(coef(m)[prm])

## ----CAO-mmltci, echo = TRUE---------------------------------------------
ci <- confint(glht(m, coef. = function(...) coef(..., fixed = FALSE)), 
    calpha = univariate_calpha())$confint
exp(ci[prm,-1])

## ----CAO-corrhide, echo = TRUE-------------------------------------------
mr <- as.array(coef(m, type = "Cor"))["ypT0ypN0",,1]
i <- which.max(abs(mr[-length(mr)]))
(ni <- names(mr)[i])
(mr <- mr[i])

## ----CAO-r2, echo = TRUE, message = FALSE--------------------------------
Omega <- as.array(coef(m, type = "Lambda"))[,,1]
1 - Omega[nrow(Omega), ncol(Omega)]^(-2)

## ----CAO-se, echo = TRUE, message = FALSE--------------------------------
sqrt(vcov(mpCR))
sqrt(vcov(m)[prm, prm])

## ----flies, echo = TRUE, results = "hide"--------------------------------
data("FruitFlies", package = "Stat2Data")

## ----flies_subset, echo = TRUE, message = FALSE--------------------------
flies <- FruitFlies 
flies <- flies[flies$Treatment %in% c("8 virgin", "8 pregnant"),]
flies$Treatment <- flies$Treatment[, drop = TRUE]
flies$Longevity <- as.double(flies$Longevity)
flies$survival <- Surv(flies$Longevity)

## ----flies_marg, echo = TRUE---------------------------------------------
coxph_w <- Coxph(survival ~ Treatment, data = flies)
coef(coxph_w)		### log-hazard ratio
confint(coxph_w)	### Wald

## ----flies_mmlt, echo = TRUE, message = FALSE----------------------------
xmod <- BoxCox(Thorax ~ 1, data = flies)
m <- Mmlt(xmod, coxph_w, data = flies, formula = ~ 1)
(cf1 <- coef(m)["survival.Treatment8 virgin"])
(ci1 <- confint(m)["survival.Treatment8 virgin",])

## ----flies_r2, echo = TRUE, message = FALSE------------------------------
Omega <- as.array(coef(m, type = "Lambda"))[,,1]
1 - Omega[nrow(Omega), ncol(Omega)]^(-2)

## ----trans_y, echo = TRUE, fig.width = 4, fig.height = 3.5, out.width='.6\\linewidth'----
q <- 0:100
cols <- c("grey20", "grey70")
### nonparametric
plot(q, log(-log(1 - ecdf(subset(flies, Treatment == "8 pregnant")$survival[,1])(q))), 
     main = "", xlab = "Time", type = "S", lwd = 1, 
     ylab = "cloglog(Probability)")
lines(q, log(-log(1 - ecdf(subset(flies, Treatment == "8 virgin")$survival[,1])(q))), 
     type = "S", lty = 2, lwd = 1)
legend("bottomright", lty = c(1, 2),
       legend = levels(flies$Treatment), bty = "n")

### model-based
nd <- expand.grid(survival = q, Treatment = sort(unique(flies$Treatment)))
nd$h <- predict(as.mlt(coxph_w), newdata = nd, type = "trafo")
fm <- nd$Treatment == "8 virgin"
lines(nd$survival[fm], nd$h[fm], lty = 2)
fm <- nd$Treatment == "8 pregnant"
lines(nd$survival[fm], nd$h[fm], lty = 1)

## ----copula, echo = TRUE, fig.with = 4, fig.height = 4, out.width='.6\\linewidth'----
m <- CoxphME(survival ~ s(Thorax, k = 5), data = flies, 
             subset = Treatment == levels(Treatment)[1])
plot(smooth_terms(m))
m <- CoxphME(survival ~ s(Thorax, k = 5), data = flies, 
             subset = Treatment == levels(Treatment)[2])
plot(smooth_terms(m), add = TRUE, lty = 2) 

## ----nami-pkgs-funs, echo = FALSE, results = "hide"----------------------
if (file.exists("packages.bib")) file.remove("packages.bib")

## sentence style for titles
toLower <- function(text) {
  parts <- strsplit(unname(text), split = ":")[[1]]
  w1 <- paste0(parts[1], ":")
  p2 <- strsplit(parts[2], split = "}")[[1]]
  p3 <- strsplit(p2[1], " ")
  p4 <- strsplit(p2[1], " ")[[1]]
  w2 <- p4[2]
  w3 <- paste(p4[3:length(p4)], collapse = " ")
  w3 <- tolower(w3)
  paste(w1, w2, w3, "},")
}
sentence_style <- FALSE

## R
x <- citation()[[1]]
b <- toBibtex(x)
b <- gsub("R:", paste0("\\\\proglang{R}:"), b)
b <- gsub("R ", paste0("\\\\proglang{R} "), b)
if (sentence_style) b["title"] <- toLower(b["title"])
b[1] <- "@Manual{R,"
cat(b, sep = "\n", file = "packages.bib", append = TRUE)

pkgv <- function(pkg) packageVersion(pkg)

pkgbib <- function(pkg) {
    x <- citation(package = pkg, auto = TRUE)[[1]]
    b <- toBibtex(x)
    b <- gsub("packaging by", "", b)
    b <- gsub("with contributions from", "", b)
    b <- gsub("Gruen", "Gr{\\\\\"u}n", b)
    b[1] <- paste("@Manual{pkg:", pkg, ",", sep = "")
#    if (is.na(b["url"])) {
#        b[length(b)] <- paste("   URL = {http://CRAN.R-project.org/package=",
#                              pkg, "},", sep = "")
#    }
    b <- b[names(b) != "url"]
    if (is.na(b["doi"])) {
        b[length(b)] <- paste("   DOI = {10.32614/CRAN.package.",
                              pkg, "}", sep = "")
        b <- c(b, "}")
    }
    b["note"] <- gsub("R package", "\\\\proglang{R} package", b["note"])
    cat(b, sep = "\n", file = "packages.bib", append = TRUE)
}
pkg <- function(pkg)
    paste("\\pkg{", pkg, "} \\citep[version~",
          pkgv(pkg), ",][]{pkg:", pkg, "}", sep = "")

pkgs <- c("tram")
sapply(pkgs, pkgbib)
out <- sapply(pkgs, pkg)

