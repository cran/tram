## ----mtram-setup, echo = FALSE, results = "hide", message = FALSE, warning = FALSE----
set.seed(290875)

pkgs <- sapply(c("mlt", "survival", "tram", "lme4", "gridExtra", 
         "lattice", "latticeExtra", "mvtnorm", "ordinalCont"), require, char = TRUE)

## ----fail, results = "asis", echo = FALSE-------------------------------------
if (any(!pkgs))
{
    cat(paste("Package(s)", paste(names(pkgs)[!pkgs], collapse = ", "), 
        "not available, stop processing.",
        "\\end{document}\n"))
    knitr::knit_exit()
}

## ----mtram-funs, echo = FALSE, results = "hide"-------------------------------
if (file.exists("packages.bib")) file.remove("packages.bib")
pkgversion <- function(pkg) {
    pkgbib(pkg)
    packageDescription(pkg)$Version
}
pkgbib <- function(pkg) {
    x <- citation(package = pkg, auto = TRUE)[[1]]
    b <- toBibtex(x)
    b <- gsub("Buehlmann", "B{\\\\\"u}hlmann", b)
    b[1] <- paste("@Manual{pkg:", pkg, ",", sep = "")
    if (is.na(b["url"])) {
        b[length(b)] <- paste("   URL = {http://CRAN.R-project.org/package=",
                              pkg, "}", sep = "")
        b <- c(b, "}")
    }
    cat(b, sep = "\n", file = "packages.bib", append = TRUE)
}

pkg <- function(pkg) {
    vrs <- try(pkgversion(pkg))
    if (inherits(vrs, "try-error")) return(NA)
    paste("\\\\pkg{", pkg, "} \\\\citep[version~",
          vrs, ",][]{pkg:", pkg, "}", sep = "")
}

pkg("mlt")
pkg("tram")
pkg("SparseGrid")
#pkg("lme4")
cat(c("@Manual{vign:mlt.docreg,",
             "    title = {Most Likely Transformations: The mlt Package},",
             "    author = {Torsten Hothorn},",
             paste("    year = ", substr(packageDescription("mlt.docreg")$Date, 1, 4), ",", sep = ""),
             paste("    note = {R package vignette version ", packageDescription("mlt.docreg")$Version, "},", sep = ""),
             "    url = {https://CRAN.R-project.org/package=mlt.docreg},",
             "}"), file = "packages.bib", append = TRUE, sep = "\n")

