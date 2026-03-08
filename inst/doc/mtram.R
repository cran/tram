## ----mtram-pkgs, echo = FALSE, results = "hide", message = FALSE, warning = FALSE----
set.seed(290875)

pkgs <- c("colorspace", "survival", "lme4", "tram", "gridExtra",
          "lattice", "latticeExtra", "mvtnorm", "ordinalCont", "tramME")
pkgs <- sapply(pkgs, require, character.only = TRUE)

## ----mtram-citation, echo = FALSE----------------------------------------
year <- substr(packageDescription("tram")$Date, 1, 4)
version <- packageDescription("tram")$Version

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

