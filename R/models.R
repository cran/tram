
.mrightORmcounting <- function(object) {
    if (!inherits(object, "Surv")) return(FALSE)
    type <- attr(object, "type")
    if (!(type %in% c("mright", "mcounting")))
        return(FALSE)
    st <- unclass(object)[, "status"]
    if (all(unique(st) %in% c(0, 1))) return(FALSE)
    return(TRUE)
}

Coxph <- function(formula, data, subset, weights, offset, cluster, na.action = na.omit, ...)
{
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "weights", "offset", "cluster"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf[[1L]] <- quote(tram_data)
    td <- eval(mf, parent.frame())

    stopifnot(inherits(td$response, "Surv") ||
              inherits(td$response, "response") ||
              is.numeric(td$response))

    if (.mrightORmcounting(td$response)) {
        ocall <- call <- match.call(expand.dots = TRUE)
        call$primary <- "Coxph"
        call[[1L]] <- quote(Compris)
        ret <- eval(call, parent.frame())
        ret$call <- ocall
        return(ret)
    }

    ret <- tram(td, transformation = "smooth", distribution = "MinExtrVal", 
                negative = FALSE, ...)
    if (!inherits(ret, "mlt")) return(ret)
    ret$call <- match.call(expand.dots = TRUE)
    if (!is.null(td$mt$z)) {
        ret$tram <- paste(ifelse(is.null(td$mt$s), "", "(Stratified)"),
                          "Shift-Scale Transformation Model")
    } else {
        ret$tram <- paste(ifelse(is.null(td$mt$s), "", "(Stratified)"), 
                          "Parametric Linear Cox Regression Model")
    }
    if (!inherits(ret, "fmlt"))
        class(ret) <- c("Coxph", class(ret))
    ret
}

Aareg <- function(formula, data, subset, weights, offset, cluster, na.action = na.omit, ...)
{
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "weights", "offset", "cluster"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf[[1L]] <- quote(tram_data)
    td <- eval(mf, parent.frame())

    stopifnot(inherits(td$response, "Surv") ||
              inherits(td$response, "response") ||
              is.numeric(td$response))

    if (is.null(td$mt$s))
        warning("no time-varying terms specified for this model")
    if (!is.null(td$mt$x))
        stop("no constant linear terms allowed in this model")

    ### we need additional trafo(0) = 0 constraints 
    ### because trafo = cumhazard
    args <- list(...)
    args$formula <- td
    args$transformation <- "smooth"
    args$distribution <- "Exponential"
    args$negative <- FALSE
    args$model_only <- TRUE
    ret <- do.call("tram", args)
    ### always start at time 0, so that the first parameter 
    ### is trafo(0), which can then be constrained to zero
    su <- mkgrid(ret$model, n = 2)[[1]]
    su[1] <- 0
    args$support <- su
    ret <- do.call("tram", args)
    if (isTRUE(list(...)$model_only)) return(ret)

    cf <- names(coef(ret))
    cf <- cf[grep("Bs1", cf)]
    ctr <- numeric(length(cf))
    names(ctr) <- cf

    ### now fit the model with support and constraints
    args <- list(...)
    args$formula <- td
    args$transformation <- "smooth"
    args$distribution <- "Exponential"
    args$negative <- FALSE
    args$support <- su
    if (is.null(args$fixed)) {
        args$fixed <- ctr
    } else {
        args$fixed <- c(args$fixed, ctr)
    }
    
    ret <- do.call("tram", args)

    if (!inherits(ret, "mlt")) return(ret)
    ret$call <- match.call(expand.dots = TRUE)
    if (!is.null(td$mt$z)) {
        ret$tram <- paste(ifelse(is.null(td$mt$s), "", "(Stratified)"),
                          "Shift-Scale Transformation Model")
    } else {
        ret$tram <- paste(ifelse(is.null(td$mt$s), "", "(Stratified)"), 
                          "Parametric Linear Aalen Regression Model")
    }
    if (!inherits(ret, "fmlt"))
        class(ret) <- c("Aareg", class(ret))
    ret
}

Survreg <- function(formula, data, subset, weights, offset, cluster, na.action = na.omit, 
                    dist = c("weibull", "logistic", "gaussian", "exponential", 
                             "rayleigh", "loggaussian", "lognormal", "loglogistic"), 
                    scale = 0, ...)
{
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "weights", "offset", "cluster"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf[[1L]] <- quote(tram_data)
    td <- eval(mf, parent.frame())

    stopifnot(inherits(td$response, "Surv") ||
              inherits(td$response, "response") ||
              is.numeric(td$response))

    dist <- match.arg(dist)
    distribution <- switch(dist, "logistic" = "Logistic",
                                 "loglogistic" = "Logistic",
                                 "gaussian" = "Normal",
                                 "loggaussian" = "Normal",
                                 "lognormal" = "Normal",
                                 "weibull" = "MinExtrVal",
                                 "exponential" = "MinExtrVal",
                                 "rayleigh" = "MinExtrVal")

    transformation <- switch(dist, "logistic" = "linear",
                                   "loglogistic" = "logarithmic",
                                   "gaussian" = "linear",
                                   "loggaussian" = "logarithmic",
                                   "lognormal" = "logarithmic",
                                   "weibull" = "logarithmic",
                                   "exponential" = "logarithmic",
                                   "rayleigh" = "logarithmic")

    args <- list(...)
    args$formula <- td
    args$transformation <- transformation
    args$distribution <- distribution
    args$negative <- TRUE
    ### check if user asked for model_only = TRUE
    if (is.null(args$model_only)) {
        args$model_only <- TRUE
        ret <- do.call("tram", args)
    } else {
        if (args$model_only) {
            ret <- do.call("tram", args)
            return(ret)
        } else {
            args$model_only <- TRUE
            ret <- do.call("tram", args)
        }
    }

    rname <- names(td$mf)[1]    
    cfnm <- names(coef(ret))

    scalecf <- grep(rname, cfnm, fixed = TRUE)
#    if (length(scalecf) > 0) {
#        if (length(grep("Intercept", cfnm[scalecf])) > 0)
#            stop("strata contain explicit intercept term")
#    }
    if (dist == "exponential") 
        scale <- 1
    if (dist == "rayleigh")
        scale <- 0.5
    if (scale > 0) {
        fixed <- rep_len(1 / scale, length(scalecf))
        names(fixed) <- cfnm[scalecf]
        ret <- tram(td, transformation = transformation, 
                    distribution = distribution, negative = TRUE, 
                    fixed = fixed, ...)
    } else {
        ret <- tram(td, transformation = transformation, 
                    distribution = distribution, negative = TRUE, ...)
    }
    if (!inherits(ret, "mlt")) return(ret)
    if (length(scalecf) > 0)
        ret$invscale <- 1 / coef(as.mlt(ret))[scalecf]
    ret$call <- match.call(expand.dots = TRUE)

    .simpleCap <- function(x) {
         s <- strsplit(x, " ")[[1]]
         paste(toupper(substring(s, 1, 1)), substring(s, 2),
               sep = "", collapse = " ")
    }

    if (!is.null(td$mt$z)) {
        ret$tram <- paste(ifelse(is.null(td$mt$s), "", "(Stratified)"),
                          "Shift-Scale Transformation Model")
    } else {
        ret$tram <- paste(ifelse(is.null(td$mt$s), "", "(Stratified)"),
                          .simpleCap(dist), "Linear Regression Model")
    }
    if (!inherits(ret, "fmlt"))
        class(ret) <- c("Survreg", class(ret))
    ret
}

Colr <- function(formula, data, subset, weights, offset, cluster, na.action = na.omit, ...)
{
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "weights", "offset", "cluster"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf[[1L]] <- quote(tram_data)
    td <- eval(mf, parent.frame())

    stopifnot(inherits(td$response, "Surv") ||
              inherits(td$response, "response") ||
              is.numeric(td$response))

    if (.mrightORmcounting(td$response)) {
        ocall <- call <- match.call(expand.dots = TRUE)
        call$primary <- "Colr"
        call[[1L]] <- quote(Compris)
        ret <- eval(call, parent.frame())
        ret$call <- ocall
        return(ret)
    }

    ret <- tram(td, transformation = "smooth", 
                distribution = "Logistic", negative = FALSE, ...)
    if (!inherits(ret, "mlt")) return(ret)
    ret$call <- match.call(expand.dots = TRUE)
    if (!is.null(td$mt$z)) {
        ret$tram <- paste(ifelse(is.null(td$mt$s), "", "(Stratified)"),
                          "Shift-Scale Transformation Model")
    } else {
        ret$tram <- paste(ifelse(is.null(td$mt$s), "", "(Stratified)"),
                          "Continuous Outcome Logistic Regression")
    }
    if (!inherits(ret, "fmlt"))
        class(ret) <- c("Colr", class(ret))
    ret
}

Polr <- function(formula, data, subset, weights, offset, cluster, na.action = na.omit, 
                 method = c("logistic", "probit", "loglog", "cloglog", "cauchit"), ...)
{
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "weights", "offset", "cluster"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf[[1L]] <- quote(tram_data)
    td <- eval(mf, parent.frame())

    stopifnot(is.ordered(td$response) || inherits(td$response, "response") || 
              (is.factor(td$response) && nlevels(td$response) == 2))

    method <- match.arg(method)
    distribution <- c("logistic" = "Logistic", "probit" = "Normal", 
                      "loglog" = "MaxExtrVal", "cloglog" = "MinExtrVal", 
                      "cauchit" = "Cauchy")
    distribution <- distribution[method]
    name <- c("logistic" = "Odds", "loglog" = "Reverse time hazards",
              "cloglog" = "Hazards")

    ### <FIXME> maybe use as.basis(<response>, Matrix = TRUE) when nlevels
    ### is large </FIXME>
    ret <- tram(td, transformation = "discrete", distribution = distribution, negative = TRUE, ...)
    if (!inherits(ret, "mlt")) return(ret)
    ret$call <- match.call(expand.dots = TRUE)
    if (!is.null(td$mt$z)) {
        ret$tram <- paste(ifelse(is.null(td$mt$s), "", "(Stratified)"),
                          "Shift-Scale Transformation Model")
    } else {
        if (method != "probit") {
            ret$tram <- paste(ifelse(is.null(td$mt$s), "", "(Stratified)"),
                              "Proportional", name[method], "Regression Model")
        } else {
            ret$tram <- paste(ifelse(is.null(td$mt$s), "", "(Stratified)"),
                              "Ordered Probit Regression Model")
        }                   
    }
    if (!inherits(ret, "fmlt"))
        class(ret) <- c("Polr", class(ret))
    ret
}

Lm <- function(formula, data, subset, weights, offset, cluster, na.action = na.omit, ...)
{
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "weights", "offset", "cluster"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf[[1L]] <- quote(tram_data)
    td <- eval(mf, parent.frame())

    stopifnot(inherits(td$response, "Surv") ||
              inherits(td$response, "response") ||
              is.numeric(td$response))

    ret <- tram(td, transformation = "linear", distribution = "Normal", 
                negative = TRUE, ...)
    if (!inherits(ret, "mlt")) return(ret)
    ret$call <- match.call(expand.dots = TRUE)
    if (!is.null(td$mt$z)) {
        ret$tram <- paste(ifelse(is.null(td$mt$s), "", "(Stratified)"),
                          "Shift-Scale Normal Regression Model")
    } else {
        ret$tram <- paste(ifelse(is.null(td$mt$s), "", "(Stratified)"),
                          "Normal Linear Regression Model")
    }
    if (!inherits(ret, "fmlt"))
        class(ret) <- c("Lm", class(ret))
    ret
}

BoxCox <- function(formula, data, subset, weights, offset, cluster, na.action = na.omit, ...)
{
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "weights", "offset", "cluster"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf[[1L]] <- quote(tram_data)
    td <- eval(mf, parent.frame())

    stopifnot(inherits(td$response, "Surv") ||
              inherits(td$response, "response") ||
              is.numeric(td$response))

    if (.mrightORmcounting(td$response)) {
        ocall <- call <- match.call(expand.dots = TRUE)
        call$primary <- "BoxCox"
        call[[1L]] <- quote(Compris)
        ret <- eval(call, parent.frame())
        ret$call <- ocall
        return(ret)
    }

    ret <- tram(td, transformation = "smooth", distribution = "Normal", 
                negative = TRUE, ...)
    if (!inherits(ret, "mlt")) return(ret)
    ret$call <- match.call(expand.dots = TRUE)
    if (!is.null(td$mt$z)) {
        ret$tram <- paste(ifelse(is.null(td$mt$s), "", "(Stratified)"),
                          "Shift-Scale Transformation Model")
    } else {
        ret$tram <- paste(ifelse(is.null(td$mt$s), "", "(Stratified)"),
                          "Non-normal (Box-Cox-Type) Linear Regression Model")
    }
    if (!inherits(ret, "fmlt"))
        class(ret) <- c("BoxCox", class(ret))
    ret
}

Lehmann <- function(formula, data, subset, weights, offset, cluster, na.action = na.omit, ...)
{
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "weights", "offset", "cluster"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf[[1L]] <- quote(tram_data)
    td <- eval(mf, parent.frame())

    stopifnot(inherits(td$response, "Surv") ||
              inherits(td$response, "response") ||
              is.numeric(td$response))

    ret <- tram(td, transformation = "smooth", distribution = "MaxExtrVal", 
                negative = TRUE, ...)
    if (!inherits(ret, "mlt")) return(ret)
    ret$call <- match.call(expand.dots = TRUE)
    if (!is.null(td$mt$z)) {
        ret$tram <- paste(ifelse(is.null(td$mt$s), "", "(Stratified)"),
                          "Shift-Scale Transformation Model")
    } else {
        ret$tram <- paste(ifelse(is.null(td$mt$s), "", "(Stratified)"),
                          "Proportional Reverse Time Hazards Linear Regression Model")
    }
    if (!inherits(ret, "fmlt"))
        class(ret) <- c("Lehmann", class(ret))
    ret
}

