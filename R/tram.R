
tram_data <- function(formula, data, subset, weights, offset, cluster, na.action = na.omit) 
{

    ## set up model.frame() call
    mf <- match.call(expand.dots = FALSE)
    mf$na.action <- na.action ### evaluate na.action
    if(missing(data)) data <- environment(formula)
    m <- match(c("formula", "data", "subset", "na.action", "weights", "offset", "cluster"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf$dot <- "sequential"

    ## formula processing
    oformula <- as.formula(formula)
    formula <- Formula::as.Formula(formula)
    mf$formula <- formula
    npart <- length(formula)
    if(any(npart < 1L)) stop("'formula' must specify at least one left-hand and one right-hand side")
    if(any(npart > 2L)) stop("'formula' must specify at least one left-hand and one right-hand side")

    ## evaluate model.frame
    mf[[1L]] <- quote(stats::model.frame)
    cl <- mf
    ### R(y) ~ x will fail
    mf <- try(eval(mf, parent.frame()), silent = TRUE)
    if (inherits(mf, "try-error")) {
        ### set-up a formula with response only
        rfm <- terms(formula, lhs = 1, rhs = 0)
        ### evaluate response only
        if (missing(data))
            data <- parent.frame()
        r <- eval(rfm[[2L]], envir = data, enclos = parent.frame())

        ### set-up a formula without response
        lhs <- formula[c(1, 2)]
        rhs <- formula[c(1, 3)]
        if (npart[1L] > 1) {
            lhs <- terms(as.Formula(lhs), rhs = 2, lhs = 0)
        } else {
            lhs <- terms(~ 1)
        }
        sxzfm <- as.Formula(lhs, rhs)

        ### get all x, s and z variables first WITHOUT subsetting or
        ### na.action
        avcl <- cl
        avcl$formula <- sxzfm
        avcl[[1L]] <- quote(stats::get_all_vars)
        avcl$subset <- NULL
        avcl$na.action <- NULL
        avcl$drop.unused.levels <- NULL
        avcl$dot <- NULL
        av <- eval(avcl, parent.frame())
        ### assign row numbers
        stopifnot(NROW(r) == NROW(av))
        av[[".index."]] <- 1:NROW(av)

        ### evaluate model frame _without_ response
        sxzfm <- as.Formula(~ .index., lhs, rhs)
        cl$formula <- sxzfm
        cl$data <- av
        cl$dot <- NULL
        mf <- eval(cl, av)
        rname <- make.names(deparse(rfm[[2L]]))
        ### store the response outside the model.frame
        response <- r[mf[[".index."]], , drop = FALSE]
        mf[[rname]] <- response
        mf[[".index."]] <- NULL
        mf <- mf[, c(rname, colnames(mf)[colnames(mf) != rname]), drop = FALSE]
    }

    response <- mf[[1L]]
    rname <- names(mf)[1L]

    ## extract terms in various combinations
    mt <- list(
      "all" = terms(formula, data = data,                        dot = "sequential"),
      "y"   = terms(formula, data = data, lhs = 1L, rhs = 0L,    dot = "sequential"),
      "s"   = if (npart[1] == 2) 
              terms(formula, data = data, lhs = 2L, rhs = 0L,    dot = "sequential"),
      "x"   = terms(formula, data = data, lhs = 0L, rhs = 1L,    dot = "sequential"),
      "z"   = if (npart[2] == 2) 
              terms(formula, data = data, lhs = 0L, rhs = 2L,    dot = "sequential")
    )

    # stopifnot(is.null(mt$z))
    ### ~ 0 or ~ 1: no need to do anything
    if (length(attr(mt$s, "variables")) == 1)
        mt$s <- NULL
    if (length(attr(mt$x, "variables")) == 1)
        mt$x <- NULL
    if (length(attr(mt$z, "variables")) == 1)
        mt$z <- NULL

    ### Surv(...) etc. will be altered anyway...
    # names(mf) <- make.names(names(mf))

    weights <- model.weights(mf)
    offset <- model.offset(mf)
    cluster <- mf[["(cluster)"]]

    ret <- list(response = response, rname = rname, weights = weights, 
                offset = offset, cluster = cluster, mf = mf, mt = mt)
    class(ret) <- "tram_data"
    ret
}

tram <- function(formula, data, subset, weights, offset, cluster, na.action = na.omit,
                 distribution = c("Normal", "Logistic", "MinExtrVal", "MaxExtrVal", "Exponential", "Cauchy", "Laplace"),
                 frailty = c("None", "Gamma", "InvGauss", "PositiveStable"),
                 transformation = c("discrete", "linear", "logarithmic", "smooth"),
                 LRtest = TRUE, 
                 prob = c(.1, .9), support = NULL, bounds = NULL, add = c(0, 0), order = 6, negative =
                 TRUE, remove_intercept = TRUE, 
                 scale = TRUE, scale_shift = FALSE, extrapolate = FALSE, log_first = FALSE, 
                 sparse_nlevels = Inf, model_only = FALSE, 
                 constraints = NULL, ...) 
{

    call <- match.call()
    fitted <- NULL
    if (!is.null(constraints)) {
#        call$constraints <- NULL
#        call$dofit <- FALSE
        fitted <- eval(call[!names(call) %in% c("constraints", "model_only")], parent.frame())
    }

    frailty <- match.arg(frailty)
    distribution <- match.arg(distribution)
    if (frailty != "None" && distribution != "MinExtrVal")
        stop("Frailties only implemented for distribution = MinExtrVal")

    if (!inherits(td <- formula, "tram_data")) {
        mf <- match.call(expand.dots = FALSE)
        m <- match(c("formula", "data", "subset", "na.action", "weights", "offset", "cluster"), names(mf), 0L)
        mf <- mf[c(1L, m)]
        mf[[1L]] <- quote(tram_data)
        td <- eval(mf, parent.frame())
    } 

    rvar <- asvar(td$response, td$rname, prob = prob, support = support,
                  bounds = bounds, add = add, sparse_nlevels = sparse_nlevels)
    if (!is.null(rvar$bounds) && log_first) {
        if (rvar$bounds[1] < sqrt(.Machine$double.eps)) 
            rvar$bounds[1] <- sqrt(.Machine$double.eps)
    }
    rbasis <- mkbasis(rvar, transformation = transformation, order = order,
                      extrapolate = extrapolate, log_first = log_first,
                      remove_intercept = !remove_intercept)

    ### for binary responses, the second parameter is redundant
    ### this should happen in the basis function, but as.basis.factor_var
    ### currently doesn't allow it
    yfixed <- NULL
    if (is.factor(td$response) && nlevels(td$response) == 2L) {
        yfixed <- 0
        names(yfixed) <- rev(colnames(rbasis(td$mf)))[1L]
    }

    iS <- NULL
    if (!is.null(td$mt$s)) {
        ### model.matrix(~ s1) has intercept
        ### model.matrix(~ 0 + s1) hasn't
        iS <- as.basis(formula(Formula(td$mt$s)[-3]), data = td$mf)
        ### model.matrix(~ s1:s2) is cell-means model WITH intercept
        ### remove if necessary
        tmp <- model.matrix(iS, data = td$mf)
        if (isTRUE(all.equal(tmp[,1], rowSums(tmp[,-1, drop = FALSE]), 
                             check.attributes = FALSE)))
            iS <- as.basis(formula(Formula(td$mt$s)[-3]), data = td$mf, 
                           remove_intercept = TRUE)
    }

    iX <- NULL
    if (!is.null(td$mt$x)) {
        ### user-supplied constraints
        if (!is.null(constraints)) {
            if (is.list(constraints)) {
                uici <- glht(fitted, linfct = constraints[[1]],
                                     rhs = constraints[[2]], 
                                     alternative = "less")
            } else {
                if (is.character(constraints)) {
                    uici <- glht(fitted, linfct = constraints)
                } else {
                    uici <- glht(fitted, linfct = constraints, 
                                 alternative = "less")
                }
            }
            ui <- uici$linfct
            ci <- uici$rhs
            if (uici$alternative == "greater") {
                ui <- -ui
                ci <- -ci
            }
        } else {
            ui <- ci <- NULL
        }
        ### NOTE: this triggers sumconstr = TRUE
        ### when there is no scale term. Otherwise h is centered and we
        ### need an explicit intercept here.
        iX <- as.basis(td$mt$x, data = td$mf, 
                       remove_intercept = remove_intercept,
                       negative = negative, ui = ui, ci = ci)
    } else {
        if (!remove_intercept) iX <- intercept_basis(negative = negative)
    }

    isX <- NULL
    if (!is.null(td$mt$z)) {
        ### user-supplied constraints
        if (!is.null(constraints)) {
            if (is.list(constraints)) {
                uici <- glht(fitted, linfct = constraints[[1]],
                                     rhs = constraints[[2]], 
                                     alternative = "less")
            } else {
                if (is.character(constraints)) {
                    uici <- glht(fitted, linfct = constraints)
                } else {
                    uici <- glht(fitted, linfct = constraints, 
                                 alternative = "less")
                }
            }
            ui <- uici$linfct
            ci <- uici$rhs
            if (uici$alternative == "greater") {
                ui <- -ui
                ci <- -ci
            }
        } else {
            ui <- ci <- NULL
        }
        ### NOTE: this triggers sumconstr = TRUE
        isX <- as.basis(td$mt$z, data = td$mf, remove_intercept = TRUE, 
                        negative = FALSE, ui = ui, ci = ci, prefix = "scl_")
    } 

    model <- ctm(response = rbasis, interacting = iS, shifting = iX, 
                 scaling = isX, scale_shift = scale_shift,
                 todistr = distribution, data = td$mf)

    if (model_only) return(model)

    ### <FIXME> this is a hack: stratum terms must not appear in the
    ###         linear predictor; so we remove them _by name_ (which isn't
    ###         save). Use terms and drop. Here we use fixed which should
    ###         be OK.
    Xfixed <- NULL
    if (!is.null(iS) && !is.null(iX)) {

        fm <- as.formula(paste("~", 
            as.character(as.expression(td$mt$x[[2]])), "+",
            as.character(as.expression(td$mt$s[[2]]))))
        nS <- colnames(model.matrix(fm, data = td$mf[1:10,])) 
        ### nS <- colnames(model.matrix(iS, data = td$mf[1:10,]))
        nX <- colnames(model.matrix(iX, data = td$mf[1:10,]))
        ### fix coefs corresponding to a stratum to zero
        if (any(xin <- !nX %in% nS)) {
            Xfixed <- numeric(sum(xin))
            names(Xfixed) <- nX[xin]
        }
    } 
    ### </FIXME>
    fixed <- c(list(...)$fixed, yfixed, Xfixed)

    if (!is.null(iS) && !is.null(isX)) {
      nS <-  as.character(td$mt$s[[2]])
      nS <- nS[!grepl("[+]", nS)]
      nsX <- as.character(td$mt$z[[2]])
      nsX <- nsX[!grepl("[+]", nsX)]
      if (any(xin <- nsX %in% nS))
        stop("scaling variables not allowed as stratifying variables")
      warning("Models with both strata and scale terms are highly experimental")
      ### note: we would need extra constraints for all baseline
      ### transformations in the discrete case
      ### we also need stratum-specific intercepts.
    }

    if (!remove_intercept && transformation == "discrete")
        fixed[names(coef(model))[1L]] <- 0

    args <- list(...)
    args$model <- model
    args$data <- td$mf
    args$weights <- td$weights
    args$offset <- td$offset
    args$scale <- scale
    args$fixed <- fixed
    ret <- do.call("mlt", args)
    ret$terms <- td$mt
    ret$cluster <- td$cluster
    if (!is.null(iX))
        ret$shiftcoef <- colnames(model.matrix(iX, data = td$mf))
    if (!is.null(isX))
        ret$scalecoef <- colnames(model.matrix(isX, data = td$mf))
    if (!is.null(iS))
        ret$stratacoef <- colnames(model.matrix(iS, data = td$mf))
    if (is.null(isX)) {
        class(ret) <- c("tram", class(ret))
    } else {
        class(ret) <- c("stram", "tram", class(ret))
    }
    ret$negative <- negative

    if (LRtest & !is.null(iX)) {
        nullmodel <- ctm(response = rbasis, interacting = iS, 
                         todistr = distribution, data = td$mf)
        if (!is.null(fixed)) {
            fixed <- fixed[names(fixed) %in% names(coef(nullmodel))]
            args$fixed <- fixed
        }
        args$model <- nullmodel
        args$theta <- NULL
        nullret <- do.call("mlt", args)
        nulllogLik <- logLik(nullret)
        fulllogLik <- logLik(ret)
        ret$LRtest <- c(LRstat = -2 * (nulllogLik - fulllogLik), 
                        df = attr(fulllogLik, "df") - attr(nulllogLik, "df"))
    }

    if (frailty != "None") {
        ret <- mlt:::fmlt(as.mlt(ret), frailty = frailty)
        ret$frailty <- frailty
    }

    ret
}
