
asvar <- function(object, name, ...)
    UseMethod("asvar")

asvar.factor <- function(object, name, ...)  
    variables::factor_var(name, levels = levels(object))

asvar.ordered <- function(object, name, sparse_nlevels = Inf, ...)
    variables::ordered_var(name, levels = levels(object),
                           sparse = nlevels(object) >= sparse_nlevels)

asvar.numeric <- function(object, name, prob = c(.1, .9), support = NULL, bounds = NULL, add = c(0, 0), ...) {
    if (is.integer(object)) {
        if (is.null(support)) 
            support <- sort(unique(object))
        return(variables::numeric_var(name, support = support, bounds = bounds, add = add, ...))  
    }
    if (is.null(support)) {
        support <- quantile(object, probs = prob, na.rm = TRUE)
        add <- range(object, na.rm = TRUE) - support
        return(variables::numeric_var(name, support = support, add = add, bounds = bounds, ...))
    }
    return(variables::numeric_var(name, support = support, bounds = bounds, add = add,...))
}

asvar.Surv <- function(object, name, prob = c(.1, .9), support = NULL, bounds = c(0, Inf), add = c(0, 0), ...) {
    if (is.null(bounds)) bounds <- c(0, Inf)
    if (is.null(support)) {
        support <- quantile(sf <- survfit(y ~ 1, data = data.frame(y = object)), probs = prob)$quantile
        first <- min(sf$time[sf$n.event > 0])
        last <- max(sf$time[sf$n.event > 0])
        if (is.na(support[1])) 
            support[1] <- first
        if (is.na(support[2])) 
            support[2] <- last
        add <- c(0 - support[1], 0)
        return(variables::numeric_var(name, support = support, bounds = bounds, add = add, ...))
    }
    variables::numeric_var(name, support = support, bounds = bounds, add = add, ...)
}

asvar.response <- function(object, name, prob = c(.1, .9), support = NULL, bounds = c(-Inf, Inf), ...) {
    if (any(sapply(object, is.factor))) {
        f <- object$exact
        if (any(!is.na(f))) return(asvar(f, name, ...))
        if (any(!is.na(object$cleft))) return(asvar(object$cleft, name, ...))
        stop("cannot determine class of response")
    }
    if (is.null(support)) {
        support <- quantile(sf <- survfit(y ~ 1, data = data.frame(y = as.Surv(object))), probs = prob)$quantile
        last <- max(sf$time[sf$n.event > 0])
        if (is.na(support[2])) 
            support[2] <- last
    }
    variables::numeric_var(name, support = support, bounds = bounds, ...)
}

mkbasis <- function(yvar, transformation = c("discrete", "linear", "logarithmic", "smooth", "positive_smooth"), 
                    order = 6, extrapolate = FALSE, log_first = FALSE, remove_intercept = FALSE) {

    transformation <- match.arg(transformation)

    if (remove_intercept) {
        if (inherits(yvar, "numeric_var")) {
            return(switch(transformation, 
                "discrete" = stop("Discrete transformation not defined for numeric variable"),
                "linear" = {
                    fm <- as.formula(paste0("~", yvar$name))
                    as.basis(fm, data = as.data.frame(mkgrid(yvar), check.names = FALSE), 
                             remove_intercept = TRUE, ui = Matrix(1), ci = 0)
                },
                "logarithmic" = log_basis(yvar, ui = "increasing", remove_intercept = TRUE),
                "smooth" = Bernstein_basis(yvar, ui = c("increasing", "zeroint"), order = order,
                                           extrapolate = extrapolate, log_first = log_first),
                "positive_smooth" = stop("Not defined")
                )
            )
         }
    }

    if (inherits(yvar, "numeric_var")) {
      return(switch(transformation, 
          "discrete" = stop("Discrete transformation not defined for numeric variable"),
          "linear" =
              polynomial_basis(yvar, coef = c(TRUE, TRUE), 
                               ui = matrix(c(0, 1), nrow = 1), ci = 0),
          "logarithmic" = 
              log_basis(yvar, ui = "increasing"),
          "smooth" = 
              Bernstein_basis(yvar, ui = "increasing", order = order,
                              extrapolate = extrapolate, log_first = log_first),
          "positive_smooth" = 
              Bernstein_basis(yvar, ui = c("positive", "increasing"), order = order,
                              extrapolate = extrapolate, log_first = log_first)
          )
      )
  }

  return(as.basis(yvar))#, data = as.data.frame(mkgrid(yvar), check.names = FALSE)))
}

