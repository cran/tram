
library("tram")
library("mvtnorm")
library("multcomp")

options(digits = 2)

set.seed(25)
chk <- function(...) all.equal(..., tol = 1e-3, check.attributes = FALSE)

J <- 4
N <- 100
S <- cov2cor(tcrossprod(matrix(runif(J * J), ncol = J)))
y <- rmvnorm(N, sigma = S)
u <- as.data.frame(plogis(y))
x <- runif(N)
d <- cbind(u, x)
un <- colnames(d)[1:J]

m <- lapply(un, function(i)
    BoxCox(as.formula(paste(i, "~ x")), data = d, bounds = c(0, 1), support = c(0, 1)))
m$data <- d
m$formula <- ~ 1
mm <- do.call("mmlt", m)

chk(c(logLik(mm)), sum(predict(mm, newdata = d, type = "density", log = TRUE)))
L <- as.array(coef(mm, type = "Lambda"))[,,1]
chk(as.array(coef(mm, type = "Lambdainv"))[,,1], solve(L))
chk(as.array(coef(mm, type = "Sigma"))[,,1], tcrossprod(solve(L)))
chk(as.array(coef(mm, type = "Cor"))[,,1], cov2cor(tcrossprod(solve(L))))

### marginal normal
m$conditional <- FALSE
mmN <- do.call("mmlt", m)

chk(logLik(mm), logLik(mmN))
chk(c(logLik(mmN)), sum(predict(mmN, newdata = d, type = "density", log = TRUE)))

cf1 <- do.call("c", lapply(m[1:J], function(x) coef(as.mlt(x))))
cf2 <- coef(mmN)[1:length(cf1)]
cbind(cf1, cf2)

sd1 <- sqrt(do.call("c", lapply(m[1:J], function(x) diag(vcov(as.mlt(x))))))
sd2 <- sqrt(diag(vcov(mmN)))[1:length(sd1)]

cbind(sd1, sd2)
vcov(mmN)["V1.x", "V4.x"]


chk(as.array(coef(mm, type = "Lambda"))[,,1], 
    as.array(coef(mmN, type = "Lambda"))[,,1])
chk(as.array(coef(mm, type = "Lambdainv"))[,,1], 
    as.array(coef(mmN, type = "Lambdainv"))[,,1])
chk(as.array(coef(mm, type = "Sigma"))[,,1], 
    as.array(coef(mmN, type = "Sigma"))[,,1])
chk(as.array(coef(mm, type = "Spearman"))[,,1], 
    as.array(coef(mmN, type = "Spearman"))[,,1])

chk(predict(mm, newdata = d, type = "density", log = TRUE), 
    predict(mmN, newdata = d, type = "density", log = TRUE))
chk(predict(mm, newdata = d, type = "distribution", log = TRUE), 
    predict(mmN, newdata = d, type = "distribution", log = TRUE))

chk(predict(mm, newdata = d, margins = 1:2, type = "density", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:2, type = "density", log = TRUE))
chk(predict(mm, newdata = d, margins = 1:2, type = "distribution", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:2, type = "distribution", log = TRUE))
chk(predict(mm, newdata = d, margins = 1:3, type = "density", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:3, type = "density", log = TRUE))
chk(predict(mm, newdata = d, margins = 1:3, type = "distribution", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:3, type = "distribution", log = TRUE))

chk(sapply(1:J, function(i) predict(mm, margins = i, newdata = d, type = "density", log = TRUE)),
    sapply(1:J, function(i) predict(mmN, margins = i, newdata = d, type = "density", log = TRUE)))

### check marginal predictions
m1 <- m[[1]]
m2 <- do.call("mmlt", m[-(3:4)])
m3 <- do.call("mmlt", m[-4])

chk(c(predict(m1, newdata = d, type = "density", log = TRUE)), 
    c(predict(mmN, newdata = d, margins = 1, type = "density", log = TRUE)))
chk(predict(m2, newdata = d, margins = 1:2, type = "density", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:2, type = "density", log = TRUE))
chk(predict(m2, newdata = d, margins = 1:2, type = "distribution", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:2, type = "distribution", log = TRUE))
chk(predict(m3, newdata = d, margins = 1:3, type = "density", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:3, type = "density", log = TRUE))
chk(predict(m3, newdata = d, margins = 1:3, type = "distribution", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:3, type = "distribution", log = TRUE))

### marginal normal, implemented differently
for (j in 1:J) m[[j]]$todistr$name <- "CarlFriedrich"
mmN <- do.call("mmlt", m)

chk(logLik(mm), logLik(mmN))
chk(c(logLik(mmN)), sum(predict(mmN, newdata = d, type = "density", log = TRUE)))

cf1 <- do.call("c", lapply(m[1:J], function(x) coef(as.mlt(x))))
cf2 <- coef(mmN)[1:length(cf1)]
cbind(cf1, cf2)

sd1 <- sqrt(do.call("c", lapply(m[1:J], function(x) diag(vcov(as.mlt(x))))))
sd2 <- sqrt(diag(vcov(mmN)))[1:length(sd1)]

cbind(sd1, sd2)
vcov(mmN)["V1.x", "V4.x"]

chk(as.array(coef(mm, type = "Lambda"))[,,1], 
    as.array(coef(mmN, type = "Lambda"))[,,1])
chk(as.array(coef(mm, type = "Lambdainv"))[,,1], 
    as.array(coef(mmN, type = "Lambdainv"))[,,1])
chk(as.array(coef(mm, type = "Sigma"))[,,1], 
    as.array(coef(mmN, type = "Sigma"))[,,1])
chk(as.array(coef(mm, type = "Spearman"))[,,1], 
    as.array(coef(mmN, type = "Spearman"))[,,1])

chk(predict(mm, newdata = d, type = "density", log = TRUE), 
    predict(mmN, newdata = d, type = "density", log = TRUE))
chk(predict(mm, newdata = d, type = "distribution", log = TRUE), 
    predict(mmN, newdata = d, type = "distribution", log = TRUE))

chk(predict(mm, newdata = d, margins = 1:2, type = "density", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:2, type = "density", log = TRUE))
chk(predict(mm, newdata = d, margins = 1:2, type = "distribution", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:2, type = "distribution", log = TRUE))
chk(predict(mm, newdata = d, margins = 1:3, type = "density", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:3, type = "density", log = TRUE))
chk(predict(mm, newdata = d, margins = 1:3, type = "distribution", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:3, type = "distribution", log = TRUE))

chk(sapply(1:J, function(i) predict(mm, margins = i, newdata = d, type = "density", log = TRUE)),
    sapply(1:J, function(i) predict(mmN, margins = i, newdata = d, type = "density", log = TRUE)))

### marginal Colr models
m <- lapply(un, function(i)
    Colr(as.formula(paste(i, "~ x")), data = d, bounds = c(0, 1), support = c(0, 1)))
m$data <- d
m$formula <- ~ 1
mmC <- do.call("mmlt", m)

chk(c(logLik(mmC)), sum(predict(mmC, newdata = d, type = "density", log = TRUE)))
logLik(mmC)

### conditional models
m <- lapply(un, function(i)
    BoxCox(as.formula(paste(i, "~ x")), data = d, bounds = c(0, 1), support = c(0, 1)))
m$data <- d
m$formula <- ~ x
mm <- do.call("mmlt", m)

chk(c(logLik(mm)), sum(predict(mm, newdata = d, type = "density", log = TRUE)))
L <- as.array(coef(mm, newdata = d, type = "Lambda"))[,,1]
chk(as.array(coef(mm, newdata = d, type = "Lambdainv"))[,,1], solve(L))
chk(as.array(coef(mm, newdata = d, type = "Sigma"))[,,1], tcrossprod(solve(L)))
chk(as.array(coef(mm, newdata = d, type = "Cor"))[,,1], cov2cor(tcrossprod(solve(L))))

### with marginal parameterisation
m$conditional <- FALSE
mmN <- do.call("mmlt", m)

chk(logLik(mm), logLik(mmN))
chk(c(logLik(mmN)), sum(predict(mmN, newdata = d, type = "density", log = TRUE)))

chk(as.array(coef(mm, newdata = d, type = "Lambda"))[,,1], 
    as.array(coef(mmN, newdata = d, type = "Lambda"))[,,1])
chk(as.array(coef(mm, newdata = d, type = "Lambdainv"))[,,1], 
    as.array(coef(mmN, newdata = d, type = "Lambdainv"))[,,1])
chk(as.array(coef(mm, newdata = d, type = "Sigma"))[,,1], 
    as.array(coef(mmN, newdata = d, type = "Sigma"))[,,1])
chk(as.array(coef(mm, newdata = d, type = "Spearman"))[,,1], 
    as.array(coef(mmN, newdata = d, type = "Spearman"))[,,1])

chk(predict(mm, newdata = d, type = "density", log = TRUE), 
    predict(mmN, newdata = d, type = "density", log = TRUE))
chk(predict(mm, newdata = d, type = "distribution", log = TRUE), 
    predict(mmN, newdata = d, type = "distribution", log = TRUE))

chk(predict(mm, newdata = d, margins = 1:2, type = "density", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:2, type = "density", log = TRUE))
chk(predict(mm, newdata = d, margins = 1:2, type = "distribution", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:2, type = "distribution", log = TRUE))
chk(predict(mm, newdata = d, margins = 1:3, type = "density", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:3, type = "density", log = TRUE))
chk(predict(mm, newdata = d, margins = 1:3, type = "distribution", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:3, type = "distribution", log = TRUE))

chk(sapply(1:J, function(i) predict(mm, margins = i, newdata = d, type = "density", log = TRUE)),
    sapply(1:J, function(i) predict(mmN, margins = i, newdata = d, type = "density", log = TRUE)))

### implemented differently
for (j in 1:J) m[[j]]$todistr$name <- "CarlFriedrich"
mmN <- do.call("mmlt", m)

chk(logLik(mm), logLik(mmN))
chk(c(logLik(mmN)), sum(predict(mmN, newdata = d, type = "density", log = TRUE)))

chk(as.array(coef(mm, newdata = d, type = "Lambda"))[,,1], 
    as.array(coef(mmN, newdata = d, type = "Lambda"))[,,1])
chk(as.array(coef(mm, newdata = d, type = "Lambdainv"))[,,1], 
    as.array(coef(mmN, newdata = d, type = "Lambdainv"))[,,1])
chk(as.array(coef(mm, newdata = d, type = "Sigma"))[,,1], 
    as.array(coef(mmN, newdata = d, type = "Sigma"))[,,1])
chk(as.array(coef(mm, newdata = d, type = "Spearman"))[,,1], 
    as.array(coef(mmN, newdata = d, type = "Spearman"))[,,1])

chk(predict(mm, newdata = d, type = "density", log = TRUE), 
    predict(mmN, newdata = d, type = "density", log = TRUE))
chk(predict(mm, newdata = d, type = "distribution", log = TRUE), 
    predict(mmN, newdata = d, type = "distribution", log = TRUE))

chk(predict(mm, newdata = d, margins = 1:2, type = "density", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:2, type = "density", log = TRUE))
chk(predict(mm, newdata = d, margins = 1:2, type = "distribution", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:2, type = "distribution", log = TRUE))
chk(predict(mm, newdata = d, margins = 1:3, type = "density", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:3, type = "density", log = TRUE))
chk(predict(mm, newdata = d, margins = 1:3, type = "distribution", log = TRUE), 
    predict(mmN, newdata = d, margins = 1:3, type = "distribution", log = TRUE))

chk(sapply(1:J, function(i) predict(mm, margins = i, newdata = d, type = "density", log = TRUE)),
    sapply(1:J, function(i) predict(mmN, margins = i, newdata = d, type = "density", log = TRUE)))

### conditional Colr
m <- lapply(un, function(i)
    Colr(as.formula(paste(i, "~ x")), data = d, bounds = c(0, 1), support = c(0, 1)))
m$data <- d
m$formula <- ~ x
mmC <- do.call("mmlt", m)

chk(c(logLik(mmC)), sum(predict(mmC, newdata = d, type = "density", log = TRUE)))
logLik(mmC)

##### FIRST SCENARIO: CONSTANT LAMBDA #####
set.seed(290875)
ll <- numeric(50)
p <- 3
X <- matrix(runif(N * p), ncol = p)
m1 <- 1 + X %*% c(2, 1, 1)
m2 <- 1 + X %*% c(1, 2, 1)
lb <- (off <- 0.5) + X %*% (cf <- c(0, 2, 0))
d <- data.frame(X)
Y <- matrix(NA, nrow = N, ncol = 2)
colnames(Y) <- c("Y1", "Y2")

cr <- numeric(N)
for (i in 1:N) {
  Si <- diag(2)
  Si[1,2] <- Si[2,1] <- .5
  cr[i] <- cov2cor(Si)[2,1]
  
  Y[i,] <- rmvnorm(1, mean = c(m1[i], m2[i]), sigma = Si)
}


##### only BoxCox margins: ##### 
d <- cbind(d, Y)
b1 <- as.mlt(Lm(Y1 ~ X1 + X2 + X3, data = d))
b2 <- as.mlt(Lm(Y2 ~ X1 + X2 + X3, data = d))

## constant correlations. expect identical logliks and lambda parameters
mm01 <- mmlt(b1, b2, formula = ~ 1, data = d)
mm02 <- mmlt(b2, b1, formula = ~ 1, data = d)

chk(logLik(mm01), logLik(mm02))

chk(c(coef(mm01)["Y2.Y1.(Intercept)"]), 
    c(coef(mm02)["Y1.Y2.(Intercept)"]))

## checking gradients
chk(c(numDeriv::grad(mm01$ll, mm02$par)),
    c(mm01$sc(mm02$par)))

## predicting marginal distributions and comparing across models with constant lambda
predict(mm01, newdata = d[1:5,], q = -2:2, 
        margins = 1, type = "distribution")
predict(mm02, newdata = d[1:5,], q = -2:2, 
        margins = 2, type = "distribution")

## expect correlations to be the same for the model with constant lambdas
chk(c(coef(mm01, newdata = d[1:5,], type = "Cor")), 
    c(coef(mm02, newdata = d[1:5,], type = "Cor")))


##### mix of BoxCox and Colr margins: ##### 
d$Y1 <- (d$Y1 - min(d$Y1))/(max(d$Y1) - min(d$Y1))

b1 <- as.mlt(Colr(Y1 ~ X1 + X2 + X3, data = d, order = 1))
b2 <- as.mlt(Lm(Y2 ~ X1 + X2 + X3, data = d))

mm01 <- mmlt(b1, b2, formula = ~ 1, data = d)
mm02 <- mmlt(b2, b1, formula = ~ 1, data = d)

chk(logLik(mm01), logLik(mm02))

### model is marginal, so expect same marginal coeff
cf1 <- coef(mm01)
cf1 <- cf1[-length(cf1)]
cf2 <- coef(mm02)
cf2 <- cf2[names(cf1)]
chk(cf1, cf2)

## checking gradient
chk(c(numDeriv::grad(mm01$ll, coef(mm01))), c(mm01$sc(coef(mm01))))
chk(c(numDeriv::grad(mm02$ll, coef(mm02))), c(mm02$sc(coef(mm02)))) 

##### SECOND SCENARIO: COVARIATE DEPENDENT LAMBDA #####
set.seed(290875)
ll <- numeric(50)
X <- matrix(runif(N * p), ncol = p)
m1 <- 1 + X %*% c(2, 1, 1)
m2 <- 1 + X %*% c(1, 2, 1)
lb <- (off <- 0.5) + X %*% (cf <- c(0, 2, 0))
d <- data.frame(X)
Y <- matrix(NA, nrow = N, ncol = 2)
colnames(Y) <- c("Y1", "Y2")

cr <- numeric(N)
for (i in 1:N) {
  L <- diag(2)
  L[2,1] <- lb[i]
  Si <- solve(L) %*% t(solve(L))
  cr[i] <- cov2cor(Si)[2,1]
  
  Y[i,] <- rmvnorm(1, mean = c(m1[i], m2[i]), sigma = Si)
}


##### only BoxCox margins: ##### 
d <- cbind(d, Y)
b1 <- as.mlt(Lm(Y1 ~ X1 + X2 + X3, data = d))
b2 <- as.mlt(Lm(Y2 ~ X1 + X2 + X3, data = d))

## constant correlations. expect identical logliks and lambda parameters
mm01 <- mmlt(b1, b2, formula = ~ 1, data = d)
mm02 <- mmlt(b2, b1, formula = ~ 1, data = d)

chk(logLik(mm01), logLik(mm02))

## x-dependent correlations. expect slightly different logliks
mm1 <- mmlt(b1, b2, formula = ~ X1 + X2 + X3, data = d)
mm2 <- mmlt(b2, b1, formula = ~ X1 + X2 + X3, data = d)

logLik(mm1)
logLik(mm2)

## checking gradients
chk(c(numDeriv::grad(mm01$ll, mm02$par)),c(mm01$sc(mm02$par)))
chk(c(numDeriv::grad(mm1$ll, mm2$par)),c(mm1$sc(mm2$par)))

### BUT
mm1 <- mmlt(b1, b2, formula = ~ X1 + X2 + X3, data = d, conditional = FALSE)
mm2 <- mmlt(b2, b1, formula = ~ X1 + X2 + X3, data = d, conditional = FALSE)

logLik(mm1)
logLik(mm2)

## predicting marginal distributions and comparing across models with constant lambda
x <- 0:4 / 4
nd <- expand.grid(X1 = x, X2 = x, X3 = x)
predict(mm01, newdata = nd[1:5,], q = -2:2, 
        margins = 1, type = "distribution")
predict(mm02, newdata = nd[1:5,], q = -2:2, 
        margins = 2, type = "distribution")

predict(mm1, newdata = nd[1:5,], q = -2:2, 
        margins = 1, type = "distribution")
predict(mm2, newdata = nd[1:5,], q = -2:2, 
        margins = 2, type = "distribution")

## expect correlations to be the same for the model with constant lambdas
chk(c(coef(mm01, newdata = nd[1:5,], type = "Cor")), 
    c(coef(mm02, newdata = nd[1:5,], type = "Cor")))

## correlations for models with x-dependent lambda
c(coef(mm1, newdata = nd[1:5,], type = "Cor"))
c(coef(mm2, newdata = nd[1:5,], type = "Cor"))


##### mix of BoxCox and Colr margins: ##### 
d$Y1 <- (d$Y1 - min(d$Y1))/(max(d$Y1) - min(d$Y1))

b1 <- as.mlt(Colr(Y1 ~ X1 + X2 + X3, data = d, order = 1))
b2 <- as.mlt(Lm(Y2 ~ X1 + X2 + X3, data = d))

mm01 <- mmlt(b1, b2, formula = ~ 1, data = d)
mm02 <- mmlt(b2, b1, formula = ~ 1, data = d)

chk(logLik(mm01), logLik(mm02))

coef(b1)
coef(b2)
coef(mm01)
coef(mm02)
# remember that: lb <- (off <- 0.5) + X %*% (cf <- c(0, 2, 0))


## checking gradient
chk(c(numDeriv::grad(mm01$ll, coef(mm01))),c(mm01$sc(coef(mm01))))
chk(c(numDeriv::grad(mm02$ll, coef(mm02))),c(mm02$sc(coef(mm02))))

## covariate-dependent Lambda
mm1 <- mmlt(b1, b2, formula = ~ X1 + X2 + X3, data = d)
mm2 <- mmlt(b2, b1, formula = ~ X1 + X2 + X3, data = d)
logLik(mm1)
logLik(mm2)

coef(b1)
coef(b2)
coef(mm1)
coef(mm2)
# remember that: lb <- (off <- 0.5) + X %*% (cf <- c(0, 2, 0))

## checking gradient for diag = TRUE
chk(c(numDeriv::grad(mm1$ll, coef(mm1))),c(mm1$sc(coef(mm1))))
chk(c(numDeriv::grad(mm2$ll, coef(mm2))),c(mm2$sc(coef(mm2))))

### very simple checks with marginal Lm models
set.seed(290875)
J <- 4
S <- cov2cor(tcrossprod(matrix(runif(J * J), ncol = J)))
x <- matrix(runif(N*2), ncol = 2)

y <- x %*% matrix(c(1, -1, -.5, .5, -.2, .2, .3, -.3), nrow = 2) + rmvnorm(N, sigma = S)
d <- data.frame(y = y, x = x)

m1 <- Lm(y.1 ~ x.1 + x.2, data = d)
m2 <- Lm(y.2 ~ x.1 + x.2, data = d)
m3 <- Lm(y.3 ~ x.1 + x.2, data = d)
m4 <- Lm(y.4 ~ x.1 + x.2, data = d)

## simple formula
mc01 <- mmlt(m1, m2, m3, m4, formula = ~ 1, data = d, conditional = FALSE)

cf <- coef(mc01)
vr <- diag(vcov(mc01))
i <- grep("x", names(cf))

### same results
ret <- cbind(c(coef(m1), coef(m2), coef(m3), coef(m4)),
             cf[i],
             c(diag(vcov(m1)), diag(vcov(m2)), diag(vcov(m3)), diag(vcov(m4))),
             vr[i])
ret

vc <- vcov(mc01)
i <- grep("x.1", colnames(vc))
vc[i,i]

summary(g1 <- glht(mmm(m1 = as.mlt(m1), m2 = as.mlt(m2), m3 = as.mlt(m3), m4 = as.mlt(m4)), mlf("x.1 = 0")))

summary(g2 <- glht(mc01, c("y.1.x.1 = 0", "y.2.x.1 = 0", "y.3.x.1 = 0", "y.4.x.1 = 0")))

vcov(g1)
vcov(g2)


#### check density
Shat <- as.array(coef(mc01, type = "Cor"))[,,1]

int <- cf[paste("y", 1:J, "(Intercept)", sep = ".")]
fct <- cf[paste("y", 1:J, "y", 1:J, sep = ".")]

d1 <- sapply(1:N, function(i) dmvnorm(int + fct * y[i,], mean = x[i,,drop = FALSE] %*% matrix(ret[,1], nrow = 2), sigma = Shat, log = TRUE))
d2 <- predict(mc01, newdata = d, type = "density", log = TRUE)

chk(c(d1), c(d2))

chk(c(logLik(mmlt(m1, m2, m3, m4, formula = ~ 1, data = d))),
    c(logLik(mc01)))
chk(c(logLik(mc01)),
    sum(d2))
