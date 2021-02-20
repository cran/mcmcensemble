## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(mcmcensemble)

## ---- eval = require("coda")--------------------------------------------------
## a log-pdf to sample from
p.log <- function(x) {
  B <- 0.03 # controls 'bananacity'
  -x[1]^2 / 200 - 1 / 2 * (x[2] + B * x[1]^2 - 100 * B)^2
}

set.seed(20201209)

res1 <- MCMCEnsemble(
  p.log,
  lower.inits = c(a = -10, b = -10), upper.inits = c(a = -5, b = -5),
  max.iter = 3000, n.walkers = 10,
  method = "stretch",
  coda = TRUE
)

summary(res1$samples)

## ---- eval = identical(Sys.getenv("IN_PKGDOWN"), "true")----------------------
#  plot(res1$samples)

## -----------------------------------------------------------------------------
res2 <- MCMCEnsemble(
  p.log,
  lower.inits = c(a = -10, b = -10), upper.inits = c(a = -5, b = -5),
  max.iter = 3000, n.walkers = 10,
  method = "differential.evolution",
  coda = TRUE
)

summary(res2$samples)

## ---- eval = identical(Sys.getenv("IN_PKGDOWN"), "true")----------------------
#  plot(res2$samples)

## -----------------------------------------------------------------------------
p.log.restricted <- function(x) {
  
  if (any(x < 0, x > 1)) {
    return(-Inf)
  }
  
  B <- 0.03 # controls 'bananacity'
  -x[1]^2 / 200 - 1 / 2 * (x[2] + B * x[1]^2 - 100 * B)^2
}

res <- MCMCEnsemble(
  p.log.restricted,
  lower.inits = c(a = 0, b = 0), upper.inits = c(a = 1, b = 1),
  max.iter = 3000, n.walkers = 10,
  method = "stretch",
  coda = TRUE
)
summary(res$samples)

## ---- eval = identical(Sys.getenv("IN_PKGDOWN"), "true")----------------------
#  plot(res$samples)

