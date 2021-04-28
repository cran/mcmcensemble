## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(20210414)

## ----setup--------------------------------------------------------------------
library(mcmcensemble)

## a log-pdf to sample from
p.log <- function(x) {
  B <- 0.03 # controls 'bananacity'
  -x[1]^2 / 200 - 1/2 * (x[2] + B * x[1]^2 - 100 * B)^2
}

unif_inits <- data.frame(
  a = runif(10, min = 0, max = 1),
  b = runif(10, min = 0, max = 1)
)

## -----------------------------------------------------------------------------
library(coda)

## -----------------------------------------------------------------------------
## use stretch move, return samples as 'coda' object
res <- MCMCEnsemble(
  p.log,
  inits = unif_inits,
  max.iter = 3000, n.walkers = 10, method = "stretch", coda = TRUE
)

## -----------------------------------------------------------------------------
class(res$samples)

## -----------------------------------------------------------------------------
summary(res$samples)

## ---- eval = identical(Sys.getenv("IN_PKGDOWN"), "true")----------------------
#  plot(res$samples)

## -----------------------------------------------------------------------------
effectiveSize(res$samples)

## -----------------------------------------------------------------------------
library(bayesplot)

## -----------------------------------------------------------------------------
res_nocoda <- MCMCEnsemble(
  p.log,
  inits = unif_inits,
  max.iter = 3000, n.walkers = 10, method = "stretch", coda = FALSE
)

res_coda <- MCMCEnsemble(
  p.log,
  inits = unif_inits,
  max.iter = 3000, n.walkers = 10, method = "stretch", coda = TRUE
)

## ---- out.width='45%', fig.show='hold'----------------------------------------
# Density of log-posterior of each parameter
mcmc_areas(res_nocoda$samples)
mcmc_areas(res_coda$samples)

mcmc_dens(res_nocoda$samples)
mcmc_dens(res_coda$samples)

# All the sample points in the parameter space
mcmc_scatter(res_nocoda$samples)
mcmc_scatter(res_coda$samples)

## -----------------------------------------------------------------------------
mcmc_trace(res_coda$samples)

## -----------------------------------------------------------------------------
mcmc_dens(res_coda$samples) + 
  overlay_function(fun = "dunif", geom = "density", color = "red", fill = "darkred", alpha = 0.5)

