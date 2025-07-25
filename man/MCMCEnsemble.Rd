% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MCMCEnsemble.R
\name{MCMCEnsemble}
\alias{MCMCEnsemble}
\title{MCMC ensemble sampler}
\usage{
MCMCEnsemble(
  f,
  inits,
  max.iter,
  n.walkers = 10 * ncol(inits),
  method = c("stretch", "differential.evolution"),
  coda = FALSE,
  ...
)
}
\arguments{
\item{f}{function that returns a single scalar value proportional to the log
probability density to sample from.}

\item{inits}{A matrix (or data.frame) containing the starting values for the
walkers. Each column is a variable to estimate and each row is a walker}

\item{max.iter}{maximum number of function evaluations}

\item{n.walkers}{number of walkers (ensemble size). An integer greater than
\code{max(3, d+1)} for stretch move and greater than \code{max(4, d+2)} for
differential evolution where \code{d == ncol(inits)}.}

\item{method}{method for proposal generation, either \code{"stretch"}, or
\code{"differential.evolution"}. This argument will be saved as an attribute
in the output (see examples).}

\item{coda}{logical. Should the samples be returned as \link[coda:mcmc.list]{coda::mcmc.list}
object? (defaults to \code{FALSE})}

\item{...}{further arguments passed to \code{f}}
}
\value{
\itemize{
\item if \code{coda = FALSE} a list with:
\itemize{
\item \emph{samples}: A three dimensional array of samples with dimensions \code{walker}
x \code{generation} x \code{parameter}
\item \emph{log.p}: A matrix with the log density evaluate for each walker at each
generation.
}
\item if \code{coda = TRUE} a list with:
\itemize{
\item \emph{samples}: A object of class \link[coda:mcmc.list]{coda::mcmc.list} containing all samples.
\item \emph{log.p}: A matrix with the log density evaluate for each walker at each
generation.
}
}

In both cases, there is an additional attribute (accessible via
\code{attr(res, "ensemble.sampler")}) recording which ensemble sampling algorithm
was used.
}
\description{
Ensemble Markov Chain Monte Carlo sampler with different strategies to
generate proposals. Either the \emph{stretch move} as proposed by Goodman and
Weare (2010), or a \emph{differential evolution jump move} similar to Braak and
Vrugt (2008).
}
\examples{
## a log-pdf to sample from
p.log <- function(x) {
    B <- 0.03                              # controls 'bananacity'
    -x[1]^2/200 - 1/2*(x[2]+B*x[1]^2-100*B)^2
}

## set options and starting point
n_walkers <- 10
unif_inits <- data.frame(
  "a" = runif(n_walkers, 0, 1),
  "b" = runif(n_walkers, 0, 1)
)


## use stretch move
res1 <- MCMCEnsemble(p.log, inits = unif_inits,
                     max.iter = 150, n.walkers = n_walkers,
                     method = "stretch")

attr(res1, "ensemble.sampler")

str(res1)


## use stretch move, return samples as 'coda' object
res2 <- MCMCEnsemble(p.log, inits = unif_inits,
                     max.iter = 150, n.walkers = n_walkers,
                     method = "stretch", coda = TRUE)

attr(res2, "ensemble.sampler")

summary(res2$samples)
plot(res2$samples)


## use different evolution move, return samples as 'coda' object
res3 <- MCMCEnsemble(p.log, inits = unif_inits,
                     max.iter = 150, n.walkers = n_walkers,
                     method = "differential.evolution", coda = TRUE)

attr(res3, "ensemble.sampler")

summary(res3$samples)
plot(res3$samples)

}
\references{
\itemize{
\item ter Braak, C. J. F. and Vrugt, J. A. (2008) Differential Evolution Markov
Chain with snooker updater and fewer chains. Statistics and Computing, 18(4),
435–446, \doi{10.1007/s11222-008-9104-9}
\item Goodman, J. and Weare, J. (2010) Ensemble samplers with affine invariance.
Communications in Applied Mathematics and Computational Science, 5(1), 65–80,
\doi{10.2140/camcos.2010.5.65}
}
}
