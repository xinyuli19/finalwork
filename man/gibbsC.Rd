% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{gibbsC}
\alias{gibbsC}
\title{A Gibbs sampler using Rcpp}
\usage{
gibbsC(a, b, n, initial, N)
}
\arguments{
\item{a}{the first parameter in rbeta(a,b)}

\item{b}{the second parameter in rbeta(a,b)}

\item{n}{the parameter in rbinom(1,n,p)}

\item{initial}{the initial values of chain}

\item{N}{the number of samples}
}
\value{
a random sample
}
\description{
A Gibbs sampler using Rcpp
}
\examples{
\dontrun{
N_0 <- 10000;burn <- 1000
a_0<-3;b_0<-4;n_0<-20
init<-c(sample(0:n_0,1),runif(1))
matC <- gibbsC(a_0,b_0,n_0,init,N_0)
}
}
