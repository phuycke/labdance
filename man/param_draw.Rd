% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/param_draw.R
\name{param_draw}
\alias{param_draw}
\title{param_draw}
\usage{
param_draw(base_par = c("a", "b", "t0", "sd"), n_drift = NULL, dynamic = FALSE)
}
\arguments{
\item{base_par}{The parameters that a user wants to generate.
Choice between a (starting point of evidence accumulation k is drawn
from U[0, A]), b (bound), t0 (non decision time), sd (standard deviation)
of the drift rate v (v_i ~ N(M, sd)) and beta (learning rate). Note that
one cannot draw drift rates and set dynamic to TRUE: if a model is
dynamic, the drift rates are replaced by a single parameter beta, and
hence no other drift rates can be drawn.If no base_par is supplied,
all available parameters are outputted.}

\item{n_drift}{Optional: only needed in non dynamic models.
The number of drift rates (i.e. one for each accumulator)}

\item{dynamic}{Optional: only needed in dynamic models.
Boolean indicating whether the model is dynamic or not.}
}
\value{
numeric containing named values
}
\description{
Generates (d)(n)LBA parameters within a reasonable range.
}
\examples{
require(labdance)

set.seed(2022)

param_draw(base_par = c("a", "b", "t0", "sd"),
           n_drift  = NULL,
           dynamic  = TRUE)

#         a         b        t0        sd
# 0.6119832 1.2354445 0.3101643 0.2719001

}
