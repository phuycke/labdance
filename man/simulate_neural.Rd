% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_neural.R
\name{simulate_neural}
\alias{simulate_neural}
\title{simulate_neural}
\usage{
simulate_neural(
  sub_id = 1,
  n_blocks = 16,
  true_pars = NULL,
  sigma_gen = NULL,
  dataset = NULL
)
}
\arguments{
\item{sub_id}{The subject identifier. Can be used to generate different
datasets that each belong to a different "subject".}

\item{n_blocks}{Determines the number of experimental blocks within a
dataset.}

\item{true_pars}{The LBA parameters used to generate the data.}

\item{sigma_gen}{Optional: only for neural models.
The variance in the generation of the true neural data.}

\item{dataset}{Optional: only when empirical data is available.
Replaces the stimuli and repetitions by information observed in
the data. Hence, this allows data to be generated relying on stimuli
actually seen by subjects.}
}
\value{
data.frame containing behavioral and neural data.
}
\description{
Helper function called by simulate.data() whenever data from
    a neural model (nLBA, dnLBA) needs to be generated.
}
\examples{
require(labdance)

set.seed(2022)

# load prepared empirical data
data("data_neural")
data_neural$block_nr = rep(1:32, nrow(data_neural) / 32)
# get LBA parameters (8 drift rates)
true = param_draw(base_par = c("a", "b", "t0", "sd"),
                  n_drift  = 8,
                  dynamic  = FALSE)

# simulate data retaining the stimulus order shown to subject 2,
# and add neural data
simulated = simulate_neural(true_pars = true,
                            sigma_gen = 0.01,
                            dataset   = data_neural)
head(simulated)

#   sub_id stim repetition block_nr       rt response    neural
# 1      1    1          1        1 2.076967        1 0.3554553
# 2      1    1          2        1 1.898463        2 0.3688368
# 3      1    3          1        1 1.274176        2 0.3277951
# 4      1    4          1        1 1.335997        2 0.3238378
# 5      1    2          1        1 1.757728        2 0.3281104
# 6      1    3          2        1 1.572040        2 0.3861202

# check stimulus order
all(data_neural$stim == simulated$stim)
# [1] TRUE

}
