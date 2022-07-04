#' @title likelihood.summed
#'
#' @description Returns sum of likelihoods based on passed data. Note that
#'     both behavioral and neural data can be inputted to this function, but
#'     the function also works with one type of data modality. When only
#'     behavioral data is passed (reaction times and choices), the likelihood
#'     based on this data is returned. Similarly, the likeilihood based on the
#'     neural data is passed when only this type of data is provided.
#'
#' @param to_optim The likelihood for parameter set to_optim is calculated.
#' @param dataset Optional: only when empirical data is available.
#'     This allows data to be generated relying on stimuli actually seen by
#'     subjects.
#' @param sigma_mod Optional: only needed in dynamic models.
#'     Neural data that is fed to the optimization algorithm has a certain
#'     'generative' process of which the 'generative variance' is unknown. For
#'     instance, we assume that the neural data comes from a normal distribution with
#'     mean mu and standard deviation sigma. When performing MLE, this sigma gen
#'     is unknown, and we have to assume a generative variance in our model. This
#'     assumed generative variance is called sigma_mod, and is needed to determine
#'     the weight that is given to the neural likelihood in the total sum of
#'     likelihoods.
#'
#' @return numeric value indicating the likelihood of a parameter set given
#'     the available behavioral and/or neural data.
#' @examples
#' require(labdance)
#'
#' # dynamic LBA
#' true = param.draw(base_par = c("a", "b", "t0", "sd", "beta"),
#'                   n_drift  = NULL,
#'                   dynamic  = T)
#' simulated = simulate.data(true_pars = true,
#'                           dataset   = NULL,
#'                           sigma_gen = NULL)
#' ll.s = likelihood.summed(to_optim = true,
#'                          dataset  = simulated)
#' ll.b = likelihood.behavioral(to_optim = true,
#'                             dataset  = simulated)
#' # summed LL should be equal to the behavioral LL
#' stopifnot(ll.s == ll.b)
#'
#' # dynamic neural LBA
#' simulated = simulate.data(true_pars = true,
#'                           dataset   = NULL,
#'                           sigma_gen = 0.01)
#' ll.b = likelihood.behavioral(to_optim = true,
#'                             dataset  = simulated)
#' ll.n = likelihood.neural(to_optim = true,
#'                          dataset  = simulated)
#' ll.s = likelihood.summed(to_optim  = true,
#'                          dataset   = simulated,
#'                          sigma_mod = 0.01)
#' # summed LL should be equal to the sum of behavioral LL and neural LL times a constant
#' stopifnot(ll.s == (ll.b + (1/(2*(0.01)^2)) * ll.n))
#'
#' @export
#' @import rtdists


likelihood.summed <- function(to_optim,
                              dataset   = NULL,
                              sigma_mod = NULL){

  ll.behavioral = likelihood.behavioral(to_optim, dataset)

  # for non neural data, only return the behavioral loglikelihood
  if (is.null(dataset$neural)){
    return(ll.behavioral)
  } else{
    # for neural data, return the sum of both
    ll.neural = likelihood.neural(to_optim, dataset)
    return(ll.behavioral + (1/(2*(sigma_mod)^2)) * ll.neural)
  }
}
