#' @title likelihood.summed
#'
#' @description Returns sum of likelihoods based on neural data and behavioral data
#'
#' @param to_optim bla
#' @param rt bla
#' @param response bla
#' @param neural_data bla
#' @param conditions bla
#' @param wr bla
#' @param netinput bla
#' @param sigma_mod bla
#'
#' @return A data.frame containing behavioral and/or neural data.
#' @examples
#' true = param_draw(base_par = c("a", "b", "t0", "sd"), n_drift  = 8, dynamic  = F)
#' nLBA  = simulate.data(sub_id   = 1,
#'                       n_blocks  = 16,
#'                       true_pars = true,
#'                       sigma_gen = 0.01)
#'
#' likelihood.summed(to_optim    = true,
#'                   rt          = nLBA$rt,
#'                   response    = nLBA$response,
#'                   neural_data = nLBA$neural,
#'                   conditions  = nLBA$repetition,
#'                   sigma_mod   = 0.01)
#'
#' @export
#' @import rtdists


likelihood.summed <- function(to_optim, rt, response, neural_data = NULL, conditions = NULL, wr = NULL,
                              netinput = NULL, sigma_mod = NULL){

  ll.behavioral = negloglik.behavioral(to_optim, rt, response, conditions, wr)

  # for non neural data, only return the behavioral loglikelihood
  if (is.null(neural_data)){
    return(ll.behavioral)
  } else{
    # for neural data, return the sum of both
    ll.neural = likelihood.neural(to_optim, neural_data, conditions, netinput)
    return(ll.behavioral + (1/(2*(sigma_mod)^2)) * ll.neural)
  }
}
