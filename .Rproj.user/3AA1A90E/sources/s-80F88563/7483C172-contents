#' @title recovery
#'
#' @description Returns sum of likelihoods based on neural data and behavioral data
#'
#' @param base_par bla
#' @param df bla
#' @param cycles bla
#' @param sigma_gen bla
#' @param sigma_mod bla
#'
#' @return A data.frame containing behavioral and/or neural data.
#' @examples
#' # simulate data based on known parameters
#' true = param_draw(base_par = c("a", "b", "t0", "sd"), n_drift  = 8, dynamic  = F)
#' nLBA  = simulate.data(sub_id    = 1,
#'                       n_blocks  = 16,
#'                       true_pars = true,
#'                       sigma_gen = 0.01)
#'
#' recovery = recovery(base_par  = c("a", "b", "t0", "sd"),
#'                     df        = nLBA,
#'                     cycles    = 500,
#'                     sigma_mod = 0.01)
#'
#' @export
#' @import rtdists


recovery <- function(base_par, df, cycles, sigma_gen = NULL, sigma_mod = NULL){

  # determine the type of data
  if ("mean_v1" %in% colnames(df)){
    dynamic = T
    n_drift = NULL
  } else{
    dynamic = F
    n_drift = length(unique(df$repetition))
  }

  # actual parameter recovery
  for (q in 1:cycles){
    o = tryCatch(optim(param_draw(base_par = base_par,                     # initial parameter guess
                                  n_drift  = n_drift,
                                  dynamic  = dynamic),
                       likelihood.summed,                                  # goal function to optimize
                       method        = "L-BFGS-B",                         # minimization method
                       rt            = df$rt,
                       response      = df$response,
                       conditions    = df$repetition,
                       wr            = df$weight_reset,
                       neural_dat    = df$neural,
                       netinput      = df$mean_v1,
                       sigma_mod     = sigma_mod,
                       lower         = rep(0, times = length(start)),      # parameter lower bound
                       upper         = rep(Inf, times = length(start)),    # parameter upper bound
                       control       = list(maxit = 5000),
                       hessian       = TRUE),
                 error = function(e){NA})
    # if o is not NA, AND if converged, AND if the LR estimate != 0 --> break
    if (length(o) > 1){
      if(o$convergence == 0){
        if(!any(o$par < 0.0001)){
          break
        }
      }
    }
  }
  if (length(o) == 1){
    return(rep(NA, times = length(start) + 2))
  } else{
    # save the lowest value of the eigen values of the Hessian
    # all should be positive (> epsilon, with epsilon = 0.01) when minimizing
    h = min(eigen(o$hessian)$values)
    return(c(o$par, o$value, h))
  }
}
