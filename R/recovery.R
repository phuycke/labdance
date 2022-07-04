#' @title recovery
#'
#' @description Optimization function that attempts to find the parameter set that
#'    fits the passed data best. Automatically determines whether passed data comes
#'    from a dynamic or non dynamic model. Minimizes the (summed) likelihood using
#'    a box constraints approach ("L-BFGS-B" algorithm (Byrd et al., 1995)).
#' @param base_par The parameters that need to be optimized. For instance, when
#'    all parameters in the dynamic neural LBA need to be optimized, c("a", "b",
#'    "t0", "sd") would need to be passed to base_par.
#' @param dataset The dataset containing the data used in optimization. Contains either
#'     behavioral data (reaction times, choice), neural data or both.
#' @param cycles The amount of 'tries' that the optimization method has. More
#'     precisely, recovery will attempt MLE for a total of cycles tries.
#' @param sigma_mod Optional: only needed in neural models.
#'     The assumed variance of the generation of the neural data, since the
#'     true value (sigma_gen) is typically unknown in real life applications.
#'
#' @return numeric containing named values
#' @examples
#' require(labdance)
#'
#' set.seed(2022)
#'
#' # dynamic LBA
#' true = param.draw(base_par = c("a", "b", "t0", "sd", "beta"),
#'                   n_drift  = NULL,
#'                   dynamic  = T)
#' simulated = simulate.data(true_pars = true,
#'                           dataset   = NULL)
#' \dontrun{
#'   recovered = recovery(base_par = c("a", "b", "t0", "sd", "beta"),
#'                        dataset  = simulated)
#' }
#' print(rbind(true, recovered[1:length(true)]))
#'
#' #              a        b        t0        sd      beta
#' # true 0.6119832 1.235444 0.3101643 0.2719001 0.1847300
#' #      0.6795545 1.272554 0.3556901 0.2720122 0.2504881
#'
#' # neural LBA
#' true = param.draw(base_par = c("a", "b", "t0", "sd"),
#'                   n_drift  = 8,
#'                   dynamic  = F)
#' simulated = simulate.data(true_pars = true,
#'                           dataset   = NULL,
#'                           sigma_gen = 0.01)
#' \dontrun{
#'   recovered = recovery(base_par  = c("a", "b", "t0", "sd"),
#'                       dataset   = simulated,
#'                        sigma_mod = 0.01)
#' }
#' print(rbind(true, recovered[1:length(true)]))
#'
#' #              a        b        t0        sd       v_1       v_2       v_3       v_4       v_5       v_6       v_7       v_8
#' # true 0.1458743 1.010073 0.4683325 0.3838641 0.2961068 0.4284725 0.4670166 0.5195746 0.5799875 0.6024224 0.6095980 0.6688995
#' #      0.6087317 1.185651 0.5912251 0.3862249 0.2979105 0.4301181 0.4662076 0.5202188 0.5790622 0.6046539 0.6074382 0.6683207
#'
#' @export
#' @import rtdists


recovery <- function(base_par,
                     dataset   = NULL,
                     cycles    = 500,
                     sigma_mod = NULL){

  # determine the type of data
  if ("beta" %in% base_par){
    dynamic = T
    n_drift = NULL
    l       = length(base_par)
  } else{
    dynamic = F
    n_drift = length(unique(dataset$repetition))
    l       = length(base_par) + n_drift
  }

  # actual parameter recovery
  for (q in 1:cycles){
    o = tryCatch(optim(param.draw(base_par = base_par,                # initial parameter guess
                                  n_drift  = n_drift,
                                  dynamic  = dynamic),
                       likelihood.summed,                             # goal function to optimize
                       method        = "L-BFGS-B",                    # minimization method
                       dataset       = dataset,
                       sigma_mod     = sigma_mod,
                       lower         = rep(0, times = l),             # parameter lower bound
                       upper         = rep(Inf, times = l),           # parameter upper bound
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
    return(rep(NA, times = l + 2))
  } else{
    # save the lowest value of the eigen values of the Hessian
    # all should be positive (> epsilon, with epsilon = 0.01) when minimizing
    h = min(eigen(o$hessian)$values)
    return(c(o$par, o$value, h))
  }
}
