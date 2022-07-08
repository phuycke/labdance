#' @title likelihood_neural
#'
#' @description Helper function: called when "likelihood.summed" is called.
#'     Calculates the likelihood given passed (neural) data.
#'
#' @param to_optim The likelihood for parameter set to_optim is calculated.
#' @param dataset Optional: only when empirical data is available.
#'     This allows data to be generated relying on stimuli actually seen by
#'     subjects.
#'
#' @return numeric value indicating the likelihood of a parameter set given
#'     the neural (or model output) data.
#' @examples
#' require(labdance)
#'
#' set.seed(2022)
#'
#' # neural LBA
#' true = param_draw(base_par = c("a", "b", "t0", "sd"),
#'                   n_drift  = 8,
#'                   dynamic  = FALSE)
#' dataset = simulate_data(true_pars = true,
#'                         dataset   = NULL,
#'                         sigma_gen = 0.01)
#' likelihood_neural(to_optim = true,
#'                   dataset  = dataset)
#' # [1] 0.04932333
#'
#' # dynamic neural LBA
#' true = param_draw(base_par = c("a", "b", "t0", "sd", "beta"),
#'                   n_drift  = NULL,
#'                   dynamic  = TRUE)
#' dataset = simulate_data(true_pars = true,
#'                         dataset   = NULL,
#'                         sigma_gen = 0.01)
#' likelihood_neural(to_optim = true,
#'                   dataset  = dataset)
#' # [1] 0.05276045
#'
#' @export
#' @import rtdists


likelihood_neural <- function(to_optim,
                              dataset = NULL) {

  # test for faulty input
  # handle bad input
  stopifnot(exprs = {
    (!is.null(names(to_optim)))
    all(c("a", "b", "t0", "sd") %in% names(to_optim))
  })
  if (!is.null(dataset)) {
    stopifnot(exprs = {
      (!is.null(names(to_optim)))
      xor("repetition" %in% names(dataset), "mean_v1" %in% names(dataset))
      })
  }
  if (!is.null(dataset$mean_v1)) stopifnot((!is.null(dataset$neural)))

  # neural LBA
  if ("repetition" %in% names(dataset)) {
    # for nLBA
    sum_ll <- 0
    for (i in seq_along(unique(dataset$repetition))) {
      sum_ll <- sum_ll + sum((dataset$neural[dataset$repetition == i] -
                                to_optim[[grep(sprintf("v_%d", i), names(to_optim))]])^2)
    }
    return(sum_ll)
  }
  # dynamic neural LBA
  if ("mean_v1" %in% names(dataset)) {
    # for dnLBA
    return(sum((dataset$neural - dataset$mean_v1)^2))
  }
}
