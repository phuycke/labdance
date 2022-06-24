#' @title likelihood.neural
#'
#' @description Calculates the likelihood based on the passed neural data
#'
#' @param to_optim the parameter set
#' @param neural_data bla
#' @param conditions bla
#' @param netinput bla
#'
#' @return A data.frame containing behavioral and/or neural data.
#' @examples
#' true = param_draw(base_par = c("a", "b", "t0", "sd"), n_drift  = 8, dynamic  = F)
#' nLBA  = simulate.data(sub_id   = 1,
#'                       n_blocks  = 16,
#'                       true_pars = true,
#'                       sigma_gen = 0.01)
#'
#' likelihood.neural(true, nLBA$neural, nLBA$repetition)
#'
#' @export
#' @import rtdists


likelihood.neural <- function(to_optim, neural_data, conditions = NULL, netinput = NULL){
  stopifnot((!is.null(conditions) & is.null(netinput)) | (is.null(conditions) & !is.null(netinput)))

  if (!is.null(conditions)){
    # for nLBA
    sum_ll = 0
    for (i in seq_along(unique(conditions))){
      sum_ll = sum_ll + sum((neural_data[conditions == i] - to_optim[[grep(sprintf("v_%d", i), names(to_optim))]])^2)
    }
    return(sum_ll)
  } else{
    # for dnLBA
    return(sum((neural_data - netinput)^2))
  }
}
