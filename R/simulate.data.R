#' @title simulate.data
#'
#' @description Function that makes calls to simulate.dynamic() or simulate.neural() depending on the type of
#'     data that is needed.
#'
#' @param sub_id bla
#' @param n_blocks bla
#' @param true_pars bla
#' @param sigma_gen bla
#'
#' @return A data.frame containing behavioral and/or neural data.
#' @examples
#' true = param_draw(dynamic  = T)
#' dnLBA = simulate.data(true_pars = true, sigma_gen = 0.01)
#'
#' @export
#' @import rtdists


simulate.data <-  function(sub_id = NULL, n_blocks = NULL, true_pars, sigma_gen = NULL){
  # (n)LBA
  if (!"beta" %in% names(true_pars)){
    return(simulate.neural(sub_id, n_blocks, true_pars, sigma_gen))
  } else{
    # d(n)LBA
    return(simulate.dynamic(true_pars, sigma_gen))
  }
}

