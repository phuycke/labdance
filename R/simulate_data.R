#' @title simulate_data
#'
#' @description Function that makes calls to simulate_dynamic() or
#'     simulate_neural() depending on the type of underlying model.
#'
#' @param sub_id Optional: only for neural models.
#'     The subject identifier. Can be used to generate different datasets that
#'     each belong to a different "subject".
#' @param n_blocks Optional: only for neural models.
#'     Determines the number of experimental blocks within a dataset.
#' @param true_pars The LBA parameters used to generate the data.
#' @param sigma_gen Optional: only for neural models.
#'     The variance in the generation of the true neural data.
#' @param dataset Optional: only when empirical data is available.
#'     This allows data to be generated relying on stimuli actually seen by
#'     subjects.
#'
#' @return data.frame containing simulated behavioral and/or neural data
#'     depending on the underlying model.
#' @examples
#' require(labdance)
#'
#' set.seed(2022)
#'
#' # load prepared empirical data
#' data("data_dynamic")
#'
#' # get LBA parameters (8 drift rates)
#' true = param_draw(base_par = c("a", "b", "t0", "sd", "beta"),
#'                   n_drift  = NULL,
#'                   dynamic  = TRUE)
#'
#' # simulate data retaining the stimulus order shown to subject 2,
#' simulated = simulate_data(true_pars = true,
#'                           dataset   = data_dynamic)
#' head(simulated)
#' #   stim target condition       rt response   mean_v1   mean_v2
#' # 1    1      1     novel 1.608958        1 0.5000000 0.5000000
#' # 2    1      1     novel 1.865296        2 0.5230748 0.4769252
#' # 3    3      2     novel 1.554814        2 0.5000000 0.5000000
#' # 4    4      2     novel 1.343585        2 0.5000000 0.5000000
#' # 5    2      1     novel 1.712398        1 0.5000000 0.5000000
#' # 6    3      2     novel 1.822126        2 0.4769252 0.5230748
#'
#' # check stimulus order
#' all(data_dynamic$stim == simulated$stim)
#' # [1] TRUE
#'
#' @export
#' @import rtdists


simulate_data <- function(sub_id    = 1,
                          n_blocks  = 16,
                          true_pars = NULL,
                          sigma_gen = NULL,
                          dataset   = NULL){

  # checking for faulty input
  stopifnot(exprs = {
    class(sub_id) %in% c("numeric", "integer")
    !(sub_id < 1 | n_blocks < 1)
    class(n_blocks) %in% c("numeric", "integer")
    (!is.null(true_pars))
    length(true_pars) > 0
    !is.null(names(true_pars))
  })
  if (!is.null(sigma_gen)){
    stopifnot(exprs = {
      class(sigma_gen) %in% c("numeric", "integer")
      (sigma_gen > 0 & sigma_gen < 1000)
    })
  }
  if (!is.null(dataset)){
    stopifnot(exprs = {
      xor(all(c("stim", "condition") %in% colnames(dataset)), # dynamic
          all(c("stim", "repetition", "block_nr") %in% colnames(dataset))) # neural
      nrow(dataset) > 0
    })
  }

  # d(n)LBA
  if ("beta" %in% names(true_pars)){
    return(simulate_dynamic(n_blocks, true_pars, sigma_gen, dataset))
  } else{
    # (n)LBA
    return(simulate_neural(sub_id, n_blocks, true_pars, sigma_gen, dataset))
  }
}

