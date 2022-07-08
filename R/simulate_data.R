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
#' load("data/data_neural.RData")
#'
#' # get dLBA parameters
#' true = param_draw(base_par = c("a", "b", "t0", "sd"),
#'                   n_drift  = 8,
#'                   dynamic  = FALSE)
#'
#' # simulate data retaining the stimulus order shown to subject 2,
#' # and add neural data
#' simulated = simulate_data(true_pars = true,
#'                           sigma_gen = 0.01,
#'                           dataset   = d)
#' head(simulated)
#'
#' #   sub_id stim repetition block_nr       rt response    neural
#' # 1      1    1          1        1 2.076967        1 0.3554553
#' # 2      1    1          2        1 1.898463        2 0.3688368
#' # 3      1    3          1        1 1.274176        2 0.3277951
#' # 4      1    4          1        1 1.335997        2 0.3238378
#' # 5      1    2          1        1 1.757728        2 0.3281104
#' # 6      1    3          2        1 1.572040        2 0.3861202
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
    xor(is.null(true_pars), is.null(dataset))
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
      nrow(dataset > 0)
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

