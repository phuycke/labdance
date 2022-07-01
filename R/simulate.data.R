#' @title simulate.data
#'
#' @description Function that makes calls to simulate.dynamic() or
#'     simulate.neural() depending on the type of underlying model.
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
#' # load prepared empirical data
#' load("data/sub-02 - simulate.neural.RData")
#'
#' # get dLBA parameters
#' true = param.draw(base_par = c("a", "b", "t0", "sd"),
#'                   n_drift  = 8,
#'                   dynamic  = FALSE)
#'
#' # simulate data retaining the stimulus order shown to subject 2,
#' # and add neural data
#' simulated = simulate.data(true_pars = true,
#'                           sigma_gen = 0.01,
#'                           dataset   = d)
#' head(simulated)
#'
#' #   sub_id stim repetition block_nr       rt response    neural
#' # 1      1    1          1        1 2.218504        2 0.2872945
#' # 2      1    1          2        1 3.002954        2 0.3563756
#' # 3      1    3          1        1 2.005866        2 0.3056450
#' # 4      1    4          1        1 2.868882        2 0.2967507
#' # 5      1    2          1        1 1.503713        2 0.2952629
#' # 6      1    3          2        1 2.535639        2 0.3499139
#'
#' @export
#' @import rtdists


simulate.data <- function(sub_id    = 1,
                          n_blocks  = 16,
                          true_pars = NULL,
                          sigma_gen = NULL,
                          dataset   = NULL){
  # d(n)LBA
  if ("beta" %in% names(true_pars)){
    return(simulate.dynamic(n_blocks, true_pars, sigma_gen, dataset))
  } else{
    # (n)LBA
    return(simulate.neural(sub_id, n_blocks, true_pars, sigma_gen, dataset))
  }
}

