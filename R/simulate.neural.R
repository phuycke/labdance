#' @title simulate.neural
#'
#' @description Helper function called by simulate.data() whenever data from
#'     a neural model (nLBA, dnLBA) needs to be generated.
#'
#' @param sub_id The subject identifier. Can be used to generate different datasets
#'     that each belong to a different "subject".
#' @param n_blocks Determines the number of experimental blocks within a dataset.
#' @param true_pars The LBA parameters used to generate the data.
#' @param sigma_gen Optional: only for neural models.
#'     The variance in the generation of the true neural data.
#' @param dataset Optional: only when empirical data is available.
#'     Replaces the stimuli and repetitions by information observed in the data. Hence,
#'     this allows data to be generated relying on stimuli actually seen by
#'     subjects.
#'
#' @return data.frame containing behavioral and neural data.
#' @examples
#' require(labdance)
#'
#' # load prepared empirical data
#' load("data/sub-02 - simulate.neural.RData")
#'
#' # get LBA parameters (8 drift rates)
#' true = param.draw(base_par = c("a", "b", "t0", "sd"),
#'                   n_drift  = 8,
#'                   dynamic  = FALSE)
#' # simulate data retaining the stimulus order shown to subject 2,
#' # and add neural data
#' simulated = simulate.neural(true_pars = true,
#'                             sigma_gen = 0.01,
#'                             dataset   = d)
#' head(simulated)
#'
#' #   sub_id stim repetition block_nr       rt response    neural
#' # 1      1    1          1        1 2.307668        2 0.3879920
#' # 2      1    1          2        1 2.249775        2 0.3650512
#' # 3      1    3          1        1 2.072044        2 0.3590010
#' # 4      1    4          1        1 2.409889        2 0.3652670
#' # 5      1    2          1        1 1.707635        2 0.3779034
#' # 6      1    3          2        1 1.931923        2 0.3952722
#'
#'
#' @export
#' @import rtdists

simulate.neural <- function(sub_id    = 1,
                            n_blocks  = 16,
                            true_pars = NULL,
                            sigma_gen = NULL,
                            dataset   = NULL){

  # checking for faulty input
  stopifnot(exprs = {
    !all(is.null(c(sub_id, n_blocks, true_pars, sigma_gen, dataset)))
    all(is.numeric(c(sub_id, n_blocks, sigma_gen)))
    (class(dataset) %in% c("NULL", "data.frame"))
    length(grep("v_", names(true_pars))) > 0
    length(names(true_pars)) == length(unique(names(true_pars)))
    all(c("a", "b", "t0", "sd") %in% names(true_pars))
    (sub_id > 0)
    (sigma_gen > 0 & sigma_gen < 100)
  })
  if (!is.null(dataset)){
    stopifnot(exprs = {
      is.data.frame(dataset)
      all(c("stim", "repetition") %in% colnames(dataset))
    })
  } else{
    stopifnot(n_blocks > 0)
  }

  # placeholder for later
  df = data.frame(rep(sub_id, times = n_blocks * 32))
  colnames(df) = "sub_id"

  # determine the stimulus order and associated repetition (simulated or empirical)
  if (is.null(dataset)){
    df$stim = rep(rep(1:4, times = 8), times = n_blocks)
    df$repetition = rep(rep(1:8, each = 4), times = n_blocks)
    df$block_nr = rep(1:n_blocks, each = 32)
  } else{
    df$stim = dataset$stim
    df$repetition = dataset$repetition
    df$block_nr = rep(1:(nrow(dataset)/32), each = 32)
  }

  # dependent variables
  df$rt = NA
  df$response = NA
  # neural data (optional)
  if (!is.null(sigma_gen)){
    df$neural = NA
  }

  # drift rates positively correlated with repetition count
  drifts = true_pars[grep("v_", names(true_pars))]

  for (i in 1:nrow(df)){
    # get stimulus and associated repetition
    stim       = df$stim[i]
    repetition = df$repetition[i]

    # determine drift rate based on repetition count
    v = c(drifts[repetition], 1-drifts[repetition])

    # simulate an LBA trial
    simulated <- rLBA(1,
                      A      = true_pars["a"],
                      b      = true_pars["b"],
                      t0     = true_pars["t0"],
                      mean_v = v,
                      sd_v   = c(true_pars["sd"], true_pars["sd"]),
                      silent = T)
    df$rt[i] = simulated$rt
    df$response[i] = simulated$response

    # add neural data (a function of the drift rate) is sigma generation is known
    if (!is.null(sigma_gen)){
      df$neural[i] = rnorm(1, v, sigma_gen)
    }
  }

  # add accuracy condition to check
  levels(df$repetition) = as.character(1:8)

  return(df)
}
