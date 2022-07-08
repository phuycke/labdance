#' @title simulate_neural
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
#' set.seed(2022)
#'
#' # load prepared empirical data
#' data("data_neural")
#' data_neural$block_nr = rep(1:32, nrow(data_neural) / 32)
#' # get LBA parameters (8 drift rates)
#' true = param_draw(base_par = c("a", "b", "t0", "sd"),
#'                   n_drift  = 8,
#'                   dynamic  = FALSE)
#'
#' # simulate data retaining the stimulus order shown to subject 2,
#' # and add neural data
#' simulated = simulate_neural(true_pars = true,
#'                             sigma_gen = 0.01,
#'                             dataset   = data_neural)
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
#' # check stimulus order
#' all(data_neural$stim == simulated$stim)
#' # [1] TRUE
#'
#' @export
#' @import rtdists
#' @import stats

simulate_neural <- function(sub_id    = 1,
                            n_blocks  = 16,
                            true_pars = NULL,
                            sigma_gen = NULL,
                            dataset   = NULL){

  # checking for faulty input
  stopifnot(exprs = {
    class(sub_id) %in% c("numeric", "integer")
    !(sub_id < 1 | n_blocks < 1)
    class(n_blocks) %in% c("numeric", "integer")
    length(true_pars) > 0
    !is.null(names(true_pars))
    !("beta" %in% names(true_pars))
  })
  if (!is.null(sigma_gen)){
    stopifnot(exprs = {
      class(sigma_gen) %in% c("numeric", "integer")
      (sigma_gen > 0 & sigma_gen < 1000)
    })
  }
  if (!is.null(dataset)){
    stopifnot(exprs = {
      all(c("stim", "repetition", "block_nr") %in% colnames(dataset))
      nrow(dataset) > 0
    })
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
                      silent = TRUE)
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
