#' @title simulate_dynamic
#'
#' @description Helper function called by simulate_data() whenever data from
#'     a dynamic model (dLBA, dnLBA) needs to be generated.
#'
#' @param true_pars The LBA parameters used to generate the data.
#' @param n_blocks Determines the number of experimental blocks within a
#'     dataset.
#' @param sigma_gen Optional: only for neural models.
#'     The variance in the generation of the true neural data.
#' @param dataset Optional: only when empirical data is available.
#'     Replaces the stimuli and conditions by information observed in the data.
#'     Hence, this allows data to be generated relying on stimuli actually
#'     seen by subjects.
#'
#' @return data.frame containing behavioral and/or neural data.
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
#' # and add neural data
#' simulated = simulate_dynamic(true_pars = true,
#'                              sigma_gen = 0.01,
#'                              dataset   = data_dynamic)
#' head(simulated)
#'
#' #   stim target condition       rt response   mean_v1   mean_v2    neural
#' # 1    1      1     novel 1.608958        1 0.5000000 0.5000000 0.4900472
#' # 2    1      1     novel 2.026692        1 0.5230748 0.4769252 0.5334783
#' # 3    3      2     novel 1.519964        1 0.5000000 0.5000000 0.5282756
#' # 4    4      2     novel 1.712398        1 0.5000000 0.5000000 0.4850896
#' # 5    2      1     novel 1.513373        2 0.5000000 0.5000000 0.5006154
#' # 6    3      2     novel 2.134347        2 0.4769252 0.5230748 0.4712491
#'
#' # check stimulus order
#' all(data_dynamic$stim == simulated$stim)
#' # [1] TRUE
#'
#' @export
#' @import rtdists
#' @import stats


# simulate RW data, reset W to 0 based every 32 trials
simulate_dynamic <- function(n_blocks  = 16,
                             true_pars = NULL,
                             sigma_gen = NULL,
                             dataset   = NULL) {

  # checking for faulty input
  stopifnot(exprs = {
    n_blocks > 0
    class(n_blocks) %in% c("numeric", "integer")
    (!is.null(true_pars))
    length(true_pars) > 0
    !is.null(names(true_pars))
    ("beta" %in% names(true_pars))
  })
  if (!is.null(sigma_gen)) {
    stopifnot(exprs = {
      class(sigma_gen) %in% c("numeric", "integer")
      (sigma_gen > 0 & sigma_gen < 1000)
    })
  }
  if (!is.null(dataset)) {
    stopifnot(exprs = {
      all(c("stim", "condition") %in% colnames(dataset))
      nrow(dataset) > 0
    })
  }

  # four stimuli and their targets
  stim <- diag(4)
  t    <- matrix(c(1, 1, 0, 0, 0, 0, 1, 1), nrow = 4)

  # define weights, dataholder and trial order
  w_nov <- matrix(0,
                  nrow = nrow(stim),
                  ncol = ncol(t))
  w_rep <- matrix(0,
                  nrow = nrow(stim),
                  ncol = ncol(t))

  df <- data.frame(matrix(NA,
                          nrow = ifelse(is.null(dataset),
                                        n_blocks * 32,
                                        nrow(dataset)),
                          ncol = 7))
  # column names (used later)
  colnames(df) <- c("stim", "target", "condition", "rt", "response",
                   "mean_v1", "mean_v2")

  # add an extra column for neural data if needed
  if (!is.null(sigma_gen)) {
    df$neural <- NA
  }

  # determine the stimuli
  if (is.null(dataset)) {
    df$stim <- rep(1:4, times = n_blocks * 32 / 4)
    df$condition <- rep(rep(c("novel", "repeating"), each = 32),
                        times = n_blocks / 2)
  } else {
    df$stim <- dataset$stim
    df$condition <- dataset$condition
  }

  # for each trial change the weights
  for (i in seq_len(nrow(df))) {
    s <- df$stim[i]
    # input at output units, logistic activation function
    if (df$condition[i] == "novel") {
      netinput <- 1 / (1 + exp(-(stim[s, ] %*% w_nov)))
    } else {
      netinput <- 1 / (1 + exp(-(stim[s, ] %*% w_rep)))
    }
    stopifnot(round(sum(netinput), 10) == 1)

    # LBA trial estimation where netinputs to output unit serve as v1 and v2
    df[i, 4:5] <- rLBA(1,
                       A      = true_pars["a"],
                       b      = true_pars["b"],
                       t0     = true_pars["t0"],
                       mean_v = as.vector(netinput),
                       sd_v   = c(true_pars["sd"], true_pars["sd"]),
                       silent = TRUE)

    # add neural data
    if (!is.null(sigma_gen)) {
      df$neural[i] <- rnorm(1, netinput[1], sigma_gen)
    }

    # weight update
    temp_a <- matrix(stim[s, ])
    temp_b <- (t[s, ] - netinput)
    # input at output units, logistic activation function
    if (df$condition[i] == "novel") {
      w_nov <- w_nov + true_pars["beta"] * (temp_a %*% temp_b)
    } else {
      w_rep <- w_rep + true_pars["beta"] * (temp_a %*% temp_b)
    }

    # save stimulus and associated target
    df[i, 6:7] <- netinput
    df$target[i] <- which.max(t[s, ])

    # reset the weights based on random draw
    if (i %% 32 == 0 && df$condition[i] == "novel") {
      w_nov <- matrix(0,
                      nrow = nrow(stim),
                      ncol = ncol(t))
    }
  }
  return(df)
}
