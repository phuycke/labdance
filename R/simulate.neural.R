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
#'
#' @return data.frame containing behavioral and neural data.
#' @examples
#' true = param_draw(base_par = c("a", "b", "t0", "sd"),
#'                   n_drift  = 8,
#'                   dynamic  = F)
#' simulated  = simulate.data(sub_id    = 1,
#'                            n_blocks  = 16,
#'                            true_pars = true,
#'                            sigma_gen = 0.01)
#' head(simulated)
#'
#' #     stim repetition       rt response block_nr    neural subject
#' # 1      1          1 1.801657        2        1 0.4283653       1
#' # 2      2          1 2.364796        2        1 0.4475578       1
#' # 3      3          1 1.465132        2        1 0.4494453       1
#' # 4      4          1 1.373814        2        1 0.4271550       1
#' # 5      1          2 1.387029        2        1 0.4611560       1
#' # 6      2          2 1.150407        2        1 0.4488713       1
#'
#' @export
#' @import rtdists

simulate.neural <- function(sub_id    = 1,
                            n_blocks  = 16,
                            true_pars = NULL,
                            sigma_gen = 0.01){

  # checking for faulty input
  stopifnot(exprs = {
    !all(is.null(c(sub_id, n_blocks, true_pars, sigma_gen)))
    all(is.numeric(c(sub_id, n_blocks, sigma_gen)))
    length(true_pars) > 5
    length(names(true_pars)) == length(unique(names(true_pars)))
    (sub_id > 0 & n_blocks > 0)
    (sigma_gen > 0 & sigma_gen < 100)
  })

  # placeholder for later
  all_dat = c()

  for (i in 1:n_blocks){
    # define a single block
    stim       = rep((4*i-3):(4*i), times = 8)
    repetition = rep(1:8, each  = 4)
    d          = data.frame(cbind(stim, repetition))

    # placeholders and block numbers
    d$rt       = -1
    d$response = -1
    d$block_nr = i

    # drift rates positively correlated with repetition count
    drifts = true_pars[grep("v_", names(true_pars))]

    for (j in 1:nrow(d)){
      # determine drift rate based on repetition count and target response
      # drift rates denote the probability on a correct and incorrect response
      v = c(drifts[d$repetition[j]], 1-drifts[d$repetition[j]])

      # simulate an LBA trial
      simulated <- rLBA(1,
                        A      = true_pars["a"],
                        b      = true_pars["b"],
                        t0     = true_pars["t0"],
                        mean_v = v,
                        sd_v   = c(true_pars["sd"], true_pars["sd"]),
                        silent = T)
      d$rt[j] = simulated$rt
      d$response[j] = simulated$response

      # add neural data (a function of the drift rate) is sigma generation is known
      if (!is.null(sigma_gen)){
        d$neural[j] = rnorm(1, v, sigma_gen)
      }
    }

    # add accuracy condition to check
    levels(d$repetition) = as.character(1:8)

    # bind together
    all_dat = rbind(all_dat, d)
  }
  # add subject number and return
  all_dat$subject = sub_id
  return(all_dat)
}

