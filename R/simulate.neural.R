#' @title simulate.neural
#'
#' @description Helper function called by simulate.data() when generating data.
#'
#' @param sub_id bla
#' @param n_blocks bla
#' @param true_pars bla
#' @param sigma_gen bla
#'
#' @return A data.frame containing behavioral and neural data.
#' @examples
#' true = param_draw(base_par = c("a", "b", "t0", "sd"),
#'                   n_drift  = 8,
#'                   dynamic  = F)
#' nLBA  = simulate.data(sub_id    = 1,
#'                       n_blocks  = 16,
#'                       true_pars = true,
#'                       sigma_gen = 0.01)
#' @export
#' @import rtdists


simulate.neural <- function(sub_id, n_blocks, true_pars, sigma_gen = NULL){

  # placeholder for later
  all_dat = c()

  for (i in 1:n_blocks){
    # define a single block
    stim       = rep((4*i-3):(4*i), times = 8)
    repetition = rep(1:8,     each  = 4)
    d          = data.frame(cbind(stim, repetition))

    # placeholders and block numbers
    d$rt = -1
    d$response = -1
    d$block_nr = i

    # drift rates positively correlated with repetition count
    drifts = true_pars[5:12]

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

