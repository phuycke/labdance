#' @title simulate.dynamic
#'
#' @description Helper function called by simulate.data() when generating data.
#'
#' @param true_pars bla
#' @param sigma_gen bla
#'
#' @return A data.frame containing behavioral and/or neural data.
#' @examples
#' true = param_draw(dynamic = T)
#' dLBA = simulate.data(true_pars = true)
#'
#' @export
#' @import rtdists


# simulate RW data, reset W to 0 based every 32 trials
simulate.dynamic <- function(true_pars, sigma_gen = NULL){

  # four stimuli and their targets
  stimuli = diag(4)
  target  = matrix(c(1, 1, 0, 0, 0, 0, 1, 1), nrow = 4)

  # define weights, dataholder and trial order
  w      = matrix(0,
                  nrow = nrow(stimuli),
                  ncol = ncol(target))
  df     = data.frame(matrix(0,
                             nrow = 512,
                             ncol = 8))
  # column names (used laeter)
  c_names = c("rt", "response", "stimulus", "target",
              "accuracy", "weight_reset", "mean_v1", "mean_v2")
  # add an extra column for neural data if needed
  if (!is.null(sigma_gen)){
    df = cbind(df, rep(0, times = nrow(df)))
    c_names = c(c_names, "neural")
  }
  trials = rep(1:nrow(stimuli),
               times = 512 / nrow(stimuli))

  # count the number of trials since last weight reset
  counter    = 1
  when_reset = round(rnorm(1, 32, 0))

  # for each trial change the weights
  for (i in 1:length(trials)){
    # determine which stimulus is shown
    s = trials[i]

    # logistic function where all(sum(netinput) == 1)
    netinput = 1 / (1 + exp(-(stimuli[s,] %*% w)))
    stopifnot(round(sum(netinput), 10) == 1)

    # LBA trial estimation where netinputs to output unit serve as v1 and v2
    df[i,1:2] = rLBA(1,
                     A      = true_pars["a"],
                     b      = true_pars["b"],
                     t0     = true_pars["t0"],
                     mean_v = as.vector(netinput),
                     sd_v   = c(true_pars["sd"], true_pars["sd"]),
                     silent = T)

    # add neural data
    if (!is.null(sigma_gen)){
      df[i,9] = rnorm(1, netinput[1], sigma_gen)
    }

    # weight update
    A = matrix(stimuli[s,])
    # TODO: dit deel verwijderen: * netinput * (1 - netinput) in lijn met cross entropy error function
    # error functie is een bernoulli verdeling (ipv een normaalverdeling zoals we initieel zeiden)
    B = (target[s, ] - netinput)
    w = w + true_pars["beta"] * (A %*% B)

    # save stimulus and associated target
    df[i,3] = s
    df[i,4] = which.max(target[s,])

    # save whether trials was correct or not
    if(which.max(target[s,]) == df[i,2]){
      df[i,5] = 1
    } else{
      df[i,5] = 0
    }

    # when to reverse is drawn from N(m, s), reset W, counter and draw from N(m,s)
    if (counter == when_reset){
      w = matrix(0, nrow = nrow(stimuli), ncol = ncol(target))
      #cat("Weight reset on trial", i, "\n")
      df[i,6]    = T
      counter    = 0
      when_reset = round(rnorm(1, 32, 0))
    } else{
      df[i,6] = F
    }

    # save the netinputs (used during estimation of LR) and increment counter for reset
    df[i,7:8] = netinput
    counter   = counter + 1
  }
  # change the column names and return the resulting dataframe
  colnames(df) = c("rt", "response", "stimulus", "target",
                   "accuracy", "weight_reset", "mean_v1", "mean_v2")
  return(df)
}
