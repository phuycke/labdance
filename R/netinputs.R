#' @title netinputs
#'
#' @description Generates how the netinputs at the output level of a dynamic
#'     model evolve over time as a function of the learning rate beta and
#'     the weight resets. Since our dynamic model has two output units, netinput
#'     has N times 2 values, where N denotes the number of trials.
#'
#' @param beta The learning rate. Influences the change in model weights. Since
#'     the netinput is the dot product of the input activation and the weights,
#'     beta has a direct impact of the level of activation at the output units.
#' @param dataset Optional: only when empirical data is available.
#'     Replaces the stimuli and condition info by empirical data. Hence,
#'     this allows data to be generated relying on stimuli actually seen by
#'     subjects.
#'
#' @return list consisting of M elements (the number of output units), where each
#'     subset consists of N elements (the number of trials).
#' @examples
#' require(labdance)
#'
#' # load prepared empirical data
#' load("data/sub-02 - simulate.dynamic.RData")
#'
#' # simulate netinputs based on data and low beta
#' ni = netinputs(beta = .2,
#'                dataset = d)
#' print(rbind(ni[[1]][20:25], ni[[2]][20:25]))
#'
#' #            [,1]      [,2]      [,3]      [,4]      [,5]      [,6]
#' # [1,] 0.4082713 0.4082713 0.5917287 0.6112978 0.6296023 0.3887022
#' # [2,] 0.5917287 0.5917287 0.4082713 0.3887022 0.3703977 0.6112978
#'
#' # simulate netinputs based on data and high beta
#' ni = netinputs(beta = .7,
#'                dataset = d)
#' print(rbind(ni[[1]][20:25], ni[[2]][20:25]))
#'
#' #            [,1]      [,2]      [,3]      [,4]     [,5]      [,6]
#' # [1,] 0.2523346 0.2523346 0.7476654 0.7795135 0.804895 0.2204865
#' # [2,] 0.7476654 0.7476654 0.2523346 0.2204865 0.195105 0.7795135
#'
#' @export
#' @import rtdists


netinputs <- function(beta    = 0.5,
                      dataset = NULL){

  stim = diag(4)
  t    = matrix(c(1, 1, 0, 0, 0, 0, 1, 1),
                nrow = 4)
  w_nov = matrix(0,
                 nrow = nrow(stim),
                 ncol = ncol(t))
  w_rep = matrix(0,
                 nrow = nrow(stim),
                 ncol = ncol(t))

  # data holder
  df = data.frame(matrix(0,
                  nrow = ifelse(is.null(dataset), 512, nrow(dataset)),
                  ncol = 4))
  colnames(df) = c("stim", "condition", "mean_v1", "mean_v2")

  # determine the stimuli
  if (is.null(dataset)){
    df$stim = rep(1:4, times = 128)
    df$condition = rep(rep(c("novel", "repeating"), each = 32), times = 8)
  } else{
    df$stim = dataset$stim
    df$condition = dataset$condition
  }

  # simulate
  for (i in 1:nrow(df)){
    s = df$stim[i]
    # input at output units, logistic activation function
    if (df$condition[i] == "novel"){
      netinput = 1 / (1 + exp(-(stim[s,] %*% w_nov)))
    } else{
      netinput = 1 / (1 + exp(-(stim[s,] %*% w_rep)))
    }
    stopifnot(round(sum(netinput), 10) == 1)

    # weight update
    A = matrix(stim[s,])
    B = (t[s, ] - netinput)
    # input at output units, logistic activation function
    if (df$condition[i] == "novel"){
      w_nov = w_nov + beta * (A %*% B)
    } else{
      w_rep = w_rep + beta * (A %*% B)
    }

    # save the netinputs (used during estimation of LR)
    df[i,3:4] = netinput

    # reset the weights based on random draw
    if (i %% 32 == 0 & df$condition[i] == "novel"){
      w_nov = matrix(0,
                     nrow = nrow(stim),
                     ncol = ncol(t))
    }
  }
  # return the actual netinputs
  return(list(df$mean_v1, df$mean_v2))
}
