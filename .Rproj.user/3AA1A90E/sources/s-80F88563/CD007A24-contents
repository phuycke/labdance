#' @title netinputs
#'
#' @description Generates how the netinputs at the output level evolve over time as a function of the learning rate
#'     beta and the weight resets
#'
#' @param beta bla
#' @param wr bla
#'
#' @return A data.frame.
#' @examples
#' ni = netinputs(beta = 0.5, wr = c(rep(0, times = 31), 1))
#'
#' @export
#' @import rtdists


netinputs <- function(beta, wr){

  stim = diag(4)
  t    = matrix(c(1, 1, 0, 0, 0, 0, 1, 1),
                nrow = 4)
  w    = matrix(0,
                nrow = nrow(stim),
                ncol = ncol(t))

  # data holder
  df      = matrix(0,
                   nrow = 512,
                   ncol = 2)
  trials  = rep(1:4,
                times = 512 / 4)

  # simulate
  for (i in 1:length(trials)){
    s = trials[i]
    # input at output units, logistic activation function
    netinput = 1 / (1 + exp(-(stim[s,] %*% w)))
    stopifnot(round(sum(netinput), 10) == 1)

    # weight update
    A = matrix(stim[s,])
    B = (t[s, ] - netinput)
    w = w + beta * (A %*% B)

    # save the netinputs (used during estimation of LR)
    df[i,1:2] = netinput

    # reset the weights based on random draw
    if (wr[i] == 1){
      w = matrix(0,
                 nrow = nrow(stim),
                 ncol = ncol(t))
    }
  }
  return(list(df[,1], df[,2]))
}
