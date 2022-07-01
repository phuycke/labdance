#' @title negloglik.behavioral
#'
#' @description Calculates the -log(likelihood) based on the passed behavioral
#'     (i.e. reaction times and choice) data
#'
#' @param to_optim The likelihood for parameter set to_optim is calculated.
#' @param dataset Bla bla
#'
#' @return numeric value indicating the likelihood of a parameter set given
#'     the available behavioral data.
#' @examples
#' true = param_draw(base_par = c("a", "b", "t0", "sd"),
#'                   n_drift  = 8,
#'                   dynamic  = F)
#' LBA  = simulate.data(1, 16, true)
#'
#' negloglik.behavioral(to_optim   = true,
#'                      rt         = LBA$rt,
#'                      response   = LBA$response,
#'                      conditions = LBA$repetition)
#' # [1] -423.1392
#'
#' @export
#' @import rtdists


negloglik.behavioral <- function(to_optim,
                                 dataset){

  # at least one of the two must be NULL
  if (is.null(dataset$repetition)){
    dataset$repetition = 1
  }

  # summed loglik
  sum_ll = 0

  for (i in seq_along(unique(dataset$repetition))){

    # assign parameters to variable names
    if ("beta" %in% names(to_optim)){
      par = c(A       = to_optim[["a"]],
              b       = to_optim[["b"]],
              t0      = to_optim[["t0"]],
              mean_v1 = F,
              mean_v2 = F,
              sd_v2   = to_optim[["sd"]])
    } else{
      par = c(A       = to_optim[["a"]],
              b       = to_optim[["b"]],
              t0      = to_optim[["t0"]],
              mean_v1 = to_optim[[grep(sprintf("v_%d", i), names(to_optim))]],
              mean_v2 = 1-to_optim[[grep(sprintf("v_%d", i), names(to_optim))]],
              sd_v2   = to_optim[["sd"]])
    }
    spar = par[!grepl("[12]$", names(par))]

    # distribution parameters
    dist_par_names  = unique(sub("[12]$", "", grep("[12]$", names(par), value = TRUE)))
    dist_par        = vector("list",
                             length = length(dist_par_names))
    names(dist_par) = dist_par_names
    for (j in dist_par_names){
      dist_par[[j]] = as.list(unname(par[grep(j, names(par))]))
    }

    # set common sd's
    dist_par$sd_v = c(dist_par$sd_v, dist_par$sd_v)

    if ("beta" %in% names(to_optim)){
      # compute netinputs based on the fed in learning rate, and use this as drift rates
      dist_par$mean_v = netinputs(beta = to_optim[[grep("beta", names(to_optim))]],
                                  dataset = dataset)
    }

    # get summed log-likelihood
    if (length(unique(dataset$repetition)) == 1){
      react = list(dataset$rt)
      resp  = list(dataset$response)
    } else{
      react = list(dataset$rt[dataset$repetition == i])
      resp  = list(dataset$response[dataset$repetition == i])
    }
    d = do.call(dLBA, args = c(rt           = react,
                               response     = resp,
                               spar,
                               dist_par,
                               distribution = "norm",
                               silent       = TRUE))
    # get -loglik for this subsection of the data
    if (any(d < 0e-10)){
      ll = 1e6
    }
    else{
      ll = -sum(log(d))
    }
    sum_ll = sum_ll + ll
  }
  return(sum_ll)
}
