#' @title likelihood_behavioral
#'
#' @description Calculates the -log(likelihood) based on the passed behavioral
#'     (i.e. reaction times and choice) data
#'
#' @param to_optim The likelihood for parameter set to_optim is calculated.
#' @param dataset Optional: only when empirical data is available.
#'     This allows data to be generated relying on stimuli actually seen by
#'     subjects.
#'
#' @return numeric value indicating the likelihood of a parameter set given
#'     the available behavioral data.
#' @examples
#' require(labdance)
#'
#' set.seed(2022)
#'
#' # get parameters, simulate data and calculate likelihood
#' true = param_draw(base_par = c("a", "b", "t0", "sd"),
#'                   n_drift  = 8,
#'                   dynamic  = FALSE)
#' simulated = simulate_data(true_pars = true,
#'                           dataset   = NULL)
#' ll.true = likelihood_behavioral(to_optim = true,
#'                                 dataset  = simulated)
#'
#' # calculate likelihood for another parameter set
#' test = param_draw(base_par = c("a", "b", "t0", "sd"),
#'                   n_drift  = 8,
#'                   dynamic  = FALSE)
#' ll.test = likelihood_behavioral(to_optim = test,
#'                                 dataset  = simulated)
#'
#' # check that likelihood is lowest for the true parameter set
#' sprintf("Negative loglikelihood for true parameters: %.02f", ll.true)
#' # [1] "Negative loglikelihood for true parameters: 747.29"
#' sprintf("Negative loglikelihood for other parameters: %.02f", ll.test)
#' # [1] "Negative loglikelihood for other parameters: 926.55"
#'
#'
#' @export
#' @import rtdists


likelihood_behavioral <- function(to_optim,
                                  dataset = NULL) {
  # handle bad input
  stopifnot(exprs = {
    (!is.null(names(to_optim)))
    all(c("a", "b", "t0", "sd") %in% names(to_optim))
  })
  if (!is.null(dataset)) {
    stopifnot(exprs = {
      (!is.null(names(dataset)))
      nrow(dataset) > 0
      all(c("rt", "response") %in% names(dataset))
      class(dataset$response) %in% c("numeric", "integer")
      min(dataset$response) == 1
      sort(unique(dataset$response)) == seq_along(unique(dataset$response))
    })
  }
  # at least one of the two must be NULL
  if (is.null(dataset$repetition)) {
    dataset$repetition <- 1
  }
  # summed loglik
  sum_ll <- 0
  for (i in seq_along(unique(dataset$repetition))) {
    # assign parameters to variable names
    if ("beta" %in% names(to_optim)) {
      par <- c(A       = to_optim[["a"]],
               b       = to_optim[["b"]],
               t0      = to_optim[["t0"]],
               mean_v1 = FALSE,
               mean_v2 = FALSE,
               sd_v2   = to_optim[["sd"]])
    } else {
      par <- c(A       = to_optim[["a"]],
               b       = to_optim[["b"]],
               t0      = to_optim[["t0"]],
               mean_v1 = to_optim[[grep(sprintf("v_%d", i),
                                        names(to_optim))]],
               mean_v2 = 1 - to_optim[[grep(sprintf("v_%d", i),
                                            names(to_optim))]],
               sd_v2   = to_optim[["sd"]])
    }
    spar <- par[!grepl("[12]$", names(par))]
    # distribution parameters
    dist_par_names <- unique(sub("[12]$",
                                 "",
                                 grep("[12]$",
                                      names(par),
                                      value = TRUE)))
    dist_par       <- vector("list",
                             length = length(dist_par_names))
    names(dist_par) <- dist_par_names
    for (j in dist_par_names) {
      dist_par[[j]] <- as.list(unname(par[grep(j, names(par))]))
    }
    # set common sd's
    dist_par$sd_v <- c(dist_par$sd_v, dist_par$sd_v)

    if ("beta" %in% names(to_optim)) {
      # compute netinputs based on the fed in learning rate,
      # and use this as drift rates
      dist_par$mean_v <- netinputs(beta = to_optim[[grep("beta",
                                                         names(to_optim))]],
                                   dataset = dataset)
    }
    # get summed log-likelihood
    if (length(unique(dataset$repetition)) == 1) {
      react <- list(dataset$rt)
      resp  <- list(dataset$response)
    } else {
      react <- list(dataset$rt[dataset$repetition == i])
      resp  <- list(dataset$response[dataset$repetition == i])
    }
    d <- do.call(dLBA, args = c(rt           = react,
                                response     = resp,
                                spar,
                                dist_par,
                                distribution = "norm",
                                silent       = TRUE))
    # get -loglik for this subsection of the data
    if (any(d < 0e-10)) {
      ll <- 1e6
    } else {
      ll <- -sum(log(d))
    }
    sum_ll <- sum_ll + ll
  }
  return(sum_ll)
}
