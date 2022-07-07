#' @title param.draw
#'
#' @description Generates (d)(n)LBA parameters within a reasonable range.
#'
#' @param base_par The parameters that a user wants to generate.
#'     Choice between a (starting point of evidence accumulation k is drawn
#'     from U[0, A]), b (bound), t0 (non decision time), sd (standard deviation)
#'     of the drift rate v (v_i ~ N(M, sd)) and beta (learning rate). Note that
#'     one cannot draw drift rates and set dynamic to TRUE: if a model is
#'     dynamic, the drift rates are replaced by a single parameter beta, and
#'     hence no other drift rates can be drawn.If no base_par is supplied,
#'     all available parameters are outputted.
#' @param n_drift Optional: only needed in non dynamic models.
#'     The number of drift rates (i.e. one for each accumulator)
#' @param dynamic Optional: only needed in dynamic models.
#'     Boolean indicating whether the model is dynamic or not.
#'
#' @return numeric containing named values
#' @examples
#' require(labdance)
#'
#' set.seed(2022)
#'
#' param.draw(base_par = c("a", "b", "t0", "sd"),
#'            n_drift  = NULL,
#'            dynamic  = T)
#'
#' #         a         b        t0        sd
#' # 0.6119832 1.2354445 0.3101643 0.2719001
#'
#' @export param.draw
#' @import rtdists


param.draw <- function(base_par = c("a", "b", "t0", "sd"),
                       n_drift  = NULL,
                       dynamic  = F){

  # checking for faulty input
  stopifnot(exprs = {
    all(is.character(base_par))
    length(base_par) > 0
    length(base_par) == length(unique(base_par))
    (class(n_drift) %in% c("numeric", "NULL", "integer"))
    is.logical(dynamic)
    ((is.null(n_drift) & isTRUE(dynamic)) |
        (!is.null(n_drift) & isFALSE(dynamic)))
  })
  if (is.numeric(n_drift)) stopifnot(n_drift > 1)
  for (e in base_par) stopifnot(e %in% c("a", "b", "t0", "sd", "beta"))
  if (!dynamic){
    if("beta" %in% base_par){
      stop("\nNon dynamic model but beta is asked.")
    }
  }

  s1 <- c(
    a    = runif(1, 0, .75),
    b    = runif(1, .75, 1.5),
    t0   = runif(1, 0.25, 0.75),
    sd   = runif(1, 0, 0.5),
    beta = runif(1, 0, 1)
  )
  if (dynamic){
    return(s1[base_par])
  } else{
    s2 <- sort(rnorm(n_drift, 0.5, 0.1), decreasing = F)
    names(s2) <- paste0("v_", seq_len(n_drift))
    return(c(s1[base_par], s2))
  }
}
