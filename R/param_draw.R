#' @title param_draw
#'
#' @description Generates (d)(n)LBA parameters within a reasonable range.
#'
#' @param base_par The parameters that a user wants to generate. Choice between a (starting point of evidence
#'      accumulation k is drawn from U[0, A]), b (bound), t0 (non decision time), sd (standard deviation)
#'      of the drift rate v (v_i ~ N(M, sd)) and beta (learning rate)
#' @param n_drift The number of drift rates (i.e. one for each accumulator)
#' @param dynamic Whether the model is dynamic or not
#'
#' @return A vector named values
#' @examples
#' param_draw(base_par = c("a", "b", "t0", "sd"),
#'            n_drift  = 8,
#'            dynamic  = F)
#' @export
#' @import rtdists


param_draw <- function(base_par, n_drift = NULL, dynamic = F) {
  stopifnot((is.null(n_drift) & dynamic) | (!is.null(n_drift) & !dynamic))
  s1 <- c(
    a    = runif(1, 0, .75),
    b    = runif(1, .75, 1.5),
    t0   = runif(1, 0.25, 0.75),
    sd   = runif(1, 0, 0.5),
    beta = runif(1, 0, 1)
  )
  if (dynamic){
    return(s1)
  } else{
    stopifnot(n_drift > 0)
    s2 <- sort(rnorm(n_drift, 0.5, 0.1), decreasing = F)
    names(s2) <- paste0("v_", seq_len(n_drift))
    return(c(s1[base_par], s2))
  }
}
