#' Simulated data for the dynamic LBA model
#'
#' This dataset is simulated, and contains data for the dynamic LBA model.
#' The main difference with data_neural.RData is that here condition, target
#' and block information is included since this is needed to adjust the weight
#' matrix. More precisely, two weight matrices are defined (one for each
#' condition), and hence the condition information is needed to adjust the
#' correct matrix.
#'
#' @docType data
#'
#' @usage data(data_dynamic)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{stim}{Code representing a stimulus shown to a subject}
#'  \item{repetition}{Amount of stimulus repetitions within an experimental block}
#'  \item{condition}{Whether this block contains novel or repeating stimuli}
#'  \item{target}{The correct response for this stimulus}
#'  \item{response}{The response given by the subject}
#'  \item{accuracy}{Whether the subject gave the correct response or not}
#'  \item{rt}{Reaction times (in seconds)}
#'  \item{block}{The block number (from 1 to 16)}
#' }
#' @references This data set was artificially created for the labdance package.
#' @keywords datasets
#' @examples
#'
#' data(data_dynamic)
#' head(data_dynamic)
#'
"data_dynamic"
