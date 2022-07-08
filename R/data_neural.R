#' Simulated data for the standard LBA model
#'
#' This dataset is simulated, and contains data for the standard LBA model.
#' The main difference with data_dynamic.RData is that here condition, target
#' and block information are not needed to simulate reaction times and
#' choice data.
#'
#' @docType data
#'
#' @usage data(data_neural)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{stim}{Code representing a stimulus shown to a subject}
#'  \item{repetition}{Amount of stimulus repetitions within an experimental
#'  block}
#'  \item{rt}{Reaction times (in seconds)}
#'  \item{response}{The response given by the subject}
#'  \item{accuracy}{Whether the subject gave the correct response or not}
#' }
#' @references This data set was artificially created for the labdance package.
#' @keywords datasets
#' @examples
#'
#' data(data_neural)
#' head(data_neural)
#'
"data_neural"
