#' @title toydata.R
#'
#' @description Data generated for one single subject for all four models
#'     (LBA, dLBA, nLBA, dnLBA)
#'
#' @details These simulated datasets contain data for one subject.
#'     Depending on the model, different variables are included in the datasets.
#'     Across datasets, reaction time measurements and choice data is available.
#'     When the model is non-dynamic, "repetitions" can also be found. This
#'     variable denotes a condition, and is used to estimate condition-specific
#'     rates. In a neural model, an extra column named "neural" can be found,
#'     denoting trial-by-trial neural data. When the model is dynamic,
#'     two extra columns can be found labeled "mean_v1" and "mean_v2".
#'     These columns denote the netinput at the output units, which in turn
#'     are used as a proxy of the drift rates on a trial-by-trial basis.
#'
#' @docType data
#'
#' @usage data(toydata)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{rt}{Trial-specific reaction times)}
#'  \item{response}{Choice data (binary responses)}
#' }
#' @references This data set was artificially created for the LABDANCE package.
#' @keywords datasets
#' @examples
#' data(dLBA)
#' head(dLBA)
#'
#' @export
"dLBA"
