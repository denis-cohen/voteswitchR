#' The 'voteswitchR' package.
#'
#' @title Data and Methods for Analyzing Comparative Vote Switching Data
#'
#' @description The \code{voteswitchR} package offers functions for data
#' management and statistical analyses to implement the conceptual
#' framework presented in Cohen, Krause and Abou-Chadi (2021).
#'
#' \code{voteswichR} (development version 0.1.0) consists of the following
#' functions:
#'
#' For data processing:
#' \enumerate{
#'   \item \code{\link{build_data_file}}: Harmonize, impute, map, rake, and
#'   aggregate vote switching data
#'   \item \code{\link{recode_switches}}: Recode aggregate switching patterns
#' }
#'
#' For model estimation and post-estimation:
#' \enumerate{
#'   \item \code{\link{run_mavcl}}: Run the MAVCL model
#'   \item \code{\link{calculate_pred_error}}: Calculate the MAEs and RMSEs of
#'   MAVCL Predictions
#'   \item \code{\link{compute_qoi}}: Compute MAVCL quantities of interest
#' }
#'
#' Package-specific data:
#' \enumerate{
#'   \item \code{\link{mappings}}: Mapping of vote choices from surveys to
#'   ParlGov and MARPOR IDs.
#'   \item \code{\link{codebook}}: Documentations of \code{\link{mappings}}.
#'   \item \code{\link{data_guide}}: Versions, download links, and access
#'   details for survey data.
#' }
#'
#' @docType package
#' @name voteswitchR-package
#' @aliases voteswitchR
#' @useDynLib voteswitchR, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#' @importFrom magrittr "%>%"
#' @importFrom stats as.formula
#' @importFrom stats median
#' @importFrom stats model.matrix
#' @importFrom stats na.omit
#' @importFrom stats quantile
#' @importFrom stats sd
#'
#' @references
#' \enumerate{
#'   \item Cohen, Denis, Krause, Werner and Abou-Chadi, Tarik (2022).
#'   Comparative vote switching: A new framework for
#'   studying dynamic multiparty competition. Conference Paper. Unpublished
#'   manuscript.
#'   \item Stan Development Team (2020). RStan: the R interface to Stan. R
#'   package version 2.21.2. https://mc-stan.org
#' }
#'
#' @keywords internal
"_PACKAGE"
