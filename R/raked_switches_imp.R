#' Imputed rake-weighted cell counts for all idiosyncratic transition matrices
#'
#' 25 imputations of the rake-weighted cell counts for the raw, idiosyncratic
#' transition matrices of all elections included in \code{voteswitchR}.
#'
#' @format A list of 25 data frames with 20415 rows and 5 columns.
#' \code{elec_id} gives the
#' election identifier, \code{switch_from} and \code{switch_to} specify the
#' sending and receiving parties, respectively, for each dyad (for identifying
#' these parties, join via the column \code{stack} in \code{\link{mappings}}).
#' \code{weights} gives the rake-weighted cell counts, \code{n} gives the total
#' number of observations per election.
"raked_switches_imp"
