#' Cell counts for all idiosyncratic transition matrices
#'
#' Cell counts for the raw, idiosyncratic transition matrices of all
#'  elections included in \code{voteswitchR}.
#'
#' @format A data frame with 20415 rows and 5 columns. \code{elec_id} gives the
#' election identifier, \code{switch_from} and \code{switch_to} specify the
#' sending and receiving parties, respectively, for each dyad (for identifying
#' these parties, join via the column \code{stack} in \code{\link{mappings}}).
#' \code{weights} gives the cell counts (if available, weighted by sampling or
#' post-stratification weights), \code{n} gives the total number of observations
#' per election.
"switches"
