#' Baseline effective conductivity estimation  (Kb)
#'
#' This function will calculation the baseline effective conductivity based on
#' the amount of sand, clay and the cation exchange capacity of the soil.
#'
#' @details
#'
#'
#'
#' @param sand percent of sand
#' @param clay percent of clay
#' @param cec cation exchange capacity of the soil (meq/100g)
#'
#' @return numeric, Kb, the baseline effective conductivity
#'
#' @export
#'
#' @source
#' page 14 of \url{https://www.ars.usda.gov/ARSUserFiles/50201000/WEPP/usersum.pdf}
#'
#' @seealso \link{effective_hydraulic_conductivity}
#'
#' @examples
#' calculate_baseline_effective_conductivity(5.2, 40.1, 29.4)
calculate_baseline_effective_conductivity <- function(sand, clay, cec) {
  stopifnot(cec > 1)

  (-0.265 + 0.0086*sand^1.8 + 11.46*cec^-.75)*(clay <= 40) +
    0.0066*exp(244/clay)*(clay>40)
}



#' @describeIn calculate_baseline_effective_conductivity
#'
calculate_Kb <- function(sand, clay, cec) {
  calculate_baseline_effective_conductivity(sand, clay, cec)
}


#' Effective hydraulic conductivity
#'
#' @format A data frame with 43 rows and 10 variables
#' \describe{
#'   \item{soil}{character, soil type}
#'   \item{sand}{numeric, sand content (\%)}
#'   \item{clay}{numeric, clay content (\%)}
#'   \item{organic matter}{numeric, organic matter content (\%)}
#'   \item{cec}{numeric, cation exchange capacity (meq/100g)}
#'   \item{simulator measured Ke}{numeric, simulator measured Ke (mm/hr)}
#'   \item{opt. constant Kef}{numeric, optimized constant Kef (mm/hr)}
#'   \item{est. constant Kef}{numeric, estimated constant Kef (mm/hr)}
#'   \item{opt. baseline Kb}{numeric, optimized baseline Kb (mm/hr)}
#'   \item{est. baseline Kb}{numeric, estimated baseline Kb (mm/hr)}
#' }
#'
#' @seealso \link{calculate_baseline_effective_conductivity}
"effective_hydraulic_conductivity"
