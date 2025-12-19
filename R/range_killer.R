#' R6 class: results of a descriptive robustness to removal of extreme values
#'
#' Object container for results produced by the \code{range_killer} function.
#'
#' @name Range_killer_res
#' @docType class
#' @keywords class
#' @seealso \code{\link{range_killer}}
#' @export
Range_killer_res <- R6::R6Class("Range_killer_res",
                         public = list(
                           #' @field Min_mean Numeric vector. Lower bound on the sample mean after
                           #'   removing the observed minimum and maximum values (worst-case minimum).
                           Min_mean = NULL,
                           #' @field RIVETS_mean Numeric vector. Mean recalculated using the original
                           #'   reported values (the "reported" or RIVETS case).
                           RIVETS_mean = NULL,
                           #' @field Max_mean Numeric vector. Upper bound on the sample mean after
                           #'   removing the observed minimum and maximum values (worst-case maximum).
                           Max_mean = NULL,
                           #' @field Min_sd Numeric vector. Lower bound on the sample standard
                           #'   deviation after removing the observed minimum and maximum values
                           #'   (or \code{-1} for impossible / inconsistent combinations).
                           Min_sd = NULL,
                           #' @field RIVETS_sd Numeric vector. SD recalculated using the original
                           #'   reported values (or \code{-1} if the recalculated variance is negative).
                           RIVETS_sd = NULL,
                           #' @field Max_sd Numeric vector. Upper bound on the sample standard
                           #'   deviation after removing the observed minimum and maximum values
                           #'   (or \code{-1} for impossible / inconsistent combinations).
                           Max_sd = NULL,
                           #' @description Create a new \code{Range_killer_res} object.
                           #' @param Min_mean,RIVETS_mean,Max_mean,Min_sd,RIVETS_sd,Max_sd
                           #'   Numeric vectors of equal length (one element per input case).
                           #' @return A new \code{Range_killer_res} object.
                           initialize = function(Min_mean = NULL,
                                                 RIVETS_mean = NULL,
                                                 Max_mean = NULL,
                                                 Min_sd = NULL,
                                                 RIVETS_sd = NULL,
                                                 Max_sd = NULL) {
                             self$Min_mean <- Min_mean
                             self$RIVETS_mean <- RIVETS_mean
                             self$Max_mean <- Max_mean
                             self$Min_sd <- Min_sd
                             self$RIVETS_sd <- RIVETS_sd
                             self$Max_sd <- Max_sd
                           },
                           #' @description Print a human-readable summary for one or more cases.
                           #'   By default prints all cases. Passing \code{n} as an integer vector
                           #'   selects which cases to display.
                           print = function(n=NULL) {
                             cat("Results from range_killer\n")
                             if(is.null(n) || n>length(self$RIVETS_mean) || n<=0)
                               n <- 1:(length(self$RIVETS_mean))
                             for(i in n) {
                               cat("\n\n--- Line", i,"---\n")
                               cat("---------------------------------------\n")
                               cat("Minimum mean: ", self$Min_mean[i], "\n")
                               cat("RIVETS mean:  ", self$RIVETS_mean[i], "\n")
                               cat("Maximum mean: ", self$Max_mean[i], "\n")
                               cat("Minimum sd: ", if(self$Min_sd[i] <0) "Impossible" else self$Min_sd[i], "\n")
                               cat("RIVETS sd:  ", if(self$RIVETS_sd[i] <0) "Impossible" else self$RIVETS_sd[i], "\n")
                               cat("Maximum sd: ", if(self$Max_sd[i] <0) "Impossible" else self$Max_sd[i], "\n")
                               cat("---------------------------------------\n")
                             }
                           }
                         )
)

#' Range-based robustness check of descriptive statistics after removing extremes
#'
#' Compute bounds on the sample mean and standard deviation that can arise when
#' the observed minimum and maximum values are removed from the sample. The
#' function explores combinations of plausible values for the descriptive
#' statistics (minimum / original / maximum) for the reported mean, standard
#' deviation, minimum and maximum and returns the worst-case (min / max)
#' resulting mean and standard deviation for each input case. It also returns
#' the recalculated mean and standard deviation obtained when using the
#' \emph{original}/RIVETS reported values.
#'
#' @param m Character scalar/vector. Point estimate of the sample mean.
#' Length must equal the number of cases.
#' @param s Character scalar/vector. Point estimate of the sample standard
#' deviation (SD). Length must equal the number of cases.
#' @param n An integer vector of sample sizes (one value per case). Length must
#' equal the number of cases. Each element must be \eqn{\ge 4} (see Details).
#' @param min Character scalar/vector. Point estimate of the sample minimum value.
#' Length must equal the number of cases.
#' @param max Character scalar/vector. Point estimate of the sample maximum value.
#' Length must equal the number of cases.
#'
#' @return A named list with six numeric vectors:
#' \describe{
#' \item{res_m_min}{minimum possible mean after removing the minimum and maximum values once.}
#' \item{res_m_max}{maximum possible mean after removing the minimum and maximum values once.}
#' \item{res_s_min}{minimum possible SD after removing the minimum and maximum values once.}
#' \item{res_s_max}{maximum possible SD after removing the minimum and maximum values once.}
#' \item{res_m_RIVETS}{mean recalculated from the exact \code{original} reported values.}
#' \item{res_s_RIVETS}{SD recalculated from the exact \code{original} reported values.}
#' }
#'
#' @details
#' For each case, the function forms the Cartesian product
#' of the three candidate values (minimum, original, maximum) provided
#' for each of the four descriptors (mean, SD, min, max). For every combination
#' it recomputes the sample mean and SD that would result after removing the
#' reported minimum and maximum observations from an original sample of size n-2.
#' Computations use the following algebraic identities:
#'
#' * New mean: \eqn{m' = (n m - (min + max)) / (n - 2)}.
#' * Sum of squares (original): \eqn{SS = (n-1) s^2 + n m^2}.
#' * New variance (unbiased): \eqn{var' = [SS' - (n-2) m'^2] / (n-3)}, where
#' \eqn{SS'} is \eqn{SS} less the squared removed values.
#'
#' The function sets the recalculated SD to \code{-1} if the computed variance
#' is negative (this flags an impossible/inconsistent combination).
#'
#' @section Requirements and edge-cases:
#' \itemize{
#' \item All corresponding components (except n) must be characters and of equal length.
#' \item Each \code{n[j]} must be at least 4. The formulas include divisions
#' by \code{(n-2)} and \code{(n-3)}; values < 4 will produce errors or
#' non-finite results.}
#'
#'
#' @keywords robust descriptive sensitivity
#' @export
range_killer <- function(m, s, n, min, max, output = FALSE) {
  L <- length_check(list(m=m, s=s, min=min, max=max))
  m <- validate_descriptive(m)
  s <- validate_descriptive(s)
  min <- validate_descriptive(min)
  max <- validate_descriptive(max)
  res_m_min <- c()
  res_m_max <- c()
  res_s_min <- c()
  res_s_max <- c()
  res_m_RIVETS <- c()
  res_s_RIVETS <- c()

  for (j in seq_len(L)) {
    m_range <- c(m$minimum[j], m$original[j], m$maximum[j])
    s_range <- c(s$minimum[j], s$original[j], s$maximum[j])
    min_range <- c(min$minimum[j], min$original[j], min$maximum[j])
    max_range <- c(max$minimum[j], max$original[j], max$maximum[j])
    m_grid <- c()
    s_grid <- c()

    cases_grid <- expand.grid(m = m_range, s = s_range, min = min_range, max = max_range)

    for(i in seq_len(nrow(cases_grid)))
    {
      # our new mean is (sum of values minus min&max) / (sample size minus two)
      m_prime <- (n[j] * cases_grid[i, "m"] - (cases_grid[i, "min"] + cases_grid[i, "max"])) / (n[j] - 2)

      # easiest way to explain this is sum of squares, let's get the original one
      SS <- (n[j] - 1) * cases_grid[i, "s"]^2 + n[j] * cases_grid[i, "m"]^2

      # and we just remove the squared values
      SSprime <- SS - (cases_grid[i, "min"]^2 + cases_grid[i, "max"]^2)

      # variance  is now...
      var_prime <- (SSprime-(n[j]-2)*m_prime^2) / (n[j]-3)

      s_prime <- if(var_prime>=0) sqrt(var_prime) else -1

      m_grid <- c(m_grid, m_prime)
      s_grid <- c(s_grid, s_prime)
    }

    res_m_min <- c(res_m_min, min(m_grid))
    res_m_max <- c(res_m_max, max(m_grid))
    res_s_min <- c(res_s_min, min(s_grid))
    res_s_max <- c(res_s_max, max(s_grid))

    # our new mean is (sum of values minus min&max) / (sample size minus two)
    m_prime <- (n[j] * m$original[j] - (min$original[j] + max$original[j])) / (n[j] - 2)

    # easiest way to explain this is sum of squares, let's get the original one
    SS <- (n[j] - 1) * s$original[j]^2 + n[j] * m$original[j]^2

    # and we just remove the squared values
    SSprime <- SS - (min$original[j]^2 + max$original[j]^2)

    # variance  is now...
    var_prime <- (SSprime-(n[j]-2)*m_prime^2) / (n[j]-3)

    s_prime <- if(var_prime>=0) sqrt(var_prime) else -1

    res_m_RIVETS <- c(res_m_RIVETS, m_prime)
    res_s_RIVETS <- c(res_s_RIVETS, s_prime)
  }

  # Return value
  result <- Range_killer_res$new(Min_mean = res_m_min,
                             RIVETS_mean = res_m_RIVETS,
                             Max_mean = res_m_max,
                             Min_sd = res_s_min,
                             RIVETS_sd = res_s_RIVETS,
                             Max_sd = res_s_max)

  if(output == TRUE)
  {
    print(result)
  }

  return(result)
}
