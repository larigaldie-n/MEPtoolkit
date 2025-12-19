#' R6 class: results of a Carlisle aggregation
#'
#' Object container for results produced by \code{\link{carlisle}}.
#'
#' @name Carlisle_res
#' @docType class
#' @keywords class
#' @format An R6 object with three public fields and two public methods:
#' \describe{
#' \item{\code{Student}}{Numeric vector of length three, \code{c(min, RIVETS, max)},
#' giving the minimum, reported (RIVETS) and maximum combined p-values
#' computed when aggregating the Student (pooled-variance) test p-values.
#' \code{NULL} when the object was built from provided p-values.}
#' \item{\code{Welch}}{Numeric vector of length three, \code{c(min, RIVETS, max)},
#' giving the analogue for Welch (unequal-variance) test p-values.
#' \code{NULL} when the object was built from provided p-values.}
#' \item{\code{Provided_p}}{Numeric vector of length three, \code{c(min, RIVETS, max)},
#' giving the combined p-values obtained when the object was created
#' directly from a vector of reported p-values. \code{NULL} when the
#' object was created from a \code{T_slicer_res} result.}
#' \item{\code{initialize(Student, Welch, Provided_p)}}{Constructor. Each
#' argument is optional; unspecified fields remain \code{NULL}.}
#' \item{\code{print()}}{Nicely formatted console representation of the
#' stored results (prints RIVETS value plus min/max combinations).}
#' }
#'
#' @description
#' Lightweight R6 container used to hold and pretty-print the output of
#' \code{\link{carlisle}}. The object stores up to two sets of aggregated
#' results (one per t-test assumption: Student and Welch) or a single set when
#' the aggregation was performed directly on a vector of reported p-values.
#'
#' @details
#' The three-element vectors stored in \code{Student}, \code{Welch} and
#' \code{Provided_p} follow the convention
#' \code{c(min_combined_p, RIVETS_combined_p, max_combined_p)}:
#' \itemize{
#' \item \strong{min_combined_p}: minimum combined p-value across the
#' enumerated rounding-consistent alternatives;
#' \item \strong{RIVETS_combined_p}: combined p-value computed from
#' the central/reported values (the 'RIVETS' case in the package);
#' \item \strong{max_combined_p}: maximum combined p-value across the
#' enumerated alternatives.
#' }
#' The object is principally intended for human inspection (via
#' \code{print}) and programmatic access to the numeric results.
#'
#' @param Student Optional numeric vector (length 3) to initialise \code{Student}.
#' @param Welch Optional numeric vector (length 3) to initialise \code{Welch}.
#' @param Provided_p Optional numeric vector (length 3) to initialise \code{Provided_p}.
#'
#' @return An R6 object of class \code{Carlisle_res} with fields described above
#' and an S3-friendly \code{print} method implemented as an R6 public method.
#'
#' @examples
#' ## Construct directly from combined p-values (min, RIVETS, max)
#' cr1 <- Carlisle_res$new(Provided_p = c(0.012, 0.045, 0.098))
#' print(cr1)
#'
#' ## Construct with separate Student and Welch results (min, RIVETS, max for each)
#' cr2 <- Carlisle_res$new(
#' Student = c(0.001, 0.020, 0.150),
#' Welch = c(0.002, 0.025, 0.160)
#' )
#' print(cr2)
#'
#' ## Typical usage: returned by carlisle(); no manual construction needed
#' @seealso \code{\link{carlisle}}, \code{\link{t_slicer}}
#' @export
Carlisle_res <- R6::R6Class("Carlisle_res",
                            public = list(
                              #' @field Student Carlisle min/RIVETS/max test results using
                              #' the range of Student p values from a T_slicer_res object
                              Student = NULL,
                              #' @field Welch Carlisle min/RIVETS/max test results using
                              #' the range of Welch p values from a T_slicer_res object
                              Welch = NULL,
                              #' @field Provided_p Carlisle min/RIVETS/max test results using
                              #' the range of p values manually provided
                              Provided_p = NULL,
                              #' @description
                              #' Create a new Carlisle_res object
                              #' @param Student Student res
                              #' @param Welch Welch res
                              #' @param Provided_p Provided_p res
                              #' @return A new `Carlisle_res` object
                              initialize = function(Student = NULL,
                                                    Welch = NULL,
                                                    Provided_p = NULL) {
                                self$Student <- Student
                                self$Welch <- Welch
                                self$Provided_p <- Provided_p
                              },
                              #' @description
                              #' Pretty printing of Carlisle results
                              print = function() {
                                cat("Results from Carlisle\n")
                                cat("---------------------------------------\n")
                                if(is.null(self$Provided_p)) {
                                  cat("RIVETS p values:\n")
                                  cat("      Student: p =", self$Student[2], "\n")
                                  cat("      Welch:   p =", self$Welch[2], "\n\n")
                                  cat("Min/max p value combinations:\n")
                                  cat("  - Minimum\n")
                                  cat("      Student: p =", self$Student[1], "\n")
                                  cat("      Welch:   p =", self$Welch[1], "\n\n")
                                  cat("  - Maximum\n")
                                  cat("      Student: p =", self$Student[3], "\n")
                                  cat("      Welch:   p =", self$Welch[3], "\n")
                                }
                                else {
                                  cat("RIVETS p values            :", self$Provided_p[2], "\n")
                                  cat("minimum p value combination:", self$Provided_p[1], "\n")
                                  cat("maximum p value combination:", self$Provided_p[3], "\n")
                                }
                                cat("---------------------------------------\n")
                              }
                            )
)

#' Carlisle test for aggregate unusualness of baseline p-values
#'
#' Evaluate whether a set of reported baseline-comparison p-values is
#' unusually concentrated near 0 or 1 — a diagnostic commonly used to screen
#' randomized trials for anomalies in their Table 1 (imbalances or
#' improbably similar groups). The function implements an aggregated
#' Z-score (Stouffer-style) combination of (possibly rounded) p-values
#' and returns the range of possible combined p-values given rounding
#' uncertainty. If supplied with the output of \code{\link{t_slicer}}, the
#' function performs the aggregation separately for the Student and Welch
#' alternatives.
#'
#' @param p Either a character vector of reported p-values (formatted as
#' strings, e.g. \code{".03"}, \code{"0.12"}, \code{"<.001"}) or a
#' \code{T_slicer_res} object produced by \code{\link{t_slicer}}.
#' If a character vector is provided, the function treats each element as a
#' (possibly rounded) reported p and constructs plausible \code{minimum}
#' / \code{original} / \code{maximum} values via \code{\link{validate_p}}.
#' @param output Logical scalar (default \code{FALSE}). If \code{TRUE} the
#' returned \code{Carlisle_res} object is printed before being returned.
#'
#' @return An object of class \code{Carlisle_res} (an R6 result container).
#' When \code{p} is a character vector the returned object contains a single
#' vector named \code{Provided_p} with three elements:
#' \code{c(min_combined, RIVETS_combined, max_combined)}, where
#' \code{RIVETS_combined} is the combined p-value obtained from the
#' reported (central) p-values and \code{min_combined} / \code{max_combined}
#' are the extreme combined p-values produced by enumerating plausible
#' round-off bounds. When \code{p} is a \code{T_slicer_res} object, the
#' returned object contains separate \code{Student} and \code{Welch}
#' components each structured as \code{c(min, RIVETS, max)}.
#'
#' @details Method and assumptions:
#' \itemize{
#' \item Each input p-value is treated as possibly rounded; the function
#' uses \code{\link{validate_p}} to obtain a \emph{minimum}, \emph{original}
#' and \emph{maximum} candidate for each reported p-value and then
#' enumerates the Cartesian product of candidate choices to obtain bounds
#' on the combined statistic.
#' \item For a given set of k independent p-values the
#' function uses the Stouffer (inverse-normal / Z-score) method to combine
#' them.
#' The combination therefore assumes approximate independence of the
#' underlying tests and that the global null (all individual nulls true)
#' holds under the null distribution.
#' \item When a \code{T_slicer_res} is provided the procedure aggregates the
#' min/RIVETS/max p-values produced separately for Student and Welch
#' t-tests to give two separate combined results (one for each
#' t-test assumption).}
#'
#' @section Interpretation and caveats:
#' \itemize{
#' \item A combined p-value very close to 0 (or very close to 1 using
#' the one-sided mapping implemented here) indicates that the baseline
#' table contains more extreme small (or large) p-values than
#' expected under chance; such departures can reflect genuine
#' systematic differences, reporting errors (e.g. mislabelled SD vs
#' SE), or data fabrication — the method is a screen, not proof.
#' \item The combining method assumes independence of tests; correlated
#' baseline variables (e.g. weight and BMI) can invalidate the
#' independence assumption and make the combined p-value
#' anticonservative. Consider this when interpreting results.}
#' @seealso \code{\link{t_slicer}}
#' @examples
#' carlisle(c(".99", ".95", ".98", ".90"), output=TRUE)
#' data <- data.frame(m1 = c("1.20", "1.25"), s1 = c("1.2", "1.25"), n1 = c(60, 60),
#'                    m2 = c("2.1", "2.15"), s2 = c("2.5", "2.55"), n2 = c(30, 30),
#'                    p = c(NA, NA))
#' res <- t_slicer(m1 = m1, s1 = s1, n1 = n1, m2 = m2, s2 = s2, n2 = n2,
#'                 data = data, output = TRUE)
#' carlisle(res, output = TRUE)
#'
#' @importFrom stats qnorm
#' @export
carlisle <- function(p, output = FALSE) {

  if(class(p)[1]!="T_slicer_res") {
    res_list <- list()
    carlisle_res <- c()

    p <- validate_p(p)

    for(i in seq_len(length(p$original))) {
      res_list[[i]] <- c(p$minimum[i], p$maximum[i])
    }
    cases_grid <- expand.grid(res_list)

    for(i in seq_len(nrow(cases_grid))) {
      carlisle_res <- c(carlisle_res, 1 - pnorm(sum(sapply(unlist(cases_grid[i,]), qnorm))/sqrt(length(unlist(cases_grid[i,])))))
    }
    carlisle_RIVETS <- 1 - pnorm(sum(sapply(p$original, qnorm))/sqrt(length(p$original)))

    result <- Carlisle_res$new(Provided_p = c(min(carlisle_res), carlisle_RIVETS, max(carlisle_res)))
  }
  else {
    res_Student <- list()
    res_Welch <- list()
    carlisle_Student_res <- c()
    carlisle_Welch_res <- c()

    for(i in seq_len(length(p$Student_RIVETS))) {
      res_Student[[i]] <- c(p$Student_min[i], p$Student_max[i])
    }

    cases_grid <- expand.grid(res_Student)


    for(i in seq_len(nrow(cases_grid))) {
      carlisle_Student_res <- c(carlisle_Student_res, 1 - pnorm(sum(sapply(unlist(cases_grid[i,]), qnorm))/sqrt(length(unlist(cases_grid[i,])))))
    }
    carlisle_Student_RIVETS <- 1 - pnorm(sum(sapply(p$Student_RIVETS, qnorm))/sqrt(length(p$Student_RIVETS)))

    for(i in seq_len(length(p$Welch_RIVETS))) {
      res_Welch[[i]] <- c(p$Welch_min[i], p$Welch_max[i])
    }

    cases_grid <- expand.grid(res_Welch)
    carlisle_Welch_res <- c()

    for(i in seq_len(nrow(cases_grid))) {
      carlisle_Welch_res <- c(carlisle_Welch_res, 1 - pnorm(sum(sapply(unlist(cases_grid[i,]), qnorm))/sqrt(length(unlist(cases_grid[i,])))))
    }
    carlisle_Welch_RIVETS <- 1 - pnorm(sum(sapply(p$Welch_RIVETS, qnorm))/sqrt(length(p$Welch_RIVETS)))

    result <- Carlisle_res$new(Student = c(min(carlisle_Student_res), carlisle_Student_RIVETS, max(carlisle_Student_res)),
                               Welch = c(min(carlisle_Welch_res), carlisle_Welch_RIVETS, max(carlisle_Welch_res)))

  }

  if(output==TRUE) {
    print(result)
  }
  return(result)
}
