#' R6 class: container for t_slicer() results
#'
#' Lightweight R6 container that holds the outputs produced by
#' \code{\link{t_slicer}}. The object stores per-case p-value summaries for
#' Student (pooled-variance) and Welch (unequal-variance) two-sample
#' t-tests, boolean consistency flags (if p-values were provided),
#' and diagnostic \pkg{ggplot2} plots.
#'
#' @name T_slicer_res
#' @docType class
#' @keywords class
#'
#' @format An R6 object with the following public fields and methods:
#' \describe{
#' \item{\code{Student_RIVETS}}{Numeric vector. Per-case p-values computed
#' from the reported (central) descriptive values for the Student (pooled)
#' t-test.}
#' \item{\code{Welch_RIVETS}}{Numeric vector. Per-case p-values computed
#' from the reported (central) descriptive values for the Welch
#' (unequal-variance) t-test.}
#' \item{\code{Student_min}}{Numeric vector. Per-case minimum plausible
#' p-value for the Student test obtained by enumerating rounding-
#' consistent alternatives.}
#' \item{\code{Student_max}}{Numeric vector. Per-case maximum plausible
#' p-value for the Student test.}
#' \item{\code{Welch_min}}{Numeric vector. Per-case minimum plausible
#' p-value for the Welch test.}
#' \item{\code{Welch_max}}{Numeric vector. Per-case maximum plausible
#' p-value for the Welch test.}
#' \item{\code{Student}}{Logical vector. Per-case indicator whether the
#' reported p-interval (if supplied to \code{t_slicer}) overlaps the
#' plausibility interval for the Student test (\code{TRUE} = consistent,
#' \code{FALSE} = inconsistent, \code{NA} = no reported p).}
#' \item{\code{Welch}}{Logical vector. Analogous indicator for the Welch
#' test.}
#' \item{\code{Plots}}{List of \pkg{ggplot2} objects (one per case). Each
#' plot visualises the min/mid/max p-values for Student and Welch and,
#' when available, the reported p-interval.}
#' \item{\code{initialize(Student_RIVETS, Welch_RIVETS, Student_min,
#' Student_max, Welch_min, Welch_max, Student, Welch, Plots)}}{Constructor.
#' All arguments are optional; unspecified fields remain \code{NULL}.}
#' \item{\code{print(n = NULL)}}{Pretty-print results to the console. If
#' \code{n} is \code{NULL} (default) prints all rows; if \code{n} is an
#' integer or integer vector prints only the specified row(s).}
#' \item{\code{plot(n = NULL)}}{Plot method. If \code{n} is \code{NULL}
#' (default) arranges all case plots into a grid (uses
#' \pkg{cowplot}::\code{plot_grid}); if \code{n} is a single integer it
#' draws the plot for that case.}
#' }
#'
#' @details
#' Typical usage: \code{t_slicer()} constructs and returns an instance of
#' \code{T_slicer_res}. The three-element p-value summaries follow the
#' package convention: \code{min} (extreme lower), \code{RIVETS} (computed from
#' reported central values), and \code{max} (extreme upper). The boolean fields
#' \code{Student} and \code{Welch} indicate whether the reported p-interval
#' overlaps the corresponding test's plausible p-interval.
#'
#' The \code{Plots} field contains \pkg{ggplot2} objects; printing the object
#' does not automatically draw plots (use \code{plot(obj)} or call
#' \code{t_slicer(..., output = TRUE)} to display graphs).
#'
#' @param Student_RIVETS Numeric vector to initialise \code{Student_RIVETS}.
#' @param Welch_RIVETS Numeric vector to initialise \code{Welch_RIVETS}.
#' @param Student_min Numeric vector to initialise \code{Student_min}.
#' @param Student_max Numeric vector to initialise \code{Student_max}.
#' @param Welch_min Numeric vector to initialise \code{Welch_min}.
#' @param Welch_max Numeric vector to initialise \code{Welch_max}.
#' @param Student Logical vector to initialise \code{Student}.
#' @param Welch Logical vector to initialise \code{Welch}.
#' @param Plots List of \pkg{ggplot2} objects to initialise \code{Plots}.
#' @return An R6 object of class \code{T_slicer_res} for programmatic access and
#' human inspection (via \code{print} and \code{plot}).
#'
#' @examples
#' ## Manually construct (rare; normally returned by t_slicer)
#' tr <- T_slicer_res$new(
#' Student_RIVETS = c(0.12, 0.05),
#' Welch_RIVETS = c(0.11, 0.06),
#' Student_min = c(0.08, 0.02),
#' Student_max = c(0.20, 0.10),
#' Welch_min = c(0.07, 0.03),
#' Welch_max = c(0.22, 0.12),
#' Student = c(TRUE, FALSE),
#' Welch = c(TRUE, FALSE),
#' Plots = list(NULL, NULL)
#' )
#' print(tr) # pretty console output
#' \dontrun{ plot(tr) } # display diagnostic plots (requires ggplot2 / cowplot)
#'
#' @seealso \code{\link{t_slicer}}, \code{\link{validate_descriptive}},
#' \code{\link{validate_p}}
#'
#' @importFrom cowplot plot_grid
#' @export
T_slicer_res <- R6::R6Class("T_slicer_res",
            public = list(
              #' @field Student_RIVETS p-value(s) from Student's t-test using
              #' RIVETS stats
              Student_RIVETS = NULL,
              #' @field Welch_RIVETS p-value(s) from Welch's t-test using
              #' RIVETS stats
              Welch_RIVETS = NULL,
              #' @field Student_min p-value(s) from Student's t-test using
              #' minimum possible values
              Student_min = NULL,
              #' @field Student_max p-value(s) from Student's t-test using
              #' maximum possible values
              Student_max = NULL,
              #' @field Welch_min p-value(s) from Welch's t-test using
              #' minimum possible values
              Welch_min = NULL,
              #' @field Welch_max p-value(s) from Welch's t-test using
              #' maximum possible values
              Welch_max = NULL,
              #' @field Student are statistics consistent with a Student's t-test?
              Student = NULL,
              #' @field Welch are statistics consistent with a Welch's t-test?
              Welch = NULL,
              #' @field Plots the resulting plot
              Plots = NULL,
              #' @description
              #' Create a new T_slicer_res object
              #' @param Student_RIVETS Numeric vector to initialise \code{Student_RIVETS}.
              #' @param Welch_RIVETS Numeric vector to initialise \code{Welch_RIVETS}.
              #' @param Student_min Numeric vector to initialise \code{Student_min}.
              #' @param Student_max Numeric vector to initialise \code{Student_max}.
              #' @param Welch_min Numeric vector to initialise \code{Welch_min}.
              #' @param Welch_max Numeric vector to initialise \code{Welch_max}.
              #' @param Student Logical vector to initialise \code{Student}.
              #' @param Welch Logical vector to initialise \code{Welch}.
              #' @param Plots List of \pkg{ggplot2} objects to initialise \code{Plots}.
              #' @return An R6 object of class \code{T_slicer_res} for programmatic access and
              #' human inspection (via \code{print} and \code{plot}).
              initialize = function(Student_RIVETS, Welch_RIVETS, Student_min,
                                    Student_max, Welch_min, Welch_max,
                                    Student, Welch, Plots) {
                self$Student_RIVETS <- Student_RIVETS
                self$Welch_RIVETS <- Welch_RIVETS
                self$Student_min <- Student_min
                self$Student_max <- Student_max
                self$Welch_min <- Welch_min
                self$Welch_max <- Welch_max
                self$Student <- Student
                self$Welch <- Welch
                self$Plots <- Plots
              },
              #' @description
              #' Pretty printing of t_slicer results
              #' @param n if NULL (default), outputs results for all
              #' lines. Otherwise, only the one from the specified line.
              print = function(n=NULL) {
                cat("Results from t_slicer\n")
                if(is.null(n) || n>length(self$Student) || n<=0)
                  n <- 1:(length(self$Student))
                for(i in n) {
                  cat("\n\n--- Line", i,"---\n")
                  cat("---------------------------------------\n")
                  cat("RIVETS p values:\n")
                  cat("      Student: p =", self$Student_RIVETS[i], "\n")
                  cat("      Welch:   p =", self$Welch_RIVETS[i], "\n\n")
                  cat("Min/max p values:\n")
                  cat("  - Minimum\n")
                  cat("      Student: p =", self$Student_min[i], "\n")
                  cat("      Welch:   p =", self$Welch_min[i], "\n\n")
                  cat("  - Maximum\n")
                  cat("      Student: p =", self$Student_max[i], "\n")
                  cat("      Welch:   p =", self$Welch_max[i], "\n")
                  cat("---------------------------------------\n")
                  cat("Student's t:  ", if(is.na(self$Student[i])) "N/A\n"
                      else if (self$Student[i] == TRUE) "Consistent\n"
                      else "Inconsistent\n")
                  cat("Welch's t:    ", if(is.na(self$Welch[i])) "N/A\n"
                      else if (self$Welch[i] == TRUE) "Consistent\n"
                      else "Inconsistent\n")
                }
              },
              #' @description
              #' Plots the results.
              #' @param n if NULL (default), plots a grid of all graphs for all
              #' lines. Otherwise, plots the graph for the specified line.
              #' @importFrom cowplot plot_grid
              plot = function(n = NULL) {
                if(is.null(n) || n>length(self$Student) || n<=0)
                  plot_grid(plotlist = self$Plots, ncol=2)
                else {
                  plot(self$Plots[[n]])
                }
              }
              )
            )

#' Compute Student and Welch t-tests using descriptive stats, and compare to reported results
#'
#' Evaluate which two-sample t-test (Student's pooled-variance or Welch's
#' unequal-variance) is consistent with reported summary statistics and — if
#' provided — a reported p-value. The function accounts for rounding
#' imprecision by working with intervals of possible reported values and
#' enumerating the plausible combinations to produce minimum/maximum possible
#' p-values for each test, plus the p-value computed from the reported
#' (central) values ("RIVETS").
#'
#' t_slicer takes as input 2 sets of descriptive statistics (means and sds)
#' and (optionally) reported p values. It computes the results of the Student's
#' and Welch's t tests, and compares the resulting with the reported p values
#' (if provided) to see which test(s) - if any - match(es)
#'
#' This is useful when a paper or report gives only means, standard deviations
#' and sample sizes (and perhaps a rounded p-value) and you want to check
#' which model of the t-test the reported result is compatible with. It is also
#' useful as an initial calculation leading to a \code{carlisle} function call,
#' in case the p-values were not given.
#'
#' @param m1 Character scalar/vector. Reported mean(s) for group 1. Values may
#' be formatted numbers (e.g. "1.23") and may represent rounded reports;
#' the function will construct a plausible interval around the reported value.
#' @param s1 Character scalar/vector. Reported standard deviation(s) for group 1
#' (same format and interpretation as \code{m1}).
#' @param n1 Numeric scalar/vector. Sample size(s) for group 1 (one integer per
#' case).
#' @param m2 Character scalar/vector. Reported mean(s) for group 2 (same
#' conventions as \code{m1}).
#' @param s2 Character scalar/vector. Reported standard deviation(s) for group 2
#' (same conventions as \code{s1}).
#' @param n2 Numeric scalar/vector. Sample size(s) for group 2 (one integer per
#' case).
#' @param p Optional character scalar/vector. Reported p-value(s). If
#' provided the function constructs an interval that reflects rounding
#' precision and checks whether the reported interval overlaps the test
#' interval(s). Use \code{NA} (or omit) when no p is reported.
#' @param data Optional dataframe. If supplied, \code{m1}, \code{s1}, \code{n1},
#' \code{m2}, \code{s2}, \code{n2} and \code{p} are interpreted as column
#' names (unquoted or quoted) and resolved inside \code{data}
#' @param output Logical scalar (default \code{FALSE}). When \code{TRUE} the
#' function prints a human-readable summary and plots the diagnostic plot(s)
#' before returning the result object.
#' @return An object of class \code{T_slicer_res} (an R6 result container). The
#' object contains the following components (each a vector of length equal to
#' the number of input cases):
#' \itemize{
#' \item \code{Student_min}, \code{Student_RIVETS}, \code{Student_max}:
#' minimum, reported (RIVETS), and maximum plausible p-values for the
#' Student (pooled variance, \code{var.equal = TRUE}) t-test.
#' \item \code{Welch_min}, \code{Welch_RIVETS}, \code{Welch_max}:
#' analogous quantities for the Welch (unequal-variance) t-test.
#' \item \code{Student}, \code{Welch}: logical vectors indicating whether
#' the reported p-interval (if supplied) is consistent with the
#' corresponding test (TRUE = overlap; FALSE = no overlap; NA = no
#' reported p).
#' \item \code{Plots}: list of \pkg{ggplot2} objects (one per case)
#' visualising the min/mid/max p-values and, when available, the
#' reported-p interval.
#' }
#' @details Implementation notes:
#' \enumerate{
#' \item Each reported descriptive value (means and SDs) is passed through
#' \code{\link{validate_descriptive}} which returns a \emph{minimum},
#' \emph{original} and \emph{maximum} candidate — reflecting the possible
#' values consistent with the reported (rounded) number.
#' \item The function forms the (Cartesian) grid of plausible combinations of
#' the minimum/maximum candidates for the two means and two SDs. For every
#' combination it computes two-sample t-test p-values using
#' \pkg{BSDA}::\code{tsum.test} with \code{var.equal = TRUE} (Student) and
#' \code{var.equal = FALSE} (Welch). The minimum and maximum p-values
#' across the grid are returned together with the p-value computed
#' from the reported (central) values (RIVETS).
#' \item If a reported p-value is supplied, it is interpreted as an
#' interval (via \code{\link{validate_p}}) and compared to the test
#' intervals; the boolean pass/fail flags indicate overlap.
#' \item A diagnostic plot is produced for each case: points show the
#' (min, mid, max) p-values for each test and, when applicable, a
#' shaded rectangle visualises the reported p-interval.
#' }
#'
#' @section Assumptions and limitations:
#' \itemize{
#' \item The numeric sample sizes \code{n1} and \code{n2} should be integers
#' ≥2. Very small sample sizes (e.g. n<5) make the t-test
#' approximation unstable and increase sensitivity to rounding.
#' \item The function depends on \pkg{BSDA}::\code{tsum.test} for fast
#' two-sample t-test computation from summary statistics; its
#' results follow the same conventions as that function.
#' }
#' @seealso \code{\link{validate_descriptive}}, \code{\link{validate_p}},
#' \pkg{BSDA}::\code{tsum.test}
#' @examples
#' ## Single reported comparison (with a reported p-value)
#' t_slicer(m1 = "1.20", s1 = "1.2", n1 = 60,
#' m2 = "2.10", s2 = "2.5", n2 = 30,
#' p = ".08", output = TRUE)
#'
#' ## Multiple rows supplied in a data.frame (columns may be quoted names)
#' df <- data.frame(
#' m1 = c("1.20", "1.25"),
#' s1 = c("1.2", "1.25"),
#' n1 = c(60, 60),
#' m2 = c("2.10", "2.15"),
#' s2 = c("2.5", "2.55"),
#' n2 = c(30, 30),
#' p = c(NA, NA)
#' )
#' t_slicer(m1 = m1, s1 = s1, n1 = n1,
#' m2 = m2, s2 = s2, n2 = n2,
#' data = df, output = TRUE)
#'
#' @import ggplot2
#' @importFrom BSDA tsum.test
#' @importFrom rlang enquo
#' @importFrom ggnewscale new_scale
#' @export
t_slicer <- function (m1, s1, n1, m2, s2, n2, p = NULL, data = NULL, output = FALSE)
{
  # Input validation
  if(!is.null(data)) {
    if(!is.data.frame(data)) stop("`data` must be a data.frame.")
    m1 <- resolve(enquo(m1), data); s1 <- resolve(enquo(s1), data);
    n1 <- resolve(enquo(n1), data); m2 <- resolve(enquo(m2), data);
    s2 <- resolve(enquo(s2), data); n2 <- resolve(enquo(n2), data);
    p  <- resolve(enquo(p), data)
  }
  L <- length_check(list(m1=m1, s1=s1, m2=m2, s2=s2, n1=n1, n2=n2, p=p))
  m1 <- validate_descriptive(m1)
  s1 <- validate_descriptive(s1)
  m2 <- validate_descriptive(m2)
  s2 <- validate_descriptive(s2)
  p <- if(!is.null(p)) validate_p(p, allow_na = TRUE)
        else validate_p(rep(NA_character_, L), allow_na = TRUE)

  plots <- list()
  Student_pass <- c()
  Welch_pass <- c()
  Student_min <- c()
  Student_RIVETS <- c()
  Student_max <- c()
  Welch_min <- c()
  Welch_RIVETS <- c()
  Welch_max <- c()

  for (j in seq_len(L)) {
    m1_range <- c(m1$minimum[j], m1$maximum[j])
    m1_num <- m1$original[j]
    s1_range <- c(s1$minimum[j], s1$maximum[j])
    s1_num <- s1$original[j]
    m2_range <- c(m2$minimum[j], m2$maximum[j])
    m2_num <- m2$original[j]
    s2_range <- c(s2$minimum[j], s2$maximum[j])
    s2_num <- s2$original[j]
    p_range <- c(p$minimum[j], p$maximum[j])
    p_num <- p$original[j]

    cases_grid <- expand.grid(m1 = m1_range, s1 = s1_range, m2 = m2_range, s2 = s2_range)

    # RIVETS values
    twelch <- tsum.test(mean.x = m1_num, s.x = s1_num, n.x = n1[j], mean.y = m2_num, s.y = s2_num, n.y = n2[j], var.equal = FALSE)$p.value
    tstudent <- tsum.test(mean.x = m1_num, s.x = s1_num, n.x = n1[j], mean.y = m2_num, s.y = s2_num, n.y = n2[j], var.equal = TRUE)$p.value

    # Get all possible min and max combinations for means and sds for g1 and g2
    p_twelch_grid <- c()
    p_tstudent_grid <- c()
    for(i in seq_len(nrow(cases_grid)))
    {
      p_twelch_grid <- c(p_twelch_grid, tsum.test(mean.x = cases_grid[i, "m1"], s.x = cases_grid[i, "s1"], n.x = n1, mean.y = cases_grid[i, "m2"], s.y = cases_grid[i, "s2"], n.y = n2, var.equal = FALSE)$p.value)
      p_tstudent_grid <- c(p_tstudent_grid, tsum.test(mean.x = cases_grid[i, "m1"], s.x = cases_grid[i, "s1"], n.x = n1, mean.y = cases_grid[i, "m2"], s.y = cases_grid[i, "s2"], n.y = n2, var.equal = TRUE)$p.value)
    }

    Student_min <- c(Student_min, min(p_tstudent_grid))
    Student_RIVETS <- c(Student_RIVETS, tstudent)
    Student_max <- c(Student_max, max(p_tstudent_grid))
    Welch_min <- c(Welch_min, min(p_twelch_grid))
    Welch_RIVETS <- c(Welch_RIVETS, twelch)
    Welch_max <- c(Welch_max, max(p_twelch_grid))

    # Plot
    plot_data <- data.frame(
      Student = c(Student_min[j], Student_RIVETS[j], Student_max[j]),
      Welch = c(Welch_min[j], Welch_RIVETS[j], Welch_max[j]),
      Value = c("Minimum", "Mid", "Maximum")
    )

    if(!is.na(p_num)) {
      rect_data <- data.frame(
        Reported = c("Student", "Welch"),
        # Right now rect X coordinates are hard-coded, but would have to be changed
        # if more tests are added
        Xmin = c(0.65, 1.65),
        Xmax = c(1.35, 2.35),
        Ymin = c(p_range[1], p_range[1]),
        Ymax = c(p_range[2], p_range[2])
      )
      if(p_range[1] <= Student_max[j] && p_range[2] >= Student_min[j]) {
        color_rect_student <- "green"
        Student_pass <- c(Student_pass, TRUE)
      }
      else {
        color_rect_student <- "red"
        Student_pass <- c(Student_pass, FALSE)
      }
      if(p_range[1] <= Welch_max[j] && p_range[2] >= Welch_min[j]) {
        color_rect_welch <- "green"
        Welch_pass <- c(Welch_pass, TRUE)
      }
      else {
        color_rect_welch <- "red"
        Welch_pass <- c(Welch_pass, FALSE)
      }
    }
    else {
      Student_pass <- c(Student_pass, NA)
      Welch_pass <- c(Welch_pass, NA)
    }

    plots[[j]] <- ggplot(plot_data) +
      geom_point(aes(x="Welch", y=.data$Welch, shape=.data$Value, fill=.data$Value), size = 5) +
      geom_point(aes(x="Student", y=.data$Student, shape=.data$Value, fill=.data$Value), size = 5) +
      theme_minimal()  +
      labs(title = "T-Test Results", y = "Potential p-values", x = "Tests") +
      theme(axis.text = element_text(size=12),
            axis.title = element_text(size=13, face="bold")) +
      scale_fill_manual(
        name= "Value",
        values = c('Minimum' = "red", 'Mid' = "blue", 'Maximum' = "red")) +
      scale_shape_manual(
        name="Value",
        values = c('Minimum' = 24, 'Mid' = 21, 'Maximum' = 25))
    if(!is.na(p_num)) {
      plots[[j]] <- plots[[j]] + new_scale("fill") +
        geom_rect(
          data = rect_data,
          aes(xmin=.data$Xmin, xmax=.data$Xmax, ymin=.data$Ymin,
              ymax=.data$Ymax, fill=.data$Reported),
          alpha=.5, color="black") +
        scale_fill_manual(
          values = c('Student' = color_rect_student, 'Welch' = color_rect_welch))
    }

  }

  # Return value
  result <- T_slicer_res$new(Student_RIVETS = Student_RIVETS,
                             Student = Student_pass, Welch = Welch_pass,
                             Welch_max = Welch_max,
                             Welch_RIVETS = Welch_RIVETS,
                             Welch_min = Welch_min,
                             Student_max = Student_max,
                             Student_min = Student_min,
                             Plots = plots)

  if(output == TRUE)
  {
    plot(result)
    print(result)
  }

  return(result)
}
