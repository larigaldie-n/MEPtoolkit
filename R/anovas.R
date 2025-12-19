#' R6 class: results of an Anova reconstruction function
#'
#' Object container for results produced by \code{anovas} functions.
#'
#' @name Anova_res
#' @docType class
#' @keywords class
#' @seealso \code{\link{anova_one_descriptive}}
#' @export
Anova_res <- R6::R6Class("Anova_res",
                            public = list(
                              #' @field Min Minimum p value from the reconstructed
                              #' ANOVA
                              Min = NULL,
                              #' @field RIVETS  p value from the reconstructed
                              #' ANOVA using RIVETS values
                              RIVETS = NULL,
                              #' @field Max Maximum p value from the reconstructed
                              #' ANOVA
                              Max = NULL,
                              #' @field Function The function that was called to create those results
                              #' (DEV: in case we end up having several anova functions)
                              Function = NULL,
                              #' @description
                              #' Create a new Anova_res object
                              #' @param Min Minimum p value
                              #' @param RIVETS RIVETS p value
                              #' @param Max Max p value
                              #' @param Function The function that was called to create those results
                              #' @return A new `Anova_res` object
                              initialize = function(Min = NULL,
                                                    RIVETS = NULL,
                                                    Max = NULL,
                                                    Function = NULL) {
                                self$Min <- Min
                                self$RIVETS <- RIVETS
                                self$Max <- Max
                                self$Function <- Function
                              },
                              #' @description
                              #' Pretty printing of anovas results
                              print = function() {
                                cat("Results from ", self$Function, "\n")
                                cat("---------------------------------------\n")
                                cat("Minimum p value: ", self$Min, "\n")
                                cat("RIVETS p value:  ", self$RIVETS, "\n")
                                cat("Maximum p value: ", self$Max, "\n")
                                cat("---------------------------------------\n")
                              }
                            )
)

#' Reconstruct results from a one-way ANOVA
#'
#' @description
#' This function allows for the reconstruction of the omnibus test from a one-way ANOVA,
#' using the descriptive statistics for each group.
#'
#' @param m Character vector. Mean for each group. \strong{Must be character} to infer rounding.
#' @param s Character vector. SD for each group. \strong{Must be character} to infer rounding.
#' @param n Numeric vector. Sample size for each group.
#'
#' @return
#' Minimum, RIVETS and maximum p values
#'
#' @examples
#' anova_one_descriptive(m = c("2.51", "3.12"), s = c("0.58", "0.34"), n = c(40, 40))
#'
#' @importFrom stats pf
#' @export
anova_one_descriptive <- function(m, s, n, output=FALSE)
{
  means <- validate_descriptive(m)
  means_RIVETS <- means$original
  SDs <- validate_descriptive(s)
  SDs_RIVETS <- SDs$original
  L <- length_check(list(m=m, s=s, n=n))
  p_value <- c()

  # Degrees of freedom for between and within
  dfb <- L - 1
  dfw <- sum(n) - L

  # Min-max calculations
  grid_list <- list()
  for(i in seq_len(L)) {
    grid_list[[i]] <- c(means$minimum[i], means$original[i], means$maximum[i])
    grid_list[[i+L]] <- c(SDs$minimum[i], SDs$original[i], SDs$maximum[i])
  }

  cases_grid <- expand.grid(grid_list)
  for(i in seq_len(nrow(cases_grid))) {
    overall_mean <- sum(n*cases_grid[i,][1:L])/sum(n)
    MSb <- sum(n * (cases_grid[i,][1:L] - overall_mean)^2)/dfb
    MSw <-  sum((n-1)*cases_grid[i,][(L+1):(2*L)]^2)/dfw
    F_value <- MSb/MSw
    p_value <- c(p_value, pf(F_value, dfb, dfw, lower.tail = FALSE))
  }

  # RIVETS calculations
  overall_mean <- sum(n*means_RIVETS)/sum(n)
  MSb <- sum(n * (means_RIVETS - overall_mean)^2)/dfb
  MSw <-  sum((n-1)*SDs_RIVETS^2)/dfw
  F_value <- MSb/MSw
  p_value_RIVETS <- pf(F_value, dfb, dfw, lower.tail = FALSE)

  result <- Anova_res$new(Min = min(p_value),
                           RIVETS = p_value_RIVETS,
                           Max = max(p_value),
                           Function = "anova_one_descriptive")

  if(output == TRUE)
  {
    print(result)
  }

  return(result)
}
