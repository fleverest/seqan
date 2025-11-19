#' Sequential Probability Ratio Test (SPRT)
#'
#' @description
#' Implements Wald's Sequential Probability Ratio Test for testing between
#' two simple hypotheses. The test continues until sufficient evidence is
#' collected to accept one hypothesis or the other.
#'
#' @details
#' The SPRT tests H0: θ = θ0 vs H1: θ = θ1 by computing the log-likelihood
#' ratio statistic:
#'   
#'   LLR = sum(log(f1(x_i) / f0(x_i)))
#'   
#' where f0 and f1 are the likelihood functions under H0 and H1.
#' 
#' Decision rules:
#' - If LLR >= log(B), reject H0 (accept H1)
#' - If LLR <= log(A), accept H0 (reject H1)  
#' - Otherwise, continue sampling
#' 
#' Where A = β/(1-α) and B = (1-β)/α for Type I error α and Type II error β.
#'
#' @field log_likelihood_ratio_fn Function that computes log(f1(x)/f0(x)) for a data point
#' @field alpha Nominal Type I error rate (probability of rejecting H0 when true)
#' @field beta Nominal Type II error rate (probability of accepting H0 when H1 is true)
#' @field lower_threshold Numeric Lower decision boundary log(A)
#' @field upper_threshold Numeric Upper decision boundary log(B)
#'
#' @examples
#' # Test if coin is fair (p=0.5) vs biased (p=0.7)
#' # For Bernoulli data, LLR = log((p1/p0)^x * ((1-p1)/(1-p0))^(1-x))
#' llr_fn <- function(x) {
#'   p0 <- 0.5
#'   p1 <- 0.7
#'   x * log(p1/p0) + (1-x) * log((1-p1)/(1-p0))
#' }
#' 
#' sprt <- SPRT(log_likelihood_ratio_fn = llr_fn, alpha = 0.05, beta = 0.05)
#' 
#' # Simulate data and test
#' set.seed(123)
#' data <- rbinom(100, 1, 0.7)  # True p = 0.7
#' update(sprt, new_x = data)
#' value(sprt)  # Current LLR
#' decision(sprt)  # Check decision
#' is_stopped(sprt)  # Check if decision made
#' 
#' @export
SPRT <- S7::new_class("SPRT",
  parent = Test,
  properties = list(
    log_likelihood_ratio_fn = S7::class_function,
    alpha = S7::class_numeric,
    beta = S7::class_numeric,
    lower_threshold = S7::class_numeric,
    upper_threshold = S7::class_numeric
  ),
  constructor = function(log_likelihood_ratio_fn,
                        alpha = 0.05,
                        beta = 0.05,
                        stream = NULL) {
    # Validate error rates
    if (alpha <= 0 || alpha >= 1) {
      stop("alpha must be between 0 and 1")
    }
    if (beta <= 0 || beta >= 1) {
      stop("beta must be between 0 and 1")
    }
    
    # Calculate decision thresholds
    # A = β/(1-α), B = (1-β)/α
    lower_threshold <- log(beta / (1 - alpha))
    upper_threshold <- log((1 - beta) / alpha)
    
    # Initialize state
    state <- new.env(parent = emptyenv())
    state$llr <- 0  # Current log-likelihood ratio
    state$llr_history <- numeric(0)  # History of LLR values
    state$n <- 0L  # Number of observations
    state$decision <- "continue"  # Current decision: "accept_H0", "reject_H0", or "continue"
    
    S7::new_object(
      S7::S7_object(),
      log_likelihood_ratio_fn = log_likelihood_ratio_fn,
      alpha = alpha,
      beta = beta,
      lower_threshold = lower_threshold,
      upper_threshold = upper_threshold,
      stream = stream,
      description = "Sequential Probability Ratio Test",
      state = state
    )
  }
)

#' @export
S7::method(update, SPRT) <- function(stat, new_x = NULL, ...) {
  # If already stopped, notify the user and continue to update, but decision won't change
  was_stopped <- is_stopped(stat)
  if (was_stopped) {
    message("SPRT has already stopped. Use reset() to restart.")
  }
  
  # If no new_x provided, fetch from stream
  if (is.null(new_x)) {
    if (is.null(stat@stream)) {
      stop("You must provide `new_x` when no stream is available.")
    }
    new_x <- fetch(stat@stream)
  }
  
  # If no new data, return
  if (length(new_x) == 0) {
    return(invisible(stat))
  }
  
  # Compute cumulative log-likelihood ratios for new observations
  new_llrs <- vapply(new_x, stat@log_likelihood_ratio_fn, numeric(1))
  
  # Update cumulative LLR
  new_cumulative_llrs <- cumsum(new_llrs) + stat@state$llr
  if (decision(stat) == "continue" && any(new_cumulative_llrs >= stat@upper_threshold | 
                                        new_cumulative_llrs <= stat@lower_threshold)) {
    message("SPRT stopping condition met.")
  }
  stat@state$llr_history <- c(stat@state$llr_history, new_cumulative_llrs)
  stat@state$llr <- stat@state$llr + sum(new_llrs)
  stat@state$n <- stat@state$n + length(new_x)
  
  # Check decision boundaries
  if (was_stopped) {
    return(invisible(stat))  # Do not change decision if already stopped
  }
  if (stat@state$llr >= stat@upper_threshold) {
    stat@state$decision <- "reject_H0"
  } else if (stat@state$llr <= stat@lower_threshold) {
    stat@state$decision <- "accept_H0"
  } else {
    stat@state$decision <- "continue"
  }
  
  invisible(stat)
}

#' @export
S7::method(reset, SPRT) <- function(object, ...) {
  # Reset all state
  object@state$llr <- 0
  object@state$llr_history <- numeric(0)
  object@state$n <- 0L
  object@state$decision <- "continue"
  
  # Reset stream if attached
  if (!is.null(object@stream)) {
    reset(object@stream)
  }
  
  invisible(object)
}

#' @export
S7::method(is_stopped, SPRT) <- function(stat, ...) {
  decision(stat) != "continue"
}

#' @export
S7::method(decision, SPRT) <- function(test, ...) {
  test@state$decision
}

#' @export
S7::method(print, SPRT) <- function(x, ...) {
  cat("Sequential Probability Ratio Test\n")
  cat("Number of observations:", x@state$n, "\n")
  cat("Current LLR:", round(x@state$llr, 4), "\n")
  cat("Decision boundaries: [", round(x@lower_threshold, 4), ",", 
      round(x@upper_threshold, 4), "]\n")
  cat("Decision:", decision(x), "\n")
  cat("Error rates: α =", x@alpha, ", β =", x@beta, "\n")
  if (!is.null(x@stream)) {
    cat("Stream attached: Yes\n")
  } else {
    cat("Stream attached: No\n")
  }
  invisible(x)
}

#' @export
S7::method(value, SPRT) <- function(stat, n = 1L, ...) {
  # Return the most recent n LLR values
  if (length(stat@state$llr_history) == 0) {
    return(numeric(0))
  }
  tail(stat@state$llr_history, n = n)
}
