#' Counter Stat
#'
#' @description
#' A simple stat that counts the number of data points seen so far.
#' 
#' @field n Integer count of data points observed.
#' 
#' @examples
#' 
#' counter <- Counter()
#' update(counter, new_x = c(1, 2, 3))
#' value(counter)
#' 
#' @export
Counter <- S7::new_class("Counter",
  parent = Stat,
  properties = list(
    state = S7::class_environment
  ),
  constructor = function(stream = NULL) {
    state <- new.env(parent = emptyenv())
    state$n <- 0L
    state$ns <- integer(0)
    S7::new_object(
      S7::S7_object(),
      stream = stream,
      description = "Data point counter",
      state = state
    )
  }
)

#' @export
S7::method(update, Counter) <- function(stat, new_x = NULL, ...) {
  # If no new_x provided, fetch from stream
  if (is.null(new_x)) {
    if (is.null(stat@stream)) {
      stop("You must provide `new_x` when no stream is available.")
    }
    new_x <- fetch(stat@stream)
  }

  new_len <- length(new_x)

  # Update statistics
  stat@state$values <- c(
    stat@state$values,
    stat@state$n + seq_len(new_len)
  )
  stat@state$n <- stat@state$n + new_len
  
  invisible(stat)
}

#' @export
S7::method(reset, Counter) <- function(object, ...) {
  # Reset count to zero
  object@state$n <- 0L
  object@state$values <- integer(0)
  
  # Reset stream if attached
  if (!is.null(object@stream)) {
    reset(object@stream)
  }
  
  invisible(object)
}

#' @export
S7::method(is_stopped, Counter) <- function(stat, ...) {
  # Counter never stops
  FALSE
}

#' @export
S7::method(print, Counter) <- function(x, ...) {
  cat("Counter Statistic\n")
  cat("Number of observations:", x@state$n, "\n")
  if (!is.null(x@stream)) {
    cat("Stream attached: Yes\n")
  } else {
    cat("Stream attached: No\n")
  }
  invisible(x)
}

#' @export
S7::method(value, Counter) <- function(stat, n = 1L, ...) {
  tail(stat@state$values, n = n)
}

#' @export
S7::method(n, Counter) <- function(stat, ...) {
  stat@state$n
}