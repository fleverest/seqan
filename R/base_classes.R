#' @import S7
NULL


#' Data Source Abstract Base Class
#'
#' @description
#' Abstract base class for data sources. A Source provides a unified interface
#' for accessing data from various backends (R objects, databases, files, APIs).
#' Sources are a stateless interface - they don't track position, that's the
#' Stream's job.
#'
#' @details
#' Concrete implementations must provide methods for:
#' - `get_n(source, n, from)`: Get n records starting from position 'from'
#' - `get_rest(source, from)`: Get all remaining records from position 'from'
#' - `get_length(source)`: Get current total length of data source
#'
#' @export
Source <- S7::new_class("Source",
  properties = list(
    description = S7::class_character
  ),
  abstract = TRUE
)

#' Get n records from source
#'
#' @param source A Source object
#' @param n Number of records to retrieve
#' @param from Starting position, 1-indexed
#' @return Vector of records
#' @export
get_n <- S7::new_generic("get_n", "source", function(source, from, n, ...) {
  S7::S7_dispatch()
})

#' Get remaining records from source
#'
#' @param source A Source object  
#' @param from Starting position
#' @return Vector of remaining records
#' @export
get_rest <- S7::new_generic("get_rest", "source", function(source, from, ...) {
  S7::S7_dispatch()
})

#' Get current length of source
#'
#' @param source A Source object
#' @return Integer length of data source
#' @export
get_length <- S7::new_generic("get_length", "source", function(source, ...) {
  S7::S7_dispatch()
})


#' Data Stream Class
#'
#' @description
#' Class for data streams. A Stream manages the flow of data from a Source to a
#' Stat, maintaining state about position and optionally transforming records.
#'
#' @details
#' A Stream:
#' - Wraps a Source and tracks the current position
#' - Optionally transforms records via a transformation function
#' - Provides a stateful interface for fetching unseen data
#'
#' Concrete implementations must provide methods for:
#' - `fetch(stream)`: Fetch new records, transform, and update position state
#' - `reset(stream)`: Reset stream state to initial position
#'
#' @examples
#' 
#' x <- c(0, 1, 0, 0, 1, 1)
#' src <- VectorSource(x)
#' stream <- Stream(source = src, transform_record = function(x) x + 1)
#' fetch(stream) # Returns c(1, 2, 1, 1, 2, 2)
#' fetch(stream) # Returns numeric(0) since no new data
#' x <- c(x, 0, 1)
#' fetch(stream) # Returns c(1, 2)
#' 
#' @export
Stream <- S7::new_class("Stream",
  properties = list(
    source = Source,  # Will be Source when defined, using class_any for now
    transform_records = S7::class_function,  # Function to transform each record
    state = S7::class_environment # For tracking internal state (index, etc)
  ),
  constructor = function(source, current_idx = 1L, transform_records = identity) {
    state <- new.env(parent = emptyenv())
    state$current_idx <- current_idx

    S7::new_object(
      S7::S7_object(),
      source = source,
      transform_records = transform_records,
      state = state
    )
  }
)

#' Fetch new data from stream
#'
#' @param stream A Stream object
#' @return Transformed records fetched from the stream's current position
#' @export
fetch <- S7::new_generic("fetch", "stream", function(stream, ...) {
  S7::S7_dispatch()
})

#' @export
S7::method(fetch, Stream) <- function(stream, ...) {
  # Get current position
  current_idx <- stream@state$current_idx
  
  # Check if we're past the end of the source
  source_length <- get_length(stream@source)
  if (current_idx > source_length) {
    return(numeric(0))  # Or appropriate empty vector type
  }
  
  # Get all remaining data from source
  raw_data <- get_rest(stream@source, from = current_idx)

  # Update state to reflect we've consumed this data
  stream@state$current_idx <- source_length + 1L

  # Transform and return
  stream@transform_records(raw_data)
}

#' Reset object to initial state
#'
#' @param object A Stream or Stat object
#' @return The object (invisibly), with state reset
#' @export
reset <- S7::new_generic("reset", "object", function(object, ...) {
  S7::S7_dispatch()
})

#' @export
S7::method(reset, Stream) <- function(object, ...) {
  # Reset stream position to beginning
  object@state$current_idx <- 1L
  invisible(object)
}

#' Sequential Statistic/Procedure Abstract Base Class
#'
#' @description
#' Abstract base class for sequential statistical procedures. A Stat (or Procedure/
#' Analysis/Test) consumes data and produces sequential statistics, decisions,
#' or estimates.
#'
#' @details
#' A Stat can operate in two modes:
#' 1. Direct data feed: User calls `update(stat, new_x = data)`
#' 2. Stream-based: Stat has an associated Stream, user calls `update(stat)`
#'
#' Concrete implementations must provide methods for:
#' - `update(stat, ...)`: Update internal state with new data
#' - `reset(stat)`: Reset to initial state
#' - `is_stopped(stat)`: Check if procedure has reached a stopping condition
#'
#' Common attributes across implementations:
#' - decision/result: Current state of the analysis
#' - history: Record of sequential statistics over time
#'
#' @name Stat
#' @export
Stat <- S7::new_class("Stat",
  properties = list(
    stream = S7::class_any,  # Optional Stream object, NULL if direct feed
    description = S7::class_character
  ),
  abstract = TRUE,
  constructor = function(stream = NULL, description = "Sequential Statistic") {
    S7::new_object(
      S7::S7_object(),
      stream = stream,
      description = description
    )
  }
)

#' Update a Stat with new data
#'
#' @param object A Stat object
#' @param ... Additional arguments (e.g., new_data to feed new data directly)
#' @return Result/decision after update (implementation-dependent)
#' @export
update <- S7::new_generic("update", "object")

#' Check if procedure has stopped
#'
#' @param stat A Stat object
#' @return Logical indicating if stopping condition reached
#' @export
is_stopped <- S7::new_generic("is_stopped", "stat")

#' Validate index is within bounds
#'
#' @keywords internal
validate_index <- function(idx, max_length, arg_name = "index") {
  if (idx < 1L || idx > max_length) {
    stop(sprintf("%s must be between 1 and %d, got %d", 
                 arg_name, max_length, idx))
  }
  invisible(TRUE)
}

#' Validate n is positive
#'
#' @keywords internal
validate_n <- function(n, arg_name = "n") {
  if (n < 1L) {
    stop(sprintf("%s must be positive, got %d", arg_name, n))
  }
  invisible(TRUE)
}
