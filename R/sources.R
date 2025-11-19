#' Source data from a vector in an environment.
#' 
#' @param x The variable name or vector containing the data.
#' @param env The environment containing the variable.
#' @return A VectorSource object.
#' @example
#' 
#' x <- c(1, 2, 3, 4, 5)
#' src <- VectorSource(x)
#' 
#' # Returns c(1, 2, 3)
#' get_n(src, from = 1, n = 3) 
#' # Returns c(4, 5)
#' get_rest(src, from = 4)
#' 
#' @export
VectorSource <- S7::new_class("VectorSource",
  parent = Source,
  properties = list(
    x = S7::class_character,
    env = S7::class_environment
  ),
  constructor = function(x, env = parent.frame()) {
    # Capture the expression
    x_expr <- substitute(x)
    
    # Handle different cases
    x_name <- if (is.character(x_expr)) {
      # Already a string: VectorSource("our_data")
      x
    } else if (is.name(x_expr)) {
      # Symbol: VectorSource(our_data)
      as.character(x_expr)
    } else {
      # Complex expression: VectorSource(my_list$data)
      # Fall back to string representation
      deparse(x_expr)
    }
    
    # Validate that variable exists in environment
    if (!exists(x_name, envir = env, inherits = FALSE)) {
      stop(sprintf("Variable '%s' not found in the specified environment", x_name))
    }
    
    S7::new_object(
      S7::S7_object(),
      description = sprintf("Vector '%s'", x_name),
      x = x_name,
      env = env
    )
  }
)

#' @export
S7::method(get_n, VectorSource) <- function(source, from, n, ...) {
  # Get the data vector from the environment
  data <- get(source@x, envir = source@env, inherits = FALSE)
  
  # Validate from index
  total_length <- length(data)
  if (from < 1L || from > total_length) {
    stop(sprintf("'from' must be between 1 and %d, got %d", total_length, from))
  }
  
  # Validate n
  if (n < 1L) {
    stop(sprintf("'n' must be positive, got %d", n))
  }
  
  # Calculate end index (don't go past end of data)
  to <- min(from + n - 1L, total_length)
  
  # Return the slice
  data[from:to]
}

#' @export
S7::method(get_rest, VectorSource) <- function(source, from, ...) {
  # Get the data vector from the environment
  data <- get(source@x, envir = source@env, inherits = FALSE)
  
  # Validate from index
  total_length <- length(data)
  if (from < 1L || from > total_length) {
    stop(sprintf("'from' must be between 1 and %d, got %d", total_length, from))
  }
  
  # Return everything from 'from' to end
  data[from:total_length]
}

#' @export
S7::method(get_length, VectorSource) <- function(source, ...) {
  # Get the data vector and return its length
  length(get(source@x, envir = source@env, inherits = FALSE))
}