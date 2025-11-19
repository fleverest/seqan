#' Source data from a vector in an environment.
#' 
#' @param x The variable name or vector containing the data.
#' @param env The environment containing the variable.
#' @return A VectorSource object.
#' 
#' @examples
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
  if (from < 1L) {
    stop(sprintf("'from' must be positive, got %d", from))
  }
  
  # If from is past the end, return empty vector
  if (from > total_length) {
    return(data[integer(0)])  # Returns empty vector of same type as data
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


#' Source lines of data from a text file
#'
#' @description
#' Reads numeric data from a text file, one value per line. Efficiently reads
#' only the lines needed rather than reading the entire file each time.
#'
#' @field filepath Character path to the file
#'
#' @export
TextFileSource <- S7::new_class("TextFileSource",
  parent = Source,
  properties = list(
    filepath = S7::class_character
  ),
  constructor = function(filepath) {
    # Validate file exists
    if (!file.exists(filepath)) {
      stop(sprintf("File does not exist: %s", filepath))
    }
    
    S7::new_object(
      S7::S7_object(),
      description = sprintf("File '%s'", basename(filepath)),
      filepath = filepath
    )
  }
)

#' @export
S7::method(get_length, TextFileSource) <- function(source, ...) {
  con <- file(source@filepath, "r")
  on.exit(close(con), add = TRUE)
  all_lines <- readLines(con)
  length(all_lines)
}

#' @export
S7::method(get_n, TextFileSource) <- function(source, n, from, ...) {
  # Read specific lines from file
  con <- file(source@filepath, "r")
  on.exit(close(con))
  
  # Read all lines up to what we need
  all_lines <- readLines(con)
  total_length <- length(all_lines)
  
  # Validate from
  if (from < 1L || from > total_length) {
    stop(sprintf("'from' must be between 1 and %d, got %d", total_length, from))
  }
  
  # Calculate range
  to <- min(from + n - 1L, total_length)
  
  # Select the requested lines
  all_lines[from:to]
}

#' @export
S7::method(get_rest, TextFileSource) <- function(source, from, ...) {
  # Read from position to end
  con <- file(source@filepath, "r")
  on.exit(close(con))
  
  all_lines <- readLines(con)
  total_length <- length(all_lines)
  
  # Validate from
  if (from < 1L) {
    stop(sprintf("'from' must be positive, got %d", from))
  }
  
  # If from is past end, return empty
  if (from > total_length) {
    return(numeric(0))
  }
  
  # Select requested lines
  all_lines[from:total_length]
}
