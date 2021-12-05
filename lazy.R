

rm(list=ls())
df <- data.frame( a = rnorm(100), b = sample(letters, 100, replace = TRUE) )

train <- df[1:70,]
test <- df[71:100,]

lazy_workflow <- function( .data ) {
  structure(list( steps = NULL,
                  train_steps = NULL,
                  test_steps = NULL,
                  baseline_data = .data ),
            class = "lazy_workflow")
}

append <- function(x, ...) {
  UseMethod("append", x)
}

append.lazy_workflow <- function(x, ...) {
  x[["steps"]] <- c( x[["steps"]], ...)
  return(x)
}

"%>%" <- function( lhs, rhs ) {

  rhs <- substitute(rhs)
  
  if( class(lhs) == "lazy_workflow" &&
      !grepl( pattern = "evaluate", x = rhs) ) 
  {
    return( append(lhs, rhs) )
  }
  lhs <- substitute(lhs)
  kind <- 1L
  env <- parent.frame()
  lazy <- TRUE
  .External2(magrittr:::magrittr_pipe)
}

# train only pipe
# test only pipe

test_workflow <- lazy_workflow(df) %>%
  dplyr::filter( a > -0.5) %>%
  dplyr::mutate( hah = a < 0)

test_pipe_still_works <- df %>%
  dplyr::filter( a > -0.5) %>%
  dplyr::mutate( hah = a < 0)

evaluate <- function(x, ...) {
  UseMethod("evaluate",x)
}

evaluate.lazy_workflow <- function(x, .data = NULL, ...) {
  if( is.null(.data) ) {
    .data <- x[["baseline_data"]]
  }
  for( step in x[["steps"]] ) {
    .data <- .data %>% (step)
  }
  return(.data)
}
