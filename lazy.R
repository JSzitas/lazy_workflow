rm(list=ls())

df <- data.frame( a = rnorm(100), b = sample(letters, 100, replace = TRUE) )

train <- df[1:70,]
test <- df[71:100,]

# define a lazy workflow, potentially initialized without data
lazy_workflow <- function( .data) {
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
  purrr::modify_at( x, .at = "steps", .f = c, ... )
}

`%>%` <- function(x, ...) {
  UseMethod("%>%", x)
}
# I do hate to have to redefine magrittr pipe like this, but it is just a very nice 
# operator. There are other solutions, but they dont have such nice dispatch on the 
# first argument
`%>%.default` <- function(x, ...) {
  rhs <- substitute(...)
  lhs <- substitute(x)
  kind <- 1L
  env <- parent.frame()
  lazy <- TRUE
  .External2(magrittr:::magrittr_pipe)
}
# define a new method - so we can dispatch on the class of x 
`%>%.lazy_workflow` <- function( x, ... ) {
  # the substitute is the real hero here - it capturtes the RHS expression, 
  # enabling us to append basically whatever we want
  append(x, substitute(...))
}
# add an evaluation function, and make it generic... which might not be too clever
# and might be overkill, but its better to have the dispatch done this way. 
evaluate <- function(x, ...) {
  UseMethod("evaluate",x)
}
evaluate.lazy_workflow <- function(x, .data = NULL, ...) {
  # we dont allow both .data to be null and to not have any baseline data, so 
  # we only check once
  if( is.null(.data) ) {
    .data <- x[["baseline_data"]]
  }
  purrr::reduce( x[["steps"]],
                 .init = .data,
                 # just pass the data to step (note that the () are needed to convert
                 # to a call )
                 .f = function(.data, step){ .data %>% (step)}  )
}

test_workflow <- df %>% 
  lazy_workflow() %>% 
  dplyr::filter( a > -0.5) %>%
  dplyr::mutate( hah = a < 0)

test_pipe_still_works <- df %>%
  dplyr::filter( a > -0.5) %>%
  dplyr::mutate( hah = a < 0)

train_result <- evaluate(test_workflow)
test_result <- evaluate(test_workflow, test)
