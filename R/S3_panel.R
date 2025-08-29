
# how to save the consort diagram for the full pipeline??




#' @title Extract Rows of \linkS4class{panel}
#' 
#' @param x a \linkS4class{panel}
#' 
#' @param i \link[base]{logical} \link[base]{vector}, row indices
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns
#' Function ?? returns a \linkS4class{panel}.
#' 
#' @export
`[.panel` <- function(x, i, ...) {
  new(
    Class = 'panel',
    m1 = x@m1[i, , drop = FALSE],
    m0 = x@m0[i, , drop = FALSE],
    id = x@id[i]
  )
}







#' @title Select a \link[base]{subset} of \linkS4class{panel}
#' 
#' @param x a \linkS4class{panel}
#' 
#' @param subset a \link[base]{language} object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @keywords internal
#' @export subset.panel
#' @export
subset.panel <- function(x, subset, ...) {
  
  e <- substitute(subset)
  e. <- e
  
  e[[2L]] |>
    deparse1() |>
    switch(EXPR = _, true_positive = {
      e.[[2L]] <- call(name = 'true_positive', quote(x))
      id <- eval(e.)
    }, false_positive = {
      e.[[2L]] <- call(name = 'false_positive', quote(x))
      id <- eval(e.)
    }, 'diff(cum_true_positive)' = {
      e.[[2L]] <- call(name = 'diff', call(name = 'cum_true_positive', quote(x))) # cannot use native pipe!
      id <- c(1L, which(eval(e.)) + 1L)
    })

  x[id, ] # `[.panel`
  
}


#' @title Sort \linkS4class{panel} by Given Criterion
#' 
#' @param x a \linkS4class{panel}
#' 
#' @param y a one-sided \link[stats]{formula}
#' 
#' @param ... additional parameters of \link[base]{order}
#' 
#' @keywords internal
#' @export sort_by.panel
#' @export
sort_by.panel <- function(x, y, ...) {
  
  if (!inherits(y, what = 'formula') || length(y) != 2L) stop('`y` must be one-sided formula')
  
  if (!is.symbol(y[[2L]])) stop('Right-hand-side of `y` must be symbol')
  
  id <- y[[2L]] |> 
    as.character() |>
    call(name = _, quote(x)) |>
    eval() |>
    order(...) # e.g. `decreasing = TRUE`
  
  x[id, ] # `[.panel`
  
}


