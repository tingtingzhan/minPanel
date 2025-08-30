
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
    id = x@id[i],
    label = x@label
  )
}







#' @title Select a \link[base]{subset} of \linkS4class{panel}
#' 
#' @param x a \linkS4class{panel}
#' 
#' @param subset a \link[base]{language} object
#' 
#' @param append.label \link[base]{logical} scalar, whether to 
#' append the criterion `subset` to `x@label`, default `FALSE`
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @keywords internal
#' @export subset.panel
#' @export
subset.panel <- function(x, subset, append.label = FALSE, ...) {
  
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
    }, cum_false_positive = {
      e.[[2L]] <- call(name = 'cum_false_positive', quote(x))
      id <- eval(e.)
    }, 'diff(cum_true_positive)' = {
      e.[[2L]] <- call(name = 'diff', call(name = 'cum_true_positive', quote(x))) # cannot use native pipe!
      id <- c(1L, which(eval(e.)) + 1L)
    })

  ret <- x[id, ] # `[.panel`
  
  if (append.label) {
    
    sign_rel <- e[[1L]] |> 
      deparse1() |> 
      switch(EXPR = _, '<=' = '\u2264', '>=' = '\u2265', '<' = '<', '>' = '>')
    
    newlabel <- e[[2L]] |>
      deparse1() |>
      switch(EXPR = _, false_positive = {
        sprintf(fmt = 'False(+) %s%d/%d', sign_rel, e[[3L]], x@m0 |> ncol())
        #sprintf(fmt = 'False\u2795 %s%d/%d', sign_rel, e[[3L]], x@m0 |> ncol())
      }, cum_false_positive = {
        sprintf(fmt = 'cumFalse(+) %s%d/%d', sign_rel, e[[3L]], x@m0 |> ncol())
        #sprintf(fmt = 'cumFalse\u2795 %s%d/%d', sign_rel, e[[3L]], x@m0 |> ncol())
      })
    
    if (length(ret@label)) {
      ret@label <- paste(ret@label, newlabel, sep = '\n')
    } else ret@label <- newlabel
      
  }
  
  return(ret)
  
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


