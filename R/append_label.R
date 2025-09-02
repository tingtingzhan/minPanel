


#' @title Append to `@label` of \linkS4class{panel}
#' 
#' @param x a \linkS4class{panel}
#' 
#' @param info \link[base]{character} scalar
#' 
#' @note
#' There is no generic function `labels<-` in package \pkg{base} !!!
#' 
#' Function \link[base]{comment<-} is not an S3 generic.
#' 
#' @export
append_label <- function(x, info) {
  
  newlabel <- info |>
    switch(EXPR = _, cum_false_positive = {
      sprintf(
        fmt = 'panelFalse(+) \u2264%d/%d', 
        x |> cum_false_positive() |> max(),
        x@m0 |> ncol()
      )
    })
  
  x@label <- c(x@label, newlabel) |>
    paste(collapse = '\n')

  return(x)
  
}
