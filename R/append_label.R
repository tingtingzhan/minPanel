


#' @title Append to `@label` of \linkS4class{panel}
#' 
#' @param x a \linkS4class{panel}
#' 
#' @param by \link[base]{character} scalar
#' 
#' @note
#' There is no generic function `labels<-` in package \pkg{base} !!!
#' 
#' Function \link[base]{comment<-} is not an S3 generic.
#' 
#' @export
append_label <- function(x, by) {
  
  newlabel <- by |>
    switch(EXPR = _, cum_false_positive = {
      sprintf(
        fmt = 'cumFalse(+) \u2264%d/%d', 
        #fmt = 'cumFalse\u2795 \u2264%d/%d', 
        x |> cum_false_positive() |> max(),
        x@m0 |> ncol()
      )
    })
  
  if (length(x@label)) {
    x@label <- paste(x@label, newlabel, sep = '\n')
  } else x@label <- newlabel
  
  return(x)
  
}
