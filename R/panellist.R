

#' @title `panellist`
#' 
#' @param ... one or more \linkS4class{panel}s
#' 
#' @export
panellist <- function(...) {
  
  z <- list(...)
  # no check written, for now
  
  class(z) <- c('panellist', 'listof', class(z)) |>
    unique.default()
  return(z)
  
}







#' @title Visualize [panellist]
#' 
#' @param object a [panellist]
#' 
#' @param ... additional parameters, currently no use
#' 
#' @keywords internal
#' @name autoplot.panellist
#' @importFrom ggplot2 autoplot
#' @export autoplot.panellist
#' @export
autoplot.panellist <- function(object, ...) {
  ggplot() + 
    autolayer.panellist(object, ...)
}

#' @rdname autoplot.panellist
#' @importFrom ggplot2 autolayer ggplot aes geom_point geom_path scale_y_continuous scale_color_discrete labs
#' @importFrom scales label_percent
#' @export autolayer.panellist
#' @export
autolayer.panellist <- function(object, ...) {
  
  n1 <- object[[1L]]@m1 |>
    ncol()
  
  cum_true_pos <- object |> 
    lapply(FUN = cum_true_positive)
  
  .x <- cum_true_pos |> lapply(FUN = seq_along) |> unlist()
  .y <- (cum_true_pos |> unlist()) / n1
  .group <- object |> 
    seq_along() |> 
    rep(times = lengths(cum_true_pos)) |>
    factor()
  .label <- object |> 
    vapply(FUN = \(i) {
      inm <- i@label
      if (length(inm)) return(inm)
      i@m1 |> nrow() |> as.character()
    }, FUN.VALUE = '')
  
  mp <- aes(x = .x, y = .y, color = .group)
  
  list(
    geom_point(mapping = mp),
    geom_path(mapping = mp, linewidth = .3),
    scale_y_continuous(name = 'True Positives Identified', labels = label_percent(), limits = c(0, 1)),
    scale_color_discrete(name = 'Total Number of\nVariant-Collections\nper Panel', labels = .label),
    labs(
      x = 'Number of Collections per Panel'
    )
  )
  
}







#' @title as_flextable.panellist
#' 
#' @param x a [panellist]
#' 
# @param orig.panel \linkS4class{panel} - must remove!!!
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @importFrom flextable as_flextable flextable autofit color align add_header_row merge_v bold hline highlight
#' @importFrom scales pal_hue
#' @importFrom stats setNames
#' @export as_flextable.panellist
#' @export
as_flextable.panellist <- function(x, ...) { # orig.panel, 
  
  nx <- length(x)
  
  vs <- x |> 
    lapply(FUN = slot, name = 'id')
  
  v0 <- vs |>
    unname() |> # just to be sure
    unlist(recursive = FALSE, use.names = TRUE)
  # unique.default() drops names!
  v1 <- v0[!duplicated(v0)]
  v <- v1[order(names(v1))]
  
  diff_v <- vs |>
    seq_along() |>
    lapply(FUN = \(i) {
      vs[seq_len(i-1L)] |>
        lapply(FUN = names) |> 
        unlist(use.names = FALSE) |>
        setdiff(x = names(vs[[i]]), y = _)
    })
  
  # amount of incremental addition
  # to determine position of ?flextable::hline
  nv <- diff_v |>
    lengths(use.names = FALSE) 
  
  nvr <- v |> lengths(use.names = FALSE) # multiple variants per collection
  
  m1. <- mapply(FUN = \(x, v) {
    x@m1[v, , drop = FALSE]
  }, x = x, v = diff_v, SIMPLIFY = FALSE) |>
    do.call(what = rbind, args = _)
  #m1. <- m1[v, , drop = FALSE]
  #m0. <- m0[v, , drop = FALSE]
  m0. <- mapply(FUN = \(x, v) {
    x@m0[v, , drop = FALSE]
  }, x = x, v = diff_v, SIMPLIFY = FALSE) |>
    do.call(what = rbind, args = _)
  
  d <- data.frame(
    'Variant-Collection' = v |> names(),
    'True(+)' = sprintf(fmt = '%d/%d', rowSums(m1.), ncol(m1.)),
    'False(+)' = sprintf(fmt = '%d/%d', rowSums(m0.), ncol(m0.)),
    'Variants in Collection' = v |>
      vapply(FUN = paste, collapse = '\n', FUN.VALUE = NA_character_),
    check.names = FALSE
  )
  
  tmp <- x |>
    setNames(nm = x |> vapply(FUN = \(i) {
      sprintf(fmt = '%s\nsize=%d', i@label, nrow(i@m1))
    }, FUN.VALUE = '')) |>
    mapply(FUN = \(x, vs) {
      ifelse(test = names(v) %in% names(vs), 
             yes = x@label,
             no = '')
    }, x = _, vs = vs, SIMPLIFY = FALSE) |>
    as.data.frame.list(check.names = FALSE)
  
  data.frame(
    d, tmp,
    check.names = FALSE
  ) |> 
    flextable() |>
    autofit() |>
    hline(i = cumsum(nv)[-length(nv)]) |>
    color(j = length(d) + seq_len(nx), color = pal_hue()(n = nx), part = 'all') |>
    highlight(i = (nvr > 1L), j = length(d), color = 'lightyellow') |>
    align(align = 'right', part = 'all') |>
    add_header_row(
      values = c('Variant-Collection', 'Individual', 'Variants in Collection', 'Panel'), 
      colwidths = c(1L, 2L, 1L, length(tmp)), 
      top = TRUE) |> 
    merge_v(part = 'header') |>
    align(align = 'center', part = 'all') |>
    #bold(i = 1L, part = 'header') |> 
    bold(part = 'header') |> 
    color(i = 1L, color = 'black', part = 'header')
  
}




