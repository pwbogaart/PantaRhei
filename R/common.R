
#' Print formatted
#'
#' @param fmt (character) Format
#' @param ... Additional parameters to be sent to sprintf()
#'
#' @keywords internal
#' @noRd
printf <- function(fmt,...) {
  msg <- sprintf(fmt,...)
  #cat(msg)
  msg <- sub("\n","",msg)
  message(msg)
}

bbox <- function(g, dev_scale) {
  gw <- grid::widthDetails(g)
  gh <- grid::heightDetails(g)

  hjust <- ifelse(is.null(g$hjust), g$just[1], g$hjust)
  vjust <- ifelse(is.null(g$vjust), g$just[2], g$vjust)

  #message("hjust = ", hjust)
  #message("vjust = ", vjust)

  x0 <- grid::convertX(g$x, "inches") * dev_scale
  y0 <- grid::convertY(g$y, "inches") * dev_scale


  if      (hjust == "left")  xx <- c(x0, x0+gw)
  else if (hjust == "right") xx <- c(x0-gw, x0   )
  else                       xx <- c(x0-gw/2, x0+gw/2)

  if      (vjust == "bottom") yy <- c(y0, y0+gh)
  else if (vjust == "top")    yy <- c(y0-gh, y0)
  else                        yy <- c(y0-gh/2, y0+gh/2)

  xx <- xx*72
  yy <- yy*72

  bb <- list(
    ll=list(x=xx[1],y=yy[1]),
    ur=list(x=xx[2],y=yy[2]),
    xx=c(xx[c(1,2,2,1)]),
    yy=c(yy[c(1,1,2,2)]),
    xmin=xx[1],
    xmax=xx[2],
    ymin=yy[1],
    ymax=yy[2],
    height=gh,
    width=gw,
    units="bigpts"
  )
  bb
}

bb_plot <- function(bb, ...) {
  gp <- grid::gpar(...)
  gp$fill <- NA
  grid::grid.polygon(bb$xx, bb$yy, default.units=bb$units, gp=gp)
}

bb_intersect <- function(bb1, bb2) {
  if (bb1$xmax <= bb2$xmin) return(FALSE)
  if (bb1$xmin >= bb2$xmax) return(FALSE)
  if (bb1$ymax <= bb2$ymin) return(FALSE)
  if (bb1$ymin >= bb2$ymax) return(FALSE)
  return(TRUE)
}

overlapping <- function(g1, g2, dev_scale) {
  bb1 <- bbox(g1, dev_scale)
  bb2 <- bbox(g2, dev_scale)
  return(bb_intersect(bb1,bb2))
}

#' Format a string
#'
#' This function adds formatting information to a character string by storing
#' this information as the character string's attributes.
#' Run the example to see how it works.
#'
#' All formatting specifiers work as if `gpar()` would be called.
#' (It is, behind the screen.)
#'
#' @param s character string to be formatted
#' @param ... formatting specifiers to be forwarded to gpar()
#'
#' @return formatted string
#' @export
#'
#' @examples
#' s <- strformat("Hello, World", fontsize=18, col="red")
#' str(s)  # show object structure
strformat <- function(s, ...) {
  attr(s,"gp") <- gpar(...)
  return(s)
}
