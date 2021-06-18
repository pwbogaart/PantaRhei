### @importFrom grid grid.newpage, grid.rect, grid.text, grid.grill, grid.points, unit

#' PantaRhei: Publication-quality Sankey diagrams
#'
#' Please read the \href{../doc/panta-rhei.html}{user manual} for more information.
#'
#' @docType package
#' @name PantaRhei
#'
#' @import grid
NULL
#> NULL

.onAttach <- function(libname, pkgname) {
  msg <- "Panta Rhei; Everything flows (Heraclitus?)\n\ntype vignette('panta-rhei') to view the user manual."
  packageStartupMessage(msg)
}
