#' Plot a bundle
#'
#' @param A Starting node
#' @param B Ending node
#' @param w flow width
#' @param cols flow colors
#' @param r minkum radius
#' @param alpha steepness of ramp
#'
#' @return NULL
#' @keywords internal
#' @noRd
plot_bundle <- function(A, B, w, cols, r, alpha)
{
  stopifnot(class(A)=="node")
  stopifnot(class(B)=="node")

  nflow <- length(w)
  stopifnot(length(cols)==nflow)

  shape <- which.shape(A, B)
  bends <- which.bends(A, B)

  # bundle analysis
  wtotal <- sum(w)
  offset  <- -sum(w)/2 + w[1]/2

  if (shape=='I') {
    stopifnot(length(bends)==0)
    for (i in 1:nflow) {
      .connect(A, B, "", w[i], r, col=cols[i], offset=offset)
      if (i<nflow) offset <- offset + w[i]/2 + w[i+1]/2
    }
  } else if (shape=='L') {
    stopifnot(length(bends)==1)
    if (bends=="right") r = r + wtotal
    for (i in 1:nflow) {
      .connect(A, B, bends, w[i], r, col=cols[i], offset=offset)
      if (i<nflow) offset <- offset + w[i]/2 + w[i+1]/2
      if (bends=="left") r <- r + w[i]
      else               r <- r - w[i]
    }
  } else if (shape=='S') {
    stopifnot(length(bends)==2)
    a <- 0.5                                       # Create node halfway A and B
    mx <- a*A$x + (1-a)*B$x
    my <- a*A$y + (1-a)*B$y
    M <- node(mx,my,A$dir)
    if (bends[1]=="left") M <- turn(M,  alpha)          # Give it the right turn
    else                  M <- turn(M, -alpha)
    r0  <- r

    Moff <- .offsets(w)                                             # and offset

    # first segment
    if (bends[1]=="right") r = r0 + wtotal
    for (i in 1:nflow) {
      .connect(A, M, bends[1], w[i], r, col=cols[i], offset=offset)
      if (i<nflow) offset <- offset + w[i]/2 + w[i+1]/2
      if (bends[1]=="left") r <- r + w[i]
      else                  r <- r - w[i]
    }

    # reset
    offset  <- -sum(w)/2 + w[1]/2

    # second segment
    if (bends[2]=="right") r = r0 + wtotal
    for (i in 1:nflow) {
      .connect(M, B, bends[2], w[i], r, col=cols[i], offset=offset)
      if (i<nflow) offset <- offset + w[i]/2 + w[i+1]/2
      if (bends[2]=="left") r <- r + w[i]
      else                  r <- r - w[i]
    }
  } else stop("unknown shape")
}
