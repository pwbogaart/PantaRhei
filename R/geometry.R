
#### Point ####

# Points are simple objects with (x,y) coordinates and optionally an ID

point <- function(x, y, id="") {
  # Create a point object
  P <- list(x=x, y=y, id=id)
  class(P) <- "point"
  P
}

print.point <- function(x, ...) {
  if (x$id=="")
    s <- sprintf("(%.2f,%.2f)\n", x$x, x$y)
  else
    s <- sprintf("(%.2f,%.2f id=%s)\n", x$x, x$y, x$dir, x$id)
  cat(s)
}

intersect <- function(A1,B1, A2,B2) {
  # Test if lines A1-B1 -- A2-B2 intersect
  S1x <- B1$x - A1$x;     S1y <- B1$y - A1$y;
  S2x <- B2$x - A2$x;     S2y <- B2$y - A2$y;

  s = (-S1y * (A1$x - A2$x) + S1x * (A1$y - A2$y)) / (-S2x * S1y + S1x * S2y);
  t = ( S2x * (A1$y - A2$y) - S2y * (A1$x - A2$x)) / (-S2x * S1y + S1x * S2y);

  collision <- (s >= 0 && s <= 1 && t >= 0 && t <= 1)
}

#### PolyPoint #####

polypoint <- function(xx, yy, id="") {
  # Create polypoint object (line; polygon, etc)
  stopifnot(length(xx)==length(yy))
  PP <- list(xx=xx, yy=yy, n=length(xx), id=id)
  class(PP) <- "polypoint"
  PP
}

#' Rotate polypoint
#'
#' @param PP coordinates of polypoint
#' @param p coordinate of pivot
#' @param theta rotation  angle
#'
#' @keywords internal
#' @noRd
rotate <- function(PP, pivot, theta) {
  # shift to center on pivot
  xx <- PP$xx - pivot$x
  yy <- PP$yy - pivot$y
  # xy <- cbind(xx,yy) # convert to matrix
  # # rotate
  # R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)),2,2)
  # str(R)
  # str(xy)
  # xy <- R %*% xy
  # # shift back
  # PP$xx <- xy[,1] + pivot$x
  # PP$yy <- xy[,2] + pivot$y

  # rotate
  xx2 <- xx*cos(theta) - yy*sin(theta)
  yy2 <- xx*sin(theta) + yy*cos(theta)
  #shift back
  PP$xx <- xx2 + pivot$x
  PP$yy <- yy2 + pivot$y
  return(PP)
}

#### Node ####

# Nodes are like Points, bith with a direction, measured as an angle

node <- function(x, y, dir=0.0, id="") {
  # Create a Node object
  if (!is.null(dir) && class(dir)=="character") {
    if (dir=="right") dir <- 0.0
    if (dir=="up")    dir <- pi/2
    if (dir=="left")  dir <- pi
    if (dir=="down")  dir <- 1.5*pi
  }
  N <- list(x=x, y=y, dir=dir, id=id)
  class(N) <- "node"
  N
}

print.node <- function(x, ...) {
  if (x$id=="")
    s <- sprintf("(%.2f,%.2f dir=%.2f)\n", x$x, x$y, x$dir)
  else
    s <- sprintf("(%.2f,%.2f dir=%.2f id=%s)\n", x$x, x$y, x$dir, x$id)
  cat(s)
}

shift <- function(P, amount, rel.dir=0.0) {

  if (rel.dir=="left")  rel.dir <- +0.5*pi
  if (rel.dir=="right") rel.dir <- -0.5*pi
  if (rel.dir=="back")  rel.dir <- +pi
  theta <- P$dir+rel.dir
  P$x <- P$x + amount * cos(theta)
  P$y <- P$y + amount * sin(theta)
  P
}

turn <- function(P, rel.dir) {
  P$dir <- P$dir + rel.dir
  P
}

node_diff <- function(P, Q) {
  dx <- Q$x - P$x
  dy <- Q$y - P$y
  c(dx,dy)
}

# shift <- function(P, amount, rel.dir=0.0)
# {
#   stopifnot(ncol(P) %in% c(2,3))
#   if (rel.dir=="left")  rel.dir <- +0.5*pi
#   if (rel.dir=="right") rel.dir <- -0.5*pi
#   x1 <- P[,1]
#   y1 <- P[,2]
#   alpha <- if (ncol(P)==3) P[,3] else 0.0 # node angle
#   theta <- alpha + rel.dir
#   P[,1] <- P[,1] + amount * cos(theta) # shift x coordinate
#   P[,2] <- P[,2] + amount * sin(theta) # shift y coordinate
#   P
# }

path <- function(P, Q, ...)
{
  RSetc <- list(...)
  nnode <- 2 + length(RSetc)
  xy <- matrix(0, nnode, 2)
  xy[1,1] <- P$x
  xy[1,2] <- P$y
  xy[2,1] <- Q$x
  xy[2,2] <- Q$y
  for (i in 3:nnode) {
    xy[i,1] <- RSetc[[i-2]]$x
    xy[i,2] <- RSetc[[i-2]]$y
  }
  xy
}

#### Comp. geometry ####
# src: wikipedia line-line intersection
####

intersection <- function(A, B, debug=FALSE) {
  # if (length(a)!=3) stop("requires a to be a tuple (x,y,dir)")
  # if (length(b)!=3) stop("requires b to be a tuple (x,y,dir)")
  stopifnot(is.finite(A$dir))
  stopifnot(is.finite(B$dir))

  x1 <- A$x
  y1 <- A$y
  AA <- shift(A, 1.0)
  x2 <- AA$x
  y2 <- AA$y
  if (debug) cat(sprintf("x1, x2 = (%.1f,%.1f), (%.1f,%.1f)\n", x1,y1, x2,y2))

  x3 <- B$x
  y3 <- B$y
  BB <- shift(B, 1.0)
  x4 <- BB$x
  y4 <- BB$y
  if (debug) cat(sprintf("x3, x4 = (%.1f,%.1f), (%.1f,%.1f)\n", x3,y3, x4,y4))

  nomx <- (x1*y2-y1*x2)*(x3-x4) - (x1-x2)*(x3*y4-y3*x4)
  nomy <- (x1*y2-y1*x2)*(y3-y4) - (y1-y2)*(x3*y4-y3*x4)
  denom <- (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)
  # print(c(nomx, nomy, denom))
  if (abs(denom)<1e-7) {
    AB <- NULL
  } else {
    px <- nomx / denom
    py <- nomy / denom
    # xx = c(x1, px, x3)
    # yy = c(y1, py, y3)
    # # grid.lines(xx,yy, default.units="native")
    AB <- node(px, py, dir=NULL)
  }
  AB
}

leftright <- function(A, B, P, human=FALSE) {
  # determines the relative position iof point P with respect to the line A-B
  x1 <- A$x
  y1 <- A$y
  x2 <- B$x
  y2 <- B$y

  x <- P$x
  y <- P$y

  d <- (x-x1)*(y2-y1)-(y-y1)*(x2-x1)
  if (human) c("left","on","right")[sign(d)+2] else sign(d)
}


Lpath <- function(A, B, r, debug=FALSE) {
  if (debug) cat(sprintf("Lpath(r=%.1f\n", r))
  N <- 20

  AB <- intersection(A,B)
  # grid.points(ab[1], ab[2], pch=16, size=unit(1,"native"), gp=gpar(col="red"))
  if (leftright(A, AB, B) < 0) { # bend to left
    alpha <- B$dir - A$dir
    # find C, the center of radius
    AA = shift(A, r, "left")
    BB = shift(B, r, "left")
    C = intersection(AA,BB)
    th1 <- A$dir - 0.5*pi
    th2 <- B$dir - 0.5*pi
    while (th2<th1) th2 <- th2+2*pi
  } else { # bend to right
    AA = shift(A, r, "right")
    BB = shift(B, r, "right")
    C = intersection(AA,BB)
    th1 <- A$dir + 0.5*pi
    th2 <- B$dir + 0.5*pi
    while (th2>th1) th2 <- th2-2*pi
  }

  theta <- seq(th1, th2, len=N)
  xx <- C$x + r * cos(theta)
  yy <- C$y + r * sin(theta)
  xxx <- c(A$x, xx, B$x)
  yyy <- c(A$y, yy, B$y)
  # grid.lines(xxx,yyy, default.units = "native")
  list(x=xxx, y=yyy)
}

.connect <- function(A, B, bend="", w, r=0.0, offset=0.0, col="black") {
  # cat(sprintf(".connect w=%.1f offset=%.1f\n", w, offset))

  dx <- node_diff(A,B)[1]
  dy <- node_diff(A,B)[2]

  # Create corners for thick path
  A1 <- shift(A, w/2, "left")
  A2 <- shift(A, w/2, "right")
  B1 <- shift(B, w/2, "left")
  B2 <- shift(B, w/2, "right")

  # offset them if within a bundle
  if (length(offset)==1) offset <- rep(offset,2)
  if (offset[1]!=0) {
    A1 <- shift(A1, offset[1], "right")
    A2 <- shift(A2, offset[1], "right")
  }
  if (offset[2]!=0) {
    B1 <- shift(B1, offset[2], "right")
    B2 <- shift(B2, offset[2], "right")
  }

  if (bend=="") { # i.e., straight
    xy <- path(A1, B1, B2, A2)
    xx <- xy[,1]
    yy <- xy[,2]
  } else {
    # rmax <- min(abs(dx), abs(dy)) - (w/2)
    # if (r > rmax) {
    #   print(c(dx, dy, w, r, rmax))
    #   msg <- sprintf("Decreasing r from %.2f to max value of %.2f", r, rmax)
    #   warning(msg)
    #   r <- rmax
    # }
    r1 = r
    r2 = ifelse(bend=="left", r+w, r-w)
    L1 <- Lpath(A1, B1, r1)
    L2 <- Lpath(A2, B2, r2)
    xx <- c(L1$x, rev(L2$x))
    yy <- c(L1$y, rev(L2$y))
  }

  grid.polygon(xx, yy, default.units="native", gp=gpar(col=NA, fill=col))
}


which.shape <- function(A, B)
{
  norm_dir <- function(theta) { # normalize direction
    two.pi <- 2*pi
    while (theta < 0)      theta <- theta + two.pi
    while (theta > two.pi) theta <- theta - two.pi
    theta
  }

  if (A$dir == B$dir) {
    # straight or parallel
    dx <- node_diff(A,B)[1]
    dy <- node_diff(A,B)[2]
    my.dir = atan2(dy,dx)
    if (norm_dir(my.dir) == norm_dir(A$dir)) {
      shape <- 'I'
    } else {
      shape <- 'S'
    }
  } else {
    shape <- 'L'
  }
  shape
}


which.bends <- function(A, B)
{
  shape <- which.shape(A, B)
  if (shape=='I') {
    bends <- character(0) # no bends
  } else if (shape=='L') {
    AB <- intersection(A,B)
    if (leftright(A, AB, B) < 0) { # bend to left
      bends <- "left"
    } else { # bend to right
      bends <- "right"
    }
  } else if (shape=='S') {
    AA <- shift(A, 1.0)
    if (leftright(A,AA, B) < 0) {
      bends <- c("left","right")
    } else {
      bends <- c("right","left")
    }
  }
  bends
}

.offsets <- function(w) {
  # Compute for a flow bundle the offset of each flow with respect to the
  # neutral position (0) for each flow involved
  # w : flow widths
  n <- length(w)
  if (n==1L) {
    offsets <- 0                                                 # By definition
  } else {
    wleft  <- c(0, cumsum(w[1:(n-1)]))
    wright <- cumsum(w)
    wcenter <- (wleft+wright)/2
    offsets <- wcenter - sum(w)/2
  }
  offsets
}


