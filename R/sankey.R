#' Plots a Sankey diagram
#'
#' @param nodes data.frame, containing the nodes definition
#' @param flows data.frame, containing the nodes definition
#' @param palette data.frame, containing the nodes definition
#' @param node_style list: containing node style specifiers:
#' \describe{
#'   \item{type}{Character: Node type; possible values are `"box"`, `"bar"` and `"arrow"`.}
#'   \item{length}{numeric: node length, as fraction plot size (default: 0.1).}
#'   \item{gp}{an object of class `gpar`, typically the output from a call to
#'             the function `gpar()`.
#'             This is basically a list of graphical parameter settings,
#'             describing the colors etc of the node.}
#'   \item{label_pos}{character: label position. values: auto, above, below, left, right, none.}
#'   \item{label_anchor}{character: label position (overrides `label_pos`). Values are NW, N, NE, W, E, SW, S, SE.}
#'   \item{label_align}{character: label alignment with respect to `label_anchor`. Values are NW, N, etc.}
#'   \item{label_gp}{an object of class `gpar`, describing the font and color of the label text.}
#'   \item{mag_pos}{similar to `label_pos`, but controls location of the node magnitude.
#'                  Value `inside` plots the node magnitude inside the node.
#'                  Value `label` plots the node magnitude beneath the node label.}
#'   \item{mag_anchor}{similar to `label_anchor`.}
#'   \item{mag_align}{similar to `label_align`.}
#'   \item{mag_gp}{similar to `label_gp`.}
#'   \item{mag_fmt}{character: format string for the node magnitude. default: `"%.1f"`.
#'                  see `?sprintf` for more information.}
#' }
#' @param title character: plot title. use `strformat()` to specify formatting.
#' @param legend logical or gpar: Specifies the plotting of a legend.
#'   valid values are NULL (default; no legend),
#'   TRUE (plot a legend using standard text size and color),
#'   or the output of a call to gpar(), to control legend text size and color.
#' @param page_margin numeric: Page margin. Either a scalar, an (x,y) vector or an (left,bot,rt,top) vector
#' @param max_width numeric: Maximum width of the flow bundles, in fraction of the plot size
#' @param rmin numeric: Minimum radius for flow path bends (as fraction of the diagram's units)
#' @param copyright character: optional copyright statement?
#' @param grill logical: Plot a coordinate grill?
#' @param verbose logical: print some diagnostic messages?
#'
#' @return The modified nodes data.frame
#' @export
#'
#' @examples
#' nodes <- data.frame(ID=c("A","B"), x=1:2, y=0)
#' flows <- data.frame(from="A", to="B", quantity=10, substance="stuff")
#' sankey(nodes, flows)
#'
#' colors <- data.frame(substance="stuff", color="blue")
#' sankey(nodes, flows, colors)
#'
#' sankey(nodes, flows, legend=TRUE)                       # Plots default legend
#' sankey(nodes, flows, legend=grid::gpar(fontsize=18, ncols=2)) # Large fonts; 2 columns
sankey <- function(nodes, flows, palette,
                   node_style=list(),
                   title=NULL,
                   legend=FALSE,
                   page_margin=0.1,
                   max_width=0.2,
                   rmin=0.2,
                   copyright=NULL,
                   grill=NULL,
                   verbose=FALSE)
{

  #----------------------------------------------------------------- Checks ----

  # First check all arguments: nodes
  if (missing(nodes)) stop("No nodes specified")
  check_nodes(nodes)
  nodes <- parse_nodes(nodes, verbose=verbose)

  if (missing(flows)) stop("No flows specified")
  check_flows(flows)
  flows <- parse_flows(flows, verbose=verbose)

  if (missing(palette)) {
    # Generate automatic palette
    subs <- unique(flows$substance)
    n <- length(subs)
    palette <- data.frame(
      substance  = subs,
      color      = grDevices::rainbow(n)
    )
  }
  palette <- parse_palette(palette, verbose=verbose)

  # Node decoration (label & magnitude)

  if (class(node_style) != "list")
    stop("node_style is not a list")

  default_node_style <- list(               # Set up default style
    type   = "box",
    length = 0.3,
    gp     = gpar(fill=grDevices::gray(0.9), col=NA),
    label_pos    = "below",
    label_anchor = "auto",
    label_align  = "auto",
    label_gp     = gpar(),
    mag_pos      = "inside",
    mag_anchor   = "auto",
    mag_align    = "auto",
    mag_gp       = gpar(),
    mag_fmt   = "%.1f"
  )
  items <- names(default_node_style)        # replace missing
  for (item in items) {
    if (! item %in% names(node_style)) {
      node_style[[item]] <- default_node_style[[item]]
    }
  }

  # Unpack
  global_label_pos    <- node_style$label_pos
  global_label_anchor <- node_style$label_anchor
  global_label_align  <- node_style$label_align
  global_mag_pos      <- node_style$mag_pos
  global_mag_anchor   <- node_style$mag_anchor
  global_mag_align    <- node_style$mag_align

  node_type   <- node_style$type
  node_length <- node_style$length
  node_gp     <- node_style$gp

  # Check legend

  if (class(legend)=="NULL") {
    plot_legend <- FALSE
  } else if (class(legend)=="logical") {
    plot_legend <- legend
    legend_gp <- gpar()
  } else if (class(legend)=="gpar") {
    plot_legend <- TRUE
    legend_gp <- legend
  } else {
    stop(sprintf("Invalid datatype for 'legend': '%s'", class(legend)))
  }

  #----------------------------------------------------- internal functions ----

  dir2num <- function(dir, inout="") {
    # determine angle (might dynamically depend on flow direction!)
    if (class(dir)=="numeric") {
      num <- dir
    } else {
      if (dir=="stock") { # special
        if      (inout=="out")  dir <- "up"
        else if (inout=="in")   dir <- "down"
        else if (inout=="")     dir <- "down" #  used for offset computations
        else stop("invalid inout")
      }
      if (dir=="bottom") {
        if      (inout=="out") dir <- "down"
        else if (inout=="in")  dir <- "up"
        else if (inout=="")    dir <- "up" # idem
        else stop("invalid inout")
      }
      std_dirs <- c(right=0.0, up=0.5*pi, left=pi, down=1.5*pi)
      stopifnot(dir %in% names(std_dirs))
      num <- unname(std_dirs[dir])
    }
    num
  }

  # remove unused nodes
  node_nodes <- unique(nodes$ID)
  flow_nodes <- unique(c(flows$from, flows$to))
  used <- node_nodes %in% flow_nodes
  if (!all(used)) {
    printf("! ignoring unused nodes: %s\n", paste(node_nodes[!used], collapse=", "))
    used_nodes <- node_nodes[used]
    keep <- nodes$ID %in% used_nodes
    nodes <- nodes[keep,]
  }
  nnode <- nrow(nodes)
  nflow <- nrow(flows)


  # # Initial treatment of alignment specifiers
  # if (class(nodes$y)=="character") {
  #   use_y_align <- TRUE
  #   # y_align <- nodes$y
  #   # nodes$y <- 0
  #   num <- grepl("^[[:digit:]]+([,.][[:digit:]]+)?$", nodes$y)
  #   # # handle initial numerical y-specifiers
  #   # nodes$y[num] <- as.numeric(y_align[num])
  #   # y_align[num] <- ""
  #   # build a list of initially known coordinates
  #   known <- list()
  #   for (i in seq_len(nnode)) {
  #     if (num[i]) known[[nodes$ID[i]]] <- as.numeric(nodes$y[i])
  #   }
  #   # iteratively solve all references
  #   # and build a list of y-alignments
  #   y_align <- list()
  #   todo <- !num
  #   for (step in 1:100) {
  #     if (!any(todo)) break
  #     printf("solve step %d\n", step)
  #     for (i in which(todo)) {
  #       dep <- nodes$ID[i] # current (dependent) node
  #       ref <- nodes$y[i]    # name of the reference node
  #       if (ref %in% names(known)) {
  #         yval <- known[[ref]]
  #         known[[dep]] <- yval
  #         todo[i] <- FALSE
  #         y_align[[dep]] <- ref
  #       }
  #     }
  #   }
  #   # Set the computed values
  #   nodes$y = 0.0
  #   for (node in names(known)) {
  #     idx <- match(node, nodes$ID)
  #     val <- known[[node]]
  #     nodes$y[idx] <- val
  #   }
  # } else use_y_align <- FALSE

  ### Aligned and computed nodes ###

  position_nodes <- function(nname, npos, debug=FALSE) {
    # Convert all node positions from a string format, which may represent
    # numbers, node references or algebraic expressions, to actual values.
    # input:
    #   nname: node names (string)
    #   npos:  node position (numeric or string)

    # Step 0: if npos (or y) is already numeric, we're done
    if (class(npos)!="character") {
      out <- list(pos=npos, align=list())
      return(out)
    }

    # OK, work to do. Step 1: compute the type
    abs_pat   <- "^-?[[:digit:]]+([,.][[:digit:]]+)?$" # absolute: e.g. 3.14
    align_pat <- "^[[:alnum:]_]+$"                     # aligned:  e.g. "import"
    comp_pat  <- "^[[:alnum:]_]+[+-][[:digit:]]"       # computed: e.g. "import+3"
    n <- length(npos)
    ntype <- character(n)
    for (i in seq_len(n)) {
      if      (grepl(abs_pat,   npos[i])) ntype[i] <- "absolute"
      else if (grepl(align_pat, npos[i])) ntype[i] <- "aligned"
      else if (grepl(comp_pat,  npos[i])) ntype[i] <- "computed"
      else {
        msg <- sprintf("Invalid node coordinate. ID='%s' pos='%s'", nname[i], npos[i])
        stop(msg, call.=FALSE)
      }
    }

    # Step 2: build a list of known coordinates
    known <- list()
    for (i in seq_len(n)) {
      if (ntype[i]=="absolute") known[[nname[i]]] <- as.numeric(npos[i])
    }

    # # Step 3: iteratively solve all alignments (and build a list for later use)
    # align <- list()
    # todo <- type=="aligned"
    # for (step in 1:100) {
    #   if (!any(todo)) break
    #   solved <- 0L
    #   printf("solve step %d\n", step)
    #   for (i in which(todo)) {
    #     dep <- name[i] # current (dependent) node
    #     ref <- x[i]    # name of the reference node
    #     if (ref %in% names(known)) {
    #       val <- known[[ref]]
    #       known[[dep]] <- val
    #       todo[i] <- FALSE
    #       align[[dep]] <- ref
    #       solved <- solved + 1L
    #     }
    #   }
    #   if (solved==0L) {
    #     print(x[todo])
    #     str(known)
    #     stop("problem solving alignment")
    #   }
    # }
    #
    # # Step 4: iteratively solve all computed coordinates
    # todo <- type=="computed"
    # for (step in 1:100) {
    #   if (!any(todo)) break
    #   solved <- 0L
    #   printf("solve step %d\n", step)
    #   for (i in which(todo)) {
    #     dep <- name[i] # current (dependent) node
    #     expr <- parse(text=x[i])
    #     refs <- all.vars(expr) # names of the reference node
    #     if (all(refs %in% names(known))) {
    #       val <- eval(expr, known) # execute computation
    #       known[[dep]] <- val
    #       todo[i] <- FALSE
    #       solved <- solved + 1L
    #     }
    #   }
    #   if (solved==0L) stop("problem solving computed")
    # }


    # Step 3+4: iteratively solve all aligned/computed coordinates
    # and build a list of alignments
    align <- list()
    todo <- ntype=="aligned" | ntype=="computed"
    iter <- 0L
    while (any(todo)) {                             # Repeat while nescessary...
      iter <- iter + 1L                   # ... but keep track of infinite loops
      if (iter>1000) stop("Problem solving node positions")
      if (debug) printf("Solving step %d\n", iter)
      for (i in which(todo)) {                     # iterate over unsolved nodes
        dep <- nname[i]                                # current (dependent) node

        if (ntype[i]=="aligned") {                       # --- case: aligned ----
          ref <- npos[i]                               # name of the reference node
          if (ref %in% names(known)) {
            known[[dep]] <- known[[ref]]                        # copy known pos
            align[[dep]] <- ref                          # register as alignment
            todo[i] <- FALSE
            if (debug) printf("  solved: %s\n", npos[i])
          }
        } else if (ntype[i]=="computed") {             # case: ---- computed ----
          expr <- parse(text=npos[i])              # convert string to R expression
          refs <- all.vars(expr)                 # names of the referenced nodes
          if (all(refs %in% names(known))) {     # continue if they're all known
            known[[dep]] <- eval(expr, known)                # compute and store
            todo[i] <- FALSE
            if (debug) printf("  solved: %s\n", npos[i])
          }
        } else stop("Can't happen: unknown node position type:", ntype[i])
      }
    }

    # Step 5: check if all nodes are known
    if (!all(nname %in% names(known))) stop("Not all nodes known")

    # Step 6: organize output
    pos <- numeric(n)
    for (i in seq_len(n)) {
      pos[i] <- known[[nname[i]]]
    }
    out <- list(pos=pos, align=align)
  }

  # Define x-coordinates of nodes
  XP <- position_nodes(nodes$ID, nodes$x)
  nodes$x <- XP$pos
  x_align <- XP$align

  # Idem y-coordinates
  YP <- position_nodes(nodes$ID, nodes$y)
  nodes$y <- YP$pos
  y_align <- YP$align

  ### end ###

  # Check for overlapping nodes
  nn <- nrow(nodes)
  for (j in 2:nn) {
    for (i in 1: (j-1)) {
      dx <- nodes$x[i] - nodes$x[j]
      dy <- nodes$y[i] - nodes$y[j]
      dist <- sqrt(dx*dx +dy*dy)
      if (dist < 1e-5) {
        msg <- sprintf("Overlapping nodes: '%s' and '%s'", nodes$ID[i], nodes$ID[j])
        stop(msg, call.=FALSE)
      }
    }
  }

  # Compute total input per node
  nodes$mag_in <- 0.0
  for (i in seq_len(nnode)) {
    idx <- flows$to == nodes$ID[i]
    nodes$mag_in[i] <- sum(flows$quantity[idx])
  }
  # Idem for node output
  nodes$mag_out <- 0.0
  for (i in seq_len(nnode)) {
    idx <- flows$from == nodes$ID[i]
    nodes$mag_out[i] <- sum(flows$quantity[idx])
  }

  # Check macro-scale balance (total in must be total out)
  total_mag_in  <- sum(nodes$mag_in)
  total_mag_out <- sum(nodes$mag_out)
  mag_balance <- total_mag_in - total_mag_out
  msg <- sprintf("Total flux balance: input=%.1f output=%.1f balance=%.1f", total_mag_in, total_mag_out, mag_balance)
  if (verbose) message(msg)


  #------------------------------------------------------------ Page set-up ----

  # Determine aspect ratio, and size, using "snpc" as units
  grid.newpage()
  #ds <- grDevices::dev.size()
  din <- graphics::par("din") * 72 # device size in bigpts
  #dev_scale <- din[1] / ds[1]
  dev_scale <- 1.0

  fig_w <- din[1]
  fig_h <- din[2]
  ref_size <- min(fig_w, fig_h) # reference size
  vp <- viewport(0,0, just=c("left","bottom"), xscale=c(0,fig_w), yscale=c(0,fig_h))
  pushViewport(vp)

  max_width   <- max_width   * ref_size
  page_margin <- page_margin * ref_size

  # Draw a frame
  fm <- 2 # frame margin
  grid.rect(x=fm, y=fm, width=fig_w-2*fm, height=fig_h-2*fm,
            default.units = "bigpts", just=c(0,0))


  #-------------------------------------------------------------- Copyright ----

  # Print optional copyright message
  if (!is.null(copyright) && !is.na(copyright) && copyright!="") {
    cm <- 6 # copyright margin
    grid.text(copyright, x=fig_w-cm, y=cm, just=c(1,0),
              default.units="bigpts", gp=gpar(fontsize=10, col="grey"))
  }

  # Compute plot width based on maximum node size
  nodes$quantity <- pmax(nodes$mag_in, nodes$mag_out)
  scale <- max_width / max(nodes$quantity)
  # Compute node+flow widths too
  nodes$width <- nodes$quantity * scale
  flows$width <- flows$quantity * scale

  #------------------------------------------------------ Coordinate trans. ----

  # Convert node coordinates user->snpc.
  # scale_nodes <- function(x, fig_w, w=0, margin=0.1) {
  #   if (length(margin)==1) margin <- c(margin,margin)
  #   xhi <- x + w/2
  #   xlo <- x - w/2
  #   xrange <- range(range(xlo), range(xhi))
  #   xmargin = 0.1 * diff(xrange)
  #   #xlim = xrange + c(-1,+1) * xmargin
  #   xlim = c(xrange[1] - xmargin[1], xrange[2]+margin[2])
  #   #x <- approx(xlim, c(0,fig_w), x)[[2]] # map xlim to 0 -- fig_w)
  #   # compute linear transformation pars for y=a*x+b
  #   a <- fig_w / diff(xlim)
  #   b <- fig_w - a * xlim[2]
  #   return(list(a=a,b=b))
  # }
  scale_nodes <- function(x, vp, w=0) {
    xhi <- x + w/2
    xlo <- x - w/2
    xrange <- range(range(xlo), range(xhi))
    # compute linear transformation pars for y=a*x+b
    a <- diff(vp) / diff(xrange)
    b <- vp[2] - a * xrange[2]
    return(list(a=a,b=b))
  }
  lintransform <- function(x, tr) {
    # apply scaling
    y <- tr$a * x + tr$b
  }

  # for the grid grill
  plot_grill <- isTRUE(grill)
  if (plot_grill) {
    minx <- min(nodes$x)
    maxx <- max(nodes$x)
    miny <- min(nodes$y)
    maxy <- max(nodes$y)
    grill_xx <- seq(floor(minx), ceiling(maxx))
    grill_yy <- seq(floor(miny), ceiling(maxy))
  }

  if (length(page_margin)==1) {
    margin <- list(
      left   = page_margin,
      bottom = page_margin,
      right  = page_margin,
      top    = page_margin
    )
  } else if (length(page_margin)==2) {
    margin <- list(
      left   = page_margin[1],
      bottom = page_margin[2],
      right  = page_margin[1],
      top    = page_margin[2]
    )
  } else if (length(page_margin)==4) {
    margin <- list(
      left   = page_margin[1],
      bottom = page_margin[2],
      right  = page_margin[3],
      top    = page_margin[4]
    )
  } else {
    msg <- sprintf("invalid page_margin (length=%d)", length(page_margin))
    stop(msg, call.=FALSE)
  }

  inner <- list(
    x = fig_w - margin$left - margin$right,
    y = fig_h - margin$top - margin$bottom
  )

  mid <- list(
    x = mean(c(margin$left,    margin$left   + inner$x)),
    y = mean(c(margin$bottom,  margin$bottom + inner$y))
  )

  # set up my_unit (1 grid cell)
  my_unit <- 1.0

  for (round in 1:2) {
    if (round==1) {
      # First round: only proper nodes
      xx <- nodes$x
      yy <- nodes$y
    } else {
      # second round: include node sizes (+width)
      xx <- c(nodes$x + nodes$width, nodes$x - nodes$width)
      yy <- c(nodes$y + nodes$width, nodes$y - nodes$width)
    }

    xscale <- inner$x / diff(range(xx))
    yscale <- inner$y / diff(range(yy))
    zscale <- min(xscale, yscale)

    # offsets: y=ax+b => b = y-ax
    xoff <- mid$x - zscale * mean(range(xx))
    yoff <- mid$y - zscale * mean(range(yy))

    # transformations
    xtr <- list(a=zscale, b=xoff)
    ytr <- list(a=zscale, b=yoff)

    nodes$x <- lintransform(nodes$x, xtr)
    nodes$y <- lintransform(nodes$y, ytr)

    # transform and plot grill
    if (plot_grill) {
      grill_xx <- lintransform(grill_xx, xtr)
      grill_yy <- lintransform(grill_yy, ytr)
    }

    my_unit <- my_unit * xtr$a
  }

  # Plot grill
  if (plot_grill) grid.grill(h=grill_yy, v=grill_xx, default.units="bigpts")

  # Node length in user coordinates
  node_length <- node_length * my_unit

  # Named nodes have a length (i.e. in the flow direction)
  # (Unnnamed 'hidden' nodes are to assist more tortuous paths)
  nodes$length <- 0
  visible <- !nodes$hidden
  if (node_type=="box") {
    nodes$length[visible] <- node_length
  } else if (node_type %in% c("bar", "arrow")) {
    nodes$length[visible] <- node_length
  } else {
    stop(paste("Unknown node type:", node_type))
  }

  # ------------------------------------------------------------- Set up bundles

  # Create bundles of parallel flows (same A->B)
  flows$bundle <- nbundle <- 0L
  for (from in nodes$ID) for (to in nodes$ID) {            # Check all pairs
    idx  <- flows$from==from & flows$to==to
    if (any(idx)) {                                     # See if there is a flow
      nbundle <- nbundle + 1L                            # Create a bundle if so
      flows$bundle[idx] <- nbundle
    }
  }
  if (verbose) message("Identified ", nbundle, " flow bundles.")
  flows <- flows[order(flows$bundle), ] # sort flows by bundle ID

  # Create a bundle database
  bundles <- data.frame(bundle=1:nbundle, from="", to="", width=0.0,
                        stringsAsFactors=FALSE)
  for (i in 1:nbundle) {
    idx <- which(flows$bundle==i)
    bundles$from[i]   <- flows$from[idx[1]]     # copy from first flow in bundle
    bundles$to[i]     <- flows$to[idx[1]]
    bundles$width[i]  <- sum(flows$width[idx])        # aggregate over all flows
  }

  # Untangle crossing bundles by identifying crossing pairs and switching them
  ready <- FALSE
  while (!ready) { # continue until no crossing pairs are found
    # determine offsets on the bundle level
    bundles$off1 <- bundles$off2 <- 0.0
    for (i in seq_len(nnode)) {
      node <- nodes$ID[i]
      ndir  <- nodes$dir[i]
      if (ndir=="stock") {                                          # special case
        idx1 <- bundles$from == node
        idx2 <- bundles$to   == node
        idx <- idx1 | idx2
        if (any(idx)) {
          #.offsets will return mixed to/from offsets. we need some special code to unravel them
          offx <- numeric(nbundle)
          offx[idx] <- .offsets(bundles$width[idx])
          bundles$off1[idx1] <- offx[idx1]
          bundles$off2[idx2] <- offx[idx2]
        }
      } else if (ndir=="bottom") {
        stop("Untangling bundles not yet implemented for 'bottom' nodes")
      } else {                                                  # Normal LR node
        # Compute offsets for all inflowing bundles
        idx <- bundles$to == node
        if (any(idx)) bundles$off2[idx] <- .offsets(bundles$width[idx])
        # idem for all outflowing bundles
        idx <- bundles$from == node
        if (any(idx)) bundles$off1[idx] <- .offsets(bundles$width[idx])
      }
    }

    # compute start/end locations of bundles
    bundles$By <- bundles$Bx <- bundles$Ay <- bundles$Ax <- 0.0
    for (i in 1:nbundle) {
      # create node object A, the bundle starting position
      from <- bundles$from[i] # get name of starting node
      j <- match(from, nodes$ID) # get index of node record
      A <- node(nodes$x[j], nodes$y[j], dir2num(nodes$dir[j])) # get location
      if (nodes$dir[j] != "stock") A <- shift(A, nodes$length[j]/2) # shift to allow for node box
      A <- shift(A, bundles$off1[i], "right") # shift from node loc to bundle start loc
      # repeat to find bundle end B
      to <- bundles$to[i]
      j <- match(to, nodes$ID)
      B <- node(nodes$x[j], nodes$y[j], dir2num(nodes$dir[j]))
      if (nodes$dir[j] != "stock") B <- shift(B, nodes$length[j]/2, "back")
      B <- shift(B, bundles$off2[i], "right")
      # put back into bundle definition
      bundles$Ax[i] <- A$x
      bundles$Ay[i] <- A$y
      bundles$Bx[i] <- B$x
      bundles$By[i] <- B$y
    }

    # check for crossing bundles
    nswitched <- 0L # bookkeep number of switches
    if (nbundle>1) {
      bundles$order <- 1:nbundle
      # check for crossing end
      for (i in 1:(nbundle-1)) {
        for (j in (i+1):nbundle) {
          #print(c(i,j, nbundle))
          if (bundles$to[i] == bundles$to[j]) { # bundles i and j have the same endnode
            A1 <- point(bundles$Ax[i], bundles$Ay[i])
            B1 <- point(bundles$Bx[i], bundles$By[i])
            A2 <- point(bundles$Ax[j], bundles$Ay[j])
            B2 <- point(bundles$Bx[j], bundles$By[j])
            if (intersect(A1,B1, A2,B2)) {
              from1 <- bundles$from[i]
              from2 <- bundles$from[j]
              to    <- bundles$to[i]
              # printf("! Switching (%s -> %s) / (%s -> %s)\n",
              #        from1,to, from2,to)
              tmp <- bundles$order[i]
              bundles$order[i] <- bundles$order[j]
              bundles$order[j] <- tmp
              nswitched <- nswitched + 1L
            }
          }
        }
      }
      # idem for crossing start
      for (i in 1:(nbundle-1)) {
        for (j in (i+1):nbundle) {
          if (bundles$from[i] == bundles$from[j]) { # bundles i and j have the same endnode
            A1 <- point(bundles$Ax[i], bundles$Ay[i])
            B1 <- point(bundles$Bx[i], bundles$By[i])
            A2 <- point(bundles$Ax[j], bundles$Ay[j])
            B2 <- point(bundles$Bx[j], bundles$By[j])
            if (intersect(A1,B1, A2,B2)) {
              from <- bundles$from[i]
              to1 <- bundles$to[i]
              to2 <- bundles$to[j]
              # printf("! Switching (%s -> %s) / (%s -> %s)\n",
              #        from,to1, from,to2)
              tmp <- bundles$order[i]
              bundles$order[i] <- bundles$order[j]
              bundles$order[j] <- tmp
              nswitched <- nswitched + 1L
            }
          }
        }
      }
      # idem for passing through "stock" node
      for (i in 1:(nbundle-1)) {
        for (j in (i+1):nbundle) {
          if (bundles$to[i] == bundles$from[j]) { # passing through...
            k <- match(bundles$to[i], nodes$ID)
            if (nodes$dir[k]=="stock") { # a "stock" node
              A1 <- point(bundles$Ax[i], bundles$Ay[i])
              B1 <- point(bundles$Bx[i], bundles$By[i])
              A2 <- point(bundles$Ax[j], bundles$Ay[j])
              B2 <- point(bundles$Bx[j], bundles$By[j])
              if (intersect(A1,B1, A2,B2)) {
                #printf("! Switching (%s -> %s) / (%s -> %s)\n",
                #       bundles$from[i], bundles$to[i], bundles$from[j], bundles$to[j])
                tmp <- bundles$order[i]
                bundles$order[i] <- bundles$order[j]
                bundles$order[j] <- tmp
                nswitched <- nswitched + 1L
              }
            }
          }
        }
      }
      # now sort the flows on untangled bundle order
      idx <- match(flows$bundle, bundles$bundle)
      flows$bundle <- bundles$order[idx]
      idx <- order(flows$bundle)
      flows <- flows[idx, ]

      # also udate the bundles
      idx <- order(bundles$order)
      bundles <- bundles[idx,]
      bundles$bundle <- 1:nbundle
    }

    ready <- nswitched==0
  }

  # ------------------------------------------------------------ Alignments ----

  # Compute alignments
  # y_align: list with names as dependents and content as references
  if (length(y_align)>0) {
    for (dep in names(y_align)) {
      ref <- y_align[[dep]]
      # compute dep -> ref alignment
      i <- which(bundles$from==dep & bundles$to==ref)
      if (length(i)>0) {
        dy <- bundles$By[i] - bundles$Ay[i]    # dy positive when B higher than A
        #printf("Align 1: dep=%s -> ref=%s dy=%.3f\n", dep, ref, dy)
        j <- which(nodes$ID==dep)                                  # Move node
        nodes$y[j] <- nodes$y[j] + dy
        j <- which(bundles$from==dep)                      # Move outgoing flows
        bundles$Ay[j] <- bundles$Ay[j] + dy
        j <- which(bundles$to==dep)                        # Move incoming flows
        bundles$By[j] <- bundles$By[j] + dy
      }
      # repeat for ref -> def alignments
      i <- which(bundles$to==dep & bundles$from==ref)
      if (length(i)>0) {
        dy <- bundles$Ay[i] - bundles$By[i]
        #printf("Align 2: ref=%s -> dep=%s dy=%.3f\n", ref, dep, dy)
        j <- which(nodes$ID==dep)
        nodes$y[j] <- nodes$y[j] + dy
        j <- which(bundles$from==dep)
        bundles$Ay[j] <- bundles$Ay[j] + dy
        j <- which(bundles$to==dep)
        bundles$By[j] <- bundles$By[j] + dy
      }
    }
  }

  # -------------------------------------------------------------- Plotting ----

  # set colors by joining
  idx <- match(flows$substance, palette$substance)
  for (i in 1:length(idx)) {
    if (!is.finite(idx[i])) {
      msg <- sprintf("Missing color for flow substance '%s'", flows$substance[i])
      stop(msg, call.=FALSE)
    }
  }
  flows$color <- palette$color[idx]

  # plot bundles
  for (phase in 1:1) {
  for (i in seq_len(nbundle)) {
    #if (phase==1 && i==focus) next
    #if (phase==2 && i!=focus) next

    bundle <- subset(flows, bundle==i) # isolate bundle
    stopifnot(nrow(bundle)>0)
    # indices of starting / ending node
    a <- match(bundle$from[1], nodes$ID)
    b <- match(bundle$to[1], nodes$ID)
    # printf("Plotting bundle #%d : %s --> %s\n", i, nodes$ID[a], nodes$ID[b])

    # location of starting node
    ax <- bundles$Ax[i]
    ay <- bundles$Ay[i]
    bx <- bundles$Bx[i]
    by <- bundles$By[i]

    # determine angle (might dynamically depend on flow direction!)
    d <- dir2num(nodes$dir[a], "out")
    A <- node(ax,ay, d, id=nodes$ID[a])
    # idem for end node
    d <- dir2num(nodes$dir[b], "in")
    B <- node(bx,by, d, id=nodes$ID[b])

    # plot
    color <- bundle$color
    #if (focus>0) if (i!=focus) color <- rep(gray(0.7), length(color))
    #if (Focus>0) if (i!=Focus) next
    radius <- rmin * my_unit
    auto_alpha <- function(A, B) {
      # Compute an automatic slope gradient for the ramp:
      # half the overall slope, but not steeper than 45%
      dx <- abs(A$x - B$x)
      dy <- abs(A$y - B$y)
      a <- atan2(dy,dx)
      min(a*2, pi/2)
    }
    alpha <- auto_alpha(A,B)

    # Adjust min radius to adjust for nbr bundles
    # shape <- which.shape(A, B)
    # bends <- which.bends(A, B)

    plot_bundle(A, B, bundle$width, color, radius, alpha)
  }
  }

  # compute node status for plotting purposes
  nodes$status <- "special"
  nodes$status[nodes$mag_in==0.0] <- "input"
  nodes$status[nodes$mag_out==0.0] <- "output"
  nodes$status[abs(nodes$mag_in - nodes$mag_out)<1e-1] <- "throughput" # Beware, very loose now! Maybe set at a few % of smallest flux?

  # # minimum graphical height for node boxes
  # min_gh <- 0.03

  horizontal <- c("left","right")
  vertical   <- c("up", "down")

  #------------------------------------------------------------- plot nodes ----


  for (i in seq_len(nnode)) {
    if (nodes$hidden[i]) next

        x <- nodes$x[i]
    y <- nodes$y[i]
    dir <- nodes$dir[i]

    # compute the node box
    if (dir %in% horizontal) {
      gw <- nodes$length[i]  # 'graphical' width and height
      gh <- nodes$width[i]
      # gh <- max(gh, min_gh)
    } else if (dir %in% vertical) {
      gw <- nodes$width[i]
      gh <- nodes$length[i]
      # gh <- max(gh, min_gh)
    } else if (dir=="stock") {
      # special case: width is sum of total incoming + outgoing widths
      idx1 <- bundles$to   == nodes$ID[i]
      idx2 <- bundles$from == nodes$ID[i]
      gw <- sum(bundles$width[idx1 | idx2])
      gh <- gw # square
      #y <- y - gh/2 # move apparent position to centre of box
    } else {
      gw <- gh <- 0
    }

    # draw_old_box <- function(x,y, gw,gh, node_gp) {
    #   if (node_gp$layered) {
    #     # outline:
    #     gp <- node_gp
    #     gp$fill <- NULL
    #     grid.rect(x=x, y=y, width=gw, height=gh, gp=gp, default.units="snpc")
    #     if # fill
    #     gp <- node_gp
    #     gp$col <- NA
    #     grid.rect(x=x, y=y, width=gw, height=gh, gp=gp, default.units="snpc")
    #   } else {
    #     # Non-layered
    #     grid.rect(x=x, y=y, width=gw, height=gh, gp=node_gp, default.units="snpc")
    #   }
    # }

    draw_shape <- function(xx, yy, node_gp, layers, head, tail, lft)
    {
      # Draw background?
      if ("bg" %in% layers) {
        gp <- node_gp
        gp$col <- NA # no stroke
        grid.polygon(xx,yy, default.units="native", gp=gp)
      }

      # All foreground?
      if ("fg" %in% layers) {
        gp <- node_gp
        gp$fill <- NA # no fill
        grid.polygon(xx, yy, default.units="native", gp=gp)
      }

      # specific foreground layers?
      if ("head" %in% layers) {
        gp <- node_gp
        gp$fill <- NULL # remove
        grid.lines(xx[c(head)], yy[c(head)], default.units="native", gp=gp)
      }
      if ("tail" %in% layers) {
        gp <- node_gp
        gp$fill <- NULL
        grid.lines(xx[c(tail)], yy[c(tail)], default.units="native", gp=gp)
      }
      # Left side?
      if ("lft" %in% layers) {
        gp <- node_gp
        gp$fill <- NULL
        grid.lines(xx[c(lft)], yy[c(lft)], default.units="native", gp=gp)
      }
    }

    draw_box <- function(x, y, gw, gh, node_gp, dir, layers=c("bg","fg")) {
      xl <- x - gw/2 # x-left
      xr <- x + gw/2 # y-left
      xx <- c(xr, xr, xl, xl)
      yhi <- y+0.5*gh
      ylo <- y-0.5*gh
      yy <- c(yhi, ylo, ylo, yhi)

      # rotate?
      pp <- polypoint(xx,yy)
      pivot <- point(x,y)
      theta <- dir2num(dir)
      pp <- rotate(pp, pivot, theta)
      xx <- pp$xx
      yy <- pp$yy

      draw_shape(xx, yy, node_gp, layers)
    }

    draw_bar <- function(x, y, gw, gh, node_gp, dir, layers=c("bg","head","tail")) {
      xl <- x - gw/2 # x-left
      xr <- x + gw/2 # y-left
      xx <- c(xr, xr, xl, xl)
      yhi <- y+0.5*gh
      ylo <- y-0.5*gh
      yy <- c(yhi, ylo, ylo, yhi)

      # rotate?
      pp <- polypoint(xx,yy)
      pivot <- point(x,y)
      theta <- dir2num(dir)
      pp <- rotate(pp, pivot, theta)
      xx <- pp$xx
      yy <- pp$yy

      draw_shape(xx, yy, node_gp, layers, 1:2, 3:4, c(4,1))
    }

    draw_arrow <- function(x, y, gw, gh, node_gp, dir, layers=c("bg","head","tail")) {
      dx <- 0.2*gh
      xl <- x - gw/2 # x-left
      xr <- x + gw/2 # y-left
      xx <- c(xr, xr+dx, xr, xl-dx,xl, xl-dx)
      yhi <- y+0.5*gh
      ylo <- y-0.5*gh
      yy <- c(yhi, y, ylo, ylo, y, yhi)

      # rotate?
      pp <- polypoint(xx,yy)
      pivot <- point(x,y)
      theta <- dir2num(dir)
      pp <- rotate(pp, pivot, theta)
      xx <- pp$xx
      yy <- pp$yy

      draw_shape(xx, yy, node_gp, layers, 1:3, 4:6, c(6,1))

      dx # needed later
    }

    # only draw the box if it is visible, i.e. area>0
    #visible <- gw*gh > 1e-7
    # Now replaced by using dot-names!
    if (!nodes$hidden[i]) {

      # detect layered drawing
      if (is.null(node_gp$layered)) node_gp$layered <- TRUE

      if (node_type=="box") {
        gw <- nodes$length[i]
        gh <- nodes$width[i]
        draw_box(x,y, gw, gh, node_gp, dir)
      } else if (node_type=="bar") {
        gw <- nodes$length[i]
        gh <- nodes$width[i]
        draw_bar(x,y, gw, gh, node_gp, dir)
      } else if (node_type=="arrow") {
        if (dir=="stock") {                 # Stock node
          # Draw background box
          nw <- gw # from above
          nl <- nodes$length[i] # node length, width
          # Draw left (incoming bundle)a
          wl <- sum(bundles$width[idx1]) # width of left/in bundle
          wr <- sum(bundles$width[idx2]) # width of right/out bundle
          xl <- (x-nw/2) + wl/2 # center of left arrow is left boundary + half bundle width
          xr <- (x-nw/2) + wl + wr/2 # idem for right bundle
          # pseudo-draw to determine dx
          dxl <- draw_arrow(xl,y, nl, wl, node_gp, "down", layers=NA)
          dxr <- draw_arrow(xr,y, nl, wr, node_gp, "up",   layers=NA)
          adj <- (dxl-dxr)
          # determine shift for the BG box
          dx <- max(dxl, dxr)
          bl <- nl + 2*dx
          draw_box(x,y-bl/2, bl, nw, node_gp, "down", layers=c("bg")) # square (!) box
          # draw bg
          draw_arrow(xl,y-dxl,   nl,     wl, node_gp, "down", layers="bg")
          draw_arrow(xr,y-adj/2, nl+adj, wr, node_gp, "up",   layers="bg")
          # # draw fg
          draw_arrow(xl,y-dxl,   nl,     wl, node_gp, "down", layers=c("head","tail"))
          draw_arrow(xr,y-adj/2, nl+adj, wr, node_gp, "up",  layers=c("head","tail","lft"))

          # adjust y for the box below
          y <- y - bl/2
          gw <- nw
          gh <- bl
          #grid.abline(intercept=y, units="native", slope=0) # for checking
        } else {
          # ignore previous computed gw/gh (rotation is achieved later)
          gw <- nodes$length[i]
          gh <- nodes$width[i]
          dx <- draw_arrow(x, y, gw, gh, node_gp, dir) # dx needed later
        }
      }
    }

    #---------------------------------------------------- Node decoration ----

    # Format node *label* text
    label_txt <- nodes$label[i]
#    label_txt <- gsub(" ","\n", msg) # space to line break
#    label_txt <- gsub("_"," ", msg) # underscore to space

    # fix Excel "\n" --> "\\n"
    label_txt <- gsub("\\\\n", "\n", label_txt)

    #nr of label lines:
    nlines <- 1 + lengths(regmatches(label_txt, gregexpr("\n", label_txt)))

    # Label position
    label_pos    <- nodes$label_pos[i]
    label_anchor <- nodes$label_anchor[i]
    label_align  <- nodes$label_align[i]
    if (label_pos   =="auto") label_pos    <- global_label_pos
    if (label_anchor=="auto") label_anchor <- global_label_anchor
    if (label_align =="auto") label_align  <- global_label_align

    if (label_pos=="none") next # skip this one

    # treat simple left/right alignments for above/below
    if (label_pos=="above") {
      if        (label_align=="left") {
        label_anchor <- "NW"
        label_align  <- "NE"
      } else if (label_align=="right") {
        label_anchor <- "NE"
        label_align  <- "NW"
      } else if (label_align %in% c("centre","center")) {
        label_anchor <- "N"
        label_align  <- "N"
      }
    }
    if (label_pos=="below") {
      if        (label_align=="left") {
        label_anchor <- "SW"
        label_align  <- "SE"
      } else if (label_align=="right") {
        label_anchor <- "SE"
        label_align  <- "SW"
      } else if (label_align %in% c("centre","center")) {
        label_anchor <- "S"
        label_align  <- "S"
      }
    }

    # approx. height of 1-line label and magnitude
    lfs <- node_style$label_gp$fontsize
    if (is.null(lfs)) lfs <- get.gpar()$fontsize
    mfs <- node_style$mag_gp$fontsize
    if (is.null(mfs)) mfs <- get.gpar()$fontsize

    # Set label align/anchor
    if (label_anchor=="auto") {
      if        (label_pos == "above") {
        label_anchor <- "N"
      } else if (label_pos == "below") {
        label_anchor <- "S"
      } else if (label_pos == "left") {
        label_anchor <- "W"
      } else if (label_pos == "right") {
        label_anchor <- "E"
      } else {
        msg <- sprintf("Invalid label_pos: '%s'", label_pos)
        stop(msg, call.=FALSE)
      }
    }

    if (label_align=="auto") {
      if        (label_pos == "above") {
        label_align <- "N"
      } else if (label_pos == "below") {
        label_align <- "S"
      } else if (label_pos == "left") {
        label_align <- "W"
      } else if (label_pos == "right") {
        label_align <- "E"
      } else {
        msg <- sprintf("Invalid label_pos: '%s'", label_pos)
        stop(msg, call.=FALSE)
      }
    }

    mag_fmt <- node_style$mag_fmt

    # Format node *magnitude* text
    if (nodes$status[i]=="input") {
      mag_txt <- sprintf(mag_fmt, nodes$mag_out[i])
    } else if (nodes$status[i]=="output") {
      mag_txt <- sprintf(mag_fmt, nodes$mag_in[i])
    } else if (nodes$status[i]=="throughput") {
      mag_txt <- sprintf(mag_fmt, nodes$mag_in[i])
    } else {
      mag_txt <- sprintf(mag_fmt, nodes$quantity[i]) # total magnitude !
    }

    # Node magnitude location
    mag_pos    <- nodes$mag_pos[i]
    mag_anchor <- nodes$mag_anchor[i]
    mag_align  <- nodes$mag_align[i]
    if (mag_pos   =="auto") mag_pos    <- global_mag_pos
    if (mag_anchor=="auto") mag_anchor <- global_mag_anchor
    if (mag_align =="auto") mag_align  <- global_mag_align

    # handle combi!
    # if (mag_pos=="label") {
    #   label_txt <- paste0(label_txt, "\n", mag_txt)
    #   mag_txt <- ""
    # }
    # Adjust mag_pos if the node is too small
    if (mag_pos=="inside" ) {
      if (dir %in% horizontal && gh < 12) {
        if (label_pos=="above") mag_pos <- "below"
        if (label_pos=="below") mag_pos <- "above"
        if (label_pos=="left")  mag_pos <- "below"
        if (label_pos=="right") mag_pos <- "below"
      } else if (dir %in% vertical && gw<24) {
        if (label_pos=="above") mag_pos <- "right"
        if (label_pos=="below") mag_pos <- "right"
        if (label_pos=="left")  mag_pos <- "right"
        if (label_pos=="right") mag_pos <- "left"
      }
    }

    if (mag_anchor=="auto") {
      if        (mag_pos == "above") {
        mag_anchor <- "N"
      } else if (mag_pos == "below") {
        mag_anchor <- "S"
      } else if (mag_pos == "left") {
        mag_anchor <- "W"
      } else if (mag_pos == "right") {
        mag_anchor <- "E"
      } else if (mag_pos == "inside") {
        mag_anchor <- "C"
      } else if (mag_pos == "label") {
        mag_anchor <- label_anchor
      } else stop("Invalid mag_anchor: ", mag_anchor)
    }

    if (mag_align=="auto") {
      if        (mag_pos == "above") {
        mag_align <- "N"
      } else if (mag_pos == "below") {
        mag_align <- "S"
      } else if (mag_pos == "left") {
        mag_align <- "W"
      } else if (mag_pos == "right") {
        mag_align <- "E"
      } else if (mag_pos == "inside") {
        mag_align <- "C"
      } else if (mag_pos == "label") {
        mag_align <- label_align
      } else stop("Invalid mag_align: ", mag_align)
    }


    # compute adjuster for "arrow" boxes"
    if (node_type!="arrow") dx <- 0

    # Label anchor coordinates
    #if (label_txt=="Material use") browser()
    lmargin <- 0.2*lfs
    lx <- 0
    xadjust <- 0
    if (grepl("W", label_anchor)) lx <- -gw/2 -lmargin - dx
    if (grepl("E", label_anchor)) lx <- +gw/2 +lmargin + dx
    #if (grepl("E", label_anchor)) xadjust <- dx
    ly <- 0
    if (grepl("N", label_anchor)) ly <-  +gh/2 + 1.7*lmargin
    if (grepl("S", label_anchor)) ly <-  -gh/2 - lmargin
    # ... and justifcation
    lj <- c("centre","centre")
    if (grepl("W", label_align)) lj[1] <- "right"
    if (grepl("E", label_align)) lj[1] <- "left"
    if (grepl("N", label_align)) lj[2] <- "bottom"
    if (grepl("S", label_align)) lj[2] <- "top"

    # Node magnitude coordinates (relative to node pos)
    mmargin <- 2
    mx <- 0
    if (grepl("W", mag_anchor)) mx <- -gw/2 -mmargin - dx
    if (grepl("E", mag_anchor)) mx <- +gw/2 +mmargin + dx
    my <- 0
    if (grepl("N", mag_anchor)) my <- +gh/2 + 1.7*mmargin
    if (grepl("S", mag_anchor)) my <- -gh/2 - mmargin
    # ... and justifcation
    mj <- c("centre","centre")
    if (grepl("W", mag_align)) mj[1] <- "right"
    if (grepl("E", mag_align)) mj[1] <- "left"
    if (grepl("N", mag_align)) mj[2] <- "bottom"
    if (grepl("S", mag_align)) mj[2] <- "top"


    # # repair clashes
    # c1 <- ly==y        # west or east
    # c2 <- ly==my       # same height
    # c3 <- lj[2]==mj[2] # same height!
    # if (c1 && c2 && c3) {
    #   lj[2] <- "bottom"
    #   mj[2] <- "top"
    # }

    vp <- viewport(x=x,y=y, default.units="bigpts", just=c("left","bottom"))
    pushViewport(vp)

    linked <- (mag_anchor==label_anchor) && (mag_align==label_align)
    # Plot label
    # Adjust pos to prevent overlap with magnitude
    #if (linked && grepl("N", label_align)) ly <- ly + 3*mfs
    lgp <- node_style$label_gp
    lgp$lineheight <- 1.0 # fix grid pkg bug
    lg <- textGrob(label_txt, lx,ly, just=lj, default.units="bigpts",gp=lgp)
    lbb <- bbox(lg, dev_scale)

    # Plot magnitude
    #if (linked && grepl("S", label_align)) my <- my - lfs
    mgp <- node_style$mag_gp
    mgp$lineheight <- 1.0 # fix bug
    mg <- textGrob(mag_txt, mx,my, just=mj, default.units="bigpts",
              gp=mgp)
    mbb <- bbox(mg, dev_scale)


    if (overlapping(lg,mg, dev_scale)) {
      #message("Overlapping...")
      if (grepl("S", label_align)) {
        # move magnitude label down
        my <- my - 1.2*lfs*nlines # take inter-line distance into account
        mg <- editGrob(mg, y=unit(my,"bigpts"))
        mbb <- bbox(mg, dev_scale)
      }
      if (grepl("N", label_align)) {
        # move label text up
        #ly <- ly + 1.2*mfs # take inter-line distance into account
        #lg <- textGrob(label_txt, lx,ly, just=lj, default.units="bigpts",gp=node_style$label_gp)
        ly <- ly + 1.2*mfs
        lg <- editGrob(lg, y=unit(ly,"bigpts"))
        lbb <- bbox(lg, dev_scale)
      }
      if (label_align %in% c("W", "E")) {
        # move label slightly UP
        lj <- lg$just
        lj[2] <- "bottom"
        ly <- ly + 4
        lg <- editGrob(lg, y=unit(ly,"bigpts"), just=lj)
        lbb <- bbox(lg, dev_scale)
        # move magnitudee slightly DOWN
        mj <- mg$just
        mj[2] <- "top"
        my <- my - 4
        mg <- editGrob(mg, y=unit(my,"bigpts"), just=mj)
        mbb <- bbox(mg, dev_scale)
      }
    }

    #bb_plot(lbb, col="red")
    #bb_plot(mbb, col="blue")

    grid.draw(lg)
    grid.draw(mg)

    popViewport()

    # printf("  msg=%s mx=%f my=%f just=%s\n", msg, mx, my, mj)
    #if (mag_txt!="") grid.text(mag_txt,   mx,my, just=mj, default.units="snpc")
    #grid.text(label_txt, lx,ly, just=lj, default.units="snpc")
  }

  # plot bundle endpoints
  # grid.points(bundles$Ax,bundles$Ay, default.units="snpc", size=unit(0.2,"char"), pch=16)
  # grid.points(bundles$Bx,bundles$By, default.units="snpc", size=unit(0.2,"char"), pch=16)
  # NULL

  #----------------------------------------------------------------- Legend ----
  if (plot_legend) {

    # use the full legend_gp for the text,
    # but use a simplified version for the boxes
    # (to prevent colored boxes!)

    legend_label_gp <- legend_gp
    legend_gp$col <- NULL

    vp <- viewport(gp=legend_gp)
    pushViewport(vp)

    gp <- get.gpar()
    fs <- gp$fontsize
    ncols <- ifelse(is.null(gp$ncols), 3, gp$ncols)

    U <- unit(fs,"bigpts")
    xoff <- U
    yoff <- U
    bh   <- U # box height
    bw   <- 2*U # box width
    hgap  <- 0.5*U
    vgap  <- 0.5*U
    n <- nrow(palette) # nr of legend entries
    nmax <- ceiling(n/ncols) # max nr per column
    x <- xoff + bw
    #browser()
    y <- ytop <- yoff + nmax*bh + (nmax-1)*vgap - (bh/2) # center of top box
    this_col_to_go <- nmax
    colw <- 0*U # column width (init at 0!)
    for (i in 1:n) {
      grid.rect(x,y, width=bw, height=bh, just=c("right","centre"),
                gp=gpar(fill=palette$color[i]))
      msg <- palette$substance[i]
      grid.text(msg, x+hgap, y, just=c("left","centre"), gp=legend_label_gp)
      colw <- max(colw, stringWidth(msg))
      this_col_to_go <- this_col_to_go - 1
      if (this_col_to_go==0) {
        y <- ytop
        x <- x + bw + colw + 2*hgap
        this_col_to_go <- nmax
        colw <- 0*U # reset
      } else {
        y <- y - bh - vgap
      }
    }
    popViewport()
  }

  #------------------------------------------------------------------ Title ----

  if (!is.null(title)) {
    # Get attributes, or use default ones.
    gp <- attr(title, "gp")
    if (is.null(gp)) gp <- gpar()
    grid.text(title, x=fig_w/2, fig_h-10, default.units="bigpts",
              just=c("centre","top"), gp=gp)
  }

  invisible(nodes) # return
}
