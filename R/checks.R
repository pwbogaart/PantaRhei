.check_field <- function(df, field, types, typ, alias=NULL)
{
  dfname <- deparse(substitute(df))
  colname <- field # for type checking; replace by alias if appropriate
  if (! field %in% names(df)) {
    if (is.null(alias)) {
      ok <- FALSE
    } else {
      ok <- alias %in% names(df)
      if (ok) colname <- alias
    }
    if (!ok) stop(sprintf("'%s' does not have a column '%s'",dfname, field), call.=FALSE)
  }
  if (! class(df[[colname]]) %in% types) stop(sprintf("%s: field '%s' is not <%s>",dfname,field,typ), call.=FALSE)
}

#' Checks the nodes data.frame
#'
#' @param nodes nodes data.frame
#' @keywords internal
#' @noRd
check_nodes <- function(nodes)
{
  if (! "data.frame" %in% class(nodes)) {
    msg <- sprintf("'nodes' is not a data.frame, but a %s", paste(class(nodes),collapse=","))
    stop(msg, call.=FALSE)
  }
  .check_field(nodes, "ID", c("character","factor"), "chr")
  .check_field(nodes, "x", c("numeric","integer","character"), "num,chr")
  .check_field(nodes, "y", c("numeric","integer","character"), "num,chr")
}

#' Checks the flows data.frame
#'
#' @param flows flows data.frame
#' @keywords internal
#' @noRd
check_flows <- function(flows)
{
  if (! "data.frame" %in% class(flows)) {
    msg <- sprintf("'flows' is not a data.frame, but a %s", paste(class(flows),collapse=","))
    stop(msg, call.=FALSE)
  }
  .check_field(flows, "from",      c("character","factor"), "chr")
  .check_field(flows, "to",        c("character","factor"), "chr")
  .check_field(flows, "quantity",  c("numeric","integer"),  "num", alias="qty")
}

#' Checks the palette data.frame
#'
#' @param palette palette data.frame
#' @keywords internal
#' @noRd
check_palette <- function(palette)
{
  if (! "data.frame" %in% class(palette)) {
    msg <- sprintf("'palette' is not a data.frame, but a %s", paste(class(palette),collapse=","))
    stop(msg, call.=FALSE)
  }
  .check_field(palette, "substance", c("character","factor"), "chr")
  .check_field(palette, "color",     c("character"),          "chr", alias="colour")
}


#' Check the consistence of the nodes, flows and palette data.frames
#'
#' @param nodes data.frame containing the nodes definition
#' @param flows data.frame containing the flows definition
#' @param palette data.frame containing the palette definition
#'
#' @return TRUE if all checks are passed; FALSE otherwise.
#' @export
#'
#' @examples
#' nodes <- data.frame(ID=c("A","B"), x=1:2, y=0)
#' flows <- data.frame(from="A", to="B", quantity=10)
#' check_consistency(nodes, flows)
check_consistency <- function(nodes, flows, palette=NULL)
{
  check_nodes(nodes)
  nodes <- parse_nodes(nodes)

  check_flows(flows)
  flows <- parse_flows(flows)

  if (!is.null(palette)) {
    check_palette(palette)
    palette <- parse_palette(palette)
  }

  # Check if all node ID's mentioned in the flows are known nodes
  node_nodes <- unique(nodes$ID)
  for (i in 1:nrow(flows)) {
    # Check "from"
    node <- flows$from[i]
    if (! node %in% node_nodes) {
      browser()
      stop(sprintf("Unknown 'from' node '%s'", node), call.=FALSE)
    }
    # idem "to
    node <- flows$to[i]
    if (! node %in% node_nodes) stop(sprintf("Unknown 'to' node '%s'", node), call.=FALSE)
  }

  # Check for alignment references
  .check_alignment <- function(items, xy, dbg=FALSE) {
    if (dbg) printf("Testing consistency of %s\n", xy)
    if (class(items) != "character") {
      if (dbg) printf("  Non-chr vector; always OK\n")
      return(TRUE)
    }

    abs_pat   <- "^-?[[:digit:]]+([,.][[:digit:]]+)?$" # absolute: e.g. 3.14
    align_pat <- "^\\.?[[:alpha:]][[:alnum:]]*$"         # aligned:  e.g. ".import"
    comp_pat  <- "^\\.?[[:alpha:]][[:print:]]*$"         # computed: e.g. ".import+3"
    for (item in items) {
      if (dbg) printf("  %s", item)
      if (grepl(abs_pat, item)) {
        if (dbg) printf("; absolute value")
        ok <- TRUE # always OK
      } else if (grepl(align_pat, item)) {
        # check if item is known
        ok <- item %in% node_nodes
        if (!ok) {
          msg <- sprintf("%s alignment error: reference to unknown node '%s'", xy, item)
          stop(msg, call.=FALSE)
        }
        if (dbg) printf("; aligned")
      } else if (grepl(comp_pat, item)) {
        # computed item, check if all variables are known
        expr <- parse(text=item)
        vv <- all.vars(expr)
        for (v in vv) {
          if (! v %in% node_nodes) {
            msg <- sprintf("%s alignment error: reference to unknown node '%s' in expression '%s'", xy, v, item)
            stop(msg, call.=FALSE)
          }
        }
        ok <- all(all.vars(expr) %in% node_nodes)
        if (dbg) printf("; computed")
      } else {
        ok <- FALSE # fallthrough case
      }
      if (dbg) printf("; %s\n", ifelse(ok,"OK","NOT OK"))
      if (!ok) stop(sprintf("%s alignment error: '%s'", xy, item), call.=FALSE)
    }
  }
  .check_alignment(nodes$x, "x")
  .check_alignment(nodes$y, "y")

  # Report any nodes that are not used for flows
  flow_nodes <- unique(c(flows$from, flows$to))
  used <- node_nodes %in% flow_nodes
  if (!all(used)) {
    msg <- sprintf("! Not all nodes used for flows: %s\n", paste(node_nodes[!used], collapse=", "))
    message(msg)
  }

  # check colors
  if (!missing(palette)) {
    subs <- unique(flows$substance)
    for (s in subs) {
      if (! s %in% palette$substance) {
        msg <- sprintf("Substance %s not found in color specification", s)
        stop(msg, call.=FALSE)
      }
    }
  }

  TRUE
}

#' Checks the mass balance of the flows involved
#'
#' For each substance involved, the balance per (internal) node is inspected.
#' If outflow exceed inflow, or vice versa, a message is printed,
#' and the function returns FALSE.
#'
#' @param nodes data.frame containing the nodes definition
#' @param flows data.frame containing the flows definition
#' @param tolerance numeric specifying a tolerance. Default is 0.01 (1%)
#'
#' @return TRUE if balanced,  FALSE if not.
#' @export
#'
#' @examples
#' nodes <- data.frame(ID=c("A","B","C"), x=1:3, y=1:3, dir=c("right","right","stock"))
#' flows <- data.frame(from=c("A","B"), to=c("B","C"), quantity=c(10,10))
#' check_balance(nodes,flows)
check_balance <- function(nodes,flows, tolerance=0.01) {
  check_nodes(nodes)
  nodes <- parse_nodes(nodes)
  check_flows(flows)
  flows <- parse_flows(flows)

  balanced <- TRUE

  subs <- unique(flows$substance)
  ns <- length(subs)
  for (s in subs) {
    # printf("Looking at: %s\n", type)
    idx <- flows$substance == s
    sub_flows <- flows[idx, ]
    flow_nodes <- base::intersect(sub_flows$from, sub_flows$to)
    for (node in flow_nodes) {
      # skip 'stock' nodes
      i <- match(node, nodes$ID)
      if (!is.na(i) && nodes$dir[i] == "stock") next

      idx <- which(sub_flows$to == node)
      inflow <- sum(sub_flows$quantity[idx])
      idx <- which(sub_flows$from == node)
      outflow <- sum(sub_flows$quantity[idx])
      # outflow too high? : shortage
      if (outflow > (1+tolerance)*inflow) {
        abs_shortage <- outflow - inflow
        rel_shortage <- 100 * abs_shortage / inflow
        msg <- sprintf("! Balance error for '%s' in node '%s': Inflow=%.2f; Outflow=%.2f; Shortage=%.2f (%.1f%%)\n",
                       s, node, inflow, outflow, abs_shortage, rel_shortage)
        warning(msg)
        balanced <- FALSE
      }
      # outflow too low? Excess...
      if (outflow < (1-tolerance)*inflow) {
        abs_excess <- inflow - outflow
        rel_excess <- 100 * abs_excess / inflow
        msg <- sprintf("! Balance error for '%s' in node '%s': Inflow=%.2f; Outflow=%.2f; Excess=%.2f (%.1f%%)\n",
                       s, node, inflow, outflow, abs_excess, rel_excess)
        warning(msg)
        balanced <- FALSE
      }
    }
  }

  return(balanced)
}
