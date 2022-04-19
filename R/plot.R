#' @import ggplot2 ggforce
#' @export
plot.configuration_fit <- function(x, ..., theme = theme_SmallGroupNetwork) {
  node <- get_node_coord(x)
  edge <- get_edge_coord(x, node)
  plot_configuration_fit(
    node, edge, theme, 
    get_attribute(x$fit, "type"), 
    get_attribute(x$fit, "directed"),
    get_attribute(x$fit, "loops")
  )
}

#' @import ggplot2 ggforce
#' @export
plot.configuration_fit_set <- function(x, ..., theme = theme_SmallGroupNetwork) {
  node <- list()
  edge <- list()
  for (i in seq_along(x)) {
    if (is.null(names(x)[i]) || names(x)[i] == "") {
      group_name <- i
    } else {
      group_name <- names(x)[i]
    }
    node[[i]] <- get_node_coord(x[[i]], group_name)
    edge[[i]] <- get_edge_coord(x[[i]], node[[i]], group_name)
  }
  node <- do.call(rbind, node)
  edge <- do.call(rbind, edge)
  plot_configuration_fit(
    node, edge, theme,
    get_attribute(x[[1]]$fit, "type"),
    get_attribute(x[[1]]$fit, "directed"),
    get_attribute(x[[1]]$fit, "loops")
  )
}

get_node_coord = function (x, group_name = "group") {
  node_n <- get_attribute(x$fit, "group_size")
  if (is.null(colnames(x$x))) {
    node_names <- seq_len(node_n)
  } else {
    node_names <- colnames(x$x)
  }
  node <- data.frame(group_name, node_names, node_circle_xy(node_n), node_circle_xy(node_n, 1.2))
  names(node) <- c("group","name","x","y","name_x","name_y")
  return(node)
}

get_edge_coord <- function (x, node, group_name = "group") {
  dim_x   <- dim(x$x)
  ntwk    <- x$x
  fit     <- x$fit
  fit_ids <- SmallGroupNetwork:::edge_ids(x$fit)
  if (!get_attribute(x$fit, "directed")) {
    ntwk[is.na(ntwk)] <- t(ntwk)[is.na(ntwk)]
    ntwk <- rowMeans(matrix(c(ntwk, t(ntwk)), ncol = 2))
    dim(ntwk) <- dim_x
    fit_ids <- fit_ids[, fit_ids["row",] < fit_ids["col",]]
  }
  ntwk[is.na(ntwk)] <- 0L
  if (get_attribute(x$fit, "type") == "binary") {
    ntwk[] <- as.integer(ntwk > 0)
  }
  
  edge_id <- data.frame(
    group = group_name,
    ego   = node[fit_ids["row",], "name"],
    alter = node[fit_ids["col",], "name"]
  )
  edge_coord <- rbind(
    cbind(edge_id, node[fit_ids["row",], c("x","y")]),
    cbind(edge_id, bezier_midpoint(
      x1 = node[fit_ids["row",], "x"], 
      y1 = node[fit_ids["row",], "y"], 
      x2 = node[fit_ids["col",], "x"], 
      y2 = node[fit_ids["col",], "y"]
    )),
    cbind(edge_id, node[fit_ids["col",], c("x","y")])
  )
  
  edge <- rbind(
    cbind(edge_coord, edge_type = "Configuration", weight = fit[t(fit_ids)]),
    cbind(edge_coord, edge_type = "Network", weight = ntwk[t(fit_ids)])
  )
  return(edge[edge$weight > 0,])
}

plot_configuration_fit <- function (node, edge, theme, type, directed, loops) {
  p <- ggplot() +
    theme +
    coord_fixed() +
    scale_color_manual(NULL, values = c("#aaaaaa","#000000")) +
    scale_linetype_manual(NULL, values = c("solid", "dashed")) +
    geom_point(aes(x = x, y = y), 
               size = 2, 
               data = node) +
    geom_text(aes(x = name_x, y = name_y, label = name),
              size = 8/.pt,
              data = node)
  
  if (type == "weighted") {
    p <- p + scale_size_binned(NULL, range = c(.25, 1.25))
  }
  if (directed) {
    if (type == "weighted") {
      p <- p + geom_bezier(
        aes(x = x, y = y, 
            group    = paste(edge_type, ego, alter), 
            size     = weight, 
            color    = edge_type,
            linetype = edge_type
        ),
        arrow = arrow(),
        data  = edge)     
    } else {
      p <- p + geom_bezier(
        aes(x = x, y = y, 
            group    = paste(edge_type, ego, alter), 
            color    = edge_type,
            linetype = edge_type
        ),
        arrow = arrow(),
        data  = edge)     
    }
  } else {
    if (type == "weighted") {
      p <- p + geom_bezier(
        aes(x = x, y = y, 
            group    = paste(edge_type, ego, alter), 
            size     = weight, 
            color    = edge_type,
            linetype = edge_type
        ),
        data  = edge)     
    } else {
      p <- p + geom_bezier(
        aes(x = x, y = y, 
            group    = paste(edge_type, ego, alter), 
            color    = edge_type,
            linetype = edge_type
        ),
        data  = edge)     
    }   
  }
  if (length(unique(node$group)) > 1) {
    p <- p + 
      facet_wrap(~ group)
  }
  return(p)
}