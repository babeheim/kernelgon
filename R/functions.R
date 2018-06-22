

kernelgon <- function(data, prob = 0.89, res = 0.1, ...){
  if(!is.numeric(prob) & prob <= 1 & prob > 0) stop("prob must be a valid probability value")
  fhat <- kde(data)
  hits <- contourLevels(fhat, prob = (1 - prob), approx = TRUE)
  hits <- sort(hits, decreasing = FALSE)
  clev <- c(
    min(c(fhat$estimate, hits) - 0.01 * max(abs(fhat$estimate))), 
    hits, 
    max(c(fhat$estimate, hits)) + 0.01 * max(abs(fhat$estimate))
  )
  within_boundary <- as.numeric(fhat$estimate > clev[2]) # 151 x 151 = 22801 pixels
  within_boundary <- matrix(within_boundary, nrow = 151)
  area_indices <- matrix_coordinates(within_boundary)
  group_tags <- id_groups(area_indices)
  groups <- sort(unique(group_tags))
  n_groups <- length(groups)
  output <- data.frame(x = numeric(), y = numeric())
  for(i in 1:n_groups){
    subgroup_indices <- area_indices[which(group_tags == groups[i]),]
    nonzeros <- data.frame(
      row = fhat$eval.points[[1]][subgroup_indices$row],
      column = fhat$eval.points[[2]][subgroup_indices$column]
    )
    subgroup_outline <- concaveman(as.matrix(nonzeros), length_threshold = res)
    colnames(subgroup_outline) <- c("x", "y")
    polygon(subgroup_outline, ...)
  }
}

matrix_coordinates <- function(binary_mat){
  xrow <- rep(1:nrow(binary_mat), each = ncol(binary_mat))
  xcol <- rep(1:ncol(binary_mat), times = nrow(binary_mat))
  ys <- xcol[binary_mat == 1]
  xs <- xrow[binary_mat == 1]
  output <- data.frame(row = ys, column = xs)
  output
} # returns relative to top left corner, standard matrix notation!

id_groups <- function(moves){
  direct_mat <- id_direct_connections(moves)
  color_network <- graph_from_adjacency_matrix(direct_mat)
  group_id <- components(color_network)$membership
  return(group_id)
}

id_direct_connections <- function(moves, tol = 1){
  direct_mat <- matrix(FALSE, nrow = nrow(moves), ncol = nrow(moves))
  diag(direct_mat) <- TRUE
  for (i in 1:nrow(moves)){
    i_y <- moves$row[i]
    i_x <- moves$column[i]
    south <- which(moves$row == (i_y - tol) & moves$column == i_x)
    north <- which(moves$row == (i_y + tol) & moves$column == i_x)
    west <- which(moves$row == (i_y) & moves$column == (i_x - tol))
    east <- which(moves$row == (i_y) & moves$column == (i_x + tol))
    if (length(south) > 0) direct_mat[i, south] <- TRUE
    if (length(north) > 0) direct_mat[i, north] <- TRUE
    if (length(west) > 0) direct_mat[i, west] <- TRUE
    if (length(east) > 0) direct_mat[i, east] <- TRUE
  }
  return(direct_mat)
}


