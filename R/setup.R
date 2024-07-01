
# Coordinates of segment cenroids (km)
x <- rep(c(0.25, 0.75), each = 5)
y <- seq(0.1, 0.9, 0.2); y <- c(y, rev(y))
xy <- cbind(x, y); rm(x, y)
# plot(xy, xlim = c(0, 1), ylim = c(0, 1), pch = as.character(1:10), asp = 1); rect(0, 0, 1, 1)

dispersion <- function(x, ...) {
  # Index of dispersion
  var(x, ...) / mean(x, ...)
}

# dispersion <- function(x, ...) {
# Index of dispersion using L1
#   m <- median(x, ...)
#   mean(abs(m - x)) / m
# }

centroid <- function(x) {
  # Calculates a centroid of a cloud of points (x is a 2-column matrix of coordinates)
  colMeans(x)
}

sdd <- function(x) {
  # Calculates standard distance deviation (2D analog of standard deviation).
  if(nrow(x) > 1)
  {
    mc <- centroid(x)
    return(sqrt(sum((x[, 1] - mc[1])^2 + (x[, 2] - mc[2])^2) / nrow(x)))
  } else
    return(0)
}


circ <- function(o, r, ...) {
  # Plots a circle with a centre 'o' and diameter 'r'.
  bearing <- 1:360 * pi / 180
  cx <- o[1] + r * cos(bearing)
  cy <- o[2] + r * sin(bearing)
  circle <- cbind(cx, cy)
  lines(circle, ...)
}


dist_dev <- function(x) {
  # Helper function to calculate sdd from a distribution of observaions in segments.
  n_spat <- xy[rep(1:10, x), , drop = FALSE]
  sdd(n_spat)
}


plot_obs <- function(x, ...) {
  # Visualisation of the spatial distribution of observations across segments 
  n_spat <- xy[rep(1:10, x), , drop = FALSE] # translated into coordinates
  n <- max(nrow(n_spat), 1)
  mc <- centroid(n_spat)
  sd <- sdd(n_spat)
  tit <- paste("dispersion =", round(sd, 2))
  subtit <- paste("n =", sum(x))
  plot(jitter(n_spat, amount = 0.04), xlim = c(0, 1), ylim = c(0, 1), asp = 1, col = adjustcolor("blue", alpha = 50 / n), main = c(tit, "\n", "\n" ,subtit), ...)
  rect(0, 0, 1, 1)
  points(mc[1], mc[2], pch = 4, col = "red", lwd = 2, cex = 2)
  circ(mc, sd, col = "red")
}


stretch <- function(x, max_val = 1) {
  
  # Linear rescaling between 0 and 'max_val'.
  max_val * scale(x, center = min(x), scale = diff(range(x)))
}


rsd <- function(x, idx = NULL, disp_max = NULL) {
  
  # Calculates a Relative Spatial Dispersion index.
  # 
  # Arguments:
  #         x : The numeric vector with the number of birds observed in each segment.
  #       idx : All permutations of the vector 'seq(10)', preferably generated in advance. 
  #   disp_max: TODO
  # 
  # The calculation is carried out in the following steps:
  #   1. The standard distance deviation is calculated for the observed spatial 
  #       arrangement of birds/territories over the survey segments.
  #   2. The maximum possible value of the index is found by:
  #       a)  redistributing the total abundance evenly across all segments,
  #       b)  finding the spatial configuration of this even distribution 
  #           that gives the maximum value of the index by enumerating all 
  #           possible permutations. This is done by the helper function 'max_rsd'.
  #   3. Rescaling of the index value between 0 and 1.
  # 
  # Interpretation: 
  # For full aggregarion (i.e. when all birds in a square were observed
  # in a single segment) the index takes zero. For the maximum possible dispersion
  # (when birds are spread as evenly as possible) the index the index takes 1. 
  
  disp_obs <- dist_dev(x)
  
  if(disp_obs == 0)
    disp_index <- 0
  
  else
    n <- sum(x)
  
  if(is.null(disp_max)) {
    
    # Generating permutations (if not provided)
    if(is.null(idx)) {
      require(combinat)
      idx <- permn(10)
    }
    
    # Calculating the maximum possible value of the index (if not provided)
    disp_max <- max_rsd(n, idx)
  }
  
  else
    if(is.data.frame(disp_max)) {
      id <- disp_max$n == n
      if(any(id))
        disp_max <- disp_max$max_rsd[id]
      else
        disp_max <- NA
    }
  
  # Rescale
  disp_index <- stretch(c(0, disp_obs, disp_max))[2, ]
  
  return(disp_index)
}




max_rsd <- function(n, idx, type = "timer") {
  
  # Calculates the maximum possible value of the Relative Spatial Dispersion index
  # for a given total abundance. This function is intended as a workhorse for 
  # the 'rsd' function, or to pre-calculate these values and then use them 
  # as a look-up table when calculating the index for the larger dataset.
  # 
  # Arguments:
  #      n: total abundance
  #    idx: list of permutations
  #   type: type of the progress bar ("none" to switch it off)
  
  require(pbapply)
  
  pbo <- pboptions(type = type)
  on.exit(pboptions(pbo), add = TRUE)
  
  # Redistribute the total abundance evenly across all segments
  modulo <- n %/% 10
  even <- rep(modulo, 10)
  remainder <- n %% 10
  even <- even + c(rep(1, remainder), rep(0, 10 - remainder))
  # even
  # plot_obs(even)
  
  # Find the spatial configuration that gives the maximum value of the index. 
  sdds <- pbsapply(idx, \(y) dist_dev(even[y]))
  # max_idx <- even[idx[[which.max(sdds)]]]
  # plot_obs(max_idx)
  disp_max <- max(sdds)
  
  return(disp_max)
}
