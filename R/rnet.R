#' Visualise route networks with tmap
#'
#' @param rnet 
#' @param palette 
#' @param lwd_multiplier The width of the largest line relative to the thinnest line
#' @param scale The width of the widest line in the dataset 
#' @param lwd 
#' @param col 
#' @param max_features 
#'
#' @return A tmap object
#' @export
#'
#' @examples
#' tm_rnet(rnet_lisbon, lwd = "ENMAC4")
tm_rnet = function(
    rnet,
    palette = "mako",
    lwd_multiplier = 4,
    scale = 5,
    lwd = names(rnet)[1],
    col = names(rnet)[2],
    max_features = 10000
    ) {
  rnet_names = names(rnet)
  rnet_names = rnet_names[-grep(pattern = "geom|id", x = rnet_names)]
  lwd_scaled = rnet[[lwd]] - min(rnet[[lwd]])
  lwd_scaled = lwd_scaled / max(lwd_scaled)
  lwd_scaled = lwd_scaled * (lwd_multiplier - 1)
  lwd_scaled = lwd_scaled + 1
  rnet$lwd = lwd_scaled
  m = tmap::tm_shape(rnet) +
    tmap::tm_lines(id = NULL, lwd = "lwd", scale = 5, popup.vars = rnet_names,
                   palette = palette)
  tmap::tmap_leaflet(m)
}

# tm_rnet(rnet)
