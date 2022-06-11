#' Visualise route networks with tmap
#' 
#' This function is for visualising route networks.
#' It was originally developed to show quietness, as defined by
#' [CycleStreets](https://www.cyclestreets.net/help/journey/howitworks/)
#'
#' @param rnet 
#' @param palette 
#' @param lwd_multiplier The width of the largest line relative to the thinnest line
#' @param scale The width of the widest line in the dataset 
#' @param lwd 
#' @param col 
#' @param max_features 
#' @param title.col
#'
#' @return A tmap object
#' @export
#'
#' @examples
#' rnet = rnet_lisbon
#' tm_rnet(rnet, lwd = "ENMAC4", col = "Quietness")
#' tm_rnet(rnet, lwd = "ENMAC4", col = "Quietness", scale = 30)
tm_rnet = function(
    rnet,
    palette = "-temperature_diverging",
    lwd_multiplier = 4,
    scale = 5,
    lwd = names(rnet)[1],
    col = names(rnet)[2],
    max_features = 10000,
    breaks = c(0, 25, 50, 75, 100),
    labels = c("0-24", "25-49", "50-75", "75+"),
    title.col = "Quietness"
    ) {
  rnet_names = names(rnet)
  rnet_names = rnet_names[-grep(pattern = "geom|id", x = rnet_names)]
  lwd_scaled = rnet[[lwd]] - min(rnet[[lwd]])
  lwd_scaled = lwd_scaled / max(lwd_scaled)
  lwd_scaled = lwd_scaled * (lwd_multiplier - 1)
  lwd_scaled = lwd_scaled + 1
  rnet$lwd = lwd_scaled
  pal = cols4all::c4a(palette = palette, n = length(breaks) - 1)
  names(pal) = labels
  rnet$cols = cut(rnet[[col]], breaks = breaks, labels = labels)
  
  # if(packageVersion("tmap") < "4.0") {
  m = tmap::tm_shape(rnet) +
    tmap::tm_lines(id = NULL,
                   lwd = "lwd",
                   scale = scale,
                   popup.vars = rnet_names,
                   col = "cols",
                   palette = pal,
                   title.col = title.col
                   )
  # } else {
  #   # Note: not working
  # m = tmap::tm_shape(rnet) +
  #   tmap::tm_lines(lwd = "lwd", lwd.scale = 5,
  #                  col.scale = palette)
  # tmap::tmap_mode("view")
  # }
  tmap::tmap_leaflet(m)
}
#' @export
#' @aliases tm_rnet
tm_rnet_multi = function(
    rnet,
    palette = "-temperature_diverging",
    lwd_multiplier = 4,
    scale = 5,
    lwd = names(rnet)[1],
    col = names(rnet)[2],
    max_features = 10000,
    breaks = c(0, 25, 50, 75, 100),
    labels = c("0-24", "25-49", "50-75", "75+")
) {
  rnet_names = names(rnet)
  rnet_names = rnet_names[-grep(pattern = "geom|id", x = rnet_names)]
  lwd_scaled = rnet[[lwd]] - min(rnet[[lwd]])
  lwd_scaled = lwd_scaled / max(lwd_scaled)
  lwd_scaled = lwd_scaled * (lwd_multiplier - 1)
  lwd_scaled = lwd_scaled + 1
  rnet$lwd = lwd_scaled
  pal = cols4all::c4a(palette = palette, n = length(breaks) - 1)
  names(pal) = labels
  rnet$cols = cut(rnet[[col]], breaks = breaks, labels = labels)
  
  # if(packageVersion("tmap") < "4.0") {
  m = tmap::tm_shape(rnet) +
    tmap::tm_lines(id = NULL, lwd = "lwd", scale = scale, popup.vars = rnet_names, col = "cols", palette = pal)
  # } else {
  #   # Note: not working
  # m = tmap::tm_shape(rnet) +
  #   tmap::tm_lines(lwd = "lwd", lwd.scale = 5,
  #                  col.scale = palette)
  # tmap::tmap_mode("view")
  # }
  tmap::tmap_leaflet(m)
}


# tm_rnet(rnet)
