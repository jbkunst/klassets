.onLoad <- function(libname = find.package("klassets"), pkgname = "klassets") {

  options(
    # this option will be used/changed to make animations using gganimate
    # the geom will be change from contour to raster
    klassets.geom_contour_fill = TRUE
    )

}
