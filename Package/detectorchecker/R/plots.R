#' @title Plots module

#' Plots ppp object's density
#'
#' @param ppp_obj ppp object
#' @param caption caption of the figure
#' @param file_path file path
#' @param adjust Kernel density bandwidth
#' @importFrom graphics image par
plot_density <- function(ppp_obj, caption, file_path = NA, adjust = 0.25) {

  if (nchar(caption) > 0) {
    par(mfrow=c(1,1), mar=c(1,1,3,1))

  } else {
    par(mar = c(0, 0, 0, 0))
  }

  if(!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  image(density(ppp_obj, adjust = adjust), main = caption)

  if(!is.na(file_path)) {
    dev.off()
  }
}

#' A function to plot NN oriented arrrows
#'
#' @param ppp_obj ppp object
#' @param caption caption of the figure
#' @param file_path file path
#' @importFrom graphics arrows plot
#' @export
plot_arrows <- function(ppp_obj, caption, file_path = NA) {

  if (nchar(caption) > 0) {
    par(mfrow=c(1,1), mar=c(1,1,3,1))

  } else {
    par(mar = c(0, 0, 0, 0))
  }

  if(!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  PPPnn <- ppp_obj[spatstat::nnwhich(ppp_obj)]

  plot(ppp_obj, main = caption)

  arrows(PPPnn$x, PPPnn$y, ppp_obj$x, ppp_obj$y,
         angle = 15, length = 0.07, col = "red")

  if(!is.na(file_path)) {
    dev.off()
  }
}
