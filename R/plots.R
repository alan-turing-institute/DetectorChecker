#' @title Plots module

#' Plots density
#'
#' @param ppp_obj ppp object
#' @param caption caption of the figure
#' @param file_path file path
#' @param adjust Kernel density bandwidth
#' @param color a list of colors
#' @importFrom graphics image par
plot_density <- function(ppp_obj, caption, file_path = NA, adjust = 0.5, color = topo.colors(50)) {
  if (nchar(caption) > 0) {
    par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))
  } else {
    par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
  }

  if (!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  image(density(ppp_obj, adjust = adjust), main = caption, col = color)

  if (!is.na(file_path)) {
    dev.off()
  }
}

#' Plots NN oriented arrrows
#'
#' @param ppp_obj ppp object
#' @param caption caption of the figure
#' @param file_path file path
#' @importFrom graphics arrows plot
#' @export
plot_arrows <- function(ppp_obj, caption, file_path = NA) {
  if (nchar(caption) > 0) {
    par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))
  } else {
    par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
  }

  if (!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  PPPnn <- ppp_obj[spatstat::nnwhich(ppp_obj)]

  plot(ppp_obj, main = caption)

  arrows(PPPnn$x, PPPnn$y, ppp_obj$x, ppp_obj$y,
    angle = 15, length = 0.07, col = "red"
  )

  if (!is.na(file_path)) {
    dev.off()
  }
}

#' Plots NN angles
#'
#' @param ppp_obj ppp object
#' @param caption caption of the figure
#' @param file_path file path
#' @export
plot_angles <- function(ppp_obj, caption, file_path = NA) {
  if (nchar(caption) > 0) {
    par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))
  } else {
    par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
  }

  if (!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  spatstat::rose(spatstat::nnorient(ppp_obj, sigma = 4),
    col = "grey", main = caption
  )

  if (!is.na(file_path)) {
    dev.off()
  }
}

#' Plots K, F, G functions
#'
#' @param ppp_obj ppp object
#' @param func Function name
#' @param file_path Output file path
#' @param caption Flag to turn on/off figure caption
#' @importFrom stats density
#' @importFrom grDevices dev.off
#' @export
plot_kfg <- function(ppp_obj, func, file_path = NA, caption = TRUE) {
  if (missing(func) || is.null(func)) {
    stop(c(
      "Analysis function name is not specified.\n",
      "Available functions: K, F, G, Kinhom, Finhom, Ginhom"
    ))
  }

  fig_caption <- ""

  if (caption > 0) {
    par(mfrow = c(1, 1), mar = c(3, 3, 4, 1))
  } else {
    par(mfrow = c(1, 1), mar = c(3, 3, 1, 1))
  }

  if (!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  if (func == "K") {
    if (caption) fig_caption <- "K-function"
    plot(spatstat::Kest(ppp_obj), main = fig_caption)
  } else if (func == "F") {
    if (caption) fig_caption <- "F-function"
    plot(spatstat::Fest(ppp_obj), main = fig_caption)
  } else if (func == "G") {
    if (caption) fig_caption <- "G-function"
    plot(spatstat::Gest(ppp_obj), main = fig_caption)
  } else if (func == "Kinhom") {
    lambda <- density(ppp_obj)
    if (caption) fig_caption <- "Inhomogeneous K-Function"

    plot(spatstat::Kinhom(ppp_obj, lambda, correction = "all"),
      cex = 0.5, main = fig_caption
    )
  } else if (func == "Finhom") {
    lambda <- density(ppp_obj)
    if (caption) fig_caption <- "Inhomogeneous F-Function"

    plot(spatstat::Finhom(ppp_obj, lambda, correction = "all"),
      cex = 0.5, main = fig_caption
    )
  } else if (func == "Ginhom") {
    lambda <- density(ppp_obj)
    if (caption) fig_caption <- "Inhomogeneous G-Function"

    plot(spatstat::Ginhom(ppp_obj, lambda, correction = "all"),
      cex = 0.5, main = fig_caption
    )
  } else {
    stop(c(
      "Cannot identify analysis function.\n",
      "Available functions: K, F, G, Kinhom, Finhom, Ginhom"
    ))
  }

  if (!is.na(file_path)) {
    dev.off()
  }
}

#' Plots dead pixel counts
#'
#' @param module_count_arr Counts per array
#' @param caption caption of the figure
#' @param file_path file path
#' @export
plot_counts <- function(module_count_arr, caption, file_path = NA) {
  if (nchar(caption) > 0) {
    par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))
  } else {
    par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
  }

  if (!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  plot(module_count_arr, main = caption)

  if (!is.na(file_path)) {
    dev.off()
  }
}
