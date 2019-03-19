library(igraph)

#' Clasifies a clump
#'
#' @param detector Detector object
#' @param x vector containing the x coordinates of a clump
#' @param y vector containing the y coordinates of a clump
#' @return the class of a clump (1 - singleton, 2 - double, 3 - triplet,
#'   4 - larger cluster, unless it actually has the shape of a line,
#'   5 (6): vertical line where closest edge is the upper (lower) one,
#'   7 (8): horizontal line where closest edge is the right (left) one)
.classify_clump <- function(detector, x, y) {

  # x and y are vectors of the same length
  # x=columns=horizontale positions
  # y=rows=verticale positions

  # Note that size of clump is given by length(x) (=length(y)).
  # range(x) length shows expansion in x-direction, range(y) expansion in y-direction.

  nr <- detector$detector_height
  nc <- detector$detector_width

  # a priori assignment for class label is 0, which means nothing and is used
  #   later to check if class label assignment failed
  clc <- 0

  # type 1: singleton
  if (length(x) == 1) {
    clc <- 1
  } # type 2: double
  else if (length(x) == 2) {
    clc <- 2
  } # type 3: triplet (L-shapes or line of 3, could be singleton + 1d infected nn)
  else if (length(x) == 3) {
    clc <- 3
  } # type 4: larger cluster (a priori assignment) with lines as special cases
  #  reassigned below to other class labels
  else if (length(x) >= 4) {
    clc <- 4
  } # type 0: Error
  else {
    stop("Clump's coordintes length cannot be zero.")
  }

  # In preparation for identifying special cases of larger clusters:
  # maximal horizontal range
  xrange <- max(x) - min(x) + 1
  # maximal vertical range
  yrange <- max(y) - min(y) + 1

  # Note: The scenario of other litte bits attached to the line apart from the
  # cluster at the end is rather tedious, but is neglegiable and also may not
  # pose problem as we delete line and then retranslate making them singletons.
  # So, define larger cluster as line if enough pixel are on the mode
  # (which defines line location).
  if (yrange > 4 & xrange < 4) {
    # x-coo of the potential line can be determined by mode(x)
    xline <- .getmode(x)

    # vertical line (with up to 10% nn allowed, apart from 2 for the cluster at the end)
    if (length(x) - 2 < length(x == xline) * 1.1) {
      # type 5: closest to upper edge
      if (nr - max(y) + 1 < min(y)) {
        clc <- 5
      } # type 6: closest to lower edge
      else {
        clc <- 6
      }
    }
  }

  if (xrange > 4 & yrange < 4) {
    # x-coo of the potential line can be determined by mode(x)
    yline <- .getmode(y)

    # horizontal line (with up to 10% nn allowed, apart from 2 for the cluster at the end)
    if (length(y) - 2 < length(y == yline) * 1.1) {
      # type 7: nearest to right side
      if (nc - max(x) + 1 < min(x)) {
        clc <- 7
      } # type 8: nearest to left side
      else {
        clc <- 8
      }
    }
  }

  return(clc)
  #
  #   if (clc==0){cat("Error in class assignment: no class could be identified for clump ", i,
  #                   ". Double check image. Trace where that clump is. Enumeration goes from
  #                   top left to bottom right. However, original image may have been transposed
  #                   and flipped.")}
}

#' Clasifies clumps with respect to xy coordinates.
#'
#' @param detector Detector object
#' @param xyc_pixel_df xyc_pixel_df
#' @return data frame with clasification results
.xyc_ply_func <- function(detector, xyc_pixel_df) {
  dataFrame <- plyr::ddply(xyc_pixel_df, "id",
    dplyr::summarise, # 1
    class = .classify_clump(detector, x, y), # 2
    size = length(x), # 3
    xct = min(x) + (max(x) - min(x)) / 2, # 4 not always a pixel (see below)
    yct = min(y) + (max(y) - min(y)) / 2, # 5 not always a pixel
    xctpix = round(median(x)), # 6 better than above, avoids centre pulled by hairs sticking out
    yctpix = round(median(y)), # 7 dito
    xmin = min(x), # 8
    xmax = max(x), # 9
    xrange = max(x) - min(x) + 1, # 10
    ymin = min(y), # 11
    ymax = max(y), # 12
    yrange = max(y) - min(y) + 1, # 13
    xmode = .getmode(x), # 14
    ymode = .getmode(y)
  ) # 15


  # Explanations:
  # Clumps have IDs and come with a set of xy coordinates.
  # For each clump, extract features and store in data frame using dplyr.
  # summarise() looks at all xy with same ID and then applies functions.
  # Here apply functions to the pair of vectors x,y.
  # In particularly, each clump now has a class.
  # Size: just use length of the vector of all coordinates (x or y same, as they are pairs)
  # Pixels are described by their centres.
  # xct, yct: do not have to be a pixel, but could be in between two.
  # Specifically, even: on line between two central pixels, odd: centre of (central) pixel
  # Hence we use xctpix, yctpix instead: not perfect, can easily construct pathological scenarios, but good enough for our data!
  # xlength, ylength: max extension in these directions
  # One a general note, while plyr seems to be so popular, it makes the code not transparent to refer to column numbers.
  # Can we do column names?

  return(dataFrame)
}

#' Modifying clusters to events (consisting of 1 pixel representing the cluster)
#' Make into a point pattern of just events rather than pixels. Using xyc_ply object.
#' Collapse in one point using centres for clusters, but end points for lines, type dependend:
#' type 5 (closest to upper edge): ymin
#' type 6 (closest to lower edge): ymax
#' type 7 (closest to right edge): xmin
#' type 8 (closest to left edge):  xmax
#' This is inspired by Perkin Elmer Detector and be replaced by other choices if desired.
#'
#' @param xyc_ply clums data frame
#' @return events
.xyc_pixels2events <- function(xyc_ply) {
  xyc_events <- xyc_ply[, c(6, 7, 1, 2, 3)]

  # This defines the default for representing a cluster by centres xctpix, yctpix.
  # For lines, this is replaced by endpoints (choice how explained above).
  for (i in 1:dim(xyc_events)[1]) {
    if (xyc_events[i, 4] == 5) {
      xyc_events[i, 1] <- xyc_ply[i, 14] # xmode
      xyc_events[i, 2] <- xyc_ply[i, 12] # ymin
    } else if (xyc_events[i, 4] == 6) {
      xyc_events[i, 1] <- xyc_ply[i, 14] # xmode
      xyc_events[i, 2] <- xyc_ply[i, 11] # ymax
    } else if (xyc_events[i, 4] == 7) {
      xyc_events[i, 1] <- xyc_ply[i, 9] # ymode
      xyc_events[i, 2] <- xyc_ply[i, 15] # xmin
    } else if (xyc_events[i, 4] == 8) {
      xyc_events[i, 1] <- xyc_ply[i, 8] # ymode
      xyc_events[i, 2] <- xyc_ply[i, 15] # xmax
    }
  }

  return(xyc_events)
}

#' Converts mask (dead pixels) to events
#'
#' @param detector Detector object
#' @param dead_pix_mask Dead pixels mask
#' @param row Module row number
#' @param col Module col number
#' @return list of pixels and events
.mask_to_events <- function(detector, dead_pix_mask, row = NA, col = NA) {
  if (!is.na(row) && !is.na(col)) {
    shift_left <- detector$module_edges_col[1, col] - 1
    shift_up <- detector$module_edges_row[1, row] - 1

    nc <- detector$module_col_sizes[col]
    nr <- detector$module_row_sizes[row]
  }

  nr <- detector$detector_height
  nc <- detector$detector_width

  # Raster object preparation:
  # Detect & classify into lines, clusters, duplicates etc using functions above
  # May do this approximately to save time!!! Really!!!!!
  # identify & group all adjacent pixels:
  # extract clusters of pixels (that are not NA)
  rr <- raster::raster(nrow = nr, ncol = nc, xmn = 0, xmx = nc, ymn = 0, ymx = nr)

  # need to both transpose matrix and flip about horizontal axis
  raster::values(rr) <- t(dead_pix_mask[, c(nr:1)])

  # detect clumps (patches) of connected cells, directions = 4 (Rook's case)
  rrc <- suppressWarnings(raster::clump(rr, directions = 4))

  # Make data frame with all pixels part of clusters (clumps) and their clump
  #  ID and centre's coordinates. Note we use ceiling, because xyc would have
  #  coordinates converted from integers to .5 ending
  xyc_df <- data.frame(
    ceiling(raster::xyFromCell(rrc, which(!is.na(raster::getValues(rrc))))),
    id = raster::getValues(rrc)[!is.na(raster::getValues(rrc))],
    .clump_module(detector, rrc)
  )

  if (!is.na(row) && !is.na(col)) {
    # check whether the row and col numbers are correct
    .check_select(detector, row, col)

    xyc_df <- xyc_df[xyc_df$mod_row == row & xyc_df$mod_col == col, ]

    xyc_df$x <- xyc_df$x - shift_left
    xyc_df$y <- xyc_df$y - shift_up
  }

  xyc_events <- .xyc_pixels2events(.xyc_ply_func(detector, xyc_df))

  return(list(pixels = xyc_df, events = xyc_events))
}

#' Identifying modules for clumps
#'
#' @param detector Detector object
#' @param rrc raster clumps objects
#' @return data frame of the modules relating the clump
.clump_module <- function(detector, rrc) {
  xy_df_temp <- data.frame(ceiling(raster::xyFromCell(rrc, which(!is.na(raster::getValues(rrc))))))

  xy_df_temp$mod_row <- NA
  xy_df_temp$mod_col <- NA

  for (i in 1:nrow(xy_df_temp)) {
    xy_df_temp$mod_row[i] <- which_module(xy_df_temp$y[i], detector$module_edges_row)
    xy_df_temp$mod_col[i] <- which_module(xy_df_temp$x[i], detector$module_edges_col)
  }

  dataFrame <- data.frame(mod_row = xy_df_temp$mod_row, mod_col = xy_df_temp$mod_col)

  return(dataFrame)
}

#' Locates and clasifies clumps of a damaged detector
#'
#' @param detector Detector object
#' @param row Module row number
#' @param col Module column number
#' @return Detector with events matrix
#' @export
find_clumps <- function(detector, row = NA, col = NA) {
  pixel_mask <- get_dead_pix_mask(detector)

  pixel_events <- .mask_to_events(detector, pixel_mask, row = row, col = col)

  detector$clumps <- list(
    pixels = pixel_events$pixels,
    events = pixel_events$events
  )

  # getting the events mask (0 and 1 s)
  detector$clumps$events_matrix <- get_events_mask(detector)

  return(detector)
}

#' Plots damaged detector events
#'
#' @param detector Detector object
#' @param file_path Output file path
#' @param caption Flag to turn on/off figure caption
#' @param incl_event_list a list of events to be included
#' @param plot_edges_gaps Plot edgees gaps
#' @export
plot_events <- function(detector, file_path = NA, caption = TRUE, incl_event_list = NA,
                        plot_edges_gaps = TRUE) {
  if (!caption) {
    main_caption <- ""
    par(mar = c(0, 0, 0, 0))
  } else {
    main_caption <- "Defective events"
    par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))
  }

  if (!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  ppp_events <- .get_clump_event_ppp(detector, incl_event_list = incl_event_list)

  # "Defective events"
  plot(ppp_events, pch = 22, main = main_caption)

  # plot(ppp_events, pch=22, col=2, main="Defective events") doesn't work, hense, cheat:
  points(ppp_events, pch = 22, col = 2)

  if (plot_edges_gaps) {
    edges_gaps <- .get_detector_ppps(detector)

    points(edges_gaps[[1]], pch = ".")
    points(edges_gaps[[2]], pch = ".")

    if ((!is.null(edges_gaps[[3]])) && (!is.null(edges_gaps[[4]]))) {
      points(edges_gaps[[3]], pch = ".")
      points(edges_gaps[[4]], pch = ".")
    }
  }

  if (!is.na(file_path)) {
    dev.off()
  }
}

#' Plots damaged detector module events
#'
#' @param detector Detector object
#' @param col Module column number
#' @param row Module row number
#' @param file_path Output file path
#' @param caption Flag to turn on/off figure caption
#' @param incl_event_list a list of events to be included
#' @export
plot_module_events <- function(detector, col, row, file_path = NA, caption = TRUE, incl_event_list = NA) {
  if (!caption) par(mar = c(0, 0, 0, 0))

  # check whether the row and col numbers are correct
  .check_select(detector, row, col)

  if (!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  width <- detector$module_col_sizes[col]
  height <- detector$module_row_sizes[row]

  ppp_frame <- spatstat::ppp(1, 1, c(1, width), c(1, height))

  if (caption) {
    main_caption <- paste(detector$name, "with damaged pixels\n (black=module edges)")
  } else {
    main_caption <- ""
  }

  plot(ppp_frame, pch = ".", cex.main = 0.7, main = main_caption)

  ppp_events <- .get_clump_event_ppp(detector,
    incl_event_list = incl_event_list,
    height = height, width = width
  )

  points(ppp_events, pch = 22, col = 2)

  if (!is.na(file_path)) {
    # shuts down the specified (by default the current) device
    dev.off()
  }
}

#' Creates ppp object for damaged detector events
#'
#' @param detector Detector object
#' @param incl_event_list a list of events to be included
#' @param height Detector height
#' @param width Detector width
#' @return ppp object for damaged detector events
.get_clump_event_ppp <- function(detector, incl_event_list = NA,
                                 height = NULL, width = NULL) {
  if (is.null(height)) {
    nr <- detector$detector_height
  } else {
    nr <- height
  }

  if (is.null(height)) {
    nc <- detector$detector_width
  } else {
    nc <- width
  }

  if (suppressWarnings(is.list(incl_event_list))) {
    events <- detector$clumps$events[detector$clumps$events$class %in% incl_event_list, ]
  } else if (suppressWarnings(!is.na(incl_event_list))) {
    events <- detector$clumps$events[detector$clumps$events$class == incl_event_list, ]
  } else {
    events <- detector$clumps$events
  }
 
  event_ppp <- spatstat::ppp(events[, 1], events[, 2], c(1, nc), c(1, nr))

  return(event_ppp)
}

#' Creates ppp object for damaged detector pixels
#'
#' @param detector Detector object
#' @param incl_event_list a list of events to be included
#' @return ppp object for damaged detector pixels
.get_clump_pixel_ppp <- function(detector, incl_event_list = NA) {
  nr <- detector$detector_height
  nc <- detector$detector_width

  if (suppressWarnings(is.list(incl_event_list))) {
    pixels <- detector$clumps$pixels[detector$clumps$pixels$id %in% incl_event_list, ]
  } else if (suppressWarnings(!is.na(incl_event_list))) {
    pixels <- detector$clumps$pixels[detector$clumps$pixels$id == incl_event_list, ]
  } else {
    pixels <- detector$clumps$pixels
  }

  pixel_ppp <- spatstat::ppp(pixels[, 1], pixels[, 2], c(1, nc), c(1, nr))

  return(pixel_ppp)
}

#' Generates events matrix (a matrix with pixels as 0 and events as 1)
#'
#' @param detector Detector object
#' @return events mask
#' @export
get_events_mask <- function(detector) {
  mask <- matrix(0, nrow = detector$detector_width, ncol = detector$detector_height)

  events_cnt <- nrow(detector$clumps$events)
  if (events_cnt > 0) {
    for (event_i in 1:events_cnt) {
      x <- detector$clumps$events[event_i, ]$xctpix
      y <- detector$clumps$events[event_i, ]$yctpix

      mask[x, y] <- 1
    }
  }

  return(mask)
}

#' Plots damaged detector pixels and events
#'
#' @param detector Detector object
#' @param file_path Output file path
#' @param caption Flag to turn on/off figure caption
#' @param incl_event_list a list of events to be included
#' @export
plot_pixels_events <- function(detector, file_path = NA, caption = TRUE, incl_event_list = NA) {
  if (!caption) {
    main_caption <- ""
    par(mar = c(0, 0, 0, 0))
  } else {
    main_caption <- "Defective pixels (black) and events (red)"
    par(mfrow = c(1, 1), mar = c(0, 0, 4, 0) + 0.1, oma = c(0, 0, 0, 0))
  }

  if (!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  ppp_pixels <- .get_clump_pixel_ppp(detector, incl_event_list = incl_event_list)
  ppp_events <- .get_clump_event_ppp(detector, incl_event_list = incl_event_list)

  # Defective pixels
  plot(ppp_pixels, pch = 22, main = main_caption)

  # Defective events
  # plot(ppp_events, pch=22, col=2, main="Defective events") doesn't work, hense, cheat:
  points(ppp_events, pch = 22, col = 2)

  if (!is.na(file_path)) {
    dev.off()
  }
}

#' Plots density graph of events of a detector or module
#'
#' @param detector Detector object
#' @param file_path Output file path
#' @param adjust Kernel density bandwidth
#' @param row Module row number
#' @param col Module column number
#' @param caption Flag to turn on/off figure caption
#' @param incl_event_list a list of events to be included
#' @export
plot_events_density <- function(detector, file_path = NA, adjust = 0.25,
                                row = NA, col = NA, caption = TRUE,
                                incl_event_list = NA) {
  main_caption <- ""

  if (!is.na(row) && !is.na(col)) {
    # check whether the row and col numbers are correct
    .check_select(detector, row, col)

    if (caption) {
      main_caption <- paste("Events density (row=", row, "col=", col, "), adjust=", adjust)
    }

    height <- detector$module_row_sizes[row]
    width <- detector$module_col_sizes[col]
  } else {
    if (caption) {
      main_caption <- paste("Events density, adjust = ", adjust)
    }

    height <- NULL
    width <- NULL
  }

  ppp_events <- .get_clump_event_ppp(detector,
    incl_event_list = incl_event_list,
    height = height, width = width
  )

  plot_density(ppp_events, main_caption, file_path = file_path, adjust = adjust)
}

#' Plots arrows graph of events of a detector or module
#'
#' @param detector Detector object
#' @param file_path Output file path
#' @param row Module row number
#' @param col Module column number
#' @param caption Flag to turn on/off figure caption
#' @param incl_event_list a list of events to be included
#' @export
plot_events_arrows <- function(detector, file_path = NA,
                               row = NA, col = NA, caption = TRUE,
                               incl_event_list = NA) {
  main_caption <- ""

  if (!is.na(row) && !is.na(col)) {
    # check whether the row and col numbers are correct
    .check_select(detector, row, col)

    if (caption) {
      main_caption <- paste("Arrows of events (row=", row, "col=", col, ")")
    }

    height <- detector$module_row_sizes[row]
    width <- detector$module_col_sizes[col]
  } else {
    if (caption) {
      main_caption <- paste("Arrows of events")
    }

    height <- NULL
    width <- NULL
  }

  ppp_events <- .get_clump_event_ppp(detector,
    incl_event_list = incl_event_list,
    height = height, width = width
  )

  plot_arrows(ppp_events, main_caption, file_path = file_path)
}

#' Plots angles graph of events of a detector or module
#'
#' @param detector Detector object
#' @param file_path Output file path
#' @param row Module row number
#' @param col Module column number
#' @param caption Flag to turn on/off figure caption
#' @param incl_event_list a list of events to be included
#' @export
plot_events_angles <- function(detector, file_path = NA,
                               row = NA, col = NA, caption = TRUE,
                               incl_event_list = NA) {
  main_caption <- ""

  if (!is.na(row) && !is.na(col)) {
    # check whether the row and col numbers are correct
    .check_select(detector, row, col)

    if (caption) {
      main_caption <- paste("Angles of events (row=", row, "col=", col, ")")
    }

    height <- detector$module_row_sizes[row]
    width <- detector$module_col_sizes[col]
  } else {
    if (caption) {
      main_caption <- paste("Angles of events")
    }

    height <- NULL
    width <- NULL
  }

  ppp_events <- .get_clump_event_ppp(detector,
    incl_event_list = incl_event_list,
    height = height, width = width
  )

  plot_angles(ppp_events, main_caption, file_path = file_path)
}

#' Plots K, F, G functions of a detector or module
#'
#' @param detector Detector object
#' @param func Function name
#' @param file_path Output file path
#' @param row Module row number
#' @param col Module column number
#' @param caption Flag to turn on/off figure caption
#' @param incl_event_list a list of events to be included
#' @export
plot_events_kfg <- function(detector, func, file_path = NA,
                            row = NA, col = NA, caption = TRUE,
                            incl_event_list = NA) {
  if (!is.na(row) && !is.na(col)) {
    # check whether the row and col numbers are correct
    .check_select(detector, row, col)

    height <- detector$module_row_sizes[row]
    width <- detector$module_col_sizes[col]
  } else {
    height <- NULL
    width <- NULL
  }

  ppp_events <- .get_clump_event_ppp(detector,
    incl_event_list = incl_event_list,
    height = height, width = width
  )

  plot_kfg(ppp_events, func, file_path = file_path, caption = caption)
}

#' Plots events count per detector or module
#'
#' @param detector Detector object
#' @param file_path Output file path
#' @param row Module row number
#' @param col Module column number
#' @param caption Flag to turn on/off figure caption
#' @param incl_event_list a list of events to be included
#' @export
plot_events_count <- function(detector, file_path = NA,
                              row = NA, col = NA, caption = TRUE,
                              incl_event_list = NA) {
  main_caption <- ""
  if (!caption) par(mar = c(0, 0, 0, 0))

  if (!is.na(row) && !is.na(col)) {
    # check whether the row and col numbers are correct
    .check_select(detector, row, col)


    if (caption) {
      main_caption <- paste(
        "Number of events in a module ",
        detector$dead_stats$module_count_arr[col][row]
      )
    }

    width <- detector$module_col_sizes[col]
    height <- detector$module_row_sizes[row]

    ppp_frame <- spatstat::ppp(1, 1, c(1, width), c(1, height))

    plot(ppp_frame, pch = ".", cex.main = 0.7, main = main_caption)

    # This works only on rectangular detectors!!!
    module_idx <- (col - 1) * detector$module_row_n + row

    ppp_events <- .get_clump_event_ppp(detector,
      incl_event_list = incl_event_list,
      height = height, width = width
    )

    text(width / 2, height / 2, label = length(ppp_events$x))

    if (!is.na(file_path)) {
      dev.off()
    }
  } else {
    ppp_events <- .get_clump_event_ppp(detector, incl_event_list = incl_event_list)

    # count of points in each quadrat
    module_count_arr <- spatstat::quadratcount(
      X = ppp_events,
      nx = detector$module_col_n, ny = detector$module_row_n
    )

    if (caption) {
      main_caption <- "Number of events in modules"
    }

    plot_counts(module_count_arr, file_path = file_path, caption = main_caption)
  }
}
