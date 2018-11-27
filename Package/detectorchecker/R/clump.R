library(igraph)

#' Clasifies a clump
#'
#' @param x vector containing the x coordinates of a clump
#' @param y vector containing the y coordinates of a clump
#' @return the class of a clump (1 - singleton, 2 - double, 3 - triplet,
#'   4 - larger cluster, unless it actually has the shape of a line,
#'   5 (6): vertical line where closest edge is the upper (lower) one,
#'   7 (8): horizontal line where closest edge is the right (left) one)
.classify_clump <- function(layout, x, y) {

  # x and y are vectors of the same length
  # x=columns=horizontale positions
  # y=rows=verticale positions

  # Note that size of clump is given by length(x) (=length(y)).
  # range(x) length shows expansion in x-direction, range(y) expansion in y-direction.

  nr <- layout$detector_height
  nc <- layout$detector_width

  # a priori assignment for class label is 0, which means nothing and is used
  #   later to check if class label assignment failed
  clc <- 0

  # type 1: singleton
  if (length(x) == 1) clc <- 1
  # type 2: double
  else if (length(x) == 2) clc <- 2
  # type 3: triplet (L-shapes or line of 3, could be singleton + 1d infected nn)
  else if (length(x) == 3) clc <- 3
  # type 4: larger cluster (a priori assignment) with lines as special cases
  #  reassigned below to other class labels
  else if (length(x) >= 4) clc <- 4
  # type 0: Error
  else stop("Clump's coordintes length cannot be zero.")

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
      if (nr - max(y) + 1 < min(y)) clc <- 5
      # type 6: closest to lower edge
      else clc <- 6
    }
  }

  if (xrange > 4 & yrange < 4) {
    # x-coo of the potential line can be determined by mode(x)
    yline <- .getmode(y)

    # horizontal line (with up to 10% nn allowed, apart from 2 for the cluster at the end)
    if (length(y) - 2 < length(y == yline) * 1.1) {
      # type 7: nearest to right side
      if (nc - max(x) + 1 < min(x)) clc <- 7
      # type 8: nearest to left side
      else clc <- 8
    }
  }

  return(clc)
#
#   if (clc==0){cat("Error in class assignment: no class could be identified for clump ", i,
#                   ". Double check image. Trace where that clump is. Enumeration goes from
#                   top left to bottom right. However, original image may have been transposed
#                   and flipped.")}
}

#' Something something dark side
#'
#' @param layout Layout object
#' @return data frame
.xyc_ply_func <- function(layout, xyc_pixel_df) {

  dataFrame <- plyr::ddply(xyc_pixel_df, "id",
    dplyr::summarise,                     # 1
    class = .classify_clump(layout, x, y), # 2
    size = length(x),                     # 3
    xct = min(x) + (max(x) - min(x)) / 2, # 4 not always a pixel (see below)
    yct = min(y) + (max(y) - min(y)) / 2, # 5 not always a pixel
    xctpix = round(median(x)),            # 6 better than above, avoids centre pulled by hairs sticking out
    yctpix = round(median(y)),            # 7 dito
    xmin = min(x),                        # 8
    xmax = max(x),                        # 9
    xrange = max(x) - min(x) + 1,         #10
    ymin = min(y),                        #11
    ymax = max(y),                        #12
    yrange = max(y) - min(y) + 1,         #13
    xmode = .getmode(x),                  #14
    ymode = .getmode(y))                  #15

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

#' MODIFYING CLUSTERS TO EVENTS (CONSISTING OF 1 PIXEL REPRESENTING THE CLUSTER)
#' Make into a point pattern of just events rather than pixels. Using xyc_ply object.
#' Collapse in one point using centres for clusters, but end points for lines, type dependend:
#' type 5 (closest to upper edge): ymin
#' type 6 (closest to lower edge): ymax
#' type 7 (closest to right edge): xmin
#' type 8 (closest to left edge):  xmax
#' This is inspired by Perkin Elmer Layout and be replaced by other choices if desired.
#' @param xyc_ply clums data frame
#' @return events
.xyc_pixels2events <- function(xyc_ply) {

  xyc_events <- xyc_ply[, c(6, 7, 1, 2, 3)]

  # This defines the default for representing a cluster by centres xctpix, yctpix.
  # For lines, this is replaced by endpoints (choice how explained above).
  for (i in 1:dim(xyc_events)[1]) {

    if (xyc_events[i, 4] == 5) {
      xyc_events[i, 1] <- xyc_ply[i, 14]   # xmode
      xyc_events[i, 2] <- xyc_ply[i, 12]   # ymin
    }

    if (xyc_events[i, 4] == 6) {
      xyc_events[i, 1] <- xyc_ply[i, 14]   # xmode
      xyc_events[i, 2] <- xyc_ply[i, 11]   # ymax
    }

    if (xyc_events[i, 4] == 7) {
      xyc_events[i, 1] <- xyc_ply[i, 9]    # ymode
      xyc_events[i, 2] <- xyc_ply[i, 15]   # xmin
    }

    if (xyc_events[i, 4] == 8) {
      xyc_events[i, 1] <- xyc_ply[i, 8]    # ymode
      xyc_events[i, 2] <- xyc_ply[i, 15]   # xmax
    }
  }

  return(xyc_events)
}

#' Converts mask (dead pixels) to events
#'
#' @param layout Layout object
#' @param dead_pix_mask Dead pixels mask
#' @return list of pixels and events
.mask_to_events <- function(layout, dead_pix_mask) {

  nr <- layout$detector_height
  nc <- layout$detector_width

  # Raster object preparation:
  # Detect & classify into lines, clusters, duplicates etc using functions above
  # May do this approximately to save time!!! Really!!!!!
  # identify & group all adjacent pixels:
  # extract clusters of pixels (that are not NA)
  rr <- raster::raster(nrow = nr, ncol = nc, xmn = 0, xmx = nc, ymn = 0, ymx = nr)

  # need to both transpose matrix and flip about horizontal axis
  raster::values(rr) <- t(dead_pix_mask[ , c(nr:1)])

  # detect clumps (patches) of connected cells, directions = 4 (Rook's case)
  rrc <- suppressWarnings(raster::clump(rr, directions = 4))

  # Make data frame with all pixels part of clusters (clumps) and their clump
  #  ID and centre's coordinates. Note we use ceiling, because xyc would have
  #  coordinates converted from integers to .5 ending
  xyc_df <- data.frame(
    ceiling(raster::xyFromCell(rrc, which(!is.na(raster::getValues(rrc))))),
    id = raster::getValues(rrc)[!is.na(raster::getValues(rrc))])

  xyc_events <- .xyc_pixels2events(.xyc_ply_func(layout, xyc_df))

  return(list(pixels = xyc_df, events = xyc_events))
}

#' Locates and clusifies clumps of a damaged layout
#'
#' @param layout Layout object
#' @export
find_clumps <- function(layout) {

  pixel_mask <- get_dead_pix_mask(layout)

  pixel_events <- .mask_to_events(layout, pixel_mask)

  layout$clumps <- list(pixels = pixel_events$pixels,
                        events = pixel_events$events)

  return(layout)
}

#' Plots damaged layout events
#' @param layout Layout object
#' @param file_path Output file path
#' @param caption Flag to turn on/off figure caption
#' @param incl_event_list a list of events to be included
#' @export
plot_events <- function(layout, file_path = NA, caption = TRUE, incl_event_list = NA) {

  if (!caption) {
    main_caption <- ""
    par(mar = c(0, 0, 0, 0))

  } else {
    main_caption <- "Defective events"
    par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))
  }

  if(!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  ppp_events <- .get_clump_event_ppp(layout, incl_event_list = incl_event_list)

  # "Defective events"
  plot(ppp_events, pch = 22, main = main_caption)

  # plot(ppp_events, pch=22, col=2, main="Defective events") doesn't work, hense, cheat:
  points(ppp_events, pch = 22, col = 2)

  if(!is.na(file_path)) {
    dev.off()
  }
}

#' Creates ppp for damaged layout events
#' @param layout Layout object
#' @param incl_event_list a list of events to be included
.get_clump_event_ppp <- function(layout, incl_event_list = NA) {

  nr <- layout$detector_height
  nc <- layout$detector_width

  if (suppressWarnings(is.list(incl_event_list))) {
    events <- layout$clumps$events[layout$clumps$events$class %in% incl_event_list, ]

  } else if (suppressWarnings(!is.na(incl_event_list))) {
    events <- layout$clumps$events[layout$clumps$events$class == incl_event_list, ]

  } else {
    events <- layout$clumps$event
  }

  event_ppp <- spatstat::ppp(events[, 1], events[, 2], c(1, nc), c(1, nr))

  return(event_ppp)
}

#' Creates ppp for damaged layout pixels
#' @param layout Layout object
#' @param incl_event_list a list of events to be included
.get_clump_pixel_ppp <- function(layout, incl_event_list = NA) {

  nr <- layout$detector_height
  nc <- layout$detector_width

  if (suppressWarnings(is.list(incl_event_list))) {
    pixels <- layout$clumps$pixels[layout$clumps$pixels$id %in% incl_event_list, ]

  } else if (suppressWarnings(!is.na(incl_event_list))) {
    pixels <- layout$clumps$pixels[layout$clumps$pixels$id == incl_event_list, ]

  } else {
    pixels <- layout$clumps$pixels
  }

  pixel_ppp <- spatstat::ppp(pixels[, 1], pixels[, 2], c(1, nc), c(1, nr))

  return(pixel_ppp)
}

#' Plots damaged layout pixels and events
#' @param layout Layout object
#' @param file_path Output file path
#' @param caption Flag to turn on/off figure caption
#' @param incl_event_list a list of events to be included
#' @export
plot_pixels_events <- function(layout, file_path = NA, caption = TRUE, incl_event_list = NA) {

  if (!caption) {
    main_caption <- ""
    par(mar = c(0, 0, 0, 0))

  } else {
    main_caption <- "Defective pixels (black) and events (red)"
    par(mfrow = c(1, 1), mar = c(0, 0, 4, 0) + 0.1, oma = c(0, 0, 0, 0))
  }

  if(!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  ppp_pixels <- .get_clump_pixel_ppp(layout, incl_event_list = incl_event_list)
  ppp_events <- .get_clump_event_ppp(layout, incl_event_list = incl_event_list)

  # Defective pixels
  plot(ppp_pixels, pch = 22, main=main_caption)

  # Defective events
  # plot(ppp_events, pch=22, col=2, main="Defective events") doesn't work, hense, cheat:
  points(ppp_events, pch = 22, col = 2)

  if(!is.na(file_path)) {
    dev.off()
  }
}

#' Plots density graph of events
#'
#' @param layout Layout object
#' @param file_path Output file path
#' @param adjust Kernel density bandwidth
#' @param caption Flag to turn on/off figure caption
#' @param incl_event_list a list of events to be included
#' @export
plot_events_density <- function(layout, file_path = NA, adjust = 0.25,
                                row = NA, col = NA, caption = TRUE,
                                incl_event_list = NA) {

  if (!is.na(row) && !is.na(col)) {
    # check whether the row and col numbers are correct
    .check_select(layout, row, col)

    if (caption) {
      main_caption <- paste("Events density (row=", row, "col=", col, "), adjust=", adjust)
    }

    # get the ppp for the selected module
    # ppp_dead <- .get_ppp_dead_module(layout, row, col)
    stop("Not implemented yet")

  } else {
    if (caption) {
      main_caption <- paste("Dead pixel density, adjust = ", adjust)
    }

    ppp_events <- .get_clump_event_ppp(layout, incl_event_list = incl_event_list)
  }

  plot_density(ppp_events, main_caption, file_path = file_path, adjust = adjust)
}
