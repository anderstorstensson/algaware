#' Clamp pie chart centres to lie inside the map panel
#'
#' @param wide Data frame with columns \code{lon}, \code{lat} and optional
#'   \code{r_pie}.
#' @param map_xlim,map_ylim Numeric length-2 vectors bounding the pie centre.
#' @return \code{wide} with clamped \code{lon}/\code{lat}.
#' @keywords internal
clamp_pie_centers <- function(wide,
                              map_xlim = c(10.5, 21.5),
                              map_ylim = c(54.2, 59.8)) {
  if (nrow(wide) == 0) return(wide)

  clamp_axis <- function(value, radius, lo, hi) {
    if (!is.finite(radius) || radius <= 0) {
      return(min(max(value, lo), hi))
    }
    inner_lo <- lo + radius
    inner_hi <- hi - radius
    if (inner_lo > inner_hi) {
      return((lo + hi) / 2)
    }
    min(max(value, inner_lo), inner_hi)
  }

  r_lat <- if ("r_pie" %in% names(wide)) wide$r_pie else rep(0, nrow(wide))
  r_lon <- r_lat / cos(wide$lat * pi / 180)

  wide$lon <- vapply(seq_len(nrow(wide)), function(i) {
    clamp_axis(wide$lon[i], r_lon[i], map_xlim[1], map_xlim[2])
  }, numeric(1L))
  wide$lat <- vapply(seq_len(nrow(wide)), function(i) {
    clamp_axis(wide$lat[i], r_lat[i], map_ylim[1], map_ylim[2])
  }, numeric(1L))
  wide
}

#' Displace pie chart centres away from each other
#'
#' Sequential, asymmetric placement so that pie charts never overlap and any
#' pie that \emph{does} get displaced is pushed far enough that its anchor (true
#' station location) lands outside the pie boundary, so the leader line and
#' anchor dot remain visible. Geometry is computed in an isotropic coordinate
#' system \eqn{(lon \cdot \cos(\bar{lat}), lat)} so that visual distance on a
#' \code{coord_sf} map corresponds approximately to Euclidean distance here.
#'
#' @param wide Data frame with columns \code{lon}, \code{lat} and \code{r_pie}
#'   (the per-station pie radius in latitude degrees). True (anchor)
#'   coordinates are read from \code{lon}/\code{lat}.
#' @param map_xlim,map_ylim Numeric length-2 vectors bounding the pie centre.
#' @param min_sep Minimum centre-to-centre separation between two pies,
#'   expressed as a multiple of the larger of the two radii. Default
#'   \code{2.40} leaves a small visible gap between touching pies.
#' @param min_disp Minimum displacement for a pie that is moved at all,
#'   expressed as a multiple of its radius. Default \code{1.60} guarantees
#'   that the anchor sits clearly outside the displaced pie.
#' @return \code{wide} with added columns \code{anchor_lon}, \code{anchor_lat}
#'   (the true station coordinates) and updated \code{lon}/\code{lat}
#'   holding the displaced pie centres.
#' @keywords internal
repel_pie_centers <- function(wide,
                              map_xlim = c(10.5, 21.5),
                              map_ylim = c(54.2, 59.8),
                              min_sep  = 2.40,
                              min_disp = 1.60) {
  n <- nrow(wide)
  anchor_lon <- wide$lon
  anchor_lat <- wide$lat
  wide$anchor_lon <- anchor_lon
  wide$anchor_lat <- anchor_lat
  if (n < 2) return(wide)

  r_pie  <- wide$r_pie
  cos_ref <- cos(mean(anchor_lat) * pi / 180)
  ax <- anchor_lon * cos_ref
  ay <- anchor_lat
  x_lo <- map_xlim[1] * cos_ref
  x_hi <- map_xlim[2] * cos_ref

  # Pair-wise minimum separation: scale by the LARGER of the two pie radii so
  # that a small pie next to a large one stays outside the large pie.
  pair_min_d <- function(i, j) min_sep * max(r_pie[i], r_pie[j])

  # Constraint score: how many other stations fall within the minimum
  # separation distance of the anchor. Stations with higher scores sit in
  # the middle of a cluster and benefit most from staying at their anchor,
  # so we place them first.
  neighbours <- integer(n)
  for (i in seq_len(n)) {
    neighbours[i] <- sum(vapply(seq_len(n), function(j) {
      if (i == j) return(0L)
      d <- sqrt((ax[i] - ax[j])^2 + (ay[i] - ay[j])^2)
      as.integer(d < pair_min_d(i, j))
    }, integer(1L)))
  }
  # Boundary proximity in pie-radius units.  Stations close to the map edge
  # have nowhere to go, so they get a priority boost that makes interior
  # stations yield to them instead of the reverse.
  boundary_margin <- vapply(seq_len(n), function(i) {
    min(
      (ax[i] - x_lo)        / r_pie[i],
      (x_hi  - ax[i])       / r_pie[i],
      (ay[i] - map_ylim[1]) / r_pie[i],
      (map_ylim[2] - ay[i]) / r_pie[i]
    )
  }, numeric(1L))
  priority  <- neighbours + 1 / pmax(boundary_margin, 0.5)
  order_idx <- order(priority, decreasing = TRUE)

  x <- rep(NA_real_, n)
  y <- rep(NA_real_, n)

  clamp_xy <- function(i, xi, yi) {
    r_x <- r_pie[i] * cos_ref / cos(anchor_lat[i] * pi / 180)
    r_y <- r_pie[i]
    x_min <- x_lo + r_x
    x_max <- x_hi - r_x
    y_min <- map_ylim[1] + r_y
    y_max <- map_ylim[2] - r_y

    if (x_min > x_max) {
      xi <- (x_lo + x_hi) / 2
    } else {
      xi <- min(max(xi, x_min), x_max)
    }
    if (y_min > y_max) {
      yi <- mean(map_ylim)
    } else {
      yi <- min(max(yi, y_min), y_max)
    }
    c(xi, yi)
  }

  enforce_min_disp <- function(xi, yi, axi, ayi, min_disp_d) {
    dx <- xi - axi
    dy <- yi - ayi
    d  <- sqrt(dx * dx + dy * dy)
    if (d > 1e-6 && d < min_disp_d) {
      s  <- min_disp_d / d
      xi <- axi + dx * s
      yi <- ayi + dy * s
    }
    c(xi, yi)
  }

  for (rank in seq_along(order_idx)) {
    i  <- order_idx[rank]
    xi <- ax[i]
    yi <- ay[i]
    min_disp_d_i <- min_disp * r_pie[i]

    # Resolve collisions with already-placed pies. Iterate because pushing
    # away from one neighbour can bring the pie into contact with another.
    for (iter in seq_len(80L)) {
      moved <- FALSE
      for (k in seq_len(rank - 1L)) {
        j  <- order_idx[k]
        dx <- xi - x[j]
        dy <- yi - y[j]
        d  <- sqrt(dx * dx + dy * dy)
        min_d_ij <- pair_min_d(i, j)
        if (d < 1e-6) {
          dx <- ax[i] - x[j]
          dy <- ay[i] - y[j]
          d  <- sqrt(dx * dx + dy * dy)
          if (d < 1e-6) { dx <- 1; dy <- 0; d <- 1 }
        }
        if (d < min_d_ij) {
          ux <- dx / d
          uy <- dy / d
          push <- (min_d_ij - d) + 1e-4
          xi <- xi + ux * push
          yi <- yi + uy * push
          moved <- TRUE
        }
      }
      if (!moved) break
    }

    # If the station was displaced at all, push it far enough that the
    # anchor ends up outside the pie. If that new position collides with a
    # placed pie, resolve the conflict by pushing away from the offender
    # while holding the minimum-displacement constraint.
    xy <- enforce_min_disp(xi, yi, ax[i], ay[i], min_disp_d_i)
    xi <- xy[1L]; yi <- xy[2L]
    # Clamp immediately so the second resolution loop never starts outside bounds.
    xy <- clamp_xy(i, xi, yi)
    xi <- xy[1L]; yi <- xy[2L]

    for (iter in seq_len(40L)) {
      moved <- FALSE
      for (k in seq_len(rank - 1L)) {
        j  <- order_idx[k]
        dx <- xi - x[j]
        dy <- yi - y[j]
        d  <- sqrt(dx * dx + dy * dy)
        min_d_ij <- pair_min_d(i, j)
        if (d < min_d_ij) {
          ux <- dx / max(d, 1e-6)
          uy <- dy / max(d, 1e-6)
          push <- (min_d_ij - d) + 1e-4
          xi <- xi + ux * push
          yi <- yi + uy * push
          moved <- TRUE
        }
      }
      xy <- clamp_xy(i, xi, yi)
      xi <- xy[1L]; yi <- xy[2L]
      if (!moved) break
    }

    x[i] <- xi
    y[i] <- yi
  }

  wide$lon <- x / cos_ref
  wide$lat <- y
  wide
}

#' Find label positions that avoid all pie charts and each other
#'
#' Uses a greedy sequential algorithm: stations are processed most-constrained
#' first (most pie neighbours), and each label is placed in the direction that
#' maximises clearance from all pie circles *and* from already-placed label
#' bounding boxes.
#'
#' @param wide Data frame with one row per station, columns
#'   \code{lon}, \code{lat}, \code{r_pie}, \code{r_lon}, and \code{label}.
#' @param map_xlim,map_ylim Numeric length-2 vectors giving the allowed label
#'   anchor range (a conservative inset of the full map extent).
#' @param n_angles Number of candidate directions to test per station.
#'   Default \code{48}.
#' @param char_w Estimated label width per character in longitude degrees.
#' @param char_h Estimated label half-height in latitude degrees.
#' @param radius_mults Candidate radial distances in units of pie radius.
#' @return \code{wide} with columns \code{label_x}, \code{label_y},
#'   \code{hjust}, and \code{vjust} appended.
#' @keywords internal
place_pie_labels <- function(wide,
                             map_xlim = c(10.8, 21.2),
                             map_ylim = c(54.4, 59.6),
                             n_angles = 48,
                             char_w   = 0.055,
                             char_h   = 0.055,
                             radius_mults = c(1.08, 1.25, 1.55, 1.9,
                                              2.4, 3.0, 3.8, 4.8)) {
  angles <- seq(0, 2 * pi, length.out = n_angles + 1L)[-(n_angles + 1L)]
  lons   <- wide$lon
  lats   <- wide$lat
  r_lats <- wide$r_pie
  r_lons <- wide$r_lon
  labels <- wide$label
  n      <- nrow(wide)
  anchor_lon <- if ("anchor_lon" %in% names(wide)) wide$anchor_lon else lons
  anchor_lat <- if ("anchor_lat" %in% names(wide)) wide$anchor_lat else lats
  leader_x <- if ("leader_x" %in% names(wide)) wide$leader_x else lons
  leader_y <- if ("leader_y" %in% names(wide)) wide$leader_y else lats

  # Process most-constrained (most close neighbours) first
  neighbour_count <- vapply(seq_len(n), function(i) {
    sum(vapply(seq_len(n), function(j) {
      if (i == j) return(0L)
      dx <- (lons[i] - lons[j]) / r_lons[j]
      dy <- (lats[i] - lats[j]) / r_lats[j]
      as.integer(sqrt(dx^2 + dy^2) < 5)
    }, integer(1L)))
  }, integer(1L))
  order_idx <- order(neighbour_count, decreasing = TRUE)

  label_x  <- numeric(n)
  label_y  <- numeric(n)
  # Stored bounding boxes of placed labels: list of c(cx, cy, hw, hh)
  placed   <- vector("list", n)

  point_in_rect <- function(px, py, left, right, bottom, top) {
    px >= left && px <= right && py >= bottom && py <= top
  }

  segments_intersect <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
    orient <- function(ax, ay, bx, by, cx, cy) {
      (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
    }
    on_segment <- function(ax, ay, bx, by, cx, cy) {
      min(ax, bx) <= cx && cx <= max(ax, bx) &&
        min(ay, by) <= cy && cy <= max(ay, by)
    }

    o1 <- orient(x1, y1, x2, y2, x3, y3)
    o2 <- orient(x1, y1, x2, y2, x4, y4)
    o3 <- orient(x3, y3, x4, y4, x1, y1)
    o4 <- orient(x3, y3, x4, y4, x2, y2)

    if (((o1 > 0 && o2 < 0) || (o1 < 0 && o2 > 0)) &&
        ((o3 > 0 && o4 < 0) || (o3 < 0 && o4 > 0))) {
      return(TRUE)
    }

    tol <- 1e-10
    if (abs(o1) <= tol && on_segment(x1, y1, x2, y2, x3, y3)) return(TRUE)
    if (abs(o2) <= tol && on_segment(x1, y1, x2, y2, x4, y4)) return(TRUE)
    if (abs(o3) <= tol && on_segment(x3, y3, x4, y4, x1, y1)) return(TRUE)
    if (abs(o4) <= tol && on_segment(x3, y3, x4, y4, x2, y2)) return(TRUE)
    FALSE
  }

  segment_hits_rect <- function(x1, y1, x2, y2, left, right, bottom, top) {
    if (point_in_rect(x1, y1, left, right, bottom, top) ||
        point_in_rect(x2, y2, left, right, bottom, top)) {
      return(TRUE)
    }
    if (max(x1, x2) < left || min(x1, x2) > right ||
        max(y1, y2) < bottom || min(y1, y2) > top) {
      return(FALSE)
    }
    edges <- rbind(
      c(left, bottom, right, bottom),
      c(right, bottom, right, top),
      c(right, top, left, top),
      c(left, top, left, bottom)
    )
    any(vapply(seq_len(nrow(edges)), function(k) {
      segments_intersect(x1, y1, x2, y2,
                         edges[k, 1], edges[k, 2], edges[k, 3], edges[k, 4])
    }, logical(1L)))
  }

  for (rank in seq_along(order_idx)) {
    i      <- order_idx[rank]
    hw_lon <- nchar(labels[i]) * char_w / 2   # half label width (lon degrees)

    best_score    <- -Inf
    best_lx       <- NA_real_
    best_ly       <- NA_real_
    best_center_x <- NA_real_

    for (theta in angles) { for (radius_mult in radius_mults) {
      lx <- lons[i] + r_lons[i] * sin(theta) * radius_mult
      ly <- lats[i] + r_lats[i] * cos(theta) * radius_mult

      # Compute the actual rendered text extent based on the implicit hjust
      # that will be assigned once the best position is chosen:
      #   sin(theta) > 0  →  label right of pie  →  hjust = 0 (left-aligned)
      #   sin(theta) < 0  →  label left of pie   →  hjust = 1 (right-aligned)
      #   sin(theta) ≈ 0  →  label above/below   →  hjust = 0.5 (centred)
      s_theta <- sin(theta)
      left   <- if (s_theta >  1e-6) lx              else
                if (s_theta < -1e-6) lx - 2 * hw_lon else lx - hw_lon
      right  <- if (s_theta >  1e-6) lx + 2 * hw_lon else
                if (s_theta < -1e-6) lx              else lx + hw_lon
      center_x <- (left + right) / 2
      bottom <- ly - char_h
      top    <- ly + char_h

      if (left < map_xlim[1] || right > map_xlim[2] ||
          bottom < map_ylim[1] || top > map_ylim[2]) next

      # Pie clearance: minimum distance from any pie EDGE to the CLOSEST
      # point on the label bounding box. Checking the full bbox (not just
      # the anchor) prevents a long label from intruding into a neighbour
      # pie even when the anchor itself sits in clear space.
      pie_score <- min(vapply(seq_len(n), function(j) {
        r_lon_j <- r_lons[j]
        r_lat_j <- r_lats[j]
        cx_clamp <- min(max(lons[j], left), right)
        cy_clamp <- min(max(lats[j], bottom), top)
        dx <- (cx_clamp - lons[j]) / r_lon_j
        dy <- (cy_clamp - lats[j]) / r_lat_j
        sqrt(dx^2 + dy^2) - 1.0
      }, numeric(1L)))

      # Label clearance: penalise overlap with already-placed bboxes.
      # Stored bboxes use centre_x so the gap calculation is symmetric.
      label_score <- if (rank <= 1L) Inf else
        min(vapply(seq_len(rank - 1L), function(k) {
          b <- placed[[order_idx[k]]]
          if (is.null(b)) return(Inf)
          # Gap between bounding boxes (negative = overlap)
          gap_x <- abs(center_x - b[1L]) - (hw_lon + b[3L])
          gap_y <- abs(ly       - b[2L]) - (char_h  + b[4L])
          min(gap_x, gap_y)          # negative when boxes overlap
        }, numeric(1L)))

      # Two-tier scoring so the label hugs the pie when possible:
      #   * If the candidate is fully clear (no pie intrusion, no label
      #     overlap), score primarily by closeness, but break ties using
      #     actual clearance and a mild preference for side placements.
      #   * Otherwise, fall back to maximising clearance.
      is_clear <- pie_score > 0.02 && label_score >= 0
      segment_penalty <- 0
      if (is_clear) {
        anchor_ok <- !any(vapply(seq_len(n), function(j) {
          point_in_rect(anchor_lon[j], anchor_lat[j], left, right, bottom, top)
        }, logical(1L)))
        segment_penalty <- sum(vapply(seq_len(n), function(j) {
          as.integer(segment_hits_rect(anchor_lon[j], anchor_lat[j],
                                       leader_x[j], leader_y[j],
                                       left, right, bottom, top))
        }, integer(1L)))
        is_clear <- anchor_ok
      }
      if (!is_clear) next
      score <- if (is_clear) {
        side_pref <- abs(s_theta)
        100 - radius_mult * 10 + pmin(pie_score, 3) +
          side_pref * 0.25 - segment_penalty * 1.5
      } else {
        pie_score + label_score * 3 - radius_mult * 0.1
      }

      if (score > best_score) {
        best_score    <- score
        best_lx       <- lx
        best_ly       <- ly
        best_center_x <- center_x
      }
    } }

    label_x[i] <- best_lx
    label_y[i] <- best_ly
    if (is.finite(best_lx) && is.finite(best_ly)) {
      placed[[i]] <- c(best_center_x, best_ly, hw_lon, char_h)
    }
  }

  wide$label_x <- label_x
  wide$label_y <- label_y
  adx          <- wide$label_x - lons
  ady          <- wide$label_y - lats
  wide$hjust   <- (1 - sign(adx)) / 2
  wide$vjust   <- (1 - sign(ady)) / 2
  wide
}

#' Build pie-chart polygon data for use with geom_polygon
#'
#' Computes arc segments for each group slice at each station. The longitude
#' radius is stretched by \code{1/cos(lat)} so that slices appear circular
#' under \code{coord_sf} (which equalises physical distances on screen).
#'
#' @param wide_data Data frame with one row per station, columns
#'   \code{lon}, \code{lat}, \code{r_pie}, and one numeric column per group.
#' @param group_cols Character vector of column names holding group values.
#' @param n_arc Number of arc points per full circle. Default \code{80}.
#' @return A data frame with columns \code{x}, \code{y}, \code{slice_id},
#'   and \code{group} suitable for \code{geom_polygon}.
#' @keywords internal
build_pie_polygons <- function(wide_data, group_cols, n_arc = 80) {
  pieces <- vector("list", nrow(wide_data) * length(group_cols))
  k <- 0L
  for (i in seq_len(nrow(wide_data))) {
    lon    <- wide_data$lon[i]
    lat    <- wide_data$lat[i]
    r_lat  <- wide_data$r_pie[i]
    r_lon  <- r_lat / cos(lat * pi / 180)
    values <- unlist(wide_data[i, group_cols])
    total  <- sum(values, na.rm = TRUE)
    if (!is.finite(total) || total <= 0) next
    props  <- values / total
    ends   <- cumsum(props) * 2 * pi
    starts <- c(0, utils::head(ends, -1))
    for (j in seq_along(group_cols)) {
      if (props[j] <= 0) next
      k <- k + 1L
      n_pts <- max(3L, ceiling(n_arc * props[j]))
      theta  <- seq(starts[j], ends[j], length.out = n_pts)
      pieces[[k]] <- data.frame(
        x        = c(lon, lon + r_lon * sin(theta), lon),
        y        = c(lat, lat + r_lat * cos(theta), lat),
        slice_id = paste0(i, "_", j),
        group    = group_cols[j],
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, pieces[seq_len(k)])
}

#' Create a phytoplankton group composition map
#'
#' Thin AlgAware-specific wrapper around \code{\link{create_pie_map}}. Draws
#' a pie chart at each station showing the relative carbon biomass
#' contributed by Diatoms, Dinoflagellates, Cyanobacteria, Cryptophytes,
#' Mesodinium spp., Silicoflagellates, and Other.
#'
#' @param station_summary Aggregated station data from
#'   \code{aggregate_station_data()}, containing columns \code{name},
#'   \code{AphiaID}, \code{carbon_ug_per_liter},
#'   \code{STATION_NAME_SHORT}, \code{LATITUDE_WGS84_SWEREF99_DD}, and
#'   \code{LONGITUDE_WGS84_SWEREF99_DD}.
#' @param phyto_groups Data frame with columns \code{name}, \code{AphiaID},
#'   and \code{phyto_group} as returned by
#'   \code{SHARK4R::assign_phytoplankton_group()}.
#' @param r_lat Pie chart radius in latitude degrees (default \code{0.28}).
#' @return A ggplot object.
#' @export
create_group_map <- function(station_summary, phyto_groups, r_lat = 0.28) {
  group_levels <- c("Diatoms", "Dinoflagellates", "Cyanobacteria",
                    "Cryptophytes", "Mesodinium spp.", "Silicoflagellates",
                    "Other")
  group_colors <- c(
    Diatoms           = "#4A90D9",
    Dinoflagellates   = "#E74C3C",
    Cyanobacteria     = "#14B8A6",
    Cryptophytes      = "#9B59B6",
    `Mesodinium spp.` = "#F1C40F",
    Silicoflagellates = "#E67E22",
    Other             = "#95A5A6"
  )
  group_labels <- c(
    Diatoms           = "Diatoms",
    Dinoflagellates   = "Dinoflagellates",
    Cyanobacteria     = "Cyanobacteria",
    Cryptophytes      = "Cryptophytes",
    `Mesodinium spp.` = "<i>Mesodinium</i> spp.",
    Silicoflagellates = "Silicoflagellates",
    Other             = "Other"
  )

  # Merge group assignments; unmatched taxa fall into "Other".
  merged <- merge(
    station_summary,
    phyto_groups[, c("name", "AphiaID", "phyto_group.plankton_group")],
    by    = c("name", "AphiaID"),
    all.x = TRUE
  )
  names(merged)[names(merged) == "phyto_group.plankton_group"] <- "phyto_group"
  merged$phyto_group[is.na(merged$phyto_group)] <- "Other"

  # Aggregate carbon biomass per station + group into the long format
  # expected by create_pie_map().
  long <- stats::aggregate(
    carbon_ug_per_liter ~
      STATION_NAME_SHORT + LATITUDE_WGS84_SWEREF99_DD +
      LONGITUDE_WGS84_SWEREF99_DD + phyto_group,
    data  = merged,
    FUN   = sum,
    na.rm = TRUE
  )
  names(long) <- c("station", "lat", "lon", "group", "value")

  create_pie_map(
    long,
    station_col  = "station",
    lon_col      = "lon",
    lat_col      = "lat",
    group_levels = group_levels,
    group_colors = group_colors,
    group_labels = group_labels,
    radius       = r_lat,
    size_by      = "total",
    xlim         = c(10, 22),
    ylim         = c(54, 60),
    title        = NULL,
    legend_title = "Taxon group"
  )
}
