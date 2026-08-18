# English month abbreviations used throughout CTD plots (locale-independent)
.MONTH_ABBR_EN <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

#' Remove duplicate CTD casts for the same station/date
#'
#' When a single station visit produces several CNV files (upcast + downcast,
#' or multiple instruments), this function keeps only the most representative
#' cast: upcasts (filenames beginning with a lowercase \code{u} followed by an
#' uppercase letter) are dropped first, then if multiple downcasts remain for
#' the same date the deepest is kept (ties broken alphabetically by filename).
#'
#' @param ctd_data Data frame with at least columns \code{file_path},
#'   \code{pressure_dbar}, and \code{sample_date}.
#' @return Filtered data frame.
#' @keywords internal
deduplicate_casts <- function(ctd_data) {
  if (is.null(ctd_data) || nrow(ctd_data) == 0) return(ctd_data)

  # Drop upcasts (conventional prefix: lower-case 'u' + upper-case letter)
  is_up <- grepl("^u[A-Z]", basename(ctd_data$file_path))
  if (any(!is_up)) ctd_data <- ctd_data[!is_up, ]

  files <- unique(ctd_data$file_path)
  if (length(files) <= 1L) return(ctd_data)

  # Per file: max depth and its sample_date
  depth_per_file <- vapply(files, function(f) {
    p <- ctd_data$pressure_dbar[ctd_data$file_path == f]
    p <- p[!is.na(p)]
    if (length(p) == 0) -Inf else max(p)
  }, numeric(1L))
  date_per_file <- vapply(files, function(f) {
    as.character(ctd_data$sample_date[ctd_data$file_path == f][1L])
  }, character(1L))

  meta <- data.frame(file_path = files, max_depth = depth_per_file,
                     sample_date = date_per_file, stringsAsFactors = FALSE)

  # Per date: keep file with greatest depth, ties broken by filename order.
  # A file whose sample_date is NA (CNV without a parsable time) forms its
  # own group keyed by file path: split() silently drops NA groups, which
  # used to discard every such cast and make the station vanish from the
  # CTD tab and report without warning.
  grp <- ifelse(is.na(meta$sample_date),
                paste0("na_date_", meta$file_path),
                meta$sample_date)
  keep <- unlist(lapply(split(meta, grp), function(d) {
    d <- d[order(-d$max_depth, d$file_path), ]
    d$file_path[1L]
  }))

  ctd_data[ctd_data$file_path %in% keep, ]
}

#' Create a CTD fluorescence profile plot for one station
#'
#' Plots all casts at the station as separate overlaid paths (one per CNV
#' file).  Depth is clamped to 0–50 m.
#'
#' @param station_ctd Data frame for one station with columns
#'   \code{chl_fluorescence}, \code{pressure_dbar}, and \code{file_path}.
#' @param station_label Character label for the station.
#' @param xlim Numeric length-2 vector for a shared x-axis limit, or NULL.
#' @param date_label Character subtitle (sample dates), or NULL.
#' @param show_x_axis Logical; show x-axis tick labels and title on this panel.
#' @param lims_points Optional data frame with columns \code{DEPH} and
#'   \code{CPHL} for discrete bottle chlorophyll measurements from the current
#'   cruise (depth \eqn{\le 50}{<= 50} m).  Points are overlaid on the profile.
#' @return A ggplot object.
#' @keywords internal
create_fluorescence_profile <- function(station_ctd, station_label,
                                        xlim = NULL, date_label = NULL,
                                        show_x_axis = TRUE,
                                        lims_points = NULL) {
  # Clamp depth to 0-50 m
  station_ctd <- station_ctd[
    !is.na(station_ctd$pressure_dbar) & station_ctd$pressure_dbar <= 50, ]

  has_lims <- !is.null(lims_points) && nrow(lims_points) > 0 &&
    any(!is.na(lims_points$CPHL))

  color_values <- c("CTD fluorescence\n(nominally \u00b5g/L)" = "#2ca02c")
  if (has_lims) {
    color_values <- c(color_values,
                      "Chl-a bottle\n(0\u201350 m)" = "#d62728")
  }

  p <- ggplot2::ggplot(station_ctd,
                       ggplot2::aes(x = .data$chl_fluorescence,
                                    y = .data$pressure_dbar,
                                    group = .data$file_path,
                                    color = "CTD fluorescence\n(nominally \u00b5g/L)")) +
    ggplot2::geom_path(linewidth = 0.8, alpha = 0.85) +
    ggplot2::scale_y_reverse(limits = c(50, 0)) +
    ggplot2::scale_color_manual(
      name = NULL,
      values = color_values
    ) +
    ggplot2::labs(
      x = "Chl fluorescence / Chl-a (\u00b5g/L)",
      y = "Depth (m)",
      title = station_label,
      subtitle = date_label
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 10, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 7, color = "gray40"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.title  = ggplot2::element_blank(),
      legend.position = if (has_lims) "bottom" else "none"
    )

  if (has_lims) {
    pts <- lims_points[!is.na(lims_points$CPHL) & !is.na(lims_points$DEPH), ]
    p <- p +
      ggplot2::geom_point(
        data = pts,
        ggplot2::aes(x = .data$CPHL, y = .data$DEPH,
                     color = "Chl-a bottle\n(0\u201350 m)"),
        size = 2, inherit.aes = FALSE
      )
  }

  if (!is.null(xlim)) {
    p <- p + ggplot2::scale_x_continuous(limits = xlim)
  }

  p <- p + ggplot2::theme(
    panel.border = ggplot2::element_rect(color = "black", fill = NA,
                                         linewidth = 0.5)
  )

  if (!show_x_axis) {
    p <- p + ggplot2::theme(
      axis.text.x  = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank()
    )
  }
  p
}

#' Create a Chl-a time series with optional historical statistics
#'
#' Plots smooth spline curves through 12 historical monthly means and the
#' std-dev ribbon (when available) plus current-year bottle observations.
#' The spline uses a cyclic boundary extension so the curve spans the full
#' calendar year.  Current-year observations are shown as points only.
#'
#' @param station_lims Data frame of LIMS data for one station
#'   (depth-filtered to \eqn{\le 20}{<= 20} m).  May be NULL.
#' @param station_stats Data frame of historical statistics (12 months) from
#'   \code{load_chl_statistics()}.  May be NULL.
#' @param station_label Character label.
#' @param current_year Integer year.
#' @param show_x_axis Logical; render month labels on the x-axis.
#' @return A ggplot object, or NULL if there is nothing to plot.
#' @keywords internal
create_chl_timeseries <- function(station_lims, station_stats,
                                  station_label, current_year,
                                  show_x_axis = TRUE) {
  has_stats <- !is.null(station_stats) && nrow(station_stats) > 0 &&
               any(!is.na(station_stats$CHLA_mean))
  has_lims  <- !is.null(station_lims) && nrow(station_lims) > 0

  if (!has_stats && !has_lims) return(NULL)

  p <- ggplot2::ggplot()
  ribbon_added  <- FALSE

  if (has_stats) {
    stats_df <- station_stats[!is.na(station_stats$CHLA_mean), ]
    stats_df$std_low <- pmax(0, stats_df$CHLA_mean - stats_df$CHLA_std)
    stats_df$std_hi  <- stats_df$CHLA_mean + stats_df$CHLA_std
    # Place monthly means at mid-month
    stats_df$date <- as.Date(paste0(current_year, "-", stats_df$MONTH, "-15"))
    stats_df <- stats_df[order(stats_df$date), ]

    if (nrow(stats_df) >= 3L) {
      # Cyclic boundary extension: prepend Dec of prev year, append Jan of
      # next year so the spline spans the full Jan 1 – Dec 31 range.
      dec_row <- stats_df[stats_df$MONTH == max(stats_df$MONTH), ]
      jan_row <- stats_df[stats_df$MONTH == min(stats_df$MONTH), ]
      dec_row$date <- as.Date(paste0(current_year - 1L, "-12-15"))
      jan_row$date <- as.Date(paste0(current_year + 1L, "-01-15"))

      ext <- rbind(dec_row, stats_df, jan_row)
      ext <- ext[order(ext$date), ]
      date_num_ext <- as.numeric(ext$date)

      year_start <- as.Date(paste0(current_year, "-01-01"))
      year_end   <- as.Date(paste0(current_year, "-12-31"))

      clip_spl <- function(spl) {
        df <- data.frame(
          date = as.Date(spl$x, origin = "1970-01-01"),
          y    = spl$y
        )
        df[df$date >= year_start & df$date <= year_end, ]
      }

      n_spl <- 300L
      spl_mean   <- stats::spline(date_num_ext, ext$CHLA_mean, n = n_spl)
      smooth_df  <- clip_spl(spl_mean)
      names(smooth_df)[2L] <- "CHLA_mean"

      std_ok <- !is.na(ext$std_low) & !is.na(ext$std_hi)
      if (sum(std_ok) >= 3L) {
        spl_lo   <- stats::spline(date_num_ext[std_ok], ext$std_low[std_ok],
                                  n = n_spl)
        spl_hi   <- stats::spline(date_num_ext[std_ok], ext$std_hi[std_ok],
                                  n = n_spl)
        ribbon_lo <- clip_spl(spl_lo)
        ribbon_hi <- clip_spl(spl_hi)
        ribbon_df <- data.frame(
          date    = ribbon_lo$date,
          std_low = pmax(0, ribbon_lo$y),
          std_hi  = ribbon_hi$y
        )
        p <- p + ggplot2::geom_ribbon(
          data = ribbon_df,
          ggplot2::aes(x = .data$date, ymin = .data$std_low,
                       ymax = .data$std_hi,
                       fill = "Mean \u00b1 std dev (1991\u20132020)"),
          alpha = 0.3
        )
        ribbon_added <- TRUE
      }

      p <- p + ggplot2::geom_line(
        data = smooth_df,
        ggplot2::aes(x = .data$date, y = .data$CHLA_mean,
                     color = "Monthly mean Chl-a\n(1991\u20132020, 0\u201320 m)"),
        linewidth = 0.8,
        show.legend = FALSE
      )
    }
  }

  if (has_lims) {
    lims_agg <- stats::aggregate(
      CPHL ~ sample_date,
      data = station_lims,
      FUN = function(x) mean(x, na.rm = TRUE)
    )
    # Points only — no connecting line
    p <- p + ggplot2::geom_point(
      data = lims_agg,
      ggplot2::aes(x = .data$sample_date, y = .data$CPHL,
                   color = "Current year Chl-a\n(bottle, 0\u201320 m)"),
      size = 2.5,
      show.legend = FALSE
    )
  }

  color_values <- c(
    "Monthly mean Chl-a\n(1991\u20132020, 0\u201320 m)" = "#1f77b4",
    "Current year Chl-a\n(bottle, 0\u201320 m)"         = "#2ca02c"
  )
  color_breaks <- names(color_values)

  # Breaks at mid-month so labels are centred within each month's span.
  # expand=c(0,0) removes default padding so the y-axis sits exactly at Jan 1.
  month_breaks <- as.Date(paste0(current_year, "-",
                                 sprintf("%02d", 1:12), "-15"))
  # Grid lines at month boundaries (1st of each month) so each month column is
  # clearly delimited. major.x is suppressed so mid-month ticks carry no grid.
  month_boundary_breaks <- as.Date(paste0(current_year, "-",
                                          sprintf("%02d", 1:12), "-01"))
  x_labels <- if (show_x_axis) .MONTH_ABBR_EN else rep("", 12L)

  # Register identical color legend keys in every panel so patchwork can
  # collect them even when one station lacks historical statistics.
  dummy_line <- data.frame(
    date = as.Date(c(paste0(current_year, "-06-15"),
                     paste0(current_year, "-06-16"))),
    y    = c(NA_real_, NA_real_)
  )
  dummy_point <- data.frame(
    date = as.Date(paste0(current_year, "-06-15")),
    y    = NA_real_
  )

  p <- p +
    ggplot2::geom_line(
      data = dummy_line,
      ggplot2::aes(x = .data$date, y = .data$y,
                   color = "Monthly mean Chl-a\n(1991\u20132020, 0\u201320 m)"),
      linewidth = 0.8,
      na.rm = TRUE,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = dummy_point,
      ggplot2::aes(x = .data$date, y = .data$y,
                   color = "Current year Chl-a\n(bottle, 0\u201320 m)"),
      size = 2.5,
      na.rm = TRUE,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_color_manual(
      name = NULL,
      values = color_values,
      breaks = color_breaks,
      drop = FALSE
    ) +
    ggplot2::scale_x_date(
      breaks = month_breaks,
      minor_breaks = month_boundary_breaks,
      labels = x_labels,
      limits = as.Date(c(paste0(current_year, "-01-01"),
                         paste0(current_year, "-12-31"))),
      expand = c(0, 0)
    ) +
    ggplot2::labs(x = "", y = "Chl-a (\u00b5g/L)", title = station_label) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title          = ggplot2::element_text(size = 10, face = "bold"),
      panel.grid.major.x  = ggplot2::element_blank(),
      panel.grid.minor.x  = ggplot2::element_line(color = "grey85",
                                                   linewidth = 0.3),
      panel.grid.minor.y  = ggplot2::element_blank(),
      legend.title        = ggplot2::element_blank(),
      panel.border        = ggplot2::element_rect(color = "black", fill = NA,
                                                  linewidth = 0.5)
    )

  # All panels must have an identical fill guide so patchwork's
  # guides = "collect" can merge them.  Panels that produced a real ribbon
  # already have the geom; panels without one get a structural dummy ribbon
  # (NA data, na.rm = TRUE so nothing is drawn) that registers the same fill
  # key type and label.  Without this, color guides from ribbon vs. no-ribbon
  # panels are considered non-identical by patchwork and appear doubled.
  if (!ribbon_added) {
    dummy_rib <- data.frame(
      date    = as.Date(paste0(current_year, "-06-15")),
      std_low = NA_real_,
      std_hi  = NA_real_
    )
    p <- p + ggplot2::geom_ribbon(
      data = dummy_rib,
      ggplot2::aes(x = .data$date, ymin = .data$std_low, ymax = .data$std_hi,
                   fill = "Mean \u00b1 std dev (1991\u20132020)"),
      alpha = 0.3, na.rm = TRUE
    )
  }
  p <- p + ggplot2::scale_fill_manual(
    name   = NULL,
    values = c("Mean \u00b1 std dev (1991\u20132020)" = "gray70"),
    breaks = "Mean \u00b1 std dev (1991\u20132020)",
    drop   = FALSE
  )

  p
}

#' Create a regional CTD composite figure (all standard stations)
#'
#' Produces a multi-station patchwork figure for one geographic region.
#' Each row shows one station: left panel is the CTD fluorescence profile
#' (0–50 m, deduplicated casts, shared x-scale), right panel is the Chl-a
#' time series with smooth spline historical statistics.
#'
#' When \code{force_two_columns = TRUE} (used for report output), a spacer
#' column is added even when no LIMS data are available, keeping the profile
#' panels at a consistent width across all regions.
#'
#' Month labels appear only on the bottom station row (English abbreviations,
#' locale-independent).
#'
#' @param ctd_data_full Data frame from \code{read_cnv_folder_all()}.
#' @param lims_data_full Data frame from \code{read_lims_data_all()}, or NULL.
#' @param chl_stats Data frame from \code{load_chl_statistics()}.
#' @param standard_stations Data frame from \code{load_standard_stations()}.
#' @param region Character region name.
#' @param current_year Integer year.
#' @param force_two_columns Logical; always use a two-column layout even
#'   without LIMS data (right column filled with spacers).  Default FALSE.
#' @return A patchwork object, or NULL if there are no data for the region.
#' @export
create_ctd_region_figure <- function(ctd_data_full, lims_data_full = NULL,
                                     chl_stats, standard_stations,
                                     region, current_year,
                                     force_two_columns = FALSE) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required for CTD region figures. ",
         "Install it with: install.packages(\"patchwork\")", call. = FALSE)
  }
  if (is.null(ctd_data_full) || nrow(ctd_data_full) == 0) return(NULL)

  region_ctd <- ctd_data_full[ctd_data_full$region == region, ]
  if (nrow(region_ctd) == 0) return(NULL)

  # Station display order: YAML order first, extras by latitude (N→S)
  std_in_region <- standard_stations$station_name[
    standard_stations$region == region
  ]
  present <- unique(region_ctd$canonical_name)
  ordered_std <- std_in_region[std_in_region %in% present]
  extra <- setdiff(present, ordered_std)
  if (length(extra) > 0L) {
    lat_extra <- stats::aggregate(
      latitude ~ canonical_name,
      data = region_ctd[region_ctd$canonical_name %in% extra, ],
      FUN = mean, na.rm = TRUE
    )
    extra <- lat_extra$canonical_name[
      order(lat_extra$latitude, decreasing = TRUE)
    ]
  }
  stations <- c(ordered_std, extra)

  has_lims     <- !is.null(lims_data_full) && nrow(lims_data_full) > 0
  two_col      <- has_lims || force_two_columns

  # Profile overlay: only bottle data from the same cruise as the CTD casts.
  # The LIMS export may span multiple cruises; filtering to CTD dates ensures
  # only same-cruise points are shown on the profile.
  cruise_dates <- unique(region_ctd$sample_date[!is.na(region_ctd$sample_date)])
  region_lims_profile <- if (has_lims) {
    lims_data_full[
      lims_data_full$region == region &
        lims_data_full$DEPH <= 50 &
        lims_data_full$sample_date %in% cruise_dates, ]
  } else {
    NULL
  }

  # Shared x-limit for profiles: span both CTD fluorescence and bottle CPHL
  # so both data series are visible on a common axis.
  max_chl <- max(region_ctd$chl_fluorescence[
    region_ctd$pressure_dbar <= 50], na.rm = TRUE)
  if (has_lims && !is.null(region_lims_profile) &&
      nrow(region_lims_profile) > 0) {
    max_cphl <- max(region_lims_profile$CPHL, na.rm = TRUE)
    if (is.finite(max_cphl)) max_chl <- max(max_chl, max_cphl)
  }
  if (!is.finite(max_chl) || max_chl <= 0) max_chl <- 10
  xlim_profile <- c(0, max_chl * 1.05)

  plots <- list()

  # The x-axis is drawn only on the last panel, so "last" must be the last
  # station that actually produces a panel -- not the last of the planned
  # list, which may be skipped below (no casts after deduplication), leaving
  # the whole figure without any x-axis labels.
  plotted_stations <- Filter(function(s) {
    s_ctd <- region_ctd[region_ctd$canonical_name == s, ]
    nrow(s_ctd) > 0L && nrow(deduplicate_casts(s_ctd)) > 0L
  }, stations)

  for (stn in stations) {
    stn_ctd <- region_ctd[region_ctd$canonical_name == stn, ]
    if (nrow(stn_ctd) == 0L) next

    # Remove duplicate casts for the same station/date
    stn_ctd <- deduplicate_casts(stn_ctd)
    if (nrow(stn_ctd) == 0L) next

    is_last <- identical(stn, plotted_stations[[length(plotted_stations)]])

    profile_dates <- sort(unique(stn_ctd$sample_date[
      !is.na(stn_ctd$sample_date)]))
    date_label <- if (length(profile_dates) > 0L) {
      paste(format(profile_dates, "%Y-%m-%d"), collapse = ", ")
    } else NULL

    stn_lims_profile <- if (!is.null(region_lims_profile) &&
                             nrow(region_lims_profile) > 0) {
      pts <- region_lims_profile[region_lims_profile$canonical_name == stn, ]
      if (nrow(pts) > 0L) pts else NULL
    } else {
      NULL
    }

    profile_p <- create_fluorescence_profile(
      stn_ctd, stn,
      xlim        = xlim_profile,
      date_label  = date_label,
      show_x_axis = is_last,
      lims_points = stn_lims_profile
    )
    plots <- c(plots, list(profile_p))

    if (two_col) {
      ts_p <- if (has_lims) {
        stn_lims <- lims_data_full[
          lims_data_full$canonical_name == stn & lims_data_full$DEPH <= 20, ]
        if (nrow(stn_lims) == 0L) stn_lims <- NULL
        create_chl_timeseries(
          stn_lims, match_stats_station(stn, chl_stats),
          stn, current_year, show_x_axis = is_last
        )
      } else {
        NULL
      }
      if (is.null(ts_p)) ts_p <- patchwork::plot_spacer()
      plots <- c(plots, list(ts_p))
    }
  }

  if (length(plots) == 0L) return(NULL)

  ncols  <- if (two_col) 2L else 1L
  widths <- if (two_col) c(1, 3) else 1

  # Apply & theme to panels before adding plot_annotation — if & is used on a
  # patchwork that already contains plot_annotation(theme=...), patchwork tries
  # to combine the theme with the annotation object and emits a spurious
  # "annotation$theme is not a valid theme" warning on some patchwork versions.
  pw <- patchwork::wrap_plots(plots, ncol = ncols) +
    patchwork::plot_layout(widths = widths, guides = "collect")

  pw <- pw &
    ggplot2::theme(legend.position = "bottom", legend.box = "horizontal")

  # Passing theme = ggplot2::theme() (an empty but valid theme object) silences
  # the "annotation$theme is not a valid theme" warning that some patchwork
  # versions emit when the annotation's internal theme slot is NULL.
  pw + patchwork::plot_annotation(title = region, theme = ggplot2::theme())
}
