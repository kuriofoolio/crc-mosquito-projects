################################################################################
# Mosquito actograms
#
# - Reads actual dates and times from the monitor file
# - Uses calendar-date row labels
# - Sets x = 0 to the configured dawn time
# - Preserves original animal/channel numbers
# - Supports optional animal exclusions and date restrictions
# - Shows invalid or unavailable readings in pale blue
# - Double plots: each row spans 48 hours
#
# Actograms require only base R.
################################################################################


# 1. Configuration -------------------------------------------------------------

data_dir <- "/Users/MAC/Desktop/R_Projects/Actograms_in_Rethomics/TcEight_example"

monitor_file <- "Monitor13_10.txt"

output_dir <- "/Users/MAC/Desktop/R_Projects/Actograms_in_Rethomics/2026_10"

# Output filename follows the input filename automatically.
output_pdf <- file.path(
  output_dir,
  paste0(tools::file_path_sans_ext(basename(monitor_file)), "_actograms.pdf")
)

# No exclusions:
excluded_animals <- integer(0)

# Example exclusions:
# excluded_animals <- c(21, 25)

# Set TRUE for the full recording.
# Set FALSE and edit the dates below for a selected period.
use_all_days <- TRUE

# UTC preserves the written monitor clock times without DST conversion.
# This does not imply that the instrument was physically in the UTC time zone.
clock_tz <- "UTC"

analysis_start <- as.POSIXct(
  "2026-10-01 00:00:00",
  tz = clock_tz
)

# Exclusive end: this example includes October 1–4.
analysis_end <- as.POSIXct(
  "2026-10-05 00:00:00",
  tz = clock_tz
)

dawn_hour <- 6

# Set this to the actual recording-bin duration.
# Timestamps are treated as the END of each bin.
bin_seconds <- 60

# "recorded": use column 10, where 0 = dark and 1 = light.
# "scheduled": use the assumed light duration below, starting at dawn_hour.
shading_mode <- "recorded"
scheduled_light_hours <- 12

# FALSE: stop after the last valid reading in the selected period.
# TRUE: also show trailing invalid readings as unavailable data.
include_trailing_invalid <- FALSE

# Optional exploratory periodogram, using the same selected period.
run_periodogram <- FALSE


# 2. Validate settings ---------------------------------------------------------

stopifnot(
  is.numeric(excluded_animals),
  !anyNA(excluded_animals),
  all(excluded_animals %in% 1:32),
  length(dawn_hour) == 1L,
  is.finite(dawn_hour),
  dawn_hour >= 0,
  dawn_hour < 24,
  dawn_hour == floor(dawn_hour),
  length(bin_seconds) == 1L,
  is.finite(bin_seconds),
  bin_seconds > 0,
  length(shading_mode) == 1L,
  shading_mode %in% c("recorded", "scheduled"),
  length(scheduled_light_hours) == 1L,
  is.finite(scheduled_light_hours),
  scheduled_light_hours > 0,
  scheduled_light_hours < 24
)

included_animals <- setdiff(1:32, excluded_animals)

if (!length(included_animals)) {
  stop("All animals have been excluded.")
}

if (!use_all_days) {
  if (
    length(analysis_start) != 1L ||
    length(analysis_end) != 1L ||
    is.na(analysis_start) ||
    is.na(analysis_end) ||
    analysis_start >= analysis_end
  ) {
    stop("Provide valid analysis dates with analysis_start before analysis_end.")
  }
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(output_dir)) {
  stop("Could not create output directory: ", output_dir)
}

cat(
  "Included animals: ", length(included_animals),
  "\nExcluded animals: ",
  if (length(excluded_animals)) {
    paste(excluded_animals, collapse = ", ")
  } else {
    "none"
  },
  "\n",
  sep = ""
)


# 3. Read the monitor file -----------------------------------------------------

input_file <- file.path(data_dir, monitor_file)

if (!file.exists(input_file)) {
  stop("File not found: ", input_file)
}

raw <- read.delim(
  input_file,
  header = FALSE,
  sep = "\t",
  quote = "",
  comment.char = "",
  stringsAsFactors = FALSE,
  colClasses = "character",
  check.names = FALSE
)

if (!nrow(raw)) {
  stop("The monitor file contains no rows.")
}

if (ncol(raw) != 42L) {
  stop("Expected 10 metadata columns and 32 activity columns.")
}


# 4. Parse dates dynamically ---------------------------------------------------

# Parse English month names independently of the computer's language settings.
date_parts <- strsplit(trimws(raw[[2]]), "[[:space:]]+")

if (any(lengths(date_parts) != 3L)) {
  stop("Unrecognised date format. Expected a date such as '25 Jul 26'.")
}

date_parts <- do.call(rbind, date_parts)

month_number <- match(
  tolower(date_parts[, 2]),
  tolower(month.abb)
)

year_number <- suppressWarnings(as.integer(date_parts[, 3]))
day_number <- suppressWarnings(as.integer(date_parts[, 1]))

if (
  anyNA(month_number) ||
  anyNA(year_number) ||
  anyNA(day_number)
) {
  stop("Some date components could not be parsed.")
}

year_number[year_number < 100] <-
  2000 + year_number[year_number < 100]

date_iso <- sprintf(
  "%04d-%02d-%02d",
  year_number,
  month_number,
  day_number
)

timestamp <- as.POSIXct(
  paste(date_iso, trimws(raw[[3]])),
  format = "%Y-%m-%d %H:%M:%S",
  tz = clock_tz
)

if (anyNA(timestamp)) {
  stop("Some timestamps could not be parsed.")
}

# Catch dates that a platform might silently normalise.
if (any(format(timestamp, "%Y-%m-%d", tz = clock_tz) != date_iso)) {
  stop("Some calendar dates are invalid.")
}

time_steps <- diff(as.numeric(timestamp))

if (any(time_steps <= 0)) {
  stop("Timestamps must be unique and strictly increasing.")
}

if (any(time_steps < bin_seconds)) {
  stop("Overlapping bins: check bin_seconds against the recording interval.")
}

if (length(time_steps) && !any(time_steps == bin_seconds)) {
  warning(
    "No adjacent timestamps match bin_seconds. ",
    "Check the recording-bin duration before interpreting the plot."
  )
}


# 5. Validate activity and light readings --------------------------------------

status <- suppressWarnings(as.numeric(raw[[4]]))
light <- suppressWarnings(as.numeric(raw[[10]]))
monitor_ids <- suppressWarnings(as.numeric(raw[[6]]))

if (anyNA(status) || any(!is.finite(status))) {
  stop("Missing or nonnumeric status codes.")
}

if (
  anyNA(monitor_ids) ||
  any(!is.finite(monitor_ids)) ||
  length(unique(monitor_ids)) != 1L
) {
  stop("The file must contain one valid monitor ID.")
}

monitor_id <- unique(monitor_ids)

record_type <- toupper(trimws(raw[[8]]))

if (any(!record_type %in% c("CT", "0"))) {
  stop("This script expects count data (Ct/CT/0) only.")
}

activity <- as.matrix(
  data.frame(
    lapply(raw[11:42], function(x) suppressWarnings(as.numeric(x)))
  )
)

# Status 1 is treated as a valid recording.
valid <- status == 1L

if (!any(valid)) {
  stop("No valid status-1 recordings.")
}

# Validate the channels being analysed.
included_counts <- activity[valid, included_animals, drop = FALSE]

if (
  any(!is.finite(included_counts)) ||
  any(included_counts < 0)
) {
  stop("Included animals have invalid activity counts in status-1 rows.")
}

if (
  shading_mode == "recorded" &&
  any(!light[valid] %in% c(0, 1))
) {
  stop("Unexpected light sensor values: expected 0 or 1 in valid rows.")
}

# Invalid recordings are unavailable, not biological zero activity.
activity[!valid, ] <- NA_real_
light[!valid] <- NA_real_

fmt <- function(x) {
  format(x, "%d %b %Y %H:%M:%S", tz = clock_tz)
}

cat(
  "\nFile timestamps: ", fmt(min(timestamp)),
  " to ", fmt(max(timestamp)), "\n",
  sep = ""
)

cat(
  "Valid timestamps: ", fmt(min(timestamp[valid])),
  " to ", fmt(max(timestamp[valid])), "\n",
  sep = ""
)

cat("\nStatus-code counts:\n")
print(table(status))


# 6. Select the analysis period ------------------------------------------------

# A reading represents [timestamp - bin_seconds, timestamp).
#
# For a restricted analysis, retain only bins fully contained in the requested
# interval. A bin ending exactly at analysis_end is included because its activity
# occurred before that boundary.

selected <- rep(TRUE, length(timestamp))

if (!use_all_days) {
  selected <-
    timestamp - bin_seconds >= analysis_start &
    timestamp <= analysis_end
}

if (!any(selected)) {
  stop("No complete recording bins fall within the selected analysis period.")
}

timestamp <- timestamp[selected]
activity <- activity[selected, , drop = FALSE]
light <- light[selected]
valid <- valid[selected]

if (!any(valid)) {
  stop("No valid recordings remain in the selected analysis period.")
}

# The plot begins with the first valid bin.
first_bin_start <- min(timestamp[valid]) - bin_seconds

last_bin_end <- if (include_trailing_invalid) {
  max(timestamp)
} else {
  max(timestamp[valid])
}

coverage_label <- paste(
  "Displayed coverage:",
  fmt(first_bin_start),
  "to",
  fmt(last_bin_end)
)

cat("\n", coverage_label, "\n", sep = "")


# 7. Dawn-based coordinates ----------------------------------------------------

origin <- as.POSIXct(
  paste(
    format(first_bin_start, "%Y-%m-%d", tz = clock_tz),
    sprintf("%02d:00:00", dawn_hour)
  ),
  format = "%Y-%m-%d %H:%M:%S",
  tz = clock_tz
)

# A reading before dawn belongs to the row beginning on the previous date.
if (origin > first_bin_start) {
  origin <- origin - 86400
}

start_hours <- as.numeric(
  difftime(timestamp - bin_seconds, origin, units = "hours")
)

end_hours <- as.numeric(
  difftime(timestamp, origin, units = "hours")
)

# A bin ending exactly at dawn belongs to the preceding row.
last_day <- floor(
  (
    as.numeric(difftime(last_bin_end, origin, units = "secs")) -
      1e-6
  ) / 86400
)

day_index <- seq.int(0L, as.integer(last_day))
row_dates <- as.Date(origin, tz = clock_tz) + day_index


# 8. Experimental groups -------------------------------------------------------

groups <- rep(NA_character_, 32)

groups[c(
  1, 3, 5, 9, 11, 13,
  17, 19, 21, 25, 27, 29
)] <- "Female virgin"

groups[c(
  2, 4, 10, 12,
  18, 20, 26, 28
)] <- "Male"

groups[c(
  6, 7, 8, 14, 15, 16,
  22, 23, 24, 30, 31, 32
)] <- "Mated female"

missing_groups <- included_animals[
  is.na(groups[included_animals])
]

if (length(missing_groups)) {
  stop(
    "Missing group assignments for included animals: ",
    paste(missing_groups, collapse = ", ")
  )
}

cat("\nIncluded animals by group:\n")
print(table(groups[included_animals]))


# 9. Draw double-plotted actograms ---------------------------------------------

# Each row begins at dawn_hour on its labelled calendar date.
# The left half covers 0–24 hours; the right half covers 24–48 hours.
# The right half repeats the next row's left half.

actoplot <- function(animal_number) {
  
  if (!animal_number %in% included_animals) {
    return(invisible(NULL))
  }
  
  a <- activity[, animal_number]
  
  observed_peak <- max(a[valid], na.rm = TRUE)
  
  # Avoid dividing by zero for animals with no detected activity.
  display_scale <- if (observed_peak > 0) observed_peak else 1
  
  plot(
    NA,
    xlim = c(0, 48),
    ylim = c(last_day + 0.6, -0.6),
    xaxs = "i",
    yaxs = "i",
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  
  for (d in day_index) {
    
    # Blue remains visible wherever valid recording coverage is absent.
    rect(
      0, d - 0.45, 48, d + 0.45,
      col = "#DBEAF2",
      border = NA
    )
    
    idx <- which(
      valid &
        start_hours < d * 24 + 48 &
        end_hours > d * 24
    )
    
    if (!length(idx)) next
    
    left <- pmax(0, start_hours[idx] - d * 24)
    right <- pmin(48, end_hours[idx] - d * 24)
    
    if (shading_mode == "recorded") {
      
      background <- ifelse(light[idx] == 0, "grey82", "white")
      
      rect(
        left, d - 0.45, right, d + 0.45,
        col = background,
        border = NA
      )
      
    } else {
      
      # Draw darkness over valid data first.
      rect(
        left, d - 0.45, right, d + 0.45,
        col = "grey82",
        border = NA
      )
      
      # Overlay scheduled light only where valid data exist.
      # This also handles bins that cross a scheduled light/dark boundary.
      for (light_start in c(0, 24)) {
        
        light_left <- pmax(left, light_start)
        light_right <- pmin(
          right,
          light_start + scheduled_light_hours
        )
        
        overlap <- which(light_right > light_left)
        
        if (length(overlap)) {
          rect(
            light_left[overlap],
            d - 0.45,
            light_right[overlap],
            d + 0.45,
            col = "white",
            border = NA
          )
        }
      }
    }
    
    # Counts are raw beam breaks.
    # Bar heights use one fixed scale per animal across all displayed days.
    positive <- which(is.finite(a[idx]) & a[idx] > 0)
    
    if (length(positive)) {
      rect(
        xleft = left[positive],
        ybottom =
          d + 0.40 - 0.8 * a[idx][positive] / display_scale,
        xright = right[positive],
        ytop = d + 0.40,
        col = "grey20",
        border = NA
      )
    }
  }
  
  abline(v = seq(0, 48, 4), col = "#CCCCCC", lwd = 0.5)
  abline(h = day_index + 0.5, col = "#CCCCCC", lwd = 0.5)
  abline(v = 24, col = "grey50", lwd = 1)
  
  axis(1, at = seq(0, 48, 4))
  
  axis(
    2,
    at = day_index,
    labels = format(row_dates, "%d %b %Y"),
    las = 1,
    cex.axis = 0.7,
    tck = -0.01
  )
  
  box()
  
  mtext(
    paste0(
      "Mosquito ", animal_number,
      " | ", groups[animal_number],
      " | Monitor ", monitor_id
    ),
    side = 3,
    line = 2.6,
    font = 2,
    cex = 1.1
  )
  
  mtext(
    coverage_label,
    side = 3,
    line = 1.2,
    cex = 0.75
  )
  
  mtext(
    sprintf(
      "Time since %02d:00 reference (h; 0 = %02d:00)",
      dawn_hour,
      dawn_hour
    ),
    side = 1,
    line = 2.3
  )
  
  mtext(
    "Date at row start",
    side = 2,
    line = 6.7,
    cex = 0.8
  )
  
  shade_label <- if (shading_mode == "recorded") {
    "Grey: sensor-reported darkness"
  } else {
    sprintf(
      "Grey: assumed %gh light / %gh dark schedule",
      scheduled_light_hours,
      24 - scheduled_light_hours
    )
  }
  
  scale_label <- if (observed_peak > 0) {
    sprintf(
      "Full bar: %g counts / %g-second bin",
      observed_peak,
      bin_seconds
    )
  } else {
    "No beam breaks detected in valid readings"
  }
  
  mtext(
    paste(
      shade_label,
      "Blue: unavailable data",
      scale_label,
      sep = " | "
    ),
    side = 1,
    line = 4,
    cex = 0.65
  )
}


# 10. Save the actogram PDF ----------------------------------------------------

write_actograms <- function() {
  
  pdf(
    output_pdf,
    width = 11,
    height = max(6, 2.2 + length(day_index) * 0.23),
    onefile = TRUE
  )
  
  on.exit(dev.off(), add = TRUE)
  
  par(mar = c(5.3, 8, 4.8, 1.2))
  
  for (animal_number in included_animals) {
    actoplot(animal_number)
  }
}

write_actograms()

cat(
  "\nSaved: ",
  normalizePath(output_pdf, mustWork = FALSE),
  "\n",
  sep = ""
)


# 11. Optional exploratory periodogram -----------------------------------------

# Disabled by default.
# If enabled, analyse a biologically appropriate period.
# Different lighting conditions should generally be examined separately.

if (run_periodogram) {
  
  required <- c("damr", "behavr", "zeitgebr", "ggetho", "ggplot2")
  
  missing <- required[
    !vapply(required, requireNamespace, logical(1), quietly = TRUE)
  ]
  
  if (length(missing)) {
    stop(
      "Actograms were saved. To run periodograms, install: ",
      paste(missing, collapse = ", ")
    )
  }
  
  period_data <- damr::read_dam_file(
    input_file,
    region_id = included_animals,
    start_datetime = format(
      origin,
      "%Y-%m-%d %H:%M:%S",
      tz = clock_tz
    ),
    tz = clock_tz
  )
  
  # Match the displayed coverage, using bin-end timestamps.
  lower_t <- as.numeric(difftime(first_bin_start, origin, units = "secs"))
  upper_t <- as.numeric(difftime(last_bin_end, origin, units = "secs"))
  
  period_data <- period_data[t > lower_t & t <= upper_t]
  
  if (!nrow(period_data)) {
    stop("Actograms were saved, but no periodogram observations remain.")
  }
  
  period_results <- zeitgebr::periodogram(
    activity,
    period_data,
    period_range = c(14, 34) * 3600,
    FUN = zeitgebr::chi_sq_periodogram,
    resample_rate = 1 / 300
  )
  
  period_results <- zeitgebr::find_peaks(period_results)
  
  print(
    ggetho::ggperio(
      period_results,
      ggplot2::aes(period, power)
    ) +
      ggplot2::geom_line() +
      ggetho::geom_peak(colour = "black") +
      ggplot2::geom_line(
        ggplot2::aes(y = signif_threshold),
        colour = "red"
      ) +
      ggplot2::theme_classic() +
      ggplot2::facet_wrap(~id)
  )
}