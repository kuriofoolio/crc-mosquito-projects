# Individual maximum Z-score across the entire recorded experiment.
# User-defined "onset" metric = maximum individual Z-score, NOT onset time.
# Uses original recording resolution, no daily or group averaging.
# Edit data_dir, monitor_file and excluded_animals below, then run all.

# 1. Setup ---------------------------------------------------------------------

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  pacman,
  damr,
  data.table,
  ggplot2
)


# 2. Configuration -------------------------------------------------------------

# Enter the original monitor channel numbers to exclude for THIS file.
# Examples: c(21, 25), or integer(0) to include every animal.
excluded_animals <- c(1,2,4,5,8,10,11,12,13,16,18,21,26,28)
clock_tz <- "UTC"  # Preserve the monitor's written clock times consistently.
stopifnot(is.numeric(excluded_animals), !anyNA(excluded_animals),
          all(excluded_animals %in% 1:32))

data_dir <- "/Users/MAC/Desktop/R_Projects/Actograms_in_Rethomics/2026_09"

monitor_file <- "Monitor13.txt"

output_dir <- data_dir

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# 3. Load DAM data -------------------------------------------------------------

input_file <- file.path(data_dir, monitor_file)
if (!file.exists(input_file)) stop("File not found: ", input_file)

# The first recorded timestamp may differ from the biological experiment start
# if the input file was trimmed. DAM columns 2 and 3 contain date and time.
first_row <- read.delim(input_file, header = FALSE, sep = "\t", nrows = 1,
                        colClasses = "character", quote = "", comment.char = "")
if (nrow(first_row) != 1L || ncol(first_row) < 3L) stop("Empty or invalid DAM file.")
date_parts <- strsplit(trimws(first_row[[2]]), "[[:space:]]+")[[1]]
if (length(date_parts) != 3L) stop("Expected DAM date such as '25 Jul 26'.")
month_number <- match(tolower(date_parts[2]), tolower(month.abb))
year_number <- as.integer(date_parts[3])
if (!is.na(year_number) && year_number < 100) year_number <- 2000 + year_number
date_iso <- sprintf("%04d-%02d-%02d", year_number, month_number,
                    as.integer(date_parts[1]))
recording_start <- as.POSIXct(paste(date_iso, trimws(first_row[[3]])),
                             format = "%Y-%m-%d %H:%M:%S", tz = clock_tz)
if (is.na(recording_start)) stop("Could not parse the first recorded timestamp.")
start_time <- format(recording_start, "%Y-%m-%d %H:%M:%S", tz = clock_tz)
cat("First recorded timestamp: ", start_time, "\n", sep = "")

dt <- read_dam_file(input_file, region_id = 1:32,
                    start_datetime = start_time, tz = clock_tz)

# 4. Assign original animal numbers and apply exclusions ------------------------
# damr IDs end in |NN (the original channel). Never use factor integer codes:
# those are positions in the factor levels and can renumber channels.
id_text <- as.character(dt$id)
animal_numbers <- suppressWarnings(as.integer(sub("^.*[|]", "", id_text)))
if (anyNA(animal_numbers) || any(!animal_numbers %in% 1:32)) {
  stop("Cannot identify original channel numbers from damr IDs; inspect dt$id.")
}
dt[, animal_number := animal_numbers]
dt <- dt[!animal_number %in% excluded_animals]
if (!nrow(dt)) stop("No observations remain after exclusions.")
cat("Excluded animals: ", paste(excluded_animals, collapse = ", "), "\n", sep = "")


# 5. Assign experimental groups -----------------------------------------------

dt[
  ,
  group := fcase(
    
    # Female virgins
    animal_number %in% c(
      1, 3, 5,
      9, 11, 13,
      17, 19, 21,
      25, 27, 29
    ),
    "Female virgin",
    
    # Males
    animal_number %in% c(
      2, 4,
      10, 12,
      18, 20,
      26, 28
    ),
    "Male",
    
    # Mated females
    animal_number %in% c(
      6, 7, 8,
      14, 15, 16,
      22, 23, 24,
      30, 31, 32
    ),
    "Mated female"
  )
]


if (anyNA(dt$group)) {
  stop("Missing group assignments for animals: ",
       paste(unique(dt[is.na(group), animal_number]), collapse = ", "))
}

# Set order of groups

dt[
  ,
  group := factor(
    group,
    levels = c(
      "Female virgin",
      "Male",
      "Mated female"
    )
  )
]


# 6. Create actual datetime ----------------------------------------------------

# recording_start was read from the monitor file above.

dt[
  ,
  datetime := recording_start + t
]


# 7. Use the full recording ----------------------------------------------------
# Optional experimental boundaries: leave NULL to use every imported observation.
# Example: analysis_start <- as.POSIXct("2026-07-25 13:00:00", tz=clock_tz)
analysis_start <- as.POSIXct("2026-08-22 00:00:00", tz=clock_tz)
analysis_end <- as.POSIXct("2026-08-26 00:00:00", tz=clock_tz)

#analysis_start <- NULL
#analysis_end <- NULL  

# Exclusive upper boundary, if supplied.
if (!is.null(analysis_start)) dt <- dt[datetime >= analysis_start]
if (!is.null(analysis_end)) dt <- dt[datetime < analysis_end]
if (!nrow(dt)) stop("No data in the requested interval.")

# Reject nonfinite or negative counts; keep genuine zero readings.
dt[!is.finite(activity) | activity < 0, activity := NA_real_]
setorder(dt, animal_number, datetime)

# 8. Standardize each animal over ALL retained dates ----------------------------
dt[, z_activity := {
  x <- activity
  s <- sd(x, na.rm = TRUE)
  if (sum(is.finite(x)) >= 2L && is.finite(s) && s > 0) {
    (x - mean(x, na.rm = TRUE)) / s
  } else {
    rep(NA_real_, .N) # Constant or unavailable data: Z-score is undefined.
  }
}, by = animal_number]

# 9. One maximum reading per animal --------------------------------------------
# Equal maxima: retain the earliest timestamp and report the number of ties.
peak_summary <- dt[, {
  good <- which(is.finite(z_activity))
  j <- if (length(good)) good[which.max(z_activity[good])] else NA_integer_
  .(n_valid_readings = sum(is.finite(activity)),
    mean_activity = if (any(is.finite(activity))) mean(activity, na.rm=TRUE) else NA_real_,
    sd_activity = sd(activity, na.rm=TRUE),
    maximum_z = if (!is.na(j)) z_activity[j] else NA_real_,
    activity_at_maximum = if (!is.na(j)) activity[j] else NA_real_,
    first_peak_datetime = if (!is.na(j)) datetime[j] else as.POSIXct(NA, tz=clock_tz),
    n_tied_maxima = if (!is.na(j)) sum(z_activity == z_activity[j], na.rm=TRUE) else 0L,
    result = if (!is.na(j)) "Maximum detected" else "Undefined Z-score: insufficient data or zero SD")
}, by = .(animal_number, group)]

# Retain animals with no imported records as explicitly unavailable rows.
roster <- data.table(animal_number = setdiff(1:32, excluded_animals))
peak_summary <- merge(roster, peak_summary, by="animal_number", all.x=TRUE, sort=TRUE)
peak_summary[is.na(result), result := "No imported records"]
peak_summary[, peak_hours_after_dawn :=
  (as.numeric(format(first_peak_datetime, "%H", tz=clock_tz)) +
   as.numeric(format(first_peak_datetime, "%M", tz=clock_tz))/60 +
   as.numeric(format(first_peak_datetime, "%S", tz=clock_tz))/3600 - 6) %% 24]

# Save every tied peak too, so no ambiguity is hidden.
all_peaks <- dt[is.finite(z_activity)]
all_peaks <- all_peaks[, .SD[z_activity == max(z_activity)], by=animal_number]
all_peaks <- all_peaks[, .(animal_number, group, datetime, activity, z_activity)]

# 10. Horizontal plot: one row per animal --------------------------------------
peak_summary[, animal_label := factor(paste("Animal", animal_number),
  levels=rev(paste("Animal", sort(animal_number))))]
plot_data <- peak_summary[is.finite(maximum_z)]
missing_data <- peak_summary[!is.finite(maximum_z)]

p <- ggplot(peak_summary, aes(y=animal_label)) +
  geom_segment(data=plot_data,
    aes(x=0, xend=maximum_z, yend=animal_label, colour=group), linewidth=0.7) +
  geom_point(data=plot_data, aes(x=maximum_z, colour=group), size=3) +
  geom_text(data=plot_data, aes(x=maximum_z, label=sprintf("%.2f", maximum_z)),
    hjust=-0.35, size=3) +
  geom_text(data=missing_data, aes(x=0), label="Undefined / no valid data",
    hjust=0, size=3, colour="grey40") +
  scale_y_discrete(drop=FALSE) +
  scale_x_continuous(expand=expansion(mult=c(0.02, 0.18))) +
  scale_colour_manual(values=c("Female virgin"="#229954", "Male"="#2874A6",
                               "Mated female"="#CB4335"), drop=FALSE) +
  labs(title="Maximum individual activity Z-score",
       subtitle=paste("Full selected recording:",
         format(min(dt$datetime), "%d %b %Y %H:%M", tz=clock_tz), "to",
         format(max(dt$datetime), "%d %b %Y %H:%M", tz=clock_tz)),
       x="Maximum Z-score across all retained days", y="Animal", colour="Group",
       caption=paste("User-defined onset metric: peak standardized activity, not onset time.",
                     "Each animal standardized using its own mean and sample SD.",
                     "Excluded:", paste(excluded_animals, collapse=", "))) +
  theme_classic(base_size=11) +
  theme(legend.position="top", plot.title=element_text(face="bold"),
        plot.caption=element_text(hjust=0, size=8))
print(p)

# 11. Save results -------------------------------------------------------------
prefix <- tools::file_path_sans_ext(basename(monitor_file))
fwrite(peak_summary[, !"animal_label"],
       file.path(output_dir, paste0(prefix, "_onset.csv")))
fwrite(all_peaks, file.path(output_dir, paste0(prefix, "_all_tied_peak_readings.csv")))
plot_height <- max(6, 2.5 + 0.25*nrow(peak_summary))
ggsave(file.path(output_dir, paste0(prefix, "_onset.png")),
       p, width=11, height=plot_height, dpi=300)
ggsave(file.path(output_dir, paste0(prefix, "_onset.pdf")),
       p, width=11, height=plot_height)
print(peak_summary)
cat("\nSaved plot and peak readings in:", normalizePath(output_dir), "\n")
