################################################################################
# Mosquito activity: horizontal violin plot
#
# - Reads recording start time automatically
# - Supports animal exclusions
# - Uses all recorded days or a selected date range
# - Calculates one mean daily activity value per mosquito
# - Updates plot headings from the filename and selected data
# - Saves only: {monitor filename}_violin.png
################################################################################


# 1. Setup ---------------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  damr,
  data.table,
  ggplot2
)


# 2. Configuration -------------------------------------------------------------

data_dir <- "/Users/MAC/Desktop/R_Projects/Actograms_in_Rethomics/2026_09"

monitor_file <- "Monitor13.txt"

output_dir <- data_dir

# July experiment: exclude animals 
excluded_animals <- c(1,2,4,5,8,10,11,12,13,16,18,21,26,28)

# For experiments with no exclusions, use:
# excluded_animals <- integer(0)

# TRUE = all imported days.
# FALSE = use analysis_start and analysis_end below.
use_all_days <- FALSE

# Preserve the clock times written in the monitor file.
clock_tz <- "UTC"

analysis_start <- as.POSIXct(
  "2026-08-22 00:00:00",
  tz = clock_tz
)

# Exclusive upper boundary:
# August 3 at midnight includes all of August 2.
analysis_end <- as.POSIXct(
  "2026-08-26 00:00:00",
  tz = clock_tz
)

experiment_name <- tools::file_path_sans_ext(
  basename(monitor_file)
)

output_png <- file.path(
  output_dir,
  paste0(experiment_name, "_violin.png")
)

stopifnot(
  is.numeric(excluded_animals),
  !anyNA(excluded_animals),
  all(excluded_animals %in% 1:32)
)

included_animals <- setdiff(1:32, excluded_animals)

if (!length(included_animals)) {
  stop("All animals have been excluded.")
}

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!dir.exists(output_dir)) {
  stop("Could not create output directory: ", output_dir)
}


# 3. Read recording start time automatically -----------------------------------

input_file <- file.path(data_dir, monitor_file)

if (!file.exists(input_file)) {
  stop("File not found: ", input_file)
}

first_row <- read.delim(
  input_file,
  header = FALSE,
  sep = "\t",
  nrows = 1,
  colClasses = "character",
  quote = "",
  comment.char = ""
)

if (nrow(first_row) != 1L || ncol(first_row) < 3L) {
  stop("The monitor file is empty or has an unexpected format.")
}

# Expected date format: "25 Jul 26".
date_parts <- strsplit(
  trimws(first_row[[2]]),
  "[[:space:]]+"
)[[1]]

if (length(date_parts) != 3L) {
  stop("Unexpected date format. Expected a date such as '25 Jul 26'.")
}

day_number <- suppressWarnings(as.integer(date_parts[1]))

month_number <- match(
  tolower(date_parts[2]),
  tolower(month.abb)
)

year_number <- suppressWarnings(as.integer(date_parts[3]))

if (anyNA(c(day_number, month_number, year_number))) {
  stop("Could not parse the date in the first row.")
}

if (year_number < 100) {
  year_number <- 2000 + year_number
}

date_iso <- sprintf(
  "%04d-%02d-%02d",
  year_number,
  month_number,
  day_number
)

recording_start <- as.POSIXct(
  paste(date_iso, trimws(first_row[[3]])),
  format = "%Y-%m-%d %H:%M:%S",
  tz = clock_tz
)

if (is.na(recording_start)) {
  stop("Could not parse the first recorded timestamp.")
}

start_time <- format(
  recording_start,
  "%Y-%m-%d %H:%M:%S",
  tz = clock_tz
)

cat("Experiment:", experiment_name, "\n")
cat("First recorded timestamp:", start_time, "\n")


# 4. Load data and preserve original channel numbers ---------------------------

dt <- damr::read_dam_file(
  input_file,
  region_id = 1:32,
  start_datetime = start_time,
  tz = clock_tz
)

if (!nrow(dt)) {
  stop("No observations were imported.")
}

# Extract original channel numbers from damr IDs.
# Do not use as.integer(id): factor codes can renumber animals.
id_text <- as.character(dt$id)

animal_numbers <- suppressWarnings(
  as.integer(sub("^.*[|]", "", id_text))
)

if (
  anyNA(animal_numbers) ||
  any(!animal_numbers %in% 1:32)
) {
  stop(
    "Cannot identify original channel numbers from damr IDs. ",
    "Inspect unique(dt$id)."
  )
}

dt[, animal_number := animal_numbers]

dt <- dt[animal_number %in% included_animals]

if (!nrow(dt)) {
  stop("No observations remain after animal exclusions.")
}


# 5. Assign experimental groups -----------------------------------------------

group_levels <- c(
  "Female virgin",
  "Male",
  "Mated female"
)

dt[, group := fcase(
  animal_number %in% c(
    1, 3, 5, 9, 11, 13,
    17, 19, 21, 25, 27, 29
  ),
  "Female virgin",
  
  animal_number %in% c(
    2, 4, 10, 12,
    18, 20, 26, 28
  ),
  "Male",
  
  animal_number %in% c(
    6, 7, 8, 14, 15, 16,
    22, 23, 24, 30, 31, 32
  ),
  "Mated female"
)]

if (anyNA(dt$group)) {
  stop(
    "Missing group assignments for animals: ",
    paste(
      unique(dt[is.na(group), animal_number]),
      collapse = ", "
    )
  )
}

dt[, group := factor(group, levels = group_levels)]


# 6. Reconstruct timestamps and select analysis period -------------------------

dt[, datetime := recording_start + t]

if (anyNA(dt$datetime)) {
  stop("Some observation timestamps could not be reconstructed.")
}

if (use_all_days) {
  
  dt_analysis <- data.table::copy(dt)
  
} else {
  
  if (
    is.na(analysis_start) ||
    is.na(analysis_end) ||
    analysis_start >= analysis_end
  ) {
    stop("analysis_start must be earlier than analysis_end.")
  }
  
  dt_analysis <- dt[
    datetime >= analysis_start &
      datetime < analysis_end
  ]
}

if (!nrow(dt_analysis)) {
  stop(
    "No observations in the selected analysis period. ",
    "Check analysis_start and analysis_end."
  )
}

plot_start <- min(dt_analysis$datetime)
plot_end <- max(dt_analysis$datetime)

cat(
  "Selected dates:",
  format(plot_start, "%d %b %Y %H:%M", tz = clock_tz),
  "to",
  format(plot_end, "%d %b %Y %H:%M", tz = clock_tz),
  "\n"
)


# 7. Calculate daily total activity --------------------------------------------

# Keep genuine zero readings.
# Invalid values must not be counted as activity.
invalid_count <- dt_analysis[
  !is.finite(activity) | activity < 0,
  .N
]

if (invalid_count > 0L) {
  warning(
    invalid_count,
    " nonfinite or negative activity readings treated as missing."
  )
}

dt_analysis[
  !is.finite(activity) | activity < 0,
  activity := NA_real_
]

dt_analysis[, calendar_day := as.Date(datetime, tz = clock_tz)]

daily_activity <- dt_analysis[
  ,
  .(
    total_activity = if (any(is.finite(activity))) {
      sum(activity, na.rm = TRUE)
    } else {
      NA_real_
    },
    
    n_observations = sum(is.finite(activity))
  ),
  by = .(
    animal_number,
    group,
    calendar_day
  )
]

# This calculation includes partial recording days.
# Missing hours are not estimated or filled.


# 8. Calculate one mean daily value per mosquito -------------------------------

mean_daily_activity <- daily_activity[
  ,
  .(
    mean_daily_activity = if (any(is.finite(total_activity))) {
      mean(total_activity, na.rm = TRUE)
    } else {
      NA_real_
    },
    
    sd_daily_activity = sd(total_activity, na.rm = TRUE),
    
    n_days = sum(is.finite(total_activity))
  ),
  by = .(
    animal_number,
    group
  )
]

plot_data <- mean_daily_activity[
  is.finite(mean_daily_activity)
]

if (!nrow(plot_data)) {
  stop("No animals have valid daily activity values to plot.")
}

unavailable_animals <- setdiff(
  included_animals,
  plot_data$animal_number
)

if (length(unavailable_animals)) {
  warning(
    "No usable activity averages for animals: ",
    paste(unavailable_animals, collapse = ", ")
  )
}


# 9. Dynamic group counts and summaries ----------------------------------------

group_summary <- plot_data[
  ,
  .(
    n_animals = uniqueN(animal_number),
    mean_activity = mean(mean_daily_activity),
    median_activity = median(mean_daily_activity),
    sd_activity = sd(mean_daily_activity),
    minimum_days = min(n_days),
    maximum_days = max(n_days)
  ),
  by = group
]

print(group_summary)

# Include sample sizes in the axis labels.
group_counts <- setNames(
  integer(length(group_levels)),
  group_levels
)

for (g in group_levels) {
  group_counts[g] <- uniqueN(
    plot_data[as.character(group) == g, animal_number]
  )
}

group_labels <- setNames(
  paste0(
    group_levels,
    " (n = ",
    group_counts[group_levels],
    ")"
  ),
  group_levels
)

# A violin density needs at least two distinct animal values.
# Groups with one animal or identical values still show their points.
density_groups <- plot_data[
  ,
  .(
    n_animals = .N,
    n_distinct = uniqueN(mean_daily_activity)
  ),
  by = group
][
  n_animals >= 2L & n_distinct >= 2L,
  as.character(group)
]

violin_data <- plot_data[
  as.character(group) %in% density_groups
]

exclusion_label <- if (length(excluded_animals)) {
  paste(excluded_animals, collapse = ", ")
} else {
  "none"
}


# 10. Create horizontal violin plot -------------------------------------------

daily_plot <- ggplot(
  plot_data,
  aes(
    x = group,
    y = mean_daily_activity,
    fill = group
  )
)

if (nrow(violin_data)) {
  daily_plot <- daily_plot +
    geom_violin(
      data = violin_data,
      trim = FALSE,
      alpha = 0.5
    )
}

daily_plot <- daily_plot +
  geom_boxplot(
    width = 0.12,
    alpha = 0.8,
    outlier.shape = NA
  ) +
  geom_point(
    position = position_jitter(
      width = 0.08,
      height = 0,
      seed = 1
    ),
    size = 2,
    alpha = 0.7
  ) +
  
  # Groups become the vertical axis; activity becomes the horizontal axis.
  coord_flip() +
  
  scale_x_discrete(
    limits = rev(group_levels),
    labels = group_labels,
    drop = FALSE
  ) +
  
  labs(
    title = paste(
      experiment_name,
      "— Mean Daily Activity by Group"
    ),
    
    subtitle = paste(
      "Selected recording:",
      format(plot_start, "%d %b %Y %H:%M", tz = clock_tz),
      "to",
      format(plot_end, "%d %b %Y %H:%M", tz = clock_tz)
    ),
    
    x = "Experimental Group",
    y = "Mean Daily Total Activity (beam breaks)",
    
    caption = paste0(
      "Each point represents one mosquito. Excluded animals: ",
      exclusion_label,
      ".\nDaily totals include partial recording days; missing hours are not filled."
    )
  ) +
  
  theme_classic(base_size = 11) +
  
  theme(
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    plot.caption = element_text(
      hjust = 0,
      size = 9
    )
  )

print(daily_plot)


# 11. Save PNG only ------------------------------------------------------------

ggsave(
  filename = output_png,
  plot = daily_plot,
  width = 11,
  height = 6,
  dpi = 300,
  bg = "white"
)


# 12. Completion message -------------------------------------------------------

cat("\nANALYSIS COMPLETE\n")
cat("Experiment:", experiment_name, "\n")
cat("Excluded animals:", exclusion_label, "\n")
cat("Animals plotted:", nrow(plot_data), "\n")

cat(
  "Selected recording:",
  format(plot_start, "%d %b %Y %H:%M", tz = clock_tz),
  "to",
  format(plot_end, "%d %b %Y %H:%M", tz = clock_tz),
  "\n"
)

cat(
  "Saved PNG:",
  normalizePath(output_png, mustWork = FALSE),
  "\n"
)