################################################################################
##                    Mosquito Z-Score Activity Profiles                     ##
##                                                                            ##
## Creates a line plot comparing standardized activity across:              ##
##                                                                            ##
##   1. Female virgins                                                       ##
##   2. Males                                                                ##
##   3. Mated females                                                        ##
##                                                                            ##
## Z-scores are calculated separately for each mosquito first, then          ##
## averaged within each group.                                               ##
################################################################################


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
excluded_animals <-  integer(0)
clock_tz <- "UTC"  # Preserve the monitor's written clock times consistently.
stopifnot(is.numeric(excluded_animals), !anyNA(excluded_animals),
          all(excluded_animals %in% 1:32))

data_dir <- "/Users/MAC/Desktop/R_Projects/Actograms_in_Rethomics/TcEight_example"


monitor_file <- "Monitor13_10.txt"

output_dir <- "/Users/MAC/Desktop/R_Projects/Actograms_in_Rethomics/2026_10"

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


# 7. Restrict to your four analysis days ---------------------------------------

# 7. Select analysis period ----------------------------------------------------

# TRUE = use all recorded days
# FALSE = use the date range specified below
use_all_days <- TRUE

if (use_all_days) {
  
  dt_analysis <- data.table::copy(dt)
  
} else {
  
  analysis_start <- as.POSIXct(
    "2026-07-30 00:00:00",
    tz = clock_tz
  )
  
  # Exclusive end: this includes Aug 2, but not Aug 3
  analysis_end <- as.POSIXct(
    "2026-08-03 00:00:00",
    tz = clock_tz
  )
  
  dt_analysis <- dt[
    datetime >= analysis_start &
      datetime < analysis_end
  ]
}

if (!nrow(dt_analysis)) {
  stop("No observations in the selected analysis period.")
}

# Actual dates included, used for the plot subtitle
plot_start <- min(dt_analysis$datetime, na.rm = TRUE)
plot_end   <- max(dt_analysis$datetime, na.rm = TRUE)

# 8. Create time-of-day --------------------------------------------------------

# Convert each observation into seconds since midnight.

dt_analysis[
  ,
  seconds_since_midnight :=
    as.numeric(
      format(datetime, "%H")
    ) * 3600 +
    
    as.numeric(
      format(datetime, "%M")
    ) * 60 +
    
    as.numeric(
      format(datetime, "%S")
    )
]


# Convert to hours

dt_analysis[
  ,
  hour_of_day :=
    seconds_since_midnight / 3600
]


# 9. Calculate z-score separately for each mosquito --------------------------

# IMPORTANT:
#
# Each mosquito gets its own mean and standard deviation.
#
# z = (activity - animal_mean) / animal_SD
#
# This means we are comparing activity patterns rather than absolute
# activity levels.

dt_analysis[
  ,
  z_activity :=
    if (
      sd(activity, na.rm = TRUE) > 0
    ) {
      (
        activity -
          mean(activity, na.rm = TRUE)
      ) /
        sd(activity, na.rm = TRUE)
    } else {
      0
    },
  by = id
]


# 10. Average z-scores across animals within each group -----------------------

# Because DAM measurements are every ~60 seconds, there will be many
# observations per hour.
#
# We therefore calculate the mean z-score for each:
#
# group × hour
#
# across all four days.

group_z_profile <- dt_analysis[
  ,
  .(
    mean_z_activity =
      mean(
        z_activity,
        na.rm = TRUE
      ),
    
    sd_z_activity =
      sd(
        z_activity,
        na.rm = TRUE
      ),
    
    n =
      sum(
        !is.na(z_activity)
      )
  ),
  by = .(
    group,
    hour_of_day
  )
]


# 11. Save the z-score data ----------------------------------------------------

fwrite(
  group_z_profile,
  file.path(
    output_dir,
    "group_z_score_activity_profile.csv"
  )
)


# 12. Create the line plot -----------------------------------------------------

z_score_plot <- ggplot(
  group_z_profile,
  aes(
    x = hour_of_day,
    y = mean_z_activity,
    group = group,
    color = group
  )
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  scale_color_manual(
    values = c(
      "Female virgin" = "green",
      "Male" = "blue",
      "Mated female" = "red"
    )
  )+
  
  
  scale_x_continuous(
    breaks = seq(0, 24, by = 2),
    limits = c(0, 24),
    expand = c(0, 0)
  ) +
  
  labs(
    title =
      "Standardized Activity Profiles by Experimental Group",
    
    subtitle = paste(
      "Mean individual z-scores:",
      format(plot_start, "%d %b %Y", tz = clock_tz),
      "to",
      format(plot_end, "%d %b %Y", tz = clock_tz)
    ),
    
    x =
      "Time of day (hours)",
    
    y =
      "Mean activity z-score"
  ) +
  
  theme_classic() +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    
    plot.subtitle = element_text(
      size = 10
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    legend.title = element_text(
      face = "bold"
    )
  )


# 13. Display plot -------------------------------------------------------------

print(z_score_plot)


# 14. Save PNG -----------------------------------------------------------------

ggsave(
  filename = file.path(
    output_dir,
    "group_z_score_activity_profile.png"
  ),
  
  plot = z_score_plot,
  
  width = 10,
  height = 6,
  dpi = 300
)


# 15. Save PDF -----------------------------------------------------------------

ggsave(
  filename = file.path(
    output_dir,
    "group_z_score_activity_profile.pdf"
  ),
  
  plot = z_score_plot,
  
  width = 10,
  height = 6
)


# 16. Completion message -------------------------------------------------------

cat("\n============================================\n")
cat("Z-SCORE ANALYSIS COMPLETE\n")
cat("============================================\n\n")

cat("Groups:\n")
print(dt_analysis[, .(n_animals = uniqueN(animal_number)), by = group])

cat("Z-score calculation:\n")
cat("  Calculated separately for each animal\n")
cat("  Then averaged within each experimental group\n\n")

cat("Files created:\n")
cat("  group_z_score_activity_profile.csv\n")
cat("  group_z_score_activity_profile.png\n")
cat("  group_z_score_activity_profile.pdf\n")