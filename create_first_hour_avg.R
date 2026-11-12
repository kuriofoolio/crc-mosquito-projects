################################################################################
##              Mosquito First-Hour Dusk Activity Analysis                   ##
##                                                                          ##
## Purpose:                                                                  ##
##                                                                          ##
## For each mosquito and each of the four analysis days:                    ##
##                                                                          ##
##   1. Calculate total activity for the entire day                         ##
##   2. Calculate activity during the first hour after dusk                 ##
##   3. Calculate the percentage of daily activity occurring during dusk    ##
##                                                                          ##
## Then:                                                                     ##
##                                                                          ##
##   4. Average the four daily percentages for each mosquito                ##
##   5. Compare the three experimental groups using a violin plot            ##
##                                                                          ##
## Groups:                                                                   ##
##                                                                          ##
## Female virgins:                                                          ##
##   1, 3, 5, 9, 11, 13, 17, 19, 21, 25, 27, 29                           ##
##                                                                          ##
## Males:                                                                    ##
##   2, 4, 10, 12, 18, 20, 26, 28                                          ##
##                                                                          ##
## Mated females:                                                           ##
##   6, 7, 8, 14, 15, 16, 22, 23, 24, 30, 31, 32                         ##
################################################################################


# 1. Setup ---------------------------------------------------------------------

rm(list = ls())

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  pacman,
  damr,
  zeitgebr,
  behavr,
  ggetho,
  sleepr,
  data.table,
  ggplot2
)


# 2. Configuration -------------------------------------------------------------

# Recording start time
start_time <- "2026-07-25 13:00:00"

# Directory containing the DAM file
data_dir <- "./TcEight_example"

# DAM file
monitor_file <- "Monitor13.txt"

# Output directory
output_dir <- "./violin"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# 3. Load DAM data -------------------------------------------------------------

dt <- read_dam_file(
  file.path(data_dir, monitor_file),
  region_id = 1:32,
  start_datetime = start_time
)


# 4. Assign experimental groups -----------------------------------------------

# Convert ID to numeric animal number
dt[, animal_number := as.integer(id)]


# Assign experimental group

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


# Set group order for plotting

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


# Check group assignment

cat("\n============================================\n")
cat("EXPERIMENTAL GROUPS\n")
cat("============================================\n\n")

print(
  dt[
    ,
    .(n_animals = uniqueN(animal_number)),
    by = group
  ]
)


# 5. Create actual datetime ----------------------------------------------------

# t is seconds from the recording start.
# Convert it to an actual date/time.

recording_start <- as.POSIXct(
  start_time,
  format = "%Y-%m-%d %H:%M:%S",
  tz = ""
)

dt[
  ,
  datetime := recording_start + t
]


# 6. Define analysis period ----------------------------------------------------

# We are analyzing:
#
# July 30
# July 31
# August 1
# August 2
#
# The end boundary is August 3 at 00:00 so that all of August 2
# is included.

analysis_start <- as.POSIXct(
  "2026-07-30 00:00:00",
  tz = ""
)

analysis_end <- as.POSIXct(
  "2026-08-03 00:00:00",
  tz = ""
)


# Keep only the four analysis days

dt_analysis <- dt[
  datetime >= analysis_start &
    datetime < analysis_end
]


# Create calendar date

dt_analysis[
  ,
  calendar_day := as.Date(datetime)
]


# Check dates

cat("\n============================================\n")
cat("ANALYSIS DAYS\n")
cat("============================================\n\n")

print(
  dt_analysis[
    ,
    .N,
    by = calendar_day
  ]
)


# 7. Calculate total daily activity -------------------------------------------

# For every mosquito on every day:
#
# total_daily_activity =
# sum of all activity recorded during that calendar day

daily_activity <- dt_analysis[
  ,
  .(
    total_daily_activity =
      sum(
        activity,
        na.rm = TRUE
      ),
    
    n_observations =
      sum(
        !is.na(activity)
      )
  ),
  by = .(
    id,
    animal_number,
    group,
    calendar_day
  )
]


# Save daily activity

fwrite(
  daily_activity,
  file.path(
    output_dir,
    "daily_total_activity.csv"
  )
)


# 8. Define the ACTUAL dusk times ---------------------------------------------

# IMPORTANT:
#
# These are the actual dusk times supplied for the experiment.
#
# We are NOT calculating dusk from the T-cycle.
#
# First hour after dusk:
#
# July 30   21:30 - 22:30
# July 31   21:29 - 22:29
# August 1  21:28 - 22:28
# August 2  21:26 - 22:26


dusk_schedule <- data.table(
  
  calendar_day = as.Date(
    c(
      "2026-07-30",
      "2026-07-31",
      "2026-08-01",
      "2026-08-02"
    )
  ),
  
  dusk_start = as.POSIXct(
    c(
      "2026-07-30 21:30:00",
      "2026-07-31 21:29:00",
      "2026-08-01 21:28:00",
      "2026-08-02 21:26:00"
    ),
    tz = ""
  ),
  
  dusk_end = as.POSIXct(
    c(
      "2026-07-30 22:30:00",
      "2026-07-31 22:29:00",
      "2026-08-01 22:28:00",
      "2026-08-02 22:26:00"
    ),
    tz = ""
  )
)


# Print dusk schedule

cat("\n============================================\n")
cat("DUSK SCHEDULE\n")
cat("============================================\n\n")

print(dusk_schedule)


# 9. Attach dusk times to activity data ---------------------------------------

# Match every activity observation to the dusk schedule
# based on its calendar day.

dt_analysis <- merge(
  dt_analysis,
  dusk_schedule,
  by = "calendar_day",
  all.x = TRUE
)


# 10. Extract first hour after dusk -------------------------------------------

# Keep only activity recorded during:
#
# July 30   21:30 - 22:30
# July 31   21:29 - 22:29
# August 1  21:28 - 22:28
# August 2  21:26 - 22:26

dusk_data <- dt_analysis[
  datetime >= dusk_start &
    datetime < dusk_end
]


# Check how many observations were captured

cat("\n============================================\n")
cat("FIRST HOUR OF DUSK OBSERVATIONS\n")
cat("============================================\n\n")

print(
  dusk_data[
    ,
    .N,
    by = calendar_day
  ]
)


# 11. Calculate first-hour dusk activity --------------------------------------

# Sum activity for each mosquito during the first hour after dusk.

dusk_activity <- dusk_data[
  ,
  .(
    first_hour_dusk_activity =
      sum(
        activity,
        na.rm = TRUE
      ),
    
    n_dusk_observations =
      sum(
        !is.na(activity)
      )
  ),
  by = .(
    id,
    animal_number,
    group,
    calendar_day
  )
]


# Save dusk activity

fwrite(
  dusk_activity,
  file.path(
    output_dir,
    "first_hour_dusk_activity.csv"
  )
)


# 12. Combine daily activity and dusk activity -------------------------------

percentage_data <- merge(
  daily_activity,
  dusk_activity,
  by = c(
    "id",
    "animal_number",
    "group",
    "calendar_day"
  ),
  all.x = TRUE
)


# If an animal has no activity during the dusk period,
# its dusk activity should be treated as zero rather than NA.

percentage_data[
  is.na(first_hour_dusk_activity),
  first_hour_dusk_activity := 0
]


# 13. Calculate percentage of daily activity occurring at dusk ---------------

percentage_data[
  ,
  dusk_activity_percentage :=
    fifelse(
      total_daily_activity > 0,
      
      (
        first_hour_dusk_activity /
          total_daily_activity
      ) * 100,
      
      NA_real_
    )
]


# 14. Display daily percentages -----------------------------------------------

cat("\n============================================\n")
cat("DAILY DUSK ACTIVITY PERCENTAGES\n")
cat("============================================\n\n")

print(
  percentage_data[
    ,
    .(
      id,
      group,
      calendar_day,
      total_daily_activity,
      first_hour_dusk_activity,
      dusk_activity_percentage
    )
  ]
)


# Save daily percentages

fwrite(
  percentage_data,
  file.path(
    output_dir,
    "daily_dusk_activity_percentage.csv"
  )
)


# 15. Average the four daily percentages per mosquito ------------------------

# Each mosquito should ideally have four daily percentages:
#
# July 30
# July 31
# August 1
# August 2
#
# We then calculate the mean of those percentages.
#
# This produces ONE value per mosquito for the violin plot.


mean_dusk_percentage <- percentage_data[
  ,
  .(
    mean_dusk_percentage =
      mean(
        dusk_activity_percentage,
        na.rm = TRUE
      ),
    
    sd_dusk_percentage =
      sd(
        dusk_activity_percentage,
        na.rm = TRUE
      ),
    
    n_days =
      sum(
        !is.na(dusk_activity_percentage)
      )
  ),
  by = .(
    id,
    animal_number,
    group
  )
]


# Save one value per animal

fwrite(
  mean_dusk_percentage,
  file.path(
    output_dir,
    "mean_first_hour_dusk_percentage_per_animal.csv"
  )
)


# 16. Check number of days per animal -----------------------------------------

cat("\n============================================\n")
cat("NUMBER OF DUSK MEASUREMENTS PER ANIMAL\n")
cat("============================================\n\n")

print(
  mean_dusk_percentage[
    ,
    .(
      minimum_days = min(n_days),
      maximum_days = max(n_days),
      mean_days = mean(n_days)
    ),
    by = group
  ]
)


# 17. Group-level summary ------------------------------------------------------

group_summary <- mean_dusk_percentage[
  ,
  .(
    n_animals =
      uniqueN(id),
    
    mean_percentage =
      mean(
        mean_dusk_percentage,
        na.rm = TRUE
      ),
    
    median_percentage =
      median(
        mean_dusk_percentage,
        na.rm = TRUE
      ),
    
    sd_percentage =
      sd(
        mean_dusk_percentage,
        na.rm = TRUE
      ),
    
    min_percentage =
      min(
        mean_dusk_percentage,
        na.rm = TRUE
      ),
    
    max_percentage =
      max(
        mean_dusk_percentage,
        na.rm = TRUE
      )
  ),
  by = group
]


# Save summary

fwrite(
  group_summary,
  file.path(
    output_dir,
    "first_hour_dusk_percentage_group_summary.csv"
  )
)


# Display summary

cat("\n============================================\n")
cat("GROUP SUMMARY\n")
cat("============================================\n\n")

print(group_summary)


# 18. Create violin plot -------------------------------------------------------

dusk_percentage_plot <- ggplot(
  mean_dusk_percentage,
  aes(
    x = group,
    y = mean_dusk_percentage,
    fill = group
  )
) +
  
  # Violin = distribution of mosquito-level averages
  geom_violin(
    trim = FALSE,
    alpha = 0.5
  ) +
  
  # Boxplot = median and IQR
  geom_boxplot(
    width = 0.12,
    alpha = 0.8,
    outlier.shape = NA
  ) +
  
  # Individual mosquito observations
  geom_jitter(
    width = 0.08,
    size = 2,
    alpha = 0.7
  ) +
  
  labs(
    title =
      "First-Hour-of-Dusk Activity by Experimental Group",
    
    subtitle =
      "Mean percentage of daily activity occurring during the first hour after dusk",
    
    x =
      "Experimental Group",
    
    y =
      "Mean First-Hour Dusk Activity (% of Daily Activity)"
  ) +
  
  theme_classic() +
  
  theme(
    legend.position = "none",
    
    plot.title = element_text(
      face = "bold",
      size = 13
    ),
    
    plot.subtitle = element_text(
      size = 10
    ),
    
    axis.title = element_text(
      face = "bold"
    )
  )


# Display plot

print(dusk_percentage_plot)


# 19. Save violin plot as PNG --------------------------------------------------

ggsave(
  filename = file.path(
    output_dir,
    "first_hour_dusk_activity_percentage_violin.png"
  ),
  
  plot = dusk_percentage_plot,
  
  width = 9,
  height = 6,
  dpi = 300
)


# 20. Save violin plot as PDF --------------------------------------------------

ggsave(
  filename = file.path(
    output_dir,
    "first_hour_dusk_activity_percentage_violin.pdf"
  ),
  
  plot = dusk_percentage_plot,
  
  width = 9,
  height = 6
)


# 21. Final output -------------------------------------------------------------

cat("\n============================================\n")
cat("ANALYSIS COMPLETE\n")
cat("============================================\n\n")

cat("Output directory:\n")
cat("  ", output_dir, "\n\n")

cat("Files created:\n")

cat("  daily_total_activity.csv\n")

cat("  first_hour_dusk_activity.csv\n")

cat("  daily_dusk_activity_percentage.csv\n")

cat("  mean_first_hour_dusk_percentage_per_animal.csv\n")

cat("  first_hour_dusk_percentage_group_summary.csv\n")

cat("  first_hour_dusk_activity_percentage_violin.png\n")

cat("  first_hour_dusk_activity_percentage_violin.pdf\n\n")

cat("Experimental groups:\n")

cat("  Female virgins: 12 animals\n")

cat("  Males:           8 animals\n")

cat("  Mated females: 12 animals\n\n")

cat("Dusk windows:\n")

cat("  July 30:   21:30 - 22:30\n")
cat("  July 31:   21:29 - 22:29\n")
cat("  August 1:  21:28 - 22:28\n")
cat("  August 2:  21:26 - 22:26\n\n")

cat("Each mosquito contributes ONE value to the violin plot.\n")

cat("That value is the mosquito's mean percentage of daily\n")

cat("activity occurring during the first hour after dusk.\n")