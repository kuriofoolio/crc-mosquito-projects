################################################################################
## Mosquito T-Cycle Sex-Specific Activity Summaries
##
## Odd IDs  = Female virgin (1,3,5,...,31)
## Even IDs = Male (2,4,6,...,32)
##
## Outputs:
## 1. Per 24-hour day: total activity for each male and average across males
## 2. First hour after dusk: total activity for each female and average across females
################################################################################

rm(list = ls())

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  pacman, damr, zeitgebr, behavr, ggetho, sleepr, data.table
)

# Configuration ---------------------------------------------------------------

start_time <- "2026-07-25 13:00:00"
data_dir <- "./TcEight_example"
monitor_file <- "Monitor13.txt"

cycle_hours <- 28
light_hours <- cycle_hours / 2

# Dusk = lights-off transition, assumed to occur after 14 h of light.
dusk_offset_hours <- light_hours

# First hour after dusk
dusk_window_hours <- 1


# Load DAM data ---------------------------------------------------------------

dt <- read_dam_file(
  file.path(data_dir, monitor_file),
  region_id = 1:32,
  start_datetime = start_time
)

# Use the raw DAM activity counts.
dt[, moving := activity > 0]


# Assign sex ------------------------------------------------------------------
# 1 = female, 2 = male, 3 = female, 4 = male, etc.

dt[, animal_number := as.integer(id)]

dt[, sex := fifelse(
  animal_number %% 2 == 1,
  "Female_virgin",
  "Male"
)]


# PER DAY: MALES --------------------------------------------------------------
# First calculate total activity for each individual male on each 24-h day.

dt[, day := floor(t / (24 * 60 * 60))]

male_daily_individual <- dt[
  sex == "Male",
  .(
    total_activity = sum(activity, na.rm = TRUE),
    n_observations = sum(!is.na(activity))
  ),
  by = .(day, id)
]

# Then calculate the average of those individual male totals.

male_daily_summary <- male_daily_individual[
  ,
  .(
    total_activity_all_males = sum(total_activity, na.rm = TRUE),
    average_total_activity_per_male = mean(total_activity, na.rm = TRUE),
    sd_total_activity_per_male = sd(total_activity, na.rm = TRUE),
    n_males = uniqueN(id)
  ),
  by = day
]

male_daily_summary[
  ,
  day_start :=
    as.POSIXct(
      start_time,
      format = "%Y-%m-%d %H:%M:%S",
      tz = ""
    ) + day * 24 * 60 * 60
]

setcolorder(
  male_daily_summary,
  c(
    "day",
    "day_start",
    "total_activity_all_males",
    "average_total_activity_per_male",
    "sd_total_activity_per_male",
    "n_males"
  )
)

fwrite(male_daily_summary, "daily_male_activity_summary.csv")

# Individual male totals are also exported for later statistical analysis.
fwrite(male_daily_individual, "daily_male_activity_by_animal.csv")


# FIRST HOUR OF DUSK: FEMALES ------------------------------------------------

cycle_seconds <- cycle_hours * 60 * 60
dusk_offset_seconds <- dusk_offset_hours * 60 * 60
dusk_window_seconds <- dusk_window_hours * 60 * 60

dt[, cycle := floor(t / cycle_seconds)]
dt[, cycle_start := cycle * cycle_seconds]
dt[, dusk_start := cycle_start + dusk_offset_seconds]
dt[, dusk_end := dusk_start + dusk_window_seconds]

# Select female observations occurring during the first hour after dusk.

female_dusk_data <- dt[
  sex == "Female_virgin" &
  t >= dusk_start &
  t < dusk_end
]

# Total activity for each female during each T-cycle's first dusk hour.

female_dusk_individual <- female_dusk_data[
  ,
  .(
    total_activity = sum(activity, na.rm = TRUE),
    n_observations = sum(!is.na(activity))
  ),
  by = .(cycle, id)
]

# Average those female totals across females.

female_dusk_summary <- female_dusk_individual[
  ,
  .(
    total_activity_all_females = sum(total_activity, na.rm = TRUE),
    average_total_activity_per_female = mean(total_activity, na.rm = TRUE),
    sd_total_activity_per_female = sd(total_activity, na.rm = TRUE),
    n_females = uniqueN(id)
  ),
  by = cycle
]

female_dusk_summary[
  ,
  dusk_start_time :=
    as.POSIXct(
      start_time,
      format = "%Y-%m-%d %H:%M:%S",
      tz = ""
    ) +
    (cycle * cycle_hours + dusk_offset_hours) * 60 * 60
]

setcolorder(
  female_dusk_summary,
  c(
    "cycle",
    "dusk_start_time",
    "total_activity_all_females",
    "average_total_activity_per_female",
    "sd_total_activity_per_female",
    "n_females"
  )
)

fwrite(
  female_dusk_summary,
  "first_hour_dusk_female_activity_summary.csv"
)

# Individual female totals are also exported.
fwrite(
  female_dusk_individual,
  "first_hour_dusk_female_activity_by_animal.csv"
)


# Console output --------------------------------------------------------------

cat("\n============================================================\n")
cat("SEX-SPECIFIC MOSQUITO ACTIVITY SUMMARY COMPLETE\n")
cat("============================================================\n\n")

cat("Odd IDs = Female virgin | Even IDs = Male\n\n")

cat("DAILY MALE ACTIVITY\n")
cat("-------------------\n")
print(male_daily_summary)

cat("\nFIRST HOUR OF DUSK - FEMALE ACTIVITY\n")
cat("-------------------------------------\n")
print(female_dusk_summary)

cat("\nFiles created:\n")
cat("  daily_male_activity_summary.csv\n")
cat("  daily_male_activity_by_animal.csv\n")
cat("  first_hour_dusk_female_activity_summary.csv\n")
cat("  first_hour_dusk_female_activity_by_animal.csv\n")
cat("\nDusk: 14 h into each 28-h cycle; window = 1 h\n")
