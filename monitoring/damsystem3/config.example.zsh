# Configured for the Otousa laboratory recording Mac.
# Expected active monitors: 13 through 20, inclusive; one-minute recordings.
MONITOR_FILES=(
  "/Users/otousalab/Desktop/DAMSystem3Data/Monitor13.txt"
  "/Users/otousalab/Desktop/DAMSystem3Data/Monitor14.txt"
  "/Users/otousalab/Desktop/DAMSystem3Data/Monitor15.txt"
  "/Users/otousalab/Desktop/DAMSystem3Data/Monitor16.txt"
  "/Users/otousalab/Desktop/DAMSystem3Data/Monitor17.txt"
  "/Users/otousalab/Desktop/DAMSystem3Data/Monitor18.txt"
  "/Users/otousalab/Desktop/DAMSystem3Data/Monitor19.txt"
  "/Users/otousalab/Desktop/DAMSystem3Data/Monitor20.txt"
)
MONITOR_IDS=(13 14 15 16 17 18 19 20)

# Create two separate checks at https://healthchecks.io/ and paste their URLs.
# Keep this file private. No email password or Healthchecks management API key is needed.
COMPUTER_PING_URL="https://hc-ping.com/REPLACE_WITH_COMPUTER_CHECK_UUID"
RECORDING_PING_URL="https://hc-ping.com/REPLACE_WITH_RECORDING_CHECK_UUID"

# Starting values for files that receive a new record every minute.
# Increase this if DAMSystem3 writes to disk less frequently.
MAX_DATA_AGE_SECONDS=180
FUTURE_TOLERANCE_SECONDS=60

# Empty means the recording Mac's local timezone. If DAM writes a different
# timezone, set its IANA name here (e.g. Africa/Nairobi). Verify before arming.
DAM_TIMEZONE=""
