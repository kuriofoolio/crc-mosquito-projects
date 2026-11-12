library(data.table)

# 1. Pull out just this individual's data
mosq24 <- dt28[id == "Monitor13_filled_to_2026-08-02.txt|24"]  
# adjust the id string to match exactly what's in your data — 
# run unique(dt28$id) first if you're not sure of the exact format

# 2. Look at the raw activity values, sorted by time
mosq24 <- mosq24[order(t)]
head(mosq24[, .(t, activity, moving)], 20)

# 3. Check for suspiciously uniform values
# Real DAM activity counts usually vary (0, 1, 2, 3, 5, 8...)
# If filled/interpolated data was inserted, it's often flat or repeated
table(mosq24$activity)

# 4. Specifically isolate the "lonely tick" points
# These are rows where activity > 0 but the surrounding rows are all 0
# i.e., a single spike with no neighbors — a red flag for a filled artifact

mosq24[, activity_prev := shift(activity, 1, type = "lag")]
mosq24[, activity_next := shift(activity, 1, type = "lead")]

isolated_points <- mosq24[activity > 0 & activity_prev == 0 & activity_next == 0]
nrow(isolated_points)
isolated_points[, .(t, activity)]

# 5. Compare the VALUE of isolated points to the rest of the data
# If every isolated point has the exact same activity value (e.g., all = 1),
# that's a strong sign of synthetic/placeholder insertion rather than real recording
table(isolated_points$activity)

# 6. Bonus — check if isolated points cluster at suspiciously regular time gaps
# (e.g., every isolated point exactly X hours apart could indicate a fill algorithm
# stamping in a placeholder at fixed intervals)
diff(isolated_points$t)