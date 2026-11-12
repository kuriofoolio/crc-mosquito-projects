Monitor20 plots: SLCMAD/2026/2026_07/Monitor20.txt
Source URL: https://drive.google.com/file/d/1GMhFpcFC-PNI06h78k_vzp1KUkSRGX7x/view
Local source: TcEight_example/Monitor20_07_25.txt (previously verified against full Drive text: 3,400,405 characters, FNV-1a checksum 1519449710).

Interpretation: The channel blocks and the existing Monitor20_environment.png suggest environmental measurements rather than mosquito movement. Units and sensor assignments are unverified. No sex/group assignments or animal exclusions are borrowed from Monitor13.

27,148 one-minute, end-stamped rows from 23 July through 11 August 2026. Status 1 is valid (27,049 rows); 99 other rows are treated as missing. The minute start determines calendar-day membership. UTC preserves recorded clock times without claiming the instrument's physical timezone.

Actogram: all recorded valid periods, double-plotted over 48 hours, one row per date starting at a 06:00 reference (not an asserted dawn). Each channel has an independent linear bar scale. Blue marks unavailable data. Only the 16 nonzero channels are shown. Channels 1,6,11,16-28 remain zero. Column 10 is always zero, so no light/dark schedule is inferred.

Violin: daily sums, one point per complete valid day, median diamonds. All 32 channels retained; zero-only channels have no density violin. Raw units are unverified; sums should not be interpreted as movement counts.

Z-score: standardize each channel across the selected data using its sample SD, average within each day's 30-minute clock bins, then average days equally. Zero-only channels have undefined z-scores (NA in CSV).

15 complete valid days: July 24,26,27,28,30,31 and August 1,2,4,5,6,7,8,9,10. Partial days and days with invalid records are excluded from violin and z-score analysis, retained in actograms and coverage CSV.

Reproduce from project root: Rscript generated_by_codex/monitor20/generate.R
Dependencies: R, data.table, ggplot2.
