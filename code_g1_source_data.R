# =====
#  GRAPH 1 — DATA SOURCING & TRANSFORMATION
#  "The Rising Wave": global mean sea level, 1900–2023
#
#  SOURCE
#    File : epa-sea-level.csv
#    Repo : https://github.com/datasets/sea-level-rise  (open data)
#    Two independent series, both in INCHES:
#      - "CSIRO Adjusted Sea Level" : Church & White tide-gauge
#                                     reconstruction, 1880–2013
#      - "NOAA Adjusted Sea Level"  : satellite altimetry, 1993–2023
#
#  WHY TWO SERIES?
#    Tide gauges give us the long history but stop in 2013.
#    Satellites are more precise but only start in 1993.
#    So need to align them: history from CSIRO, modern era from NOAA.
#
#  OUTPUT
#    data_g1_sea_level_1900_2023.csv  ->  year, mm  (mm above the 1900 level)
# =====

library(dplyr)

# ---- 1. DOWNLOAD 
url <- "https://raw.githubusercontent.com/datasets/sea-level-rise/master/data/epa-sea-level.csv"
raw <- read.csv(url)

str(raw)
tail(raw)
# ---- 2. Convert UNITS: inches -> mm
inch2MM <- 25.4

csiro <- raw %>%
  select(year = Year, val = CSIRO.Adjusted.Sea.Level) %>%
  filter(!is.na(val)) %>%
  mutate(mm = val * inch2MM)

noaa <- raw %>%
  select(year = Year, val = NOAA.Adjusted.Sea.Level) %>%
  filter(!is.na(val)) %>%
  mutate(mm = val * inch2MM)

cat("CSIRO:", min(csiro$year), "-", max(csiro$year),
    "| NOAA:", min(noaa$year), "-", max(noaa$year), "\n")

# ---- 3. ALIGN THE TWO SERIES 
# The two datasets use different zero points (different reference
# periods), so NOAA sits at a different height. We shift NOAA up
# or down by a constant so that, over the years they BOTH cover,
# their averages match. 

overlap.years <- intersect(csiro$year, noaa$year)

offset <- mean(csiro$mm[csiro$year %in% overlap.years]) -
          mean(noaa$mm [noaa$year  %in% overlap.years])

cat("Overlap:", length(overlap.years), "years |",
    "offset applied to NOAA:", round(offset, 1), "mm\n")

# increase noaa by offset
noaa <- noaa %>% mutate(mm = mm + offset)

# ---- 4. COMBINE
# CSIRO up to and including 2013, NOAA satellite era after that.
CUT <- 2013

sea <- bind_rows(
    csiro %>% filter(year <= CUT) %>% select(year, mm),
    noaa  %>% filter(year >  CUT) %>% select(year, mm)
  ) %>%
  arrange(year)

# ---- 5. RE-BASELINE TO 1900 = 0
# The raw numbers are relative. We want the
# story "how much has it risen since 1900", so we subtract the
# 1900 value and every number becomes 'mm above the 1900 level'.

base_1900 <- sea$mm[sea$year == 1900]
sea <- sea %>%
  mutate(mm = mm - base_1900) %>%
  filter(year >= 1900)
tail(sea)
# ---- 6. CHECKS 
cat("\nFinal series:", nrow(sea), "years,",
    min(sea$year), "-", max(sea$year), "\n")
cat("Total rise since 1900:", round(tail(sea$mm, 1)), "mm\n\n")

# rate per era check
eras <- list(c(1900,1930), c(1930,1960), c(1960,1990), c(1990,2023))
for (e in eras) {
  d <- filter(sea, year >= e[1], year <= e[2])
  rate <- coef(lm(mm ~ year, data = d))[2]
  cat(sprintf("  %d-%d: %.2f mm/yr\n", e[1], e[2], rate))
}

# ---- 7. SAVE -------------------------------------------------
write.csv(sea, "data_g1_sea_level_1900_2023.csv", row.names = FALSE)


