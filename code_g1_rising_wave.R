# =====
#  THE RISING WAVE  
#
#  Reads:  data_g1_sea_level_1900_2023
#  Writes: graph1_rising_wave.png
# =====

library(ggplot2)
library(dplyr)
library(grid)
library(showtext)

# ---- FONT: Bitter 
font_add_google("Bitter", "bitter")
showtext_auto()
showtext_opts(dpi = 300)      # keeps text the right size when saving at 300 dpi
FONT <- "bitter"

# ---- 1. DATA 
df <- read.csv("data_g1_sea_level_1900_2023.csv")
# use spline to create smooth wave line through 124 years
sm <- as.data.frame(spline(df$year, df$mm, n = 900))
names(sm) <- c("year", "mm")

# added to position xlabs
years_df <- data.frame(
  x = c(1901, 1925, 1950, 1975, 2000, 2022),
  lab = c(1900, 1925, 1950, 1975, 2000, 2023),
  h = c(0, .5, .5, .5, .5, 1)          # first left-aligned, last right-aligned
)
# ---- 2. PALETTE
sky <- linearGradient(c("#FDE9C9", "#FFD79E", "#F2A97C", "#D9776A", "#A85A62"),
                      x1 = .5, y1 = 1, x2 = .5, y2 = 0)
ocean <- linearGradient(c("#D4F1F4", "#189AB4", "#05445E"),
                        x1 = .5, y1 = 1, x2 = .5, y2 = 0)

INK <- "#7d3f2f"   # warm dark text, reads on the peach sky

# ---- 3. GEOMETRY ----
BASE <- -70
TOP  <- max(sm$mm) + 45
yr   <- c(1900, 1925, 1950, 1975, 2000, 2023)

# ---- 4. FOAM 
set.seed(7)
crest <- subset(sm, year > max(sm$year) - 12)
foam  <- data.frame(year = runif(130, min(crest$year), max(crest$year)))
foam$mm <- approx(sm$year, sm$mm, foam$year)$y + runif(130, -4, 12)
foam$sz <- runif(130, 0.2, 2.4)
foam$al <- runif(130, 0.3, 0.9)

# ---- 5. PLOT 
p <- ggplot() +
  # faint guide lines where the y-axis used to be
  annotate("segment", x = 1900, xend = 2023, y = c(100, 200), yend = c(100, 200),
           colour = INK, alpha = .18, linewidth = .3) +

  # the ocean: one body of water, seabed up to the real data line
  geom_ribbon(data = sm, aes(x = year, ymin = BASE, ymax = mm), fill = ocean, colour = NA) +
  geom_line(data = sm, aes(year, mm), colour = "white",   linewidth = 2.6, alpha = .12) +
  geom_line(data = sm, aes(year, mm), colour = "#eaf5ff", linewidth = 0.9) +
  geom_point(data = df, aes(year, mm), colour = "#eaf5ff", size = 1.1, alpha = .5) +
  geom_point(data = foam, aes(year, mm, size = sz, alpha = al), colour = "white") +
  scale_size_identity() + scale_alpha_identity() +

  # ---- scale as fixed annotations (left, on the sky) ----
  annotate("text", x = 1902, y = c(100, 200) + 9, label = c("+100 mm", "+200 mm"),
           colour = INK, family = FONT, size = 3.4, hjust = 0, alpha = .85) +
  # the headline number, at the crest
  annotate("text", x = 2015, y = 235, 
           label = "Global sea levels have risen 2.35m since 1900 — and are rising at an increased rate",
           colour = INK, family = FONT, size = 8, fontface = "bold", hjust = 1) +
  geom_text(data = years_df,
          aes(x = x, y = BASE * 0.55, label = lab, hjust = h),
          colour = "white", family = FONT, size = 3.4, alpha = .9) +
  
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(1900, 2023), ylim = c(BASE, TOP), clip = "off") +

  theme_void(base_family = FONT) +
  theme(
    panel.background = element_rect(fill = NA,  colour = NA),
    plot.background  = element_rect(fill = sky, colour = NA),
    # plot.margin      = margin(0, 0, 0, 0)      # <- edge to edge, no border
    plot.margin = margin(14, 44, 14, 44)   # generous left/right so dates + mm labels survive
  )

# ---- 6. EXPORT 
ggsave("graph1_rising_wave.png", p,
       width = 320, height = 240, units = "mm", dpi = 300,
       device = ragg::agg_png)

