# =====
#  GRAPH 2 — "WHAT IS THE SEA MADE OF?"
#  The same rising wave as graph 1 but filled with the two parts
#  warm water expanding + melted ice.
#
#  Reads:  data_g1_sea_level_1900_2023.csv   (output data of code_g1_source_data)
#  Writes: graph2_sea_made_of.png
# =====

library(ggplot2)
library(dplyr)
library(grid)
library(showtext)

font_add_google("Bitter", "bitter"); showtext_auto(); showtext_opts(dpi = 300)
FONT <- "bitter"

# ---- 1. DATA 
df <- read.csv("data_g1_sea_level_1900_2023.csv")

# ---- 2. THE SPLIT 
#  (IPCC AR6 / Frederikse 2020) put melted ice at
#  roughly 60% of the rise, with its share GROWING over time
#  as the ice sheets accelerate. We apply that proportion to the
#  real measured total. 

melted_ice_percent <- 0.6  
warm_water_percent   <- 0.4      

df <- df %>%
  mutate(
    warm_mm  = mm * warm_water_percent,     # warm water
    ice_mm   = mm * melted_ice_percent            
  )
tail(df)

# smooth both surfaces (interpolation only — passes through every real point)
s_warm <- as.data.frame(spline(df$year, df$warm_mm, n = 900)); names(s_warm) <- c("year","warm")
s_tot  <- as.data.frame(spline(df$year, df$mm,      n = 900)); names(s_tot)  <- c("year","tot")
sm <- left_join(s_warm, s_tot, by = "year")

# ---- 3. PALETTE 
#  Opens in the dark teal that graph 1's deep ocean closes on,
#  then warms downward to a low orange glow at the waterline.
sky <- linearGradient(c("#63706B",   
                        "#B8977A",   
                        "#EF8446"), 
                      x1 = .5, y1 = 1, x2 = .5, y2 = 0)

# melted ice: bright, cold
ice_fill  <- linearGradient(c("#F6FDFF", "#AFDDEE"), x1 = .5, y1 = 1, x2 = .5, y2 = 0)

# warm water: contrasts with the pale ice
warm_fill <- linearGradient(c("#2A7FB8", "#06263D"), x1 = .5, y1 = 1, x2 = .5, y2 = 0)

# ----
INK      <- "#F3E6D6"   # light text, for the dark upper sky
INK_DARK <- "#123449"   # dark text, for ON the pale ice

# ---- 4. GEOMETRY
BASE <- -35
TOP  <- max(sm$tot) + 45
yr   <- c(1900, 1925, 1950, 1975, 2000, 2023)
years_df <- data.frame(x = c(1901,1925,1950,1975,2000,2022),
                       lab = yr, h = c(0,.5,.5,.5,.5,1))

# ---- 5. FOAM on the crest 
set.seed(7)
crest <- subset(sm, year > max(sm$year) - 12)
foam  <- data.frame(year = runif(130, min(crest$year), max(crest$year)))
foam$mm <- approx(sm$year, sm$tot, foam$year)$y + runif(130, -4, 12)
foam$sz <- runif(130, 0.2, 2.4); foam$al <- runif(130, 0.3, 0.9)

# ---- 6. PLOT 
p <- ggplot() +
  annotate("segment", x = 1900, xend = 2023, y = c(100,200), yend = c(100,200),
           colour = INK, alpha = .16, linewidth = .3) +
  ## y axis
  annotate("text", x = 1902, y = c(100, 200) + 9, label = c("+100 mm", "+200 mm"),
           colour = INK, family = FONT, size = 3.4, hjust = 0, alpha = .85) +
  
  # LOWER BAND: warm water, expanding
  geom_ribbon(data = sm, aes(x = year, ymin = BASE, ymax = warm),
              fill = warm_fill, colour = NA) +
  # UPPER BAND: melted ice
  geom_ribbon(data = sm, aes(x = year, ymin = warm, ymax = tot),
              fill = ice_fill, colour = NA) +
  
  # the boundary between them, and the sea surface
  geom_line(data = sm, aes(year, warm), colour = "#8FC7DC", linewidth = .5, alpha = .8) +
  geom_line(data = sm, aes(year, tot),  colour = "white",   linewidth = 2.6, alpha = .12) +
  geom_line(data = sm, aes(year, tot),  colour = "#F2FBFF", linewidth = .9) +
  geom_point(data = foam, aes(year, mm, size = sz, alpha = al), colour = "white") +
  scale_size_identity() + scale_alpha_identity() +
  
  # sub-zero footer — its OWN colour (not ocean)
  geom_ribbon(data = sm, aes(x = year, ymin = BASE, ymax = 0),
              fill = INK_DARK, colour = NA) +    
  # warm water, expanding 
  geom_ribbon(data = sm, aes(x = year, ymin = pmax(warm, 0), ymax = tot),
              fill = ice_fill, colour = NA) +
  geom_ribbon(data = sm, aes(x = year, ymin = 0, ymax = pmax(warm, 0)),
              fill = warm_fill, colour = NA) +

  # ---- labels
  annotate("text", x = 2000, y = 100, label = "MELTED ICE",
         colour = INK_DARK, family = FONT, size = 5, fontface = "bold") +
  annotate("text", x = 2000, y = 30, label = "Warm Water expanding",
           colour = "#AFDDEE", family = FONT, size = 3.6) +
  
  #  scale + headline
  ##annotate("text", x = 1902, y = c(100,200) + 9, label = c("+100 mm","+200 mm")) +
  annotate("text", x = 2000, y = 235, 
           label = "Data suggests that around 60% of the sea rise is due to MELTED ICE ",
           colour = INK, family = FONT, size = 7, fontface = "bold", hjust = 1) +
  geom_text(data = years_df, aes(x = x, y = BASE * .55, label = lab, hjust = h),
            colour = "#AFDDEE", family = FONT, size = 3.4, alpha = .9) +
  
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(1900,2023), ylim = c(BASE,TOP), clip = "off") +
  theme_void(base_family = FONT) +
  theme(panel.background = element_rect(fill = NA,  colour = NA),
        plot.background  = element_rect(fill = sky, colour = NA),
        plot.margin      = margin(0,0,0,0))

ggsave("graph2_sea_made_of.png", p, 
       width = 320, height = 240, 
       units = "mm", dpi = 300, device = ragg::agg_png)
