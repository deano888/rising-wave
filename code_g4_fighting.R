# =====
#  GRAPH 4 — "WE ARE NOT DROWNING. WE ARE FIGHTING."
#  Sunlit water; four islands glow as points of light, each a
#  verified act of Pacific climate leadership.
#
#  Writes: graph4_fighting.png   
# =====

library(ggplot2); library(dplyr); library(ggtext)
library(showtext); library(grid)

font_add_google("Bitter", "bitter"); showtext_auto(); showtext_opts(dpi = 300)
FONT <- "bitter"

set.seed(7)

# ============================================================
#  TEXT COLOURs
#  Three presets; flip ONE line to switch the whole poster.
# ------------------------------------------------------------
#  "deep"    = deep sea-teal ink (current look; elegant, subtle)
#  "white"   = crisp white (max pop, movie-poster punch)
#  "pacific" = warm sunlit gold headline + white tags (island sunset)
palette_choice <- "deep"

pal <- list(
  deep    = list(head="#0f3a4a", sub="#1c4f5e", name="#0d3644", body="#164a58",
                 shadow=NA),
  white   = list(head="#ffffff", sub="#eaf7f2", name="#ffffff", body="#eefcff",
                 shadow="#0a2b38"),                 # dark halo so white reads on light top
  pacific = list(head="#ffd77a", sub="#ffe9c2", name="#ffffff", body="#d9f3ee",
                 shadow="#0a2b38")
)[[palette_choice]]

# ---- sunlit-water background
grad_stops <- c("#1c566880","#2a7884","#4aa4a8","#7ec6c4","#c4e0d2","#eee2c4","#d2c4d6")
sky <- linearGradient(c("#1c5668","#2a7884","#4aa4a8","#7ec6c4","#c4e0d2","#eee2c4","#d2c4d6"),
                      stops = c(0,.22,.45,.66,.80,.90,1), x1=.5,y1=0,x2=.5,y2=1)

# sparkle data (denser & brighter toward the top)
n <- 460
bok <- data.frame(x = runif(n), y = runif(n)^0.7)            # bias upward
bok$keep <- runif(n) < (bok$y*0.9 + 0.1)
bok <- bok[bok$keep,]
bok$size  <- runif(nrow(bok), .3, 4.2) * (0.5 + bok$y)
bok$alpha <- runif(nrow(bok), .12, .55) * (0.5 + bok$y)
bok$col   <- sample(c("#ffffff","#fdf3d0","#eaf7f2"), nrow(bok), replace=TRUE)

# ---- the four glowing island tags (verified) ----
tags <- data.frame(
  x    = c(0.26, 0.72, 0.25, 0.72),
  y    = c(0.60, 0.60, 0.34, 0.34),
  name = c("TOKELAU","VANUATU","TUVALU","FIJI  &  THE PACIFIC"),
  body = c("First nation to reach<br>100% solar power (2012)",
           "Took climate justice to the<br>World Court \u2014 and won (2025)",
           "Building the world's first<br>digital nation to survive the sea",
           "Chaired COP23 and drove<br>the global 1.5\u00b0C goal"))

# draw text with an optional soft shadow (halo) for pop
lab <- function(g, x, y, label, size, colour, shadow, fontface="plain", md=FALSE){
  geomfun <- if (md) geom_richtext else annotate
  if (!is.na(shadow)) {
    for (dx in c(-1,1)*0.0016) for (dy in c(-1,1)*0.0016)
      g <- g + annotate("text", x=x+dx, y=y+dy, label=gsub("<br>","\n",gsub("<.*?>","",label)),
                        colour=shadow, family=FONT, size=size, fontface=fontface,
                        lineheight=.95, alpha=.5)
  }
  g
}

p <- ggplot() +
  # background
  annotate("rect", xmin=0, xmax=1, ymin=0, ymax=1, fill=sky) +
  # ripples
  annotate("segment",
           x=runif(55,0,.75), xend=runif(55,0,.75)+runif(55,.05,.28),
           y=runif(55,0,.8),  yend=runif(55,0,.8),
           colour="white", alpha=runif(55,.03,.12), linewidth=runif(55,.2,1)) +
  # bokeh
  geom_point(data=bok, aes(x,y), colour=bok$col, size=bok$size, alpha=bok$alpha) +
  coord_cartesian(xlim=c(0,1), ylim=c(0,1), expand=FALSE, clip="off") +
  theme_void()

# ---- headline (with optional shadow for white/pacific) ----
if (!is.na(pal$shadow)) {
  p <- p +
    annotate("text", x=.5+.002, y=.905-.002, label="WE ARE NOT DROWNING.",
             colour=pal$shadow, family=FONT, fontface="bold", size=13, alpha=.45) +
    annotate("text", x=.5+.002, y=.855-.002, label="WE ARE FIGHTING.",
             colour=pal$shadow, family=FONT, fontface="bold", size=13, alpha=.45)
}
p <- p +
  annotate("text", x=.5, y=.905, label="WE ARE NOT DROWNING.",
           colour=pal$head, family=FONT, fontface="bold", size=13) +
  annotate("text", x=.5, y=.855, label="WE ARE FIGHTING.",
           colour=pal$head, family=FONT, fontface="bold", size=13) +
  annotate("text", x=.5, y=.815, label="The Pacific nations who have caused less damage are leading the way",
           colour=pal$sub, family=FONT, fontface="italic", size=7)

# ---- glowing island lights ----
for (i in seq_len(nrow(tags))) {
  tx <- tags$x[i]; ty <- tags$y[i]
  p <- p +
    annotate("point", x=tx, y=ty, size=26, colour="white", alpha=.10) +
    annotate("point", x=tx, y=ty, size=13, colour="white", alpha=.20) +
    annotate("point", x=tx, y=ty, size=4.5, colour="white", alpha=.95)
}

# ---- tag text (rich text for the <br> line breaks) ----
p <- p +
  geom_richtext(data=tags, aes(x=x, y=y-0.05, label=name),
                colour="white", family=FONT, fontface="bold", size=6.5,
                # colour=pal$name, family=FONT, fontface="bold", size=5,
                fill=NA, label.color=NA) +
  geom_richtext(data=tags, aes(x=x, y=y-0.088, label=body),
                colour="white", family=FONT, size=4.5,
                #colour=pal$body, family=FONT, size=3.6,
                fill=NA, label.color=NA, lineheight=1.3) +
  annotate("text", x=.5, y=.02,
           label="Sources: UNDP (Tokelau) \u00b7 ICJ Advisory Opinion 23 Jul 2025 (Vanuatu) \u00b7 Tuvalu Digital Nation \u00b7 UNFCCC COP23",
           colour="#2b5b68", family=FONT, size=3, alpha=.8)

ggsave("graph4_fighting.png", p, 
       width = 320, height = 240, 
       units = "mm", dpi = 300, device = ragg::agg_png)

