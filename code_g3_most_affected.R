# =====
#  GRAPH 3 "Which countries are most affected?"
#  LEFT : CO2 per person        (real, Our World in Data 2024)
#  RIGHT: % of people living within 10 m of the sea  (LECZ 0-10m)
#         Pacific = SPC (official) ; references = CIESIN (outline)
#
#  Reads:  co-emissions-per-capita.csv    
#  Writes: graph3_most_affected.png
# =====
library(ggplot2); library(dplyr); library(patchwork)
library(ggtext);  library(showtext); library(grid)
font_add_google("Bitter","bitter"); showtext_auto(); showtext_opts(dpi=300)
FONT <- "bitter"; set.seed(3)

# =====
#  SOURCES (every figure traceable)
#  CO2 per capita (tonnes/person): Our World in Data, latest year from
#     your CSV. Underlying: Global Carbon Budget 2025. Licence CC BY.
#     https://ourworldindata.org/co2-emissions
#  % population within 10 m of sea (LECZ 0-10 m):
#   - PACIFIC (OFFICIAL): SPC Pacific Data Hub "Population in Low
#     Elevation Coastal Zones" (DF_POP_LECZ), ALOS AW3D30, 2024.
#     https://sdd.spc.int/dataset/df_pop_lecz
#   - REFERENCES (CIESIN/Columbia, SRTM, 10 m):
#       Bangladesh 46 / Netherlands ~55 / World ~11
#         McGranahan, Balk & Anderson (2007), Environment & Urbanization 19(1):17-37
#       China ~12  -> Yang et al. (2019), IJERPH 16(20):4012
#   Pacific uses AW3D30; references use SRTM -> references drawn as
#   OUTLINE dots, labelled as a different source.
# =====

### left graph
co2raw <- read.csv("data_g3_co-emissions-per-capita.csv", check.names = FALSE)
names(co2raw) <- c("Entity","Code","Year","co2")
co2 <- co2raw %>% group_by(Entity) %>% slice_max(Year, n=1) %>% ungroup()
co2_keep <- c("Qatar","Saudi Arabia","Australia","United States","China","Japan","India",
              "United Kingdom","Netherlands","Bangladesh",
              "Tuvalu","Kiribati","Marshall Islands","Tonga","Samoa","Vanuatu",
              "Fiji","Solomon Islands","Papua New Guinea","Nauru")
pac <- c("Tuvalu","Kiribati","Marshall Islands","Tonga","Samoa","Vanuatu",
         "Fiji","Solomon Islands","Papua New Guinea","Nauru")
co2d <- co2 %>% filter(Entity %in% co2_keep) %>%
  transmute(country=Entity, value=co2, pacific=Entity %in% pac)

EMIT<-"#5f93b4"; ICE<-"#eafaff"; GLOW<-"#8fd6ee"; DIM<-"#9fbdd0"
sea <- linearGradient(c("#0a374a","#062236","#06162a","#0a2a44"),
                      stops=c(0,.3,.62,1), x1=.5,y1=1,x2=.5,y2=0)
short <- c("United States"="USA","United Kingdom"="UK","Saudi Arabia"="S.ARABIA",
           "Australia"="AUS","Marshall Islands"="MARSHALL IS.","Solomon Islands"="SOLOMON IS.",
           "Papua New Guinea"="PNG","Netherlands"="NETHERLANDS","Bangladesh"="BANGLADESH",
           "World (average)"="WORLD AVG")
sh <- function(x) {
  out <- unname(short[x])          # single brackets: works on a vector
  ifelse(is.na(out), toupper(x), out)   # fall back to UPPERCASE if not in the lookup
}

co2d <- co2d %>% mutate(y=value, x=0.5+runif(n(),-.16,.16))
pac_i <- which(co2d$pacific); co2d$x[pac_i] <- seq(0.14,0.86,length.out=length(pac_i))
co2_named <- c("Qatar","Saudi Arabia","Australia","United States","China","India")
ymax1 <- 45
left <- ggplot(co2d, aes(x,y)) +
  annotate("segment",x=.06,xend=.94,y=c(0,10,20,30,40),yend=c(0,10,20,30,40),
           colour="#3a5f76",linewidth=.3,alpha=.6) +
  annotate("text",x=.02,y=c(0,10,20,30,40),label=paste0(c(0,10,20,30,40)," t"),
           colour="white",family=FONT,size=2.8,fontface="bold",hjust=1) +
  geom_point(data=filter(co2d,!pacific),colour=EMIT,alpha=.5,size=3) +
  geom_point(data=filter(co2d,pacific),colour=GLOW,alpha=.16,size=11) +
  geom_point(data=filter(co2d,pacific),colour=ICE,fill=ICE,shape=21,stroke=.5,size=4.2) +
  geom_text(data=filter(co2d, country %in% co2_named),
            aes(label=sh(country)),colour=DIM,family=FONT,size=2.4,hjust=0,nudge_x=.05) +
  geom_text(data=filter(co2d,pacific),aes(label=sh(country)),colour="white",
            family=FONT,fontface="bold",size=2.2,vjust=2.4) +
  labs(title="CO\u2082 PER PERSON", subtitle="tonnes/person, 2024 \u00b7 Our World in Data") +
  scale_x_continuous(limits=c(-.02,1.15),expand=c(0,0)) +
  scale_y_continuous(limits=c(-ymax1*.16,ymax1*1.08),expand=c(0,0)) +
  theme_void(base_family=FONT) +
  theme(plot.background=element_rect(fill=sea,colour=NA),
        plot.title=element_text(colour="white",face="bold",size=15,hjust=.5,margin=margin(6,0,1,0)),
        plot.subtitle=element_text(colour=DIM,size=8.5,hjust=.5,margin=margin(0,0,4,0)),
        plot.margin=margin(4,6,4,6))

## right graph

# load percent living 10m official data
lecz <- read.csv("data_g3_pacific_islands_living_below_10m.csv")
countries_wanted <- c("Marshall Islands", "Tuvalu",  
                      "Nauru",     
                      "Kiribati", 
                      "Fiji",    
                      "Tonga",    
                      "Samoa",    
                      "Vanuatu",    
                      "Solomon Islands", 
                      "Papua New Guinea")
lecz <- lecz %>% 
  filter(Pacific.Island.Countries.and.territories %in% countries_wanted &
           TIME_PERIOD == 2024 & 
           ELEVATION == "10M" &
           UNIT_MEASURE == "PERCENT") %>% 
  select(Pacific.Island.Countries.and.territories, INDICATOR, ELEVATION, Elevation, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE) %>% 
  rename(country = Pacific.Island.Countries.and.territories, 
         pct = OBS_VALUE) %>% 
  mutate(country = if_else(country == "Papua New Guinea", "PNG", country)) %>% 
  mutate(country = if_else(country == "Solomon Islands", "SOLOMON IS.", country)) %>%  
  mutate(country = if_else(country == "Marshall Islands", "MARSHALL IS.", country))    

# confirmed this is what we want now prep for rbind with other countries
lecz <- lecz %>% 
  select(country, pct) %>% 
  mutate(grp = "pacific")

# source other countries from internet 10M pct for context, although does have different measure
lecz_context <- tibble::tribble(
  ~country,            ~pct, ~grp,
  "Netherlands",       55,  "ref",
  "Bangladesh",        46,  "ref",
  "China",             12,  "ref"
)

# rbind 2 groups
lecz <- rbind(lecz, lecz_context)


lecz <- lecz %>% mutate(y=pct, x=0.5+runif(n(),-.15,.15))
atoll_i <- which(lecz$grp=="pacific" & lecz$pct>=70)
lecz$x[atoll_i] <- seq(0.2,0.8,length.out=length(atoll_i))
lecz$x[lecz$country == "China"] <- 0.4   # move China clear of the cluster
lecz$x[lecz$country == "Samoa"] <- 0.80   # move Samoa clear of the cluster
lecz$x[lecz$country == "SOLOMON IS."] <- 0.57   # move SOl IS. clear of the cluster
lecz$x[lecz$country == "Vanuatu"] <- 0.65   # move Vanuatu clear of the cluster
lecz$x[lecz$country == "PNG"] <- 0.3   # move PNG clear of the cluster
lecz$x[lecz$country == "Fiji"] <- 0.32   # move Fiji clear of the cluster
lecz_dots <- filter(lecz, country != "World (average)")
ymax2 <- 100
right <- ggplot(lecz, aes(x,y)) +
  annotate("segment",x=.06,xend=.94,y=c(0,25,50,75,100),yend=c(0,25,50,75,100),
           colour="#3a5f76",linewidth=.3,alpha=.6) +
  annotate("text",x=1.12,y=c(0,25,50,75,100),label=paste0(c(0,25,50,75,100),"%"),
           colour="#d6ecf7",family=FONT,size=3.2,fontface="bold",hjust=1) +
  geom_point(data=filter(lecz,grp=="ref"),shape=21,colour=EMIT,fill=NA,stroke=1.1,size=4) +
  geom_text(data=filter(lecz,grp=="ref"),aes(label=paste0(sh(country)," ",pct,"%")),
            colour=DIM,family=FONT,size=2.4,hjust=0,nudge_x=.05) +
  geom_point(data=filter(lecz,grp=="pacific"),colour=GLOW,alpha=.16,size=12) +
  geom_point(data=filter(lecz,grp=="pacific"),colour=ICE,fill=ICE,shape=21,stroke=.5,size=4.4) +
  geom_text(data=filter(lecz,grp=="pacific"),aes(label=sh(country)),colour="white",
            family=FONT,fontface="bold",size=2.1,vjust=2.5) +
  # ad world ave line
  annotate("segment", x = .06, xend = .94, y = 11, yend = 11,
           colour = "#eacb7a", linewidth = .6, linetype = "22", alpha = .9) +
  annotate("text", x = .23, y = 11, label = "world average  11%",
           colour = "#eacb7a", family = FONT, size = 2.6, hjust = 1, vjust = -0.8) +
  labs(title="LIVING WITHIN 10 m OF THE SEA", subtitle="% of each nation's population") +
  scale_x_continuous(limits=c(-.02,1.2),expand=c(0,0)) +
  scale_y_continuous(limits=c(-ymax2*.16,ymax2*1.08),expand=c(0,0)) +
  theme_void(base_family=FONT) +
  theme(plot.background=element_rect(fill=sea,colour=NA),
        plot.title=element_text(colour="white",face="bold",size=15,hjust=.5,margin=margin(6,0,1,0)),
        plot.subtitle=element_text(colour=DIM,size=8.5,hjust=.5,margin=margin(0,0,4,0)),
        plot.margin=margin(4,6,4,6))
# title_md <- "<span style='color:#eafaff;'>\u25cf</span>  <span style='color:#dff2fb;'>Pacific countries tend to emit the least CO\u2082 \u2014 yet many of their people live dangerously within reach of a rising sea levels</span>"
title_md <- "<span style='color:#dff2fb;'>Pacific countries tend to emit the least CO\u2082 \u2014 yet many of their people live dangerously within reach of a rising sea levels</span>"
#title_md <- "Pacific countries tend to emit the least CO\u2082 \u2014 yet many of their people live dangerously within reach of a rising sea levels"
final <- (left | right) +
  plot_annotation(
    title = title_md,
    caption = "CO\u2082: Our World in Data / Global Carbon Budget 2025 (CC BY). Coastal exposure \u2014 Pacific: SPC Pacific Data Hub (ALOS AW3D30, 0-10 m); reference nations \u25cb: CIESIN/Columbia (SRTM, 0-10 m).",
    theme = theme(
      plot.background=element_rect(fill="#06162a",colour=NA),
      plot.title=element_markdown(family=FONT,size=15,hjust=.5,margin=margin(8,0,2,0)),
      plot.caption=element_text(colour="#6f93a8",family=FONT,size=5.8,hjust=.5,margin=margin(4,0,4,0))))

ggsave("graph3_most_affected.png", final,
       width = 320, height = 240, 
       units="mm",dpi=300, device=ragg::agg_png, bg="#06162a")
