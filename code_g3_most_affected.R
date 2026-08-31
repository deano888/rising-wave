# =====
#  GRAPH 3 (FINAL) — "Which countries are most affected?"
#  LEFT : greenhouse-gas emissions per person (tonnes CO2e)
#         Pacific = SPC OFFICIAL dataset (glowing) ; refs = EDGAR (outline)
#  RIGHT: % of people living within 10 m of the sea (LECZ 0-10 m)
#         Pacific = SPC (glowing) ; references = CIESIN (outline)
#
#  Reads:  nothing external now (values embedded + cited)
#  Writes: graph3_final_R.png
#  install.packages(c("ggplot2","dplyr","ggtext","patchwork","ragg","showtext"))
#
#  SOURCES
#   GHG per person: PACIFIC = SPC Pacific Data Hub, "Greenhouse gas
#     emission per capita" (GHG_EMI_CAPITA), 2024 [OFFICIAL challenge dataset].
#     REFERENCES = EDGAR/JRC-IEA 2025 (all GHG, tonnes CO2e, 2024).
#   Coastal exposure (LECZ 0-10 m): PACIFIC = SPC (DF_POP_LECZ, AW3D30);
#     REFERENCES = CIESIN/Columbia (McGranahan et al. 2007, SRTM);
#     China = Yang et al. 2019.
#   Excludes Palau & New Caledonia (tourism / nickel-export distortion).
# =====
library(ggplot2); library(dplyr); library(patchwork)
library(ggtext);  library(showtext); library(grid)
font_add_google("Bitter","bitter"); showtext_auto(); showtext_opts(dpi=300)
FONT <- "bitter"; set.seed(3)

# ---- LEFT DATA: GHG per person (tonnes CO2e) ----
spc_file <- "data_g3_Greenhouse_gas_emissions_per_capita.csv"  # <- your file
spc <- read.csv(spc_file, check.names = FALSE)

pac_keep <- c("Fiji","Tonga","Samoa","Vanuatu","Papua New Guinea",
              "Kiribati","Solomon Islands","Tuvalu","Marshall Islands")

ghg_pac <- spc %>%
  filter(CLIMATE_CHANGE_INDICATORS == "GHG_EMI_CAPITA") %>%
  filter(!is.na(OBS_VALUE)) %>%
  rename(country = `Pacific Island Countries and territories`,
         year = TIME_PERIOD, value = OBS_VALUE) %>%
  mutate(year = as.numeric(year),
         value = as.numeric(value)) %>% 
  filter(country %in% pac_keep) %>%
  group_by(country) %>% slice_max(year, n = 1) %>% ungroup() %>%
  transmute(country, value = round(value, 1), grp = "pacific")

# reference nations (EDGAR/JRC 2025, all-GHG tonnes CO2e, 2024) stay as typed values
ghg_ref <- tibble::tribble(
  ~country,        ~value, ~grp,
  "Qatar",         54.5, "ref",
  "Saudi Arabia",  22.8, "ref",
  # "Australia",     22.3, "ref",
  "United States", 17.3, "ref",
  "China",         10.8, "ref",
  "Japan",         8.5,  "ref",
  "India",         3.0,  "ref"
)

ghg <- bind_rows(ghg_pac, ghg_ref)

# ---- palette + labels ----
EMIT<-"#5f93b4"; ICE<-"#eafaff"; GLOW<-"#8fd6ee"; DIM<-"#9fbdd0"
sea <- linearGradient(c("#0a374a","#062236","#06162a","#0a2a44"),
                      stops=c(0,.3,.62,1), x1=.5,y1=1,x2=.5,y2=0)
short <- c("United States"="USA","Saudi Arabia"="S.ARABIA","Australia"="AUS",
           "Marshall Islands"="MARSHALL IS.","Solomon Islands"="SOLOMON IS.",
           "Papua New Guinea"="PNG","Netherlands"="NETHERLANDS","Bangladesh"="BANGLADESH")
sh <- function(x){ out <- unname(short[x]); ifelse(is.na(out), toupper(x), out) }

# =====
#  LEFT — GHG per person
# =====
ghg <- ghg %>% mutate(y=value, x=0.5+runif(n(),-.15,.15))
pac_i <- which(ghg$grp=="pacific"); ghg$x[pac_i] <- seq(0.14,0.86,length.out=length(pac_i))
ghg_named <- c("Qatar","Saudi Arabia","United States","China","India", "Japan")
ghg$x[ghg$country == "India"] <- 0.25   # move India clear of the cluster
ymax1 <- 58
left <- ggplot(ghg, aes(x,y)) +
  annotate("segment",x=.06,xend=.94,y=c(0,10,20,30,40,50),yend=c(0,10,20,30,40,50),
           colour="#3a5f76",linewidth=.3,alpha=.6) +
  annotate("text",x=.02,y=c(0,10,20,30,40),label=paste0(c(0,10,20,30,40)," t"),
           colour="white",family=FONT,size=2.8,fontface="bold",hjust=1) +
  geom_point(data=filter(ghg,grp=="ref"),shape=21,colour=EMIT,fill=NA,stroke=1.1,size=4) +
  geom_text(data=filter(ghg, country %in% ghg_named),
            aes(label=paste0(sh(country)," ",value," t")),colour=DIM,family=FONT,size=2.4,hjust=0,nudge_x=.05) +
  geom_point(data=filter(ghg,grp=="pacific"),colour=GLOW,alpha=.16,size=11) +
  geom_point(data=filter(ghg,grp=="pacific"),colour=ICE,fill=ICE,shape=21,stroke=.5,size=4.2) +
  geom_text(data=filter(ghg,grp=="pacific"),aes(label=sh(country)),colour="white",
            family=FONT,fontface="bold",size=2.1,vjust=2.4) +
  labs(title="GREENHOUSE GAS PER PERSON", subtitle="tonnes CO\u2082e, 2024") +
  scale_x_continuous(limits=c(-.02,1.18),expand=c(0,0)) +
  scale_y_continuous(limits=c(-ymax1*.16,ymax1*1.08),expand=c(0,0)) +
  theme_void(base_family=FONT) +
  theme(plot.background=element_rect(fill=sea,colour=NA),
        plot.title=element_text(colour="white",face="bold",size=15,hjust=.5,margin=margin(6,0,1,0)),
        plot.subtitle=element_text(colour=DIM,size=8.5,hjust=.5,margin=margin(0,0,4,0)),
        plot.margin=margin(10,40,10,40))

# =====
#  RIGHT — % within 10 m of the sea
# =====
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
  mutate(country = if_else(country == "Samoa", "SAMOA", country)) %>%
  mutate(country = if_else(country == "TONGA", "TONGA", country)) %>%
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
  "CHINA",             12,  "ref"
)

# rbind 2 groups
lecz <- rbind(lecz, lecz_context)

lecz <- lecz %>% mutate(y=pct, x=0.5+runif(n(),-.15,.15))
lecz$x[lecz$country == "CHINA"] <- 0.12   # move China clear of the cluster
lecz$x[lecz$country == "SAMOA"] <- 0.6   # move SAMOA clear of the cluster
lecz$x[lecz$country == "TONGA"] <- 0.7   # move TONGA clear of the cluster
atoll_i <- which(lecz$grp=="pacific" & lecz$pct>=70)
lecz$x[atoll_i] <- seq(0.2,0.8,length.out=length(atoll_i))
ymax2 <- 100
right <- ggplot(lecz, aes(x,y)) +
  annotate("segment",x=.06,xend=.94,y=c(0,25,50,75,100),yend=c(0,25,50,75,100),
           colour="#3a5f76",linewidth=.3,alpha=.6) +
  annotate("text",x=1.12,y=c(0,25,50,75,100),label=paste0(c(0,25,50,75,100),"%"),
           colour="white",family=FONT,size=3.6,fontface="bold",hjust=1) +
  # world-average dotted baseline (warm gold, right label)
  annotate("segment",x=.06,xend=.94,y=11,yend=11,colour="#eacb7a",linewidth=.6,linetype="22",alpha=.9) +
  annotate("text",x=.94,y=11,label="world average  11%",colour="#eacb7a",family=FONT,size=2.5,hjust=1,vjust=-0.8) +
  geom_point(data=filter(lecz,grp=="ref"),shape=21,colour=EMIT,fill=NA,stroke=1.1,size=4) +
  geom_text(data=filter(lecz,grp=="ref"),aes(label=paste0(sh(country)," ",pct,"%")),
            colour=DIM,family=FONT,size=2.4,hjust=0,nudge_x=.05) +
  geom_point(data=filter(lecz,grp=="pacific"),colour=GLOW,alpha=.16,size=12) +
  geom_point(data=filter(lecz,grp=="pacific"),colour=ICE,fill=ICE,shape=21,stroke=.5,size=4.4) +
  geom_text(data=filter(lecz,grp=="pacific"),aes(label=sh(country)),colour="white",
            family=FONT,fontface="bold",size=2.1,vjust=2.5) +
  labs(title="LIVING WITHIN 10 m OF THE SEA", subtitle="% of each nation's population 2024") +
  scale_x_continuous(limits=c(-.02,1.2),expand=c(0,0)) +
  scale_y_continuous(limits=c(-ymax2*.16,ymax2*1.08),expand=c(0,0)) +
  theme_void(base_family=FONT) +
  theme(plot.background=element_rect(fill=sea,colour=NA),
        plot.title=element_text(colour="white",face="bold",size=15,hjust=.5,margin=margin(6,0,1,0)),
        plot.subtitle=element_text(colour=DIM,size=8.5,hjust=.5,margin=margin(0,0,4,0)),
        plot.margin=margin(10,40,10,40))

# ---- title (glowing dot = Pacific key) + dual-source caption ----
title_md <- "<span style='color:#dff2fb;'>Pacific countries tend to emit the least Greenhouse gas yet many of their people live dangerously within reach of rising sea levels</span>"
final <- (left | right) +
  plot_annotation(
    title = title_md,
    #caption = "GHG/person \u2014 Pacific: SPC Pacific Data Hub (official); references \u25cb: EDGAR/JRC 2025. Coastal exposure \u2014 Pacific: SPC (AW3D30); references \u25cb: CIESIN (SRTM). Excludes Palau & New Caledonia (tourism / nickel-export distortion).",
    theme = theme(
      plot.background=element_rect(fill="#06162a",colour=NA),
      plot.title=element_markdown(family=FONT,size=15,hjust=.5,margin=margin(8,0,2,0)),
      plot.caption=element_text(colour="#6f93a8",family=FONT,size=5.6,hjust=.5,margin=margin(4,0,4,0))))

ggsave("graph3_final_R.png", final, width=320, height=240, units="mm",
       dpi=300, device=ragg::agg_png, bg="#06162a")

