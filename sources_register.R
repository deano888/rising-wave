# GRAPH 1 — Global mean sea level 1900-2023
#   Our World in Data (CSIRO/Church & White + NOAA)
#   https://ourworldindata.org/grapher/sea-level
#
# GRAPH 2 — "around 60% of the rise is melted ice"
#   IPCC AR6 WGI (2021), Chapter 9, Executive Summary
#   "Ocean thermal expansion (38%) and mass loss from glaciers (41%)
#    dominate the total change from 1901 to 2018."
#   https://www.ipcc.ch/report/ar6/wg1/chapter/chapter-9/
#
# GRAPH 3 — CO2 per person vs coastal exposure
#   CO2 per capita (2024): Our World in Data / Global Carbon Budget 2025 (CC BY)
#     https://ourworldindata.org/co2-emissions
#   % within 10 m of sea, PACIFIC (OFFICIAL): SPC Pacific Data Hub,
#     Population in Low Elevation Coastal Zones (DF_POP_LECZ), ALOS AW3D30
#     https://sdd.spc.int/dataset/df_pop_lecz
#   % within 10 m of sea, references (Bangladesh 46%, Netherlands ~55%, World ~11%):
#     McGranahan, Balk & Anderson (2007), Environment & Urbanization 19(1):17-37
#     https://doi.org/10.1177/0956247807076960
#   China ~12%: Yang et al. (2019), IJERPH 16(20):4012
#     https://doi.org/10.3390/ijerph16204012
#
# GRAPH 4 — Pacific leadership
#   Tokelau ~100% solar (2012): UNDP  https://www.undp.org/
#   Vanuatu ICJ opinion (23 Jul 2025): ICJ  https://www.icj-cij.org/case/187
#   Tuvalu digital nation: Govt of Tuvalu  https://www.tuvalu.tv/
#   Fiji COP23 / 1.5C advocacy: UNFCCC  https://unfccc.int/cop23
# ============================================================

sources <- data.frame(
  graph = c("1","2","3","3","3","3","4","4","4","4"),
  official = c("no","no","no","YES","no","no","no","no","no","no"),
  dataset = c(
    "Global mean sea level 1900-2023",
    "Sea-level budget (~60% melted ice)",
    "CO2 emissions per person (2024)",
    "% living within 10 m of sea - Pacific",
    "% living within 10 m of sea - Bangladesh/Netherlands/World",
    "% living within 10 m of sea - China",
    "Tokelau ~100% solar (2012)",
    "Vanuatu ICJ climate opinion (2025)",
    "Tuvalu digital nation",
    "Fiji COP23 / 1.5C advocacy"),
  source = c(
    "Our World in Data (CSIRO/Church & White + NOAA)",
    "IPCC AR6 WGI 2021, Chapter 9",
    "Our World in Data / Global Carbon Budget 2025 (CC BY)",
    "SPC Pacific Data Hub (DF_POP_LECZ, ALOS AW3D30)",
    "McGranahan, Balk & Anderson (2007), Env. & Urbanization 19(1)",
    "Yang et al. (2019), IJERPH 16(20):4012",
    "UNDP","ICJ (case 187)","Government of Tuvalu","UNFCCC"),
  url = c(
    "https://ourworldindata.org/grapher/sea-level",
    "https://www.ipcc.ch/report/ar6/wg1/chapter/chapter-9/",
    "https://ourworldindata.org/co2-emissions",
    "https://sdd.spc.int/dataset/df_pop_lecz",
    "https://doi.org/10.1177/0956247807076960",
    "https://doi.org/10.3390/ijerph16204012",
    "https://www.undp.org/","https://www.icj-cij.org/case/187",
    "https://www.tuvalu.tv/","https://unfccc.int/cop23"),
  stringsAsFactors = FALSE)

print(sources[, c("graph","official","dataset","source")], right = FALSE)
write.csv(sources, "sources.csv", row.names = FALSE)

