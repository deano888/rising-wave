# ============================================================
#  THE RISING WAVE — SOURCES (only what we used)
# ============================================================
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
# GRAPH 3 — Greenhouse-gas per person vs coastal exposure
#   GHG per person (tonnes CO2e), PACIFIC (OFFICIAL challenge dataset):
#     SPC Pacific Data Hub - "Greenhouse gas emission per capita"
#     (DF_CLIMATE_CHANGE / GHG_EMI_CAPITA), 2024.
#     https://stats.pacificdata.org/
#   GHG per person, REFERENCE nations (same metric, all GHG, 2024):
#     EDGAR / JRC-IEA 2025 Report, European Commission
#     https://edgar.jrc.ec.europa.eu/report_2025
#     Qatar 54.5 | Saudi Arabia 22.8 | Australia 22.3 | USA 17.3 |
#     China 10.8 | Japan 8.5 | Netherlands 8.4 | India 3.0 |
#     Bangladesh 1.25 | World average 6.56
#
#   % of population living within 10 m of sea level (LECZ 0-10 m):
#     PACIFIC (OFFICIAL): SPC Pacific Data Hub, Population in Low
#       Elevation Coastal Zones (DF_POP_LECZ), ALOS AW3D30.
#       https://sdd.spc.int/dataset/df_pop_lecz
#     REFERENCES (CIESIN/Columbia, SRTM, 10 m):
#       Bangladesh 46%, Netherlands ~55%, World ~11%
#         McGranahan, Balk & Anderson (2007), Environment & Urbanization 19(1):17-37
#         https://doi.org/10.1177/0956247807076960
#       China ~12%  ->  Yang et al. (2019), IJERPH 16(20):4012
#         https://doi.org/10.3390/ijerph16204012
#
#   EXCLUDED from the Pacific group (with reason):
#     * Palau (EDGAR 66.7 t) - per-capita figure is distorted: a tiny
#       resident population (~18k) with far more tourists, whose flight/
#       boat/diesel emissions sit in the numerator while tourists are
#       excluded from the denominator (IMF 2023). Not a resident footprint.
#     * New Caledonia (EDGAR 17.9 t) - dominated by nickel mining and
#       smelting for export (ores/alloys ~96% of exports); reflects
#       industrial processing for other countries, not residents' living.
#     Both are also non-sovereign territories, not Pacific nations.
#
# GRAPH 4 — Pacific leadership
#   Tokelau ~100% solar (2012): UNDP  https://www.undp.org/
#   Vanuatu ICJ opinion (23 Jul 2025): ICJ  https://www.icj-cij.org/case/187
#   Tuvalu digital nation: Govt of Tuvalu  https://www.tuvalu.tv/
#   Fiji COP23 / 1.5C advocacy: UNFCCC  https://unfccc.int/cop23
#
#   OFFICIAL challenge datasets used (compliance):
#     - Greenhouse gas emission per capita (GHG_EMI_CAPITA)  [graph 3]
#     - (Coastal-exposure LECZ is SPC data but NOT on the official list)

# EDGAR reference figures for graph 3 (all-GHG, tonnes CO2e/person, 2024)
edgar_ref <- data.frame(
  country = c("Qatar","Saudi Arabia","Australia","United States","China",
              "Japan","Netherlands","India","Bangladesh","World (average)"),
  ghg_pc  = c(54.5, 22.8, 22.3, 17.3, 10.8, 8.5, 8.4, 3.0, 1.25, 6.56),
  source  = "EDGAR/JRC-IEA 2025",
  stringsAsFactors = FALSE
)


