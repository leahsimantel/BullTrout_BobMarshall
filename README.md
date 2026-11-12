# Chapter 2 Public Spatial Data

This README documents the public spatial data sources for the Chapter 2 Analysis R script. The filenames below identify the files used in the analysis. Some are project-prepared versions of the public downloads.

## NHDPlus stream flowlines

- **Public source:** [U.S. EPA NHDPlus Version 2.1 — Pacific Northwest, Vector Processing Unit 17](https://www.epa.gov/waterdata/nhdplus-pacific-northwest-data-vector-processing-unit-17)
- **Script input:** `NHDFlowline_Network_clipped_NoLakes.shp`
- **Preparation:** The script uses a project copy of the NHDFlowline network clipped to the study area with lake segments removed. The public download provides the regional network.

## HUC12 watershed boundaries

- **Public source:** [USGS Watershed Boundary Dataset (WBD)](https://www.usgs.gov/national-hydrography/access-national-hydrography-products#Watershed)
- **Script input:** `HUC12_BMWA`, a layer in `BT_model_comparisons.gdb`
- **Preparation:** The project layer contains 12-digit hydrologic unit boundaries for the Bob Marshall Wilderness Area study area. To reproduce the script as written, use the project geodatabase or export the study-area boundaries to a file geodatabase with the same layer name.

## Fire perimeters

- **Cannon fire perimeter:** [Monitoring Trends in Burn Severity (MTBS) downloads](https://burnseverity.cr.usgs.gov/direct-download). Search for event ID `MT4771611356020220807` in the **2022** event files. The script reads `mt4771611356020220807_20220801_20230722_burn_bndy.shp`.
- **Historical fire atlas:** The script also reads `fire_perimeters_cce_1972_2022.shp`. The [Aldo Leopold Wilderness Research Institute archive](https://www.leopold.wilderness.net/publications-and-data/archived-data/) lists the Crown of the Continent Ecosystem fire atlas for **1972–2012**. Use the project’s **1972–2022** copy to reproduce the analysis. The public archive link documents the earlier release.

## Original Climate Shield bull trout models — Isaak et al. (2015)

Download the **SpoKoot bull trout BK0** shapefiles from the [USDA Forest Service Cold-Water Climate Shield project](https://research.fs.usda.gov/rmrs/projects/cold-water-climate-shield). The analysis uses the same BK0 setting across three climate periods.

| Climate scenario | Script input |
| --- | --- |
| Baseline, 1980s | `B0BK01980_SpoKoot.shp` |
| Moderate, 2040s | `B0BK02040_SpoKoot.shp` |
| Extreme, 2080s | `B0BK02080_SpoKoot.shp` |

[Isaak et al. (2015), *The cold-water climate shield*](https://doi.org/10.1111/gcb.12879)

## Revised Climate Shield bull trout models — Isaak et al. (2022)

Download the three Appendix S6 shapefiles from the [USDA Forest Service Revised Bull Trout Climate Shield Model](https://research.fs.usda.gov/rmrs/products/dataandtools/revised-bull-trout-climate-shield-model-arcgis-shapefiles).

| Climate scenario | Download archive | Script input |
| --- | --- | --- |
| Baseline | Scenarios 1–8 | `BullTroutPatchScenarios_BaselineClimate_Isaak_et_al_2022_AppendixS6.shp` |
| Moderate | Scenarios 9–12 | `BullTroutPatchScenarios_ModerateClimate_Isaak_et_al_2022_AppendixS6.shp` |
| Extreme | Scenarios 13–16 | `BullTroutPatchScenarios_ExtremeClimate_Isaak_et_al_2022_AppendixS6.shp` |

[Isaak et al. (2022), *Do metapopulations and management matter for relict headwater bull trout populations in a warming climate?*](https://doi.org/10.1002/eap.2594)

## Elevation

- **Public download:** [USGS The National Map Downloader](https://apps.nationalmap.gov/downloader/). Select **Elevation Products (3DEP)** and **1/3 arc-second DEM** for approximately 10 m elevation data covering the South Fork Flathead River basin.
- **Script input:** `South_Fork_Flathead_DEM.tif`, the project’s analysis-ready GeoTIFF. 

## File setup

Extract the download archives before running the script. Keep each shapefile’s `.shp`, `.shx`, `.dbf`, and `.prj` together, plus `.cpg` where provided. Keep the `.gdb` directory intact.

Update the input paths in the R script to match the locations of your local copies. The complete analysis also requires the project’s reach-pixel CSVs and barrier layer (see 'data_files' folder and 'shapefiles' folder). Additionally, to recreate this script you will need the field and eDNA workbooks, which can be found in the release dated 11/11/2026.
