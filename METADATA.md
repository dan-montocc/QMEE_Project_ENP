## Metadata File for ENP datsets uploaded to repo

__Authors:__ Megan Ridgway and Danielle Montocchio

__Last Update:__ March 11, 2021

_ENP_FishData_1996to2000.csv and ENP_FishData_2000to2005.csv_

These datasets contain the total fish weight and biomass of fish captured in trap nets in ENP grouped by fish species (ranging from 1996 to 2000 and 2000 to 2005, respectively).

| Variable | Unit | Storage Type| Description | Code Meaning |
| -------- | ---- | ------------ | ------------ | ----|
| Date | NA | date | Sample collection date | NA |
| Month | NA | text | Sample collection month | NA |
| Year | NA | date | Sample collection year | NA |
| Area | NA | text | Sample collection area descriptor | NA |
| SITENAME | NA | text | Name of collection site | NA |
| FPO | NA | text | Type of sample collected (fish, plant or other) | NA |
| Plot | NA | text | Plot ID number | NA |
| SpeciesName | NA | text | Genus and species name | 1 = dry (not sampled) |
| TotalSpeciesWeight | gram | number (real) | Total species weight | Missing value = -9999 |
| NumofThrowTraps | NA | number (count/integer) | number of throw traps | Missing value = -9999|
| SpeciesBiomass | g/m<sup>2<sup> | number (real) | Species biomass | Missing value = -9999 |

_ENP_FPOData.csv_

This dataset contains the species count and density of fish, plants and other fauna captured in trap nets in ENP grouped by species (ranging from 2000 to 2005).

| Variable | Unit | Storage Type| Description | Code Meaning |
| -------- | ---- | ------------ | ------------ | ----|
| Date | NA | date | Sample collection date | NA |
| Month | NA | text | Sample collection month | NA |
| Year | NA | date | Sample collection year | NA |
| Area | NA | text | Sample collection area descriptor | NA |
| SITENAME | NA | text | Name of collection site | NA |
| FPO | NA | text | Type of sample collected (fish, plant or other) | NA |
| Plot | NA | text | Plot ID number | NA |
| SpeciesName | NA | text | Genus and species name | 1 = dry (not sampled) |
| SpeciesCount | NA | number (count/integer) | Total species count | Missing value = -9999 |
| NumofThrowTraps | NA | number (count/integer) | number of throw traps | Missing value = -9999|
| SpeciesDensity | # of individuals/m<sup>2<sup> | number (real) | Species density | Missing value = -9999 |

_ENP_HabitatData.csv_

This dataset contains habitat data such as plant coverage, plant height, and periphyton coverage (ranging from 1996 to 2008).

| Variable | Unit | Storage Type| Description | Code Meaning |
| -------- | ---- | ------------ | ------------ | ----|
| Date | NA | date | Sample collection date | NA |
| Month | NA | text | Sample collection month | NA |
| Year | NA | date | Sample collection year | NA |
| Area | NA | text | Sample collection area descriptor | NA |
| SITENAME | NA | text | Name of collection site | NA |
| Avg%PlantCover | % | number (real) | Average percent plant cover | Missing value = -9999 |
| AvgPlantHeight | cm | number (real) | Average plant height | Missing value = -9999 |
| Avg%PeriphytonCover | % | number (real) | Average percent periphyton cover | Missing value = -9999 |
| AvgPeriphytonVolume | mL | number (real) | Average periphyton volume | Missing value = -9999|
| AvgWaterDepth | cm | number (real) | Average water depth | Missing value = -9999 |
| Comments | NA | text | Sampling comments | HELCOP= Helicopter was used to access sites. Only five throws taken  NODATA= No data were collected for this sample  NODEPT= No depth data was recorded for the sample  NOPTHT= No plant height was recorded for this sample  NOPVOL= No Periphyton Volume was recorded for the sample  NOPVPC= No periphyton volume or periphyton cover were recorded for the sample  NPHPLC= No plant height or plant cover were recorded for the sample  NPLTCV= No plant cover was recorded for the sample  SITDRY= Site was visited but too dry to sample (depth < 0.5cm)|

_ENP_pH_TP_OM_Data.csv_

This dataset contains each site's physical characteristics such as water depth, pH and temperature (ranging from 2006 to 2008).

| Variable | Unit | Storage Type| Description | Code Meaning |
| -------- | ---- | ------------ | ------------ | ----|
| REGION | NA | text | Project descriptor | NA |
| Site_ID | NA | ordinal | Collection site ID number | NA |
| Replicate | NA | ordinal | Replicate ID number (3 replicates taken per site) | Missing value = -9999 |
| SampleType | NA | text | Type of sample collected | NA |
| SDI | NA | data | Incremental soil depth from surface for given sample | Missing value = -9999 |
| SAMID | NA | text | Concatination of sample descriptors to produce sample ID | NA |
| Date | NA | datetime | Date sample was collected | NA |
| TIME | NA | datetime | Time collect | NA |
| Water_Depth | centimeters | data | Water depth as measured in field with meter stick | Missing value = -9999 |
| Water_Temperature | Celsius | data | Water temperature | Missing value = -9999 |
| WPH | NA | data | Water pH as measured in the field | Missing value = -9999 |
| WPH_Notes | NA | text | Notes regarding pH value | NA |
| CDEP | centimeter | data | Average soild core depth | Missing value = -9999 |
| SPH | NA | data | pH as measured in 1:1 soil slurry | Missing value = -9999 |
| SH2O | percent | data | Water content as percentage of wet weight soil | Missing value = -9999 |
| ASH | percent | data | Percent Ash in dry samples after ashing at 550 C | Missing value = -9999 |
| OM | percent | data | Percent organic matter (OM) as difference between dry weight - Ash | Missing value = -9999 |
| FBD | grams per cubic centimeter | data | Field bulk density | Missing value = -9999 |
| FBGTPs | micrograms per gram | data | Total P in soils as ug TP/g dw solid analyzed by Freshwater Biogeochemistry Laboratory | Missing value = -9999 |
| TP | grams per square meter | data | Total P in soils on an area basis. Uses core depth, FBD, and FBGTPs to calculate | Missing value = -9999 |
| TC | milligrams per gram | data | Total carbon in soil/floc sample as determined by SERC lab - conducted on 1/3 samples | Missing value = -9999 |
| TN | milligrams per gram | data | Total nitrogen in soil/floc sample as determined by SERC lab - conducted on 1/3 samples | Missing value = -9999 |
| SRECTPs | micrograms per gram | data | Total P in soils as ug TP/g dw solid analyzed by Southeast Environmental Research Center | Missing value = -9999 |
| FBGTPw | micrograms per liter | data | Surface water Total P as ug/L analyzed by Freshwater Biogeochemistry Group | Missing value = -9999 |
| SRECTPw | micrograms per liter | data | Surface water Total P as ug/L analyzed by Southeast Environmental Research Center | Missing value = -9999 |

_ENP_Temp_Sal_DO_Data.csv_

This dataset contains information on the environmental conditions in the Shark River Slough including dissolved oxygen, water temperature and salinity (ranging from 2005 to 2014).

| Variable | Unit | Storage Type| Description | Code Meaning |
| -------- | ---- | ------------ | ------------ | ----|
| Date | NA | datetime | Collection date | NA |
| Year | NA | NA | Year of sample collection | NA |
| DayofYear | NA | NA | Day of year sample collected | NA |
| Time | NA | datetime | Time of day sample collected | NA |
| LatitudeDD | NA | coordinate | Latitude | Missing value = -9999 |
| LongitudeDD | NA | coordinate | Longitude | Missing value = -9999 |
| WaterTemp | Celsius | data | Water temperature | Missing value = -9999 |
| Salinity | parts per thousand | data | Water salinity | Missing value = -9999 |
| DO | milligrams per liter | data | Water dissolved oxygen | Missing value = -9999 |
| DO% | percent | data | Percent saturation of dissolved oxygen | Missing value = -9999 |
| AirTemp | Celsius | data | Air temperature | Missing value = -9999 |