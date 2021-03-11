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
