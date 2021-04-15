## Metadata File for ENP datsets uploaded to repo

__Authors:__ Megan Ridgway and Danielle Montocchio

__Last Update:__ April 15, 2021

__NB:__ Some columns may be missing in files due to data cleaning in Excel.

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

_ENP_WQ_1996to2005.csv_

This dataset contains surface water quality data in South Florida Coastal Waters (ranging from 1996 to 2005).

| Variable | Unit | Storage Type| Description | Code Meaning |
| -------- | ---- | ------------ | ------------ | ----|
| SURV | NA | ordinal | Survey number | NA |
| Date | NA | datetime | Date of sampling | Missing value = -9999 |
| TIME | NA | NA | Time of sample collection | Missing value = -9999 |
| STA | NA | ordinal | WQM station ID where samples collected | NA |
| SITE | NA | text | Name of station collection site | NA |
| LATDEC | degree | coordinate | Decimal latitude | NA |
| LONDEC | degree | coordinate | Decimal longitude | NA |
| DEPTH | meter | data | Station depth | NA |
| BAY | FB = Florida Bay, WWB = White Water Bay | text | Accronym of Bay where samples are collected | NA |
| NOX | micromoles/L | data | Nitrate + Nitrite | Missing value = -9999 |
| NO3 | micromoles/L | data | Nitrate | Missing value = -9999 |
| NO2 | micromoles/L | data | Nitrite | Missing value = -9999 |
| NH4 | micromoles/L | data | Ammonium | Missing value = -9999 |
| TN | micromoles/L | data | Total nitrogen | Missing value = -9999 |
| DIN | micromoles/L | data | Dissolved inorganic nitrogen | Missing value = -9999 |
| TON | micromoles/L | data | Total organic nitrogen | Missing value = -9999 |
| TP | micromoles/L | data | Total phosphorus | Missing value = -9999 |
| SRP | micromoles/L | data | Soluble reactive phosphorus | Missing value = -9999 |
| APA | micromoles/L | data | Alkaline phosphatase activity | Missing value = -9999 |
| CHLA | micrograms/L | data | Chlorophyll a | Missing value = -9999 |
| TOC | micromoles/L | data | Total organic carbon | Missing value = -9999 |
| SiO2 | micromoles/L | data | Silicon dioxide in water | Missing value = -9999 |
| SAL_S | PSU | data | Surface salinity | Missing value = -9999 |
| SAL_B | PSU | data | Bottom salinity | Missing value = -9999 |
| TEMP_S | celcius | data | Surface water temperature | Missing value = -9999 |
| TEMP_B | celcius | data | Bottom water temperature | Missing value = -9999 |
| DO_S | mg/L | data | Surface dissolved oxygen | Missing value = -9999 |
| DO_B | mg/L | data | Bottom dissolved oxygen | Missing value = -9999 |
| TURB | NTU | data | Turbidity | Missing value = -9999 |
| Kd | /m | data | Diffuse light attenuation coefficient | Missing value = -9999 |
| TN_TP | dimensionless | data | Surface total nitrogen to total phosphorus ratio | Missing value = -9999 |
| N_P | dimensionless | data | Surface Din to SRP ratio | Missing value = -9999 |
| DIN_TP | dimensionless | data | Dissolved inorganic nitrogen to total phosphorus ratio | Missing value = -9999 |
| DSIGT | kg/cubic meter | data | Delta Sigma-T | Missing value = -9999 |
| Si_DIN | dimensionless | data | Silicate to dissolved inorganic nitrogen ratio | Missing value = -9999 |

