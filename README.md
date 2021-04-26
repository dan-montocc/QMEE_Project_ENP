# QMEE_Project_ENP
QMEE group project repository. Group members are Megan Ridgway and Danielle Montocchio

__Project Title:__ _The effects of abiotic and biotic factors on periphyton and fish communities in Everglades National Park, Florida, USA_

__Raw Datasets:__ `ENP_FishData_1996to2000.csv`; `ENP_FishData_2000to2005.csv`; `ENP_HabitatData.csv`; `ENP_WQ_1996to2005.csv`

__Processed Datasets:__ `AllFish_AggregatedData.rds`; `Fish_ThermalGuild_AggregatedData.rds`; `Peri_WQ_AggregatedData.rds`; `Peri_AllFish.rds`; `Peri_WQ_fishFuncGrp_Join.rds` (found in `Joined_Cleaned_Data` folder under their respective Hypothesis sub-folder)

__Data Source:__ Florida Coastal Everglades Long Term Ecological Research Network Data Repository

__Data Link:__ https://fcelter.fiu.edu/data/index.html

__Metadata File:__ 'METADATA.md'

__Data Description:__ Data ranges from the years of 1996 to 2005 for various abiotic and biotic variables collected in Shark River Slough and Taylor Slough, Everglades National Park (ENP) in the southern tip of Florida's coast. Specifically, we have chosen data that contains species presence/absence for fish, as well as their abundance, and biomass where applicable. Habitat data consists of plant cover, plant height, and periphyton cover. Environmental variables consist of water temperature, water depth, salinity, pH, dissolved oxygen (DO), and various nutrient concentrations in the water column and sediment. Further details can be found in the metadata file, 'METADATA.md'.

__Data Collection Methods Files:__ Found in repo folder named 'field_and_lab_protocols' (unfortunately these documents are only available in PDF file format).

__Biological Questions:__ 

We have three main areas of interest in which we would like to evaluate the effect of abiotic conditions on fish or periphyton abundance. Firstly, we would like to explore what impact water temperature and dissolved oxygen (DO) has on fish biomass, and fish diversity, both as a net impact and/or guild-specific impact? Secondly, we would like to determine whether average plant cover and DO impacts periphyton density? Thirdly, does periphyton and plant coverage impact fish biomass, and/or composition of fish functional groups (i.e.: benthivore, predator, piscivore, herbivore, etc...)?

__Hypotheses:__

_Hypothesis 1 - Temperature and DO effects on fish_

With regards to water temperature and DO, we predict that as water temperatures increase, and DO decreases, fish net biomass should decrease, as well as the overall fish diversity should decrease. This is because, higher temperatures and lower DO tends to decrease the carrying capacity of an aquatic environment to support a larger fish community. In terms of diversity, overall fish species diversity should decrease with increasing temperatures and decreased DO, as more sensitive species would no longer be able to inhabit the area, and only hypoxic-resistant species would remain (Farwell et al., 2006).

_Hypothesis 2 - Plant coverage, TP, SRP, and DO impact on periphyton_

Periphyton is a critical food resource for many fish species, particularly herbivorous and omnivorous species. Periphyton coverage has been found to be linked to nutrient loading (particularly TP) and other water quality parameters, such as DO (Kannavillil and Kurissery, 2013). Periphyton also requires a hard substrate to attach to and grow, such as rocks, logs, and aquatic plants. We predict that as plant coverage/density increases, periphyton coverage should also increase. With regards to water quality parameters, we predict that as TP or SRP increases, and as DO decreases, periphyton coverage should increase.

_Hypothesis 3 - Periphyton coverage and its impact on fish_

Since many fish species eat periphyton, we predict that as periphyton coverage increases, so too should net fish biomass (van Dam et al., 2002). This is because we predict that increasing amounts of periphyton provides more food, particularly for species that are herbivorous and omnivorous in nature. Therefore, changes in periphyton coverage is predicted to have a stronger effect on these groups of fish species, more so than piscivore or apex carnivore species. We do however predict some effect of periphyton coverage on piscivore and carnivore fish species, due to a bottom-up effect through the food web.

__Proposed Statistics and General Thoughts about the Data:__

We likely will be testing these hypotheses by using methods such as linear regressions (or generalized linear regression models) and/or ANOVA analyses. 

To address potential confounding variables we will likely need to plot and explore the data points of the variables across temporal and/or spatial scales. That is, compare sites to one another, to see if certain sites may be outliers due to their location and/or proximity to an external factor that may influence the environment (e.g. proximity to a road). We will also need to compare the months sample points were collected in, to see whether seasonal variability is having a significant effect on the variables, particularly temperature, plant growth, and fish species presence. In dealing with a potential temporal factor of influence, we may have to include it as a factor in our model, or further filter our data to a specific sampling month (unsure of which is more appropriate at this point of time).

__Data Analysis Files:__ `hypo_1_II.R` (Hypothesis 1); `hypo_2_II.R` (Hypothesis 2); `hypo_3_II.R` (Hypothesis 3)

__Plotting Outputs:__ `Plots_Graphs` folder for graphs; `OutputsII.md` for model summaries

__References:__

Farwell, M., Fox, M.G., Moyes, C.D., and Burness, G. (2006) Can hypoxia tolerance explain differences in distribution of two co-occuring north temperate sunfishes? _Environmental Biology of Fishes._ __78__, 83-90. 

Kanavillil, N., and Kurissery, S. (2013) Temporal variation of periphyton communities: A 3-year study from northwest Lake Simcoe, Ontario, Canada. _Inland Waters._ __3__, 473-486.

van Dam, A., Beveridge, M., Azim, E., and Verdegem, M. (2002) The potential of fish production based on periphyton. _Reviews in Fish Biology and Fisheries._ __12__, 1-31.
