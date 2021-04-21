## Journal file for keeping data analysis notes such as diagnostic results

__Linking datasets with common identifier__

_March 30, 2021_

Unfortunately, the data we obtained has been split up into various research projects (one collecting fish data, another abiotic variables, etc.), and therefore the data files lack a common field (matching site ID) to join them by to compare and analyze. To resolve this issue, the data was uploaded into QGIS in order to see the spatial distribution of all the sites sampled in Everglades National Park (ENP) and create a common ID field. Upon doing this, we realized that the proximity of these sites to one another varied considerably. Therefore, we have decided to classify sampling location by general waterway; Shark River Slough in the northwest and Taylor Slough in the southeast. Additionally, we discovered that the temperature and dissolved oxygen dataset only overlapped temporally with the biotic data for one year, so we added 'ENP_WQ_1996to2005.csv', which contains abiotic data collected by another research team and overlaps temporally with our biotic data. 

_(Danielle Montocchio)_

__Fish data cleaning, determining functional groups and thermal guilds, creating join file.__

_March 30 2021_

First, we compiled a list of the unique species names observed over the two fish data sets (one from 1996-2000 and one from 2000-2005). We omitted the data points where the fish could not be identified, or could only be identified to its genus (e.g. "unidentified fish" or "Lepomis sp." were ommited). Then, we corrected the spelling of six different species, as they were recorded using two different spellings and inflated the total number of species included in the data set. After eliminating these duplicates, we found that there were 38 different species observed over the course of this study. Using a fish database (FishBase), as well as additional literature, we performed a literature search to determine which food items make up the highest proportion of each species' diet, and assigned then a functional group accordingly. The functional groups were originally as follows: piscivore, planktivore, detrivore, insectivore, periphytivore, algivore and omnivore. However, based on a preliminary literature search, it appeared that fish who feed on insects typically also feed on other invertebrates such as crustaceans, therefore this functional group was modified to "invertivores". Second, we found no record that any of the species included in this study have been observed to feed on periphyton, rather, it is the invertebrates that likely feed on it most regularly. For this reason, periphytivore was eliminated as a functional group and instead, we will use invertivores as our most direct link to the influence of periphyton on fish populations. 

Thermal guilds were defined into four broad categories based on thermal optima of a given species in its __native__ habitat (i.e., if it is invasive to Florida, thermal guild was determined based on its native environment) : cold-water (< 19&deg;C), cool-water (>= 19&deg;C and < 22&deg;C), warm-water (>= 22&deg;C), and cool/warm-water (significant oerlap in the cool and warm categories). These category cut-offs were taken from Wehrly, Wiley, and Seelbach (2011). With all functional/thermal groups assigned, the functional group data was merged with each fish data set to create two merged files which were then uploaded to the repo.

_(Megan Ridgway)_

_April 1, 2021_

__Resources for interpreting diagnostics__

To ensure we evaluate our models without bias, we will evaluate various diagnostic plots and outputs before looking at the model summary. These links are helpful for interpretation of said diagnostic plots:

https://www.andrew.cmu.edu/user/achoulde/94842/homework/regression_diagnostics.html 

https://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R5_Correlation-Regression/R5_Correlation-Regression7.html

https://stats.idre.ucla.edu/stata/webbooks/reg/chapter2/stata-webbooksregressionwith-statachapter-2-regression-diagnostics/

_(Danielle Montocchio)_

__Data joining and cleaning (and a lot of troubleshooting)__

_April 6, 2021_

Since the data we chose was split up over multiple data files, the data needed to be synthesized before any comparisons between explanatory variables could be made. 

We created a 'joining_cleaning_file.R' file, where we can consolidate and clean all of our data into manageable data sets. We started with a file which assigned the appropriate "area" (meaning either the Taylor Slough or Shark River) for each study site, then created a join file to incorporate the area into the larger water quality data set. We had a small issue with 83 observations being eliminated when making the join file, which ended up being a repeated spelling error for one of the site names. When the incorrectly spelled data points were replaced with the correct spelling, we had a merged water quality data file which included area names. 

Next, we had to merge the two fish data sets, which ranged from 1996-2000 and 2000-2005 into one larger data set in order to join the fish data with the water quality data. This was more complicated than we expected since the files needed to be join by date and area, but of course, the dates weren't formatted the same way. After switching them both to day-month-year format, the files still didn't want to join together, so instead we jumped ahead and began pairing down the data sets individually first. All unnecessary columns were removed and aggregated the total species weight and fish biomass data by month level for year, site and fish species. Duplicate values (i.e observations of the same species over the course of one month) were averaged. The water quality data was then aggregated the same way, where water depth, various nutrient parameters, salinity, temperature, turbidity and dissolved oxygen were aggregated for by month, year and area. 

The water quality data file was then merged with the fish data file, giving us a complete data set which can now be used to test our first hypothesis (see README.R file). 

Considering our fish data had 38 unique fish species, we also created a further aggregated dataset, totaling biomass and species weight by month, year and area. We then also generated a species richness column, which counted the number of unique species collected by month, year and area. Once NA rows were removed, this aggregated dataset contained 89 observations.

_(Megan Ridgway and Danielle Montocchio)_

__Hypothesis 1 Data Analysis Steps__

_March 9, 2021_

Before model construction to view diagnostic plots, a subset list of the predicted explanatory variables was made and compared with a correlation matrix. The results of which showed that both the surface and bottom temperature and DO measurements were nearly 100% correlated (Pearson correlation coefficient of ~1). This would make sense due to the shallow water depths of the river sloughs in the study area. Since a majority of our fish species are expected to occupy the bottom for longer periods of time than the surface, this measurement was used.

Temperature and DO were found to be highly correlated with one another, yet negatively, which makes sense given the effect that temperature has on the capacity for oxygen to remain dissolved in liquids.

All the following steps were repeated for the three levels of aggregated datasets, going from highest resolution to lowest (i.e. species-level --> thermal guild-level --> all fish):

  1. Check variable distributions for approximate normality
      1. explanatory variables were roughly normal
      2. Biomass (one of the response variables) was not
          1. this was log-transformed
    
  2. Remove all NA's and 0's (same thing in this dataset) as these had no biological meaning for the variables (such as 0 depth and biomass = dry site/not sampled)
 
  3. A full model with all explanatory variables was created for the species-level dataset
      1. residuals of this model were evaluated with boxplots broken up into potentially influential categorical variables
          * i.e. Area, Month, Year --> all were found to have no differential effect on the residuals
              * March appeared to be an outlier, this was determined to be because there was only one observation in this category
      2. residuals with thermal guild-level dataset were evaluated
          * thermal guild was evaluated to have an effect

  4. Afterwards stepwise linear regressions (performed backwards and forwards) was performed
      1. diagnostic plots were evaluated
          1. species-level data results indicated model assumption violations
          2. thermal-guild level --> DO (bottom), depth, and guild was found to be significant
          3. all fish --> DO (bottom) and depth was the best fitted model for logBiomass
          4. all fish --> salinity (bottom) was best fit for species richness
              * this was a generalized linear model with a Poisson error distribution and a logarithmic link function

_(Danielle Montocchio)_

__Implementing post-presentation changes__

_April 15, 2021_

Following the feedback from our progress presentation, we changed our course of action for our project. We are no longer doing step-wise regressions, and instead we are going to build a model based on the variables we believe are biologically relevant to our hypothesis including temperature and dissolved oxygen (full model), then build a subset of this model with all the same variables except temperature and DO, which we will compare using an ANOVA to determine the effect size of temperature and DO. 

For the full fish model, we chose the following variables: surface and bottom temperature, DO and salinity, water depth, ammonia, total nitrogen, chlorophyll-a and turbidity as our predictor variables and fish biomass as our response variable. 

First, we tested the colinearity of the predictor variables with a correlation matrix and found the surface and bottom measurements were highly correlated and we will therefore use only the bottom measurements (more biologically relevant since more fish will be present at the bottom). NH4 and depth, ChlA and depth, and NH4 and turbidity were weakly correlated. 

Then, we looked at categorical variable effect size on residuals (using boxplots) for months, years, and areas. No one area or year had a larger effect size. Two months (March and May) looked slightly different than the rest, but this was due to the fact that each only had 1 observation.

Next, we looked at diagnostic plots for the model, and found that we needed to log-transform biomass (due to a lack of normality). Observations 79 and 80 had high leverage and they were omitted to further improve the diagnostic plots. 

We made the second (subset) model with all the same variable, except temperature and DO, then plotted them both using a coefficient plot. The coefficient plot showed that depth has the largest effect on fish biomass, but also has the  highest variance of all variables. THe next largest effect was from NH4, followed by DO and ChlA. 

Finally, we compared the two models using an ANOVA and found that temperature and DO had little effect on fish biomass. 

_(Megan Ridgway)_


__PCA Analysis of Thermal and Functional Groups__

_April 21, 2021_

To further understand which variables may be driving differences in the thermal guilds and functional groups of fish species, we ran PCA biplot analyses of these  two datasets. Similar to our regressions, cold thermal guild species seemed to differ from the other guilds, based on depth and DO. Interestingly however, functional group clustering completely overlapped, indicating that none of the selected active variables effected the functional groups in different amounts/degrees. This may be due to the fact that species were assigned to functional groups based on their __main__ food source; however, many species were quite non-preferential in their feeding habits, and therefore ate species outside of their "group".

_(Danielle Montocchio)_