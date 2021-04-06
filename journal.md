## Journal file for keeping data analysis notes such as diagnostic results

__Linking datasets with common identifier__

_March 30, 2021_

Unfortunately, the data we obtained has been split up into various research projects (one collecting fish data, another abiotic variables, etc.), and therefore the data files lack a common field (matching site ID) to join them by to compare and analyze. To resolve this issue, the data was uploaded into QGIS in order to see the spatial distribution of all the sites sampled in Everglades National Park (ENP) and create a common ID field. Upon doing this, we realized that the proximity of these sites to one another varied considerably. Therefore, we have decided to classify sampling location by general waterway; Shark River Slough in the northwest and Taylor Slough in the southeast. Additionally, we discovered that the temperature and dissolved oxygen dataset only overlapped temporally with the biotic data for one year, so we added 'ENP_WQ_1996to2005.csv', which contains abiotic data collected by another research team and overlaps temporally with our biotic data. 

_(Danielle Montocchio)_

__Fish data cleaning, determining functional groups, creating join file.__

_March 30 2021_

First, we compiled a list of the unique species names observed over the two fish data sets (one from 1996-2000 and one from 2000-2005). We omitted the data points where the fish could not be identified, or could only be identified to its genus (e.g. "unidentified fish" or "Lepomis sp." were ommited). Then, we corrected the spelling of six different species, as they were recorded using two different spellings and inflated the total number of species included in the data set. After eliminating these duplicates, we found that there were 38 different species observed over the course of this study. Using a fish database (FishBase), as well as additional literature, we performed a literature search to determine which food items make up the highest proportion of each species' diet, and assigned then a functional group accordingly. The functional groups were originally as follows: piscivore, planktivore, detrivore, insectivore, periphytivore, algivore and omnivore. However, based on a preliminary literature search, it appeared that fish who feed on insects typically also feed on other invertebrates such as crustaceans, therefore this functional group was modified to "invertivores". Second, we found no record that any of the species included in this study have been observed to feed on periphyton, rather, it is the invertebrates that likely feed on it most regularly. For this reason, periphytivore was eliminated as a functional group and instead, we will use invertivores as our most direct link to the influence of periphyton on fish populations. With all functional groups assigned, the functional group data was merged with each fish data set to create two merged files which were then uploaded to the repo.

_(Megan Ridgway)_

_April 1, 2021_

__Resources for interpreting diagnostics__

To ensure we evaluate our models without bias, we will evaluate various diagnostic plots and outputs before looking at the model summary. These links are helpful for interpretation of said diagnostic plots:

https://www.andrew.cmu.edu/user/achoulde/94842/homework/regression_diagnostics.html 

https://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R5_Correlation-Regression/R5_Correlation-Regression7.html

https://stats.idre.ucla.edu/stata/webbooks/reg/chapter2/stata-webbooksregressionwith-statachapter-2-regression-diagnostics/

_(Danielle Montocchio)_
