## Journal file for keeping data analysis notes such as diagnostic results

__Linking datasets with common identifier__

_March 30, 2021_

Unfortunately, the data we obtained has been split up into various research projects (one collecting fish data, another abiotic variables, etc.), and therefore the data files lack a common field (matching site ID) to join them by to compare and analyze. To resolve this issue, the data was uploaded into QGIS in order to see the spatial distribution of all the sites sampled in Everglades National Park (ENP) and create a common ID field. Upon doing this, we realized that the proximity of these sites to one another varied considerably. Therefore, we have decided to classify sampling location by general waterway; Shark River Slough in the northwest and Taylor Slough in the southeast. Additionally, we discovered that the temperature and dissolved oxygen dataset only overlapped temporally with the biotic data for one year, so we added 'ENP_WQ_1996to2005.csv', which contains abiotic data collected by another research team and overlaps temporally with our biotic data. 

_~ Danielle Montocchio_