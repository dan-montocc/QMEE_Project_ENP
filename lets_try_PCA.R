#PCA Attempt...
#code from STDA
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

#load in libraries...
library(FactoMineR)
library(factoextra)
library(tidyverse)

#aesthetics load-in
library(extrafont)
loadfonts(device="win")       #Register fonts for Windows bitmap output
windowsFonts(A = windowsFont("Times New Roman"))

#THERMAL GUILD
#read in thermal guild data
fish_therm <- readRDS("Joined_Cleaned_Data/Hypothesis_1/Fish_ThermalGuild_AggregatedData.rds")

#subset of active individuals (thermal guild) and active variables (explanatory)
fish_therm_PCA_sub <- fish_therm_sub1[,c("ThermalGuild","Depth",
                                         "Sal_B","Temp_B","NH4","DO_B","Turb","ChlA")]

names(fish_therm_PCA_sub)[names(fish_therm_PCA_sub) == "Temp_B"] <- "Temperature"
names(fish_therm_PCA_sub)[names(fish_therm_PCA_sub) == "DO_B"] <- "DO"
names(fish_therm_PCA_sub)[names(fish_therm_PCA_sub) == "Turb"] <- "Turbidity"
names(fish_therm_PCA_sub)[names(fish_therm_PCA_sub) == "ChlA"] <- "Chl-a"
names(fish_therm_PCA_sub)[names(fish_therm_PCA_sub) == "Sal_B"] <- "Salinity"

therm.pca <- PCA(fish_therm_PCA_sub[,-1], graph = FALSE)

library(ggsci)#get journal based colour palette

fviz_pca_ind(therm.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = fish_therm_PCA_sub$ThermalGuild, # color by groups
             palette = "npg",
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Thermal Guild"
)

fviz_pca_biplot(therm.pca, 
                col.ind = fish_therm_PCA_sub$ThermalGuild, palette = "npg", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Thermal Guild",
                font.family = "A") 

fviz_pca_biplot(therm.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = fish_therm_PCA_sub$ThermalGuild, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "npg",
                addEllipses = TRUE,
                # Variables
                col.var = "contrib",
                gradient.cols = "Dark2",
                arrowsize = 0.75,
                labelsize = 5,
                legend.title = list(fill = "Thermal Guild", color = "Contribution"),
                font.family = "A", title = ""
)

#FUNCTIONAL GROUP
Peri_fish_dat <- readRDS("Joined_Cleaned_Data/Hypothesis_3/Peri_WQ_FishFuncGrp_Join.rds")

#subset of active individuals (functional group) and active variables (explanatory)
fish_func_PCA_sub <- Peri_fish_dat_sub1[,c("FunctionalGroup","AvgWaterDepth",
                                         "Sal_B","Temp_B","NH4","DO_B","Turb",
                                         "Avg.PeriphytonCover","Avg.PlantCover")]

names(fish_func_PCA_sub)[names(fish_func_PCA_sub) == "Temp_B"] <- "Temperature"
names(fish_func_PCA_sub)[names(fish_func_PCA_sub) == "DO_B"] <- "DO"
names(fish_func_PCA_sub)[names(fish_func_PCA_sub) == "Turb"] <- "Turbidity"
names(fish_func_PCA_sub)[names(fish_func_PCA_sub) == "Avg.PeriphytonCover"] <- "Periphyton"
names(fish_func_PCA_sub)[names(fish_func_PCA_sub) == "Sal_B"] <- "Salinity"
names(fish_func_PCA_sub)[names(fish_func_PCA_sub) == "Avg.PlantCover"] <- "Macrophytes"
names(fish_func_PCA_sub)[names(fish_func_PCA_sub) == "AvgWaterDepth"] <- "Depth"

func.pca <- PCA(fish_func_PCA_sub[,-1], graph = FALSE)

fviz_pca_ind(func.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = fish_func_PCA_sub$FunctionalGroup, # color by groups
             palette = "npg",
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Functional Group", title = ""
) + theme(text=element_text(family="A", size=12))


fviz_pca_biplot(func.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = fish_func_PCA_sub$FunctionalGroup, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "npg",
                addEllipses = TRUE,
                arrowsize = NULL,
                labelsize = NULL,
                legend.title = "Functional Group",
                font.family = "A", title = ""
)
