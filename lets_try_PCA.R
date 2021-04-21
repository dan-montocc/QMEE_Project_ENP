#PCA Attempt...
#code from STDA
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

#load in libraries...
library(FactoMineR)
library(factoextra)
library(tidyverse)

#THERMAL GUILD
#read in thermal guild data
fish_therm <- read.csv("Joined_Cleaned_Data/Hypothesis_1/FishThermGuild_Join_WQ_1996to2005.csv")
fish_therm <- fish_therm[,-c(1)]
summary(fish_therm)
fish_therm_sub1 <- filter(fish_therm, TotalBiomass != 0)
fish_therm_sub1$logBiomass <- log10(fish_therm_sub1$TotalBiomass)
fish_therm_sub1 <- filter(fish_therm_sub1, SRP > 0)
fish_therm_sub1 <- filter(fish_therm_sub1, Sal_B > 0)
fish_therm_sub1 <- filter(fish_therm_sub1, TOC > 0)
fish_therm_sub1 <- fish_therm_sub1 %>% filter(!is.na(DO_B))
summary(fish_therm_sub1)
fish_therm_sub1$Area <- as.factor(fish_therm_sub1$Area)
fish_therm_sub1$Month <- as.factor(fish_therm_sub1$Month)
fish_therm_sub1$Year <- as.factor(fish_therm_sub1$Year)
fish_therm_sub1$ThermalGuild <- as.factor(fish_therm_sub1$ThermalGuild)
summary(fish_therm_sub1)

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
                legend.title = "Thermal Guild") 

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
                legend.title = list(fill = "Thermal Guild", color = "Contribution")
)

#FUNCTIONAL GROUP
Peri_fish_dat <- read.csv(here("Joined_Cleaned_Data/Hypothesis_3/Peri_WQ_FishFuncGrp_1995to2005_Join.csv"))
Peri_fish_dat <- Peri_fish_dat[,-c(1)]
summary(Peri_fish_dat)
Peri_fish_dat_sub1 <- filter(Peri_fish_dat, Avg.PeriphytonCover != 0)
Peri_fish_dat_sub1 <- filter(Peri_fish_dat_sub1, SRP > 0)
Peri_fish_dat_sub1 <- filter(Peri_fish_dat_sub1, Sal_B > 0)
Peri_fish_dat_sub1 <- filter(Peri_fish_dat_sub1, TOC > 0)
Peri_fish_dat_sub1 <- Peri_fish_dat_sub1 %>% filter(!is.na(DO_B))
Peri_fish_dat_sub1 <- filter(Peri_fish_dat_sub1, TotalBiomass > 0)
Peri_fish_dat_sub1$logBiomass <- log10(Peri_fish_dat_sub1$TotalBiomass)
Peri_fish_dat_sub1$Area <- as.factor(Peri_fish_dat_sub1$Area)
Peri_fish_dat_sub1$Month <- as.factor(Peri_fish_dat_sub1$Month)
Peri_fish_dat_sub1$Year <- as.factor(Peri_fish_dat_sub1$Year)
Peri_fish_dat_sub1$FunctionalGroup <- as.factor(Peri_fish_dat_sub1$FunctionalGroup)
summary(Peri_fish_dat_sub1)

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
             legend.title = "Functional Group"
)


fviz_pca_biplot(func.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = fish_func_PCA_sub$FunctionalGroup, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "npg",
                addEllipses = TRUE,
                # Variables
                col.var = "contrib",
                gradient.cols = "Dark2",
                arrowsize = 0.75,
                labelsize = 5,
                legend.title = list(fill = "Functional Group", color = "Contribution")
)
