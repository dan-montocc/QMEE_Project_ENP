#Hypothesis 3 analysis

library(here)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(dotwhisker)
library(broom)

#load in data
Peri_fish_dat <- read.csv(here("Joined_Cleaned_Data/Hypothesis_3/Peri_WQ_FishFuncGrp_1995to2005_Join.csv"))
Peri_fish_dat <- Peri_fish_dat[,-c(1)]

#evaluate collinearity of predictor variables with correlation matrix
#code referenced from STHDA.com
##http://www.sthda.com/english/wiki/correlation-matrix-an-r-function-to-do-all-you-need
source("http://www.sthda.com/upload/rquery_cormat.r")
all_fish_xsub <- all_fish[,c("Temp_S", "Temp_B","DO_S","DO_B","Depth","Sal_S","Sal_B", 
                             "NH4", "ChlA","Turb")]
rquery.cormat(all_fish_xsub)