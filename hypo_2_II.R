#Hypothesis 2 analysis

library(here)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(dotwhisker)
library(broom)

#load in data
Peri_dat <- read.csv(here("Joined_Cleaned_Data/Hypothesis_2/ENP_Peri_WQ_1995to2005_Join.csv"))
Peri_dat <- Peri_dat[,-c(1)]
summary(Peri_dat)
Peri_dat_sub1 <- filter(Peri_dat, Avg.PeriphytonCover != 0)
Peri_dat_sub1 <- filter(Peri_dat_sub1, SRP > 0)
Peri_dat_sub1 <- filter(Peri_dat_sub1, SAL_B > 0)
Peri_dat_sub1 <- filter(Peri_dat_sub1, TOC > 0)
Peri_dat_sub1 <- Peri_dat_sub1 %>% filter(!is.na(DO_B))
Peri_dat_sub1$Area <- as.factor(Peri_dat_sub1$Area)
Peri_dat_sub1$Month <- as.factor(Peri_dat_sub1$Month)
Peri_dat_sub1$Year <- as.factor(Peri_dat_sub1$Year)
summary(Peri_dat_sub1)

#evaluate collinearity of predictor variables with correlation matrix
#code referenced from STHDA.com
##http://www.sthda.com/english/wiki/correlation-matrix-an-r-function-to-do-all-you-need
source("http://www.sthda.com/upload/rquery_cormat.r")
Peri_ysub <- Peri_dat_sub1[,c("Avg.PeriphytonCover","AvgPeriphytonVolume")]
rquery.cormat(Peri_ysub)

Peri_xsub <- Peri_dat_sub1[,c("TEMP_B","DO_B","SAL_B", 
                              "NO3", "CHLA","TURB","TN","DIN","TP","SRP",
                              "TOC","AvgWaterDepth","Avg.PlantCover")]
rquery.cormat(Peri_xsub)

#boxplots of Year, Month, and Area
Perifullmod <- lm(Avg.PeriphytonCover ~ TEMP_B + DO_B + SAL_B + NO3 + CHLA +
                    TURB + TN + DIN + TP + SRP + TOC + AvgWaterDepth
                  + Avg.PlantCover,Peri_dat_sub1)

boxplot(residuals(Perifullmod) ~ Peri_dat_sub1$Area) #area does not seem to effect the residuals disproportionately 
boxplot(residuals(Perifullmod) ~ Peri_dat_sub1$Month)#pattern in month?
#observation in each month is low
boxplot(residuals(Perifullmod) ~ Peri_dat_sub1$Year)
summary(Peri_dat_sub1$Year) #add Year to model as random effect

library(nlme)
Perifullmod <- lme(Avg.PeriphytonCover ~ TEMP_B + DO_B + SAL_B + NO3 + CHLA +
                    TURB + TN + DIN + TP + SRP + TOC + AvgWaterDepth
                  + Avg.PlantCover, random = Year,data = Peri_dat_sub1)
#diagnose model
plot.merMod(Perifullmod) #looks good

Perifullmod_noYr <- lm(Avg.PeriphytonCover ~ TEMP_B + DO_B + SAL_B + NO3 + CHLA +
                    TURB + TN + DIN + TP + SRP + TOC + AvgWaterDepth
                  + Avg.PlantCover,Peri_dat_sub1)
plot(Perifullmod_noYr)
summary(Perifullmod)

#subPerimod = model without variables of interest (plant cover and DO)
Perisubmod <- lm(Avg.PeriphytonCover ~ TEMP_B + SAL_B + NO3 + CHLA +
                    TURB + TN + DIN + TP + SRP + TOC + AvgWaterDepth + Year,Peri_dat_sub1)
