##Hypothesis 2

library(here)
library(tidyverse)
library(data.table)
library(dplyr)
library(corrplot)
library(ggpubr)

#data load in
Peri_wq <- read.csv(here("Joined_Cleaned_Data/Hypothesis_2/ENP_Peri_WQ_1995to2005_Join.csv"))
Peri_wq <- Peri_wq[,-c(1)]
summary(Peri_wq$SAL_B)
#remove 0's and NAs
Peri_sub1 <- filter(Peri_wq, SAL_B > 0)
Peri_sub1[is.na(Peri_sub1)]<-0
Peri_sub1 <- filter(Peri_sub1, DEPTH != 0)
summary(Peri_sub1$SAL_B)

#correlation matrix
#explanatory subset
Peri_exp_var_list <- Peri_sub1[,c("Avg.PlantCover","AvgWaterDepth",
                                  "NO3", "TN","TP","SRP","TOC","SAL_S","SAL_B",
                                  "TEMP_S","TEMP_B","TURB")]

source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(Peri_exp_var_list)


#check variable distributions
hist(Peri_sub1$Avg.PeriphytonCover)

hist(Peri_sub1$Avg.PlantCover)
summary(Peri_sub1$Avg.PlantCover)
hist(log10(Peri_sub1$Avg.PlantCover))# better
Peri_sub1$logPlantCover <- log10(Peri_sub1$Avg.PlantCover)

hist(Peri_sub1$AvgWaterDepth)

hist(Peri_sub1$NO3)
hist(log10(Peri_sub1$NO3))
hist(sqrt(Peri_sub1$NO3))
Peri_sub1$sqrtNO3 <- sqrt(Peri_sub1$NO3)

hist(Peri_sub1$TN)
hist(log10(Peri_sub1$TN))
hist(sqrt(Peri_sub1$TN))
Peri_sub1$sqrtTN <- sqrt(Peri_sub1$TN)

hist(Peri_sub1$TP)
hist(log10(Peri_sub1$TP))
hist(sqrt(Peri_sub1$TP))
Peri_sub1$logTP <- log10(Peri_sub1$TP)

hist(Peri_sub1$SRP)
summary(Peri_sub1$SRP)
Peri_sub1 <- filter(Peri_sub1, SRP >= 0)
summary(Peri_sub1$SRP)
hist(Peri_sub1$SRP)
hist(log10(Peri_sub1$SRP))
hist(sqrt(Peri_sub1$SRP))
Peri_sub1$logSRP <- log10(Peri_sub1$SRP)

hist(Peri_sub1$TOC)
Peri_sub1 <- filter(Peri_sub1, TOC >= 0)
hist(Peri_sub1$TOC)
hist(log10(Peri_sub1$TOC))
hist(sqrt(Peri_sub1$TOC))
Peri_sub1$logTOC <- log10(Peri_sub1$TOC)

hist(Peri_sub1$SAL_B)
hist(log10(Peri_sub1$SAL_B))
hist(sqrt(Peri_sub1$SAL_B))
hist(Peri_sub1$TEMP_B)
hist(log10(Peri_sub1$TEMP_B))
hist(sqrt(Peri_sub1$TEMP_B))

hist(Peri_sub1$TURB)
hist(log10(Peri_sub1$TURB))
hist(sqrt(Peri_sub1$TURB))
Peri_sub1$logTURB <- log10(Peri_sub1$TURB)


#check for influence of Area, Month, and/or Year
fullPeriMod <- lm(Avg.PeriphytonCover ~ logPlantCover + AvgWaterDepth + 
                    sqrtNO3 + sqrtTN + logTP + logSRP + logTOC + TEMP_B +
                    DO_B + logTURB, Peri_sub1)
nothingPeriMod <- lm(Avg.PeriphytonCover ~ 1, Peri_sub1)
boxplot(residuals(fullPeriMod) ~ Peri_sub1$Area) #no effect
boxplot(residuals(fullPeriMod) ~ Peri_sub1$Month) 
summary(Peri_sub1$Month == "June") #two obs
boxplot(residuals(fullPeriMod) ~ Peri_sub1$Year) #maybe include??
Peri_sub1$Year <- as.factor(Peri_sub1$Year)
fullPeriMod <- lm(Avg.PeriphytonCover ~ logPlantCover + AvgWaterDepth + 
                    sqrtNO3 + sqrtTN + logTP + logSRP + logTOC + TEMP_B +
                    + DO_B + logTURB + Year, Peri_sub1)

backwards = step(fullPeriMod)
formula(backwards)
forwards = step(nothingPeriMod,
                scope=list(lower=formula(nothingPeriMod),upper=formula(fullPeriMod)), direction="forward")
formula(forwards)
bothways = step(nothingPeriMod, list(lower=formula(nothingPeriMod),upper=formula(fullPeriMod)),
                direction="both",trace=0)
formula(bothways)

PeriStepMod <- lm(Avg.PeriphytonCover ~ Year + logPlantCover + AvgWaterDepth +
                    sqrtTN + logTOC + logTURB + logSRP, data = Peri_sub1)
plot(PeriStepMod)

#plot sub model to visualize

library(ggplot2); theme_set(theme_bw())
#knitr::opts_chunk$set(echo=FALSE,
#dev.args = list(png = list(type = "cairo")))
library(ggiraph)
library(ggiraphExtra)
library(plyr)

PeriSubMod <- lm(Avg.PeriphytonCover ~ logPlantCover + logSRP, Peri_sub1)
ggPredict(PeriSubMod,interactive=TRUE)
