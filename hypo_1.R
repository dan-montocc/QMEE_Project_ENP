##create a model!!
#Hypothesis 1
library(here)
library(tidyverse)
library(data.table)
library(ggiraph)
library(ggiraphExtra)
library(moonBook)
library(ggpubr)
library(ggplot2); theme_set(theme_bw())
knitr::opts_chunk$set(echo=FALSE,
                      dev.args = list(png = list(type = "cairo")))
##read in datasets
all_fish <- read.csv(here("Joined_Cleaned_Data/Hypothesis_1/ALL_FishJoinWQ_1996to2005.csv"))
all_fish <- all_fish[,-c(1)]
fish_therm <- read.csv("Joined_Cleaned_Data/Hypothesis_1/FishThermGuild_Join_WQ_1996to2005.csv")
fish_therm <- fish_therm[,-c(1)]
list(as.factor(fish_therm$Area))
list(as.factor(fish_therm$ThermalGuild))

#check variable distribution to see if transforms are necessary
hist(fish_therm$TotalBiomass)
hist(log10(fish_therm$TotalBiomass))
fish_therm$logBiomass <- log10(fish_therm$TotalBiomass)#log transform is better normal distribution
is.na(fish_therm)<-sapply(fish_therm, is.infinite)
fish_therm[is.na(fish_therm)]<-0
fish_therm <- fish_therm[-c(53,54,55,59,60,61,62,63,76,278,239),]

hist(fish_therm$Temp_B)
hist(fish_therm$Temp_S)
hist(fish_therm$DO_S)
hist(fish_therm$DO_B)


#build thermal guild model with biomass
biomass <- lm(logBiomass ~ (Temp_B + Temp_S + DO_S + DO_B)*Area, fish_therm)
plot(biomass)
biomass_simple <- lm(logBiomass ~ Temp_B + DO_B, fish_therm)
plot(biomass_simple)
ggplot(fish_therm,aes(y=logBiomass,x=Temp_B,color=Area))+geom_point()+stat_smooth(method="lm",se=FALSE)
boxplot(residuals(biomass_simple) ~ fish_therm$Area)
biomass_simple2 <- lm(logBiomass ~ Temp_S + DO_S, fish_therm)
plot(biomass_simple2)

#correlation matrix
res <- cor.test(fish_therm$Temp_B, fish_therm$DO_B, 
                method = "pearson")
res

biomassGuild <- lm(logBiomass ~ (DO_S + DO_B + Temp_S + Temp_B)*ThermalGuild, fish_therm)
plot(biomassGuild)

#leverage of 1 from 5 obs
#create subset and re-run
fish_thermsub <- fish_therm[-c(4,104,180,254,259),]

biomassGuild <- lm(logBiomass ~ (DO_S + DO_B + Temp_S + Temp_B)*ThermalGuild, fish_thermsub)
plot(biomassGuild)
biomassGuildBottom <- lm(logBiomass ~ (DO_B + Temp_B)*ThermalGuild, fish_thermsub)
plot(biomassGuildBottom)
biomassBottom <- lm(logBiomass ~ DO_B + Temp_B,fish_thermsub)
plot(biomassBottom)
ggplot(fish_therm,aes(y=logBiomass,x=DO_B,color=ThermalGuild))+geom_point()+stat_smooth(method="lm",se=FALSE)
ggplot(fish_therm,aes(y=logBiomass,x=Temp_B,color=ThermalGuild))+geom_point()+stat_smooth(method="lm",se=FALSE)

