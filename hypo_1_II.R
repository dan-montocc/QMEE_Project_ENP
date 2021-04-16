##Hypothesis 1
####Take 2....

library(here)
library(ggplot2)
library(tidyverse)
library(corrplot)


#load in data
all_fish <- read.csv(here("Joined_Cleaned_Data/Hypothesis_1/ALL_FishJoinWQ_1996to2005.csv"))
all_fish <- all_fish[,-c(1)]
fish_therm <- read.csv("Joined_Cleaned_Data/Hypothesis_1/FishThermGuild_Join_WQ_1996to2005.csv")
fish_therm <- fish_therm[,-c(1)]
spdat <- read.csv("Joined_Cleaned_Data/Hypothesis_1/FishSpecies_Join_WQ_1996to2005.csv")
spdat <- spdat[,-c(1)]

#check for variable redundancy in explanatory variables
#response variables = TotalBiomass, SpeciesRichness
#potential predictors = Temp (bottom and surface), DO (bottom and surface), Salinity (bottom and surface), water depth

#evaluate collinearity of predictor variables with correlation matrix
#code referenced from STHDA.com
##http://www.sthda.com/english/wiki/correlation-matrix-an-r-function-to-do-all-you-need
source("http://www.sthda.com/upload/rquery_cormat.r")
all_fish_xsub <- all_fish[,c("Temp_S", "Temp_B","DO_S","DO_B","Depth","Sal_S","Sal_B", 
                             "NH4", "ChlA","Turb")]
rquery.cormat(all_fish_xsub)


#ALL FISH MODEL 
all_fish$logBiomass <- log10(all_fish$TotalBiomass)
fullfishmod <- lm(logBiomass ~ Depth + NH4 + ChlA + Sal_B +
                    Temp_B + DO_B + Turb, all_fish)
plot(fullfishmod)

#79 and 80 obs have high leverage
##removed
all_fish_sub1 <- all_fish[-c(79,80),]
fullfishmod <- lm(logBiomass ~ Depth + NH4 + ChlA + Sal_B +
                    Temp_B + DO_B + Turb, all_fish_sub1)
plot(fullfishmod)

#look at categorical variable effect size on residuals
boxplot(residuals(fullfishmod) ~ all_fish$Area) #area does not seem to effect the residuals disproportionately 
boxplot(residuals(fullfishmod) ~ all_fish$Month)#March different
summary(all_fish$Month == "March")# only one obs 
summary(all_fish$Month == "May")
##since study area is Florida, seasons are not as pronounced as they are in north
boxplot(residuals(fullfishmod) ~ all_fish$Year) #no specific Year stands out

##looks good
summary(fullfishmod)

#coefficient plot
library(dotwhisker)
library(broom)

ov <- names(sort(coef(fullfishmod),decreasing=TRUE))
ov

dwplot(fullfishmod, order_vars = ov) %>%
  relabel_predictors(c(Depth = "Depth",
                       DO_B = "Dissolved oxygen",
                       Turb = "Turbidity",
                       Sal_B = "Salinity",
                       Temp_B = "Temperature",
                       ChlA = "Chlorophyll-a",
                       NH4 = "NH4")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + theme(legend.position = "none") +
  geom_vline(xintercept=0,lty=2)
##only plotting two of 7??

dwplot(fullfishmod) %>%
  relabel_predictors(c(Depth = "Depth",
                       DO_B = "Dissolved oxygen",
                       Turb = "Turbidity",
                       Sal_B = "Salinity",
                       Temp_B = "Temperature",
                       ChlA = "Chlorophyll-a",
                       NH4 = "NH4")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + theme(legend.position = "none") +
  geom_vline(xintercept=0,lty=2)


#build model without DO and temp to assess effect size of these two
subfishmod <- lm(logBiomass ~ Depth + NH4 + ChlA + Sal_B + Turb, all_fish_sub1)
subfishmod2 <- lm(logBiomass ~ Depth + DO_B + NH4 + ChlA + Sal_B + Turb, all_fish_sub1)

dwplot(list(fullfishmod,subfishmod)) %>%
  relabel_predictors(c(Depth = "Depth",
                       DO_B = "Dissolved oxygen",
                       Turb = "Turbidity",
                       Sal_B = "Salinity",
                       Temp_B = "Temperature",
                       ChlA = "Chlorophyll-a",
                       NH4 = "NH4")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + 
  geom_vline(xintercept=0,lty=2) + 
  theme(legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

anova(fullfishmod,subfishmod)

dwplot(list(fullfishmod,subfishmod2)) %>%
  relabel_predictors(c(Depth = "Depth",
                       DO_B = "Dissolved oxygen",
                       Turb = "Turbidity",
                       Sal_B = "Salinity",
                       Temp_B = "Temperature",
                       ChlA = "Chlorophyll-a",
                       NH4 = "NH4")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + 
  geom_vline(xintercept=0,lty=2) + 
  theme(legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

anova(fullfishmod,subfishmod2)

subfishmod3 <- lm(logBiomass ~ Depth + Temp_B + NH4 + ChlA + Sal_B + Turb, all_fish_sub1)

dwplot(list(fullfishmod,subfishmod3)) %>%
  relabel_predictors(c(Depth = "Depth",
                       DO_B = "Dissolved oxygen",
                       Turb = "Turbidity",
                       Sal_B = "Salinity",
                       Temp_B = "Temperature",
                       ChlA = "Chlorophyll-a",
                       NH4 = "NH4")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + 
  geom_vline(xintercept=0,lty=2) + 
  theme(legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

anova(fullfishmod,subfishmod3)

#THERMAL GUILD MODEL

#data cleaning
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

#build full model with thermal guild interaction
fishthermmod <- lm (logBiomass ~ (Depth + NH4 + ChlA + Sal_B +
                      Temp_B + DO_B + Turb)*ThermalGuild, fish_therm_sub1)
plot(fishthermmod)

#remove obs with leverage of 1 = 4, 101, 235, 240
fish_therm_sub2 <- fish_therm_sub1[-c(4,101,235,240),]
fishthermmod <- lm (logBiomass ~ (Depth + NH4 + ChlA + Sal_B +
                                    Temp_B + DO_B + Turb)*ThermalGuild, fish_therm_sub2)
plot(fishthermmod)
summary(fishthermmod)

#creating faceted coefficient plot for thermal guild data
library(coefplot)

modelCI <- coefplot:::buildModelCI(fishthermmod)
ov2 <- names(sort(coef(fishthermmod),decreasing=TRUE))
ov2
coefname <- list("Warm guild","Depth:Cool/warm guild","Depth", "Depth:Warm guild",
                 "Dissolved oxygen","Chlorophyll-a:Warm guild", "Turbidity:Cool/warm guild",
                 "Turbidity:Warm guild", "Temperature", "Salinity:Cool/warm guild",
                 "Turbidity", "Salinity", "Chlorophyll-a", "Salinity:Warm guild",
                 "NH4", "Temperature:Cool/warm guild", "NH4:Warm guild", "Chlorophyll-a:Cool/warm guild",
                 "NH4:Cool/warm guild", "Temperature:Warm guild", "Dissolved oxygen:Cool/warm guild",
                 "Dissolved oxygen:Warm guild", "Cool/warm guild")

coefplot:::buildPlotting.default(
  modelCI, ylab = "", title = NULL, xlab = "Coefficient estimate", color = "indianred",
  cex = 0.8, textAngle = 0, numberAngle = 0, shape = 16, linetype = 1, outerCI = 2,
  innerCI = 1, multi = FALSE, zeroColor = "grey", zeroLWD = 1, zeroType = 2, numeric = FALSE,
  fillColor = "grey", alpha = 1/2, horizontal = FALSE, facet = TRUE, scales = "free",
  value = "Coefficient estimate", coefficient = "coefname", errorHeight = 0, dodgeHeight = 1,
  interactive = TRUE)

#attempt 2
dwplot(fishthermmod) %>%
  relabel_predictors(c(Depth = "Depth",
                       DO_B = "Dissolved oxygen",
                       Turb = "Turbidity",
                       Sal_B = "Salinity",
                       Temp_B = "Temperature",
                       ChlA = "Chlorophyll-a",
                       NH4 = "NH4")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + theme(legend.position = "none") +
  geom_vline(xintercept=0,lty=2) + facet_wrap(~ThermalGuld)
