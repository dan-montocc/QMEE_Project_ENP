##create a model!!
#Hypothesis 1
library(here)
library(tidyverse)
library(data.table)
library(dplyr)
library(corrplot)
library(ggpubr)

##read in datasets
all_fish <- read.csv(here("Joined_Cleaned_Data/Hypothesis_1/ALL_FishJoinWQ_1996to2005.csv"))
all_fish <- all_fish[,-c(1)]
fish_therm <- read.csv("Joined_Cleaned_Data/Hypothesis_1/FishThermGuild_Join_WQ_1996to2005.csv")
fish_therm <- fish_therm[,-c(1)]
all_fish$Area <- as.factor(all_fish$Area)
fish_therm$Area <- as.factor(fish_therm$Area)
fish_therm$ThermalGuild <- as.factor(fish_therm$ThermalGuild)
spdat <- read.csv("Joined_Cleaned_Data/Hypothesis_1/FishSpecies_Join_WQ_1996to2005.csv")
spdat <- spdat[,-c(1)]
spdat$Area <- as.factor(spdat$Area)
spdat$SpeciesName <- as.factor(spdat$SpeciesName)


#response variables = TotalBiomass, SpeciesRichness
#potential predictors = Temp (bottom and surface), DO (bottom and surface), Salinity (bottom and surface), water depth

#evaluate collinearity of predictor variables with correlation matrix
#code referenced from STHDA.com
##http://www.sthda.com/english/wiki/correlation-matrix-an-r-function-to-do-all-you-need
source("http://www.sthda.com/upload/rquery_cormat.r")
all_fish_xsub <- all_fish[,c("Temp_S", "Temp_B","DO_S","DO_B","Depth","Sal_S","Sal_B")]
rquery.cormat(all_fish_xsub)

fish_therm_xsub <- fish_therm[,c("Temp_S", "Temp_B","DO_S","DO_B","Depth","Sal_S","Sal_B")]
rquery.cormat(fish_therm_xsub)

spdat_xsub <- spdat[,c("TEMP_S", "TEMP_B","DO_S","DO_B","DEPTH","SAL_S","SAL_B")]
rquery.cormat(spdat_xsub)
#temperature and DO bottom and surface highly correlated, use bottom only

#check variable distribution to see if transforms are necessary
#response variable 1
hist(spdat$SpeciesBiomass)
hist(log10(spdat$SpeciesBiomass))
hist(sqrt(spdat$SpeciesBiomass))
spdat$logBiomass <- log10(spdat$SpeciesBiomass)#log transform is better normal distribution
is.na(spdat)<-sapply(spdat, is.infinite)
spdat[is.na(spdat)]<-0
spdat_sub1 <- filter(spdat, logBiomass != 0)
spdat_sub1 <- filter(spdat_sub1, DEPTH != 0)#remove zeroes in y and depth (zero's have no biological meaning in this case; 0 depth = dry)
hist(spdat_sub1$logBiomass)

#predictors
hist(spdat_sub1$TEMP_B)
hist(spdat_sub1$SAL_B)
summary(spdat_sub1$SAL_B) #negative should not be there; artifact of dataset NAs coding
spdat_sub1 <- filter(spdat_sub1, SAL_B > 0)
hist(spdat_sub1$DEPTH)##unusual distribution? 
hist(sqrt(spdat_sub1$DEPTH))
hist(log10(spdat_sub1$DEPTH))
hist(spdat$DO_B)
#all others look relatively normal

#look at residuals for area and species to see if they need to included as a factor interaction term in model
spmod1 <- lm(logBiomass ~ DO_B + TEMP_B + SAL_B, spdat_sub1)
boxplot(residuals(spmod1) ~ spdat_sub1$Area) #area does not seem to effect the residuals disproportionately 
boxplot(residuals(spmod1) ~ spdat_sub1$SpeciesName) #should include species as a factor/predictor
boxplot(residuals(spmod1) ~ spdat_sub1$Month)#March different
summary(spdat_sub1$Month == "March")# only one obs 
summary(spdat_sub1$Month == "April")
##since study area is Florida, seasons are not as pronounced as they are in north
boxplot(residuals(spmod1) ~ spdat_sub1$Year) #no specific Year stands out

#repeat for thermal guild file to look at effect of thermal guild category
fish_therm$logBiomass <- log10(fish_therm$TotalBiomass)#log transform is better normal distribution
is.na(fish_therm)<-sapply(fish_therm, is.infinite)
fish_therm[is.na(fish_therm)]<-0
fish_therm_sub1 <- filter(fish_therm, logBiomass != 0)
fish_therm_sub1 <- filter(fish_therm_sub1, Depth != 0)
fish_therm_sub1 <- filter(fish_therm_sub1, Sal_B > 0)


#stepwise linear regression at the species level
#stepwise methodology from STHDA
##http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
library(caret)
library(leaps)

#predictors will be temp, DO, Sal, Depth, and Species
spdat_sub2 <- spdat_sub1[,c("SpeciesName","logBiomass","TEMP_B","DO_B","SAL_B","DEPTH")]

# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
Species_step.model <- train(logBiomass ~., data = spdat_sub2,
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
Species_step.model$results
Species_step.model$bestTune
summary(Species_step.model$finalModel)
##investigate error messages

#move to thermal guild
fish_therm_sub2 <- fish_therm_sub1[,c("logBiomass","Temp_B","DO_B","Sal_B","Depth")]
fish_therm_sub3 <- fish_therm_sub1[,c("ThermalGuild","logBiomass","Temp_B","DO_B","Sal_B","Depth")]

# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
Guild_step.model <- train(logBiomass ~., data = fish_therm_sub2,
                            method = "leapSeq", 
                            tuneGrid = data.frame(nvmax = 1:5),
                            trControl = train.control
)
Guild_step.model$results
Guild_step.model$bestTune
summary(Guild_step.model$finalModel)
coef(Guild_step.model$finalModel, 2)
#selected model
Guild_mod <- lm(logBiomass ~ DO_B + Depth, fish_therm_sub3)
#diagnostics
plot(Guild_mod)
boxplot(residuals(Guild_mod) ~ fish_therm_sub3$ThermalGuild)

#plotting
##load in graphing libraries
library(ggplot2); theme_set(theme_bw())
#knitr::opts_chunk$set(echo=FALSE,
                      #dev.args = list(png = list(type = "cairo")))
library(ggiraph)
library(ggiraphExtra)
library(plyr)

#graph one - DO and Depth
ggPredict(Guild_mod,interactive = TRUE)

#interaction with guild
Guild_mod2 <- lm(logBiomass ~ DO_B*Depth*ThermalGuild, fish_therm_sub3)
plot(Guild_mod2)

#remove rows with leverage of 1 and 0.5 (only one obs)
fish_therm_sub4 <- fish_therm_sub3[-c(104,192),]
Guild_mod2 <- lm(logBiomass ~ DO_B*Depth*ThermalGuild, fish_therm_sub4)
plot(Guild_mod2)
ggPredict(Guild_mod2,interactive=TRUE)
summary(Guild_mod)
summary(Guild_mod2)



###repeat for ALL fish aggreagated data and species richness
all_fish$logBiomass <- log10(all_fish$TotalBiomass)
is.na(all_fish)<-sapply(all_fish, is.infinite)
all_fish[is.na(all_fish)] <- 0
all_fish_sub1 <- filter(all_fish, logBiomass != 0)
all_fish_sub1 <- filter(all_fish_sub1, Depth != 0)
all_fish_sub1 <- filter(all_fish_sub1, Sal_B > 0)

all_fish_sub2 <- all_fish_sub1[,c("logBiomass","Temp_B","DO_B","Sal_B","Depth")]

# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
Fish_step.model <- train(logBiomass ~., data = all_fish_sub2,
                          method = "leapSeq", 
                          tuneGrid = data.frame(nvmax = 1:4),
                          trControl = train.control
)
Fish_step.model$results
Fish_step.model$bestTune
summary(Fish_step.model$finalModel)
coef(Fish_step.model$finalModel, 4)
#selected model
Fish_mod <- lm(logBiomass ~ DO_B + Temp_B + Depth + Sal_B, all_fish_sub2)
#diagnostics
plot(Fish_mod)
summary(Fish_mod)

#try with 3 variables max
coef(Fish_step.model$finalModel, 3)
#do Not include depth
Fish_mod2 <- lm(logBiomass ~ DO_B + Temp_B + Sal_B, all_fish_sub2)
ggPredict(Fish_mod2,interactive = TRUE)

#try with 2 variables max
coef(Fish_step.model$finalModel, 2)
Fish_mod3 <- lm(logBiomass ~ DO_B + Depth, all_fish_sub2)
ggPredict(Fish_mod3,interactive = FALSE)

##creating stepwise GLM for all fish and species richness
#species richness is count data --> use Poisson distribution
fullmod <- glm(SpeciesRichness ~ DO_B + Temp_B + Sal_B + Depth, data = all_fish_sub1, family=poisson)
backwards = step(fullmod)
formula(backwards)

nothingmod <- glm(SpeciesRichness ~ 1, family=poisson, all_fish_sub1)
forwards = step(nothingmod,
                scope=list(lower=formula(nothingmod),upper=formula(fullmod)), direction="forward")
formula(forwards)

bothways = step(nothingmod, list(lower=formula(nothingmod),upper=formula(fullmod)),
         direction="both",trace=0)
formula(bothways)
summary(bothways)

#salinity is the best fit for species richness
SpRich_mod1 <- glm(SpeciesRichness ~ Sal_B, all_fish_sub1,family=poisson)
plot(SpRich_mod1)
summary(SpRich_mod1)
SpRich_mod2 <- glm(SpeciesRichness ~ DO_B + Depth, all_fish_sub1, family=poisson)##same variables as total biomass
plot(SpRich_mod2)
summary(SpRich_mod2)
##slight overdispersion
SpRich_mod3 <- glm(SpeciesRichness ~ DO_B + Depth, all_fish_sub1, family=quasipoisson)
plot(SpRich_mod3)
summary(SpRich_mod3)



##plotting
#code referenced from The Analysis factor
##https://www.theanalysisfactor.com/glm-r-overdispersion-count-regression/

summary(all_fish_sub1$Sal_B)
Salsmooth <- seq(3,42,length.out=391)
Y <- predict(SpRich_mod1, list(Sal_B = Salsmooth))
plot(all_fish_sub1$Sal_B, all_fish_sub1$SpeciesRichness, xlab="Salinity (bottom)", 
     ylab = "Species Richness", pch=16)
lines(Salsmooth, exp(Y), lwd = 2, col="blue")

ggPredict(SpRich_mod1, "Sal_B") %>% plot(rawdata = TRUE, jitter = .01)

summary(all_fish_sub1$DO_B)
DO_B_smooth <- seq(1,9, length.out = 88)
Depth_smooth <- seq(1,3,length.out = 88)
Y2 <- predict(SpRich_mod3, list(DO_B = DO_B_smooth, Depth = Depth_smooth))

ggplot(all_fish_sub1,aes(y=SpeciesRichness,x=DO_B,color=Depth)) + geom_point() +
  geom_line(aes(x = DO_B_smooth, y = exp(Y2)), linetype=1, colour="darkgreen", size =0.5)

##one more model
SpRich_mod4 <- glm(SpeciesRichness ~ DO_B + Sal_B, all_fish_sub1,family=poisson)
plot(SpRich_mod4)
summary(SpRich_mod4)
Y3 <- predict(SpRich_mod4, list(DO_B = DO_B_smooth,Sal_B = Salsmooth))
ggplot(all_fish_sub1,aes(y=SpeciesRichness,x=Sal_B,color=DO_B)) + geom_point() +
  geom_line(aes(x = Salsmooth, y = exp(Y3)), linetype=1, colour="darkgreen", size =0.5)

ggplot(all_fish_sub1,aes(y=SpeciesRichness,x=DO_B,color=Sal_B)) + geom_point() +
  geom_line(aes(x = DO_B_smooth, y = exp(Y3)), linetype=1, colour="darkgreen", size =0.5)


#single plot with DO
SpRich_mod5 <- glm(SpeciesRichness ~ DO_B, all_fish_sub1,family=poisson)
DO_B_smooth2 <- seq(1,9,length.out=391)
Y4 <- predict(SpRich_mod5, list(DO_B = DO_B_smooth2))
plot(all_fish_sub1$DO_B, all_fish_sub1$SpeciesRichness, xlab="DO (bottom)", 
     ylab = "Species Richness", pch=16)
lines(DO_B_smooth2, exp(Y4), lwd = 2, col="darkblue")
