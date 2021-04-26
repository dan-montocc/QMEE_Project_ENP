###re-run code from start to finish to clean up appending 1995 to 2000 and functional and thermal guild column join
library(tidyverse)
##fish functional group variable add
funcgrps <- read.csv("fish_func_therm_groups.csv", header = TRUE)
funcgrps <- funcgrps %>% rename(SpeciesName = ï..SpeciesName) #rename column (why the added characters???)
fishdat96 <- read.csv("ENP_FishData_1996to2000.csv", header = TRUE)
fishdat2000 <- read.csv("ENP_FishData_2000to2005.csv", header = TRUE)

#two dataset appending
fishdat96_05 <- rbind(fishdat96,fishdat2000)
3011+3912
dim(fishdat96_05)

##fish functional group variable add
library(dplyr)
func96_05 <- inner_join(funcgrps,fishdat96_05, by ="SpeciesName" )

##loss of observation...why?
library(arsenal)
summary(comparedf(func96_05, fishdat96_05))##??????????


##aggregate to month level for year, site, and fish species
#new fish aggregated dataset
#remove unnecessary columns
fishdat96_05_sub1 <- func96_05[,-c(3,5,9,10,11,13)]

fishdat96_05_Agg1 <- aggregate(cbind(TotalSpeciesWeight,SpeciesBiomass) ~ Month + Year + Area + SpeciesName + CommonName + ThermalGuild, fishdat96_05_sub1 , mean)

write.csv(fishdat96_05_Agg1, file = "ENP_FishThermalGuild_1996to2005.csv")

fishdat96_05_sub2 <- func96_05[,-c(4,5,9,10,11,13)]

fishdat96_05_Agg2 <- aggregate(cbind(TotalSpeciesWeight,SpeciesBiomass) ~ Month + Year + Area + SpeciesName + CommonName + FunctionalGroup, fishdat96_05_sub2 , mean)

write.csv(fishdat96_05_Agg2, file = "ENP_FishFuncGrp_1996to2005.csv")

##water quality site name to area join
enp_wq <- read.csv("ENP_WQ_1996to2005.csv", header = TRUE)
wq_area_name <- read.csv("WQ_Parameter_SiteJoin.csv", header = TRUE)

wq_merge <- merge(enp_wq, wq_area_name,all=TRUE)
which(is.na(wq_merge$Area))

##loss of observations...why?
summary(comparedf(enp_wq, wq_merge))
list(as.factor(enp_wq$SITE))
class(enp_wq$SITE)
enp_wq$SITE <- as.factor(enp_wq$SITE)
levels(enp_wq$SITE)
##Ponce de Leon Bay MISSPELLED!
#fixed

write.csv(wq_merge, file = "ENP_WQ_1996to2005_AreaAdd.csv")


#join WQ to fish

#new wq aggregated dataset
##cleaned in excel for month add
wq_96_05 <- read.csv("ENP_WQ_1996to2005_MonthAdd.csv")
wq_96_05_mean <- aggregate(cbind(DEPTH, NOX, NO3, NO2,NH4, TN, DIN, TON, TP, SRP, CHLA,TOC,SAL_S, SAL_B, TEMP_S,TEMP_B, DO_S,DO_B,TURB,pH) ~ Month + Year + Area, wq_96_05 , mean)

fish_wq_merge <- merge(fishdat96_05_Agg2, wq_96_05_mean, all.x=TRUE)
##check merge for accuracy....

which(is.na(fish_wq_merge$DO))
summary(fish_wq_merge$TotalSpeciesWeight)

#convert -9999 to na
library(naniar)
fish_wq_merge <- replace_with_na_all(data = fish_wq_merge, condition = ~.x == -9999)
summary(fish_wq_merge$TotalSpeciesWeight)
summary(fish_wq_merge$SpeciesBiomass)
summary(fish_wq_merge$DO_S)
summary(fish_wq_merge$TEMP_B)
summary(fish_wq_merge$pH)

#create and move file to finalized data folder
library(here)
write.csv(fish_wq_merge, file = here("Joined_Cleaned_Data/Hypothesis_1/FishSpecies_Join_WQ_1996to2005.csv"))


#aggregate to functional group level
library(data.table)

DT <- data.table(fish_wq_merge)
func_fish_wq <- DT[, list(TotalWeight = sum(TotalSpeciesWeight,na.rm=TRUE), TotalBiomass = sum(SpeciesBiomass,na.rm=TRUE),
                         Depth = mean(DEPTH,na.rm=TRUE), NOX = mean(NOX,na.rm=TRUE), NO3 = mean(NO3,na.rm=TRUE),
                         NO2 = mean(NO2,na.rm=TRUE),NH4 = mean(NH4,na.rm=TRUE), TN = mean(TN,na.rm=TRUE), 
                         DIN= mean(DIN,na.rm=TRUE), TON=mean(TON,na.rm=TRUE), TP=mean(TP,na.rm=TRUE),
                         SRP=mean(SRP,na.rm=TRUE), ChlA=mean(CHLA,na.rm=TRUE),TOC=mean(TOC,na.rm=TRUE),
                         Sal_S=mean(SAL_S,na.rm=TRUE), Sal_B=mean(SAL_B,na.rm=TRUE),Temp_S=mean(TEMP_S,na.rm=TRUE),
                         Temp_B=mean(TEMP_B,na.rm=TRUE), DO_S=mean(DO_S,na.rm=TRUE),DO_B=mean(DO_B,na.rm=TRUE),
                         Turb=mean(TURB,na.rm=TRUE),pH=mean(pH,na.rm=TRUE)), by = list(Month,Year,Area,FunctionalGroup)]##not elegant, but works
#remove NaN's
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

func_fish_wq[is.nan(func_fish_wq)] <- NA

write.csv(func_fish_wq, file = here("Joined_Cleaned_Data/Hypothesis_3/FishFuncGrp_Join_WQ_1996to2005.csv"))

#aggregate to thermal guild level
fish_wq_merge2 <- merge(fishdat96_05_Agg1, wq_96_05_mean, all.x=TRUE)
fish_wq_merge2 <- replace_with_na_all(data = fish_wq_merge2, condition = ~.x == -9999)

DT <- data.table(fish_wq_merge2)
therm_fish_wq <- DT[, list(TotalWeight = sum(TotalSpeciesWeight,na.rm=TRUE), TotalBiomass = sum(SpeciesBiomass,na.rm=TRUE),
                          Depth = mean(DEPTH,na.rm=TRUE), NOX = mean(NOX,na.rm=TRUE), NO3 = mean(NO3,na.rm=TRUE),
                          NO2 = mean(NO2,na.rm=TRUE),NH4 = mean(NH4,na.rm=TRUE), TN = mean(TN,na.rm=TRUE), 
                          DIN= mean(DIN,na.rm=TRUE), TON=mean(TON,na.rm=TRUE), TP=mean(TP,na.rm=TRUE),
                          SRP=mean(SRP,na.rm=TRUE), ChlA=mean(CHLA,na.rm=TRUE),TOC=mean(TOC,na.rm=TRUE),
                          Sal_S=mean(SAL_S,na.rm=TRUE), Sal_B=mean(SAL_B,na.rm=TRUE),Temp_S=mean(TEMP_S,na.rm=TRUE),
                          Temp_B=mean(TEMP_B,na.rm=TRUE), DO_S=mean(DO_S,na.rm=TRUE),DO_B=mean(DO_B,na.rm=TRUE),
                          Turb=mean(TURB,na.rm=TRUE),pH=mean(pH,na.rm=TRUE)), by = list(Month,Year,Area,ThermalGuild)]##not elegant, but works
#remove NaN's
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

therm_fish_wq[is.nan(therm_fish_wq)] <- NA

write.csv(therm_fish_wq, file = here("Joined_Cleaned_Data/Hypothesis_1/FishThermGuild_Join_WQ_1996to2005.csv"))


#aggregate to all fish by month and year
ALL_fish_wq <- subset(fish_wq_merge, select = -c(SpeciesName,FunctionalGroup, CommonName)) #drop species specific fields

DT <- data.table(ALL_fish_wq)
ALL_fish_wq <- DT[, list(TotalWeight = sum(TotalSpeciesWeight,na.rm=TRUE), TotalBiomass = sum(SpeciesBiomass,na.rm=TRUE),
          Depth = mean(DEPTH,na.rm=TRUE), NOX = mean(NOX,na.rm=TRUE), NO3 = mean(NO3,na.rm=TRUE),
          NO2 = mean(NO2,na.rm=TRUE),NH4 = mean(NH4,na.rm=TRUE), TN = mean(TN,na.rm=TRUE), 
          DIN= mean(DIN,na.rm=TRUE), TON=mean(TON,na.rm=TRUE), TP=mean(TP,na.rm=TRUE),
          SRP=mean(SRP,na.rm=TRUE), ChlA=mean(CHLA,na.rm=TRUE),TOC=mean(TOC,na.rm=TRUE),
          Sal_S=mean(SAL_S,na.rm=TRUE), Sal_B=mean(SAL_B,na.rm=TRUE),Temp_S=mean(TEMP_S,na.rm=TRUE),
          Temp_B=mean(TEMP_B,na.rm=TRUE), DO_S=mean(DO_S,na.rm=TRUE),DO_B=mean(DO_B,na.rm=TRUE),
          Turb=mean(TURB,na.rm=TRUE),pH=mean(pH,na.rm=TRUE)), by = list(Month,Year,Area)]##not elegant, but works

#remove NaN's
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

ALL_fish_wq[is.nan(ALL_fish_wq)] <- NA

#create species richness column for month, year, area
fish_SpeciesRichness <- count(fish_wq_merge, Year, Month, Area)

ALL_fish_wq_richness <- merge(ALL_fish_wq,fish_SpeciesRichness)
ALL_fish_wq_richness <- ALL_fish_wq_richness %>% rename(SpeciesRichness = n) #rename added column
ALL_fish_wq_richness <- ALL_fish_wq_richness[-c(18,20,21,22),] #removes rows with no WQ data
ALL_fish_wq_richness <- replace_with_na_all(data = ALL_fish_wq_richness, condition = ~.x == -9999)

write.csv(ALL_fish_wq_richness, file = here("Joined_Cleaned_Data/Hypothesis_1/ALL_FishJoinWQ_1996to2005.csv"))




##data joining and cleaning for hypotheses 2 and 3
Peri_dat <- read.csv("ENP_HabitatData.csv")
Peri_dat <- replace_with_na_all(data = Peri_dat, condition = ~.x == -9999)
Peri_dat <- aggregate(cbind(Avg.PlantCover,AvgPlantHeight,Avg.PeriphytonCover,AvgPeriphytonVolume,AvgWaterDepth) ~ Month + Year + Area, Peri_dat , mean)


##merge WQ to habitat
Peri_wq <- merge(Peri_dat, wq_96_05_mean,all.x=TRUE)
Peri_wq <- replace_with_na_all(data = Peri_wq, condition = ~.x == -9999)
write.csv(Peri_wq, file = here("Joined_Cleaned_Data/Hypothesis_2/ENP_Peri_WQ_1995to2005_Join.csv"))

##merge fish to peri??
#suggest we may omit this hypothesis
#alright by me! -M
Peri_fish_wq <- merge(Peri_dat, func_fish_wq,all.x=TRUE)

Peri_fish_wq <- replace_with_na_all(data = Peri_fish_wq, condition = ~.x == -9999)

write.csv(Peri_fish_wq, file = here("Joined_Cleaned_Data/Hypothesis_3/Peri_WQ_FishFuncGrp_1995to2005_Join.csv"))


#aggregate to all fish level
All_fish_Sp_Agg <- aggregate(cbind(TotalSpeciesWeight,SpeciesBiomass) ~ Month + Year + Area + SpeciesName + CommonName, func96_05 , mean)
All_fish_Sp_Agg <- filter(All_fish_Sp_Agg, SpeciesBiomass > 0)
Peri_fish_wq <- merge(Peri_wq, All_fish_Sp_Agg,all.x=TRUE)
library(data.table)
DT <- data.table(Peri_fish_wq)

ALL_fish_Peri <- DT[, list(TotalWeight = sum(TotalSpeciesWeight,na.rm=TRUE), TotalBiomass = sum(SpeciesBiomass,na.rm=TRUE),
                           Depth = mean(DEPTH,na.rm=TRUE), NOX = mean(NOX,na.rm=TRUE), NO3 = mean(NO3,na.rm=TRUE),
                           NO2 = mean(NO2,na.rm=TRUE),NH4 = mean(NH4,na.rm=TRUE), TN = mean(TN,na.rm=TRUE), 
                           DIN= mean(DIN,na.rm=TRUE), TON=mean(TON,na.rm=TRUE), TP=mean(TP,na.rm=TRUE),
                           SRP=mean(SRP,na.rm=TRUE), ChlA=mean(CHLA,na.rm=TRUE),TOC=mean(TOC,na.rm=TRUE),
                           Sal_S=mean(SAL_S,na.rm=TRUE), Sal_B=mean(SAL_B,na.rm=TRUE),Temp_S=mean(TEMP_S,na.rm=TRUE),
                           Temp_B=mean(TEMP_B,na.rm=TRUE), DO_S=mean(DO_S,na.rm=TRUE),DO_B=mean(DO_B,na.rm=TRUE),
                           Turb=mean(TURB,na.rm=TRUE),pH=mean(pH,na.rm=TRUE),
                           Avg.PeriphytonCover=mean(Avg.PeriphytonCover,na.rm=TRUE),
                           Avg.PlantCover=mean(Avg.PlantCover,na.rm=TRUE),
                           AvgWaterDepth=mean(AvgWaterDepth,na.rm=TRUE)),by = list(Month,Year,Area)]##not elegant, but works

#remove NaN's
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

ALL_fish_Peri[is.nan(ALL_fish_Peri)] <- NA
ALL_fish_Peri <- ALL_fish_Peri %>% filter(!is.na(Depth))
fish_SpeciesRichness <- count(All_fish_Sp_Agg, Year, Month, Area)

ALL_fish_Peri_richness <- merge(ALL_fish_Peri,fish_SpeciesRichness)
ALL_fish_Peri_richness <- ALL_fish_Peri_richness %>% rename(SpeciesRichness = n) #rename added column

write.csv(ALL_fish_Peri_richness, file = here("Joined_Cleaned_Data/Hypothesis_3/Peri_WQ_FishRichness_1995to2005_Join.csv"))



###final r data creation for hypotheses testing

#Hypothesis 1 Data
#ALL FISH

all_fish <- read.csv(here("Joined_Cleaned_Data/Hypothesis_1/ALL_FishJoinWQ_1996to2005.csv"))
all_fish <- filter(all_fish, TotalBiomass != 0)
all_fish$logBiomass <- log10(all_fish$TotalBiomass)
all_fish <- filter(all_fish, SRP > 0)
all_fish <- filter(all_fish, Sal_B > 0)
all_fish <- filter(all_fish, TOC > 0)
all_fish <- all_fish %>% filter(!is.na(DO_B))
summary(all_fish)
saveRDS(all_fish, file = "Joined_Cleaned_Data/Hypothesis_1/AllFish_AggregatedData.rds")

#THERMAL GUILD
fish_therm <- read.csv("Joined_Cleaned_Data/Hypothesis_1/FishThermGuild_Join_WQ_1996to2005.csv")
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
fish_therm_sub1$logBiomass <- log10(fish_therm_sub1$TotalBiomass)
summary(fish_therm_sub1)
saveRDS(fish_therm_sub1, file = "Joined_Cleaned_Data/Hypothesis_1/Fish_ThermalGuild_AggregatedData.rds")


#Hypothesis 2 Data

Peri_dat <- read.csv(here("Joined_Cleaned_Data/Hypothesis_2/ENP_Peri_WQ_1995to2005_Join.csv"))
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
saveRDS(Peri_dat_sub1, file = "Joined_Cleaned_Data/Hypothesis_2/Peri_WQ_AggregatedData.rds")


#Hypothesis 3 Data

#ALL FISH
Peri_all_dat <- read.csv(here("Joined_Cleaned_Data/Hypothesis_3/Peri_WQ_FishRichness_1995to2005_Join.csv"))
summary(Peri_all_dat)
Peri_all_dat_sub1 <- filter(Peri_all_dat, Avg.PeriphytonCover != 0)
Peri_all_dat_sub1 <- filter(Peri_all_dat_sub1, SRP > 0)
Peri_all_dat_sub1 <- filter(Peri_all_dat_sub1, Sal_B > 0)
Peri_all_dat_sub1 <- filter(Peri_all_dat_sub1, TOC > 0)
Peri_all_dat_sub1 <- Peri_all_dat_sub1 %>% filter(!is.na(DO_B))
Peri_all_dat_sub1 <- filter(Peri_all_dat_sub1, TotalBiomass > 0)
Peri_all_dat_sub1$logBiomass <- log10(Peri_all_dat_sub1$TotalBiomass)
Peri_all_dat_sub1$Area <- as.factor(Peri_all_dat_sub1$Area)
Peri_all_dat_sub1$Month <- as.factor(Peri_all_dat_sub1$Month)
Peri_all_dat_sub1$Year <- as.factor(Peri_all_dat_sub1$Year)
saveRDS(Peri_all_dat_sub1, file = "Joined_Cleaned_Data/Hypothesis_3/Peri_AllFish.rds")

#FUNCTIONAL GROUP
Peri_fish_dat <- read.csv(here("Joined_Cleaned_Data/Hypothesis_3/Peri_WQ_FishFuncGrp_1995to2005_Join.csv"))
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
saveRDS(Peri_fish_dat_sub1, file = "Joined_Cleaned_Data/Hypothesis_3/Peri_WQ_FishFuncGrp_Join.rds")
