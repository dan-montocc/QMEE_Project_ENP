###re-run code from start to finish to clean up appending 1995 to 2000 and functional and thermal guild column join

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

fishdat96_05_Agg <- aggregate(cbind(TotalSpeciesWeight,SpeciesBiomass) ~ Month + Year + Area + SpeciesName, func96_05 , mean)

write.csv(fishdat96_05_Agg, file = "ENP_FishFunctionalGrp_1996to2005.csv")


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
wq_96_05_mean <- aggregate(cbind(DEPTH, NOX, NO3, NO2,NH4, TN, DIN, TON, TP, SRP, CHLA,TOC,SAL_S, SAL_B, TEMP_S,TEMP_B, DO_S,DO_B,TURB,pH) ~ Month + Year + Area, wq_merge , mean)

fish_wq_merge <- merge(fishdat96_05_Agg, wq_96_05_mean, all.x=TRUE)
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
write.csv(fish_wq_merge, file = here("Joined_Cleaned_Data/FishJoinWQ_1996to2005.csv"))



#aggregate to all fish by month and year
library(tidyverse)
ALL_fish_wq <- subset(fish_wq_merge, select = -c(SpeciesName,FunctionalGroup,ThermalGuild)) #drop species specific fields
library(data.table)
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

write.csv(ALL_fish_wq, file = here("Joined_Cleaned_Data/ALL_FishJoinWQ_1996to2005.csv"))




##data joining and cleaning for hypotheses 2 and 3
Peri_dat <- read.csv("ENP_HabitatData.csv")
Peri_dat <- aggregate(cbind(Avg.PlantCover,AvgPlantHeight,Avg.PeriphytonCover,AvgPeriphytonVolume,AvgWaterDepth) ~ Month + Year + Area, Peri_dat , mean)

##checking for NAs
which(Peri_dat == -9999)

##merge WQ to habitat
Peri_wq <- merge(Peri_dat, wq_96_05_mean,all.x=TRUE)
#remove NAs
Peri_wq <- replace_with_na_all(data = Peri_wq, condition = ~.x == -9999)

write.csv(Peri_wq, file = here("Joined_Cleaned_Data/ENP_Peri_WQ_1995to2005_Join.csv"))

##merge fish to peri??
#suggest we may omit this hypothesis
#alright by me! -M