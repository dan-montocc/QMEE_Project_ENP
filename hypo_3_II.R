#Hypothesis 3 analysis

library(here)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(dotwhisker)
library(broom)

#load in data and cleaning
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


#evaluate collinearity of predictor variables with correlation matrix
#code referenced from STHDA.com
##http://www.sthda.com/english/wiki/correlation-matrix-an-r-function-to-do-all-you-need
source("http://www.sthda.com/upload/rquery_cormat.r")

Peri_fish_xsub <- Peri_fish_dat_sub1[,c("Temp_B","DO_B","Sal_B", 
                              "ChlA","Turb","AvgWaterDepth","Avg.PlantCover", "Avg.PeriphytonCover")]
rquery.cormat(Peri_fish_xsub)

#boxplots of Year, Month, and Area
Peri_fishfullmod <- lm(logBiomass ~ Temp_B + DO_B + Sal_B + ChlA +
                    Turb + AvgWaterDepth + Avg.PeriphytonCover +
                  + Avg.PlantCover,Peri_fish_dat_sub1)

boxplot(residuals(Peri_fishfullmod) ~ Peri_fish_dat_sub1$Area) #area does not seem to effect the residuals disproportionately 
boxplot(residuals(Peri_fishfullmod) ~ Peri_fish_dat_sub1$Month)#pattern in month?
summary(Peri_fish_dat_sub1$Month == "March")
#observation in March is 1, ok to not account for month
boxplot(residuals(Peri_fishfullmod) ~ Peri_fish_dat_sub1$Year)
 #Year does not have clear effect size differences
boxplot(residuals(Peri_fishfullmod) ~ Peri_fish_dat_sub1$FunctionalGroup) #include as categorical
summary(Peri_fish_dat_sub1$FunctionalGroup)

#FUNCTIONAL GROUP BIOMASS
## this automates most of the name changes
rename_term <- function(x) {
  return(x
         %>% str_replace("DO_B","Dissolved oxygen")
         %>% str_replace("Turb","Turbidity")
         %>% str_replace("Temp_B","Temperature")
         %>% str_replace("Sal_B","Salinity")
         %>% str_replace("ChlA","Chlorophyll-A")
         %>% str_replace("AvgWaterDepth","Water depth")
         %>% str_replace("Avg.PlantCover","% Plant Cover")
         %>% str_replace("Avg.PeriphytonCover","% Periphyton Cover")
         ## BMB: we have to specify a **replacement** value, in this
         ## case it's whatever alphabetic (or slash) characters come after
         ## "FunctionalGroup"
         %>% str_replace("FunctionalGroup([[:alpha:]/]+)","\\1")
         ## uppercase first letter in string, and first letter following a colon
         %>% str_replace_all("(^|:)[[:alpha:]]",toupper) 
  )
}

## scale all numeric variables except the response
## (REALLY need to scale vars if you're going to draw a coefficient plot for effects with different units!
Peri_fish_scale <- Peri_fish_dat_sub1 %>% mutate(across(where(is.numeric) & !logBiomass, ~drop(scale(.))))

## this formula is admittedly a little bit inscrutable but is what is needed to get separate
## estimates for each guild.
## alternatively you *could* fit each guild separately (this is a slightly different model, and
## wouldn't allow you to test interactions [differences among guilds])
## you could probably also do this with 'effects' or 'emmeans' packages
Peri_fish_sep <- lm (logBiomass ~ -1 + FunctionalGroup + (AvgWaterDepth + ChlA + Sal_B +
                                                            Avg.PlantCover + Avg.PeriphytonCover +
                                                            Temp_B + DO_B + Turb):FunctionalGroup, Peri_fish_scale)

op <- par(mar=c(2,2,2,2),mfrow=c(2,2))  ## BMB avoid interactive pauses
plot(Peri_fish_sep)
par(op)

Peri_fish_full <- lm (logBiomass ~ (AvgWaterDepth + ChlA + Sal_B + Avg.PlantCover + Avg.PeriphytonCover +
                                    Temp_B + DO_B + Turb)*FunctionalGroup, Peri_fish_dat_sub1)

tt <- (broom::tidy(Peri_fish_sep, conf.int=TRUE)
    ## add "(Intercept)" to intercept terms
    %>% mutate(across(term,~ifelse(grepl("ivore$",.),paste(.,"(Intercept)",sep=":"), .)))
    ## split into guild + environmental covariate
    %>% separate(term,into=c("Functional_Group","term"),sep=":")
    ## don't need prefix
    %>% mutate(across(Functional_Group,~str_remove(.,"FunctionalGroup")))
    %>% mutate(across(term, rename_term))
    ## generally not interested in intercept, and messes up axes
    %>% filter(term!="(Intercept)")  
    ## order terms by average estimate (across guilds)
    ## (guilds differ slightly, but this still works pretty well)
    %>% mutate(across(term, ~reorder(factor(.), estimate)))
)

gg0 <- (ggplot(tt, aes(estimate, term))
        + geom_pointrange(aes(xmin=conf.low, xmax=conf.high))
        + geom_vline(xintercept=0, lty=2)
)

print(gg0 + facet_wrap(~Functional_Group, labeller=label_both))

## or (this is more compact, and better if you want to compare guilds)

library(colorspace)
print(ggplot(tt, aes(estimate, term))
      + geom_pointrange(aes(xmin=conf.low, xmax=conf.high,
                            colour=Functional_Group),
                        position=position_dodge(width=0.5))
      + geom_vline(xintercept=0, lty=2)
      + scale_color_discrete_qualitative() +
        theme_bw() + xlab("Coefficient estimate") + ylab("") + labs(color='Functional Group') 
)

##ALL Fish Model
Peri_all_dat <- read.csv(here("Joined_Cleaned_Data/Hypothesis_3/Peri_WQ_FishRichness_1995to2005_Join.csv"))
Peri_all_dat <- Peri_all_dat[,-c(1)]
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

#create full model
Fish_Peri_dat_scale <- Peri_all_dat_sub1 %>% mutate(across(where(is.numeric) & !logBiomass & !SpeciesRichness, ~drop(scale(.))))
Peri_allfishfullmod <- lm(logBiomass ~ Temp_B + DO_B + Sal_B + ChlA +
                                  Turb + AvgWaterDepth + Avg.PeriphytonCover +
                                  + Avg.PlantCover,Peri_all_dat_sub1)
Peri_allfishfullmod_scale <- lm(logBiomass ~ Temp_B + DO_B + Sal_B + ChlA +
                         Turb + AvgWaterDepth + Avg.PeriphytonCover +
                         + Avg.PlantCover,Fish_Peri_dat_scale)
plot(Peri_allfishfullmod)
