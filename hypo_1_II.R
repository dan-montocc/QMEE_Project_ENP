##Hypothesis 1

####Take 2....
#load-in libraries...
library(here)
library(ggplot2)
library(tidyverse)
library(corrplot)


#load in data
all_fish <- read.csv(here("Joined_Cleaned_Data/Hypothesis_1/ALL_FishJoinWQ_1996to2005.csv"))
all_fish <- all_fish[,-c(1)]
fish_therm <- read.csv("Joined_Cleaned_Data/Hypothesis_1/FishThermGuild_Join_WQ_1996to2005.csv")
fish_therm <- fish_therm[,-c(1)]

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
##Full Model
all_fish$logBiomass <- log10(all_fish$TotalBiomass)
fullfishmod <- lm(logBiomass ~ Depth + NH4 + ChlA + Sal_B +
                      Temp_B + DO_B + Turb, all_fish)

## set 2x2 format to avoid pause when running non-interactively
op <- par(mar=c(4.5,4.5,2,2),mfrow=c(2,2))
plot(fullfishmod, sub.caption = "")

#79 and 80 obs have high leverage
##removed
all_fish_sub1 <- all_fish[-c(79,80),]

fullfishmod <- lm(logBiomass ~ Depth + NH4 + ChlA + Sal_B +
                      Temp_B + DO_B + Turb, all_fish_sub1,
                  ## make sure NAs are included in residuals where appropriate
                  na.action=na.exclude)

plot(fullfishmod, sub.caption = "")
par(op) ## restore original graphical params

#look at categorical variable effect size on residuals
boxplot(residuals(fullfishmod) ~ all_fish_sub1$Area) #area does not seem to affect the residuals disproportionately 
boxplot(residuals(fullfishmod) ~ all_fish_sub1$Month)#March different
summary(all_fish$Month == "March")# only one obs 
summary(all_fish$Month == "May")
##since study area is Florida, seasons are not as pronounced as they are in north
boxplot(residuals(fullfishmod) ~ all_fish_sub1$Year) #no specific Year stands out

#Scaled Full Model
all_fish_scale <- all_fish_sub1 %>% mutate(across(where(is.numeric) & !logBiomass, ~drop(scale(.))))
fullfishmod_scaled <- lm(logBiomass ~ Depth + NH4 + ChlA + Sal_B +
                           Temp_B + DO_B + Turb, all_fish_scale,
                         ## make sure NAs are included in residuals where appropriate
                         na.action=na.exclude)
op <- par(mar=c(4.5,4.5,2,2),mfrow=c(2,2))
plot(fullfishmod_scaled, sub.caption = "")
par(op)


#coefficient plot
library(dotwhisker)
library(broom)

ov <- names(sort(abs((coef(fullfishmod_scaled))),decreasing=TRUE))
ov

#Scaled Full Model Coefficient plot
dwplot(fullfishmod_scaled) %>%
  relabel_predictors(c(Depth = "Depth",
                       DO_B = "Dissolved oxygen",
                       Turb = "Turbidity",
                       Sal_B = "Salinity",
                       Temp_B = "Temperature",
                       ChlA = "Chlorophyll-a",
                       NH4 = "NH4")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + theme(legend.position = "none") +
  geom_vline(xintercept=0,lty=2)

#Subset Model
#build model without DO and temp to assess effect size of these two
#scaled
subfishmod_scaled <- lm(logBiomass ~ Depth + NH4 + ChlA + Sal_B + Turb, all_fish_scale,
                        na.action=na.exclude)

#NOT scaled Sub Model
subfishmod <- lm(logBiomass ~ Depth + NH4 + ChlA + Sal_B + Turb, all_fish_sub1,
                 na.action=na.exclude)

#Scaled Full and Subset Model Coefficient Plot
dwplot(list(fullfishmod_scaled,subfishmod_scaled)) %>%
  relabel_predictors(c(Depth = "Depth",
                       DO_B = "Dissolved oxygen",
                       Turb = "Turbidity",
                       Sal_B = "Salinity",
                       Temp_B = "Temperature",
                       ChlA = "Chlorophyll-a",
                       NH4 = "NH4")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + 
  geom_vline(xintercept=0,lty=2) + 
  theme(legend.background = element_rect(colour="white"),
        legend.title = element_blank()) 

#Scaled Models ANOVA
anova(fullfishmod_scaled,subfishmod_scaled)

#NOT Scaled Subset and Full Model Coefficient plot
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

#NOT scaled Subset and Full Model ANOVA
anova(fullfishmod,subfishmod)

#SUMMARY...selected model output 

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

#Full Model
#build full model with thermal guild interaction
fishthermmod <- lm (logBiomass ~ (Depth + NH4 + ChlA + Sal_B +
                      Temp_B + DO_B + Turb)*ThermalGuild, fish_therm_sub1)
op <- par(mar=c(4.5,4.5,2,2),mfrow=c(2,2))
plot(fishthermmod, sub.caption = "")

#remove obs with leverage of 1 = 4, 101, 235, 240
fish_therm_sub2 <- fish_therm_sub1[-c(4,101,235,240),]
fishthermmod <- lm (logBiomass ~ (Depth + NH4 + ChlA + Sal_B +
                                    Temp_B + DO_B + Turb)*ThermalGuild, fish_therm_sub2)
plot(fishthermmod, sub.caption = "")
par(op)

#creating faceted coefficient plot for thermal guild data
## BMB's code

## this automates most of the name changes
rename_term1 <- function(x) {
    return(x
        %>% str_replace("DO_B","Dissolved oxygen")
        %>% str_replace("Turb","Turbidity")
        %>% str_replace("Sal_B","Salinity")
        %>% str_replace("ChlA","Chlorophyll-A")
        ## replace "ThermalGuildcold/warm" (e.g.) with "warm/cold guild"
        %>% str_replace("ThermalGuild([[:alpha:]/]+)","\\1 guild")
        ## uppercase first letter in string, and first letter following a colon
        %>% str_replace_all("(^|:)[[:alpha:]]",toupper) 
    )
}

## scale all numeric variables except the response
## (REALLY need to scale vars if you're going to draw a coefficient plot for effects with different units!
fish_therm_scale <- fish_therm_sub2 %>% mutate(across(where(is.numeric) & !logBiomass, ~drop(scale(.))))

## this formula is admittedly a little bit inscrutable but is what is needed to get separate
## estimates for each guild.
## alternatively you *could* fit each guild separately (this is a slightly different model, and
## wouldn't allow you to test interactions [differences among guilds])
## you could probably also do this with 'effects' or 'emmeans' packages
fishthermmod_sep <- lm (logBiomass ~ -1 + ThermalGuild + (Depth + NH4 + ChlA + Sal_B +
                                    Temp_B + DO_B + Turb):ThermalGuild, fish_therm_scale)

op <- par(mar=c(4.5,4.5,2,2),mfrow=c(2,2))
plot(fishthermmod_sep, sub.caption = "")
par(op)

tt1 <- (broom::tidy(fishthermmod_sep, conf.int=TRUE)
    ## add "(Intercept)" to intercept terms
    %>% mutate(across(term,~ifelse(grepl("(cool|warm)$",.),paste(.,"(Intercept)",sep=":"), .)))
    ## split into guild + environmental covariate
    %>% separate(term,into=c("thermal_guild","term"),sep=":")
    ## don't need prefix
    %>% mutate(across(thermal_guild,~str_remove(.,"ThermalGuild")))
    ## fix names
    %>% mutate(across(term, rename_term1))
    ## generally not interested in intercept, and messes up axes
    %>% filter(term!="(Intercept)")  
    ## order terms by average estimate (across guilds)
    ## (guilds differ slightly, but this still works pretty well)
    %>% mutate(across(term, ~reorder(factor(.), estimate)))
)

#gg0 <- (ggplot(tt, aes(estimate, term))
    #+ geom_pointrange(aes(xmin=conf.low, xmax=conf.high))
    #+ geom_vline(xintercept=0, lty=2)
#)

#print(gg0 + facet_wrap(~thermal_guild, labeller=label_both))

#Guild Coefficient Plot
library(colorspace)
print(ggplot(tt, aes(estimate, term))
    + geom_pointrange(aes(xmin=conf.low, xmax=conf.high,
                          colour=thermal_guild),
                      position=position_dodge(width=0.25))
    + geom_vline(xintercept=0, lty=2)
    + scale_color_discrete_qualitative() +
      theme_bw() + xlab("Coefficient estimate") + ylab("") + labs(color='Thermal Guild') 
    )

#SUMMARY..selected model output


##SPECIES RICHNESS MODEL

#NOT Scaled Full Model
SpRich_mod1 <- glm(SpeciesRichness ~ Depth + NH4 + ChlA + Sal_B +
                     DO_B + Temp_B + Turb, all_fish,family=poisson)

op <- par(mar=c(4.5,4.5,2,2),mfrow=c(2,2))
plot(SpRich_mod1, sub.caption = "")
par(op)

#remove obs 80 and 79
all_fish_sub2 <- all_fish[-c(79,80),]
SpRich_mod1 <- glm(SpeciesRichness ~ Depth + NH4 + ChlA + Sal_B +
                     DO_B + Temp_B + Turb, all_fish_sub2,family=poisson)

op <- par(mar=c(4.5,4.5,2,2),mfrow=c(2,2))
plot(SpRich_mod1, sub.caption = "")
par(op)

#Scaled Full Model
Rich_fish_scale <- all_fish_sub2 %>% mutate(across(where(is.numeric) & !SpeciesRichness, ~drop(scale(.))))

SpRich_mod_scale <- glm(SpeciesRichness ~ Depth + NH4 + ChlA + Sal_B +
                     DO_B + Temp_B + Turb, Rich_fish_scale,family=poisson)

op <- par(mar=c(4.5,4.5,2,2),mfrow=c(2,2))
plot(SpRich_mod_scale, sub.caption = "")
par(op)

#NOT Scaled Subset Model
SpRich_mod2 <- glm(SpeciesRichness ~ Depth + NH4 + ChlA + Sal_B +
                     Turb, all_fish_sub2,family=poisson)

#Scaled Subset Model
SpRich_mod2_scale <- glm(SpeciesRichness ~ Depth + NH4 + ChlA + Sal_B +
                     Turb, Rich_fish_scale,family=poisson)

#NOT Scaled Full and Subset Model Coefficient plot
library(dotwhisker)
ov3 <- names(sort(abs(coef(SpRich_mod1)),decreasing=TRUE))
ov3

dwplot(list(SpRich_mod1,SpRich_mod2)) %>%
  relabel_predictors(c(Depth = "Depth",
                       Sal_B = "Salinity",
                       DO_B = "Dissolved oxygen",
                       Turb = "Turbidity",
                       Temp_B = "Temperature",
                       NH4 = "NH4",
                       ChlA = "Chlorophyll-a")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + 
  geom_vline(xintercept=0,lty=2) + 
  theme(legend.background = element_rect(colour="white"),
        legend.title = element_blank()) 

dwplot(SpRich_mod1) %>%
  relabel_predictors(c(DO_B = "Dissolved oxygen",
                       Turb = "Turbidity",
                       Temp_B = "Temperature",
                       NH4 = "NH4",
                       "ChlA" = "Chlorophyll-a",
                       Sal_B = "Salinity",
                       Depth = "Depth")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + 
  geom_vline(xintercept=0,lty=2) + 
  theme(legend.position = "none") 

#Scaled Full and Subset Model Coefficient plot
ov3 <- names(sort(abs(coef(SpRich_mod_scale)),decreasing=TRUE))
ov3

dwplot(list(SpRich_mod_scale,SpRich_mod2_scale)) %>%
  relabel_predictors(c(Depth = "Depth",
                       Sal_B = "Salinity",
                       DO_B = "Dissolved oxygen",
                       Turb = "Turbidity",
                       Temp_B = "Temperature",
                       NH4 = "NH4",
                       ChlA = "Chlorophyll-a")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + 
  geom_vline(xintercept=0,lty=2) + 
  theme(legend.background = element_rect(colour="white"),
        legend.title = element_blank()) 

dwplot(SpRich_mod1) %>%
  relabel_predictors(c(DO_B = "Dissolved oxygen",
                       Turb = "Turbidity",
                       Temp_B = "Temperature",
                       NH4 = "NH4",
                       "ChlA" = "Chlorophyll-a",
                       Sal_B = "Salinity",
                       Depth = "Depth")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + 
  geom_vline(xintercept=0,lty=2) + 
  theme(legend.position = "none") 

#NOT Scaled Full and Sub ANOVA
anova(SpRich_mod1,SpRich_mod2)

#Scaled Full and Sub ANOVA
anova(SpRich_mod_scale,SpRich_mod2_scale)

#SUMMARY...selected model output