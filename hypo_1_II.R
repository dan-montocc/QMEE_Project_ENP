##Hypothesis 1

####Take 2....
#load-in libraries...
library(here)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(car)

#aesthetics load-in
library(extrafont)
windowsFonts(A = windowsFont("Times New Roman"))
library(ggsci)

#load in data
all_fish <- readRDS("Joined_Cleaned_Data/Hypothesis_1/AllFish_AggregatedData.rds")
fish_therm <- readRDS("Joined_Cleaned_Data/Hypothesis_1/Fish_ThermalGuild_AggregatedData.rds")

#check for variable redundancy in explanatory variables
#response variables = TotalBiomass, SpeciesRichness
#potential predictors = Temp (bottom and surface), DO (bottom and surface), Salinity (bottom and surface), water depth

#evaluate collinearity of predictor variables with correlation matrix
#code referenced from STHDA.com
##http://www.sthda.com/english/wiki/correlation-matrix-an-r-function-to-do-all-you-need
source("http://www.sthda.com/upload/rquery_cormat.r")
all_fish_xsub <- all_fish[,c("Temp_S", "Temp_B","DO_S","DO_B","Depth","Sal_S","Sal_B", 
                             "NH4", "ChlA","Turb")]
names(all_fish_xsub)[names(all_fish_xsub) == "Temp_B"] <- "Temp(bottom)"
names(all_fish_xsub)[names(all_fish_xsub) == "DO_B"] <- "DO(bottom)"
names(all_fish_xsub)[names(all_fish_xsub) == "Temp_S"] <- "Temp(surface)"
names(all_fish_xsub)[names(all_fish_xsub) == "DO_S"] <- "DO(surface)"
names(all_fish_xsub)[names(all_fish_xsub) == "Turb"] <- "Turbidity"
names(all_fish_xsub)[names(all_fish_xsub) == "ChlA"] <- "Chlorophyll-a"
names(all_fish_xsub)[names(all_fish_xsub) == "Sal_B"] <- "Salinity(bottom)"
names(all_fish_xsub)[names(all_fish_xsub) == "Sal_S"] <- "Salinity(surface)"

par(family = "A")
rquery.cormat(all_fish_xsub)


#ALL FISH MODEL
##Full Model
fullfishmod <- lm(logBiomass ~ Depth + NH4 + ChlA + Sal_B +
                      Temp_B + DO_B + Turb, all_fish)

## set 2x2 format to avoid pause when running non-interactively
op <- par(mar=c(4.5,4.5,2,2),mfrow=c(2,2), family = "A")
plot(fullfishmod, sub.caption = "", caption = "", family ="A")

#74 obs has high leverage
##removed
all_fish_sub1 <- all_fish[-c(74),]

fullfishmod <- lm(logBiomass ~ Depth + NH4 + ChlA + Sal_B +
                      Temp_B + DO_B + Turb, all_fish_sub1,
                  ## make sure NAs are included in residuals where appropriate
                  na.action=na.exclude)

plot(fullfishmod, sub.caption = "")
par(op) ## restore original graphical params

#look at categorical variable effect size on residuals
par(family = "A", cex = 1.2, mfrow=c(1,3), las = 2)
boxplot(residuals(fullfishmod) ~ all_fish_sub1$Area, col = c("#E64B35FF", "#4DBBD5FF"),
        ylab = "Residuals(All Fish Model)", xlab = "") #area does not seem to affect the residuals disproportionately 
boxplot(residuals(fullfishmod) ~ all_fish_sub1$Month, col = "#91D1C2FF",
        ylab = "", xlab = "")#March different
summary(all_fish$Month == "March")# only one obs 
summary(all_fish$Month == "May")
##since study area is Florida, seasons are not as pronounced as they are in north
boxplot(residuals(fullfishmod) ~ all_fish_sub1$Year, col = "#91D1C2FF",
        ylab = "", xlab = "") #no specific Year stands out

#Scaled Full Model
all_fish_scale <- all_fish_sub1 %>% mutate(across(where(is.numeric) & !logBiomass, ~drop(scale(.))))
fullfishmod_scaled <- lm(logBiomass ~ Depth + NH4 + ChlA + Sal_B +
                           Temp_B + DO_B + Turb, all_fish_scale,
                         ## make sure NAs are included in residuals where appropriate
                         na.action=na.exclude)
op <- par(mar=c(4.5,4.5,2,2),mfrow=c(2,2))
plot(fullfishmod_scaled, sub.caption = "", caption = "", family ="A")
par(op)


#coefficient plot
library(dotwhisker)
library(broom)

ov <- names(sort(abs((coef(fullfishmod_scaled))),decreasing=TRUE))
ov

#Scaled Full Model Coefficient plot
dwplot(fullfishmod_scaled) %>%
  relabel_predictors(c(Depth = "Depth",
                       NH4 = "NH4",
                       DO_B = "Dissolved oxygen",
                       Sal_B = "Salinity",
                       ChlA = "Chlorophyll-a",
                       Turb = "Turbidity",
                       Temp_B = "Temperature")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + theme(legend.position = "none") +
  geom_vline(xintercept=0,lty=2) + theme(text=element_text(family="A", size=12)) +
  scale_color_npg()

#Subset Model
#build model without DO and temp to assess effect size of these two
#scaled
subfishmod_scaled <- lm(logBiomass ~ Depth + NH4 + ChlA + Sal_B + Turb, all_fish_scale,
                        na.action=na.exclude)

#Scaled Full and Subset Model Coefficient Plot
dwplot(list(fullfishmod_scaled,subfishmod_scaled)) %>%
  relabel_predictors(c(Depth = "Depth",
                       NH4 = "NH4",
                       DO_B = "Dissolved oxygen",
                       Sal_B = "Salinity",
                       ChlA = "Chlorophyll-a",
                       Turb = "Turbidity",
                       Temp_B = "Temperature")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + 
  geom_vline(xintercept=0,lty=2) + 
  theme(legend.background = element_rect(colour="white"),
        legend.title = element_blank()) + theme(text=element_text(family="A", size=12)) +
  scale_color_npg()

#Scaled Models ANOVA
anova(fullfishmod_scaled,subfishmod_scaled)

#SUMMARY...selected model output 
summary(fullfishmod_scaled)
summary(fullfishmod)
confint(fullfishmod, level = 0.95)


#THERMAL GUILD MODEL
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
fishthermmod_sep2 <- lm (logBiomass ~ -1 + ThermalGuild + (Depth + NH4 + ChlA + Sal_B +
                                                            Temp_B + DO_B + Turb):ThermalGuild, fish_therm_sub2)

op <- par(mar=c(4.5,4.5,2,2),mfrow=c(2,2), family = "A")
plot(fishthermmod_sep, sub.caption = "", caption = "")
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
    %>% mutate(across(term, ~reorder(factor(.), abs(estimate))))
)

#Guild Coefficient Plot
print(ggplot(tt1, aes(estimate, term))
    + geom_pointrange(aes(xmin=conf.low, xmax=conf.high,
                          colour=thermal_guild),
                      position=position_dodge(width=0.25))
    + geom_vline(xintercept=0, lty=2) +
      theme_bw() + xlab("Coefficient estimate") + ylab("") + labs(color='Thermal Guild') +
      theme(text=element_text(family="A", size=12)) + scale_color_npg()
    )

#SUMMARY..selected model output
summary(fishthermmod_sep)
summary(fishthermmod_sep2)
confint(fishthermmod_sep2, level = 0.95)


##SPECIES RICHNESS MODEL
#NOT Scaled Full Model
SpRich_mod1 <- glm(SpeciesRichness ~ Depth + NH4 + ChlA + Sal_B +
                     DO_B + Temp_B + Turb, all_fish,family=poisson)

op <- par(mar=c(4.5,4.5,2,2),mfrow=c(2,2), family = "A")
plot(SpRich_mod1, sub.caption = "", caption = "")
par(op)

#boxplots
par(family = "A", cex = 1.2, mfrow=c(1,3), las = 2)
boxplot(residuals(SpRich_mod1) ~ all_fish$Area, col = c("#E64B35FF", "#4DBBD5FF"),
        ylab = "Residuals(Species Richness Model)", xlab = "") #area does not seem to affect the residuals disproportionately 
boxplot(residuals(SpRich_mod1) ~ all_fish$Month, col = "#91D1C2FF",
        ylab = "", xlab = "")#March different
summary(all_fish$Month == "March")# only one obs 
summary(all_fish$Month == "May")
##since study area is Florida, seasons are not as pronounced as they are in north
boxplot(residuals(SpRich_mod1) ~ all_fish$Year, col = "#91D1C2FF",
        ylab = "", xlab = "") #no specific Year stands out

#Scaled Full Model
Rich_fish_scale <- all_fish %>% mutate(across(where(is.numeric) & !SpeciesRichness, ~drop(scale(.))))

SpRich_mod_scale <- glm(SpeciesRichness ~ Depth + NH4 + ChlA + Sal_B +
                     DO_B + Temp_B + Turb, Rich_fish_scale,family=poisson)

op <- par(mar=c(4.5,4.5,2,2),mfrow=c(2,2), family = "A")
plot(SpRich_mod_scale, sub.caption = "", caption = "")
par(op)

#Scaled Subset Model
SpRich_mod2_scale <- glm(SpeciesRichness ~ Depth + NH4 + ChlA + Sal_B +
                     Turb, Rich_fish_scale,family=poisson)


#Scaled Full and Subset Model Coefficient plot
ov3 <- names(sort(abs(coef(SpRich_mod_scale)),decreasing=TRUE))
ov3

dwplot(list(SpRich_mod_scale,SpRich_mod2_scale)) %>%
  relabel_predictors(c(Sal_B = "Salinity",
                       Depth = "Depth",
                       DO_B = "Dissolved oxygen",
                       NH4 = "NH4",
                       Temp_B = "Temperature",
                       Turb = "Turbidity",
                       ChlA = "Chlorophyll-a")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + 
  geom_vline(xintercept=0,lty=2) + 
  theme(legend.background = element_rect(colour="white"),
        legend.title = element_blank()) + scale_color_npg() +
  theme(text=element_text(family="A", size=12))

dwplot(SpRich_mod_scale) %>%
  relabel_predictors(c(Sal_B = "Salinity",
                       Depth = "Depth",
                       DO_B = "Dissolved oxygen",
                       NH4 = "NH4",
                       Temp_B = "Temperature",
                       Turb = "Turbidity",
                       ChlA = "Chlorophyll-a")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + 
  geom_vline(xintercept=0,lty=2) + 
  theme(legend.position = "none") + scale_color_npg() +
  theme(text=element_text(family="A", size=12))

#Scaled Full and Sub ANOVA
anova(SpRich_mod_scale,SpRich_mod2_scale)

#SUMMARY...selected model output
summary(SpRich_mod_scale)
summary(SpRich_mod1)
confint(SpRich_mod1, level = 0.95)
with(summary(SpRich_mod1), 1 - deviance/null.deviance) #McFadden's pseudo r-squared
