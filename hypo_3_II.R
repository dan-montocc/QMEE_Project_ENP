###Hypothesis 3 analysis

#load-in libraries...
library(here)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(dotwhisker)
library(broom)
library(colorspace)

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

#FUNCTIONAL GROUP BIOMASS MODEL
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

#Scaled Full Functional Group Model
## scale all numeric variables except the response
## (REALLY need to scale vars if you're going to draw a coefficient plot for effects with different units!
Peri_fish_scale <- Peri_fish_dat_sub1 %>% mutate(across(where(is.numeric) & !logBiomass, ~drop(scale(.))))

#Separated by Group model
Peri_fish_sep <- lm (logBiomass ~ -1 + FunctionalGroup + (AvgWaterDepth + ChlA + Sal_B +
                                                            Avg.PlantCover + Avg.PeriphytonCover +
                                                            Temp_B + DO_B + Turb):FunctionalGroup, Peri_fish_scale)
#diagnostics
op <- par(mar=c(4.5,4.5,2,2),mfrow=c(2,2))
plot(Peri_fish_sep)
par(op)

#NOT Scaled Functional Group Full Model (simplified interpretation)
Peri_fish_full <- lm (logBiomass ~ (AvgWaterDepth + ChlA + Sal_B + Avg.PlantCover + Avg.PeriphytonCover +
                                      Temp_B + DO_B + Turb)*FunctionalGroup, Peri_fish_dat_sub1)
#diagnostics
op <- par(mar=c(4.5,4.5,2,2),mfrow=c(2,2))
plot(Peri_fish_sep)
par(op)

#Scaled Subset Model - no peri or plant cover
#Separated by Group model
Peri_fish_sep_sub <- lm (logBiomass ~ -1 + FunctionalGroup + (AvgWaterDepth + ChlA + Sal_B +
                                                            Temp_B + DO_B + Turb):FunctionalGroup, Peri_fish_scale)

#NOT Scaled Functional Group Subset Model (simplified interpretation)
Peri_fish_full_sub <- lm (logBiomass ~ (AvgWaterDepth + ChlA + Sal_B +
                                      Temp_B + DO_B + Turb)*FunctionalGroup, Peri_fish_dat_sub1)

#ANOVA of NOT Scaled Models (Full and Sub)
anova(Peri_fish_full,Peri_fish_full_sub)

#ANOVA of Scaled Models (Full and Sub)
anova(Peri_fish_sep,Peri_fish_sep_sub)


#Scaled Full Model Coefficient Plot
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

print(ggplot(tt, aes(estimate, term))
      + geom_pointrange(aes(xmin=conf.low, xmax=conf.high,
                            colour=Functional_Group),
                        position=position_dodge(width=0.5))
      + geom_vline(xintercept=0, lty=2)
      + scale_color_discrete_qualitative() +
        theme_bw() + xlab("Coefficient estimate") + ylab("") + labs(color='Functional Group') 
)

#Scaled Full AND Subset Coefficient Plot
tt <- (broom::tidy(Peri_fish_sep, conf.int=TRUE)
       %>% mutate(across(term,~ifelse(grepl("ivore$",.),paste(.,"(Intercept)",sep=":"), .)))
       %>% separate(term,into=c("Functional_Group","term"),sep=":")
       %>% mutate(across(Functional_Group,~str_remove(.,"FunctionalGroup")))
       %>% mutate(across(term, rename_term))
       %>% filter(term!="(Intercept)")  
       %>% mutate(across(term, ~reorder(factor(.), estimate)))
)

tt$model <- "Model 1"

tt2 <- (broom::tidy(Peri_fish_sep_sub, conf.int=TRUE)
        %>% mutate(across(term,~ifelse(grepl("ivore$",.),paste(.,"(Intercept)",sep=":"), .)))
        %>% separate(term,into=c("Functional_Group","term"),sep=":")
        %>% mutate(across(Functional_Group,~str_remove(.,"FunctionalGroup")))
        %>% mutate(across(term, rename_term))
        %>% filter(term!="(Intercept)")  
        %>% mutate(across(term, ~reorder(factor(.), estimate)))
)

tt2$model <- "Model 2"

mod_combine <- rbind(tt,tt2)

gg0 <- (ggplot(mod_combine, aes(estimate, term,group=model))
        + geom_pointrange(aes(xmin=conf.low, xmax=conf.high,colour=factor(model)))
        + geom_vline(xintercept=0, lty=2) +
          theme_bw() + xlab("Coefficient estimate") + ylab("") +
          theme(legend.title = element_blank())
)

#SUMMARY...selected model output


##ALL Fish Model

#load-in and cleaning...
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

#Scaled Full Model
Fish_Peri_dat_scale <- Peri_all_dat_sub1 %>% mutate(across(where(is.numeric) & !logBiomass & !SpeciesRichness, ~drop(scale(.))))

Peri_allfishfullmod_scale <- lm(logBiomass ~ Temp_B + DO_B + Sal_B + ChlA +
                         Turb + AvgWaterDepth + Avg.PeriphytonCover +
                         + Avg.PlantCover,Fish_Peri_dat_scale)
#diagnostics
op <- par(mar=c(4.5,4.5,2,2),mfrow=c(2,2))
plot(Peri_allfishfullmod_scale, sub.caption = "")

#NOT Scaled Full Model
Peri_allfishfullmod <- lm(logBiomass ~ Temp_B + DO_B + Sal_B + ChlA +
                            Turb + AvgWaterDepth + Avg.PeriphytonCover +
                            + Avg.PlantCover,Peri_all_dat_sub1)
#diagnostics
plot(Peri_allfishfullmod, sub.caption = "")
par(op)

#Scaled Subset Model - missing plant and peri cover
Peri_allfishfullmod_scale_sub <- lm(logBiomass ~ Temp_B + DO_B + Sal_B + ChlA +
                                  Turb + AvgWaterDepth,Fish_Peri_dat_scale)

#NOT Scaled Subset Model
Peri_allfishfullmod_sub <- lm(logBiomass ~ Temp_B + DO_B + Sal_B + ChlA +
                            Turb + AvgWaterDepth,Peri_all_dat_sub1)

#ANOVA of NOT Scaled Models (Full and Sub)
anova(Peri_allfishfullmod,Peri_allfishfullmod_sub)

#ANOVA of Scaled Models (Full and Sub)
anova(Peri_allfishfullmod_scale,Peri_allfishfullmod_scale_sub)


#Scaled Full Model Coefficient Plot
tt <- (broom::tidy(Peri_allfishfullmod_scale, conf.int=TRUE)
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

print(ggplot(tt, aes(estimate, term))
      + geom_pointrange(aes(xmin=conf.low, xmax=conf.high,
                            colour=Functional_Group),
                        position=position_dodge(width=0.5))
      + geom_vline(xintercept=0, lty=2)
      + scale_color_discrete_qualitative() +
        theme_bw() + xlab("Coefficient estimate") + ylab("") + labs(color='Functional Group') 
)

#Scaled Full AND Subset Coefficient Plot
tt <- (broom::tidy(Peri_allfishfullmod_scale, conf.int=TRUE)
       %>% mutate(across(term,~ifelse(grepl("ivore$",.),paste(.,"(Intercept)",sep=":"), .)))
       %>% separate(term,into=c("Functional_Group","term"),sep=":")
       %>% mutate(across(Functional_Group,~str_remove(.,"FunctionalGroup")))
       %>% mutate(across(term, rename_term))
       %>% filter(term!="(Intercept)")  
       %>% mutate(across(term, ~reorder(factor(.), estimate)))
)

tt$model <- "Model 1"

tt2 <- (broom::tidy(Peri_allfishfullmod_scale_sub, conf.int=TRUE)
        %>% mutate(across(term,~ifelse(grepl("ivore$",.),paste(.,"(Intercept)",sep=":"), .)))
        %>% separate(term,into=c("Functional_Group","term"),sep=":")
        %>% mutate(across(Functional_Group,~str_remove(.,"FunctionalGroup")))
        %>% mutate(across(term, rename_term))
        %>% filter(term!="(Intercept)")  
        %>% mutate(across(term, ~reorder(factor(.), estimate)))
)

tt2$model <- "Model 2"

mod_combine <- rbind(tt,tt2)

gg0 <- (ggplot(mod_combine, aes(estimate, term,group=model))
        + geom_pointrange(aes(xmin=conf.low, xmax=conf.high,colour=factor(model)))
        + geom_vline(xintercept=0, lty=2) +
          theme_bw() + xlab("Coefficient estimate") + ylab("") +
          theme(legend.title = element_blank())
)

#SUMMARY...selected model output


#SPECIES RICHNESS MODEL

#NOT Scaled Full Model
SpRich_mod1 <- glm(SpeciesRichness ~ Temp_B + DO_B + Sal_B + ChlA +
                     Turb + AvgWaterDepth + Avg.PeriphytonCover +
                     + Avg.PlantCover,Peri_all_dat_sub1,family=poisson)
#diagnostics
op <- par(mar=c(4.5,4.5,2,2),mfrow=c(2,2))
plot(SpRich_mod1, sub.caption = "")

#Scaled Full Model
SpRich_mod_scale <- glm(SpeciesRichness ~ Temp_B + DO_B + Sal_B + ChlA +
                          Turb + AvgWaterDepth + Avg.PeriphytonCover +
                          + Avg.PlantCover, Fish_Peri_dat_scale,family=poisson)
#diagnostics
plot(SpRich_mod_scale, sub.caption = "")
par(op)

#NOT Scaled Subset Model
SpRich_mod2 <- glm(SpeciesRichness ~ Temp_B + DO_B + Sal_B + ChlA +
                     Turb + AvgWaterDepth,Peri_all_dat_sub1,family=poisson)

#Scaled Subset Model
SpRich_mod2_scale <- glm(SpeciesRichness ~ Temp_B + DO_B + Sal_B + ChlA +
                           Turb + AvgWaterDepth, Fish_Peri_dat_scale,family=poisson)

#NOT Scaled Full and Subset Model Coefficient plot
ov3 <- names(sort(abs(coef(SpRich_mod1)),decreasing=TRUE))
ov3

dwplot(list(SpRich_mod1,SpRich_mod2)) %>%
  relabel_predictors(c(DO_B = "Dissolved oxygen",
                       ChlA = "Chlorophyll-a",
                       Sal_B = "Salinity",
                       Avg.PlantCover = "% Plant Cover",
                       Turb = "Turbidity",
                       Avg.PeriphytonCover = "% Periphyton Cover",
                       Temp_B = "Temperature",
                       AvgWaterDepth = "Water depth")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + 
  geom_vline(xintercept=0,lty=2) + 
  theme(legend.background = element_rect(colour="white"),
        legend.title = element_blank()) 

#NOT Scaled Full MOdel Coefficient Plot
dwplot(SpRich_mod1) %>%
  relabel_predictors(c(DO_B = "Dissolved oxygen",
                       ChlA = "Chlorophyll-a",
                       Sal_B = "Salinity",
                       Avg.PlantCover = "% Plant Cover",
                       Turb = "Turbidity",
                       Avg.PeriphytonCover = "% Periphyton Cover",
                       Temp_B = "Temperature",
                       AvgWaterDepth = "Water depth")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + 
  geom_vline(xintercept=0,lty=2) + 
  theme(legend.position = "none") 

#Scaled Full and Subset Model Coefficient plot
ov3 <- names(sort(abs(coef(SpRich_mod_scale)),decreasing=TRUE))
ov3

dwplot(list(SpRich_mod_scale,SpRich_mod2_scale)) %>%
  relabel_predictors(c(Sal_B = "Salinity",
                       Avg.PeriphytonCover = "% Periphyton Cover",
                       Avg.PlantCover = "% Plant Cover",
                       ChlA = "Chlorophyll-a",
                       AvgWaterDepth = "Water depth",
                       DO_B = "Dissolved oxygen",
                       Turb = "Turbidity",
                       Temp_B = "Temperature")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + 
  geom_vline(xintercept=0,lty=2) + 
  theme(legend.background = element_rect(colour="white"),
        legend.title = element_blank()) 

#Scaled Full Coefficient plot
dwplot(SpRich_mod1) %>%
  relabel_predictors(c(Sal_B = "Salinity",
                       Avg.PeriphytonCover = "% Periphyton Cover",
                       Avg.PlantCover = "% Plant Cover",
                       ChlA = "Chlorophyll-a",
                       AvgWaterDepth = "Water depth",
                       DO_B = "Dissolved oxygen",
                       Turb = "Turbidity",
                       Temp_B = "Temperature")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + 
  geom_vline(xintercept=0,lty=2) + 
  theme(legend.position = "none") 

#NOT Scaled Full and Sub ANOVA
anova(SpRich_mod1,SpRich_mod2)

#Scaled Full and Sub ANOVA
anova(SpRich_mod_scale,SpRich_mod2_scale)

#SUMMARY...selected model output
