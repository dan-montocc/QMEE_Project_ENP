###Hypothesis 3 analysis

#load-in libraries...
library(here)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(dotwhisker)
library(broom)
library(colorspace)

#aesthetics load-in
library(extrafont)
windowsFonts(A = windowsFont("Times New Roman"))
library(ggsci)

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

names(Peri_fish_xsub)[names(Peri_fish_xsub) == "Temp_B"] <- "Temperature"
names(Peri_fish_xsub)[names(Peri_fish_xsub) == "DO_B"] <- "DO"
names(Peri_fish_xsub)[names(Peri_fish_xsub) == "Turb"] <- "Turbidity"
names(Peri_fish_xsub)[names(Peri_fish_xsub) == "ChlA"] <- "Chlorophyll-a"
names(Peri_fish_xsub)[names(Peri_fish_xsub) == "Sal_B"] <- "Salinity"
names(Peri_fish_xsub)[names(Peri_fish_xsub) == "AvgWaterDepth"] <- "Depth"
names(Peri_fish_xsub)[names(Peri_fish_xsub) == "Avg.PlantCover"] <- "Plant_Cover"
names(Peri_fish_xsub)[names(Peri_fish_xsub) == "Avg.PeriphytonCover"] <- "Peri_Cover"


par(family = "A")
rquery.cormat(Peri_fish_xsub)

#boxplots of Year, Month, and Area
Peri_fishfullmod <- lm(logBiomass ~ Temp_B + DO_B + Sal_B + ChlA +
                    Turb + AvgWaterDepth + Avg.PeriphytonCover +
                  + Avg.PlantCover,Peri_fish_dat_sub1)

par(family = "A", cex = 1.2, mfrow=c(1,2), las = 2, mar=c(6,4.5,2,2))
boxplot(residuals(Peri_fishfullmod) ~ Peri_fish_dat_sub1$Area, col = c("#E64B35FF", "#4DBBD5FF"),
        ylab = "Residuals(Functional Group Model)", xlab = "") #area does not seem to effect the residuals disproportionately 
boxplot(residuals(Peri_fishfullmod) ~ Peri_fish_dat_sub1$Month, col = "#91D1C2FF",
        ylab = "Residuals(Functional Group Model)", xlab = "")#pattern in month?
summary(Peri_fish_dat_sub1$Month == "March")
#observation in March is 1, ok to not account for month
boxplot(residuals(Peri_fishfullmod) ~ Peri_fish_dat_sub1$Year, col = "#91D1C2FF",
        ylab = "Residuals(Functional Group Model)", xlab = "")
 #Year does not have clear effect size differences
boxplot(residuals(Peri_fishfullmod) ~ Peri_fish_dat_sub1$FunctionalGroup, col = "#91D1C2FF",
        ylab = "Residuals(Functional Group Model)", xlab = "") #include as categorical
summary(Peri_fish_dat_sub1$FunctionalGroup)

#FUNCTIONAL GROUP BIOMASS MODEL
## this automates most of the name changes
rename_term <- function(x) {
  return(x
         %>% str_replace("DO_B","Dissolved oxygen")
         %>% str_replace("Turb","Turbidity")
         %>% str_replace("Temp_B","Temperature")
         %>% str_replace("Sal_B","Salinity")
         %>% str_replace("ChlA","Chlorophyll-a")
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
Peri_fish_sep_scale <- lm (logBiomass ~ -1 + FunctionalGroup + (AvgWaterDepth + ChlA + Sal_B +
                                                            Avg.PlantCover + Avg.PeriphytonCover +
                                                            Temp_B + DO_B + Turb):FunctionalGroup, Peri_fish_scale)
#diagnostics
op <- par(mar=c(4.5,4.5,2,2),mfrow=c(2,2))
plot(Peri_fish_sep_scale, caption = "", family ="A", sub.caption = "")
par(op)

#NOT Scaled Functional Group Full Model (simplified interpretation)
Peri_fish_full <- lm (logBiomass ~ (AvgWaterDepth + ChlA + Sal_B + Avg.PlantCover + Avg.PeriphytonCover +
                                      Temp_B + DO_B + Turb)*FunctionalGroup, Peri_fish_dat_sub1)

#NOT scaled full model (separated)
Peri_fish_sep <- lm (logBiomass ~ -1 + FunctionalGroup + (AvgWaterDepth + ChlA + Sal_B +
                                                                  Avg.PlantCover + Avg.PeriphytonCover +
                                                                  Temp_B + DO_B + Turb):FunctionalGroup, Peri_fish_dat_sub1)
#Scaled Full Model Coefficient Plot
tt <- (broom::tidy(Peri_fish_sep_scale, conf.int=TRUE)
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
    %>% mutate(across(term, ~reorder(factor(.), abs(estimate))))
)

print(ggplot(tt, aes(estimate, term))
      + geom_pointrange(aes(xmin=conf.low, xmax=conf.high,
                            colour=Functional_Group),
                        position=position_dodge(width=0.5))
      + geom_vline(xintercept=0, lty=2)
      + scale_color_npg() + theme_bw() + xlab("Coefficient estimate") +
        ylab("") + labs(color='Functional Group') +
        theme(text=element_text(family="A", size=12))
)

#SUMMARY...selected model output
summary(Peri_fish_sep_scale)
summary(Peri_fish_sep)


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
plot(Peri_allfishfullmod_scale, caption = "", family ="A", sub.caption = "")

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

#ANOVA of Scaled Models (Full and Sub)
anova(Peri_allfishfullmod_scale,Peri_allfishfullmod_scale_sub)


#Scaled Full Model Coefficient Plot
ov <- names(sort(abs((coef(Peri_allfishfullmod_scale))),decreasing=TRUE))
ov
dwplot(Peri_allfishfullmod_scale) %>%
  relabel_predictors(c(Sal_B = "Salinity",
                       AvgWaterDepth = "Water depth",
                       Avg.PlantCover = "% Plant Cover",
                       DO_B = "Dissolved oxygen",
                       Temp_B = "Temperature",
                       ChlA = "Chlorophyll-a",
                       Avg.PeriphytonCover = "% Periphyton Cover",
                       Turb = "Turbidity")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + 
  geom_vline(xintercept=0,lty=2) + 
  theme(legend.background = element_rect(colour="white"),
        legend.title = element_blank()) + theme(text=element_text(family="A", size=12)) +
  scale_color_npg() + theme(legend.position = "none") 

#Scaled Full AND Subset Coefficient Plot

dwplot(list(Peri_allfishfullmod_scale,Peri_allfishfullmod_scale_sub)) %>%
  relabel_predictors(c(Sal_B = "Salinity",
                       AvgWaterDepth = "Water depth",
                       Avg.PlantCover = "% Plant Cover",
                       DO_B = "Dissolved oxygen",
                       Temp_B = "Temperature",
                       ChlA = "Chlorophyll-a",
                       Avg.PeriphytonCover = "% Periphyton Cover",
                       Turb = "Turbidity")) +
  theme_bw() + xlab("Coefficient estimate") + ylab("") + 
  geom_vline(xintercept=0,lty=2) + 
  theme(legend.background = element_rect(colour="white"),
        legend.title = element_blank()) + theme(text=element_text(family="A", size=12)) +
  scale_color_npg()
#SUMMARY...selected model output
summary(Peri_allfishfullmod_scale)
summary(Peri_allfishfullmod)

#SPECIES RICHNESS MODEL

#NOT Scaled Full Model
SpRich_mod1 <- glm(SpeciesRichness ~ Temp_B + DO_B + Sal_B + ChlA +
                     Turb + AvgWaterDepth + Avg.PeriphytonCover +
                     + Avg.PlantCover,Peri_all_dat_sub1,family=poisson)
#diagnostics
op <- par(mar=c(4.5,4.5,2,2),mfrow=c(2,2), family = "A")
plot(SpRich_mod1, sub.caption = "", caption = "")

#Scaled Full Model
SpRich_mod_scale <- glm(SpeciesRichness ~ Temp_B + DO_B + Sal_B + ChlA +
                          Turb + AvgWaterDepth + Avg.PeriphytonCover
                          + Avg.PlantCover, Fish_Peri_dat_scale,family=poisson)
#diagnostics
plot(SpRich_mod_scale, sub.caption = "", caption = "")
par(op)

#Scaled Subset Model
SpRich_mod2_scale <- glm(SpeciesRichness ~ Temp_B + DO_B + Sal_B + ChlA +
                           Turb + AvgWaterDepth, Fish_Peri_dat_scale,family=poisson)


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
        legend.title = element_blank()) + theme(text=element_text(family="A", size=12)) +
  scale_color_npg()

#Scaled Full Coefficient plot
dwplot(SpRich_mod_scale) %>%
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
  theme(legend.position = "none") + theme(text=element_text(family="A", size=12)) +
  scale_color_npg()

#Scaled Full and Sub ANOVA
anova(SpRich_mod_scale,SpRich_mod2_scale)

#SUMMARY...selected model output
summary(SpRich_mod_scale)
summary(SpRich_mod1)
