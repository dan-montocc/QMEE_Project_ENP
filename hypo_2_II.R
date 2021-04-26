###Hypothesis 2 analysis

#load-in libraries...
library(here)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(dotwhisker)
library(broom)

#aesthetics load-in
library(extrafont)
windowsFonts(A = windowsFont("Times New Roman"))
library(ggsci)


#load in data
Peri_dat_sub1 <- readRDS("Joined_Cleaned_Data/Hypothesis_2/Peri_WQ_AggregatedData.rds")

#evaluate collinearity of predictor variables with correlation matrix
#code referenced from STHDA.com
##http://www.sthda.com/english/wiki/correlation-matrix-an-r-function-to-do-all-you-need
source("http://www.sthda.com/upload/rquery_cormat.r")

par(family = "A")

Peri_xsub <- Peri_dat_sub1[,c("TEMP_B","DO_B","SAL_B", 
                              "NO3", "CHLA","TURB","TN","DIN","TP","SRP",
                              "TOC","AvgWaterDepth","Avg.PlantCover")]
names(Peri_xsub)[names(Peri_xsub) == "TEMP_B"] <- "Temperature"
names(Peri_xsub)[names(Peri_xsub) == "DO_B"] <- "DO"
names(Peri_xsub)[names(Peri_xsub) == "TURB"] <- "Turbidity"
names(Peri_xsub)[names(Peri_xsub) == "CHLA"] <- "Chlorophyll-a"
names(Peri_xsub)[names(Peri_xsub) == "SAL_B"] <- "Salinity"
names(Peri_xsub)[names(Peri_xsub) == "AvgWaterDepth"] <- "Depth"
names(Peri_xsub)[names(Peri_xsub) == "Avg.PlantCover"] <- "Plant_Cover"
rquery.cormat(Peri_xsub)


#PERIPHYTON MODEL
#NOT scaled Full Model

#boxplots of Year, Month, and Area
Perifullmod <- lm(Avg.PeriphytonCover ~ TEMP_B + DO_B + SAL_B + NO3 + CHLA +
                    TURB + TN + DIN + TP + SRP + TOC + AvgWaterDepth
                  + Avg.PlantCover,Peri_dat_sub1)

par(family = "A", cex = 1.2, mfrow=c(1,3), las = 2, mar=c(6,4.5,2,2))
boxplot(residuals(Perifullmod) ~ Peri_dat_sub1$Area, col = c("#E64B35FF", "#4DBBD5FF"),
        ylab = "Residuals(Periphyton Model)", xlab = "") #area does not seem to effect the residuals disproportionately 
boxplot(residuals(Perifullmod) ~ Peri_dat_sub1$Month, col = "#91D1C2FF",
        ylab = "", xlab = "")#pattern in month?
#observation in each month is low
boxplot(residuals(Perifullmod) ~ Peri_dat_sub1$Year, col = "#91D1C2FF",
        ylab = "", xlab = "")
summary(Peri_dat_sub1$Year) #add Year to model as random effect

#NOT Scaled Full Model with Random Effects
library(lme4)
#full peri model with year as random intercept effect
Perifullmod <- lmer(Avg.PeriphytonCover ~ TEMP_B + DO_B + SAL_B + NO3 + CHLA +
                    TURB + TN + DIN + TP + SRP + TOC + AvgWaterDepth
                  + Avg.PlantCover + (1|Year),data = Peri_dat_sub1)

#scaling x's to be standardized
Peri_dat_scale <- Peri_dat_sub1 %>% mutate(across(where(is.numeric) & !Avg.PeriphytonCover, ~drop(scale(.))))

#Scaled Full Model
#try again to see if warning is resolved
Perifullmod_scaled <- lmer(Avg.PeriphytonCover ~ TEMP_B + DO_B + SAL_B + NO3 + CHLA +
                      TURB + TN + DIN + TP + SRP + TOC + AvgWaterDepth
                    + Avg.PlantCover + (1|Year),data = Peri_dat_scale)

##diagnostics of model
#code from mixed models lecture notes
#BB coding
library(lattice)
library(car)
library(ggpubr)

#create dataset ggplot can read
#pieced together from multiple sources
#https://rpubs.com/therimalaya/43190 (especially helpful)
PerifullmodF <- fortify.merMod(Perifullmod_scaled)

#residuals plot
residp <- ggplot(PerifullmodF, aes(.fitted,.resid)) + geom_point(colour="black", shape = 1) +
  geom_hline(yintercept=0, col="red") + theme_bw() +
  theme(text=element_text(family="A", size=12)) + xlab("Predicted values") +
  ylab("Residuals") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#scale location plot
scalep <- ggplot(PerifullmodF, aes(.fitted,sqrt(abs(.scresid)))) + geom_point(colour="black", shape = 1) +
  geom_smooth(method="loess", col= "red", se = FALSE) + theme_bw() +
  theme(text=element_text(family="A", size=12)) + xlab("Fitted values") +
  ylab(bquote(sqrt("|Standardized residuals|"))) + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#QQ
qqp <- ggplot(PerifullmodF, aes(sample = .scresid)) + stat_qq(shape = 1) + stat_qq_line(linetype = 2, col="black") +
  theme_bw() + theme(text=element_text(family="A", size=12)) + xlab("Theoretical quantiles") +
  ylab("Standardized residuals") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

##leverage plot
#code adapted from https://stackoverflow.com/questions/48962406/add-cooks-distance-levels-to-ggplot2
PerifullmodF$.cooksd <- cooks.distance(Perifullmod_scaled)
PerifullmodF$.hat <- hatvalues(Perifullmod_scaled)
cd_cont_pos <- function(leverage, level, model) {sqrt(level*length(coef(model))*(1-leverage)/leverage)}
cd_cont_neg <- function(leverage, level, model) {-cd_cont_pos(leverage, level, model)}
leverp <- ggplot(PerifullmodF, aes(.hat, .scresid)) + geom_point(shape = 1, na.rm=TRUE) +
  stat_smooth(method="loess", na.rm=TRUE, se = FALSE, col = "red") +
  theme_bw() + theme(text=element_text(family="A", size=12)) + xlab("Leverage") +
  ylab("Standardized residuals") + geom_hline(yintercept = 0, linetype = 2) +
  stat_function(fun = cd_cont_pos, args = list(level = 0.5, model = Perifullmod), xlim = c(min(PerifullmodF$.hat),max(PerifullmodF$.hat)), lty = 2, colour = "red") +
  stat_function(fun = cd_cont_neg, args = list(level = 0.5, model = Perifullmod), xlim = c(min(PerifullmodF$.hat),max(PerifullmodF$.hat)), lty = 2, colour = "red") +
  stat_function(fun = cd_cont_pos, args = list(level = 1, model = Perifullmod), xlim = c(min(PerifullmodF$.hat),max(PerifullmodF$.hat)), lty = 2, colour = "red") +
  stat_function(fun = cd_cont_neg, args = list(level = 1, model = Perifullmod), xlim = c(min(PerifullmodF$.hat),max(PerifullmodF$.hat)), lty = 2, colour = "red") +
  scale_y_continuous(limits = c(-2.5, 2.5)) + xlim(min(PerifullmodF$.hat),max(PerifullmodF$.hat)) + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggarrange(residp, scalep, qqp, leverp, ncol = 2, nrow = 2)

#Scaled Subset Model - Plant cover, DO and SRP removed
Perisubmod <- lmer(Avg.PeriphytonCover ~ TEMP_B + SAL_B + NO3 + CHLA +
                      TURB + TN + DIN + TOC + AvgWaterDepth
                   + (1|Year),data = Peri_dat_scale)

#ANOVA of Scaled Full and Subset Models
anova(Perifullmod_scaled,Perisubmod)

#Scaled Coefficient Plot (Full and Subset)

library(dotwhisker)
library(broom.mixed)

rename_term <- function(x) {
  return(x
         %>% str_replace("DO_B","Dissolved oxygen")
         %>% str_replace("TURB","Turbidity")
         %>% str_replace("SAL_B","Salinity")
         %>% str_replace("CHLA","Chlorophyll-a")
         %>% str_replace("AvgWaterDepth","Depth")
         %>% str_replace("Avg.PlantCover","% Plant Cover")
         %>% str_replace("TEMP_B","Temperature")
         )
}

tt <- (broom::tidy(Perifullmod_scaled, conf.int=TRUE)
        %>% mutate(across(term, rename_term))
        %>% filter(term!="(Intercept)")
        %>% filter(term!="sd__(Intercept)")
        %>% filter(term!="sd__Observation")
        %>% mutate(across(term, ~reorder(factor(.), abs(estimate))))
)


tt$model <- "Model 1"

tt2 <- (broom::tidy(Perisubmod, conf.int=TRUE)
        %>% mutate(across(term, rename_term))
        %>% filter(term!="(Intercept)")
        %>% filter(term!="sd__(Intercept)")
        %>% filter(term!="sd__Observation")
        %>% mutate(across(term, ~reorder(factor(.), abs(estimate))))
)


tt2$model <- "Model 2"

mod_combine <- rbind(tt,tt2)

gg0 <- (ggplot(mod_combine, aes(estimate, term,group=model))
        + geom_pointrange(aes(xmin=conf.low, xmax=conf.high,colour=factor(model)), position=position_dodge(width=0.5))
        + geom_vline(xintercept=0, lty=2) +
          theme_bw() + xlab("Coefficient estimate") + ylab("") +
          theme(legend.title = element_blank()) +
          theme(text=element_text(family="A", size=12)) +
          scale_color_npg()
)

print(gg0)

#Scaled Coefficient Plot (Full Model ONLY)
gg0 <- (ggplot(tt, aes(estimate, term))
        + geom_pointrange(aes(xmin=conf.low, xmax=conf.high,colour="indianred"))
        + geom_vline(xintercept=0, lty=2) +
          theme_bw() + xlab("Coefficient estimate") + ylab("") +
          theme(legend.position = "none") +
          theme(text=element_text(family="A", size=12)) +
          scale_color_npg()
)

print(gg0)

#SUMMARY...selected model output
library(nlme)
Peri_lmemod <- lme(Avg.PeriphytonCover ~ TEMP_B + SAL_B + NO3 + CHLA +
                     TURB + TN + DIN + TP + SRP + TOC + AvgWaterDepth + Avg.PlantCover + DO_B,
                   random = ~1|Year,data = Peri_dat_scale)
anova(Peri_lmemod)
summary(Peri_lmemod)
summary(Perifullmod_scaled)
confint.merMod(Perifullmod_scaled)
