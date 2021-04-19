#Hypothesis 2 analysis

library(here)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(dotwhisker)
library(broom)

#load in data
Peri_dat <- read.csv(here("Joined_Cleaned_Data/Hypothesis_2/ENP_Peri_WQ_1995to2005_Join.csv"))
Peri_dat <- Peri_dat[,-c(1)]
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

#evaluate collinearity of predictor variables with correlation matrix
#code referenced from STHDA.com
##http://www.sthda.com/english/wiki/correlation-matrix-an-r-function-to-do-all-you-need
source("http://www.sthda.com/upload/rquery_cormat.r")
Peri_ysub <- Peri_dat_sub1[,c("Avg.PeriphytonCover","AvgPeriphytonVolume")]
rquery.cormat(Peri_ysub)

Peri_xsub <- Peri_dat_sub1[,c("TEMP_B","DO_B","SAL_B", 
                              "NO3", "CHLA","TURB","TN","DIN","TP","SRP",
                              "TOC","AvgWaterDepth","Avg.PlantCover")]
rquery.cormat(Peri_xsub)

#boxplots of Year, Month, and Area
Perifullmod <- lm(Avg.PeriphytonCover ~ TEMP_B + DO_B + SAL_B + NO3 + CHLA +
                    TURB + TN + DIN + TP + SRP + TOC + AvgWaterDepth
                  + Avg.PlantCover,Peri_dat_sub1)

boxplot(residuals(Perifullmod) ~ Peri_dat_sub1$Area) #area does not seem to effect the residuals disproportionately 
boxplot(residuals(Perifullmod) ~ Peri_dat_sub1$Month)#pattern in month?
#observation in each month is low
boxplot(residuals(Perifullmod) ~ Peri_dat_sub1$Year)
summary(Peri_dat_sub1$Year) #add Year to model as random effect

library(lme4)
#full peri model with year as random intercept effect
Perifullmod <- lmer(Avg.PeriphytonCover ~ TEMP_B + DO_B + SAL_B + NO3 + CHLA +
                    TURB + TN + DIN + TP + SRP + TOC + AvgWaterDepth
                  + Avg.PlantCover + (1|Year),data = Peri_dat_sub1)

#scaling x's to be standardized
Peri_dat_scale <- Peri_dat_sub1 %>% mutate(across(where(is.numeric) & !Avg.PeriphytonCover, ~drop(scale(.))))

#try again to see if warning is resolved
Perifullmod <- lmer(Avg.PeriphytonCover ~ TEMP_B + DO_B + SAL_B + NO3 + CHLA +
                      TURB + TN + DIN + TP + SRP + TOC + AvgWaterDepth
                    + Avg.PlantCover + (1|Year),data = Peri_dat_scale)

##diagnostics of model
#code from mixed models lecture notes
#BB coding
library(lattice)
library(car)

plot(Perifullmod)  ## fitted vs residual
## scale-location
plot(Perifullmod, sqrt(abs(resid(.))) ~ fitted(.),
     type=c("p","smooth"), col.line="red")
## Q-Q plots
qqmath(Perifullmod)

##leverage plot
#code from rdrr.io
##https://rdrr.io/cran/car/man/infIndexPlot.html

infIndexPlot(Perifullmod, vars=c("Cook"),
             id=TRUE, grid=FALSE, main="")

#obs 87 and 94 may be outliers, remove and retry
Peri_dat_scale2 <- Peri_dat_scale[-c(87,94),]

Perifullmod <- lmer(Avg.PeriphytonCover ~ TEMP_B + DO_B + SAL_B + NO3 + CHLA +
                      TURB + TN + DIN + TP + SRP + TOC + AvgWaterDepth
                    + Avg.PlantCover + (1|Year),data = Peri_dat_scale2)

infIndexPlot(Perifullmod, vars=c("Cook"),
             id=TRUE, grid=FALSE, main="")
#shifts to new obs, leave old obs in, leverage not that high
Perifullmod <- lmer(Avg.PeriphytonCover ~ TEMP_B + DO_B + SAL_B + NO3 + CHLA +
                      TURB + TN + DIN + TP + SRP + TOC + AvgWaterDepth
                    + Avg.PlantCover + (1|Year),data = Peri_dat_scale)

summary(Perifullmod)

Perisubmod <- lmer(Avg.PeriphytonCover ~ TEMP_B + SAL_B + NO3 + CHLA +
                      TURB + TN + DIN + TP + SRP + TOC + AvgWaterDepth
                   + (1|Year),data = Peri_dat_scale)

library(lmerTest)

anova::lmerTest(Perifullmod,Perisubmod)
#plotting coefficients
library(dotwhisker)
library(broom.mixed)
rename_term <- function(x) {
  return(x
         %>% str_replace("DO_B","Dissolved oxygen")
         %>% str_replace("TURB","Turbidity")
         %>% str_replace("SAL_B","Salinity")
         %>% str_replace("CHLA","Chlorophyll-A")
         %>% str_replace("AvgWaterDepth","Water depth")
         %>% str_replace("Avg.PlantCover","% Plant Cover")
         %>% str_replace("TEMP_B","Temperature")
         )
}
tt <- (broom::tidy(Perifullmod, conf.int=TRUE)
       %>% mutate(across(term, rename_term))
       %>% filter(term!="(Intercept)") 
       %>% filter(term!="sd__(Intercept)")
       %>% filter(term!="sd__Observation")
         
)

tt$model <- "Model 1"

tt2 <- (broom::tidy(Perisubmod, conf.int=TRUE)
        %>% mutate(across(term, rename_term))
        %>% filter(term!="(Intercept)")
        %>% filter(term!="(Intercept)") 
        %>% filter(term!="sd__(Intercept)")
        %>% filter(term!="sd__Observation")
)


tt2$model <- "Model 2"

mod_combine <- rbind(tt,tt2)

gg0 <- (ggplot(mod_combine, aes(estimate, term,group=model))
        + geom_pointrange(aes(xmin=conf.low, xmax=conf.high,colour=factor(model)))
        + geom_vline(xintercept=0, lty=2) +
          theme_bw() + xlab("Coefficient estimate") + ylab("") +
          theme(legend.title = element_blank())
)

print(gg0)

gg0 <- (ggplot(tt, aes(estimate, term,group=model))
        + geom_pointrange(aes(xmin=conf.low, xmax=conf.high,colour="indianred"))
        + geom_vline(xintercept=0, lty=2) +
          theme_bw() + xlab("Coefficient estimate") + ylab("") +
          theme(legend.position = "none")
)

print(gg0)
