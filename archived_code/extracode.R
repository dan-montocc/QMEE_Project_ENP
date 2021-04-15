#data experimentation/junk code
thermmod1 <- lm(logBiomass ~ DO_B + Temp_B + Sal_B, fish_therm_sub1)
boxplot(residuals(thermmod1) ~ fish_therm_sub1$ThermalGuild) #cold appears to have an effect; use thermal guild as factor
summary(fish_therm_sub1$ThermalGuild == "cold") #five obs

#build thermal guild model with biomass
biomass <- lm(logBiomass ~ (Temp_B + Temp_S + DO_S + DO_B)*Area, fish_therm)
plot(biomass)
biomass_simple <- lm(logBiomass ~ Temp_B + DO_B, fish_therm)
plot(biomass_simple)
ggplot(fish_therm,aes(y=logBiomass,x=Temp_B,color=Area))+geom_point()+stat_smooth(method="lm",se=FALSE)
boxplot(residuals(biomass_simple) ~ fish_therm$Area)
biomass_simple2 <- lm(logBiomass ~ Temp_S + DO_S, fish_therm)
plot(biomass_simple2)

#correlation matrix
res <- cor.test(fish_therm$Temp_B, fish_therm$DO_B, 
                method = "pearson")
res

biomassGuild <- lm(logBiomass ~ (DO_S + DO_B + Temp_S + Temp_B)*ThermalGuild, fish_therm)
plot(biomassGuild)

#leverage of 1 from 5 obs
#create subset and re-run
fish_thermsub <- fish_therm[-c(4,104,180,254,259),]

##plotting
library(ggiraph)
library(ggiraphExtra)
library(moonBook)
biomassGuild <- lm(logBiomass ~ (DO_S + DO_B + Temp_S + Temp_B)*ThermalGuild, fish_thermsub)
plot(biomassGuild)
biomassGuildBottom <- lm(logBiomass ~ (DO_B + Temp_B)*ThermalGuild, fish_thermsub)
plot(biomassGuildBottom)
biomassBottom <- lm(logBiomass ~ DO_B + Temp_B,fish_thermsub)
plot(biomassBottom)
ggplot(fish_therm,aes(y=logBiomass,x=DO_B,color=ThermalGuild))+geom_point()+stat_smooth(method="lm",se=FALSE)
ggplot(fish_therm,aes(y=logBiomass,x=Temp_B,color=ThermalGuild))+geom_point()+stat_smooth(method="lm",se=FALSE)

