##fish functional group variable add
funcgrps <- read.csv("fish_functional_groups.csv", header = TRUE)
fishdat96 <- read.csv("ENP_FishData_1996to2000.csv", header = TRUE)

merge1996to2000 <- merge(funcgrps, fishdat96)
write.csv(merge1996to2000, file = "ENP_FishFunctionalGrp_1996to2000.csv")

fishdat2000 <- read.csv("ENP_FishData_2000to2005.csv", header = TRUE)

merge2000to2005 <- merge(funcgrps,fishdat2000)
write.csv(merge2000to2005, file = "ENP_FishFunctionalGrp_2000to2005.csv")

##water quality site name to area join
enp_wq <- read.csv("ENP_WQ_1996to2005.csv", header = TRUE)
wq_area_name <- read.csv("WQ_Parameter_SiteJoin.csv", header = TRUE)

wq_merge <- merge(enp_wq, wq_area_name,all=TRUE)
which(is.na(wq_merge$Area))

##loss of observations...why?
library("arsenal")
summary(comparedf(enp_wq, wq_merge))
list(as.factor(enp_wq$SITE))
class(enp_wq$SITE)
enp_wq$SITE <- as.factor(enp_wq$SITE)
levels(enp_wq$SITE)
##Ponce de Leon Bay MISSPELLED!
#fixed

write.csv(wq_merge, file = "ENP_WQ_1996to2000_AreaAdd.csv")

##appending 2000 to 2005 fish to 1996 file
fishdat96 <- read.csv("ENP_FishData_1996to2000.csv", header = TRUE)
fishdat2000 <- read.csv("ENP_FishData_2000to2005.csv", header = TRUE)

dim(fishdat96)
dim(fishdat2000)

fishdat96_05 <- rbind(fishdat96,fishdat2000)
3077+4045
dim(fishdat96_05)
write.csv(fishdat96_05, file = "ENP_FishFunctionalGrp_1996to2005.csv")

#join WQ to fish
fishdat96_05 <- read.csv("ENP_FishFunctionalGrp_1996to2005.csv")

##aggregate to month level for year, site, and fish species
library("data.table")
keys <- colnames(fishdat96_05)[!grep1()]
wq_fish <- merge(fishdat96_05,wq_merge, all.y=TRUE)
which(is.na(wq_fish$Plot))
