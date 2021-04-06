funcgrps <- read.csv("fish_functional_groups.csv", header = TRUE)
fishdat96 <- read.csv("ENP_FishData_1996to2000.csv", header = TRUE)

merge1996to2000 <- merge(funcgrps, fishdat96)
write.csv(merge1996to2000, file = "ENP_FishFunctionalGrp_1996to2000.csv")

fishdat2000 <- read.csv("ENP_FishData_2000to2005.csv", header = TRUE)

merge2000to2005 <- merge(funcgrps,fishdat2000)
write.csv(merge2000to2005, file = "ENP_FishFunctionalGrp_2000to2005.csv")
