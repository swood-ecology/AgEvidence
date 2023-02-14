#### LOAD LIBRARIES ####
library(tidyverse)
library(readxl)
library(plyr)
library(ggplot2)

# Data files, Excel Format ######
cc <- read_excel("data/ContinuousCover_Kenya_complete.xlsx", sheet = "Results")
nm <- read_excel("data/NutrientMgmt_Kenya_complete.xlsx", sheet = "Results")
till_results <- read_excel("data/Tillage_Kenya_complete.xlsx", sheet="Results")






###Convert Nutrient Management to CSV#####
nm_results <- read_excel("data/NutrientMgmt_Kenya_complete.xlsx", sheet="Results")
nm_ref <- read_excel("data/NutrientMgmt_Kenya_complete.xlsx", sheet="Reference")
nm_expd <- read_excel("data/NutrientMgmt_Kenya_complete.xlsx", sheet="ExpD_Location")
nm_cash <- read_excel("data/NutrientMgmt_Kenya_complete.xlsx", sheet="CashCrop")
nm_trtmt <- read_excel("data/NutrientMgmt_Kenya_complete.xlsx", sheet="Treatment")

write.csv(nm_results, paste0("data/NutrientMgmt/NutrientMgmt_Kenya_complete_Results.csv"), col.names=FALSE)
write.csv(nm_ref, paste0("data/NutrientMgmt/NutrientMgmt_Kenya_complete_Ref.csv"), col.names=FALSE)
write.csv(nm_expd, paste0("data/NutrientMgmt/NutrientMgmt_Kenya_complete_ExpD.csv"), col.names=FALSE)
write.csv(nm_cash, paste0("data/NutrientMgmt/NutrientMgmt_Kenya_complete_CashCrop.csv"), col.names=FALSE)
write.csv(nm_trtmt, paste0("data/NutrientMgmt/NutrientMgmt_Kenya_complete_Treatment.csv"), col.names=FALSE)


###Convert Tillage to CSV#####
till_results <- read_excel("data/Tillage_Kenya_complete.xlsx", sheet="Results")
till_ref <- read_excel("data/Tillage_Kenya_complete.xlsx", sheet="Reference")
till_expd <- read_excel("data/Tillage_Kenya_complete.xlsx", sheet="ExpD_Location")
till_cash <- read_excel("data/Tillage_Kenya_complete.xlsx", sheet="CashCrop")
till_trtmt <- read_excel("data/Tillage_Kenya_complete.xlsx", sheet="Treatment")

write.csv(till_results, paste0("data/Tillage/Tillage_Kenya_complete_Results.csv"), col.names=FALSE)
write.csv(till_ref, paste0("data/Tillage/Tillage_Kenya_complete_Ref.csv"), col.names=FALSE)
write.csv(till_expd, paste0("data/Tillage/Tillage_Kenya_complete_ExpD.csv"), col.names=FALSE)
write.csv(till_cash, paste0("data/Tillage/Tillage_Kenya_complete_CashCrop.csv"), col.names=FALSE)
write.csv(till_trtmt, paste0("data/Tillage/Tillage_Kenya_complete_Treatment.csv"), col.names=FALSE)

###Convert Continuous Cover to CSV#####

cc_results <- read_excel("data/ContinuousCover_Kenya_complete.xlsx", sheet="Results")
cc_ref <- read_excel("data/ContinuousCover_Kenya_complete.xlsx", sheet="Reference")
cc_expd <- read_excel("data/ContinuousCover_Kenya_complete.xlsx", sheet="ExpD_Location")
cc_cash <- read_excel("data/ContinuousCover_Kenya_complete.xlsx", sheet="CashCrop")
cc_trtmt <- read_excel("data/ContinuousCover_Kenya_complete.xlsx", sheet="Treatment")

write.csv(cc_results, paste0("data/ContinuousCover/ContinuousCover_Kenya_complete_Results.csv"), col.names = FALSE)
write.csv(cc_ref, paste0("data/ContinuousCover/ContinuousCover_Kenya_complete_Ref.csv"), col.names=FALSE)
write.csv(cc_expd, paste0("data/ContinuousCover/ContinuousCover_Kenya_complete_ExpD.csv"), col.names=FALSE)
write.csv(cc_cash, paste0("data/ContinuousCover/ContinuousCover_Kenya_complete_CashCrop.csv"), col.names=FALSE)
write.csv(cc_trtmt, paste0("data/ContinuousCover/ContinuousCover_Kenya_complete_Treatment.csv"), col.names=FALSE)

