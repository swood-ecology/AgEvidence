# Update the nutrient management trt1_names
library(openxlsx)
library(xlsx)
library(dplyr)
library(readxl)



nm_k <- read_excel("data/NutrientMgmt_Kenya_complete.xlsx", sheet = "Results")
cc_k <- read_excel("data/ContinuousCover_Kenya_complete.xlsx", sheet = "Results")
till_k <- read_excel("data/Tillage_Kenya_complete.xlsx", sheet = "Results")

trt1_list <- till_k %>% select(group_level1, group_level2, group_level3, trt1_name) %>% unique()

#reduce # of trt1_name so visualizations are logical
#full details of each treatment are provided in col trt1_details 

trt1_name.rename <- function(data) {
  # Original functions
  data %>%
    mutate(
      trt1_name =
        ifelse(trt1_name == "Nitrogen Mineral fertilzer" | trt1_name == "Nitrogen Mineral Fertilizer",
               "Nitrogen (N) Mineral Fertilizer",
               trt1_name)
    ) %>%
    mutate(
      trt1_name =
        ifelse(trt1_name == "Phosphorus Mineral Fertilizer",
               "Phosphorus (P) Mineral Fertilizer",
               trt1_name)
    ) %>%
    mutate(
      trt1_name =
        ifelse(trt1_name == "Potassium Mineral Fertilizer",
               "Potassium (K) Mineral Fertilizer",
               trt1_name)
    ) %>%
  
    mutate(
      trt2_name =
        ifelse(trt2_name == "Storage",
               "Carbon Storage",
               trt2_name)
    ) %>%
    mutate(
      trt1_name =
        ifelse(trt1_name == "Micronutrient Mineral Fertilizer",
               "Micronutrient Fertilizer",
               trt1_name)
    ) %>%
    mutate(
      trt1_name =
        ifelse(trt1_name == "Phosphorus Mineral Fertilizer;Nitrogen Mineral Fertilizer" |
                 trt1_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer",
               "NP Mineral Fertilizer",
               trt1_name)
    ) %>%
    mutate(
      trt1_name =
        ifelse(trt1_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Foliar Fertilizer",
               "NP Mineral Fertilizer",
               trt1_name)
    ) %>%
    mutate(
      trt1_name =
        ifelse(trt1_name == "Nitrogen Mineral Fertilizer;Potassium Mineral Fertilizer",
               "NK Mineral Fertilizer",
               trt1_name)
    ) %>%
    mutate(
      trt1_name =
        ifelse(trt1_name == "Phosphorus Mineral Fertilizer;Potassium Mineral Fertilizer",
               "PK Mineral Fertilizer",
               trt1_name)
    ) %>%
    mutate(
      trt1_name =
        ifelse(trt1_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Potassium Mineral Fertilizer",
               "NPK Mineral Fertilizer",
               trt1_name)
    ) %>%
    mutate(
      trt1_name =
        ifelse(trt1_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Potassium Mineral Fertilizer;Micronutrient Mineral Fertilizer",
               "NPK Mineral Fertilizer",
               trt1_name)
    ) %>%
    mutate(
      trt1_name =
        ifelse(trt1_name == "Green Manure" |
                 trt1_name == "Incinerated Organics" |
                 trt1_name == "Green Manure;Incinerated Organics" |
                 trt1_name == "Nitrogen Mineral Fertilizer;Incinerated Organics" |
                 trt1_name == "Green Manure;Nitrogen Mineral Fertilizer;Incinerated Organics" |
                 trt1_name == "Animal Manure;Incinerated Organics" |
                 trt1_name == "Animal Manure;Phosphorus Mineral Fertilizer;Foliar Fertilizer" |
                 trt1_name == "Animal Manure;Incinerated Organics;Foliar Fertilizer" |
                 trt1_name == "Animal Manure;Phosphorus Mineral Fertilizer" |
                 trt1_name == "Animal Manure" |
                 trt1_name == "Insect Frass" |
                 trt1_name == "Animal Manure;Incinerated Organics;Phosphorus Mineral Fertilizer" |
                 trt1_name == "Green Manure;Animal Manure;Legume Intercrop" |
                 trt1_name == "Green Manure;Animal Manure" |
                 trt1_name == "Legume Intercrop" |
                 trt1_name == "Legume Intercrop;Green Manure" |
                 trt1_name == "Green Manure;Nitrogen Mineral Fertilizer" |
                 trt1_name == "Unspecified Mineral Fertilizer;Green Manure" |
                 trt1_name == "Unspecified Mineral Fertilizer;Green Manure;Animal Manure" |
                 trt1_name == "Animal Manure;Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer" |
                 trt1_name == "Phosphorus Mineral Fertilizer;Green Manure" |
                 trt1_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Green Manure" |
                 trt1_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Green Manure;Animal Manure" |
                 trt1_name == "Nitrogen Mineral Fertilizer;Green Manure" |
                 trt1_name == "Green Manure;Phosphorus Mineral Fertilizer;Potassium Mineral Fertilizer" |
                 trt1_name == "Incinerated Organics;Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Potassium Mineral Fertilizer" |
                 trt1_name == "Green Manure;Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Potassium Mineral Fertilizer" |
                 trt1_name == "Legume Intercrop;Nitrogen Mineral Fertilizer" |
                 trt1_name == "Phosphorus Mineral Fertilizer;Animal Manure" |
                 trt1_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Animal Manure" |
                 trt1_name == "Green Manure;Phosphorus Mineral Fertilizer",
               "NA",
               trt1_name)
    )
}


nm_k_new <- trt1_name.rename(nm_k)  
trt1_new <- data.frame(unique(nm_k_new$trt1_name))


trt2_name.rename <- function(data) {
  # Original functions
  data %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Green Manure;Animal Manure" |
                 trt2_name == "Green Manure; Animal Manure" |
                 trt2_name == "Animal Manure; Green Manure" |
                 trt2_name ==  "Animal Manure;Green Manure",
               "Organic Amendment Combination",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Legume Intercrop;Green Manure",
               "Organic Amendment Combination",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Biologicals",
               "Integrated Fertility Management",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Green Manure;Incinerated Organics",
               "Organic Amendment Combination",
               trt2_name)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Microbial Biomass",
               "Microbial biomass",
               group_level3)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Animal Manure;Incinerated Organics" | 
                 trt2_name == "Animal Manure;Incinerated Organics;Foliar Fertilizer",
               "Organic Amendment Combination",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Green Manure;Animal Manure;Legume Intercrop",
               "Organic Amendment Combination",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Phosphorus Mineral Fertilizer;Animal Manure" |
                 trt2_name == "Animal Manure;Phosphorus Mineral Fertilizer" |
                 trt2_name == "Animal Manure;Phosphorus Mineral Fertilizer;Foliar Fertilizer" |
                 trt2_name == "Animal Manure;Potassium Mineral Fertilizer" |
                 trt2_name == "Animal Manure; Nitrogen Mineral Fertilizer" |
                 trt2_name == "Animal Manure;Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer" |
                 trt2_name == "Animal Manure;Phosphorus Mineral Fertilizer;Nitrogen Mineral Fertilizer" |
                 trt2_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Animal Manure" |
                 trt2_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Potassium Mineral Fertilizer;Animal Manure",
               "Integrated Fertility Management",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Green Manure;Nitrogen Mineral Fertilizer" |
                 trt2_name == "Nitrogen Mineral Fertilizer;Green Manure" |
                 trt2_name == "Green Manure; Nitrogen Mineral Fertilizer" |
                 trt2_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Green Manure" |
                 trt2_name == "Phosphorus Mineral Fertilizer;Green Manure" |
                 trt2_name == "Green Manure;Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Potassium Mineral Fertilizer" |
                 trt2_name == "Green Manure;Phosphorus Mineral Fertilizer;Potassium Mineral Fertilizer" |
                 trt2_name == "Nitrogen Mineral Fertilizer; Green Manure" |
                 trt2_name == "Green Manure; Phosphorus Mineral Fertilizer" |
                 trt2_name == "Green Manure;Phosphorus Mineral Fertilizer" |
                 trt2_name == "Green Manure;Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer" |
                 trt2_name == "Green Manure;Phosphorus Mineral Fertilizer;Nitrogen Mineral Fertilizer",
               "Integrated Fertility Management",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Potassium Mineral Fertilizer;Incinerated Organics" |
                 trt2_name == "Nitrogen Mineral Fertilizer;Incinerated Organics" |
                 trt2_name == "Incinerated Organics;Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Potassium Mineral Fertilizer",
               "Integrated Fertility Management",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Legume Intercrop; Nitrogen Mineral Fertilizer" |
                 trt2_name == "Legume Intercrop;Nitrogen Mineral Fertilizer" |
                 trt2_name == "Legume Intercrop;Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer" |
                 trt2_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Legume Intercrop",
               "Integrated Fertility Management",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Green Manure;Legume Intercrop;Phosphorus Mineral Fertilizer" |
                 trt2_name == "Green Manure;Legume Intercrop;Phosphorus Mineral Fertilizer" |
                 trt2_name == "Legume Intercrop;Green Manure; Nitrogen Mineral Fertilizer" |
                 trt2_name == "Green Manure;Legume Intercrop;Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer",
               "Integrated Fertility Management",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Green Manure;Legume Intercrop",
               "Organic Amendment Combination",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Animal Manure;Incinerated Organics;Phosphorus Mineral Fertilizer",
               "Integrated Fertility Management",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Green Manure;Nitrogen Mineral Fertilizer;Incinerated Organics",
               "Integrated Fertility Management",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Nitrogen Mineral Fertilizer" |
                 trt2_name == "Nitrogen Mineral fertilzer" ,
               "N Mineral Fertilizer",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Phosphorus Mineral Fertilizer",
               "P Mineral Fertilizer",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer" |
                 trt2_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Foliar Fertilizer" |
                 trt2_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Micronutrient Mineral Fertilizer" |
                 trt2_name == "Phosphorus Mineral Fertilizer; Nitrogen Mineral Fertilizer" |
                 trt2_name == "Nitrogen Mineral Fertilizer; Phosphorus Mineral Fertilizer" |
                 trt2_name == "Phosphorus Mineral Fertilizer;Nitrogen Mineral Fertilizer",
               "NP Mineral Fertilizer",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Nitrogen Mineral Fertilizer;Potassium Mineral Fertilizer",
               "NK Mineral Fertilizer",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Phosphorus Mineral Fertilizer;Potassium Mineral Fertilizer",
               "PK Mineral Fertilizer",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Potassium Mineral Fertilizer" |
                 trt2_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Potassium Mineral Fertilizer;Foliar Fertilizer" |
                 trt2_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Potassium Mineral Fertilizer;Micronutrient Mineral Fertilizer" |
                 trt2_name == "Nitrogen Mineral Fertilizer; Phosphorus Mineral Fertilizer; Potassium Mineral Fertilizer",
               "NPK Mineral Fertilizer",
               trt2_name)
    ) %>%
    mutate(
      trt2_name =
        ifelse(trt2_name == "Green Manure;Animal Manure;Phosphorus Mineral Fertilizer" |
                 trt2_name == "Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Green Manure;Animal Manure" |
                 trt2_name == "Unspecified Mineral Fertilizer;Green Manure;Animal Manure" |
                 trt2_name == "Animal Manure;Nitrogen Mineral Fertilizer;Phosphorus Mineral Fertilizer;Green Manure",
               "Integrated Fertility Management",
               trt2_name)
    )
}

nm_k_new <- trt2_name.rename(nm_k_new)
trt2_new <- nm_k_new %>% 
  select(trt1_name, trt2_name) %>%
  count() %>% unique()

unique(trt2_new$trt2_name)
write.csv(trt2_new, "trtmt_all.csv")

write.csv(nm_k_new, "nm_results.csv")


write.csv(rv_list, "nm_econ_list.csv")

####Economic grouping updates ####

nm_k <- read_excel("data/NutrientMgmt_Kenya_complete.xlsx", sheet = "Results")
cc_k <- read_excel("data/ContinuousCover_Kenya_complete.xlsx", sheet = "Results")
till_k <- read_excel("data/Tillage_Kenya_complete.xlsx", sheet = "Results")

cc_rv_list <- cc_k.new %>%
            filter(group_level1 == "Economics") %>%
            select(rv, group_level2, group_level3) %>%
            unique()
nm_rv_list <- nm_k.new %>%
  filter(group_level1 == "Economics") %>%
  select(rv, group_level2, group_level3) %>%
  unique()

till_rv_list <- till_k.new %>%
  filter(group_level1 == "Economics") %>%
  select(rv, group_level2, group_level3) %>%
  unique() 

rv_list <- full_join(cc_rv_list, nm_rv_list)
rv_list2 <- full_join(rv_list, till_rv_list)

rv_list2 <- unique(rv_list2)

write.csv(rv_list2, "econ_list.csv")

econ_groups <- read.csv("econ_list.csv")
econ_groups <- econ_groups[-1 ]

###now reassign rv's to groups

econ_list <- read.csv("econ_list.csv")

group_levels.rename <- function(data) {
  # Original functions
  data %>%
    mutate(
      group_level3 =
        ifelse(rv == "benefit cost ratio, season 1"|
               rv == "benefit cost ratio, season 2"|
               rv == "benefit cost ratio, season 3"|
               rv == "benefit-cost ratio, Desmodium cut 12 weeks after planting maize, long rains"|
               rv == "benefit-cost ratio, Desmodium cut 12 weeks after planting maize, short rains"|
               rv == "benefit-cost ratio, Desmodium cut 18 weeks after planting maize, long rains"|
               rv == "benefit-cost ratio, Desmodium cut 18 weeks after planting maize, short rains"|
               rv == "benefit-cost ratio, Desmodium cut 9 weeks after planting maize, long rains"|
               rv == "benefit-cost ratio, Desmodium cut 9 weeks after planting maize, short rains"|
               rv == "benefit-cost ratio, long rains"|
               rv == "benefits/costs ratio"|
               rv == "cumulative benfit-cost ratio, good fertility soils, crop residue added"|
               rv == "cumulative benfit-cost ratio, good fertility soils, crop residue removed"|
               rv == "cumulative benfit-cost ratio, medium fertility soils, crop residue added"|
               rv == "cumulative benfit-cost ratio, medium fertility soils, crop residue removed"|
               rv == "cumulative benfit-cost ratio, poor fertility soils, crop residue added"|
               rv == "cumulative benfit-cost ratio, poor fertility soils, crop residue removed"|
               rv ==  "benefit to cost ratio, no anti-erosion plant barriers"|
               rv ==  "benefit to cost ratio, Napier anti-erosion plant barriers"|
               rv == "benefit to cost ratio, Leucaena anti-erosion plant barriers",
               "Benefit cost ratio (GRTC)",
               group_level3)) %>%
        mutate(
          group_level3 =
            ifelse(rv == "benefit to cost ratio (grosss benefits and total variable costs) , Leucaena anti-erosion plant barriers"|
                     rv =="benefit to cost ratio (grosss benefits and total variable costs), Napier anti-erosion plant barriers"|
                     rv =="benefit to cost ratio (grosss benefits and total variable costs), no anti-erosion plant barriers"|
                     rv =="benefit-cost ratio",
                   "Benefit cost ratio (GRVC)",
                   group_level3) )%>% 
            mutate(
              group_level3 =
                ifelse(rv == "benefit-cost ratio, fertile site"|
                       rv == "benefit-cost ratio, poor fertility site",
                    "Benefit cost ratio (NRTC)",
                       group_level3)) %>%
    mutate(
      group_level2 =
        ifelse(group_level3 == "Benefit cost ratio (NRTC)"|
                 group_level3 == "Benefit cost ratio (GRVC)"|
                 group_level3 == "Benefit cost ratio (GRTC)",
               "Economic Performance",
               group_level2)) %>%
        mutate(
          group_level3 =
            ifelse(rv == "average seasonal gross margin, continuous maize, crop residue"|
                     rv == "average seasonal gross margin, continuous maize, no crop residue"|
                     rv == "average seasonal gross margin, maize-soybean intercrop, crop residue"|
                     rv == "average seasonal gross margin, maize-soybean intercrop, no crop residue"|
                     rv == "average seasonal gross margin, maize-soybean rotation, crop residue"|
                     rv == "average seasonal gross margin, maize-soybean rotation, no crop residue"|
                     rv == "Common beans & soybean yield gross margins"|
                     rv == "cumulative gross benefits, good fertility soils, crop residue added"|
                     rv == "cumulative gross benefits, good fertility soils, crop residue removed"|
                     rv == "cumulative gross benefits, medium fertility soils, crop residue added"|
                     rv == "cumulative gross benefits, medium fertility soils, crop residue removed"|
                     rv == "cumulative gross benefits, poor fertility soils, crop residue added"|
                     rv == "cumulative gross benefits, poor fertility soils, crop residue removed"|
                     rv == "gross margin, conventional tillage, continuous maize"|
                     rv == "gross margin, conventional tillage, maize-soybean intercrop"|
                     rv == "gross margin, conventional tillage, maize-soybean rotation"|
                     rv == "gross margin, reduced tillage, continuous maize"|
                     rv == "gross margin, reduced tillage, maize-soybean intercrop"|
                     rv == "gross margin, reduced tillage, maize-soybean rotation"|
                     rv == "Gross margins per hectare"|
                     rv == "Maize yield gross margins"|
                     rv == "net benefits (difference between grosss benefits and total variable costs), Leucaena anti-erosion plant barriers"|
                     rv == "net benefits (difference between grosss benefits and total variable costs), Napier anti-erosion plant barriers"|
                     rv == "net benefits (difference between grosss benefits and total variable costs), no anti-erosion plant barriers"|
                     rv == "marginal rate of return for maize-bean intercropped system, long rains"|
                       rv == "marginal rate of return for maize-bean intercropped system, short rains",
                   "Gross margin",
                   group_level3))%>% 
        mutate(
          group_level3 =
            ifelse(rv == "average seasonal gross revenue, continuous maize, crop residue"|
                     rv == "average seasonal gross revenue, continuous maize, no crop residue"|
                     rv == "average seasonal gross revenue, maize-soybean intercrop, crop residue"|
                     rv == "average seasonal gross revenue, maize-soybean intercrop, no crop residue"|
                     rv == "average seasonal gross revenue, maize-soybean rotation, crop residue"|
                     rv == "average seasonal gross revenue, maize-soybean rotation, no crop residue"|
                     rv == "gross revenue, conventional tillage,  crop residue added"|
                     rv == "gross revenue, conventional tillage, no crop residue added"|
                     rv == "gross revenue, reduced tillage, crop residue added"|
                     rv == "gross revenue, reduced tillage, no crop residue added"|
                     rv == "maize revenue at $0.20/kg"|
                     rv == "net income (returns to land), high fertility soil"|
                     rv == "net income (returns to land), low fertility soil"|
                     rv == "net income (returns to land), medium fertility soil"|
                     rv == "net income of maize-bean intercropped system, long rains"|
                     rv == "net income of maize-bean intercropped system, short rains"|
                     rv == "profit by season, season 1"|
                     rv == "profit by season, season 2"|
                     rv == "profit by season, season 3"|
                     rv == "profit by season, season 4"|
                     rv == "profit by season, season 5"|
                     rv == "profit by season, season 6"|
                     rv == "Total gross field benefits"|
                     rv == "total profits, main seasons"|
                     rv == "total profits, minor seasons"|
                     rv == "total revenue, main seasons"|
                     rv == "total revenue, minor seasons"|
                     rv == "total value of produce (crop, livestock, tree)"|
                     rv == "gross output (1 USD = 103.85 Ksh)",
                   "Gross return",
                   group_level3)) %>%
    mutate(
      group_level3 =
        ifelse(rv == "profit (discounted), maize-Crotalaria rotation"|
                 rv == "profit (discounted), maize-soybean rotation"|
                 rv == "profit (discounted), monocrop"|
                 rv == "profit (discounted), push pull system"|
                 rv == "net benefit, Desmodium cut 12 weeks after planting maize, long rains"|
                 rv == "net benefit, Desmodium cut 12 weeks after planting maize, short rains"|
                 rv == "net benefit, Desmodium cut 18 weeks after planting maize, long rains"|
                 rv == "net benefit, Desmodium cut 18 weeks after planting maize, short rains"|
                 rv == "net benefit, Desmodium cut 9 weeks after planting maize, long rains"|
                 rv == "net benefit, Desmodium cut 9 weeks after planting maize, short rains"|
                 rv == "net benefit, long rains"|
                 rv == "net benefits"|
                 rv == "net benefits, fertile site"|
                 rv == "net benefits, maize-bean intercrop, conventional planting arrangement"|
                 rv == "net benefits, maize-bean intercrop, improved (MBILI) planting arrangement"|
                 rv == "net benefits, maize-cowpea intercrop, conventional planting arrangement"|
                 rv == "net benefits, maize-cowpea intercrop, improved (MBILI) planting arrangement"|
                 rv == "net benefits, maize-groundnut intercrop, conventional planting arrangement"|
                 rv == "net benefits, maize-groundnut intercrop, improved (MBILI) planting arrangement"|
                 rv == "net benefits, poor fertility site",
               "Net return",
                 group_level3)) %>%
        mutate(
          group_level2 =
            ifelse(group_level3 == "Gross return"|
                     group_level3 == "Gross margin"|
                     group_level3 == "Net return",
                   "Income",
                   group_level2)) %>%
            mutate(
              group_level3 =
                ifelse(rv == "nitrous oxide emission intensity (N2OEI)",
                       "Growing season nitrous oxide",
                       group_level3)) %>%
            mutate(
              group_level2 =
                ifelse(group_level3 == "Growing season nitrous oxide",
                       "Nitrogen Emissions",
                       group_level2)) %>%
                mutate(
                  group_level1 =
                    ifelse(group_level2 == "Nitrogen Emissions",
                           "Climate Mitigation",
                           group_level1)) %>%
        mutate(
          group_level3 =
            ifelse(rv == "cumulative labor costs, good fertility soils, crop residue added"|
                     rv == "cumulative labor costs, good fertility soils, crop residue removed"|
                     rv == "cumulative labor costs, medium fertility soils, crop residue added"|
                     rv == "cumulative labor costs, medium fertility soils, crop residue removed"|
                     rv == "cumulative labor costs, poor fertility soils, crop residue added"|
                     rv == "cumulative labor costs, poor fertility soils, crop residue removed"|
                     rv == "cumulative labor hours, good fertility soils, crop residue added"|
                     rv == "cumulative labor hours, good fertility soils, crop residue removed"|
                     rv == "cumulative labor hours, medium fertility soils, crop residue added"|
                     rv == "cumulative labor hours, medium fertility soils, crop residue removed"|
                     rv == "cumulative labor hours, poor fertility soils, crop residue added"|
                     rv == "cumulative labor hours, poor fertility soils, crop residue removed"|
                     rv == "labor costs"|
                     rv == "labor costs for weed control and land prepartion"|
                     rv == "labor costs, fertile site"|
                     rv == "labor costs, poor fertility site"|
                     rv == "labor requirements for the practice"|
                     rv == "labour costs, first season"|
                     rv == "labour costs, second through fourth seasons"|
                     rv == "total cost of labor (1 USD = 103.85 Ksh)"|
                     rv == "returns to labour, high fertility soil"|
                     rv == "returns to labour, low fertility soil"|
                     rv == "returns to labour, medium fertility soil",
                   "Labour cost",
                   group_level3)) %>%
          mutate(
            group_level3 =
              ifelse(rv == "cumulative total costs, good fertility soils, crop residue added"|
                       rv ==  "cumulative total costs, good fertility soils, crop residue removed"|
                       rv == "cumulative total costs, medium fertility soils, crop residue added"|
                       rv == "cumulative total costs, medium fertility soils, crop residue removed"|
                       rv == "cumulative total costs, poor fertility soils, crop residue added"|
                       rv == "cumulative total costs, poor fertility soils, crop residue removed"|
                       rv == "total costs"|
                       rv == "total costs, first season"|
                       rv == "total costs, main seasons"|
                       rv == "total costs, minor seasons"|
                       rv == "total costs, season 3"|
                       rv == "total costs, seasons 1 & 2"|
                       rv == "total costs, second through fourth seasons"|
                       rv == "total production cost of maize-bean intercropped system, long rains"|
                       rv == "total production cost of maize-bean intercropped system, short rains"|
                       rv == "costs, maize-bean intercrop, conventional planting arrangement"|
                       rv == "costs, maize-bean intercrop, improved (MBILI) planting arrangement"|
                       rv == "costs, maize-cowpea intercrop, conventional planting arrangement"|
                       rv == "costs, maize-cowpea intercrop, improved (MBILI) planting arrangement"|
                       rv == "costs, maize-groundnut intercrop, conventional planting arrangement"|
                       rv == "costs, maize-groundnut intercrop, improved (MBILI) planting arrangement",
                     "Total cost",
                     group_level3)) %>%
          mutate(
            group_level3 =
              ifelse(rv == "average fertilizer cost for maize grain production"|
                       rv == "average seasonal total variable costs, continuous maize, crop residue"|
                       rv == "average seasonal total variable costs, continuous maize, no crop residue"|
                       rv == "average seasonal total variable costs, maize-soybean intercrop, crop residue"|
                       rv == "average seasonal total variable costs, maize-soybean intercrop, no crop residue"|
                       rv == "average seasonal total variable costs, maize-soybean rotation, crop residue"|
                       rv == "average seasonal total variable costs, maize-soybean rotation, no crop residue"|
                       rv == "Cost of seed, fertilizer, land clearing/preparation, inoculum, planting, weeding, and harvesting"|
                       rv == "cumulative non-labor costs, good fertility soils, crop residue added"|
                       rv == "cumulative non-labor costs, good fertility soils, crop residue removed"|
                       rv == "cumulative non-labor costs, medium fertility soils, crop residue added"|
                       rv == "cumulative non-labor costs, medium fertility soils, crop residue removed"|
                       rv == "cumulative non-labor costs, poor fertility soils, crop residue added"|
                       rv == "cumulative non-labor costs, poor fertility soils, crop residue removed"|
                       rv == "input costs, season 3"|
                       rv == "input costs, seasons 1 & 2"|
                       rv == "labor and material costs for weed control and land prepartion"|
                       rv == "non-labor costs, fertile site"|
                       rv == "non-labor costs, poor fertility site"|
                       rv == "seed and fertilizer costs, first season"|
                       rv == "seed and fertilizer costs, second through fourth seasons"|
                       rv == "total variable costs (1 USD = 103.85 Ksh)"|
                       rv == "total variable costs, conventional tillage, continuous maize"|
                       rv == "total variable costs, conventional tillage, crop residue added"|
                       rv == "total variable costs, conventional tillage, crop residue added"|
                       rv == "total variable costs, conventional tillage, maize-soybean intercrop"|
                       rv == "total variable costs, conventional tillage, maize-soybean rotation"|
                       rv == "total variable costs, conventional tillage, no crop residue added"|
                       rv == "total variable costs, conventional tillage, no crop residue added"|
                       rv == "total variable costs, reduced tillage, continuous maize"|
                       rv == "total variable costs, reduced tillage, crop residue added"|
                       rv == "total variable costs, reduced tillage, crop residue added"|
                       rv == "total variable costs, reduced tillage, maize-soybean intercrop"|
                       rv ==  "total variable costs, reduced tillage, maize-soybean rotation"|
                       rv == "total variable costs, reduced tillage, no crop residue added"|
                       rv == "total variable costs, reduced tillage, no crop residue added"|
                       rv == "variable cost, Leucaena anti-erosion plant barriers"|
                       rv == "variable cost, Napier anti-erosion plant barriers"|
                       rv == "variable cost, no anti-erosion plant barriers"|
                       rv == "productivity of capital, Trench 2W",
                     "Variable cost",
                     group_level3)) %>%
          mutate(
            group_level2 =
              ifelse(group_level3 == "Labour Cost"|
                       group_level3 == "Total Cost"|
                       group_level3 == "Variable Cost",
                     "Costs",
                     group_level2)) %>%
      mutate(
        group_level3 =
          ifelse(rv == "percentage economic loss in maize, intercrop planted in alternate rows, major cropping season"|
                   rv == "percentage economic loss in maize, intercrop planted in alternate rows, minor cropping season"|
                   rv == "percentage economic loss in maize, intercrop planted in mixed rows, major cropping season"|
                   rv == "percentage economic loss in maize, intercrop planted in mixed rows, minor cropping season"|
                   rv == "percentage economic loss in maize, major cropping season"|
                   rv == "percentage economic loss in maize, minor cropping season",
                 "Maize",
                 group_level3)) %>%
        mutate(
          group_level3 =
            ifelse(rv == "percentage economic loss in sorghum main stem, intercrop planted in alternate rows, major cropping season"|
                 rv == "percentage economic loss in sorghum main stem, intercrop planted in alternate rows, minor cropping season"|
                 rv == "percentage economic loss in sorghum main stem, intercrop planted in mixed rows, major cropping season"|
                 rv == "percentage economic loss in sorghum main stem, intercrop planted in mixed rows, minor cropping season"|
                 rv == "percentage economic loss in sorghum main stem, major cropping season"|
                 rv == "percentage economic loss in sorghum main stem, minor cropping season"|
                 rv ==  "percentage economic loss in sorghum tillers, intercrop planted in alternate rows, major cropping season"|
                 rv ==  "percentage economic loss in sorghum tillers, intercrop planted in alternate rows, minor cropping season"|
                 rv == "percentage economic loss in sorghum tillers, intercrop planted in mixed rows, major cropping season"|
                 rv == "percentage economic loss in sorghum tillers, intercrop planted in mixed rows, minor cropping season"|
                 rv == "percentage economic loss in sorghum tillers, major cropping season"|
                 rv == "percentage economic loss in sorghum tillers, minor cropping season",
               "Sorghum",
               group_level3)) %>%
        mutate(
          group_level2 =
            ifelse(rv == "percentage economic loss in maize, intercrop planted in alternate rows, major cropping season"|
                     rv == "percentage economic loss in maize, intercrop planted in alternate rows, minor cropping season"|
                     rv == "percentage economic loss in maize, intercrop planted in mixed rows, major cropping season"|
                     rv == "percentage economic loss in maize, intercrop planted in mixed rows, minor cropping season"|
                     rv == "percentage economic loss in maize, major cropping season"|
                     rv == "percentage economic loss in maize, minor cropping season"|
                     rv == "percentage economic loss in sorghum main stem, intercrop planted in alternate rows, major cropping season"|
                     rv == "percentage economic loss in sorghum main stem, intercrop planted in alternate rows, minor cropping season"|
                     rv == "percentage economic loss in sorghum main stem, intercrop planted in mixed rows, major cropping season"|
                     rv == "percentage economic loss in sorghum main stem, intercrop planted in mixed rows, minor cropping season"|
                     rv == "percentage economic loss in sorghum main stem, major cropping season"|
                     rv == "percentage economic loss in sorghum main stem, minor cropping season"|
                     rv == "percentage economic loss in sorghum tillers, intercrop planted in alternate rows, major cropping season"|
                     rv == "percentage economic loss in sorghum tillers, intercrop planted in alternate rows, minor cropping season"|
                     rv == "percentage economic loss in sorghum tillers, intercrop planted in mixed rows, major cropping season"|
                     rv == "percentage economic loss in sorghum tillers, intercrop planted in mixed rows, minor cropping season"|
                     rv == "percentage economic loss in sorghum tillers, major cropping season"|
                     rv == "percentage economic loss in sorghum tillers, minor cropping season",
                   "Yields",
                   group_level2)) %>%
        
        mutate(
          group_level1 =
            ifelse(group_level2 == "Yields",
                   "Crop Yields",
                   group_level1)) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "French bean yield loss due to pests",
               "Yield loss due to pests",
               group_level3)) %>%
  mutate(
    group_level3 =
      ifelse(group_level3 == "Natural enemies",
             "Natural enemy",
             group_level3)) %>%
      mutate(
        group_level3 =
          ifelse(group_level3 == "Water",
                 "Water discharge",
                 group_level3)) %>%
    mutate(
      group_level2 =
        ifelse(group_level3 == "Crop water use efficiency",
               "Water Use",
               group_level2)) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Crop water use efficiency",
               "Crop",
               group_level3))%>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Microbial Biomass",
               "Microbial biomass",
               group_level3))
  
  
  
  
  }


cc_k.new <- group_levels.rename(cc_k)
nm_k.new <- group_levels.rename(nm_k_new)
till_k.new <- group_levels.rename(till_k)

trt1_list <- cc_k.new %>% select(group_level1, group_level2, group_level3, trt1_name) %>% unique()
trt1_list <- nm_k.new %>% select(group_level1, group_level2, group_level3, trt1_name) %>% unique()
trt1_list <- till_k.new %>% select(group_level1, group_level2, group_level3, trt1_name) %>% unique()


#manually add updated 'Results' worksheet to complete file.
write.csv(cc_k.new, "data/cc_results.csv")
write.csv(nm_k.new, "data/nm_results.csv")
write.csv(till_k.new, "data/till_results.csv")


#use updated matrix as "complete" worksheet
#openxlsx doesn't consistentlywork 


#Cover Crops
cc_wb <- loadWorkbook("data/ContinuousCover_Kenya_complete.xlsx")
openxlsx::removeWorksheet(cc_wb, "Results")
openxlsx::addWorksheet(cc_wb, "Results")
openxlsx::writeData(cc_wb, "Results", cc_k.new)
openxlsx::saveWorkbook(cc_wb, "data/ContinuousCover_AgEKenya_complete2.xlsx", overwrite = T)

#Nutrient Management
nm_wb <- loadWorkbook("data/NutrientMgmt_Kenya_complete.xlsx")
openxlsx::removeWorksheet(nm_wb, "Results")
openxlsx::addWorksheet(nm_wb, "Results")
openxlsx::writeData(nm_wb, "Results", nm_k.new)
openxlsx::saveWorkbook(nm_wb, "data/NutrientMgmt_Kenya_complete2.xlsx", overwrite = T)

#Tillage
till_wb <- loadWorkbook("data/Tillage_Kenya_complete.xlsx")
openxlsx::removeWorksheet(till_wb, "Results")
openxlsx::addWorksheet(till_wb, "Results")
openxlsx::writeData(till_wb, "Results", till_k.new)
openxlsx::saveWorkbook(till_wb, "data/Tillage_Kenya_complete2.xlsx", overwrite = T)
