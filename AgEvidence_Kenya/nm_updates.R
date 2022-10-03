# Update the nutrient management trt1_names
library(openxlsx)
library(xlsx)


nm_k <- read_excel("data/NutrientMgmt_Kenya_complete.xlsx", sheet = "Results")
nm_k$trt1_nameorg <- nm_k$trt1_name
nm_k$trt2_nameorg <- nm_k$trt2_name

trt1 <- data.frame(unique(nm_k$trt1_name))
write.csv(trt1, "trt1_list_nm.csv")

trt2 <- data.frame(unique(nm_k$trt2_name))
write.csv(trt2, "trt2_list_nm.csv")

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

