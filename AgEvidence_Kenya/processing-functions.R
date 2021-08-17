#### FUNCTIONS ####

# Make changes to the normative grouping files
ne.mods <- function(data) {
  data %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Invertebrate Foliar Community Diversity",
               "Aboveground Invertebrates",
               group_level3)
    )%>%
    mutate(
      group_level3 =
        group_level3 %>% str_to_title()
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Ph",
               "pH",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Cec",
               "CEC",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "(Ler)",
               "(LER)",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Natural Abundance Of 13c",
               "Natural Abundance of 13C",
               group_level3)
    )
}

# Code markdown style into the data for HTML rendering
mkdown <- function(data){
  data$rv <- data %>%
    select(rv) %>% as_vector() %>% 
    str_replace_all(
      c("NH4-N"="NH~4~-N",
        "NH4"="NH~4~",
        "NO3-N"="NO~3~-N",
        "NO3"="NO~3~",
        "N2O"="N~2~O",
        "N2O-N"="N~2~O-N",
        "CO2-C"="CO~2~-C",
        "CO2"="CO~2~",
        "CH4"="CH~4~",
        "CH4-C"="CH~4~-C"
        )
      )
  data$'rv_units' <- data %>%
    select('rv_units') %>% as_vector() %>%
    str_replace_all(
      c("\\^3"="^3^",
        "\\^2"="^2^",
        "\\^-1"="^-1^",
        "\\^1/2"="^1/2^",
        "CO2-C"="CO~2~-C",
        "NO3-Nconc"="NO~3~-N~conc~",
        "NO3-Nyield"="NO~3~-N~yield~",
        "N2O-N"="N~2~O-N",
        "micrograms N2O/g  soil/hr"="micrograms N~2~O/g",
        "NO3-N"="NO~3~-N",
        "CO2"="CO~2~",
        "H2O"="H~2~O",
        "# (log10(x+1))"="# (log~10~(x+1))",
        "0/00"="per thousand",
        "delta-13C"="delta^13^C",
        "CH4-C"="CH~4~-C",
        "NH4-N"="NH~4~-N",
        "NA"="",
        "NH4"="NH~4~",
        "NO3"="NO~3~",
        "N2O"="N~2~O",
        "CH4"="CH~4~"
      )
    )
  return(data)
}

# Assorted changes to the GL1s
gl1.rename <- function(data) {
  # Original functions
  data %>%
    mutate(
      group_level1 =
        ifelse(group_level3 == "French bean yield loss due to pests" | group_level3 == "Yield loss due to pests",
               "Pests",
               group_level1)
    )
}

# Assorted changes to the GL2s
gl2.rename <- function(data) {
  # Original functions
  data %>%
    mutate(
      group_level2 =
        ifelse(group_level3 == "Loss of soil organic carbon",
               "Carbon Emissions",
               group_level2)
    ) %>%
    mutate(
      group_level2 =
        ifelse(group_level2 == "Storage",
               "Carbon Storage",
               group_level2)
    ) %>%
    mutate(
      group_level2 =
        ifelse(group_level3 == "Maize stalk biomass",
               "Crop Growth",
               group_level2)   
    ) %>%
    mutate(
      group_level2 =
        ifelse(group_level2 == "Fodder Yields" | group_level2 == "Grain Yields",
               "Yields",
               group_level2)  
    ) %>%
    mutate(
      group_level2 =
        ifelse(group_level3 == "Maize stalk nitrogen content",
               "Plant Nutrient Content",
               group_level2)  
    ) %>%
  mutate(
    group_level2 =
      ifelse(group_level3 == "Benefit-cost ratio",
             "Income",
             group_level2)  
  ) %>%
    mutate(
      group_level2 =
        ifelse(group_level3 == "Pest Damage",
               "Expenses",
               group_level2)  
    ) %>%
    mutate(
      group_level2 =
        ifelse(group_level2 == "Pathogens",
               "Crop Damage",
               group_level2)  
    ) %>%
    mutate(
      group_level2 =
        ifelse(group_level2 == "Invertebrate Pests",
               "Invertebrates",
               group_level2)  
    )%>%
    mutate(
      group_level2 =
        ifelse(group_level3 == "Predators (#)" ,
               "Pest Natural Enemies",
               group_level2) 
    )%>%
    mutate(
      group_level2 =
        ifelse(group_level3 == "Bacterivores (#)" | group_level3 == "Fungivores (#)" | group_level3 == "Nematode community (#)" | 
                 group_level3 == "Omnivores (#)" | group_level3 == "Predators (#)" ,
               "Non-Pests",
               group_level2)  
    )%>%
    mutate(
      group_level2 =
        ifelse(group_level3 == "Non-Predator & Pests",
               "Non-Pests",
               group_level2)  
    )%>%
    mutate(
      group_level2 =
        ifelse(group_level2 == "Micronutrients" | group_level2 == "Micro-nutrients",
               "Micro-Nutrients",
               group_level2)  
    )%>%
    mutate(
        group_level2 =
          ifelse(rv == "maize grain selenium concentration increase ratio relative to 5 g Se/ha, 10 g Se/ha added",
                 "Plant Nutrient Content",
                 group_level2)  
    )%>%
    mutate(
      group_level2 =
        ifelse(group_level2 == "Water Use",
               "Abiotic Factors",
               group_level2)
  )%>%
    mutate(
      group_level2 =
        ifelse(group_level3 == "Bactivorous nematdoes",
               "Non-Pests",
               group_level2)   
      )
}

# Assorted changes to the GL3s
gl3.rename <- function(data) {
  data %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Total greenhouse gas emissions",
               "Total greenhouse gases",
               group_level3)
    )%>%
    mutate(
      group_level3 =
        ifelse(rv == "maize grain selenium concentration increase ratio relative to 5 g Se/ha, 10 g Se/ha added",
               "Crop micro-nutrient content",
               group_level3)
      
     
    )%>%
    mutate(
    group_level3 =
      ifelse(rv == "crop water productivity, silicon added" | rv == "crop water productivity, no silicon added",
             "Evapotranspiration",
             group_level3)
    
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Microbial Biomass",
               "Microbial biomass",
               group_level3)
      
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Ammonia" | group_level3 == "Volatilized Ammonia",
               "Volatilized ammonia",
               group_level3)
      
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Maize root biomass" | group_level3 == "Root biomass",
               "Belowground biomass",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Maize stalk biomass",
               "Aboveground biomass",
               group_level3) 
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Blue pea",
               "Blue pea (fodder)",
               group_level3) 
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Leucaena",
               "Leucaena (fodder)",
               group_level3) 
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Napier grass",
               "Napier grass (fodder)",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Maize stalk nitrogen content",
               "Crop nitrogen content",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Carbon content",
               "Crop carbon content",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Maize stalk phosphorus content",
               "Crop phosphorus content",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Maize stalk potassium content",
               "Crop potassium content",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Nitrogen content" | group_level3 == "Root nitrogen content",
               "Crop nitrogen content",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Pest Damage" | group_level3 == "Economic Loss from Pest Damage",
               "Economic loss from pest damage",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Carbon decomposition rate in buried residues",
               "Carbon decomposition rate of buried residues",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Nitrogen decomposition rate in buried residues",
               "Nitrogen decomposition rate of buried residues",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Phosphorus decomposition rate in buried residues",
               "Phosphorus decomposition rate of buried residues",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Soil bulk density",
               "Bulk density",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Aggregate Size",
               "Aggregate size",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "French bean yield loss due to pests" | "Yield loss due to pests",
               "Damage from pests",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Crop Infections",
               "Damage from pathogens",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Bacterivores (#)",
               "Bactivorous nematodes",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Fungivores (#)",
               "Fungivorous nematodes",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Herbivores (#)",
               "Plant-parasitic",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Nematode community (#)",
               "Nematode community",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Omnivores (#)",
               "Omnivorous nematodes",
               group_level3)
    ) %>% 
    mutate(
      group_level3 =
        ifelse(group_level3 == "Predators (#)",
               "Predacious nematodes",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Natural enemy (#)",
               "Predacious invertebrates",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Striga (#)",
               "Striga",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "total nitrogen",
               "Total nitrogen",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Water",
               "Water discharge",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        group_level3 %>% str_to_title()
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Ph",
               "pH",
               group_level3)
    ) %>%
  mutate(
    group_level3 =
      ifelse(group_level3 == "(Ler)",
             "(LER)",
             group_level3)
  ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Cec",
               "CEC",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Natural Abundance Of 13c",
               "Natural Abundance of 13C",
               group_level3)
    )
   return(data)
}

# Generate two separate columns based on GLs and NEs
grouping <- function(data) {
  data %>%
    mutate(grouping=
             ifelse(is.na(group_level1_alt), 
                    paste(group_level1,"|",group_level2,"|",group_level3),
                    paste(group_level1,"|",group_level2,"|",group_level3,";",
                          group_level1_alt,"|",group_level2_alt,"|",group_level3)
             ),
           normative_effect=
             ifelse(is.na(group_level1_alt),
                    paste(norm_interp2,"|",norm_interp3),
                    paste(norm_interp2,"|",norm_interp3,";",
                          norm_interp2_alt,"|",norm_interp3_alt)
             )
    )
}

# Make sure a negative change doesn't become positive because of sign switching
sign.correction <- function(data) {
  data %>%
    mutate(per_change=
             ifelse(trt2_value < trt1_value & per_change > 0, 
                    per_change * -1,
                    per_change
                    )
           ) %>%
    mutate(per_change=
             ifelse(trt1_value < 0 & trt2_value > 0,
                    abs(per_change),
                    per_change
                    )
           )
}


