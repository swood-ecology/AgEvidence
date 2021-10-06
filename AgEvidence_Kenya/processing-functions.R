#### FUNCTIONS ####

# Make changes to the normative grouping files
ne.mods <- function(data) {
  data %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Respiration" | group_level3 == "Carbon Dioxide (CO2)" | group_level3 == "Carbon dioxide (CO2)",
               "Carbon dioxide",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Nitrous oxide (N20)" | group_level3 == "Nitrous oxide (N2O)",
               "Nitrous oxide",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Nitric oxide (NO)",
               "Nitric oxide",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Herbivores (#)",
               "Herbivores",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Invertebrates (#)",
               "Invertebrates",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Natural enemy (#)",
               "Natural enemies",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Nematode community (#)",
               "Nematode community",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Nematodes (#)",
               "Nematodes",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Seed corn maggot (#)" | group_level3 == "Seedcorn maggot (#)",
               "Seedcorn maggot",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Weed community (#)",
               "Weed community",
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

# Assorted changes to the GL2s
gl2.rename <- function(data) {
  # Original functions
  data %>%
    mutate(
      group_level2 =
        ifelse(rv == "greenhouse gas intensity (N2O/grain)",
               "Nitrogen Emissions",
               group_level2)
    ) %>%
    mutate(
      group_level2 =
        ifelse(group_level2 == "Storage",
               "Carbon Storage",
               group_level2)
    ) %>%
    mutate(group_level2 =
             ifelse(group_level1=="Other Soil Properties" & 
                      (group_level3=="Aggregate size"|group_level3=="Aggregate stability"|
                         group_level3=="Air-filled pore space"|group_level3=="Air-filled pores"|
                         group_level3=="Total pore space"|group_level3=="Water-filled pore space"), 
                    "Soil Structure",
                    group_level2)
    ) %>%
    mutate(
      group_level2 =
        ifelse(group_level2 == "Micronutrients",
               "Micro-nutrients",
               group_level2)
    ) %>%
    mutate(group_level2 = 
             ifelse(group_level1=="Other Soil Properties" & 
                      group_level3=="Decomposition rate of surface residue",
                    "Biotic Factors",
                    group_level2)) %>%
    mutate(group_level2 = 
             ifelse(group_level2=="Crop Quality" & 
                      group_level3=="Maize",
                    "Plant Nutrient Content",
                    group_level2)) %>%
    mutate(group_level2 = 
             ifelse(group_level1=="Other Soil Properties" & group_level3=="Soil organic matter content",
                    "Chemical Properties",
                    group_level2)) %>%
    mutate(group_level2 = 
             ifelse(group_level2=="Crop Biomass",
                    "Crop Growth",
                    group_level2)) %>%
    # New functions
    mutate(
      group_level2 =
        ifelse(group_level3 == "Water infiltration",
               "Physical Properties",
               group_level2
        )
    ) %>%
    mutate(
      group_level2 =
        ifelse(rv == "soil methane oxidation (CH4-C) in in April" | rv == "soil methane oxidation (CH4-C) in in August",
               "Biotic Factors",
               group_level2)
      ) %>%
    mutate(
      group_level2 =
        ifelse(rv == "water extractable organic carbon (0-5 cm)" | rv == "water extractable organic carbon (5-20 cm)" |
                 rv == "active carbon concentration" | rv == "maximum mineralizable soil carbon (Michaelis-Menten equation)" |
               rv == "mineralizable soil carbon" | rv == "natural abundance of 13-C in soil (delta 13-C)" | 
                 rv == "delta 13 C in soil" | rv =="eroded soil organic carbon stock (C3-C) in topsoil",
               "Chemical Properties",
               group_level2)
    ) %>%
    mutate(
      group_level2 =
        ifelse(rv =="eroded soil organic carbon stock (C3-C) in topsoil",
               "Physical Properties",
               group_level2)
    ) %>%
    mutate(
      group_level2 =
        ifelse(group_level2 == "Corn root biomass",
               "Corn root development",
               group_level2
        )
    ) 
}

# Assorted changes to the GL3s
gl3.rename <- function(data) {
  data %>%
    mutate(
      group_level3 =
        ifelse(rv == "greenhouse gas intensity (N2O/grain)",
               "Nitrous oxide",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Ammonia",
               "Volatilized Ammonia",
               group_level3)) %>%
      mutate(
        group_level3 =
          ifelse(group_level2 == "Plant Nutrient Content" & group_level3 == "Nitrogen content",
                 "Crop Nitrogen Content",
                 group_level3)) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Respiration" | group_level3 == "Carbon Dioxide (CO2)" | group_level3 == "Carbon dioxide (CO2)",
               "Carbon dioxide",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Nitrous oxide (N20)" | group_level3 == "Nitrous oxide (N2O)",
               "Nitrous oxide",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(rv == "soil methane oxidation (CH4-C) in in April" | rv == "soil methane oxidation (CH4-C) in in August",
               "Methane oxidation",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(rv == "water extractable organic carbon (0-5 cm)" | rv == "water extractable organic carbon (5-20 cm)" |
                 rv == "active carbon concentration" | rv == "maximum mineralizable soil carbon (Michaelis-Menten equation)" |
               rv == "mineralizable soil carbon",
               "Labile carbon",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(rv == "natural abundance of 13-C in soil (delta 13-C)" | rv == "delta 13 C in soil",
               "Natural abundance of 13C",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(rv == "eroded soil organic carbon stock (C3-C) in topsoil",
               "Erosion",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Soybean Tissue Biomass",
               "Soybean aboveground biomass",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Ammonium (NH4)",
               "Ammonium",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Nitrate (NO3)",
               "Nitrate",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level2 == "Nutrient Runoff" & group_level3 == "Nitrate",
               "Nitrogen",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Nitric oxide (NO)",
               "Nitric oxide",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Air-filled pores",
               "Air-filled pore space",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Temperature",
               "Soil Temperature",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Drainage",
               "Water discharge",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Invertebrate foliar community (#)" | group_level3 == "Invertebrate foliar community (diversity)",
               "Aboveground invertebrates",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Early season (crop stages: preplanting to V6)",
               "Early season",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Mid season (crop stages: V7 to R1)",
               "Mid season",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Late season (crop stages: harvest & post harvest)",
               "Late season",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Western corn rootworm (# days for 50% emergence)",
               "Western corn rootworm",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Amaranthus (#)",
               "Amaranthus",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Bean leaf beetles on soybeans (#)",
               "Bean leaf beetles on soybeans",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Broadleafs (#)",
               "Broadleafs",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Cocklebur (#)",
               "Cocklebur",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Corn (# lodged)",
               "Corn lodging",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Corn borer (#)",
               "Corn borer",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Corn rootworm (#)",
               "Corn rootworm",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Dead nettle (#)",
               "Dead nettle",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Earthworms (#)",
               "Earthworms",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Fall Panicum (#)",
               "Fall panicum",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Foxtail (#)",
               "Foxtail",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "General pests (#)",
               "General pests",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Grasses (#)",
               "Grasses",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Herbivores (#)",
               "Herbivores",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Bacterivores (#)",
               "Bacterivores",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Omnivores (#)",
               "Omnivores",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Fungivores (#)",
               "Fungivores",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Predators (#)",
               "Predators",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Invertebrate non-predator & pests (#)",
               "Invertebrate non-predator & pests",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Invertebrate predator (#)",
               "Invertebrate predators",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Invertebrate predators in soybean (#)",
               "Invertebrate predators in soybean",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Invertebrate soil community (#)",
               "Invertebrate soil community",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Invertebrates (#)",
               "Invertebrates",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Lambsquarters (#)",
               "Lambsquarters",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Natural enemy (#)",
               "Natural enemies",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Nematode community (#)",
               "Nematode community",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Nematodes (#)",
               "Nematodes",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Northern corn rootworm (#)",
               "Northern corn rootworm",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Pigweed (#)",
               "Pigweed",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Seed corn maggot (#)" | group_level3 == "Seedcorn maggot (#)",
               "Seedcorn maggot",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Soybean (# lodged)",
               "Soybean lodging",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Soybean aphid (#)",
               "Soybean aphid",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Soybean cyst nematode (#)",
               "Soybean cyst nematode",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Velvetleaf (#)",
               "Velvetleaf",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Waterhemp (#)",
               "Waterhemp",
               group_level3
        )
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Weed community (#)",
               "Weed community",
               group_level3)
    ) %>%
    mutate(
      group_level3 =
        ifelse(group_level3 == "Western corn rootworm (# days for 50% emergence)" | group_level3 == "Western corn rootworm (#)",
               "Western corn rootworm",
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
        ifelse(group_level3 == "French bean yield loss due to pests" | group_level3 == "Yield loss due to pests",
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
