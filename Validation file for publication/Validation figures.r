# produce validation graph
setwd("/Users/james/Desktop/Emory University - Ph.D./dietaryindex_package/dietaryindex/Validation file for publication/Final validation files")
library(ggplot2)
library(dplyr)
library(readr)    
ACS2020_V1_validation_result = read_csv("ACS2020_V1_validation_result.csv")
ACS2020_V2_validation_result = read_csv("ACS2020_V2_validation_result.csv")
AHEI_validation_result = read_csv("AHEI_validation_result.csv")
AHEIP_validation_result = read_csv("AHEIP_validation_result.csv")
DASH_validation_result = read_csv("DASH_validation_result.csv")
DASHI_validation_result = read_csv("DASHI_validation_result.csv")
DII_validation_result = read_csv("DII_validation_result.csv")
HEI2015_validation_result = read_csv("HEI2015_validation_result.csv")
HEI2020_validation_result = read_csv("HEI2020_validation_result.csv")
MED_validation_result = read_csv("MED_validation_result.csv")
MEDI_validation_result = read_csv("MEDI_validation_result.csv")
MEDI_V2_validation_result = read_csv("MEDI_V2_validation_result.csv")
PHDI_validation_result = read_csv("PHDI_validation_result.csv")

setwd("/Users/james/Desktop/Emory University - Ph.D./dietaryindex_package/dietaryindex/Validation file for publication/HEI2015_NHANES_SAS_1718")
SAS_HEI2015_1718 = read_csv("SAS_HEI2015_1718.csv")
dietaryindex_HEI2015_1718 = read_csv("dietaryindex_HEI2015_1718.csv")

# Define a function to compute accuracy
get_accuracy <- function(x, y) {
  # round x to 2 decimal places
  x <- round(x, 2)
  # round y to 2 decimal places
  y <- round(y, 2)
  return ((sum(x == y, na.rm = TRUE) / length(x)) * 100)
}

# Initialize a data frame to store results for the total dietary index score for all dietary indexes
results <- data.frame(
  Dataset = character(),
  Accuracy = numeric(),
  stringsAsFactors = FALSE
)

# List of all your datasets and their corresponding column names
datasets <- list(
  ACS2020_V1 = list(data = ACS2020_V1_validation_result, cols = c("EXP_ACS_ALL", "ACS2020_V1_ALL")),
  ACS2020_V2 = list(data = ACS2020_V2_validation_result, cols = c("EXP_ACS_ALL", "ACS2020_V2_ALL")),
  AHEI = list(data = AHEI_validation_result, cols = c("EXP_AHEI_ALL", "AHEI_ALL")),
  AHEIP = list(data = AHEIP_validation_result, cols = c("EXP_AHEIP_ALL", "AHEIP_ALL")),
  DASH = list(data = DASH_validation_result, cols = c("EXP_DASH_ALL", "DASH_ALL")),
  DASHI = list(data = DASHI_validation_result, cols = c("EXP_DASHI_ALL", "DASHI_ALL")),
  DII = list(data = DII_validation_result, cols = c("EXP_DII_ALL", "DII_ALL")),
  HEI2015 = list(data = HEI2015_validation_result, cols = c("EXP_HEI2015_ALL", "HEI2015_ALL")),
  HEI2020 = list(data = HEI2020_validation_result, cols = c("EXP_HEI2020_ALL", "HEI2020_ALL")),
  MED = list(data = MED_validation_result, cols = c("EXP_MED_ALL", "MED_ALL")),
  MEDI = list(data = MEDI_validation_result, cols = c("EXP_MEDI_ALL", "MEDI_ALL")),
  MEDI_V2 = list(data = MEDI_V2_validation_result, cols = c("EXP_MEDI_ALL", "MEDI_V2_ALL")),
  PHDI = list(data = PHDI_validation_result, cols = c("EXP_PHDI_ALL", "PHDI_ALL"))
)

# Compute accuracy for each dataset
for (name in names(datasets)) {
  dataset <- datasets[[name]]
  accuracy <- get_accuracy(dataset$data[[dataset$cols[1]]], dataset$data[[dataset$cols[2]]])
  results <- rbind(results, data.frame(Dataset = name, Accuracy = accuracy))
}

# Print results
print(results)


# Plot results
ggplot(results, aes(x = Dataset, y = Accuracy, fill=Dataset)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Accuracy (%)") +
    xlab("Dietary indexes") +
    ggtitle("Comparison of Accuracy: dietaryindex-calculated vs. hand-calculated Dietary Index Values") +
    # increase the title size
    theme(
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20)) 


###### list all the column names
# r$> colnames(ACS2020_V1_validation_result)
#  [1] "id"                       "gender"                   "kcal"                     "vegetable"                "vegetable_unique"         "fruit"                    "fruit_unique"            
#  [8] "whole_grain"              "red_meat"                 "process_food"             "ssb"                      "EXP_ACS_ALL"              "EXP_ACS_VEGETABLE"        "EXP_ACS_VEGETABLE_UNIQUE"
# [15] "EXP_ACS_FRUIT"            "EXP_ACS_FRUIT_UNIQUE"     "EXP_ACS_WHOLE_GRAIN"      "EXP_ACS_RED_MEAT"         "EXP_ACS_PROCESSED_FOOD"   "EXP_ACS_SSB"              "GENDER"                  
# [22] "ACS2020_V1_ALL"           "ACS2020_VEG"              "ACS2020_VEG_ITEMS"        "ACS2020_FRT"              "ACS2020_FRT_ITEMS"        "ACS2020_WGRAIN"           "ACS2020_REDPROC_MEAT"    
# [29] "ACS2020_HPFRG_RATIO"      "ACS2020_SSB_FRTJ"   

# r$> colnames(ACS2020_V2_validation_result)
#  [1] "id"                       "gender"                   "kcal"                     "vegetable"                "vegetable_unique"         "fruit"                    "fruit_unique"            
#  [8] "whole_grain"              "red_meat"                 "process_food"             "ssb"                      "EXP_ACS_ALL"              "EXP_ACS_VEGETABLE"        "EXP_ACS_VEGETABLE_UNIQUE"
# [15] "EXP_ACS_FRUIT"            "EXP_ACS_FRUIT_UNIQUE"     "EXP_ACS_WHOLE_GRAIN"      "EXP_ACS_RED_MEAT"         "EXP_ACS_PROCESSED_FOOD"   "EXP_ACS_SSB"              "GENDER"                  
# [22] "ACS2020_V2_ALL"           "TOTALKCAL_ACS2020"        "ACS2020_VEG"              "ACS2020_VEG_ITEMS"        "ACS2020_FRT"              "ACS2020_FRT_ITEMS"        "ACS2020_WGRAIN"          
# [29] "ACS2020_REDPROC_MEAT"     "ACS2020_HPFRG"            "ACS2020_SSB_FRTJ"  

# r$> colnames(AHEI_validation_result)
#  [1] "id"                    "gender"                "kcal"                  "vegetable"             "fruit"                 "whole_grain"           "nut_legume"            "n3_fat"               
#  [9] "pufa"                  "ssb_fruit_juice"       "red_processed_meat"    "trans_fat"             "sodium"                "alcohol"               "EXP_AHEI_ALL"          "EXP_AHEI_NOETOH"      
# [17] "EXP_AHEI_VEG"          "EXP_AHEI_FRT"          "EXP_AHEI_WGRAIN"       "EXP_AHEI_NUTSLEG"      "EXP_AHEI_N3FAT"        "EXP_AHEI_PUFA"         "EXP_AHEI_SSB_FRTJ"     "EXP_AHEI_REDPROC_MEAT"
# [25] "EXP_AHEI_TRANS"        "EXP_AHEI_SODIUM"       "EXP_AHEI_ALCOHOL"      "GENDER"                "AHEI_ALL"              "AHEI_NOETOH"           "AHEI_VEG"              "AHEI_FRT"             
# [33] "AHEI_WGRAIN"           "AHEI_NUTSLEG"          "AHEI_N3FAT"            "AHEI_PUFA"             "AHEI_SSB_FRTJ"         "AHEI_REDPROC_MEAT"     "AHEI_TRANS"            "AHEI_SODIUM"          
# [41] "AHEI_ALCOHOL"   

# r$> colnames(AHEIP_validation_result)
#  [1] "id"                            "gender"                        "kcal"                          "vegetable"                     "whole_fruit"                   "white_meat_red_meat"          
#  [7] "fiber"                         "trans_fat"                     "poly_fat_sat_fat"              "calcium"                       "folate"                        "iron"                         
# [13] "EXP_AHEIP_ALL"                 "EXP_AHEIP_VEGETABLE"           "EXP_AHEIP_WHOLE_FRUIT"         "EXP_AHEIP_WHITE_MEAT_RED_MEAT" "EXP_AHEIP_FIBER"               "EXP_AHEIP_TRANS_FAT"          
# [19] "EXP_AHEIP_POLY_FAT_SAT_FAT"    "EXP_AHEIP_CALCIUM"             "EXP_AHEIP_FOLATE"              "EXP_AHEIP_IRON"                "AHEIP_ALL"                     "AHEIP_VEG"                    
# [25] "AHEIP_FRT"                     "AHEIP_WHITEREAD"               "AHEIP_FIBER"                   "AHEIP_TRANS"                   "AHEIP_POLYSAT"                 "AHEIP_CALCIUM"                
# [31] "AHEIP_FOLATE"                  "AHEIP_IRON"        

# r$> colnames(DASH_validation_result)
#  [1] "id"                          "gender"                      "kcal"                        "fruit"                       "vegetable"                   "nut_legume"                  "whole_grain"                 "low_fat_dairy"              
#  [9] "sodium"                      "red_processed_meat"          "ssb"                         "EXP_DASH_ALL"                "EXP_DASH_FRUIT"              "EXP_DASH_VEGETABLE"          "EXP_DASH_NUT_LEGUME"         "EXP_DASH_WHOLE_GRAIN"       
# [17] "EXP_DASH_LOW_FAT_DAIRY"      "EXP_DASH_SODIUM"             "EXP_DASH_RED_PROCESSED_MEAT" "EXP_DASH_SSB"                "DASH_ALL"                    "DASH_FRT"                    "DASH_VEG"                    "DASH_NUTSLEG"               
# [25] "DASH_WGRAIN"                 "DASH_LOWF_DAIRY"             "DASH_SODIUM"                 "DASH_REDPROC_MEAT"           "DASH_SSB_FRTJ"    

# r$> colnames(DASHI_validation_result)
#  [1] "id"                           "gender"                       "kcal"                         "vegetable"                    "fruit"                        "nut_legume"                   "low_fat_dairy"                "whole_grain"                 
#  [9] "poultry_fish"                 "red_processed_meat"           "discret_oil_fat"              "snacks_sweets"                "sodium"                       "EXP_DASHI_ALL"                "EXP_DASHI_VEGETABLE"          "EXP_DASHI_FRUIT"             
# [17] "EXP_DASHI_NUT_LEGUME"         "EXP_DASHI_LOW_FAT_DAIRY"      "EXP_DASHI_WHOLE_GRAIN"        "EXP_DASHI_POULTRY_FISH"       "EXP_DASHI_RED_PROCESSED_MEAT" "EXP_DASHI_DISCRET_OIL_FAT"    "EXP_DASHI_SNACKS_SWEETS"      "EXP_DASHI_SODIUM"            
# [25] "DASHI_ALL"                    "DASHI_TOTALKCAL"              "DASHI_VEG"                    "DASHI_FRT"                    "DASHI_NUTSLEG"                "DASHI_LOWFATDAIRY"            "DASHI_WGRAIN"                 "DASHI_WHITEMEAT"             
# [33] "DASHI_REDPROC_MEAT"           "DASHI_FATOIL"                 "DASHI_SNACKS_SWEETS"          "DASHI_SODIUM"       

# r$> colnames(DII_validation_result)
#   [1] "id"                  "Alcohol"             "vitamin B12"         "vitamin B6"          "Beta-carotene"       "Caffeine"            "Carbohydrate"        "Cholesterol"         "Energy"              "Eugenol"             "Total fat"          
#  [12] "Fiber"               "Folic acid"          "Garlic"              "Ginger"              "Iron"                "Magnesium"           "MUFA"                "Niacin"              "n-3 fatty acid"      "n-6 fatty acid"      "Onion"              
#  [23] "Protein"             "PUFA"                "Riboflavin"          "Saffron"             "Saturated fat"       "Selenium"            "Thiamin"             "Trans fat"           "Turmeric"            "Vitamin A"           "Vitamin C"          
#  [34] "Vitamin D"           "Vitamin E"           "Zinc"                "Green/black tea"     "Flavan-3-ol"         "Flavones"            "Flavonols"           "Flavonones"          "Anthocyanidins"      "Isoflavones"         "Pepper"             
#  [45] "Thyme_oregano"       "Rosemary"            "EXP_DII_ALL"         "EXP_ALCOHOL_DII"     "EXP_VITB12_DII"      "EXP_VITB6_DII"       "EXP_BCAROTENE_DII"   "EXP_CAFFEINE_DII"    "EXP_CARB_DII"        "EXP_CHOLES_DII"      "EXP_KCAL_DII"       
#  [56] "EXP_EUGENOL_DII"     "EXP_TOTALFAT_DII"    "EXP_FIBER_DII"       "EXP_FOLICACID_DII"   "EXP_GARLIC_DII"      "EXP_GINGER_DII"      "EXP_IRON_DII"        "EXP_MG_DII"          "EXP_MUFA_DII"        "EXP_NIACIN_DII"      "EXP_N3FAT_DII"      
#  [67] "EXP_N6FAT_DII"       "EXP_ONION_DII"       "EXP_PROTEIN_DII"     "EXP_PUFA_DII"        "EXP_RIBOFLAVIN_DII"  "EXP_SAFFRON_DII"     "EXP_SATFAT_DII"      "EXP_SE_DII"          "EXP_THIAMIN_DII"     "EXP_TRANSFAT_DII"    "EXP_TURMERIC_DII"   
#  [78] "EXP_VITA_DII"        "EXP_VITC_DII"        "EXP_VITD_DII"        "EXP_VITE_DII"        "EXP_ZN_DII"          "EXP_TEA_DII"         "EXP_FLA3OL_DII"      "EXP_FLAVONES_DII"    "EXP_FLAVONOLS_DII"   "EXP_FLAVONONES_DII"  "EXP_ANTHOC_DII"     
#  [89] "EXP_ISOFLAVONES_DII" "EXP_PEPPER_DII"      "EXP_THYME_DII"       "EXP_ROSEMARY_DII"    "DII_ALL"             "DII_NOETOH"          "REPEATNUM"           "ALCOHOL_DII"         "VITB12_DII"          "VITB6_DII"           "BCAROTENE_DII"      
# [100] "CAFFEINE_DII"        "CARB_DII"            "CHOLES_DII"          "KCAL_DII"            "EUGENOL_DII"         "TOTALFAT_DII"        "FIBER_DII"           "FOLICACID_DII"       "GARLIC_DII"          "GINGER_DII"          "IRON_DII"           
# [111] "MG_DII"              "MUFA_DII"            "NIACIN_DII"          "N3FAT_DII"           "N6FAT_DII"           "ONION_DII"           "PROTEIN_DII"         "PUFA_DII"            "RIBOFLAVIN_DII"      "SAFFRON_DII"         "SATFAT_DII"         
# [122] "SE_DII"              "THIAMIN_DII"         "TRANSFAT_DII"        "TURMERIC_DII"        "VITA_DII"            "VITC_DII"            "VITD_DII"            "VITE_DII"            "ZN_DII"              "TEA_DII"             "FLA3OL_DII"         
# [133] "FLAVONES_DII"        "FLAVONOLS_DII"       "FLAVONONES_DII"      "ANTHOC_DII"          "ISOFLAVONES_DII"     "PEPPER_DII"          "THYME_DII"           "ROSEMARY_DII"    

# r$> colnames(HEI2015_validation_result)
#  [1] "id"                       "gender"                   "kcal"                     "total_fruit"              "whole_fruit"              "total_vegetable"          "green_and_bean"           "total_protein"            "seafood_plant_protein"   
# [10] "whole_grain"              "dairy"                    "fatty_acid"               "refined_grain"            "sodium"                   "added_sugar"              "saturated_fat"            "EXP_HEI2015_ALL"          "EXP_HEI2015_TOTALFRT"    
# [19] "EXP_HEI2015_FRT"          "EXP_HEI2015_VEG"          "EXP_HEI2015_GREENNBEAN"   "EXP_HEI2015_TOTALPRO"     "EXP_HEI2015_SEAPLANTPRO"  "EXP_HEI2015_WHOLEGRAIN"   "EXP_HEI2015_DAIRY"        "EXP_HEI2015_FATTYACID"    "EXP_HEI2015_REFINEDGRAIN"
# [28] "EXP_HEI2015_SODIUM"       "EXP_HEI2015_ADDEDSUGAR"   "EXP_HEI2015_SATFAT"       "TOTALKCAL_HEI2015"        "HEI2015_ALL"              "HEI2015_TOTALFRT"         "HEI2015_FRT"              "HEI2015_VEG"              "HEI2015_GREENNBEAN"      
# [37] "HEI2015_TOTALPRO"         "HEI2015_SEAPLANTPRO"      "HEI2015_WHOLEGRAIN"       "HEI2015_DAIRY"            "HEI2015_FATTYACID"        "HEI2015_REFINEDGRAIN"     "HEI2015_SODIUM"           "HEI2015_ADDEDSUGAR"       "HEI2015_SATFAT"   

# r$> colnames(HEI2020_validation_result)
#  [1] "id"                       "gender"                   "age"                      "kcal"                     "total_fruit"              "whole_fruit"              "total_vegetable"          "green_and_bean"           "total_protein"           
# [10] "seafood_plant_protein"    "whole_grain"              "dairy"                    "fatty_acid"               "refined_grain"            "sodium"                   "added_sugar"              "saturated_fat"            "EXP_HEI2020_ALL"         
# [19] "EXP_HEI2020_TOTALFRT"     "EXP_HEI2020_FRT"          "EXP_HEI2020_VEG"          "EXP_HEI2020_GREENNBEAN"   "EXP_HEI2020_TOTALPRO"     "EXP_HEI2020_SEAPLANTPRO"  "EXP_HEI2020_WHOLEGRAIN"   "EXP_HEI2020_DAIRY"        "EXP_HEI2020_FATTYACID"   
# [28] "EXP_HEI2020_REFINEDGRAIN" "EXP_HEI2020_SODIUM"       "EXP_HEI2020_ADDEDSUGAR"   "EXP_HEI2020_SATFAT"       "TOTALKCAL_HEI2020"        "HEI2020_ALL"              "HEI2020_TOTALFRT"         "HEI2020_FRT"              "HEI2020_VEG"             
# [37] "HEI2020_GREENNBEAN"       "HEI2020_TOTALPRO"         "HEI2020_SEAPLANTPRO"      "HEI2020_WHOLEGRAIN"       "HEI2020_DAIRY"            "HEI2020_FATTYACID"        "HEI2020_REFINEDGRAIN"     "HEI2020_SODIUM"           "HEI2020_ADDEDSUGAR"      
# [46] "HEI2020_SATFAT"    

# r$> colnames(MED_validation_result)
#  [1] "id"                         "gender"                     "kcal"                       "fruit"                      "vegetable"                  "whole_grain"                "legume"                     "nut"                       
#  [9] "fish"                       "red_processed_meat"         "monofat_satfat"             "alcohol"                    "EXP_MED_ALL"                "EXP_MED_FRUIT"              "EXP_MED_VEGETABLE"          "EXP_MED_WHOLE_GRAIN"       
# [17] "EXP_MED_LEGUME"             "EXP_MED_NUT"                "EXP_MED_FISH"               "EXP_MED_RED_PROCESSED_MEAT" "EXP_MED_MONOFAT_SATFAT"     "EXP_MED_ALCOHOL"            "MED_ALL"                    "MED_NOETOH"                
# [25] "MED_FRT"                    "MED_VEG"                    "MED_WGRAIN"                 "MED_LEGUMES"                "MED_NUTS"                   "MED_FISH"                   "MED_REDPROC_MEAT"           "MED_MONSATFAT"             
# [33] "MED_ALCOHOL"   

# r$> colnames(MEDI_validation_result)
#  [1] "id"                   "gender"               "kcal"                 "olive_oil"            "vegetable"            "fruit"                "legume"               "nut"                  "fish"                 "alcohol"             
# [11] "ssb"                  "sweets"               "discret_fat"          "red_meat"             "EXP_MEDI_ALL"         "EXP_MEDI_OLIVE_OIL"   "EXP_MEDI_VEGETABLE"   "EXP_MEDI_FRUIT"       "EXP_MEDI_LEGUME"      "EXP_MEDI_NUT"        
# [21] "EXP_MEDI_FISH"        "EXP_MEDI_ALCOHOL"     "EXP_MEDI_SSB"         "EXP_MEDI_SWEETS"      "EXP_MEDI_DISCRET_FAT" "EXP_MEDI_RED_MEAT"    "MEDI_ALL"             "MEDI_NOETOH"          "MEDI_OLIVE_OIL"       "MEDI_FRT"            
# [31] "MEDI_VEG"             "MEDI_LEGUMES"         "MEDI_NUTS"            "MEDI_FISH"            "MEDI_ALCOHOL"         "MEDI_SSB"             "MEDI_SWEETS"          "MEDI_DISCRET_FAT"     "MEDI_REDPROC_MEAT"   

# r$> colnames(MEDI_V2_validation_result)
#  [1] "id"                   "gender"               "kcal"                 "olive_oil"            "vegetable"            "fruit"                "legume"               "nut"                  "fish"                 "alcohol"             
# [11] "ssb"                  "sweets"               "discret_fat"          "red_meat"             "EXP_MEDI_ALL"         "EXP_MEDI_OLIVE_OIL"   "EXP_MEDI_VEGETABLE"   "EXP_MEDI_FRUIT"       "EXP_MEDI_LEGUME"      "EXP_MEDI_NUT"        
# [21] "EXP_MEDI_FISH"        "EXP_MEDI_ALCOHOL"     "EXP_MEDI_SSB"         "EXP_MEDI_SWEETS"      "EXP_MEDI_DISCRET_FAT" "EXP_MEDI_RED_MEAT"    "MEDI_V2_ALL"          "MEDI_V2_NOETOH"       "MEDI_OLIVE_OIL"       "MEDI_FRT"            
# [31] "MEDI_VEG"             "MEDI_LEGUMES"         "MEDI_NUTS"            "MEDI_FISH"            "MEDI_ALCOHOL"         "MEDI_SSB"             "MEDI_SWEETS"          "MEDI_DISCRET_FAT"     "MEDI_REDPROC_MEAT"   

# r$> colnames(PHDI_validation_result)
#  [1] "id"                            "gender"                        "TOTALKCAL_PHDI.x"              "WGRAIN_SERV_PHDI"              "STARCHY_VEG_SERV_PHDI"         "VEG_SERV_PHDI"                 "FRT_SERV_PHDI"                
#  [8] "DAIRY_SERV_PHDI"               "REDPROC_MEAT_SERV_PHDI"        "POULTRY_SERV_PHDI"             "EGG_SERV_PHDI"                 "FISH_SERV_PHDI"                "NUTS_SERV_PHDI"                "LEGUMES_SERV_PHDI"            
# [15] "SOY_SERV_PHDI"                 "ADDED_FAT_UNSAT_SERV_PHDI"     "ADDED_FAT_SAT_TRANS_SERV_PHDI" "ADDED_SUGAR_SERV_PHDI"         "EXP_PHDI_ALL"                  "EXP_PHDI_WGRAIN"               "EXP_PHDI_STARCHY_VEG"         
# [22] "EXP_PHDI_VEG"                  "EXP_PHDI_FRT"                  "EXP_PHDI_DAIRY"                "EXP_PHDI_REDPROC_MEAT"         "EXP_PHDI_POULTRY"              "EXP_PHDI_EGG"                  "EXP_PHDI_FISH"                
# [29] "EXP_PHDI_NUTS"                 "EXP_PHDI_LEGUMES"              "EXP_PHDI_SOY"                  "EXP_PHDI_ADDED_FAT_UNSAT"      "EXP_PHDI_ADDED_FAT_SAT"        "EXP_PHDI_ADDED_SUGAR"          "PHDI_ALL"                     
# [36] "TOTALKCAL_PHDI.y"              "PHDI_WGRAIN"                   "PHDI_VEG"                      "PHDI_FRT"                      "PHDI_DAIRY"                    "PHDI_REDPROC_MEAT"             "PHDI_POULTRY"                 
# [43] "PHDI_EGG"                      "PHDI_FISH"                     "PHDI_NUTS"                     "PHDI_LEGUMES"                  "PHDI_SOY"                      "PHDI_ADDED_FAT_UNSAT"          "PHDI_ADDED_FAT_SAT"           
# [50] "PHDI_ADDED_SUGAR"   

# r$> colnames(SAS_HEI2015_1718)
#  [1] "SEQN"                     "DR1TKCAL"                 "HEI2015C1_TOTALVEG"       "HEI2015C2_GREEN_AND_BEAN" "HEI2015C3_TOTALFRUIT"     "HEI2015C4_WHOLEFRUIT"     "HEI2015C5_WHOLEGRAIN"     "HEI2015C6_TOTALDAIRY"     "HEI2015C7_TOTPROT"       
# [10] "HEI2015C8_SEAPLANT_PROT"  "HEI2015C9_FATTYACID"      "HEI2015C10_SODIUM"        "HEI2015C11_REFINEDGRAIN"  "HEI2015C12_SFAT"          "HEI2015C13_ADDSUG"        "HEI2015_TOTAL_SCORE"     

# r$> colnames(dietaryindex_HEI2015_1718)
#  [1] "SEQN"                 "HEI2015_ALL"          "HEI2015_TOTALFRT"     "HEI2015_FRT"          "HEI2015_VEG"          "HEI2015_GREENNBEAN"   "HEI2015_TOTALPRO"     "HEI2015_SEAPLANTPRO"  "HEI2015_WHOLEGRAIN"   "HEI2015_DAIRY"       
# [11] "HEI2015_FATTYACID"    "HEI2015_REFINEDGRAIN" "HEI2015_SODIUM"       "HEI2015_ADDEDSUGAR"   "HEI2015_SATFAT"    

# Initialize a data frame to store results for all dietary index component scores in ACS2020_V1
results_ACS2020_V1 <- data.frame(
  Dataset = character(),
  Accuracy = numeric(),
  stringsAsFactors = FALSE
)

# List of all your datasets and their corresponding column names
datasets_ACS2020_V1 <- list(
    ACS2020_V1_ALL = list(data = ACS2020_V1_validation_result, cols = c("EXP_ACS_ALL", "ACS2020_V1_ALL")),
    ACS2020_VEG = list(data = ACS2020_V1_validation_result, cols = c("EXP_ACS_VEGETABLE", "ACS2020_VEG")),
    ACS2020_VEG_ITEMS = list(data = ACS2020_V1_validation_result, cols = c("EXP_ACS_VEGETABLE_UNIQUE", "ACS2020_VEG_ITEMS")),
    ACS2020_FRT = list(data = ACS2020_V1_validation_result, cols = c("EXP_ACS_FRUIT", "ACS2020_FRT")),
    ACS2020_FRT_ITEMS = list(data = ACS2020_V1_validation_result, cols = c("EXP_ACS_FRUIT_UNIQUE", "ACS2020_FRT_ITEMS")),
    ACS2020_WGRAIN = list(data = ACS2020_V1_validation_result, cols = c("EXP_ACS_WHOLE_GRAIN", "ACS2020_WGRAIN")),
    ACS2020_REDPROC_MEAT = list(data = ACS2020_V1_validation_result, cols = c("EXP_ACS_RED_MEAT", "ACS2020_REDPROC_MEAT")),
    ACS2020_HPFRG_RATIO = list(data = ACS2020_V1_validation_result, cols = c("EXP_ACS_PROCESSED_FOOD", "ACS2020_HPFRG_RATIO")),
    ACS2020_SSB_FRTJ = list(data = ACS2020_V1_validation_result, cols = c("EXP_ACS_SSB", "ACS2020_SSB_FRTJ"))
)

# Compute accuracy for each dataset
for (name in names(datasets_ACS2020_V1)) {
    dataset <- datasets_ACS2020_V1[[name]]
    accuracy <- get_accuracy(dataset$data[[dataset$cols[1]]], dataset$data[[dataset$cols[2]]])
    results_ACS2020_V1 <- rbind(results_ACS2020_V1, data.frame(Dataset = name, Accuracy = accuracy))
}

# Print results
print(results_ACS2020_V1)

# Plot results
ggplot(results_ACS2020_V1, aes(x = Dataset, y = Accuracy, fill=Dataset)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Accuracy (%)") +
    xlab("ACS2020_V1") +
    ggtitle("Comparison of Accuracy: dietaryindex-calculated vs. hand-calculated Dietary Index Values") +
    # increase the title size
    theme(
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20))

# Initialize a data frame to store results for all dietary index component scores in ACS2020_V2
results_ACS2020_V2 <- data.frame(
  Dataset = character(),
  Accuracy = numeric(),
  stringsAsFactors = FALSE
)

# List of all your datasets and their corresponding column names for ACS2020_V2
datasets_ACS2020_V2 <- list(
    ACS2020_V2_ALL = list(data = ACS2020_V2_validation_result, cols = c("EXP_ACS_ALL", "ACS2020_V2_ALL")),
    ACS2020_VEG = list(data = ACS2020_V2_validation_result, cols = c("EXP_ACS_VEGETABLE", "ACS2020_VEG")),
    ACS2020_VEG_ITEMS = list(data = ACS2020_V2_validation_result, cols = c("EXP_ACS_VEGETABLE_UNIQUE", "ACS2020_VEG_ITEMS")),
    ACS2020_FRT = list(data = ACS2020_V2_validation_result, cols = c("EXP_ACS_FRUIT", "ACS2020_FRT")),
    ACS2020_FRT_ITEMS = list(data = ACS2020_V2_validation_result, cols = c("EXP_ACS_FRUIT_UNIQUE", "ACS2020_FRT_ITEMS")),
    ACS2020_WGRAIN = list(data = ACS2020_V2_validation_result, cols = c("EXP_ACS_WHOLE_GRAIN", "ACS2020_WGRAIN")),
    ACS2020_REDPROC_MEAT = list(data = ACS2020_V2_validation_result, cols = c("EXP_ACS_RED_MEAT", "ACS2020_REDPROC_MEAT")),
    ACS2020_HPFRG = list(data = ACS2020_V2_validation_result, cols = c("EXP_ACS_PROCESSED_FOOD", "ACS2020_HPFRG")),
    ACS2020_SSB_FRTJ = list(data = ACS2020_V2_validation_result, cols = c("EXP_ACS_SSB", "ACS2020_SSB_FRTJ"))
)

# Compute accuracy for each dataset in ACS2020_V2
for (name in names(datasets_ACS2020_V2)) {
    dataset <- datasets_ACS2020_V2[[name]]
    accuracy <- get_accuracy(dataset$data[[dataset$cols[1]]], dataset$data[[dataset$cols[2]]])
    results_ACS2020_V2 <- rbind(results_ACS2020_V2, data.frame(Dataset = name, Accuracy = accuracy))
}

# Print results
print(results_ACS2020_V2)

# Plot results
ggplot(results_ACS2020_V2, aes(x = Dataset, y = Accuracy, fill=Dataset)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Accuracy (%)") +
    xlab("ACS2020_V2") +
    ggtitle("Comparison of Accuracy: dietaryindex-calculated vs. hand-calculated Dietary Index Values") +
    # increase the title size
    theme(
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20))

# Initialize a data frame to store results for all dietary index component scores in AHEI
results_AHEI <- data.frame(
  Dataset = character(),
  Accuracy = numeric(),
  stringsAsFactors = FALSE
)

# List of all your datasets and their corresponding column names for AHEI
datasets_AHEI <- list(
    AHEI_ALL = list(data = AHEI_validation_result, cols = c("EXP_AHEI_ALL", "AHEI_ALL")),
    AHEI_NOETOH = list(data = AHEI_validation_result, cols = c("EXP_AHEI_NOETOH", "AHEI_NOETOH")),
    AHEI_VEG = list(data = AHEI_validation_result, cols = c("EXP_AHEI_VEG", "AHEI_VEG")),
    AHEI_FRT = list(data = AHEI_validation_result, cols = c("EXP_AHEI_FRT", "AHEI_FRT")),
    AHEI_WGRAIN = list(data = AHEI_validation_result, cols = c("EXP_AHEI_WGRAIN", "AHEI_WGRAIN")),
    AHEI_NUTSLEG = list(data = AHEI_validation_result, cols = c("EXP_AHEI_NUTSLEG", "AHEI_NUTSLEG")),
    AHEI_N3FAT = list(data = AHEI_validation_result, cols = c("EXP_AHEI_N3FAT", "AHEI_N3FAT")),
    AHEI_PUFA = list(data = AHEI_validation_result, cols = c("EXP_AHEI_PUFA", "AHEI_PUFA")),
    AHEI_SSB_FRTJ = list(data = AHEI_validation_result, cols = c("EXP_AHEI_SSB_FRTJ", "AHEI_SSB_FRTJ")),
    AHEI_REDPROC_MEAT = list(data = AHEI_validation_result, cols = c("EXP_AHEI_REDPROC_MEAT", "AHEI_REDPROC_MEAT")),
    AHEI_TRANS = list(data = AHEI_validation_result, cols = c("EXP_AHEI_TRANS", "AHEI_TRANS")),
    AHEI_SODIUM = list(data = AHEI_validation_result, cols = c("EXP_AHEI_SODIUM", "AHEI_SODIUM")),
    AHEI_ALCOHOL = list(data = AHEI_validation_result, cols = c("EXP_AHEI_ALCOHOL", "AHEI_ALCOHOL"))
)

# Compute accuracy for each dataset in AHEI
for (name in names(datasets_AHEI)) {
    dataset <- datasets_AHEI[[name]]
    accuracy <- get_accuracy(dataset$data[[dataset$cols[1]]], dataset$data[[dataset$cols[2]]])
    results_AHEI <- rbind(results_AHEI, data.frame(Dataset = name, Accuracy = accuracy))
}

# Print results
print(results_AHEI)

# Plot results
ggplot(results_AHEI, aes(x = Dataset, y = Accuracy, fill=Dataset)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Accuracy (%)") +
    xlab("AHEI") +
    ggtitle("Comparison of Accuracy: dietaryindex-calculated vs. hand-calculated Dietary Index Values") +
    # increase the title size
    theme(
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20))

# Initialize a data frame to store results for all dietary index component scores in AHEIP
results_AHEIP <- data.frame(
  Dataset = character(),
  Accuracy = numeric(),
  stringsAsFactors = FALSE
)

# List of all your datasets and their corresponding column names for AHEIP
datasets_AHEIP <- list(
    AHEIP_ALL = list(data = AHEIP_validation_result, cols = c("EXP_AHEIP_ALL", "AHEIP_ALL")),
    AHEIP_VEGETABLE = list(data = AHEIP_validation_result, cols = c("EXP_AHEIP_VEGETABLE", "AHEIP_VEG")),
    AHEIP_WHOLE_FRUIT = list(data = AHEIP_validation_result, cols = c("EXP_AHEIP_WHOLE_FRUIT", "AHEIP_FRT")),
    AHEIP_WHITE_MEAT_RED_MEAT = list(data = AHEIP_validation_result, cols = c("EXP_AHEIP_WHITE_MEAT_RED_MEAT", "AHEIP_WHITEREAD")),
    AHEIP_FIBER = list(data = AHEIP_validation_result, cols = c("EXP_AHEIP_FIBER", "AHEIP_FIBER")),
    AHEIP_TRANS_FAT = list(data = AHEIP_validation_result, cols = c("EXP_AHEIP_TRANS_FAT", "AHEIP_TRANS")),
    AHEIP_POLY_FAT_SAT_FAT = list(data = AHEIP_validation_result, cols = c("EXP_AHEIP_POLY_FAT_SAT_FAT", "AHEIP_POLYSAT")),
    AHEIP_CALCIUM = list(data = AHEIP_validation_result, cols = c("EXP_AHEIP_CALCIUM", "AHEIP_CALCIUM")),
    AHEIP_FOLATE = list(data = AHEIP_validation_result, cols = c("EXP_AHEIP_FOLATE", "AHEIP_FOLATE")),
    AHEIP_IRON = list(data = AHEIP_validation_result, cols = c("EXP_AHEIP_IRON", "AHEIP_IRON"))
)

# Compute accuracy for each dataset in AHEIP
for (name in names(datasets_AHEIP)) {
    dataset <- datasets_AHEIP[[name]]
    accuracy <- get_accuracy(dataset$data[[dataset$cols[1]]], dataset$data[[dataset$cols[2]]])
    results_AHEIP <- rbind(results_AHEIP, data.frame(Dataset = name, Accuracy = accuracy))
}

# Print results
print(results_AHEIP)

# Plot results
ggplot(results_AHEIP, aes(x = Dataset, y = Accuracy, fill=Dataset)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Accuracy (%)") +
    xlab("AHEIP") +
    ggtitle("Comparison of Accuracy: dietaryindex-calculated vs. hand-calculated Dietary Index Values") +
    # increase the title size
    theme(
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20))

# Initialize a data frame to store results for all dietary index component scores in DASH
results_DASH <- data.frame(
  Dataset = character(),
  Accuracy = numeric(),
  stringsAsFactors = FALSE
)

# List of all your datasets and their corresponding column names for DASH
datasets_DASH <- list(
    DASH_ALL = list(data = DASH_validation_result, cols = c("EXP_DASH_ALL", "DASH_ALL")),
    DASH_FRUIT = list(data = DASH_validation_result, cols = c("EXP_DASH_FRUIT", "DASH_FRT")),
    DASH_VEGETABLE = list(data = DASH_validation_result, cols = c("EXP_DASH_VEGETABLE", "DASH_VEG")),
    DASH_NUT_LEGUME = list(data = DASH_validation_result, cols = c("EXP_DASH_NUT_LEGUME", "DASH_NUTSLEG")),
    DASH_WHOLE_GRAIN = list(data = DASH_validation_result, cols = c("EXP_DASH_WHOLE_GRAIN", "DASH_WGRAIN")),
    DASH_LOW_FAT_DAIRY = list(data = DASH_validation_result, cols = c("EXP_DASH_LOW_FAT_DAIRY", "DASH_LOWF_DAIRY")),
    DASH_SODIUM = list(data = DASH_validation_result, cols = c("EXP_DASH_SODIUM", "DASH_SODIUM")),
    DASH_RED_PROCESSED_MEAT = list(data = DASH_validation_result, cols = c("EXP_DASH_RED_PROCESSED_MEAT", "DASH_REDPROC_MEAT")),
    DASH_SSB = list(data = DASH_validation_result, cols = c("EXP_DASH_SSB", "DASH_SSB_FRTJ"))
)

# Compute accuracy for each dataset in DASH
for (name in names(datasets_DASH)) {
    dataset <- datasets_DASH[[name]]
    accuracy <- get_accuracy(dataset$data[[dataset$cols[1]]], dataset$data[[dataset$cols[2]]])
    results_DASH <- rbind(results_DASH, data.frame(Dataset = name, Accuracy = accuracy))
}

# Print results
print(results_DASH)

# Plot results
ggplot(results_DASH, aes(x = Dataset, y = Accuracy, fill=Dataset)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Accuracy (%)") +
    xlab("DASH") +
    ggtitle("Comparison of Accuracy: dietaryindex-calculated vs. hand-calculated Dietary Index Values") +
    # increase the title size
    theme(
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20))

# Initialize a data frame to store results for all dietary index component scores in DASHI
results_DASHI <- data.frame(
  Dataset = character(),
  Accuracy = numeric(),
  stringsAsFactors = FALSE
)

# List of all your datasets and their corresponding column names for DASHI
datasets_DASHI <- list(
    DASHI_ALL = list(data = DASHI_validation_result, cols = c("EXP_DASHI_ALL", "DASHI_ALL")),
    DASHI_VEGETABLE = list(data = DASHI_validation_result, cols = c("EXP_DASHI_VEGETABLE", "DASHI_VEG")),
    DASHI_FRUIT = list(data = DASHI_validation_result, cols = c("EXP_DASHI_FRUIT", "DASHI_FRT")),
    DASHI_NUT_LEGUME = list(data = DASHI_validation_result, cols = c("EXP_DASHI_NUT_LEGUME", "DASHI_NUTSLEG")),
    DASHI_LOW_FAT_DAIRY = list(data = DASHI_validation_result, cols = c("EXP_DASHI_LOW_FAT_DAIRY", "DASHI_LOWFATDAIRY")),
    DASHI_WHOLE_GRAIN = list(data = DASHI_validation_result, cols = c("EXP_DASHI_WHOLE_GRAIN", "DASHI_WGRAIN")),
    DASHI_POULTRY_FISH = list(data = DASHI_validation_result, cols = c("EXP_DASHI_POULTRY_FISH", "DASHI_WHITEMEAT")),
    DASHI_RED_PROCESSED_MEAT = list(data = DASHI_validation_result, cols = c("EXP_DASHI_RED_PROCESSED_MEAT", "DASHI_REDPROC_MEAT")),
    DASHI_DISCRET_OIL_FAT = list(data = DASHI_validation_result, cols = c("EXP_DASHI_DISCRET_OIL_FAT", "DASHI_FATOIL")),
    DASHI_SNACKS_SWEETS = list(data = DASHI_validation_result, cols = c("EXP_DASHI_SNACKS_SWEETS", "DASHI_SNACKS_SWEETS")),
    DASHI_SODIUM = list(data = DASHI_validation_result, cols = c("EXP_DASHI_SODIUM", "DASHI_SODIUM"))
)

# Compute accuracy for each dataset in DASHI
for (name in names(datasets_DASHI)) {
    dataset <- datasets_DASHI[[name]]
    accuracy <- get_accuracy(dataset$data[[dataset$cols[1]]], dataset$data[[dataset$cols[2]]])
    results_DASHI <- rbind(results_DASHI, data.frame(Dataset = name, Accuracy = accuracy))
}

# Print results
print(results_DASHI)

# Plot results
ggplot(results_DASHI, aes(x = Dataset, y = Accuracy, fill=Dataset)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Accuracy (%)") +
    xlab("DASHI") +
    ggtitle("Comparison of Accuracy: dietaryindex-calculated vs. hand-calculated Dietary Index Values") +
    # increase the title size
    theme(
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20))

# Initialize a data frame to store results for all dietary index component scores in DII
results_DII <- data.frame(
  Dataset = character(),
  Accuracy = numeric(),
  stringsAsFactors = FALSE
)

# List of all your datasets and their corresponding column names for DII
datasets_DII <- list(
    DII_ALL = list(data = DII_validation_result, cols = c("EXP_DII_ALL", "DII_ALL")),
    ALCOHOL_DII = list(data = DII_validation_result, cols = c("EXP_ALCOHOL_DII", "ALCOHOL_DII")),
    VITB12_DII = list(data = DII_validation_result, cols = c("EXP_VITB12_DII", "VITB12_DII")),
    VITB6_DII = list(data = DII_validation_result, cols = c("EXP_VITB6_DII", "VITB6_DII")),
    BCAROTENE_DII = list(data = DII_validation_result, cols = c("EXP_BCAROTENE_DII", "BCAROTENE_DII")),
    CAFFEINE_DII = list(data = DII_validation_result, cols = c("EXP_CAFFEINE_DII", "CAFFEINE_DII")),
    CARB_DII = list(data = DII_validation_result, cols = c("EXP_CARB_DII", "CARB_DII")),
    CHOLESTEROL_DII = list(data = DII_validation_result, cols = c("EXP_CHOLES_DII", "CHOLES_DII")),
    TOTALFAT_DII = list(data = DII_validation_result, cols = c("EXP_TOTALFAT_DII", "TOTALFAT_DII")),
    FIBER_DII = list(data = DII_validation_result, cols = c("EXP_FIBER_DII", "FIBER_DII")),
    FOLICACID_DII = list(data = DII_validation_result, cols = c("EXP_FOLICACID_DII", "FOLICACID_DII")),
    GARLIC_DII = list(data = DII_validation_result, cols = c("EXP_GARLIC_DII", "GARLIC_DII")),
    GINGER_DII = list(data = DII_validation_result, cols = c("EXP_GINGER_DII", "GINGER_DII")),
    IRON_DII = list(data = DII_validation_result, cols = c("EXP_IRON_DII", "IRON_DII")),
    MAGNESIUM_DII = list(data = DII_validation_result, cols = c("EXP_MG_DII", "MG_DII")),
    MUFA_DII = list(data = DII_validation_result, cols = c("EXP_MUFA_DII", "MUFA_DII")),
    NIACIN_DII = list(data = DII_validation_result, cols = c("EXP_NIACIN_DII", "NIACIN_DII")),
    N3FAT_DII = list(data = DII_validation_result, cols = c("EXP_N3FAT_DII", "N3FAT_DII")),
    N6FAT_DII = list(data = DII_validation_result, cols = c("EXP_N6FAT_DII", "N6FAT_DII")),
    ONION_DII = list(data = DII_validation_result, cols = c("EXP_ONION_DII", "ONION_DII")),
    PROTEIN_DII = list(data = DII_validation_result, cols = c("EXP_PROTEIN_DII", "PROTEIN_DII")),
    PUFA_DII = list(data = DII_validation_result, cols = c("EXP_PUFA_DII", "PUFA_DII")),
    RIBOFLAVIN_DII = list(data = DII_validation_result, cols = c("EXP_RIBOFLAVIN_DII", "RIBOFLAVIN_DII")),
    SAFFRON_DII = list(data = DII_validation_result, cols = c("EXP_SAFFRON_DII", "SAFFRON_DII")),
    SATFAT_DII = list(data = DII_validation_result, cols = c("EXP_SATFAT_DII", "SATFAT_DII")),
    SELENIUM_DII = list(data = DII_validation_result, cols = c("EXP_SE_DII", "SE_DII")),
    THIAMIN_DII = list(data = DII_validation_result, cols = c("EXP_THIAMIN_DII", "THIAMIN_DII")),
    TRANSFAT_DII = list(data = DII_validation_result, cols = c("EXP_TRANSFAT_DII", "TRANSFAT_DII")),
    TURMERIC_DII = list(data = DII_validation_result, cols = c("EXP_TURMERIC_DII", "TURMERIC_DII")),
    VITAMINA_DII = list(data = DII_validation_result, cols = c("EXP_VITA_DII", "VITA_DII")),
    VITAMINC_DII = list(data = DII_validation_result, cols = c("EXP_VITC_DII", "VITC_DII")),
    VITAMIND_DII = list(data = DII_validation_result, cols = c("EXP_VITD_DII", "VITD_DII")),
    VITAMINE_DII = list(data = DII_validation_result, cols = c("EXP_VITE_DII", "VITE_DII")),
    ZINC_DII = list(data = DII_validation_result, cols = c("EXP_ZN_DII", "ZN_DII")),
    TEA_DII = list(data = DII_validation_result, cols = c("EXP_TEA_DII", "TEA_DII")),
    FLA3OL_DII = list(data = DII_validation_result, cols = c("EXP_FLA3OL_DII", "FLA3OL_DII")),
    FLAVONES_DII = list(data = DII_validation_result, cols = c("EXP_FLAVONES_DII", "FLAVONES_DII")),
    FLAVONOLS_DII = list(data = DII_validation_result, cols = c("EXP_FLAVONOLS_DII", "FLAVONOLS_DII")),
    FLAVONONES_DII = list(data = DII_validation_result, cols = c("EXP_FLAVONONES_DII", "FLAVONONES_DII")),
    ANTHOCYANIDINS_DII = list(data = DII_validation_result, cols = c("EXP_ANTHOC_DII", "ANTHOC_DII")),
    ISOFLAVONES_DII = list(data = DII_validation_result, cols = c("EXP_ISOFLAVONES_DII", "ISOFLAVONES_DII")),
    PEPPER_DII = list(data = DII_validation_result, cols = c("EXP_PEPPER_DII", "PEPPER_DII")),
    THYME_DII = list(data = DII_validation_result, cols = c("EXP_THYME_DII", "THYME_DII")),
    ROSEMARY_DII = list(data = DII_validation_result, cols = c("EXP_ROSEMARY_DII", "ROSEMARY_DII"))
)

# Compute accuracy for each dataset in DII
for (name in names(datasets_DII)) {
    dataset <- datasets_DII[[name]]
    accuracy <- get_accuracy(dataset$data[[dataset$cols[1]]], dataset$data[[dataset$cols[2]]])
    results_DII <- rbind(results_DII, data.frame(Dataset = name, Accuracy = accuracy))
}

# Print results
print(results_DII)

# Plot results
ggplot(results_DII, aes(x = Dataset, y = Accuracy, fill=Dataset)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Accuracy (%)") +
    xlab("DII") +
    ggtitle("Comparison of Accuracy: dietaryindex-calculated vs. hand-calculated Dietary Index Values") +
    # increase the title size
    theme(
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20))

# Initialize a data frame to store results for all dietary index component scores in HEI2015
results_HEI2015 <- data.frame(
  Dataset = character(),
  Accuracy = numeric(),
  stringsAsFactors = FALSE
)

# List of all your datasets and their corresponding column names for HEI2015
datasets_HEI2015 <- list(
    HEI2015_ALL = list(data = HEI2015_validation_result, cols = c("EXP_HEI2015_ALL", "HEI2015_ALL")),
    HEI2015_TOTALFRT = list(data = HEI2015_validation_result, cols = c("EXP_HEI2015_TOTALFRT", "HEI2015_TOTALFRT")),
    HEI2015_FRT = list(data = HEI2015_validation_result, cols = c("EXP_HEI2015_FRT", "HEI2015_FRT")),
    HEI2015_VEG = list(data = HEI2015_validation_result, cols = c("EXP_HEI2015_VEG", "HEI2015_VEG")),
    HEI2015_GREENNBEAN = list(data = HEI2015_validation_result, cols = c("EXP_HEI2015_GREENNBEAN", "HEI2015_GREENNBEAN")),
    HEI2015_TOTALPRO = list(data = HEI2015_validation_result, cols = c("EXP_HEI2015_TOTALPRO", "HEI2015_TOTALPRO")),
    HEI2015_SEAPLANTPRO = list(data = HEI2015_validation_result, cols = c("EXP_HEI2015_SEAPLANTPRO", "HEI2015_SEAPLANTPRO")),
    HEI2015_WHOLEGRAIN = list(data = HEI2015_validation_result, cols = c("EXP_HEI2015_WHOLEGRAIN", "HEI2015_WHOLEGRAIN")),
    HEI2015_DAIRY = list(data = HEI2015_validation_result, cols = c("EXP_HEI2015_DAIRY", "HEI2015_DAIRY")),
    HEI2015_FATTYACID = list(data = HEI2015_validation_result, cols = c("EXP_HEI2015_FATTYACID", "HEI2015_FATTYACID")),
    HEI2015_REFINEDGRAIN = list(data = HEI2015_validation_result, cols = c("EXP_HEI2015_REFINEDGRAIN", "HEI2015_REFINEDGRAIN")),
    HEI2015_SODIUM = list(data = HEI2015_validation_result, cols = c("EXP_HEI2015_SODIUM", "HEI2015_SODIUM")),
    HEI2015_ADDEDSUGAR = list(data = HEI2015_validation_result, cols = c("EXP_HEI2015_ADDEDSUGAR", "HEI2015_ADDEDSUGAR")),
    HEI2015_SATFAT = list(data = HEI2015_validation_result, cols = c("EXP_HEI2015_SATFAT", "HEI2015_SATFAT"))
)

# Compute accuracy for each dataset in HEI2015
for (name in names(datasets_HEI2015)) {
    dataset <- datasets_HEI2015[[name]]
    accuracy <- get_accuracy(dataset$data[[dataset$cols[1]]], dataset$data[[dataset$cols[2]]])
    results_HEI2015 <- rbind(results_HEI2015, data.frame(Dataset = name, Accuracy = accuracy))
}

# Print results
print(results_HEI2015)

# Plot results
ggplot(results_HEI2015, aes(x = Dataset, y = Accuracy, fill=Dataset)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Accuracy (%)") +
    xlab("HEI2015") +
    ggtitle("Comparison of Accuracy: dietaryindex-calculated vs. hand-calculated Dietary Index Values") +
    # increase the title size
    theme(
        plot.title = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 15))
    

# Initialize a data frame to store results for all HEI2020 component scores
results_HEI2020 <- data.frame(
  Dataset = character(),
  Accuracy = numeric(),
  stringsAsFactors = FALSE
)

# List of all the datasets and their corresponding column names for HEI2020
datasets_HEI2020 <- list(
  HEI2020_ALL = list(data = HEI2020_validation_result, cols = c("EXP_HEI2020_ALL", "HEI2020_ALL")),
  HEI2020_TOTALFRT = list(data = HEI2020_validation_result, cols = c("EXP_HEI2020_TOTALFRT", "HEI2020_TOTALFRT")),
  HEI2020_FRT = list(data = HEI2020_validation_result, cols = c("EXP_HEI2020_FRT", "HEI2020_FRT")),
  HEI2020_VEG = list(data = HEI2020_validation_result, cols = c("EXP_HEI2020_VEG", "HEI2020_VEG")),
  HEI2020_GREENNBEAN = list(data = HEI2020_validation_result, cols = c("EXP_HEI2020_GREENNBEAN", "HEI2020_GREENNBEAN")),
  HEI2020_TOTALPRO = list(data = HEI2020_validation_result, cols = c("EXP_HEI2020_TOTALPRO", "HEI2020_TOTALPRO")),
  HEI2020_SEAPLANTPRO = list(data = HEI2020_validation_result, cols = c("EXP_HEI2020_SEAPLANTPRO", "HEI2020_SEAPLANTPRO")),
  HEI2020_WHOLEGRAIN = list(data = HEI2020_validation_result, cols = c("EXP_HEI2020_WHOLEGRAIN", "HEI2020_WHOLEGRAIN")),
  HEI2020_DAIRY = list(data = HEI2020_validation_result, cols = c("EXP_HEI2020_DAIRY", "HEI2020_DAIRY")),
  HEI2020_FATTYACID = list(data = HEI2020_validation_result, cols = c("EXP_HEI2020_FATTYACID", "HEI2020_FATTYACID")),
  HEI2020_REFINEDGRAIN = list(data = HEI2020_validation_result, cols = c("EXP_HEI2020_REFINEDGRAIN", "HEI2020_REFINEDGRAIN")),
  HEI2020_SODIUM = list(data = HEI2020_validation_result, cols = c("EXP_HEI2020_SODIUM", "HEI2020_SODIUM")),
  HEI2020_ADDEDSUGAR = list(data = HEI2020_validation_result, cols = c("EXP_HEI2020_ADDEDSUGAR", "HEI2020_ADDEDSUGAR")),
  HEI2020_SATFAT = list(data = HEI2020_validation_result, cols = c("EXP_HEI2020_SATFAT", "HEI2020_SATFAT"))
)

# Compute accuracy for each dataset in HEI2020
for (name in names(datasets_HEI2020)) {
  dataset <- datasets_HEI2020[[name]]
  accuracy <- get_accuracy(dataset$data[[dataset$cols[1]]], dataset$data[[dataset$cols[2]]])
  results_HEI2020 <- rbind(results_HEI2020, data.frame(Dataset = name, Accuracy = accuracy))
}

# Print results
print(results_HEI2020)

# Plot results
ggplot(results_HEI2020, aes(x = Dataset, y = Accuracy, fill = Dataset)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Accuracy (%)") +
  xlab("HEI2020") +
  ggtitle("Comparison of Accuracy: dietaryindex-calculated vs. hand-calculated Dietary Index Values") +
  # increase the title size
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20)
  )

# Initialize a data frame to store results for all MED component scores
results_MED <- data.frame(
  Dataset = character(),
  Accuracy = numeric(),
  stringsAsFactors = FALSE
)

# List of all the datasets and their corresponding column names for MED
datasets_MED <- list(
  MED_ALL = list(data = MED_validation_result, cols = c("EXP_MED_ALL", "MED_ALL")),
  MED_FRUIT = list(data = MED_validation_result, cols = c("EXP_MED_FRUIT", "MED_FRT")),
  MED_VEGETABLE = list(data = MED_validation_result, cols = c("EXP_MED_VEGETABLE", "MED_VEG")),
  MED_WHOLE_GRAIN = list(data = MED_validation_result, cols = c("EXP_MED_WHOLE_GRAIN", "MED_WGRAIN")),
  MED_LEGUME = list(data = MED_validation_result, cols = c("EXP_MED_LEGUME", "MED_LEGUMES")),
  MED_NUT = list(data = MED_validation_result, cols = c("EXP_MED_NUT", "MED_NUTS")),
  MED_FISH = list(data = MED_validation_result, cols = c("EXP_MED_FISH", "MED_FISH")),
  MED_RED_PROCESSED_MEAT = list(data = MED_validation_result, cols = c("EXP_MED_RED_PROCESSED_MEAT", "MED_REDPROC_MEAT")),
  MED_MONOFAT_SATFAT = list(data = MED_validation_result, cols = c("EXP_MED_MONOFAT_SATFAT", "MED_MONSATFAT")),
  MED_ALCOHOL = list(data = MED_validation_result, cols = c("EXP_MED_ALCOHOL", "MED_ALCOHOL"))
)

# Compute accuracy for each dataset in MED
for (name in names(datasets_MED)) {
  dataset <- datasets_MED[[name]]
  accuracy <- get_accuracy(dataset$data[[dataset$cols[1]]], dataset$data[[dataset$cols[2]]])
  results_MED <- rbind(results_MED, data.frame(Dataset = name, Accuracy = accuracy))
}

# Print results
print(results_MED)

# Plot results
ggplot(results_MED, aes(x = Dataset, y = Accuracy, fill = Dataset)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Accuracy (%)") +
  xlab("MED") +
  ggtitle("Comparison of Accuracy: dietaryindex-calculated vs. hand-calculated Dietary Index Values") +
  # increase the title size
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20)
  )

# Initialize a data frame to store results for all MEDI component scores
results_MEDI <- data.frame(
  Dataset = character(),
  Accuracy = numeric(),
  stringsAsFactors = FALSE
)

# List of all the datasets and their corresponding column names for MEDI
datasets_MEDI <- list(
  MEDI_ALL = list(data = MEDI_validation_result, cols = c("EXP_MEDI_ALL", "MEDI_ALL")),
  MEDI_OLIVE_OIL = list(data = MEDI_validation_result, cols = c("EXP_MEDI_OLIVE_OIL", "MEDI_OLIVE_OIL")),
  MEDI_VEGETABLE = list(data = MEDI_validation_result, cols = c("EXP_MEDI_VEGETABLE", "MEDI_VEG")),
  MEDI_FRUIT = list(data = MEDI_validation_result, cols = c("EXP_MEDI_FRUIT", "MEDI_FRT")),
  MEDI_LEGUME = list(data = MEDI_validation_result, cols = c("EXP_MEDI_LEGUME", "MEDI_LEGUMES")),
  MEDI_NUT = list(data = MEDI_validation_result, cols = c("EXP_MEDI_NUT", "MEDI_NUTS")),
  MEDI_FISH = list(data = MEDI_validation_result, cols = c("EXP_MEDI_FISH", "MEDI_FISH")),
  MEDI_ALCOHOL = list(data = MEDI_validation_result, cols = c("EXP_MEDI_ALCOHOL", "MEDI_ALCOHOL")),
  MEDI_SSB = list(data = MEDI_validation_result, cols = c("EXP_MEDI_SSB", "MEDI_SSB")),
  MEDI_SWEETS = list(data = MEDI_validation_result, cols = c("EXP_MEDI_SWEETS", "MEDI_SWEETS")),
  MEDI_DISCRET_FAT = list(data = MEDI_validation_result, cols = c("EXP_MEDI_DISCRET_FAT", "MEDI_DISCRET_FAT")),
  MEDI_RED_MEAT = list(data = MEDI_validation_result, cols = c("EXP_MEDI_RED_MEAT", "MEDI_REDPROC_MEAT"))
)

# Compute accuracy for each dataset in MEDI
for (name in names(datasets_MEDI)) {
  dataset <- datasets_MEDI[[name]]
  accuracy <- get_accuracy(dataset$data[[dataset$cols[1]]], dataset$data[[dataset$cols[2]]])
  results_MEDI <- rbind(results_MEDI, data.frame(Dataset = name, Accuracy = accuracy))
}

# Print results
print(results_MEDI)

# Plot results
ggplot(results_MEDI, aes(x = Dataset, y = Accuracy, fill = Dataset)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Accuracy (%)") +
  xlab("MEDI") +
  ggtitle("Comparison of Accuracy: dietaryindex-calculated vs. hand-calculated Dietary Index Values") +
  # increase the title size
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20)
  )

# Initialize a data frame to store results for all MEDI_V2 component scores
results_MEDI_V2 <- data.frame(
  Dataset = character(),
  Accuracy = numeric(),
  stringsAsFactors = FALSE
)

# List of all the datasets and their corresponding column names for MEDI_V2
datasets_MEDI_V2 <- list(
  MEDI_V2_ALL = list(data = MEDI_V2_validation_result, cols = c("EXP_MEDI_ALL", "MEDI_V2_ALL")),
  MEDI_V2_OLIVE_OIL = list(data = MEDI_V2_validation_result, cols = c("EXP_MEDI_OLIVE_OIL", "MEDI_OLIVE_OIL")),
  MEDI_V2_VEGETABLE = list(data = MEDI_V2_validation_result, cols = c("EXP_MEDI_VEGETABLE", "MEDI_VEG")),
  MEDI_V2_FRUIT = list(data = MEDI_V2_validation_result, cols = c("EXP_MEDI_FRUIT", "MEDI_FRT")),
  MEDI_V2_LEGUME = list(data = MEDI_V2_validation_result, cols = c("EXP_MEDI_LEGUME", "MEDI_LEGUMES")),
  MEDI_V2_NUT = list(data = MEDI_V2_validation_result, cols = c("EXP_MEDI_NUT", "MEDI_NUTS")),
  MEDI_V2_FISH = list(data = MEDI_V2_validation_result, cols = c("EXP_MEDI_FISH", "MEDI_FISH")),
  MEDI_V2_ALCOHOL = list(data = MEDI_V2_validation_result, cols = c("EXP_MEDI_ALCOHOL", "MEDI_ALCOHOL")),
  MEDI_V2_SSB = list(data = MEDI_V2_validation_result, cols = c("EXP_MEDI_SSB", "MEDI_SSB")),
  MEDI_V2_SWEETS = list(data = MEDI_V2_validation_result, cols = c("EXP_MEDI_SWEETS", "MEDI_SWEETS")),
  MEDI_V2_DISCRET_FAT = list(data = MEDI_V2_validation_result, cols = c("EXP_MEDI_DISCRET_FAT", "MEDI_DISCRET_FAT")),
  MEDI_V2_RED_MEAT = list(data = MEDI_V2_validation_result, cols = c("EXP_MEDI_RED_MEAT", "MEDI_REDPROC_MEAT"))
)

# Compute accuracy for each dataset in MEDI_V2
for (name in names(datasets_MEDI_V2)) {
  dataset <- datasets_MEDI_V2[[name]]
  accuracy <- get_accuracy(dataset$data[[dataset$cols[1]]], dataset$data[[dataset$cols[2]]])
  results_MEDI_V2 <- rbind(results_MEDI_V2, data.frame(Dataset = name, Accuracy = accuracy))
}

# Print results
print(results_MEDI_V2)

# Plot results
ggplot(results_MEDI_V2, aes(x = Dataset, y = Accuracy, fill = Dataset)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Accuracy (%)") +
  xlab("MEDI_V2") +
  ggtitle("Comparison of Accuracy: dietaryindex-calculated vs. hand-calculated Dietary Index Values") +
  # increase the title size
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20)
  )

# Initialize a data frame to store results for all PHDI component scores
results_PHDI <- data.frame(
  Dataset = character(),
  Accuracy = numeric(),
  stringsAsFactors = FALSE
)

# List of all the datasets and their corresponding column names for PHDI
datasets_PHDI <- list(
  PHDI_ALL = list(data = PHDI_validation_result, cols = c("EXP_PHDI_ALL", "PHDI_ALL")),
  PHDI_WGRAIN = list(data = PHDI_validation_result, cols = c("EXP_PHDI_WGRAIN", "PHDI_WGRAIN")),
  PHDI_STARCHY_VEG = list(data = PHDI_validation_result, cols = c("EXP_PHDI_STARCHY_VEG", "PHDI_STARCHY_VEG")),
  PHDI_VEG = list(data = PHDI_validation_result, cols = c("EXP_PHDI_VEG", "PHDI_VEG")),
  PHDI_FRT = list(data = PHDI_validation_result, cols = c("EXP_PHDI_FRT", "PHDI_FRT")),
  PHDI_DAIRY = list(data = PHDI_validation_result, cols = c("EXP_PHDI_DAIRY", "PHDI_DAIRY")),
  PHDI_REDPROC_MEAT = list(data = PHDI_validation_result, cols = c("EXP_PHDI_REDPROC_MEAT", "PHDI_REDPROC_MEAT")),
  PHDI_POULTRY = list(data = PHDI_validation_result, cols = c("EXP_PHDI_POULTRY", "PHDI_POULTRY")),
  PHDI_EGG = list(data = PHDI_validation_result, cols = c("EXP_PHDI_EGG", "PHDI_EGG")),
  PHDI_FISH = list(data = PHDI_validation_result, cols = c("EXP_PHDI_FISH", "PHDI_FISH")),
  PHDI_NUTS = list(data = PHDI_validation_result, cols = c("EXP_PHDI_NUTS", "PHDI_NUTS")),
  PHDI_LEGUMES = list(data = PHDI_validation_result, cols = c("EXP_PHDI_LEGUMES", "PHDI_LEGUMES")),
  PHDI_SOY = list(data = PHDI_validation_result, cols = c("EXP_PHDI_SOY", "PHDI_SOY")),
  PHDI_ADDED_FAT_UNSAT = list(data = PHDI_validation_result, cols = c("EXP_PHDI_ADDED_FAT_UNSAT", "PHDI_ADDED_FAT_UNSAT")),
  PHDI_ADDED_FAT_SAT = list(data = PHDI_validation_result, cols = c("EXP_PHDI_ADDED_FAT_SAT", "PHDI_ADDED_FAT_SAT")),
  PHDI_ADDED_SUGAR = list(data = PHDI_validation_result, cols = c("EXP_PHDI_ADDED_SUGAR", "PHDI_ADDED_SUGAR"))
)

# Compute accuracy for each dataset in PHDI
for (name in names(datasets_PHDI)) {
  dataset <- datasets_PHDI[[name]]
  accuracy <- get_accuracy(dataset$data[[dataset$cols[1]]], dataset$data[[dataset$cols[2]]])
  results_PHDI <- rbind(results_PHDI, data.frame(Dataset = name, Accuracy = accuracy))
}

# Print results
print(results_PHDI)

# Plot results
ggplot(results_PHDI, aes(x = Dataset, y = Accuracy, fill = Dataset)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Accuracy (%)") +
  xlab("PHDI") +
  ggtitle("Comparison of Accuracy: dietaryindex-calculated vs. hand-calculated Dietary Index Values") +
  # increase the title size
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20)
  )

# Initialize a data frame to store results for all HEI2015_1718 component scores
results_HEI2015_1718 <- data.frame(
  Dataset = character(),
  Accuracy = numeric(),
  stringsAsFactors = FALSE
)

# Create a list with SAS_HEI2015_1718 and dietaryindex_HEI2015_1718
datasets_HEI2015_1718 <- list(
    HEI2015_ALL = list(data1 = SAS_HEI2015_1718, data2 = dietaryindex_HEI2015_1718, cols = c("HEI2015_TOTAL_SCORE", "HEI2015_ALL")),
    HEI2015_TOTALFRT = list(data1 = SAS_HEI2015_1718, data2 = dietaryindex_HEI2015_1718, cols = c("HEI2015C3_TOTALFRUIT", "HEI2015_TOTALFRT")),
    HEI2015_FRT = list(data1 = SAS_HEI2015_1718, data2 = dietaryindex_HEI2015_1718, cols = c("HEI2015C4_WHOLEFRUIT", "HEI2015_FRT")),
    HEI2015_VEG = list(data1 = SAS_HEI2015_1718, data2 = dietaryindex_HEI2015_1718, cols = c("HEI2015C1_TOTALVEG", "HEI2015_VEG")),
    HEI2015_GREENNBEAN = list(data1 = SAS_HEI2015_1718, data2 = dietaryindex_HEI2015_1718, cols = c("HEI2015C2_GREEN_AND_BEAN", "HEI2015_GREENNBEAN")),
    HEI2015_TOTALPRO = list(data1 = SAS_HEI2015_1718, data2 = dietaryindex_HEI2015_1718, cols = c("HEI2015C7_TOTPROT", "HEI2015_TOTALPRO")),
    HEI2015_SEAPLANTPRO = list(data1 = SAS_HEI2015_1718, data2 = dietaryindex_HEI2015_1718, cols = c("HEI2015C8_SEAPLANT_PROT", "HEI2015_SEAPLANTPRO")),
    HEI2015_WHOLEGRAIN = list(data1 = SAS_HEI2015_1718, data2 = dietaryindex_HEI2015_1718, cols = c("HEI2015C5_WHOLEGRAIN", "HEI2015_WHOLEGRAIN")),
    HEI2015_DAIRY = list(data1 = SAS_HEI2015_1718, data2 = dietaryindex_HEI2015_1718, cols = c("HEI2015C6_TOTALDAIRY", "HEI2015_DAIRY")),
    HEI2015_FATTYACID = list(data1 = SAS_HEI2015_1718, data2 = dietaryindex_HEI2015_1718, cols = c("HEI2015C9_FATTYACID", "HEI2015_FATTYACID")),
    HEI2015_REFINEDGRAIN = list(data1 = SAS_HEI2015_1718, data2 = dietaryindex_HEI2015_1718, cols = c("HEI2015C11_REFINEDGRAIN", "HEI2015_REFINEDGRAIN")),
    HEI2015_SODIUM = list(data1 = SAS_HEI2015_1718, data2 = dietaryindex_HEI2015_1718, cols = c("HEI2015C10_SODIUM", "HEI2015_SODIUM")),
    HEI2015_ADDEDSUGAR = list(data1 = SAS_HEI2015_1718, data2 = dietaryindex_HEI2015_1718, cols = c("HEI2015C13_ADDSUG", "HEI2015_ADDEDSUGAR")),
    HEI2015_SATFAT = list(data1 = SAS_HEI2015_1718, data2 = dietaryindex_HEI2015_1718, cols = c("HEI2015C12_SFAT", "HEI2015_SATFAT"))
    )

# Compute accuracy for each dataset in PHDI
for (name in names(datasets_HEI2015_1718)) {
  dataset <- datasets_HEI2015_1718[[name]]
  data1_values <- as.numeric(dataset$data1[[dataset$cols[1]]])  # Convert to numeric
  data2_values <- as.numeric(dataset$data2[[dataset$cols[2]]])  # Convert to numeric

  accuracy <- get_accuracy(data1_values, data2_values)
  results_HEI2015_1718 <- rbind(results_HEI2015_1718, data.frame(Component = name, Accuracy = accuracy))
}

# Print results
print(results_HEI2015_1718)

# Plot results
ggplot(results_HEI2015_1718, aes(x = Component, y = Accuracy, fill = Component)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Accuracy (%)") +
  xlab("Component") +
  ggtitle("Comparison of Accuracy: dietaryindex-calculated vs. SAS-calculated Dietary Index Values from NCI") +
  # increase the title size
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20)
  )