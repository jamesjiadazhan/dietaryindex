# remove memory
rm(list=ls())

install.packages("devtools") #If you don't have "devtools" installed already
devtools::install_github("jamesjiadazhan/dietaryindex") # Install the package from GitHub


library(ggplot2)
library(dplyr)
library(tidyr)

library(dietaryindex)
# remove dietaryindex from library if you want to reload the package
# detach("package:dietaryindex", unload=TRUE)

# include COMPLEX SURVEY DESIGN #######################################
library(survey)
options(survey.lonely.psu = "adjust") #accounts for the lonely psu problem from subsetting survey data to small groups


# Load the data
data("DASH_trial")
data("PREDIMED_trial")
data("NHANES_20172018")

# set up working dictionary
setwd("/Users/james/Desktop/NHANES_combined")

# Load the NHANES data from 2005 to 2018
## NHANES 2005-2006
load("NHANES_20052006.rda")

## NHANES 2007-2008
load("NHANES_20072008.rda")

## NHANES 2009-2010
load("NHANES_20092010.rda")

## NHANES 2011-2012
load("NHANES_20112012.rda")

## NHANES 2013-2014
load("NHANES_20132014.rda")

## NHANES 2015-2016
load("NHANES_20152016.rda")

# DASHI from the DASH trial
DASHI_DASH = DASHI(
    SERV_DATA = DASH_trial, 
    RESPONDENTID = DASH_trial$Diet_Type,
    TOTALKCAL_DASHI = DASH_trial$Kcal, 
    TOTAL_FAT_DASHI = DASH_trial$Totalfat_Percent, 
    SAT_FAT_DASHI = DASH_trial$Satfat_Percent, 
    PROTEIN_DASHI = DASH_trial$Protein_Percent, 
    CHOLESTEROL_DASHI = DASH_trial$Cholesterol, 
    FIBER_DASHI = DASH_trial$Fiber, 
    POTASSIUM_DASHI = DASH_trial$Potassium, 
    MAGNESIUM_DASHI = DASH_trial$Magnesium, 
    CALCIUM_DASHI = DASH_trial$Calcium, 
    SODIUM_DASHI = DASH_trial$Sodium)

# MEDI for the PREDIMED trial
MEDI_PREDIMED = MEDI(
  SERV_DATA = PREDIMED_trial,
  RESPONDENTID = PREDIMED_trial$Diet_Type,
  OLIVE_OIL_SERV_MEDI = PREDIMED_trial$Virgin_Oliveoil,
  FRT_SERV_MEDI = PREDIMED_trial$Fruits, 
  VEG_SERV_MEDI = PREDIMED_trial$Vegetables,
  LEGUMES_SERV_MEDI = PREDIMED_trial$Legumes,
  NUTS_SERV_MEDI = PREDIMED_trial$Total_nuts,
  FISH_SEAFOOD_SERV_MEDI = PREDIMED_trial$Fish_Seafood,
  ALCOHOL_SERV_MEDI = PREDIMED_trial$Alcohol,
  SSB_SERV_MEDI = PREDIMED_trial$Soda_Drinks,
  SWEETS_SERV_MEDI = PREDIMED_trial$Sweets,
  DISCRET_FAT_SERV_MEDI = PREDIMED_trial$Refined_Oliveoil,
  REDPROC_MEAT_SERV_MEDI = PREDIMED_trial$Meat)

# set up survey design for NHANES data 2017-2018 day 1 and day 2
## filter out the missing values for the weight variable WTDR2D
NHANES_20172018_design_d1d2 = NHANES_20172018$FPED %>%
    filter(!is.na(WTDR2D))

## NHANES 2017-2018
# DASHI for day 1 and day 2
DASHI_NHANES = DASHI_NHANES_FPED(NUTRIENT_PATH=NHANES_20172018$NUTRIENT, NUTRIENT_PATH2=NHANES_20172018$NUTRIENT2)

# MEDI for day 1 and day 2
MEDI_NHANES = MEDI_NHANES_FPED(FPED_IND_PATH=NHANES_20172018$FPED_IND, NUTRIENT_IND_PATH=NHANES_20172018$NUTRIENT_IND, FPED_IND_PATH2=NHANES_20172018$FPED_IND2, NUTRIENT_IND_PATH2=NHANES_20172018$NUTRIENT_IND2)

# DASH for day 1 and day 2
DASH_NHANES = DASH_NHANES_FPED(NHANES_20172018$FPED_IND, NHANES_20172018$NUTRIENT_IND, NHANES_20172018$FPED_IND2, NHANES_20172018$NUTRIENT_IND2)

# MED for day 1 and day 2
MED_NHANES = MED_NHANES_FPED(FPED_PATH=NHANES_20172018$FPED, NUTRIENT_PATH=NHANES_20172018$NUTRIENT, DEMO_PATH=NHANES_20172018$DEMO, FPED_PATH2=NHANES_20172018$FPED2, NUTRIENT_PATH2=NHANES_20172018$NUTRIENT2)

# AHEI for day 1 and day 2
AHEI_NHANES = AHEI_NHANES_FPED(NHANES_20172018$FPED_IND, NHANES_20172018$NUTRIENT_IND, NHANES_20172018$FPED_IND2, NHANES_20172018$NUTRIENT_IND2)

# DII for day 1 and day 2
DII_NHANES = DII_NHANES_FPED(FPED_PATH=NHANES_20172018$FPED, NUTRIENT_PATH=NHANES_20172018$NUTRIENT, DEMO_PATH=NHANES_20172018$DEMO, FPED_PATH2=NHANES_20172018$FPED2, NUTRIENT_PATH2=NHANES_20172018$NUTRIENT2)

# HEI2020 for day 1 and day 2
HEI2020_NHANES_1718 = HEI2020_NHANES_FPED(FPED_PATH=NHANES_20172018$FPED, NUTRIENT_PATH=NHANES_20172018$NUTRIENT, DEMO_PATH=NHANES_20172018$DEMO, FPED_PATH2=NHANES_20172018$FPED2, NUTRIENT_PATH2=NHANES_20172018$NUTRIENT2)

# Merge all the previous data frames into one data frame by SEQN
NHANES_20172018_dietaryindex_d1d2 = inner_join(NHANES_20172018_design_d1d2, DASHI_NHANES, by = "SEQN") %>%
    inner_join(MEDI_NHANES, by = "SEQN") %>%
    inner_join(DASH_NHANES, by = "SEQN") %>%
    inner_join(MED_NHANES, by = "SEQN") %>%
    inner_join(AHEI_NHANES, by = "SEQN") %>%
    inner_join(DII_NHANES, by = "SEQN") %>%
    inner_join(HEI2020_NHANES_1718, by = "SEQN")

# set up survey design for NHANES data 2017-2018 day 1 and day 2
NHANES_design_1718_d1d2 <- svydesign(
    id = ~SDMVPSU, 
    strata = ~SDMVSTRA, 
    weight = ~WTDR2D, 
    data = NHANES_20172018_dietaryindex_d1d2, #set up survey design on the full dataset #can restrict at time of analysis 
    nest = TRUE) 

# generate the svymean object for DASHI_ALL
DASHI_1718_svymean = svymean(~DASHI_ALL, design = NHANES_design_1718_d1d2, na.rm = TRUE)
# extract the mean from the svymean object
DASHI_1718_svymean_mean = DASHI_1718_svymean[["DASHI_ALL"]]

# generate the svymean object for MEDI_ALL
MEDI_1718_svymean = svymean(~MEDI_ALL, design = NHANES_design_1718_d1d2, na.rm = TRUE)
# extract the mean from the svymean object
MEDI_1718_svymean_mean = MEDI_1718_svymean[["MEDI_ALL"]]

# generate the svymean object for DASH_ALL
DASH_1718_svymean = svymean(~DASH_ALL, design = NHANES_design_1718_d1d2, na.rm = TRUE)
# extract the mean from the svymean object
DASH_1718_svymean_mean = DASH_1718_svymean[["DASH_ALL"]]

# generate the svymean object for MED_ALL
MED_1718_svymean = svymean(~MED_ALL, design = NHANES_design_1718_d1d2, na.rm = TRUE)
# extract the mean from the svymean object
MED_1718_svymean_mean = MED_1718_svymean[["MED_ALL"]]

# generate the svymean object for AHEI_ALL
AHEI_1718_svymean = svymean(~AHEI_ALL, design = NHANES_design_1718_d1d2, na.rm = TRUE)
# extract the mean from the svymean object
AHEI_1718_svymean_mean = AHEI_1718_svymean[["AHEI_ALL"]]

# generate the svymean object for DII_ALL
DII_1718_svymean = svymean(~DII_ALL, design = NHANES_design_1718_d1d2, na.rm = TRUE)
# extract the mean from the svymean object
DII_1718_svymean_mean = DII_1718_svymean[["DII_ALL"]]

# generate the svymean object for HEI2020_ALL
HEI2020_1718_svymean = svymean(~HEI2020_ALL, design = NHANES_design_1718_d1d2, na.rm = TRUE)
# extract the mean from the svymean object
HEI2020_1718_svymean_mean = HEI2020_1718_svymean[["HEI2020_ALL"]]

# HEI2020 for day 1
## 2017-2018
HEI2020_NHANES_1718_d1 = HEI2020_NHANES_FPED(FPED_PATH=NHANES_20172018$FPED, NUTRIENT_PATH=NHANES_20172018$NUTRIENT, DEMO_PATH=NHANES_20172018$DEMO)
## add year column
HEI2020_NHANES_1718_d1 = HEI2020_NHANES_1718_d1 %>%
    mutate(year = "2017-2018")
# select only necessary columns from FPED data
NHANES_20172018_FPED = NHANES_20172018$FPED %>%
    select(SEQN, SDMVPSU, SDMVSTRA, WTDRD1)
## inner join with the NHANES 2017-2018 day 1 FPED data
HEI2020_NHANES_1718_d1 = inner_join(NHANES_20172018_FPED, HEI2020_NHANES_1718_d1, by = "SEQN")

## 2015-2016
HEI2020_NHANES_1516_d1 = HEI2020_NHANES_FPED(FPED_PATH=NHANES_20152016$FPED, NUTRIENT_PATH=NHANES_20152016$NUTRIENT, DEMO_PATH=NHANES_20152016$DEMO)
## add year column
HEI2020_NHANES_1516_d1 = HEI2020_NHANES_1516_d1 %>%
    mutate(year = "2015-2016")
# select only necessary columns from FPED data
NHANES_20152016_FPED = NHANES_20152016$FPED %>%
    select(SEQN, SDMVPSU, SDMVSTRA, WTDRD1)
## inner join with the NHANES 2015-2016 day 1 FPED data
HEI2020_NHANES_1516_d1 = inner_join(NHANES_20152016_FPED, HEI2020_NHANES_1516_d1, by = "SEQN")

## 2013-2014
HEI2020_NHANES_1314_d1 = HEI2020_NHANES_FPED(FPED_PATH=NHANES_20132014$FPED, NUTRIENT_PATH=NHANES_20132014$NUTRIENT, DEMO_PATH=NHANES_20132014$DEMO)
## add year column
HEI2020_NHANES_1314_d1 = HEI2020_NHANES_1314_d1 %>%
    mutate(year = "2013-2014")
# select only necessary columns from FPED data
NHANES_20132014_FPED = NHANES_20132014$FPED %>%
    select(SEQN, SDMVPSU, SDMVSTRA, WTDRD1)
## inner join with the NHANES 2013-2014 day 1 FPED data
HEI2020_NHANES_1314_d1 = inner_join(NHANES_20132014_FPED, HEI2020_NHANES_1314_d1, by = "SEQN")

## 2011-2012
HEI2020_NHANES_1112_d1 = HEI2020_NHANES_FPED(FPED_PATH=NHANES_20112012$FPED, NUTRIENT_PATH=NHANES_20112012$NUTRIENT, DEMO_PATH=NHANES_20112012$DEMO)
## add year column
HEI2020_NHANES_1112_d1 = HEI2020_NHANES_1112_d1 %>%
    mutate(year = "2011-2012")
# select only necessary columns from FPED data
NHANES_20112012_FPED = NHANES_20112012$FPED %>%
    select(SEQN, SDMVPSU, SDMVSTRA, WTDRD1)
## inner join with the NHANES 2011-2012 day 1 FPED data
HEI2020_NHANES_1112_d1 = inner_join(NHANES_20112012_FPED, HEI2020_NHANES_1112_d1, by = "SEQN")

## 2009-2010
HEI2020_NHANES_0910_d1 = HEI2020_NHANES_FPED(FPED_PATH=NHANES_20092010$FPED, NUTRIENT_PATH=NHANES_20092010$NUTRIENT, DEMO_PATH=NHANES_20092010$DEMO)
## add year column
HEI2020_NHANES_0910_d1 = HEI2020_NHANES_0910_d1 %>%
    mutate(year = "2009-2010")
# select only necessary columns from FPED data
NHANES_20092010_FPED = NHANES_20092010$FPED %>%
    select(SEQN, SDMVPSU, SDMVSTRA, WTDRD1)
## inner join with the NHANES 2009-2010 day 1 FPED data
HEI2020_NHANES_0910_d1 = inner_join(NHANES_20092010_FPED, HEI2020_NHANES_0910_d1, by = "SEQN")

## 2007-2008
HEI2020_NHANES_0708_d1 = HEI2020_NHANES_FPED(FPED_PATH=NHANES_20072008$FPED, NUTRIENT_PATH=NHANES_20072008$NUTRIENT, DEMO_PATH=NHANES_20072008$DEMO)
## add year column
HEI2020_NHANES_0708_d1 = HEI2020_NHANES_0708_d1 %>%
    mutate(year = "2007-2008")
# select only necessary columns from FPED data
NHANES_20072008_FPED = NHANES_20072008$FPED %>%
    select(SEQN, SDMVPSU, SDMVSTRA, WTDRD1)
## inner join with the NHANES 2007-2008 day 1 FPED data
HEI2020_NHANES_0708_d1 = inner_join(NHANES_20072008_FPED, HEI2020_NHANES_0708_d1, by = "SEQN")

## 2005-2006
HEI2020_NHANES_0506_d1 = HEI2020_NHANES_FPED(FPED_PATH=NHANES_20052006$FPED, NUTRIENT_PATH=NHANES_20052006$NUTRIENT, DEMO_PATH=NHANES_20052006$DEMO)
## add year column
HEI2020_NHANES_0506_d1 = HEI2020_NHANES_0506_d1 %>%
    mutate(year = "2005-2006")
# select only necessary columns from FPED data
NHANES_20052006_FPED = NHANES_20052006$FPED %>%
    select(SEQN, SDMVPSU, SDMVSTRA, WTDRD1)
## inner join with the NHANES 2005-2006 day 1 FPED data
HEI2020_NHANES_0506_d1 = inner_join(NHANES_20052006_FPED, HEI2020_NHANES_0506_d1, by = "SEQN")

# combine all the HEI2020 day 1 data by rows
HEI2020_NHANES_d1 = rbind(HEI2020_NHANES_0506_d1, HEI2020_NHANES_0708_d1, HEI2020_NHANES_0910_d1, HEI2020_NHANES_1112_d1, HEI2020_NHANES_1314_d1, HEI2020_NHANES_1516_d1, HEI2020_NHANES_1718_d1)
# add toddler column
HEI2020_NHANES_d1_2 = HEI2020_NHANES_d1 %>%
    mutate(toddler = ifelse(RIDAGEYR < 2, "toddler", "nontoddler")) %>%
    ## rearrange the column order
    select(SEQN, SDMVPSU, SDMVSTRA, WTDRD1, year, toddler, everything())

# create the survey design for HEI2020 day 1
HEI2020_NHANES_design_d1 = svydesign(
    id = ~SDMVPSU, 
    strata = ~SDMVSTRA, 
    weight = ~WTDRD1, 
    data = HEI2020_NHANES_d1_2, #set up survey design on the full dataset #can restrict at time of analysis 
    nest = TRUE)

######################################################################
# Case study 1: A comparative analysis of results derived from clinical trials (i.e., DASH and PREDIMED) juxtaposed with findings from an epidemiological study (i.e., NHANES) from 2017-2018, utilizing DASHI and MEDI dietary indexes
## labels on X axis for datasets
x_case1 = c("DASHI_DASH_DASHlowSodium", "DASHI_DASH_DASHMedSodium", "DASHI_DASH_Control", "MEDI_PREDIMED_Med_Oliveoil", "MEDI_PREDIMED_Med_Nuts", "MEDI_PREDIMED_Control", "DASHI_NHANES", "MEDI_NHANES")
## values on Y axis for datasets
y_case1 = c(DASHI_DASH$DASHI_ALL[2]/9, DASHI_DASH$DASHI_ALL[3]/9, DASHI_DASH$DASHI_ALL[5]/9, MEDI_PREDIMED$MEDI_ALL[1]/11,  MEDI_PREDIMED$MEDI_ALL[2]/11, MEDI_PREDIMED$MEDI_ALL[3]/11, DASHI_1718_svymean_mean/9, MEDI_1718_svymean_mean/11)
## Create a vector of data source
z_case1 = c("DASH_trial", "DASH_trial", "DASH_trial", "PREDIMED_trial", "PREDIMED_trial", "PREDIMED_trial", "NHANES_20172018", "NHANES_20172018")
## Create a vector of the dietary index type
w_case1 = c("DASHI", "DASHI", "DASHI", "MEDI", "MEDI", "MEDI", "DASHI", "MEDI")

# multiply all y values by 100 to get the percentage
y_case1 = y_case1*100

## create a data frame
df_case1 = data.frame(x_case1, y_case1, z_case1, w_case1)

# reorder the z_case1 factor levels so taht the order of NHANES 2017-2018 is always on the left side
df_case1$z_case1 = factor(df_case1$z_case1, levels = c("NHANES_20172018", "DASH_trial", "PREDIMED_trial"))

# reorder the x_case1 factor levels
df_case1$x_case1 = factor(df_case1$x_case1, levels = c("DASHI_NHANES", "DASHI_DASH_Control", "DASHI_DASH_DASHMedSodium", "DASHI_DASH_DASHlowSodium", "MEDI_NHANES", "MEDI_PREDIMED_Control", "MEDI_PREDIMED_Med_Nuts", "MEDI_PREDIMED_Med_Oliveoil"))

## plot the data with histogram with different colors for different x values
ggplot(df_case1, aes(x=z_case1, y=y_case1, fill=x_case1)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  # create a facet grid with the dietary index type
  facet_wrap(. ~ w_case1, scales = "free_x") +
  labs(
    y = "Mean dietary index percentile", 
    fill = "Dieary indexes for specific diets"
    ) +
  theme(
    # increase the plot title size
    plot.title = element_text(size=18),
    # remove the x axis title
    axis.title.x = element_blank(),
    # remove the x axis text
    axis.text.x = element_blank(),
    # increase the y axis title and text size
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size=14),
    # increase the legend title and text size
    legend.title = element_text(size=18),
    legend.text = element_text(size=16),
    # increase the facet label size
    strip.text = element_text(size = 16)
  ) +
  # add numeric labels to the bars and increase their size
  geom_text(aes(label = round(y_case1, 2)), vjust = -0.5, size = 4.5, position = position_dodge(0.9)) +
  # add custom fill labels
  scale_fill_discrete(labels = c(
    "DASHI_DASH_DASHlowSodium" = "DASHI for DASH trial low sodium diet",
    "DASHI_DASH_DASHMedSodium" = "DASHI for DASH trial medium sodium diet",
    "DASHI_DASH_Control" = "DASHI for DASH trial control diet",
    "MEDI_PREDIMED_Med_Oliveoil" = "MEDI for PREDIMED mediterranean olive oil diet",
    "MEDI_PREDIMED_Med_Nuts" = "MEDI for PREDIMED mediterranean nuts diet",
    "MEDI_PREDIMED_Control" = "MEDI for PREDIMED control diet",
    "DASHI_NHANES" = "DASHI for NHANES 2017-18",
    "MEDI_NHANES" = "MEDI for NHANES 2017-18"
  ))


###################################################################
# Case study 2: A time series of cross-sectional computation of the HEI2020 from 2005 to 2018
# Define a function to call svymean and unweighted count
getSummary <- function(varformula, byformula, design){
  # Get mean, stderr, and unweighted sample size
  ## unweighted count
  c <- svyby(varformula, byformula, design, unwtd.count) 
  ## weighted mean by complex survey design
  p <- svyby(varformula, byformula, design, svymean) 
  ## join the two data frames by removing the standard error column in the unweighted count data frame
  outSum <- left_join(select(c, -se), p) 
  ## export results
  outSum
}

#' ### Calculate weighted mean and se of HEI2020_ALL by year and toddler status
#' By year and age
HEI2020_ALL_survery_summary = getSummary(~HEI2020_ALL, ~year + toddler, HEI2020_NHANES_design_d1)

# Adjust group labels by changing "toddler" to "Toddler" and nontoddler to "Non-toddler (children and adults)"
HEI2020_ALL_survery_summary$toddler = ifelse(HEI2020_ALL_survery_summary$toddler == "toddler", "Toddler", "Non-toddler (children and adults)")

# rename HEI2020_ALL as Mean_HEI2020, se as SE_HEI2020
HEI2020_ALL_survery_summary = HEI2020_ALL_survery_summary %>%
    rename(Mean_HEI2020 = HEI2020_ALL, SE_HEI2020 = se)

# Plot data with error bars 
ggplot(HEI2020_ALL_survery_summary, aes(x=year, y=Mean_HEI2020, color=toddler, group=toddler)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Mean_HEI2020 - SE_HEI2020, ymax = Mean_HEI2020 + SE_HEI2020), width=0.2) +
  theme_bw() +
  labs(x = "NHANES Cycle", y = "Mean HEI2020", color = "Group") +
  theme(plot.title = element_text(size=22),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=14, angle = 90, hjust = 1),
        axis.text.y = element_text(size=14),
        legend.text = element_text(size=16),
        legend.title = element_text(size=18),
        aspect.ratio = 0.5) 

###################################################################
# Case study 3: Comprehensive Calculation of Multiple Dietary indexes, using the NHANES data in 2017-2018
## create 2 vectors for the x and y axis
x_case3 = c("DASHI", "MEDI", "DASH", "MED", "HEI2020", "AHEI", "DII")
y_case3 = c(DASHI_1718_svymean_mean/9, MEDI_1718_svymean_mean/11, DASH_1718_svymean_mean/40, MED_1718_svymean_mean/9, HEI2020_1718_svymean_mean/100, AHEI_1718_svymean_mean/110, 0.75)
# multiply all y values by 100 to get the percentage
y_case3 = y_case3*100

## create a data frame
df_case3 = data.frame(x_case3, y_case3)

## plot the data with histogram with different colors for different x values
ggplot(df_case3, aes(x_case3, y_case3, fill=x_case3)) + 
  geom_bar(stat = "identity") + theme_bw() + 
  labs(x = "", y = "Mean dietary index percentile", fill = "Dietary Index") + 
  theme(plot.title = element_text(size=22)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(size=18)) +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(size=14)) +
  # increase the size of the legend title
  theme(legend.title = element_text(size=18)) +
  # increase the size of the legend text
  theme(legend.text = element_text(size=16)) +
  theme(aspect.ratio = 0.5) +
  # add numeric labels to the bars
  geom_text(aes(label = round(y_case3, 2)), vjust = -0.5, size = 4.5)
