## dietaryindex
___
### Overview
___

Version 0.14.0: The original DASH trial and PREDIMED trial data are available as example data. You can calculate DASHI using the DASH trial data and MEDI using the PREDIMED trial data, which allows you compare different DASH or MED diets in multiple studies to improve the inconsistencies of evaluating and defining DASH or MED diets. American Cancer Society 2020 diet score (ACS2020_V1 and ACS2020_V2) are available as generic functions. 

The goal of **dietaryindex** is to offer streamlined methods to standardize the definition of dietary patterns and assess the adherence to dietary patterns in epidemiologic and clinical studies, facilitating precision nutrition. 

The package **dietaryindex** calculates dietary indexes by 2 steps:
- Step 1. Calculate the serving size of each food and nutrient category
- Step 2. Calculate the individual dietary index using the serving size information

The package can calculate the following dietary pattern indexes using all dietary assessments if the serving sizes of foods/beverages are given (do Step 1 by yourself and Step 2 is done for you): 
- Healthy Eating Index 2015 (HEI2015)
- Alternative Healthy Eating Index (AHEI)
- Dietary Approaches to Stop Hypertension Index (DASH)
- DASH Index in serving sizes from the DASH trial(DASHI)
- Mediterranean Diet Index (MED)
- MED Index in serving sizes from the PREDIMED trial (MEDI)
- Dietary Inflammation Index (DII)
- American Cancer Society 2020 diet score (ACS2020_V1 and ACS2020_V2)

Meanwhile, the **dietaryindex** package can calculate many dietary indexes within 1 step (Step 1 + Step 2) using the following dietary assessments:
- It can calculate HEI2015, AHEI, DASH, MED, and DII for the NHANES_FPED (after 2005).
- It can calculate HEI2015, AHEI, DASH, MED, and DII for the ASA24
- It can calculate HEI2015, AHEI, DASH, MED for the DHQ3
- It can calculate HEI2015, AHEI, DASH, MED, DII, AHEIP for the Block FFQ (DataDictionary_Muldoon_4M_microbiome_nutsDec2013)

Notes:
- If you are interested in the methods of the dietary indexes, an excel sheet for the serving size of all dietary indexes is provided: DIETARYINDEX_SERVING_SIZE_CHART_JAMES_ZHAN_BH_FINAL.xlsx
- The outputs of the generic functions (e.g. HEI2015, DII) include dietary indexes and their component scores. 
- The outputs of the specific functions (e.g. DASH_ASA24) include dietary indexes, their component scores, and their food/drink serving sizes.
- The **dietaryindex** package relies on the **dplyr**, **readr**, and **haven** packages. The **dietaryindex** package will install those packages for you automatically.


### Installation
___

**dietaryindex** is currently not available on [CRAN]


To install the development version hosted on this GitHub repository, use the **devtools** package and the following:

```
install.packages("devtools") #If you don't have "devtools" installed already
devtools::install_github("jamesjiadazhan/dietaryindex")
```


### Getting Started
___
```
library(dietaryindex)

```

The **dietaryindex** package currently contains the following key functions:

- NHANES_FPED (after 2005)
  - `HEI2015_NHANES_FPED()`, Calculating HEI2015 with 1 step using the NHANES_FPED data (after 2005)
  - `AHEI_NHANES_FPED()`, Calculating AHEI with 1 step using the NHANES_FPED data (after 2005)
  - `DASH_NHANES_FPED()`, Calculating DASH with 1 step using the NHANES_FPED data (after 2005)
  - `MED_NHANES_FPED()`, Calculating MED with 1 step using the NHANES_FPED data (after 2005)
  - `DII_NHANES_FPED()`, Calculating DII with 1 step using the NHANES_FPED data (after 2005)

- ASA24
  - `HEI2015_ASA24()`, Calculating HEI2015 with 1 step using the ASA24 data
  - `AHEI_F_ASA24()`, Calculate the AHEI (female only) within 1 step using the ASA24 data
  - `AHEI_M_ASA24()`, Calculate the AHEI (male only) within 1 step using the ASA24 data
  - `DASH_ASA24()`, Calculating DASH with 1 step using the ASA24 data
  - `MED_ASA24()`, Calculating MED with 1 step using the ASA24 data
  - `DII_ASA24()`, Calculating DII with 1 step using the ASA24 data

- DHQ3
  - `HEI2015_DHQ3()`, Calculating HEI2015 with 1 step using the DHQ3 data
  - `AHEI_DHQ3()`, Calculate the AHEI (female or male) within 1 step using the DHQ3 data
  - `DASH_DHQ3()`, Calculating DASH with 1 step using the DHQ3 data. The data is Detailed analysis file, ending with detail.csv
  - `MED_DHQ3()`, Calculating MED with 1 step using the DHQ3 data
  
- BLOCK
  - `HEI2015_BLOCK`, Calculating HEI2015 with 1 step using the BLOCK data
  - `MED_BLOCK`, Calculating MED with 1 step using the BLOCK data
  - `DII_BLOCK`, Calculating DII with 1 step using the BLOCK data
  - `DASH_BLOCK`, Calculating DASH with 1 step using the BLOCK data
  - `AHEI_BLOCK`, Calculating AHEI with 1 step using the BLOCK data
  - `AHEIP_BLOCK`, Calculating AHEIP with 1 step using the BLOCK data

- Generic functions
  - `HEI2015()`, Healthy Eating Index 2015 
    - Ref: https://www.fns.usda.gov/how-hei-scored
  - `AHEI()`, alternative healthy eating index 
    - Ref: https://academic.oup.com/jn/article/142/6/1009/4688968?login=false
  - `AHEIP()` , alternative healthy eating index - pregnancy
    - Ref: https://www.jandonline.org/article/S0002-8223(09)00288-0/fulltext
  - `DASH()`, Dietary Approaches to Stop Hypertension 
    - Ref:https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/414155
  - `DASHI()`, DASH Index in serving sizes from the DASH trial 
    - Ref:
      - https://www.nhlbi.nih.gov/education/dash-eating-plan
      - https://www.nejm.org/doi/10.1056/NEJM199704173361601?url_ver=Z39.88-2003&rfr_id=ori:rid:crossref.org&rfr_dat=cr_pub%20%200www.ncbi.nlm.nih.gov
      - https://www.dietaryguidelines.gov/sites/default/files/2019-05/1995%20Dietary%20Guidelines%20for%20Americans.pdf
      - https://www.sciencedirect.com/science/article/pii/S0002822399004125
      - https://www.nejm.org/doi/full/10.1056/nejm200101043440101
  - `MED()`, Mediterranean diet 
    - Ref: https://www.ahajournals.org/doi/10.1161/CIRCULATIONAHA.108.816736?url_ver=Z39.88-2003&rfr_id=ori:rid:crossref.org&rfr_dat=cr_pub%20%200pubmed
  - `MEDI()`, Mediterranean diet in serving sizes from the PREDIMED trial 
    - Ref: https://www.nejm.org/doi/full/10.1056/nejmoa1800389
  - `DII()`, Dietary Inflammation Index 
    - Ref: https://www.cambridge.org/core/journals/public-health-nutrition/article/designing-and-developing-a-literaturederived-populationbased-dietary-inflammatory-index/30BE2C2295CE93DC6B54F9F9AD50CC68
  - `ACS2020_V1()`, American Cancer Society 2020 diet score
  - `ACS2020_V2()`, Alternate calculation method of the American Cancer Society 2020 diet score, intended for use when percent calories from highly processed foods and refined grains is not available (uses daily servings per 1000 calories instead)
    - Ref: https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2793171



### Examples:
___

#### Calculating HEI2015 for NHANES_FPED
```
FPED_PATH = "/Users/james/Desktop/fped_dr1tot_1718.sas7bdat"
NUTRIENT_PATH = "/Users/james/Desktop/DR1TOT_J.XPT"
DEMO_PATH = "/Users/james/Desktop/DEMO_J.XPT"

HEI2015_NHANES_FPED(FPED_PATH, NUTRIENT_PATH, DEMO_PATH)

#Use the example data
data("NHANES_20172018")
HEI2015_NHANES_FPED(NHANES_20172018$FPED, NHANES_20172018$NUTRIENT, NHANES_20172018$DEMO)
```

#### Calculating MED for NHANES_FPED
```
FPED_PATH = "/Users/james/Desktop/fped_dr1tot_1718.sas7bdat"
NUTRIENT_PATH = "/Users/james/Desktop/DR1TOT_J.XPT"
DEMO_PATH = "/Users/james/Desktop/DEMO_J.XPT"

MED_NHANES_FPED(FPED_PATH, NUTRIENT_PATH, DEMO_PATH)

#Use the example data
data("NHANES_20172018")
MED_NHANES_FPED(NHANES_20172018$FPED, NHANES_20172018$NUTRIENT, NHANES_20172018$DEMO)

```

#### Calculating DII for NHANES_FPED
```
FPED_PATH = "/Users/james/Desktop/fped_dr1tot_1718.sas7bdat"
NUTRIENT_PATH = "/Users/james/Desktop/DR1TOT_J.XPT"
DEMO_PATH = "/Users/james/Desktop/DEMO_J.XPT"

DII_NHANES_FPED(FPED_PATH, NUTRIENT_PATH, DEMO_PATH)

#Use the example data
data("NHANES_20172018")
DII_NHANES_FPED(NHANES_20172018$FPED, NHANES_20172018$NUTRIENT, NHANES_20172018$DEMO)

```

#### Calculating AHEI for NHANES_FPED
```
FPED_IND_PATH = "/Users/james/Desktop/data/fped_dr1iff.sas7bdat"
NUTRIENT_IND_PATH = "/Users/james/Desktop/data/DR1IFF_J"

AHEI_NHANES_FPED(FPED_IND_PATH, NUTRIENT_IND_PATH)

#Use the example data
data("NHANES_20172018")
AHEI_NHANES_FPED(NHANES_20172018$FPED_IND, NHANES_20172018$NUTRIENT_IND)
```

#### Calculating DASH for NHANES_FPED
```
FPED_IND_PATH = "/Users/james/Desktop/data/fped_dr1iff.sas7bdat"
NUTRIENT_IND_PATH = "/Users/james/Desktop/data/DR1IFF_J"

DASH_NHANES_FPED(FPED_IND_PATH, NUTRIENT_IND_PATH)

#Use the example data
data("NHANES_20172018")
DASH_NHANES_FPED(NHANES_20172018$FPED_IND, NHANES_20172018$NUTRIENT_IND)

```

#### Calculating HEI2015 for ASA24
```
DATA_PATH = "/Users/james/Desktop/data/Totals.csv"
HEI2015_ASA24(DATA_PATH)

#Use the example data
data("ASA24_exp")
HEI2015_ASA24(ASA24_exp)
```
#### Calculating MED for ASA24
```
DATA_PATH = "/Users/james/Desktop/data/Totals.csv"
MED_ASA24(DATA_PATH)

#Use the example data
data("ASA24_exp")
MED_ASA24(ASA24_exp)
```

#### Calculating DII for ASA24
```
DATA_PATH = "/Users/james/Desktop/data/Totals.csv"
DII_ASA24(DATA_PATH)

#Use the example data
data("ASA24_exp")
DII_ASA24(ASA24_exp)
```


#### Calculating DASH for ASA24
```
DATA_PATH = "/Users/james/Desktop/data/items.csv"
DASH_ASA24(DATA_PATH)

#Use the example data
data("ASA24_exp_detailed")
DASH_ASA24(ASA24_exp_detailed)
```

#### Calculating AHEI for ASA24
```
DATA_PATH = "/Users/james/Desktop/data/items.csv"
AHEI_F_ASA24(DATA_PATH)
AHEI_M_ASA24(DATA_PATH)

#Use the example data
data("ASA24_exp_detailed")
AHEI_F_ASA24(ASA24_exp_detailed) # for participants who are all female
AHEI_M_ASA24(ASA24_exp_detailed) # for participants who are all male
```


#### Calculating HEI2015 for DHQ3
```
DATA_PATH = "/Users/james/Desktop/data/results.csv"
HEI2015_DHQ3(DATA_PATH)

#Use the example data
data("DHQ3_exp")
HEI2015_DHQ3(DHQ3_exp)
```

#### Calculating MED for DHQ3
```
DATA_PATH = "/Users/james/Desktop/data/results.csv"
MED_DHQ3(DATA_PATH)

#Use the example data
data("DHQ3_exp")
MED_DHQ3(DHQ3_exp)
```

#### Calculating AHEI for DHQ3
```
DATA_PATH = "/Users/james/Desktop/data/detail.csv"
AHEI_DHQ3(DATA_PATH)

#Use the example data
data("DHQ3_exp_detailed")
AHEI_DHQ3(DHQ3_exp_detailed)
```

#### Calculating DASH for DHQ3
```
DATA_PATH = "/Users/james/Desktop/data/detail.csv"
DASH_DHQ3(DATA_PATH)

#Use the example data. Attention: the example data here is DHQ3_exp_detailed, which is different from DHQ3_exp. DHQ3_exp_detailed is only used for DASH_DHQ3
data("DHQ3_exp_detailed")
DASH_DHQ3(DHQ3_exp_detailed)
```

#### Calculating AHEI for BLOCK
```
DATA_PATH = "/Users/james/Desktop/block_exp.csv"
AHEI_BLOCK(DATA_PATH)

#Use the example data
data("BLOCK_exp")
AHEI_BLOCK(BLOCK_exp)
```

#### Calculating AHEIP for BLOCK
```
#Use the example data
data("BLOCK_exp")
AHEIP_BLOCK(BLOCK_exp)
```

#### Calculating DASH for BLOCK
```
#Use the example data
data("BLOCK_exp")
DASH_BLOCK(BLOCK_exp)
```

#### Calculating DII for BLOCK
```
#Use the example data
data("BLOCK_exp")
DII_BLOCK(BLOCK_exp)
```

#### Calculating HEI2015 for BLOCK
```
#Use the example data
data("BLOCK_exp")
HEI2015_BLOCK(BLOCK_exp)
```

#### Calculating MED for BLOCK
```
#Use the example data
data("BLOCK_exp")
MED_BLOCK(BLOCK_exp)
```

#### Calculating HEI2015 for your own dietary assessment tool
```
DATA_PATH <- "/Users/james/Desktop/data.csv"
SERV_DATA <- read_csv(DATA_PATH)

HEI2015(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$TOTALKCAL, SERV_DATA$VEG_SERV, SERV_DATA$FRT_SERV, SERV_DATA$WGRAIN_SERV, SERV_DATA$NUTSLEG_SERV, SERV_DATA$N3FAT_SERV, SERV_DATA$PUFA_SERV, SERV_DATA$SSB_FRTJ_SERV, SERV_DATA$REDPROC_MEAT_SERV, SERV_DATA$TRANS_SERV, SERV_DATA$SODIUM_SERV, SERV_DATA$ALCOHOL_SERV)

#Use the example data
data("SERV_DATA_exp")
HEI2015(SERV_DATA_exp, SERV_DATA_exp$UserName, SERV_DATA_exp$TOTALKCAL, SERV_DATA_exp$TOTALFRT_SERV_HEI2015_exp, SERV_DATA_exp$FRT_SERV_HEI2015_exp, SERV_DATA_exp$VEG_SERV_HEI2015_exp, SERV_DATA_exp$GREENNBEAN_SERV_HEI2015_exp, SERV_DATA_exp$TOTALPRO_SERV_HEI2015_exp,  SERV_DATA_exp$SEAPLANTPRO_SERV_HEI2015_exp, SERV_DATA_exp$WHOLEGRAIN_SERV_HEI2015_exp, SERV_DATA_exp$DAIRY_SERV_HEI2015_exp, SERV_DATA_exp$FATTYACID_SERV_HEI2015_exp, SERV_DATA_exp$REFINEDGRAIN_SERV_HEI2015_exp,  SERV_DATA_exp$SODIUM_SERV_HEI2015_exp, SERV_DATA_exp$ADDEDSUGAR_SERV_HEI2015_exp, SERV_DATA_exp$SATFAT_SERV_HEI2015_exp)

```

#### Calculating AHEI for your own dietary assessment tool
```
DATA_PATH <- "/Users/james/Desktop/data.csv"
SERV_DATA <- read_csv(DATA_PATH)

AHEI(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$GENDER, SERV_DATA$VEG_SERV, SERV_DATA$FRT_SERV, SERV_DATA$WGRAIN_SERV, SERV_DATA$NUTSLEG_SERV, SERV_DATA$N3FAT_SERV, SERV_DATA$PUFA_SERV, SERV_DATA$SSB_FRTJ_SERV, SERV_DATA$REDPROC_MEAT_SERV, SERV_DATA$TRANS_SERV,SODIUM_SERV, SERV_DATA$ALCOHOL_SERV)

#Use the example data
data("SERV_DATA_exp")
AHEI(SERV_DATA_exp, SERV_DATA_exp$UserName, SERV_DATA_exp$SEX, SERV_DATA_exp$TOTALKCAL, SERV_DATA_exp$VEG_SERV_AHEI_exp, SERV_DATA_exp$FRT_SERV_AHEI_exp, SERV_DATA_exp$WGRAIN_SERV_AHEI_exp, SERV_DATA_exp$NUTSLEG_SERV_AHEI_exp, SERV_DATA_exp$N3FAT_SERV_AHEI_exp, SERV_DATA_exp$PUFA_SERV_AHEI_exp, SERV_DATA_exp$SSB_FRTJ_SERV_AHEI_exp, SERV_DATA_exp$REDPROC_MEAT_SERV_AHEI_exp, SERV_DATA_exp$TRANS_SERV_AHEI_exp, SERV_DATA_exp$SODIUM_SERV_AHEI_exp, SERV_DATA_exp$ALCOHOL_SERV_AHEI_exp)

```

#### Calculating DASH for your own dietary assessment tool
```
DATA_PATH <- "/Users/james/Desktop/data.csv"
SERV_DATA <- read_csv(DATA_PATH)

DASH(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$TOTALKCAL_DASH, SERV_DATA$FRT_FRTJ_SERV_DASH, SERV_DATA$VEG_SERV_DASH, SERV_DATA$NUTSLEG_SERV_DASH, SERV_DATA$WGRAIN_SERV_DASH, SERV_DATA$LOWF_DAIRY_SERV_DASH, SERV_DATA$SODIUM_SERV_DASH, SERV_DATA$REDPROC_MEAT_SERV_DASH, SERV_DATA$SSB_FRTJ_SERV_DASH)

#Use the example data
data("SERV_DATA_exp")
DASH(SERV_DATA_exp, SERV_DATA_exp$UserName, SERV_DATA_exp$TOTALKCAL, SERV_DATA_exp$FRT_FRTJ_SERV_DASH_exp, SERV_DATA_exp$VEG_SERV_DASH_exp, SERV_DATA_exp$NUTSLEG_SERV_DASH_exp, SERV_DATA_exp$WGRAIN_SERV_DASH_exp, SERV_DATA_exp$LOWF_DAIRY_SERV_DASH_exp, SERV_DATA_exp$SODIUM_SERV_DASH_exp, SERV_DATA_exp$REDPROC_MEAT_SERV_DASH_exp, SERV_DATA_exp$SSB_FRTJ_SERV_DASH_exp)


```

#### Calculating DASHI for your own dietary assessment tool
```
DATA_PATH <- "/Users/james/Desktop/data.csv"
SERV_DATA <- read_csv(DATA_PATH)

DASHI(SERV_DATA, RESPONDENTID, SERV_DATA$TOTALKCAL_DASHI, SERV_DATA$VEG_SERV_DASHI, SERV_DATA$FRT_FRTJ_SERV_DASHI, SERV_DATA$NUTSLEG_SERV_DASHI, SERV_DATA$LOWF_DAIRY_SERV_DASHI, SERV_DATA$WGRAIN_SERV_DASHI, SERV_DATA$WHITEMEAT_SERV_DASHI, SERV_DATA$REDPROC_MEAT_SERV_DASHI, SERV_DATA$FATOIL_SERV_DASHI, SERV_DATA$SWEETS_SERV_DASHI, SERV_DATA$SODIUM_SERV_DASHI)

#Use the example data
data("DASH_trial")
DASHI(
  SERV_DATA = DASH_trial, 
  RESPONDENTID = DASH_trial$Diet_Type,
  TOTALKCAL_DASHI = DASH_trial$Kcal,
  VEG_SERV_DASHI = DASH_trial$Vegetables, 
  FRT_FRTJ_SERV_DASHI = DASH_trial$Fruits_Juices, 
  NUTSLEG_SERV_DASHI = DASH_trial$Nuts_Seeds_Legumes, 
  LOWF_DAIRY_SERV_DASHI = DASH_trial$Lowfat_Dairy,
  WGRAIN_SERV_DASHI = DASH_trial$Wholegrains, 
  WHITEMEAT_SERV_DASHI = DASH_trial$Whitemeat, 
  REDPROC_MEAT_SERV_DASHI = DASH_trial$Beef_Pork_Ham, 
  FATOIL_SERV_DASHI = DASH_trial$Fat_Oils_salad_dressing, 
  SWEETS_SERV_DASHI = DASH_trial$Snacks_Sweets,
  SODIUM_SERV_DASHI = DASH_trial$Sodium)

```

#### Calculating MED for your own dietary assessment tool
```
DATA_PATH <- "/Users/james/Desktop/data.csv"
SERV_DATA <- read_csv(DATA_PATH)

MED(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$FRT_FRTJ_SERV, SERV_DATA$VEG_SERV, SERV_DATA$WGRAIN_SERV, SERV_DATA$LEGUMES_SERV, SERV_DATA$NUTS_SERV,FISH_SERV, SERV_DATA$REDPROC_MEAT_SERV, SERV_DATA$MONSATFAT_SERV, SERV_DATA$ALCOHOL_SERV)

#Use the example data
data("SERV_DATA_exp")
MED(SERV_DATA_exp, SERV_DATA_exp$UserName, SERV_DATA_exp$FRT_FRTJ_SERV_MED_exp, SERV_DATA_exp$VEG_SERV_MED_exp, SERV_DATA_exp$WGRAIN_SERV_MED_exp, SERV_DATA_exp$LEGUMES_SERV_MED_exp, SERV_DATA_exp$NUTS_SERV_MED_exp, SERV_DATA_exp$FISH_SERV_MED_exp, SERV_DATA_exp$REDPROC_MEAT_SERV_MED_exp, SERV_DATA_exp$MONSATFAT_SERV_MED_exp, SERV_DATA_exp$ALCOHOL_SERV_MED_exp)


```

#### Calculating MEDI for your own dietary assessment tool
```
DATA_PATH <- "/Users/james/Desktop/data.csv"
SERV_DATA <- read_csv(DATA_PATH)

MEDI(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$OLIVE_OIL_SERV_MEDI, SERV_DATA$FRT_SERV_MEDI, SERV_DATA$VEG_SERV_MEDI, SERV_DATA$LEGUMES_SERV_MEDI, SERV_DATA$NUTS_SERV_MEDI, SERV_DATA$FISH_SEAFOOD_SERV_MEDI, SERV_DATA$ALCOHOL_SERV_MEDI, SERV_DATA$SSB_SERV_MEDI, SERV_DATA$SWEETS_SERV_MEDI, SERV_DATA$DISCRET_FAT_SERV_MEDI, SERV_DATA$REDPROC_MEAT_SERV_MEDI)

#Use the example data
data("PREDIMED_trial")

# MEDI is 0/1 point scoring criteria.
MEDI(
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

# MEDI_V2 is 5 point scoring criteria
MEDI_V2(
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
    REDPROC_MEAT_SERV_MEDI = PREDIMED_trial$Meat
)

```

#### Calculating DII for your own dietary assessment tool
```
DATA_PATH <- "/Users/james/Desktop/data.csv"
SERV_DATA <- read_csv(DATA_PATH)

DII(SERV_DATA, SERV_DATA$RESPONDENTID, REPEATNUM=1, SERV_DATA$ALCOHOL_DII, SERV_DATA$VITB12_DII, SERV_DATA$VITB6_DII, SERV_DATA$BCAROTENE_DII, SERV_DATA$CAFFEINE_DII, SERV_DATA$CARB_DII, SERV_DATA$CHOLES_DII, SERV_DATA$KCAL_DII, SERV_DATA$EUGENOL_DII, SERV_DATA$TOTALFAT_DII, SERV_DATA$FIBER_DII, SERV_DATA$FOLICACID_DII, SERV_DATA$GARLIC_DII, SERV_DATA$GINGER_DII, SERV_DATA$IRON_DII, SERV_DATA$MG_DII, SERV_DATA$MUFA_DII, SERV_DATA$NIACIN_DII, SERV_DATA$N3FAT_DII, SERV_DATA$N6FAT_DII, SERV_DATA$ONION_DII, SERV_DATA$PROTEIN_DII, SERV_DATA$PUFA_DII, SERV_DATA$RIBOFLAVIN_DII, SERV_DATA$SAFFRON_DII, SERV_DATA$SATFAT_DII, SERV_DATA$SE_DII, SERV_DATA$THIAMIN_DII, SERV_DATA$TRANSFAT_DII, SERV_DATA$TURMERIC_DII, SERV_DATA$VITA_DII, SERV_DATA$VITC_DII, SERV_DATA$VITD_DII, SERV_DATA$VITE_DII, SERV_DATA$ZN_DII, SERV_DATA$TEA_DII, SERV_DATA$FLA3OL_DII, SERV_DATA$FLAVONES_DII, SERV_DATA$FLAVONOLS_DII, SERV_DATA$FLAVONONES_DII, SERV_DATA$ANTHOC_DII, SERV_DATA$ISOFLAVONES_DII, SERV_DATA$PEPPER_DII, SERV_DATA$THYME_DII, SERV_DATA$ROSEMARY_DII)

#Use the example data
data("DHQ3_exp")
DII(DHQ3_exp, DHQ3_exp$`Respondent ID`, 1, DHQ3_exp$`Alcohol (g)`, DHQ3_exp$`Vitamin B12 (mcg)`, DHQ3_exp$`Vitamin B6 (mg)`)

```

#### Calculating ACS2020_V1 or ACS2020_V2 for your own dietary assessment tool
```
DATA_PATH <- "/Users/james/Desktop/data.csv"
SERV_DATA <- read_csv(DATA_PATH)

ACS2020_V1(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$GENDER, SERV_DATA$VEG_SERV_ACS2020, SERV_DATA$VEG_ITEMS_SERV_ACS2020, SERV_DATA$FRT_SERV_ACS2020, SERV_DATA$FRT_ITEMS_SERV_ACS2020, SERV_DATA$WGRAIN_SERV_ACS2020, SERV_DATA$SSB_FRTJ_SERV_ACS2020, SERV_DATA$REDPROC_MEAT_SERV_ACS2020, SERV_DATA$HPFRG_RATIO_SERV_ACS2020)

ACS2020_V2(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$GENDER, SERV_DATA$TOTALKCAL_ACS2020, SERV_DATA$VEG_SERV_ACS2020, SERV_DATA$VEG_ITEMS_SERV_ACS2020, SERV_DATA$FRT_SERV_ACS2020, SERV_DATA$FRT_ITEMS_SERV_ACS2020, SERV_DATA$WGRAIN_SERV_ACS2020, SERV_DATA$SSB_FRTJ_SERV_ACS2020, SERV_DATA$REDPROC_MEAT_SERV_ACS2020, SERV_DATA$HPFRG_SERV_ACS2020)

```

#### Add dietary index output to your own data and save the result
```
# Store the output of HEI2015 in "HEI2015_output"
data("SERV_DATA_exp")
HEI2015_output = HEI2015(SERV_DATA_exp, SERV_DATA_exp$UserName, SERV_DATA_exp$TOTALKCAL, SERV_DATA_exp$TOTALFRT_SERV_HEI2015_exp, SERV_DATA_exp$FRT_SERV_HEI2015_exp, SERV_DATA_exp$VEG_SERV_HEI2015_exp, SERV_DATA_exp$GREENNBEAN_SERV_HEI2015_exp, SERV_DATA_exp$TOTALPRO_SERV_HEI2015_exp,  SERV_DATA_exp$SEAPLANTPRO_SERV_HEI2015_exp, SERV_DATA_exp$WHOLEGRAIN_SERV_HEI2015_exp, SERV_DATA_exp$DAIRY_SERV_HEI2015_exp, SERV_DATA_exp$FATTYACID_SERV_HEI2015_exp, SERV_DATA_exp$REFINEDGRAIN_SERV_HEI2015_exp,  SERV_DATA_exp$SODIUM_SERV_HEI2015_exp, SERV_DATA_exp$ADDEDSUGAR_SERV_HEI2015_exp, SERV_DATA_exp$SATFAT_SERV_HEI2015_exp)


# Merge the HEI2015_output with your own data by the participant ID
# Here, the HEI2015_output'S participant ID is "RESPONDENTID", while the participant ID in your selected data may vary (SERV_DATA_exp's participant ID UserName) and it should be the column name of SERV_DATA$RESPONDENTID in the HEI2015 function
Merged_HEI2015_output = left_join(SERV_DATA_exp, HEI2015_output, by=c("UserName" = "RESPONDENTID"))

# Save the result on your computer
readr::write_csv(Merged_HEI2015_output, "/your_output_file_location/Merged_HEI2015_output.csv")

```



### Related Work
___

**dietaryindex** is mainly intended as a versatile tool to help for calculating different dietary indexes conveniently. It is designed to be flexible to work for almost all types of dietary assessment tools, including food frequency questionnaires, 24-hours dietary recalls, and even food records, while itself supports many 1-step dietary index calculations for NHANES, ASA24, and DHQ3.  Please follow the instruction of your specific dietary assessment tools and relevant articles regarding how to accurately define the serving size (see above) if it is not provided in our package, as they are the key to obtain high-quality dietary indexes. **dietaryindex** also provides some help in defining the serving size in the help file, argument section. Note: some very specific dietary index components (low-fat dairy and sugar sweetened beverage) are not easily available and thus are difficult to assess. The author used individual-level food data to compute the population-level food group data. For example, the sugar sweetened beverage serving is estimated by dividing the total added sugar intakes in grams from beverages by 26, because 1 bottle (8 oz) of Coke has 26 g added sugars and this is used as the benchmark, as different sugar sweetened beverages have largely different added sugar contents. Please use your own judgment to determine if the dietary indexes calculated using the **dietaryindex** package is appropriate for your research.

For NHANES data:
FPED file refers to the DR1TOT file in the Food Patterns equivalents for foods in the WWEIA (https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/fped-databases/). This is a exe zip file, so please unzip this file first to retrieve the SAS file on Windows. If you are a Mac user and have trouble unzipping the exe file, you can reach out to **James Jiada Zhan** via jzha832@emory.edu, so he could share unzipped FPED data with you via OneDrive individually as a courtesy.

NUTRIENT file refers to the DR1TOT file in the Dietary Interview - Total Nutrient Intakes, First Day, Dietary Data (example: 05-06 https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Dietary&CycleBeginYear=2005). 

DEMO file refers to the DEMO file in the Demographic Variables & Sample Weights (example: 05-06 https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics&CycleBeginYear=2005)

DBQ file refers to the DBQ file in the Diet Behavior & Nutrition, Questionnaire Data (example: 05-06 https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Questionnaire&CycleBeginYear=2005)

### Contributing & Notes

**dietaryindex** is licensed under the [MIT License]. Please check out the [Contribution guide](https://github.com/jamesjiadazhan/dietaryindex/blob/main/CONTRIBUTING.md) for questions, feature requests and bug reports. The maintainer will review pull requests and incorporate contributions at his discretion. You may also reach out to the maintainer, **James Jiada Zhan**, via his email: jzha832@emory.edu. **James Jiada Zhan** home page at Emory is: https://www.sph.emory.edu/phd-students/profile/index.php?FID=jiada-zhan-12906. **Becky Hodge** provided significant contributions to validate this package. Thanks a lot for her help. 