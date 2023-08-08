### Getting Started
___
```
# load the package. After installing the package, the package has to be loaded before using
library(dietaryindex)
```

The **dietaryindex** package currently contains the following key functions:

- Generic functions
  - `HEI2020()`, `HEI2020_V2()`, Healthy Eating Index 2020 & HEI-Toddlers-2020
    - Ref: https://pubmed.ncbi.nlm.nih.gov/37201748/
    - Ref: https://pubmed.ncbi.nlm.nih.gov/37209965/
    - The only difference for HEI2020_V2 is that it has different serving size definitions for added sugar (tsp, teaspoon) and saturated fat (g, gram). In HEI2020, the required serving sizes for added sugar and saturated fat are % of total energy, which may be hard to calculate. Thus, HEI2020_V2 is provided separately as a potentially convenient option. Note, 1 tsp of added sugar is equal to 4 grams in dietaryindex.
  - `HEI2015()`, Healthy Eating Index 2015 
    - Ref: https://www.fns.usda.gov/how-hei-scored
  - `AHEI()`, alternative healthy eating index 
    - Ref: https://academic.oup.com/jn/article/142/6/1009/4688968?login=false
  - `AHEIP()` , alternative healthy eating index - pregnancy
    - Ref: https://www.jandonline.org/article/S0002-8223(09)00288-0/fulltext
  - `DASH()`, Dietary Approaches to Stop Hypertension 
    - Ref:https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/414155
  - `DASHI()`, DASH Index in nutrients from the DASH trial 
    - Ref:
      - https://www.nejm.org/doi/10.1056/NEJM199704173361601?url_ver=Z39.88-2003&rfr_id=ori:rid:crossref.org&rfr_dat=cr_pub%20%200www.ncbi.nlm.nih.gov
      - https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/413957
  - `MED()`, Alternate Mediterranean Diet Score (aMED)
    - Ref: https://www.ahajournals.org/doi/10.1161/CIRCULATIONAHA.108.816736?url_ver=Z39.88-2003&rfr_id=ori:rid:crossref.org&rfr_dat=cr_pub%20%200pubmed
  - `MEDI()`, Mediterranean diet in serving sizes from the PREDIMED trial 
    - Ref: https://www.nejm.org/doi/full/10.1056/nejmoa1800389
  - `DII()`, Dietary Inflammation Index 
    - Ref: https://www.cambridge.org/core/journals/public-health-nutrition/article/designing-and-developing-a-literaturederived-populationbased-dietary-inflammatory-index/30BE2C2295CE93DC6B54F9F9AD50CC68
  - `ACS2020_V1()`, American Cancer Society 2020 diet score
  - `ACS2020_V2()`, Alternate calculation method of the American Cancer Society 2020 diet score, intended for use when percent calories from highly processed foods and refined grains is not available (uses daily servings per 1000 calories instead)
    - Ref: https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2793171
  - `PHDI()`, Planetary Health Diet Index from the EAT-Lancet Commission
    - Ref: https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(18)31788-4/fulltext

- NHANES_FPED (after 2005) 
  - `HEI2020_NHANES_FPED()`, Calculating HEI2020 & HEI-Toddlers-2020 with 1 step using the NHANES_FPED data (after 2005)
  - `HEI2015_NHANES_FPED()`, Calculating HEI2015 with 1 step using the NHANES_FPED data (after 2005)
  - `AHEI_NHANES_FPED()`, Calculating AHEI with 1 step using the NHANES_FPED data (after 2005)
  - `DASH_NHANES_FPED()`, Calculating DASH (quintile-based) with 1 step using the NHANES_FPED data (after 2005)
  - `DASHI_NHANES_FPED()`, Calculating DASHI (nutrient-based from the DASH trial) with 1 step using the NHANES_FPED data (after 2005)
  - `MED_NHANES_FPED()`, Calculating aMED (median-based) with 1 step using the NHANES_FPED data (after 2005)
  - `MEDI_NHANES_FPED()`, Calculating MEDI (serving size-based from the PREDIMED trial) with 1 step using the NHANES_FPED data (after 2005)
  - `DII_NHANES_FPED()`, Calculating DII with 1 step using the NHANES_FPED data (after 2005)

Note: all NHANES functions allow users to enter the first day data, or the second day data, or first day + second day data and return the results accordingly. See examples later. 

- ASA24
  - `HEI2015_ASA24()`, Calculating HEI2015 with 1 step using the ASA24 data
  - `AHEI_F_ASA24()`, Calculate the AHEI (female only) within 1 step using the ASA24 data
  - `AHEI_M_ASA24()`, Calculate the AHEI (male only) within 1 step using the ASA24 data
  - `DASH_ASA24()`, Calculating DASH with 1 step using the ASA24 data
  - `MED_ASA24()`, Calculating aMED with 1 step using the ASA24 data
  - `DII_ASA24()`, Calculating DII with 1 step using the ASA24 data

- DHQ3
  - `HEI2015_DHQ3()`, Calculating HEI2015 with 1 step using the DHQ3 data
  - `AHEI_DHQ3()`, Calculate the AHEI (female or male) within 1 step using the DHQ3 data
  - `DASH_DHQ3()`, Calculating DASH with 1 step using the DHQ3 data. The data is Detailed analysis file, ending with detail.csv
  - `MED_DHQ3()`, Calculating aMED with 1 step using the DHQ3 data
  
- BLOCK
  - `HEI2015_BLOCK()`, Calculating HEI2015 with 1 step using the BLOCK data
  - `MED_BLOCK()`, Calculating aMED with 1 step using the BLOCK data
  - `DII_BLOCK()`, Calculating DII with 1 step using the BLOCK data
  - `DASH_BLOCK()`, Calculating DASH with 1 step using the BLOCK data
  - `AHEI_BLOCK()`, Calculating AHEI with 1 step using the BLOCK data
  - `AHEIP_BLOCK()`, Calculating AHEIP with 1 step using the BLOCK data


### Examples:
___

#### Calculating HEI2020 for NHANES_FPED
```
# Using the first day nutrition data
FPED_PATH_1 = "/Users/james/Desktop/fped_dr1tot_1718.sas7bdat"
NUTRIENT_PATH_1 = "/Users/james/Desktop/DR1TOT_J.XPT"
DEMO_PATH_1 = "/Users/james/Desktop/DEMO_J.XPT"

HEI2020_NHANES_FPED(FPED_PATH=FPED_PATH_1, NUTRIENT_PATH=NUTRIENT_PATH_1, DEMO_PATH=DEMO_PATH_1)

# Use the NHANES example data in 2017-2018 using the first day + second day nutrition data
data("NHANES_20172018")
HEI2020_NHANES_FPED(FPED_PATH=NHANES_20172018$FPED, NUTRIENT_PATH=NHANES_20172018$NUTRIENT, DEMO_PATH=NHANES_20172018$DEMO, FPED_PATH2=NHANES_20172018$FPED2, NUTRIENT_PATH2=NHANES_20172018$NUTRIENT2)

# Use the NHANES example data in 2017-2018 using only the first day nutrition data
data("NHANES_20172018")
HEI2020_NHANES_FPED(FPED_PATH=NHANES_20172018$FPED, NUTRIENT_PATH=NHANES_20172018$NUTRIENT, DEMO_PATH=NHANES_20172018$DEMO)

# Use the NHANES example data in 2017-2018 using only the second day nutrition data
data("NHANES_20172018")
HEI2020_NHANES_FPED(DEMO_PATH=NHANES_20172018$DEMO, FPED_PATH2=NHANES_20172018$FPED2, NUTRIENT_PATH2=NHANES_20172018$NUTRIENT2) 
# the sequence of DEMO, FPED, NUTRIENT data entry do not matter if you use "DEMO_PATH", "FPED_PATH2", "NUTRIENT_PATH2" to specify the data input
```

#### Calculating HEI2015 for NHANES_FPED
```
# Using the first day nutrition data
FPED_PATH_1 = "/Users/james/Desktop/fped_dr1tot_1718.sas7bdat"
NUTRIENT_PATH_1 = "/Users/james/Desktop/DR1TOT_J.XPT"
DEMO_PATH_1 = "/Users/james/Desktop/DEMO_J.XPT"

HEI2015_NHANES_FPED(FPED_PATH=FPED_PATH_1, NUTRIENT_PATH=NUTRIENT_PATH_1, DEMO_PATH=DEMO_PATH_1)

# Use the NHANES example data in 2017-2018 using the first day + second day nutrition data
data("NHANES_20172018")
HEI2015_NHANES_FPED(FPED_PATH=NHANES_20172018$FPED, NUTRIENT_PATH=NHANES_20172018$NUTRIENT, DEMO_PATH=NHANES_20172018$DEMO, FPED_PATH2=NHANES_20172018$FPED2, NUTRIENT_PATH2=NHANES_20172018$NUTRIENT2)
```

#### Calculating AHEI for NHANES_FPED
```
FPED_IND_PATH = "/Users/james/Desktop/data/fped_dr1iff.sas7bdat"
NUTRIENT_IND_PATH = "/Users/james/Desktop/data/DR1IFF_J"

AHEI_NHANES_FPED(FPED_IND_PATH, NUTRIENT_IND_PATH)

# Use the NHANES example data in 2017-2018 using the first day + second day nutrition data
data("NHANES_20172018")
AHEI_NHANES_FPED(NHANES_20172018$FPED_IND, NHANES_20172018$NUTRIENT_IND, NHANES_20172018$FPED_IND2, NHANES_20172018$NUTRIENT_IND2)
```

#### Calculating aMED for NHANES_FPED
```
FPED_PATH = "/Users/james/Desktop/fped_dr1tot_1718.sas7bdat"
NUTRIENT_PATH = "/Users/james/Desktop/DR1TOT_J.XPT"
DEMO_PATH = "/Users/james/Desktop/DEMO_J.XPT"

MED_NHANES_FPED(FPED_PATH, NUTRIENT_PATH, DEMO_PATH)

# Use the NHANES example data in 2017-2018 using the first day + second day nutrition data
data("NHANES_20172018")
MED_NHANES_FPED(FPED_PATH=NHANES_20172018$FPED, NUTRIENT_PATH=NHANES_20172018$NUTRIENT, DEMO_PATH=NHANES_20172018$DEMO, FPED_PATH2=NHANES_20172018$FPED2, NUTRIENT_PATH2=NHANES_20172018$NUTRIENT2)
```

#### Calculating MEDI for NHANES_FPED
```
FPED_IND_PATH = "/Users/james/Desktop/data/fped_dr1iff.sas7bdat"
NUTRIENT_IND_PATH = "/Users/james/Desktop/data/DR1IFF_J"

MEDI_NHANES_FPED(FPED_IND_PATH, NUTRIENT_IND_PATH)

# Use the NHANES example data in 2017-2018 using the first day + second day nutrition data
data("NHANES_20172018")
MEDI_NHANES_FPED(FPED_IND_PATH=NHANES_20172018$FPED_IND, NUTRIENT_IND_PATH=NHANES_20172018$NUTRIENT_IND, FPED_IND_PATH2=NHANES_20172018$FPED_IND2, NUTRIENT_IND_PATH2=NHANES_20172018$NUTRIENT_IND2)
```

#### Calculating DASH for NHANES_FPED
```
FPED_IND_PATH = "/Users/james/Desktop/data/fped_dr1iff.sas7bdat"
NUTRIENT_IND_PATH = "/Users/james/Desktop/data/DR1IFF_J"

DASH_NHANES_FPED(FPED_IND_PATH, NUTRIENT_IND_PATH)

# Use the NHANES example data in 2017-2018 using the first day + second day nutrition data
data("NHANES_20172018")
DASH_NHANES_FPED(NHANES_20172018$FPED_IND, NHANES_20172018$NUTRIENT_IND, NHANES_20172018$FPED_IND2, NHANES_20172018$NUTRIENT_IND2)
```

#### Calculating DASHI for NHANES_FPED
```
FPED_IND_PATH = "/Users/james/Desktop/data/fped_dr1iff.sas7bdat"
NUTRIENT_IND_PATH = "/Users/james/Desktop/data/DR1IFF_J"

DASHI_NHANES_FPED(FPED_IND_PATH, NUTRIENT_IND_PATH)

# Use the NHANES example data in 2017-2018 using the first day + second day nutrition data
data("NHANES_20172018")
DASHI_NHANES_FPED(NUTRIENT_PATH = NHANES_20172018$NUTRIENT, NUTRIENT_PATH2 = NHANES_20172018$NUTRIENT2)
```

#### Calculating DII for NHANES_FPED
```
FPED_PATH = "/Users/james/Desktop/fped_dr1tot_1718.sas7bdat"
NUTRIENT_PATH = "/Users/james/Desktop/DR1TOT_J.XPT"
DEMO_PATH = "/Users/james/Desktop/DEMO_J.XPT"

DII_NHANES_FPED(FPED_PATH, NUTRIENT_PATH, DEMO_PATH)

# Use the NHANES example data in 2017-2018 using the first day + second day nutrition data
data("NHANES_20172018")
DII_NHANES_FPED(FPED_PATH=NHANES_20172018$FPED, NUTRIENT_PATH=NHANES_20172018$NUTRIENT, DEMO_PATH=NHANES_20172018$DEMO, FPED_PATH2=NHANES_20172018$FPED2, NUTRIENT_PATH2=NHANES_20172018$NUTRIENT2)
```

#### Calculating HEI2015 for ASA24
```
DATA_PATH = "/Users/james/Desktop/data/Totals.csv"
HEI2015_ASA24(DATA_PATH)

#Use the example data
data("ASA24_exp")
HEI2015_ASA24(ASA24_exp)
```
#### Calculating aMED for ASA24
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

#### Calculating aMED for DHQ3
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

#### Calculating aMED for BLOCK
```
#Use the example data
data("BLOCK_exp")
MED_BLOCK(BLOCK_exp)
```

#### Calculating HEI2020 for your own dietary assessment tool
```
#Use the example data
data("HEI2020_VALIDATION")
HEI2020(SERV_DATA = HEI2020_VALIDATION,RESPONDENTID = HEI2020_VALIDATION$id,AGE = HEI2020_VALIDATION$age,TOTALKCAL_HEI2020 = HEI2020_VALIDATION$kcal,TOTALFRT_SERV_HEI2020 = HEI2020_VALIDATION$total_fruit,FRT_SERV_HEI2020 = HEI2020_VALIDATION$whole_fruit,VEG_SERV_HEI2020 = HEI2020_VALIDATION$total_vegetable,GREENNBEAN_SERV_HEI2020 = HEI2020_VALIDATION$green_and_bean,TOTALPRO_SERV_HEI2020 = HEI2020_VALIDATION$total_protein,SEAPLANTPRO_SERV_HEI2020 = HEI2020_VALIDATION$seafood_plant_protein,WHOLEGRAIN_SERV_HEI2020 = HEI2020_VALIDATION$whole_grain,DAIRY_SERV_HEI2020 = HEI2020_VALIDATION$dairy,FATTYACID_SERV_HEI2020 = HEI2020_VALIDATION$fatty_acid,REFINEDGRAIN_SERV_HEI2020 = HEI2020_VALIDATION$refined_grain,SODIUM_SERV_HEI2020 = HEI2020_VALIDATION$sodium,ADDEDSUGAR_SERV_HEI2020 = HEI2020_VALIDATION$added_sugar,SATFAT_SERV_HEI2020 = HEI2020_VALIDATION$saturated_fat)

HEI2020_V2(SERV_DATA = HEI2020_VALIDATION,RESPONDENTID = HEI2020_VALIDATION$id,AGE = HEI2020_VALIDATION$age,TOTALKCAL_HEI2020 = HEI2020_VALIDATION$kcal,TOTALFRT_SERV_HEI2020 = HEI2020_VALIDATION$total_fruit,FRT_SERV_HEI2020 = HEI2020_VALIDATION$whole_fruit,VEG_SERV_HEI2020 = HEI2020_VALIDATION$total_vegetable,GREENNBEAN_SERV_HEI2020 = HEI2020_VALIDATION$green_and_bean,TOTALPRO_SERV_HEI2020 = HEI2020_VALIDATION$total_protein,SEAPLANTPRO_SERV_HEI2020 = HEI2020_VALIDATION$seafood_plant_protein,WHOLEGRAIN_SERV_HEI2020 = HEI2020_VALIDATION$whole_grain,DAIRY_SERV_HEI2020 = HEI2020_VALIDATION$dairy,FATTYACID_SERV_HEI2020 = HEI2020_VALIDATION$fatty_acid,REFINEDGRAIN_SERV_HEI2020 = HEI2020_VALIDATION$refined_grain,SODIUM_SERV_HEI2020 = HEI2020_VALIDATION$sodium,ADDEDSUGAR_SERV_HEI2020 = HEI2020_VALIDATION$added_sugar,SATFAT_SERV_HEI2020 = HEI2020_VALIDATION$saturated_fat)

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
#Use the example data
data("DASH_trial")
DASHI(SERV_DATA = DASH_trial, RESPONDENTID = DASH_trial$Diet_Type,TOTALKCAL_DASHI = DASH_trial$Kcal, TOTAL_FAT_DASHI = DASH_trial$Totalfat_Percent, SAT_FAT_DASHI = DASH_trial$Satfat_Percent, PROTEIN_DASHI = DASH_trial$Protein_Percent, CHOLESTEROL_DASHI = DASH_trial$Cholesterol, FIBER_DASHI = DASH_trial$Fiber, POTASSIUM_DASHI = DASH_trial$Potassium, MAGNESIUM_DASHI = DASH_trial$Magnesium, CALCIUM_DASHI = DASH_trial$Calcium, SODIUM_DASHI = DASH_trial$Sodium)
```

#### Calculating aMED for your own dietary assessment tool
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

#### Calculating PHDI for your own dietary assessment tool
```
#Use the example data
data("PHDI_VALIDATION")

PHDI(SERV_DATA=PHDI_VALIDATION, PHDI_VALIDATION$id, PHDI_VALIDATION$gender, PHDI_VALIDATION$TOTALKCAL_PHDI, PHDI_VALIDATION$WGRAIN_SERV_PHDI, PHDI_VALIDATION$STARCHY_VEG_SERV_PHDI, PHDI_VALIDATION$VEG_SERV_PHDI, PHDI_VALIDATION$FRT_SERV_PHDI, PHDI_VALIDATION$DAIRY_SERV_PHDI, PHDI_VALIDATION$REDPROC_MEAT_SERV_PHDI, PHDI_VALIDATION$POULTRY_SERV_PHDI, PHDI_VALIDATION$EGG_SERV_PHDI, PHDI_VALIDATION$FISH_SERV_PHDI, PHDI_VALIDATION$NUTS_SERV_PHDI, PHDI_VALIDATION$LEGUMES_SERV_PHDI, PHDI_VALIDATION$SOY_SERV_PHDI, PHDI_VALIDATION$ADDED_FAT_UNSAT_SERV_PHDI, PHDI_VALIDATION$ADDED_FAT_SAT_TRANS_SERV_PHDI, PHDI_VALIDATION$ADDED_SUGAR_SERV_PHDI)
```

#### Merge 2 dietary index results together
```
data(NHANES_20152016)
data(NHANES_20172018)

# HEI_NHANES_FPED
## 2017-2018 day 1 and day 2
HEI2020_NHANES_FPED_1718 = HEI2020_NHANES_FPED(
    FPED_PATH=NHANES_20172018$FPED, 
    NUTRIENT_PATH=NHANES_20172018$NUTRIENT, 
    DEMO_PATH=NHANES_20172018$DEMO, 
    FPED_PATH2=NHANES_20172018$FPED2, 
    NUTRIENT_PATH2=NHANES_20172018$NUTRIENT2)

## 2015-2016 day 1 and day 2
HEI2020_NHANES_FPED_1516 = HEI2020_NHANES_FPED(
    FPED_PATH=NHANES_20152016$FPED, 
    NUTRIENT_PATH=NHANES_20152016$NUTRIENT, 
    DEMO_PATH=NHANES_20152016$DEMO, 
    FPED_PATH2=NHANES_20152016$FPED2, 
    NUTRIENT_PATH2=NHANES_20152016$NUTRIENT2)

# Now, merge the two datasets
HEI2020_NHANES_FPED_17181516 = rbind(HEI2020_NHANES_FPED_1718, HEI2020_NHANES_FPED_1516)

HEI2020_NHANES_FPED_17181516_df = as.data.frame(HEI2020_NHANES_FPED_17181516)
# Save the result on your computer
readr::write_csv(HEI2020_NHANES_FPED_17181516_df, "/your_output_file_location/HEI2020_NHANES_FPED_17181516_df.csv")
```

#### Merge dietary index results with other NHANES data
```
data(NHANES_20172018)

# HEI_NHANES_FPED
## 2017-2018 day 1 and day 2
HEI2020_NHANES_FPED_1718 = HEI2020_NHANES_FPED(
    FPED_PATH=NHANES_20172018$FPED, 
    NUTRIENT_PATH=NHANES_20172018$NUTRIENT, 
    DEMO_PATH=NHANES_20172018$DEMO, 
    FPED_PATH2=NHANES_20172018$FPED2, 
    NUTRIENT_PATH2=NHANES_20172018$NUTRIENT2)

# Blood pressure data
## library(haven)
BP_1718 = read_xpt("BPX_J.XPT")

## library(dplyr)
BP_HEI2020_1718 = inner_join(HEI2020_NHANES_FPED_1718, BP_1718, by="SEQN")
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
