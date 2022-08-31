## dietaryindex

### Overview
___

Version 0.9.0: now support XPT and SAS file importing and calculating the HEI2015, AHEI, DASH, MED for the NHANES data after 2005 (NHANES_FPED). Bugs fixed since version 0.8.0.

The main goal of this package **dietaryindex** is for calculating different dietary pattern indexes or scores easily and conveniently. 

**dietaryindex** calculates dietary indexes by 2 steps:
1. Calculate the serving size of each food and nutrient category
2. Calculate the individual dietary index

Currently, the **dietaryindex** package works for the following 4 dietary assessment tools to calculate many dietary indexes within 1 steps:
1. It can calculate HEI2015, AHEI, AHEIP, DASH, DASHI, MED, MEDI, and DII for the Block FFQ. 
2. It can calculate HEI2015, AHEI, DASH, MED, and DII for the NHANES_FPED (after 2005).
3. It can calculate HEI2015, AHEI, DASH, MED, and DII for the ASA24
4. It can calculate HEI2015, AHEI, DASH, MED, and DII for the NIH-AARP

This package can also help you calculating these dietary pattern indexes from all other dietary assessments, if you provide the relevant serving sizes for each food/nutrient category.
- All you need to do is to provide the relevant serving sizes for each food/nutrient category in the index.
- The excel sheet for the serving size of all dietary indexes is provided: DIETARYINDEX_SERVING_SIZE_CHART_JAMES_ZHAN.xlsx


The **dietaryindex** package relies on the **dplyr**, **readr**, and **haven** packages. Please install them ahead.

### Installation
___

**dietaryindex** is currently not available on [CRAN]


To install the development version hosted on this GitHub repository, use the **devtools** package and the following:

```
# install.packages("devtools")
devtools::install_github("jamesjiadazhan/dietaryindex")
```

To install **dplyr**, **readr**, and **haven** packages if you are new to R or don't have them, use the following:
```
install.packages("dplyr")
install.packages("readr")
install.packages("haven")
```

### Getting Started
___
```
library(dietaryindex)
library(dplyr)
library(readr)
library(haven)
```

The **dietaryindex** package currently contains the following key functions:
>`HEI2015()`, Healthy Eating Index 2015 (https://www.fns.usda.gov/how-hei-scored)

>`AHEI()`, alternative healthy eating index (https://pubmed.ncbi.nlm.nih.gov/22513989/)

>`AHEIP()` , alternative healthy eating index - pregnancy (https://pubmed.ncbi.nlm.nih.gov/19465182/)

>`DASH()`, Dietary Approaches to Stop Hypertension (https://pubmed.ncbi.nlm.nih.gov/18413553/)

>`DASHI()`, Dietary Approaches to Stop Hypertension Index (modified from multiple sources to provide most updated serving size-based DASH index, including https://www.nhlbi.nih.gov/education/dash-eating-plan, https://pubmed.ncbi.nlm.nih.gov/17324731/, https://www.dietaryguidelines.gov/sites/default/files/2020-12/Dietary_Guidelines_for_Americans_2020-2025.pdf, https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/414155, https://www.nejm.org/doi/full/10.1056/nejm199704173361601)

>`MED()`, Mediterranean diet (https://pubmed.ncbi.nlm.nih.gov/33574608/)

>`MEDI()`, Mediterranean diet index, serving size-based (https://pubmed.ncbi.nlm.nih.gov/28160450/)



>`HEI2015_BLOCK()`, Calculate the HEI2015 dietary index with 1 step for the Block FFQ per 1 day

>`AHEI_BLOCK()`, Calculate the AHEI dietary index with 1 step for the Block FFQ per 1 day

>`AHEIP_BLOCK()` ,Calculate the AHEIP dietary index with 1 step for the Block FFQ per 1 day

>`DASH_BLOCK()`, Calculate the DASH dietary index with 1 step for the Block FFQ per 1 day

>`DASHI_BLOCK()`, Calculate the DASHI dietary index with 1 step for the Block FFQ per 1 day

>`MED_BLOCK()`, Calculate the MED dietary index with 1 step for the Block FFQ per 1 day

>`MEDI_BLOCK()`, Calculate the MEDI dietary index with 1 step for the Block FFQ per 1 day

>`DII_BLOCK()`, Calculate the DII dietary index with 1 step for the Block FFQ per 1 day



>`HEI2015_AARP()`, Calculate the HEI2015 dietary index for the NIH-AARP per 1 day



>`HEI2015_NHANES_FPED()`, Calculating the serving sizes for HEI2015 with 1 step using the NHANES_FPED data (after 2005)

>`AHEI_NHANES_FPED()`, Calculating the serving sizes for AHEI with 1 step using the NHANES_FPED data (after 2005)

>`DASH_NHANES_FPED()`, Calculating the serving sizes for DASH with 1 step using the NHANES_FPED data (after 2005)

>`MED_NHANES_FPED()`, Calculating the serving sizes for MED with 1 step using the NHANES_FPED data (after 2005)

>`DII_NHANES_FPED()`, Calculating the serving sizes for DII with 1 step using the NHANES_FPED data (after 2005)



>`HEI2015_ASA24()`, Calculating the serving sizes for HEI2015 with 1 step using the ASA24 data

>`AHEI_F_ASA24()`, Calculate the AHEI (female only) within 1 step using the ASA24 data

>`AHEI_M_ASA24()`, Calculate the AHEI (male only) within 1 step using the ASA24 data

>`DASH_ASA24()`, Calculating the serving sizes for DASH with 1 step using the ASA24 data

>`MED_ASA24()`, Calculating the serving sizes for MED with 1 step using the ASA24 data

>`DII_ASA24()`, Calculating the serving sizes for DII with 1 step using the ASA24 data

### Examples:
___
#### Calculating AHEI for BLOCK
```
DATA_PATH <- "/Users/james/Desktop/data.csv"
RAW_DATA <- read_csv(DATA_PATH)

AHEI_BLOCK = AHEI_SERV(RAW_DATA)

```

#### Calculating HEI2015 for NHANES_FPED
```
FPED_PATH = "/Users/james/Desktop/fped_dr1tot_1718.sas7bdat"
NUTRIENT_PATH = "/Users/james/Desktop/DR1TOT_J.XPT"
DEMO_PATH = "/Users/james/Desktop/DEMO_J.XPT"

HEI2015_NHANES_FPED(FPED_PATH, NUTRIENT_PATH, DEMO_PATH)

```

#### Calculating HEI2015 for ASA24
```
DATA_PATH = "/Users/james/Desktop/data/Totals.csv"

HEI2015_ASA24(DATA_PATH)
```

#### Calculating AHEI for NHANES_FPED
```
FPED_PATH = "/Users/james/Desktop/fped_dr1tot_1718.sas7bdat"
NUTRIENT_PATH = "/Users/james/Desktop/DR1TOT_J.XPT"
DEMO_PATH = "/Users/james/Desktop/DEMO_J.XPT"

AHEI_NHANES_FPED(FPED_PATH, NUTRIENT_PATH, DEMO_PATH)
```

#### Calculating DASH for NHANES_FPED
```
FPED_PATH = "/Users/james/Desktop/data/fpre_dr1tot_1718.sas7bdat"
NUTRIENT_PATH = "/Users/james/Desktop/data/DR1TOT_J.XPT"
DEMO_PATH = "/Users/james/Desktop/data/DEMO_J.XPT"
DBQ_PATH = "/Users/james/Desktop/data/DBQ_J.XPT"

DASH_NHANES_FPED(FPED_PATH, NUTRIENT_PATH, DEMO_PATH, DBQ_PATH)

```

#### Calculating MED for NHANES_FPED
```
FPED_PATH = "/Users/james/Desktop/fped_dr1tot_1718.sas7bdat"
NUTRIENT_PATH = "/Users/james/Desktop/DR1TOT_J.XPT"
DEMO_PATH = "/Users/james/Desktop/DEMO_J.XPT"

MED_NHANES_FPED(FPED_PATH, NUTRIENT_PATH, DEMO_PATH)
```

#### Calculating HEI2015 for your own dietary assessment tool
```
DATA_PATH <- "/Users/james/Desktop/data.csv"
SERV_DATA <- read_csv(DATA_PATH)

AHEI(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$TOTALKCAL, SERV_DATA$VEG_SERV, SERV_DATA$FRT_SERV, SERV_DATA$WGRAIN_SERV, SERV_DATA$NUTSLEG_SERV, SERV_DATA$N3FAT_SERV, SERV_DATA$PUFA_SERV, SERV_DATA$SSB_FRTJ_SERV, SERV_DATA$REDPROC_MEAT_SERV, SERV_DATA$TRANS_SERV, SERV_DATA$SODIUM_SERV, SERV_DATA$ALCOHOL_SERV)
```

#### Calculating AHEI for your own dietary assessment tool
```
DATA_PATH <- "/Users/james/Desktop/data.csv"
SERV_DATA <- read_csv(DATA_PATH)

AHEI(SERV_DATA, RESPONDENTID, VEG_SERV, FRT_SERV, WGRAIN_SERV, NUTSLEG_SERV, N3FAT_SERV, PUFA_SERV, SSB_FRTJ_SERV, REDPROC_MEAT_SERV, TRANS_SERV,SODIUM_SERV, ALCOHOL_SERV)

#The same result as the previous one
AHEI(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$VEG_SERV, SERV_DATA$FRT_SERV, SERV_DATA$WGRAIN_SERV, SERV_DATA$NUTSLEG_SERV, SERV_DATA$N3FAT_SERV, SERV_DATA$PUFA_SERV, SERV_DATA$SSB_FRTJ_SERV, SERV_DATA$REDPROC_MEAT_SERV, SERV_DATA$TRANS_SERV,SODIUM_SERV, SERV_DATA$ALCOHOL_SERV)
```

#### Calculating MED for your own dietary assessment tool
```
DATA_PATH <- "/Users/james/Desktop/data.csv"
SERV_DATA <- read_csv(DATA_PATH)

DASH(SERV_DATA, RESPONDENTID, FRT_FRTJ_SERV, VEG_SERV, NUTSLEG_SERV, WGRAIN_SERV, LOWF_DAIRY_SERV, SODIUM_SERV, REDPROC_MEAT_SERV, SSB_FRTJ_SERV)
DASH(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$FRT_FRTJ_SERV, SERV_DATA$VEG_SERV, SERV_DATA$NUTSLEG_SERV, SERV_DATA$WGRAIN_SERV, SERV_DATA$LOWF_DAIRY_SERV, SERV_DATA$SODIUM_SERV, SERV_DATA$REDPROC_MEAT_SERV, SERV_DATA$SSB_FRTJ_SERV)
```

### Related Work
___

**dietaryindex** is mainly intended as a tool to help for calculating different dietary indexes conveniently. It is designed to be flexible to work for almost all types of dietary assessment tools, including food frequency questionnaires, 24-hours dietary recalls, and even food records, while itself supports many 1-step dietary index calculations for NHANES, ASA24, BLOCK, and AARP.  Please follow the instruction of your specific dietary assessment tools and relevant articles regarding how to accurately define the serving size (see above) if it is not provided in our package, as they are the key to obtain high-quality dietary indexes. **dietaryindex** also provides some help in defining the serving size in the help file, argument section. Note: some very specific dietary index components (low-fat dairy) are difficult to assess, so the author(s) used his best judgment to estimate those components based on the other existing data, such as the Per capita consumption of low fat cottage cheese in the United States from 2000 to 2020 and the proportion of low-fat milk consumption in the NHANES data. Please use your own judgment to determine if the dietary indexes calculated using the **dietaryindex** package is appropriate for your research.

This package requires the **dplyr**, **readr**, and **haven** packages to be installed. Library statements of the dplyr, readr, and haven packages are included for your convenience. 

For NHANES data:
FPED file refers to the DR1TOT file in the Food Patterns equivalents for foods in the WWEIA (https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/fped-databases/). This is a zip file, so please unzip this file first to retrieve the SAS file. 

NUTRIENT file refers to the DR1TOT file in the Dietary Interview - Total Nutrient Intakes, First Day, Dietary Data (example: 05-06 https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Dietary&CycleBeginYear=2005). 

DEMO file refers to the DEMO file in the Demographic Variables & Sample Weights (example: 05-06 https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics&CycleBeginYear=2005)

DBQ file refers to the DBQ file in the Diet Behavior & Nutrition, Questionnaire Data (example: 05-06 https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Questionnaire&CycleBeginYear=2005)

### Contributing

**dietaryindex** is licensed under the [MIT License]. Please check out the [Contribution guide](https://github.com/jamesjiadazhan/dietaryindex/blob/main/CONTRIBUTING.md) for questions, feature requests and bug reports. The maintainer will review pull requests and incorporate contributions at his discretion. You may also reach out to the maintainer, James Jiada Zhan, via his email: jzha832@emory.edu.
