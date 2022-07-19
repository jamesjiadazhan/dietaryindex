## dietaryindex

### Overview
___

Version 0.7.0: now support XPT and SAS file importing and calculating the HEI2015, AHEI, DASH, MED for the NHANES data

The main goal of this package **dietaryindex** is for calculating different dietary pattern indexes or scores easily and conveniently. 

**dietaryindex** calculates dietary indexes by 2 steps:
1. Calculate the serving size of each food and nutrient category
2. Calculate the individual dietary index

Currently, the **dietaryindex** package can 3 things to calculate dietary indexes within 1-2 steps.
    1. It can calculate AHEI, AHEIP, DASH, DASHI, MED, MEDI, and HEI2015 for the Block FFQ. 
    2. It can calculate AHEI, DASH, MED, and HEI2015 for the NHANES_FPED (after 2005).
    3. It can calculate HEI2015 for the ASA24

This package can also help you calculating these dietary pattern indexes from all other dietary assessments (e.g. other FFQ, food record), if the relevant serving sizes for each food/nutrient category in the index are provided. All you need to do is to provide the relevant serving sizes for each food/nutrient category in the index.

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

The **dietaryindex** package currently contains 14 key functions:
>`HEI2015()`, Healthy Eating Index 2015 (https://www.fns.usda.gov/how-hei-scored)

>`AHEI()`, alternative healthy eating index (https://pubmed.ncbi.nlm.nih.gov/22513989/)

>`AHEIP()` , alternative healthy eating index - pregnancy (https://pubmed.ncbi.nlm.nih.gov/19465182/)

>`DASH()`, Dietary Approaches to Stop Hypertension (https://pubmed.ncbi.nlm.nih.gov/18413553/)

>`DASHI()`, Dietary Approaches to Stop Hypertension Index (modified from multiple sources to provide most updated serving size-based DASH index, including https://www.nhlbi.nih.gov/education/dash-eating-plan, https://pubmed.ncbi.nlm.nih.gov/17324731/, https://www.dietaryguidelines.gov/sites/default/files/2020-12/Dietary_Guidelines_for_Americans_2020-2025.pdf, https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/414155, https://www.nejm.org/doi/full/10.1056/nejm199704173361601)

>`MED()`, Mediterranean diet (https://pubmed.ncbi.nlm.nih.gov/33574608/)

>`MEDI()`, Mediterranean diet index, serving size-based (https://pubmed.ncbi.nlm.nih.gov/28160450/)

>`HEI2015_SERV()`, Calculate the serving sizes needed for calculating the HEI2015 dietary index per 1 day

>`AHEI_SERV()`, Calculate the serving sizes needed for calculating the AHEI dietary index per 1 day

>`AHEIP_SERV()` ,Calculate the serving sizes needed for calculating the AHEIP dietary index per 1 day

>`DASH_SERV()`, Calculate the serving sizes needed for calculating the DASH dietary index per 1 day

>`DASHI_SERV()`, Calculate the serving sizes needed for calculating the DASHI dietary index per 1 day

>`MED_SERV()`, Calculate the serving sizes needed for calculating the MED dietary index per 1 day

>`MEDI_SERV()`, Calculate the serving sizes needed for calculating the MEDI dietary index per 1 day

>`HEI2015_NHANES_FPED()`, Calculating the serving sizes for HEI2015 with 1 step using the NHANES_FPED data (after 2005)

>`AHEI_NHANES_FPED()`, Calculating the serving sizes for AHEI with 1 step using the NHANES_FPED data (after 2005)

>`DASH_NHANES_FPED()`, Calculating the serving sizes for DASH with 1 step using the NHANES_FPED data (after 2005)

>`MED_NHANES_FPED()`, Calculating the serving sizes for MED with 1 step using the NHANES_FPED data (after 2005)

>`HEI2015_ASA24()`, Calculating the serving sizes for HEI2015 with 1 step using the ASA24 data

### Examples:
___
#### Calculating AHEI for BLOCK
```
DATA_PATH <- "/Users/james/Desktop/data.csv"
RAW_DATA <- read_csv(DATA_PATH)

AHEI_DATA = AHEI_SERV(RAW_DATA, TYPE="BLOCK")
AHEI_DATA2 = AHEI(AHEI_DATA,   
                  VEG_SERV,
                  FRT_SERV,
                  WGRAIN_SERV,
                  NUTSLEG_SERV,
                  N3FAT_SERV,
                  PUFA_SERV,
                  SSB_FRTJ_SERV,
                  REDPROC_MEAT_SERV,
                  TRANS_SERV,
                  SODIUM_SERV,
                  ALCOHOL_SERV)
AHEI_DATA2
```

#### Calculating HEI2015 for NHANES_FPED
```
FPED_PATH = "/Users/james/Desktop/fped_dr1tot_1718.sas7bdat"
NUTRIENT_PATH = "/Users/james/Desktop/DR1TOT_J.XPT"
DEMO_PATH = "/Users/james/Desktop/DEMO_J.XPT"

HEI2015RESULT = HEI2015_NHANES_FPED(FPED_PATH, NUTRIENT_PATH, DEMO_PATH)
```

### Related Work
___

**dietaryindex** is mainly intended as a tool to help for calculating different dietary indexes with given food/nutrient serving sizes. It is designed to be flexible to work for almost all types of dietary assessment tools, including food frequency questionnaires, 24-hours dietary recalls, and even food records. Please follow the instruction of your specific dietary assessment tools and relevant articles regarding how to accurately define the serving size (see above) if it is not provided in our package, as they are the key to obtain high-quality dietary indexes. **dietaryindex** also provides some help in defining the serving size in the help file, argument section. 

This package requires the **dplyr**, **readr**, and **haven** packages to be installed. Library statements of the dplyr, readr, and haven packages are included for your convenience. 

For NHANES data, FPED file refers to the DR1TOT file in the Food Patterns equivalents for foods in the WWEIA (https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/fped-databases/). This is a zip file, so please unzip this file first to retrieve the SAS file. NUTRIENT file refers to the DR1TOT file in the Dietary Interview - Total Nutrient Intakes, First Day (example: 05-06 https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Dietary&CycleBeginYear=2005). DEMO file refers to the DEMO file in the Demographic Variables & Sample Weights (example: 05-06 https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics&CycleBeginYear=2005)

### Contributing

**dietaryindex** is licensed under the [MIT License]. Please check out the [Contribution guide](https://github.com/jamesjiadazhan/dietaryindex/blob/main/CONTRIBUTING.md) for questions, feature requests and bug reports. The maintainer will review pull requests and incorporate contributions at his discretion. You may also reach out to the maintainer, James Jiada Zhan, via his email: jzha832@emory.edu.
