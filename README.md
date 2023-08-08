# dietaryindex
<img src="https://github.com/jamesjiadazhan/dietaryindex/assets/108076575/6d867440-98d7-4a61-b436-15f4de55eaa4" width=350>

___
## Overview
___

**dietaryindex** is an R package that provides user-friendly, streamlined methods for standardizing the compilation of dietary intake data into index-based dietary patterns to enable the assessment of adherence to these patterns in epidemiologic and clinical studies, promoting precision nutrition.

If you are using the Dietaryindex package in your research, **please be sure to cite our original work**. By doing so, you not only add credibility to your findings but also recognize and appreciate our intellectual efforts and contributions. The appropriate citation is as follows:

- *Jiada James Zhan, Rebecca A Hodge, Anne Dunlop, et al. Dietaryindex: A User-Friendly and Versatile R Package for Standardizing Dietary Pattern Analysis in Epidemiological and Clinical Studies. bioRxiv. Published online August 07, 2023:2023.08.07.548466. doi:10.1101/2023.08.07.548466*



As of version 1.0.2, the package supports the latest version of the Healthy Eating Index (HEI2020) for National Health and Nutrition Examination Survey (NHANES) data. NHANES data from 2005 to 2018 are compiled and available within the dietaryindex package. Data can be accessed with commands like `data("NHANES_20172018")` or `data("NHANES_20152016")`. Also, all NHANES functions allow users to enter the first day data, or the second day data, or first day + second day data and return the results accordingly. 

The **dietaryindex** package performs calculations in two steps:
1. Computation of the serving size of each food and nutrient category.
2. Computation of the individual dietary index using the serving size information.

This package can calculate the following dietary pattern indexes:
- Healthy Eating Index 2020 (HEI2020 & HEI-Toddlers-2020) 
- Healthy Eating Index 2015 (HEI2015)
- Alternative Healthy Eating Index (AHEI)
- Dietary Approaches to Stop Hypertension Index (DASH)
- DASH Index in serving sizes from the DASH trial (DASHI)
- Alternate Mediterranean Diet Score (aMED)
- MED Index in serving sizes from the PREDIMED trial (MEDI)
- Dietary Inflammation Index (DII)
- American Cancer Society 2020 diet score (ACS2020_V1 and ACS2020_V2)
- Planetary Health Diet Index from the EAT-Lancet Commission (PHDI)

**dietaryindex** has compiled NHANES data from 2005 - 2020 for your convenience. This includes NHANES 2005-2006, 2007-2008, 2009-2010, 2011-2012, 2013-2014, 2015-2016, 2017-2018, 2017-2020. To retrieve the data:
- Download the **NHANES_combined** folder from the Google Drive (https://drive.google.com/drive/u/2/folders/1umjhuS22aHEW_bU5AjYa8vrae91gsb0D)
- Download the **NHANES_combined** folder from the GitHub dietaryindex_NHANES page (https://github.com/jamesjiadazhan/dietaryindex_NHANES/tree/main/data/NHANES_combined)


Then, use the following codes to load the data:
```
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

## NHANES 2017-2018 from dietaryindex package
data("NHANES_20172018")

## NHANES 2017-2020
load("NHANES_20172020.rda")

```

For a detailed explanation of these indexes, please check the attached Excel files:
- [dietaryindex_SERVING_SIZE_DEFINITION.xlsx](https://github.com/jamesjiadazhan/dietaryindex/blob/main/dietaryindex_SERVING_SIZE_DEFINITION.xlsx)
- [dietaryindex_SCORING_ALGORITHM.xlsx](https://github.com/jamesjiadazhan/dietaryindex/blob/main/dietaryindex_SCORING_ALGORITHM.xlsx)

**dietaryindex** has been thoroughly validated for accuracy and reliability. We've ensured that all functions within **dietaryindex** perform as expected. Validation files and R codes can be found here: 
- [Validation.md](https://github.com/jamesjiadazhan/dietaryindex/blob/main/Validation%20file%20for%20publication/Validation.md)

Package dependencies: **dplyr**, **readr**, **haven** (automatically installed).


## Installation
___

Currently, **dietaryindex** is not available on [CRAN]

To install from this GitHub repository, use the **devtools** package:

```
install.packages("devtools") #If you don't have "devtools" installed already
devtools::install_github("jamesjiadazhan/dietaryindex") # Install the package from GitHub
```

If the previous steps are not working, you can try the following steps:
```
library(devtools) # Load devtools
install_github("jamesjiadazhan/dietaryindex")
```

If it is still not working, you can download the package directly and then install it in your local folder. You can:
1. Download the package as a zip file
<img width="500" alt="image" src="https://github.com/jamesjiadazhan/dietaryindex/assets/108076575/75a4f099-d407-4825-bfa1-e01b1f316c97">

2. Unzip it
   
3. Use the following code to install the package locally. Remember to change the file path to your own file path:
- Windows:
  <img width="400" alt="image" src="https://github.com/jamesjiadazhan/dietaryindex/assets/108076575/b9b811e9-869b-43a0-bf8c-4575ef0bc7b6">
- Mac: right click the file and then press the Option key to show the path bar momentarily
<img width="265" alt="image" src="https://github.com/jamesjiadazhan/dietaryindex/assets/108076575/4d980a96-11f4-4852-9263-a11752add444">


```
install.packages("/Users/james/Desktop/Emory University - Ph.D./dietaryindex_package/dietaryindex-main", repos = NULL, type = "source")
```


If something happens like the following, first try to enter 1 in the terminal (lower box). If not successful, then try to enter 2. **It will take a while if you are a new R user.**
```
  These packages have more recent versions available.
  It is recommended to update all of them.
  Which would you like to update?

  1: All                          
  2: CRAN packages only           
  3: None                         
  4: tzdb  (0.3.0 -> 0.4.0) [CRAN]
  5: vroom (1.6.1 -> 1.6.3) [CRAN]
```


## Getting Started
___
To start using dietaryindex, load the dependency packages first and then load the dietaryindex package after installation:
```
library(dplyr)
library(haven)
library(readr)
# Loading dependency packages first can help avoid the namespace conflict if you want to use dplyr by yourself later in addition to the internal use of dplyr within the diataryindex package
library(dietaryindex)
```

Detailed function descriptions, examples, and NHANES data access instructions are provided here: 
- [Manual](https://github.com/jamesjiadazhan/dietaryindex/blob/main/Manual.md)


## Demonstrations
___

Case study 1. A comparative analysis of results derived from clinical trials (i.e., The Dietary Approaches to Stop Hypertension (DASH) trial and Prevención con Dieta Mediterránea (PREDIMED) trial) juxtaposed with findings from an epidemiological study (i.e., NHANES) from 2017-2018, utilizing DASHI and MEDI dietary indexes.
![Figure 3  DASHI and MEDI dietary indexes in DASH and PREDIMED trials and NHANES 17-18](https://github.com/jamesjiadazhan/dietaryindex/assets/108076575/c0b79f28-7856-471a-8fde-8ce81de80517)


Case study 2. A time series of cross-sectional computation of the HEI2020 in the NHANES dataset spanning 2005 to 2018, stratifying into toddler and non-toddler populations.
![Figure 4  HEI2020 from 2005 to 2018](https://github.com/jamesjiadazhan/dietaryindex/assets/108076575/3606ef7b-56e6-450d-895f-9f00c2e984cb)


Case study 3. A comprehensive calculation of multiple dietary indexes—HEI2020, AHEI, DASH, DASHI, MED, MEDI, DII—within a single year (2017-2018), leveraging data from the NHANES study.
![Figure 5  Multiple Dietary indexes, using the NHANES data in 2017-2018](https://github.com/jamesjiadazhan/dietaryindex/assets/108076575/85218a10-8bd1-4d39-a08f-09f2a0945d3c)


All the R codes for the demonstrations can be found here: 
- [Case study 1,2,3.r](https://github.com/jamesjiadazhan/dietaryindex/blob/main/Case%20study%201%2C2%2C3.r)


## Related Work
___

**dietaryindex** is mainly intended as a versatile tool to help for calculating different dietary indexes conveniently. It is designed to be flexible to work for almost all types of dietary assessment tools, including food frequency questionnaires, 24-hour dietary recalls, and even food records, while it also supports many 1-step dietary index calculations for NHANES, ASA24, and DHQ3.  Please follow the instruction of your specific dietary assessment tools and relevant articles regarding how to accurately define the serving size (see above) if it is not provided in our package, as they are the key to obtaining high-quality dietary indexes. **dietaryindex** also provides some help in defining the serving size in the help file, argument section. Note: some very specific dietary index components (low-fat dairy products and sugar-sweetened beverages) are not easily available and thus are difficult to assess. The author used individual-level food data to compute the population-level food group data. For example, the sugar-sweetened beverage serving is estimated by dividing the total added sugar intake in grams from beverages by 26, because 1 bottle (8 oz) of Coke has 26 g added sugars and this is used as the benchmark, as different sugar-sweetened beverages have largely different added sugar contents. Please use your own judgment to determine if the dietary indexes calculated using the **dietaryindex** package is appropriate for your research.

Just to emphasize, detailed function descriptions, examples, and NHANES data access instructions are provided here: 
- [Manual](https://github.com/jamesjiadazhan/dietaryindex/blob/main/Manual.md)

For NHANES data:

- FPED population file refers to the files in the Food Patterns equivalents for foods in the WWEIA (https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/fped-databases/). They look like FPED_DR1TOT or FPED_DR2TOT.
  - The FPED individual files look like FPED_DR1IFF or FPED_DR2IFF.

- NUTRIENT population file refers to the files in the Dietary Interview - Total Nutrient Intakes, First Day or Second Day (example: 05-06 https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Dietary&CycleBeginYear=2005). They look like 
  - The NUTRIENT individual files look like DR1IFF_D or DR2IFF_D.

- DEMO file refers to the DEMO file in the Demographic Variables & Sample Weights (example: 05-06 https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics&CycleBeginYear=2005)

FPED, NUTRIENT, and DEMO files are available within the package and in the Google Drive collected by the package developer for your convenience (https://drive.google.com/drive/folders/1umjhuS22aHEW_bU5AjYa8vrae91gsb0D?usp=share_link). 

### Contributing & Notes
**dietaryindex** is licensed under the [MIT License](https://github.com/jamesjiadazhan/dietaryindex/blob/main/other/LICENSE.txt). Please check out the [Contribution guide](https://github.com/jamesjiadazhan/dietaryindex/blob/main/CONTRIBUTING.md) for questions, feature requests, and bug reports. The maintainer will review pull requests and incorporate contributions at his discretion. You may also reach out to the maintainer, **Jiada (James) Zhan**, via his email: jzha832@emory.edu. **Jiada (James) Zhan** home page at Emory is: https://www.sph.emory.edu/phd-students/profile/index.php?FID=jiada-zhan-12906. **Becky Hodge** at the American Cancer Society provided substantial contributions to validate this package. **Michael L Orr** at Dean Jones/ Young-Mi Go Lab at Emory University helped design the dietaryindex logo. Thanks a lot for their help.
