# dietaryindex
<img src="https://github.com/jamesjiadazhan/dietaryindex/assets/108076575/45209422-a330-4e91-b2bf-4bdb829b65de" width=350>


___
## Overview
___

**dietaryindex** is an R package that provides user-friendly, streamlined methods for standardizing the compilation of dietary intake data into index-based dietary patterns to enable the assessment of adherence to these patterns in epidemiologic and clinical studies, promoting precision nutrition.

If you are using the Dietaryindex package in your research, **please be sure to cite our original work**. By doing so, you not only add credibility to your findings but also recognize and appreciate our intellectual efforts and contributions. The appropriate citation is as follows:

- *Jiada James Zhan, Rebecca A Hodge, Anne Dunlop, et al. Dietaryindex: A User-Friendly and Versatile R Package for Standardizing Dietary Pattern Analysis in Epidemiological and Clinical Studies. bioRxiv. Published online August 07, 2023:2023.08.07.548466. doi:10.1101/2023.08.07.548466*
- https://www.biorxiv.org/content/10.1101/2023.08.07.548466v2


As of version 1.0.3, the package supports the latest version of the Healthy Eating Index (HEI2020 and HEI-Toddlers 2020) for National Health and Nutrition Examination Survey (NHANES) data and ASA24. Now ASA24 can generate flexible outputs, depending on if RECALL_SUMMARIZE is TRUE or FALSE. If RECALL_SUMMARIZE is TRUE, all dietary recall results will be summarized via average. If not, all individual recall results will be shown. Also, all NHANES functions allow users to enter the first day data, or the second day data, or first day + second day data and return the results accordingly. 

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


**dietaryindex** has compiled NHANES data from 1999 - 2020 for your convenience. This includes NHANES 1999-2000, 2001-2002, 2003-2004, 2005-2006, 2007-2008, 2009-2010, 2011-2012, 2013-2014, 2015-2016, 2017-2018, 2017-2020. To retrieve the data:
- Download the **NHANES_combined** folder from the Google Drive (https://drive.google.com/drive/u/2/folders/1umjhuS22aHEW_bU5AjYa8vrae91gsb0D)
- Download the **NHANES_combined** folder from the GitHub dietaryindex_NHANES page (https://github.com/jamesjiadazhan/dietaryindex_NHANES/tree/main/data/NHANES_combined)


Then, use the following codes to load the data:
```
# set up working dictionary
setwd("/Users/james/Desktop/NHANES_combined")

# Load the NHANES data from 2005 to 2020
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


For NHANES data:

- FPED population file refers to the files in the Food Patterns equivalents for foods in the WWEIA (https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/fped-databases/). They look like FPED_DR1TOT or FPED_DR2TOT.
  - ![image](https://github.com/jamesjiadazhan/dietaryindex/assets/108076575/bffd4196-fdc8-40ce-be83-21d701a5f408)


- The FPED individual files look like FPED_DR1IFF or FPED_DR2IFF.
  - ![image](https://github.com/jamesjiadazhan/dietaryindex/assets/108076575/199ba4bd-702f-4530-9f44-8dfab4c6d9f0)
 

- NUTRIENT population file refers to the files in the Dietary Interview - Total Nutrient Intakes, First Day or Second Day (example: 05-06 https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Dietary&CycleBeginYear=2005). They look like DR1TOT_J or DR2TOT_J
  - ![image](https://github.com/jamesjiadazhan/dietaryindex/assets/108076575/a36e1de0-c7b0-4c48-a320-55c7b5034c5d)
 
- The NUTRIENT individual files look like DR1IFF_D or DR2IFF_D.
  - ![image](https://github.com/jamesjiadazhan/dietaryindex/assets/108076575/721e7677-fcac-40e0-8a9b-1b61cfcaf180)
 

- DEMO file refers to the DEMO file in the Demographic Variables & Sample Weights (example: 05-06 https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics&CycleBeginYear=2005)
  - ![image](https://github.com/jamesjiadazhan/dietaryindex/assets/108076575/75898baa-c615-4446-bca2-9ccd01bf5879)


For a detailed explanation of these indexes, please check the attached Excel files:
- [dietaryindex_SERVING_SIZE_DEFINITION.xlsx](https://github.com/jamesjiadazhan/dietaryindex/blob/main/dietaryindex_SERVING_SIZE_DEFINITION.xlsx)
- [dietaryindex_SCORING_ALGORITHM.xlsx](https://github.com/jamesjiadazhan/dietaryindex/blob/main/dietaryindex_SCORING_ALGORITHM.xlsx)

**dietaryindex** has been thoroughly validated for accuracy and reliability. We've ensured that all functions within **dietaryindex** perform as expected. Validation files and R codes can be found here: 
- [Validation.md](https://github.com/jamesjiadazhan/dietaryindex/blob/main/Validation%20file%20for%20publication/Validation.md)

Package dependencies: **dplyr**, **readr**, **haven** (automatically installed).



## Detailed function descriptions, examples, and NHANES data access instructions are provided here: 
___
- [Manual with data format and function output overviews](https://jamesjiadazhan.github.io/dietaryindex_manual/)



## Demonstrations
### We included complex survey design when analzying NHANES data so all results are weighted accordingly to represent US population.
___

Case study 1. A comparative analysis of results derived from clinical trials (i.e., The Dietary Approaches to Stop Hypertension (DASH) trial and Prevención con Dieta Mediterránea (PREDIMED) trial) juxtaposed with findings from an epidemiological study (i.e., NHANES) from 2017-2018, utilizing DASHI and MEDI dietary indexes.
![Figure 5  DASHI and MEDI dietary indexes comparison](https://github.com/jamesjiadazhan/dietaryindex/assets/108076575/1e4b391a-faea-45c7-913b-65fee2349e47)


Case study 2. A time series of cross-sectional computation of the HEI2020 in the NHANES dataset spanning 2005 to 2018, stratifying into toddler and non-toddler populations.
![Figure 6  HEI2020 from 2005 to 2018](https://github.com/jamesjiadazhan/dietaryindex/assets/108076575/eb61257b-afae-497a-8263-1e677356a060)


Case study 3. A comprehensive calculation of multiple dietary indexes—HEI2020, AHEI, DASH, DASHI, MED, MEDI, DII—within a single year (2017-2018), leveraging data from the NHANES study.
![Figure 7  Multiple Dietary indexes, using the NHANES 17-18](https://github.com/jamesjiadazhan/dietaryindex/assets/108076575/b33705e6-2ab6-4019-958e-dda0ec2d1c71)


All the R codes for the demonstrations can be found here: 
- [Case study 1,2,3.r](https://github.com/jamesjiadazhan/dietaryindex/blob/main/Case%20study%201%2C2%2C3.r)


## Related Work
___

**dietaryindex** is mainly intended as a versatile tool to help for calculating different dietary indexes conveniently. It is designed to be flexible to work for almost all types of dietary assessment tools, including food frequency questionnaires, 24-hour dietary recalls, and even food records, while it also supports many 1-step dietary index calculations for NHANES, ASA24, and DHQ3.  Please follow the instruction of your specific dietary assessment tools and relevant articles regarding how to accurately define the serving size (see above) if it is not provided in our package, as they are the key to obtaining high-quality dietary indexes. **dietaryindex** also provides some help in defining the serving size in the help file, argument section. Note: some very specific dietary index components (low-fat dairy products and sugar-sweetened beverages) are not easily available and thus are difficult to assess. The author used individual-level food data to compute the population-level food group data. For example, the sugar-sweetened beverage serving is estimated by dividing the total added sugar intake in grams from beverages by 26, because 1 bottle (8 oz) of Coke has 26 g added sugars and this is used as the benchmark, as different sugar-sweetened beverages have largely different added sugar contents. Please use your own judgment to determine if the dietary indexes calculated using the **dietaryindex** package is appropriate for your research.


 


### Contributing & Notes
**dietaryindex** is licensed under the [MIT License](https://github.com/jamesjiadazhan/dietaryindex/blob/main/other/LICENSE.txt). Please check out the [Contribution guide](https://github.com/jamesjiadazhan/dietaryindex/blob/main/CONTRIBUTING.md) for questions, feature requests, and bug reports. The maintainer will review pull requests and incorporate contributions at his discretion. You may also reach out to the maintainer, **Jiada (James) Zhan**, via his email: jzha832@emory.edu. **Jiada (James) Zhan** home page at Emory is: https://www.sph.emory.edu/phd-students/profile/index.php?FID=jiada-zhan-12906. **Becky Hodge** at the American Cancer Society provided substantial contributions to validate this package. **Michael L Orr** at Dean Jones/ Young-Mi Go Lab at Emory University helped design the dietaryindex logo. Thanks a lot for their help.
