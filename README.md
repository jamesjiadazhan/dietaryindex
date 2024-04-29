# dietaryindex 

## Overview
___

**dietaryindex** is an R package that provides user-friendly, streamlined methods for standardizing the compilation of dietary intake data into index-based dietary patterns to enable the assessment of adherence to these patterns in epidemiologic and clinical studies, promoting precision nutrition.

## User-friendly tutorial page
___
### We have created a modern, user-friendly tutorial page for the dietaryindex package.
- ### [Click here](https://jamesjiadazhan.github.io/dietaryindex_manual/index.html)


## Citation
___
If you are using the Dietaryindex package in your research, **please be sure to cite our original work**. By doing so, you not only add credibility to your findings but also recognize and appreciate our intellectual efforts and contributions. The appropriate citation is as follows:

- *Jiada James Zhan, Rebecca A Hodge, Anne Dunlop, et al. Dietaryindex: A User-Friendly and Versatile R Package for Standardizing Dietary Pattern Analysis in Epidemiological and Clinical Studies. bioRxiv. Published online August 07, 2023:2023.08.07.548466. doi:10.1101/2023.08.07.548466*
  - https://www.biorxiv.org/content/10.1101/2023.08.07.548466v2

## How dietaryindex works
___
The **dietaryindex** package performs calculations in two steps:

1. Computation of the serving size of each food and nutrient category.

2. Computation of the individual dietary index using the serving size information.

## What dietaryindex can do
___
This package can calculate the following dietary pattern indexes. For more details, [check here](https://jamesjiadazhan.github.io/dietaryindex_manual/reference/index.html)

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

## How to use dietaryindex functions appropriately with specific data inputs
___
For a detailed explanation of these indexes, please check the attached Excel files:

- [dietaryindex_SERVING_SIZE_DEFINITION.xlsx](https://github.com/jamesjiadazhan/dietaryindex/blob/main/dietaryindex_SERVING_SIZE_DEFINITION.xlsx)

- [dietaryindex_SCORING_ALGORITHM.xlsx](https://github.com/jamesjiadazhan/dietaryindex/blob/main/dietaryindex_SCORING_ALGORITHM.xlsx)

## Validation
___
**dietaryindex** has been thoroughly validated for accuracy and reliability. We've ensured that all functions in the **dietaryindex** work as expected. 

### Validation R codes can be found in the Validation page within Article header on the top of the page
- ### [Click here](https://jamesjiadazhan.github.io/dietaryindex_manual/articles/validation.html)

- For validation details, download the **Validation file for publication.zip** to obtain the following files:

  - **Validation.r**: This R script contains all code used to generate the final validation files during the validation process.

  - **Validation figures.r**: This R script contains all codes used to produce validation figures during the validation.

  - **Raw validation files**: This folder houses all the raw simulation data files utilized in our validation.

  - **Final validation files**: This folder contains the final output files from our validation process. Columns prefixed with "EXP" represent the manually computed dietary index results, which serve as our gold standard. The subsequent columns show results produced by the dietaryindex package.

  - **HEI2015_NHANES_SAS_1718**: This folder includes all data files used in validating the HEI2015 results. We compared the SAS results (obtained using SAS codes from https://epi.grants.cancer.gov/hei/sas-code.html) and the dietaryindex results using the NHANES 1718 dataset. The results were found to be a 100% match, after rounding to two decimal places.

  - **Figure 1. Comparison of Accuracy**: dietaryindex-calculated vs. hand-calculated Dietary Index Values using the simulation datasets (sample sizes range from 10 to 26).

  - **Figure 2. Accuracy of HEI2015 in NHANES**: dietaryindex-calculated vs. SAS-calculated results from National Cancer Institute using the NHANES 2017-2018 data (n=7122).

  - **Figure 3. Accuracy of HEI2015 in ASA24**: dietaryindex-calculated vs. SAS-calculated results from National Cancer Institute using the ASA24 example data (n=21).

  - **Figure 4. Accuracy of HEI2015 in DHQ3**: dietaryindex-calculated vs. internal-calculated results from National Cancer Institute using the DHQ3 example data (n=23).
  
  - **Supplementary Material 3 Validation Figures.docx**: This word document includes all figures for the detailed validation of each dietary index, including the total dietary index and individual component dietary indexes. 



## Installation
___

Currently, **dietaryindex** is not available on [CRAN]

To install from this GitHub repository, use the **devtools** package:

Package dependencies: **dplyr**, **readr**, **haven** (automatically installed).

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
### Check the "Get started" header on the top of the page
- ### [Click here](https://jamesjiadazhan.github.io/dietaryindex_manual/articles/dietaryindex.html)


## Related Work
___

**dietaryindex** is mainly intended as a versatile tool to help for calculating different dietary indexes conveniently. It is designed to be flexible to work for almost all types of dietary assessment tools, including food frequency questionnaires, 24-hour dietary recalls, and even food records, while it also supports many 1-step dietary index calculations for NHANES, ASA24, and DHQ3.  Please follow the instruction of your specific dietary assessment tools and relevant articles regarding how to accurately define the serving size (see above) if it is not provided in our package, as they are the key to obtaining high-quality dietary indexes. **dietaryindex** also provides some help in defining the serving size in the help file, argument section. Note: some very specific dietary index components (low-fat dairy products and sugar-sweetened beverages) are not easily available and thus are difficult to assess. The author used individual-level food data to compute the population-level food group data. For example, the sugar-sweetened beverage serving is estimated by dividing the total added sugar intake in grams from beverages by 26, because 1 bottle (8 oz) of Coke has 26 g added sugars and this is used as the benchmark, as different sugar-sweetened beverages have largely different added sugar contents. Please use your own judgment to determine if the dietary indexes calculated using the **dietaryindex** package is appropriate for your research.


For NHANES data:

- FPED population file refers to the files in the Food Patterns equivalents for foods in the WWEIA (https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/fped-databases/). They look like FPED_DR1TOT or FPED_DR2TOT.
  - The FPED individual files look like FPED_DR1IFF or FPED_DR2IFF.

- NUTRIENT population file refers to the files in the Dietary Interview - Total Nutrient Intakes, First Day or Second Day (example: 05-06 https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Dietary&CycleBeginYear=2005). They look like 
  - The NUTRIENT individual files look like DR1IFF_D or DR2IFF_D.

- DEMO file refers to the DEMO file in the Demographic Variables & Sample Weights (example: 05-06 https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics&CycleBeginYear=2005)

FPED, NUTRIENT, and DEMO files are available within the package and in the Google Drive collected by the package developer for your convenience (https://drive.google.com/drive/folders/1umjhuS22aHEW_bU5AjYa8vrae91gsb0D?usp=share_link). 

### Contributing & Notes
___
**dietaryindex** is licensed under the [MIT License](https://github.com/jamesjiadazhan/dietaryindex/blob/main/other/LICENSE.txt). Please check out the [Contribution guide](https://github.com/jamesjiadazhan/dietaryindex/blob/main/CONTRIBUTING.md) for questions, feature requests, and bug reports. The maintainer will review pull requests and incorporate contributions at his discretion. You may also reach out to the maintainer, **Jiada (James) Zhan**, via his email: jzha832@emory.edu. **Jiada (James) Zhan** home page at Emory is: https://www.sph.emory.edu/phd-students/profile/index.php?FID=jiada-zhan-12906. **Becky Hodge** at the American Cancer Society provided substantial contributions to validate this package. **Michael L Orr** at Dean Jones/ Young-Mi Go Lab at Emory University helped design the dietaryindex logo. Thanks a lot for their help.
