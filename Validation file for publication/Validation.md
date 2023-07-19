### Validation
___
Validation files:
- [Validation.r](https://github.com/jamesjiadazhan/dietaryindex/blob/main/Validation%20file%20for%20publication/Validation.r): This R script contains all code used to generate the final validation files during the validation process.
- [Validation figures.r](https://github.com/jamesjiadazhan/dietaryindex/blob/main/Validation%20file%20for%20publication/Validation%20figures.r): This R script contains all codes used to produce validation figures during the validation.
- [Raw validation files](https://github.com/jamesjiadazhan/dietaryindex/tree/main/Validation%20file%20for%20publication/Raw%20validation%20files): This folder houses all the raw simulation data files utilized in our validation.
- [Final validation files](https://github.com/jamesjiadazhan/dietaryindex/tree/main/Validation%20file%20for%20publication/Final%20validation%20files): This folder contains the final output files from our validation process. Columns prefixed with "EXP" represent the manually computed dietary index results, which serve as our gold standard. The subsequent columns show results produced by the dietaryindex package.
- [HEI2015_NHANES_SAS_1718](https://github.com/jamesjiadazhan/dietaryindex/tree/main/Validation%20file%20for%20publication/HEI2015_NHANES_SAS_1718): This folder includes all data files used in validating the HEI2015 results. We compared the SAS results (obtained using SAS codes from https://epi.grants.cancer.gov/hei/sas-code.html) and the dietaryindex results using the NHANES 1718 dataset. The results were found to be a 100% match, after rounding to two decimal places.
- [Supplementary Material 3 Validation Figures.docx](https://github.com/jamesjiadazhan/dietaryindex/blob/main/Validation%20file%20for%20publication/Supplementary%20Material%203%20Validation%20Figures.docx): This word document includes all figures for the detailed validation of each dietary index, including the total dietary index and individual component dietary indexes.

All dietaryindex-calculated results exhibited a full 100% accuracy when subjected to 2-decimal rounding by comparisons with the hand-calculated results using the simulation data ([Raw validation files](https://github.com/jamesjiadazhan/dietaryindex/tree/main/Validation%20file%20for%20publication/Raw%20validation%20files)):
![Figure 1  Overall accuracy of dietaryindex scoing algorithms](https://github.com/jamesjiadazhan/dietaryindex/assets/108076575/1b5145a1-dc17-49a7-a87d-a4cf09412fc0)


Utilizing the NHANES 2017-2018 data ([HEI2015_NHANES_SAS_1718](https://github.com/jamesjiadazhan/dietaryindex/tree/main/Validation%20file%20for%20publication/HEI2015_NHANES_SAS_1718)), the HEI2015 outputs from the dietaryindex package aligned almost perfectly (99.95% - 100%) with the results derived from the SAS codes from the National Cancer Institute, again featuring a 2-decimal rounding precision:
![Figure 2  Comparison of accuracy_dietaryindex vs SAS from NCI](https://github.com/jamesjiadazhan/dietaryindex/assets/108076575/14ec8779-75c0-49f7-9152-f0e7772f2a72)

