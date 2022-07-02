## dietaryindex

### Overview
___
The main goal of this package **dietaryindex** is for calculating different dietary pattern indexes or scores easily and conveniently if serving sizes for each food and nutrient have been calculated previously. The serving sizes for most dietary indexes have to be calculated manually ahead except for the BLOCK (BLOCK FFQ), which has functions to calculate all dietary indexes. **dietaryindex** would work for all existing dietary assessment tools (e.g. FFQ, ASA24) once the serving sizes are available. The serving size calculation functions for HEI2015 are available for NIH-AARP and NHANES.

### Installation
___

**dietaryindex** is currently not available on [CRAN]


To install the development version hosted on this GitHub repository, use the **devtools** package and the following:

```
# install.packages("devtools")
devtools::install_github("jamesjiadazhan/dietaryindex")
```
### Getting Started
___
```
library(dietaryindex)
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

>`NHANES_FPED_PRE_HEI15()`, Prepare the NHANES_FPED data (after 2005) for calculating the serving sizes for HEI2015

### Examples:

___

### Related Work
___

**dietaryindex** is mainly intended as a tool to help for calculating different dietary indexes with given food/nutrient serving sizes. It would work for all types of food frequency questionnaires and even 24-hours dietary recalls, but you would have to manually calculate the serving size before using the package except for the BLOCK FFQ. Currently, the serving size calculation functions are available for the BLOCK FFQ for all dietary indexes. The serving size calculation functions for HEI2015 are available for NIH-AARP and NHANES. Please follow the instruction of your specific dietary assessment tools and relevant articles regarding how to accurately define the serving size (see above) if it is not provided, as they are the key to obtain high-quality dietary indexes. **dietaryindex** also provides some help in defining the serving size in the help file, argument section. 

This package requires the **dplyr** package to be installed. Library statement of the dplyr package is included though for your convenience. 

### Contributing

**dietaryindex** is licensed under the [MIT License]. Questions, feature requests and bug reports are welcome via the [issue queue](https://github.com/jamesjiadazhan/dietaryindex/issues). The maintainer will review pull requests and incorporate contributions at his discretion. You may also reach out to the maintainer, James Jiada Zhan, via his email: jzha832@emory.edu.
