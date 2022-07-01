## dietaryindex

### Overview
___
The goal of this package **dietaryindex** is for calculating different dietary pattern indexes or scores easily and conveniently if serving sizes for each food and nutrient have been calculated previously. The serving sizes have to be calculated ahead. **dietaryindex** would work for all existing dietary assessment tools (e.g. FFQ, ASA24) once the serving sizes are available.

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

The **dietaryindex** package currently contains 8 key functions:
>`HEI2015()`, Healthy Eating Index 2015 (https://www.fns.usda.gov/how-hei-scored)

>`AHEI()`, alternative healthy eating index (https://pubmed.ncbi.nlm.nih.gov/22513989/)

>`AHEIP()` , alternative healthy eating index - pregnancy (https://pubmed.ncbi.nlm.nih.gov/19465182/)

>`DASH()`, Dietary Approaches to Stop Hypertension (https://pubmed.ncbi.nlm.nih.gov/18413553/)

>`DASHI()`, Dietary Approaches to Stop Hypertension Index (modified from multiple sources to provide most updated serving size-based DASH index, including https://www.nhlbi.nih.gov/education/dash-eating-plan, https://pubmed.ncbi.nlm.nih.gov/17324731/, https://www.dietaryguidelines.gov/sites/default/files/2020-12/Dietary_Guidelines_for_Americans_2020-2025.pdf, https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/414155, https://www.nejm.org/doi/full/10.1056/nejm199704173361601)

>`MED()`, Mediterranean diet (https://pubmed.ncbi.nlm.nih.gov/33574608/)

>`MEDI()`, Mediterranean diet index, serving size-based (https://pubmed.ncbi.nlm.nih.gov/28160450/)
  
  
### Related Work
___

**dietaryindex** is intended as a tool to help for calculating different dietary indexes with given food/nutrient serving sizes. It would work for all types of food frequency questionnaires and even 24-hours dietary recalls, but you would have to calculate the serving size before using the package. Please follow the instruction of your specific dietary assessment tools and relevant articles regarding how to accurately define the serving size (see above), as they are the key to obtain high-quality dietary indexes. **dietaryindex** also provides some help in defining the serving size in the help file, argument section. 

This package requires the **dplyr** package to be installed. Library statement of the dplyr package is included though for your convenience. 

### Contributing

**dietaryindex** is licensed under the [MIT License]. Questions, feature requests and bug reports are welcome via the [issue queue](https://github.com/jamesjiadazhan/dietaryindex/issues). The maintainer will review pull requests and incorporate contributions at his discretion.
