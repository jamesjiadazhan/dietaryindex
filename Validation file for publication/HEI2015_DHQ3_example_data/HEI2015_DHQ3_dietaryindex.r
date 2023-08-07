library(dplyr)
library(haven)
library(readr)
# Loading dependency packages first can help avoid the namespace conflict if you want to use dplyr by yourself later in addition to the internal use of dplyr within the diataryindex package
# Load the dietaryindex package
library(dietaryindex)

# set working directory
setwd("/Users/james/Desktop/Emory University - Ph.D./dietaryindex_package/dietaryindex/Validation file for publication/HEI2015_DHQ3_example_data")

# Read in the DHQ3 data
DHQ3_exp = read_csv("Sample total daily results.csv", skip = 1)

# Calculate HEI-2015 score using the dietaryindex package
HEI2015_DHQ3_exp = HEI2015_DHQ3(DHQ3_exp)

# Write the HEI-2015 score calculated by dietaryindex to a csv file
write_csv(HEI2015_DHQ3_exp, "HEI2015_DHQ3_dietaryindex.csv")


