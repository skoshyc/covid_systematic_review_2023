# covid_systematic_review_2023
This repository contains the data and code used for the COVID systematic review 2023
The R packages meta  (Balduzzi S, Rücker G, Schwarzer G (2019), How to perform a meta-analysis with R: a practical tutorial, Evidence-Based Mental Health; 22: 153-160) and dmetar (Harrer, M., Cuijpers, P., Furukawa, T. & Ebert, D. D. (2019). dmetar: Companion R Package For The Guide 'Doing Meta-Analysis in R'. R package version 0.1.0. URL http://dmetar.protectlab.org/). Forest plots were generated again using the R package meta. Outliers were removed using the dmetar package. 

The code details are as follows:
Each of the codes COVID_SR_measure_name (where measure_name can be either sensitivity, specificity, PPV or NPV) calculates the pooled values and generates the forest plots for the Overall, Symptomatic and Asymptomatic subgroups. The code also calculates the total sample numbers to include in the GRADE Pro software.

The Master File of Data extraction_New.xlsx is the data sheet used in all the codes. 

The Data extraction tool.xlsx provides a template for our data extraction tool, information about the different columns and a sample data extraction. 
