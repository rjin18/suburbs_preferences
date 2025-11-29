Project: "Postwar American Suburbanization and the Evolution of Political Preferences"
This project analyzes the long-run relationship between postwar American suburbanization and political preferences.

Structure:
'code/' subfolder contains all scripts used in this project

Data (not included):
Decennial Census (county characteristics), Leip's Atlas (county presidential election vote shares), CCES (political survey)

'code/' subfolder:
1_cleanGISoutput.R
- process data from GIS before moving onto a classification algorithm

2_county_classification.R
-test out various logistic regression classification models

2_county_UDgradient.R
-generate a second measurement of Suburbanization based on Clark (1951)

3_estimation_setup.R
-create estimation sample

4_descriptive_tabfig.R
-creates descriptive tables/figures for the paper

5_estimation_election_OLS.R
-run baseline OLS

6_estimation_election_IV.R
-estimate main specs using 2SLS and highway development as an instrument for suburbanization

7_individual_estimation_setup.R
-Prepare separate individual level datasets with ANES and CCES data

8_individual_OLS.R
-Run specs using OLS at the individual level

9_individual_IV.R
-Run specs using highway mileage as instrument for suburbanization at the individual level; using ANES and CCES data

10_individual_IV_Mar.R
-Run specs using ANES and CCES data and including marriage as a RHS variable

11_individual_tables.R
-Run final specs at individual level 

11_individual_tables.R
-Run final specs at individual level and create tex output that will turn into Tables 4 and 5 and Appendix Tables C.1 and C.2
