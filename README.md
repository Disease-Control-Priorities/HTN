# HTN

Code and input data for the Modeling cardiovascular outcomes associated with 80-80-80 population blood pressure targets paper. 

The model can be run from the model>intervention_full2022.R script. However, the input data (base_ratese_2022.csv) is too large to upload but may be provided upon request. 

Otherwise, to run code in its entirety is possible but will take considerable time and computational power. The following scripts can be run in order without needing additional datasets sent as described below:

1. data_preprocessing > 1. get_BPS > get_bp2022.R
2. data_preprocessing > 2. get_AARCs > base_rates_full2019.R
3. data_preprocessing > 2. get_AARCs > new_rates2019.R
4. data_preprocessing > 3. adjustments > adjustments.Rmd
5. data_preprocessing > 3. adjustments > bgmx_change.R
6. data_preprocessing > 4. coverage > coverage_newfig.R
7. model > intervention_full2022.R (this is the model! you can run this without the other files if you email and ask for base_rates_2022.csv)


