# Geospatial analysis

Geospatial analysis of plasma Selenim (Se) concentration in women (15-49 years old) and maize Se concentration in Malawi.

This study is part of my PhD on the spatial variation of mineral composition in foods and its impact in nutritional outcomes. 

Main **objective** is to understand the spatial variation in the concentration in the plasma selenium (Se) in women (15-45 years old) and its relationship with the spatial variation in maize Se concentration in Malawi. 

To generate the cleaned version of the maize Se concentration used in this analysis you need to get the following datasets:

1) [GeoNutrition](https://figshare.com/articles/dataset/Cereal_grain_mineral_micronutrient_and_soil_chemistry_data_from_GeoNutrition_surveys_in_Ethiopia_and_Malawi/15911973)

Kumssa, D.B., Mossa, A.W., Amede, T. et al. Cereal grain mineral micronutrient and soil chemistry data from GeoNutrition surveys in Ethiopia and Malawi. Sci Data 9, 443 (2022).

2) [Chilimba](https://rdcu.be/d68Lx) 

Chilimba, A., Young, S., Black, C. et al. Maize grain and soil surveys reveal suboptimal dietary selenium intake is widespread in Malawi. Sci Rep 1, 72 (2011).

And, run the following script in the outlined order:

00_cleaning-maize.R 

01_maize-model.R

To generate the cleaned version of the plasma Se concentration used in this analysis you need access the data from [DHS website](https://dhsprogram.com/methodology/survey/survey-display-483.cfm?showall=yes), and to get the following datasets:

1) DHS Survey data (MWIR7AFL.dta)
2) DHS Biomarkers data (MW_WRA.dta)
3) DHS GPS location ("MWGE7AFL.shp")

And, run the following script in the outlined order:

00_cleaning-dhs.R 

To generate the boundaries used to generate the aggregation levels (10-60 km & Enumeration Area (EA) group & District)

00_cleaning-boundaries.R

00_cleaning-location.R


For detail information on the data, methods and analysis, please check the documentation (in the doc folder)