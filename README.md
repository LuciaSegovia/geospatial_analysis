# Geospatial analysis

Geospatial analysis of plasma Se concentration in women of reproductive age and maize Se concetration in Malawi

This study is part of my PhD on the spatial variation of mineral composition in foods and its impact in nutritional outcomes. 

Main **objective** is to understand the spatial variation in Plasma Se in WRA and its relationship with the spatial variation in maize Se concentration in Malawi. 

To generate the cleaned version of the maize Se concentration used in this analysis you need to get the following datasets:

1) GeoNutrition - REF
2) Chilimba - REF
3) CHELSEA - REF

And, run the following script in the outlined order:

00_cleaning-maize.R 
00_cleaning-location.R

To generate the cleaned version of the plasma Se concentration used in this analysis you need access the data from DHS website, and to get the following datasets:

1) DHS Survey data (MWIR7AFL.dta)
2) DHS Biomarkers data (MW_WRA.dta)
3) DHS GPS location ("MWGE7AFL.shp")

And, run the following script in the outlined order:

00_cleaning-dhs.R 
00_cleaning-location.R
