<h1>
Analytics using SFO crime data from Kaggle </h1><br><br>

<h2>Graphical analysis & Data Exploration </h2>
(1) HEATMAPS: <br>
Code for heatmap and charts given in program "hmap.R". <br>
To view images/charts or run the code online use the Kaggle platform link below: <br>
https://www.kaggle.com/anu2analytics/sf-crime/heatmap-for-crime-categories-and-area <br>
One heatmap image added to this folder as heatmap_SFO.png <br><br>

(2) CHI-SQ TEST & CORRELATION VISUALIZATION: <br>
program "relations.R" performs the following tasks: <br>
<ol> (a) chi-sq test to check if relationship exists between crimecategory (categorical variable)
and other factors like district, x/y coordinaates, month, year, etc.</ol>
<ol> (b) corrgram function to visualize correlation matrix and dependencies.</ol>
Related images include corr_categ_vars.jpg, corr_matrx_quant.jpg & cov_variables.jpg . <br><br>

<h2>Prediction Algorithm: </h2>
Multinomial regression.
<br><br>

<h2>Model 1: simple formula with default options. </h2>
Formula: Category ~ DayOfWeek PdDistrict<br>
Score: 2.65 <br>
Output file: multinom.zip <br> <br>

<h2>Model 2: complex formula using higher number of iterations. </h2>
Formula:  multinom(Category ~ DayOfWeek + year + mth + PdDistrict, 
                data = mytrain, maxit = 500)<br>
Score: 2.60 <br>
Output file: multinom_dates.zip <br>

