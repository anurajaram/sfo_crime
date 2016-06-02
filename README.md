<h1>
Analytics using SFO crime data from Kaggle </h1><br><br>

<h2>Algorithm: </h2>
Multinomial regression. <br><br>
<br><br>

<h2>Model 1: simple formula with default options. </h2>
Formula: Category ~ DayOfWeek PdDistrict<br>
Score: 2.65 <br>
Output file: multinom.csv <br> <br>

<h2>Model 2: complex formula using higher number of iterations. </h2>
Formula:  multinom(Category ~ DayOfWeek + year + mth + PdDistrict, 
                data = mytrain, maxit = 500)<br>
Score: 2.60 <br>
Output file: multinom_dates.csv <br>

