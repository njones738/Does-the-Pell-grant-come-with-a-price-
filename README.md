# Does the Pell grant come with a price?

This project is a continuation of my project ["Access to Higher Education"](https://github.com/njones738/Access-to-Higher-Education). 

__Project Purpose:__  

The goal of this research is to use the CollegeScorecard dataset to determine which post-secondary schools and states are helping low-income students succeed. This success is measured by the institution’s student graduation rate, loan default rate two years after graduation, dropout rate, and the typical amount of debt acquired by an independent student or a dependent student with their parent as they exit the institution, either by graduating or dropping out. The proportion of students still trying to finish their degree 8 years after their first enrollment is 0.4%, I am one of those 0.4%. I had to learn the implications of the dependency status the hard way. I had to find a way to attend school, even when I was homeless, had medical issues, and could not afford food. I had to learn financial and life lessons that are usually taught by the family. 

I propose that an educational institution provide mandatory training on the implications of dependent and independent status, as to whether the student will receive any of the parental loans that are taken out in their behalf. In addition, 4-year institutions, which have the majority of their student populations receiving a Pell grant, have lower graduation rates and higher dropout rates, resulting in higher loan default rates 2 years after the student leaves the institution. Educational institutions need to take their stewardship of Pell grant recipients seriously by providing coaching that will help them with academic success—skills that a high-income family generally provide for their children. By meeting the needs of Pell grant recipients, they will better meet the needs of all students at their institutions.

The CollegeScorecard dataset includes measurements of the following parameters for 6,654 post-secondary institutions:

* **INSTNM:** The name of the school.
* **STABBR:** The state abbreviation.
* **LATITUDE & LONGITUDE:** The latitude and longitude of the school.
* **PCTPELL:** The percent of the student's population that receive a Pell grant.
* **ICLEVEL:** The institutional class level (either 2-year or 4-year institution).
* **CONTROL:** Whether the control of the institution is public or private.
* **OMENRUP_ALL:** The 8-year outcome status measured as the proportion of students that did not receive an award (either a training certificate, associate, bachelor, or graduate degree) within 8 years of entry and their status is unknown.
* **OMAWDP8_ALL:** The 8-year outcome status measured as the proportion of students that did receive an award (either a training certificate, associate, bachelor, or graduate degree) within 8 years of entry.
* **BBRR2_FED_UG_DFLT:** The Borrower-based Repayment Rate measured as the percentage of undergraduate federal student loan borrowers in default after 2 years. Loans need to be paid back as compared to a Pell grant, which does not. These loans are either government-subsidized (interest starts accruing after graduation) or unsubsidized (interest starts accruing at the time the loan is taken) loans.
* **IND_DEBT_MDN:** The median amount of direct loans (government-subsidized and unsubsidized) an independent student receives while attending that university.
* **DEP_DEBT_MDN:** The median amount of direct loans (government-subsidized and unsubsidized) a dependent student receives while attending that university.
* **PLUS_DEBT_INST_MD:** The median Parent Loan for Undergraduate Students (PLUS) loan debt disbursed at the school for all students.
* **PELLCAT:** Indicator for majority Pell institution, where a majority Pell institution has over 50% of their students receiving a Pell grant.

Combining the latitude and longitude in the CollegeScorecard dataset with the United States Census shapefiles, I created a United States map for my analysis of the relationship between the typical amount of debt an independent student receives and the typical amount of debt a dependent student with their parents' debt receives while attending school.

- __Tools used:__   
* R
* Python
* Github
* VSCode

- __Results:__  
* **Independent and Dependent Student Debt Vertical Stratified Bar Chart (Figure 1):** The length of the gold bar represents the median independent student debt for each state. The length of the grey bar represents the median dependent student debt with their parents for each state. The length of the gold bar is superimposed over the grey bar and both bar lengths start from zero. Dependent students with their parents accumulate more debt than independent students across all states. For all institutions in the dataset, the vertical red line corresponds to the median debt for independent students as they exit the institution they attended. The blue line corresponds to the median debt for dependent students and their parents as they exit the institutions. 
* **Independent and Dependent Student Debt U.S. Map (Figure 2):** The relationship between the typical amount of debt a dependent student receives with their parents (Y) and the typical amount of debt an independent student receives (X) is displayed for each state in blue. In comparison, the black line is the relationship for the whole dataset including all states if the two debt amounts were equal. The black line and the grey background of all datapoints in the set standardize the size of the graph for all states. 
* **Student Dropout Rate Stratified by combinations of Control and Majority/Minority Pell Status (Figure 3):** The dropout rate for 4-year minority Pell institutions is much lower (16%) than other combinations (42%, 36%, 36%). Notably, 2-year minority Pell institutions have the highest dropout rate of all (42%). One source suggested that students at 2-year institutions do not realize that they can use the Pell grant for a 2-year institution. Possibly, by informing their students about the Pell grant opportunity, some of the minority Pell institutions may be able to switch to the majority Pell classification and lower their dropout rate.
* **Student Graduation Rate Stratified by combinations of Control and Majority/Minority Pell Status (Figure 4):** The overall 2-year minority Pell graduation rate (28%) is half the overall 2-year majority Pell graduation rate (56%). Again, informing 2-year minority Pell institution students about the Pell grant opportunity may improve graduation rates. 
* **Breakdown of Pell, Years, and Control Waffle Charts (Figure 5):** The number of minority Pell institutions exceeds the number of majority Pell institutions. Within the majority Pell institutions, the number of private institutions greatly exceeds the number of public institutions for both 2-year and 4-year schools. The fact that private institutions greatly exceed public in the majority Pell category suggests that private institutions are better at conveying the availability of the Pell grant to their students.
* **Default Rate Stratified by Graduation Rate (Figure 6):** The default rates for institutions with graduation rates of 50% or less are 12.56% and 10.07% In contrast, the default rates for institutions with graduation rates greater than 50% are 5.67% and 5.28%. Increasing the graduation rates may help students pay off their loans. For institutions with a graduation rate greater than 50% the median 2-year default rate is 5.67% while for institutions with a graduation rate of 50% or less the median 2-year default rate is 11.09%. I am 95% confident that the true difference in the center of the two distributions is between 4.81% and 5.42%.
* **Student Default Rate Stratified by combinations of Control and Majority/Minority Pell Status (Figure 7):** For 2-year institutions, the default rates are similar. However, for 4-year institutions, the loan default rate for majority Pell institutions is much higher (11%). This 11% agrees with the fact that in Figure 4, the 4-year majority Pell institutions had a graduation rate of 41%, which puts them in the category in Figure 6 that is associated with a loan default rate of 10.07%. The focus is again on increasing graduation rate to improve loan repayment.

- __Methods__

* **Coding Methods:** I used tidyverse to pipe functions (e.g. mutate, filter, select, and lapply). I used packages that include stringr, pplyr, dplyr, magrittr, ggplot, ggpubr, ggh4x, geofacet, sf, sp, and tidycensus. My favorite package is magrittr because of the assignment pipe %<>%.
* **Data Wrangling:** For the CollegeScorecard dataset, I created an anonymous function that checks each column for missing values, and then I dropped all columns with zero entries. Observations that were missing their institutions name, proportion of Pell recipients in their student population, their longitude and latitude, and their state were dropped. This left a total of 952 variables and 6186 observations. As I did my analyses, I created multi-layer filters to create the needed subsets. 
* **Spatial Analysis:** The census shapefiles and data from the packages tigris and tidycensus were used to create maps of the institutional locations by state. I joined together the polygons of the states that are provided in the shapefile with the latitude and longitude of each school from the CollegeScorecard data.
* **Nonparametric Binomial Distribution Test and Confidence Intervals:** The binomial test was used to verify the National Center for Education Statistics’ claim that the average net price at a 4-year institution is $18,500. 
* **K-sample Permutation Tests:** The K-sample Permutation Test was used to analyze whether graduation rate quartiles predicted the loan default rate.
* **Permutation Tests on the Median:** For majority and minority Pell schools, I tested to see if there was a difference between the median dropout rate, graduation rate, and the 2-year loan default rate.
* **Permutation test on the slope:** The permutation test on the slope was used to determine if there was a relationship between independent and dependent student debt.
* **Geom_smooth:** This function in ggplot was used to map the slope of independent student debt by dependent student debt over the state of the institution.
* **Graphics:** Stratified waffle charts, stratified histograms, a stratified vertical bar chart, and a geofaceted map were used to visualize the findings.
 
## Folder structure

```
- readme.md
- data
---- csc_dict.csv
---- MERGED2018_19_PP.csv
---- RANKING_DATA.csv
---- state_fp_codes.csv
- documents
---- JONES.AnalyticsDay.FALL21-poster.pptx
---- JONES.AnalyticsDay.FALL21-poster.pdf
- images
- scripts
---- csc_preprocess.r
---- MASTER_FILE.r
---- UnitedState_spatialGraphic.r
```

## Data sources

### [The CollegeScorecard dataset](https://collegescorecard.ed.gov/data/)
### [The CollegeScorecard data Dictionary](https://data.ed.gov/dataset/college-scorecard-all-data-files-through-6-2020/resources?resource=658b5b83-ac9f-4e41-913e-9ba9411d7967)
### [Can the parents of independent students take out a PLUS loan?](https://studentloanhero.com/featured/parent-plus-loan-pay-college/)
 * No, the parents of independent students cannot take out a PLUS loan.