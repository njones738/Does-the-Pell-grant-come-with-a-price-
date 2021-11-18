# Personal data projects

This project is a continuation of my project ["Access to Higher Education"](https://github.com/njones738/Access-to-Higher-Education). 

- __Project Purpose:__  
This project seeks to further understand the typcial amount of debt accumulated by an independent student and dependent student. The Parent PLUS loan system is suppose to be used to allow the parents of dependent students to take on the debt that their child would otherwise have to take on. Since the PLUS loan system is only for dependent students, this researcher summed the amount typically accumulated by a dependent student with the amount typically accumulated by a parent for each school. In this researchers experience, the PLUS loan system does not check to see if the parent is committing to their obligation to utilize the loan they received on the financial needs of their child. In the situation where the parent does not commit to their obligation, the students ability to acquire the needed financial support is greatly effected and may lead to the student not completing their education and not able to pay back the debt they accumulated while attending school. A spatial analysis was conducted to see if there is a relationship between the typical amount of debt an independent student and a dependent student for each state.

- __Tools used:__   
R was used with VScode to conduct parametric and nonparametric tests, clean and manipulate parameters, and visualize the data. Packages such as tidyverse, tidycensus, tigris, sf, sp, ggplot, ggh4x, magrittr, jmuoutliers, perms, and stats were used.

- __Results:__  
It was found that for every $1,000 an independent student accumulates, a dependent student accummulates $1,500. These finding varies by state and future projects will study states like Utah and West Virginia where the relationship between the two debt amounts are near 1:1.


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
---- Notebook.docx
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