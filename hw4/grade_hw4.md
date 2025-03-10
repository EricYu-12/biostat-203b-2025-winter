*Zhiyuan Yu*

### Overall Grade: 242/250

### Late penalty

- Is the homework submitted (git tag time) before deadline? Take 10 pts off per day for late submission.  

### Quality of report: 10/10

-   Is the final report in a human readable format html? 

-   Is the report prepared as a dynamic document (Quarto) for better reproducibility?

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how results are produced by just reading the report? Take some points off if the solutions are too succinct to grasp, or there are too many typos/grammar. 

### Completeness, correctness and efficiency of solution: 197/200

- Q1 (100/100)

If `collect` before end of Q1.7, take 20 points off.

If ever put the BigQuery token in Git, take 50 points off.

Cohort in Q1.7 should match that in HW3.

  - Line 153. You can use `rename()` instead of `mutate()`

  - Q1.8. "Other" should be placed at the end. `-0.0` 

Q1.8 summaries should roughly match those given.
  
- Q2 (97/100)

    -  Your app can remove extreme values only for numerical variables. `+5.0`. 

    -  1st tab: I tried many times, but "Lab Measurements" and "Vitals" did not show up. But I know you wrote the code. `-10.0`

    -  Separate missing data summary looks good. `+2.0`

    -  For demographic summary, labeling and coloring look redundant because these are in the x-axis. `+0.0`

### Usage of Git: 10/10

-   Are branches (`main` and `develop`) correctly set up? Is the hw submission put into the `main` branch?

-   Are there enough commits (>=5) in develop branch? Are commit messages clear? The commits should span out not clustered the day before deadline. 
          
-   Is the hw submission tagged? 

-   Are the folders (`hw1`, `hw2`, ...) created correctly? 
  
-   Do not put a lot auxiliary files into version control. 

-   If those gz data files are in Git, take 5 points off.

### Reproducibility: 5/10

-   Are the materials (files and instructions) submitted to the `main` branch sufficient for reproducing all the results? Just click the `Render` button will produce the final `html`? 

-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

    - "~/203b-hw/hw4/mimiciv_shiny/mimic_icu_cohort.rds" does not work. `-5.0`
    
    - We assume you are in `mimiciv_shiny`. So you can just use `"mimic_icu_cohort.rds"`

### R code style: 20/20

For bash commands, only enforce the 80-character rule. Take 2 pts off for each violation. 

No violations.

-   [Rule 2.6](https://style.tidyverse.org/syntax.html#long-function-calls) The maximum line length is 80 characters. Long URLs and strings are exceptions.  

-   [Rule 2.5.1](https://style.tidyverse.org/syntax.html#indenting) When indenting your code, use two spaces.  

-   [Rule 2.2.4](https://style.tidyverse.org/syntax.html#infix-operators) Place spaces around all infix operators (=, +, -, &lt;-, etc.).  

-   [Rule 2.2.1.](https://style.tidyverse.org/syntax.html#commas) Do not place a space before a comma, but always place one after a comma.  

-   [Rule 2.2.2](https://style.tidyverse.org/syntax.html#parentheses) Do not place spaces around code in parentheses or square brackets. Place a space before left parenthesis, except in a function call.
