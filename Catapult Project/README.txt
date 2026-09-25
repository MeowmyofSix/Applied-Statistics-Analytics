Catapult Experimental Design
Experimental Design | R | Two-Way ANOVA | Data Visualization

Project Overview
This project investigates how launch angle and pull-back angle affect the distance traveled by a ping-pong ball launched from a catapult.

A two-factor factorial experiment was conducted to evaluate the individual effects of both variables and determine whether an interaction existed between them.

Experimental Design
- Six launch angles and six pull-back angles
- Five replicates per treatment combination
- 180 total observations
- Response variable: launch distance in inches

Tools and Methods
- R and R Markdown
- ggplot2 and dplyr
- Two-way ANOVA with interaction
- Residual diagnostics and post hoc comparisons

Key Findings
Both launch angle and pull-back angle had statistically significant effects on launch distance, with a significant interaction between the two factors.
The model predicted a maximum launch distance of 157 inches at a launch angle of 0° and pull-back angle of 90°.
Three subsequent validation trials exceeded that prediction substantially. Because new rubber bands were used during validation, these results highlight the importance of controlling experimental conditions.

Repository Contents
Catapult Project/
├── Data/
│   └── Catapult Project.xlsx
├── Images/
│   ├── Figure 1.png
│   ├── Figure 2.png
│   └── Figure 3.jpg
├── Catapult Project MeowmyofSix.Rmd
└── README.md

Reproducing the Analysis
Download the project folder, open the R Markdown file in RStudio, install the required R packages and knit the document. The analysis uses relative file paths.