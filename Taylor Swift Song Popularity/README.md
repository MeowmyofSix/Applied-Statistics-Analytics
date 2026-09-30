# Bop or Flop: Predicting Taylor Swift Song Popularity

## Project Overview

This project examines whether the audio characteristics of Taylor Swift songs can be used to predict song popularity. It was completed as a graduate data mining project for STA 691 at Central Michigan University.

Using R, we analyzed Spotify audio characteristics across 172 songs from Taylor Swift's original album releases from *Taylor Swift* through *Midnights*. Two modeling objectives were considered:

1. Predicting Spotify popularity as a continuous response.
2. Classifying songs as a "bop" or "flop" using the median popularity score as the classification threshold.

The project included exploratory data analysis, data transformation, feature assessment, model development, cross-validation, and comparison of predictive performance across a variety of statistical and machine-learning methods.

## Methods

Methods evaluated included:

### Continuous Popularity
- Multiple linear regression
- Best subset selection
- Ridge regression
- Lasso regression
- Principal component regression (PCR)
- Partial least squares (PLS)
- Regression trees
- Bagging
- Random forests
- Boosting

### Bop/Flop Classification
- Logistic regression
- Linear discriminant analysis
- Naive Bayes
- Classification trees
- Support vector classifiers/machines
  - Linear kernel
  - Radial kernel
  - Polynomial kernel

## Key Results

For prediction of continuous Spotify popularity, random forest produced the lowest test mean squared error (MSE = 118.856), followed by bagging (MSE = 122.543).

For classification of songs as a "bop" or "flop," the classification tree produced the lowest test error rate (0.256).

Across the models, several audio characteristics consistently emerged as important predictors of popularity, particularly:

- Energy
- Loudness
- Speechiness
- Acousticness
- The interaction between energy and loudness

## Tools & Skills Demonstrated

- R / RStudio
- Exploratory data analysis
- Data transformation and feature engineering
- Statistical modeling
- Regression and classification
- Model selection and regularization
- Cross-validation
- Ensemble methods
- Support vector machines
- Model evaluation and comparison
- Data visualization
- Technical reporting

## Repository Contents

- `Taylor_Swift_Song_Popularity.R` — R code used for exploratory analysis, statistical modeling, model evaluation, and visualization.
- `Taylor_Swift_Bop_or_Flop_Report.pdf` — Full technical report describing the methodology, results, and conclusions.
- `taylor_swift_spotify_OG.xlsx` — Dataset used for the analysis.

## Data

The analysis used the *Taylor Swift Spotify Dataset* originally published on Kaggle by Jarred Priester. The original dataset contained 530 observations across multiple releases and editions.

For this analysis, Taylor's Version re-recordings, deluxe editions, and live releases were excluded to avoid representing the same songs multiple times and to focus on the original album releases. The final analytical dataset contained 172 songs across 10 albums.

## Project Context

This project was completed collaboratively by Jordan Leh and Lisa Platt as part of graduate coursework in Applied Statistics at Central Michigan University in May 2024.

The statistical analysis was performed in R/RStudio. Analytical outputs and visualizations were incorporated into Microsoft Word to produce the final technical report.