# Bayesian-Classification

## Overview
The Bayesian-Classification repository is a Bayesian data modeling pipeline for predicting breast cancer diagnosis based on cell nuclei features. The dataset used for this task is the [Breast Cancer Wisconsin (Diagnostic)](https://archive.ics.uci.edu/ml/datasets/breast+cancer+wisconsin+(diagnostic)) dataset.
This pipeline includes data preprocessing, exploratory analysis, data modeling, and model evaluation. Notably, it compares the performance of different models under both Bayesian and Frequentist frameworks. 
Bayesian Lasso and Bayesian Logistic Regression exhibit a 20% improvement in model accuracy compared to Frequentist methods.

## Files
The repository includes the following files:
* `Bayesian ssvs.R`: This file incorporates Stochastic Search Variable Selection (SSVS) and Bayesian Logistic Regression for feature selection, model training, and prediction.
* `Bayesian lasso.R`: This file incorporates Bayesian Lasso and Bayesian Logistic Regression for feature selection, model training, and prediction.
* `Frequentist PCA.R`: This file incorporates Principal Component Analysis (PCA) and Frequentist Logistic Regression for feature selection, model training, and prediction.
* `Bayesian_pipeline.R`: This script integrates the entire data preprocessing, exploratory analysis, and the three modeling approaches into a seamless workflow.
  
## Author
* Yaxuan Zhang
* Olivia Zang
