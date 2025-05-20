# Tourism Data Analysis in R (2002–2021) – Turkey

This project involves a comprehensive analysis of Turkey's quarterly tourism data from 2002 to 2021, focusing on the relationship between tourist arrivals, total tourism income, and various spending categories (e.g., accommodation, food & beverage, health, etc.).

Key steps and components of the analysis include:

🧹 Data Cleaning & Preprocessing: Handled missing values using multiple imputation techniques including linear regression and random forest imputation.

📈 Exploratory Data Analysis (EDA): Used time series plots, boxplots, and seasonal-trend decomposition (STL) to explore changes over time and across seasons.

📐 Feature Engineering: Calculated category income ratios and labeled them as High/Low using z-scores for binary classification tasks.

🔍 Label Prediction Models: Implemented and compared six machine learning models:

Logistic Regression

Decision Tree

Random Forest

XGBoost (Boosting)

Naive Bayes

K-Nearest Neighbors (KNN)

🧪 Model Evaluation: Used confusion matrix metrics (accuracy, precision, recall, F1-score) to evaluate performance across models and categories.

🔄 Data Splitting & Sampling: Applied stratified train-test split to ensure balanced class distributions.

📊 Visualization: Created clear, publication-ready visualizations using ggplot2 and autoplot for insights and reporting.

