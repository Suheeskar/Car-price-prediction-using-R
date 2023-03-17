# **Car Price prediction using R**

A Chinese automobile company Geely Auto aspires to enter the US market by setting up their manufacturing unit there and producing cars locally to give competition to their US and European counterparts.

They have contracted an automobile consulting company to understand the factors on which the pricing of cars depends. Specifically, they want to understand the factors affecting the pricing of cars in the American market, since those may be very different from the Chinese market. The company wants to know:

Which variables are significant in predicting the price of a car
How well those variables describe the price of a car Based on various market surveys, the consulting firm has gathered a large dataset of different types of cars across the American market.

**Approach**

1) Importing libraries

2) Reading the concerned dataset

3) Data Understanding

4) Data cleaning

5) Data visualization

6) PCA

7) Splitting the Data and feature scaling

8) Building a various regression models like ("lm","rf","svmRadial","xgbTree","xgbLinear")

9) Ensembling the Models

10) Making Predictions Using the Models

11) Model Evaluation using RMSE

**Conclusion**

out of all RMSE values, Ensembled method gives the less error.

| RMSE | Values |
| ----------- | ----------- |
| Ensemble | 0.138 |
| SVM | 0.172 |
| RF  | 0.138 |
| XGBT | 0.173 |
| XGBL | 0.162 |
